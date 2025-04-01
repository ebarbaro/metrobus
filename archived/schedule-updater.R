while (1==1) {

  rm(list = ls(all.names = TRUE)) 
  closeAllConnections()
  start_time <-  Sys.time()
  {
    if (Sys.info()['sysname'] == "Linux") {
      setwd("/home/eab")
      wd <- "/home/eab"
      path <- "/home/eab/Projects/metrobus/routes/"
    }
    else if (Sys.info()['sysname'] == "Windows") {
      setwd("C:/Users/ebarbaro")
      wd <- "C:/Users/ebarbaro"
      path <- "C:/Users/ebarbaro/R/Sandbox/metrobus/routes/"
    }
  }
  
  ## bus position (Returns bus positions for the given route. If no parameters are specified, all bus positions are returned. Bus positions are refreshed approximately every 7 to 10 seconds.)
  bus_pos <- GET("https://api.wmata.com/Bus.svc/json/jBusPositions", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  
  
  {
    if (Sys.info()['sysname'] == "Linux") {
      bus_pos <- suppressWarnings(rbindlist(bus_pos,fill = TRUE)) 
      bus_pos <- bus_pos[!grepl("EMPLOYEE SHUTTLE",bus_pos$TripHeadsign),]
    }
    else if (Sys.info()['sysname'] == "Windows") {
      bus_pos <- suppressWarnings(rbindlist(bus_pos$BusPositions,fill = TRUE)) 
      bus_pos <- bus_pos[!grepl("EMPLOYEE SHUTTLE",bus_pos$TripHeadsign),]
  
    }
  }

  lines <- subset(bus_pos,select=c(RouteID,DirectionText))
  lines <- lines[!duplicated(lines[,c("RouteID","DirectionText")]),]
  
  ## get bus incidents
  bus_incidents <- GET("https://api.wmata.com/Incidents.svc/json/BusIncidents", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON() 

    {
    if (Sys.info()['sysname'] == "Linux") {
      bus_incidents <- suppressWarnings(rbindlist(bus_incidents,fill = TRUE))
      bus_incidents$RoutesAffected <- as.character(bus_incidents$RoutesAffected)
    }
    else if (Sys.info()['sysname'] == "Windows") {
        bus_incidents <- suppressWarnings(rbindlist(bus_incidents$BusIncidents,fill = TRUE))
    }
  }

  bus_incidents$IncidentLastUpdated <- gsub("T"," ",bus_incidents$DateUpdated)
  bus_incidents$IncidentLastUpdated <- ymd_hms(bus_incidents$IncidentLastUpdated, tz = Sys.timezone())
  bus_incidents_sub <- subset(bus_incidents,!(Description %like% "operator availability"))
  bus_incidents_sub <- left_join(bus_incidents_sub,lines,by=c("RoutesAffected"="RouteID"))
  bus_incidents_sub_x <- bus_incidents_sub %>%
                              group_by(RoutesAffected) %>%
                              filter(!n()>1)
  
  {                           
    if (nrow(bus_incidents_sub_x)>=1) {
    bus_incidents_sub_x$DirectionText <- ifelse((bus_incidents_sub_x$DirectionText == "NORTH"),"SOUTH",
                                          ifelse((bus_incidents_sub_x$DirectionText == "SOUTH"),"NORTH",
                                           ifelse((bus_incidents_sub_x$DirectionText == "EAST"),"WEST", 
                                            ifelse((bus_incidents_sub_x$DirectionText == "WEST"),"EAST",NA))))
    
    bus_incidents_sub <- rbind(bus_incidents_sub,bus_incidents_sub_x)
    rm(bus_incidents_sub_x)
    }
    else {
      rm(bus_incidents_sub_x)
    }
   }
  
  bus_incidents_sub$DirectionAffected <- ifelse((grepl("Northbound|northbound",bus_incidents_sub$Description)&!grepl("NORTH",bus_incidents_sub$DirectionText)),NA,
                                          ifelse((grepl("Southbound|southbound",bus_incidents_sub$Description)&!grepl("SOUTH",bus_incidents_sub$DirectionText)),NA,
                                           ifelse((grepl("Eastbound|eastbound",bus_incidents_sub$Description)&!grepl("EAST",bus_incidents_sub$DirectionText)),NA,
                                            ifelse((grepl("Westbound|westbound",bus_incidents_sub$Description)&!grepl("WEST",bus_incidents_sub$DirectionText)),NA,bus_incidents_sub$DirectionText))))
  
  bus_incidents_sub$IncidentType <- ifelse((bus_incidents_sub$Description %like% "operator availability"),"Notice",
                                     ifelse((bus_incidents_sub$Description %like% "detour"),paste0("Detour (",str_sub(bus_incidents_sub$DirectionAffected,1,1),"B)"),
                                      ifelse((bus_incidents_sub$Description %like% "delay"),paste0("Delay (",str_sub(bus_incidents_sub$DirectionAffected,1,1),"B)"),bus_incidents_sub$IncidentType)))
  bus_incidents_sub$IncidentDescription <- bus_incidents_sub$Description
  bus_incidents_sub$IncidentShortDescription <- paste0(gsub("\\..*","",bus_incidents_sub$Description),".") 
  bus_incidents_sub <- bus_incidents_sub[!is.na(bus_incidents_sub$DirectionAffected),]
  bus_incidents_sub <- bus_incidents_sub[!duplicated(bus_incidents_sub[,c("DirectionAffected","IncidentID")]),]
  bus_incidents <- subset(bus_incidents_sub,select=c(RoutesAffected,DirectionAffected,IncidentType,IncidentShortDescription,IncidentDescription,IncidentID,IncidentLastUpdated))
  end_time <-  Sys.time()
  
  {
    if ((nrow(bus_pos) > 0) & (nrow(bus_incidents) > 0)) {
      
    if (Sys.info()['sysname'] == "Linux") {
        pg <- dbConnect(RPostgres::Postgres()
                        , host=Sys.getenv("pg_host")
                        , port=Sys.getenv("pg_port")
                        , dbname="wmata"
                        , user=Sys.getenv("pg_user")
                        , password=Sys.getenv("pg_password"))
      
         if (nrow(bus_incidents) > 0) {
		   bus_incidents$StartTime <- start_time
		   bus_incidents$EndTime <- end_time
		   bus_incidents$trigger_timestamp <- Sys.time()
           save(bus_incidents, file = paste0("/home/eab/Projects/metrobus/routes/images/workspaces/bus_incidents.RData"))
           dbWriteTable(pg,"bus_incidents",bus_incidents,row.names = FALSE, overwrite = FALSE, append = TRUE) 
           log <- data.frame(
             RouteID = 999,
             Source = "bus_incidents",
             rows_added = length(unique(bus_incidents$RoutesAffected)),
             StartTime = start_time,
             EndTime = end_time,
             trigger_timestamp = Sys.time())
           dbWriteTable(pg,"log",log, row.names = FALSE, overwrite = FALSE, append = TRUE)  
           suppressWarnings(rm(log))
    
         }
        
         if (nrow(bus_pos) > 0) {
           
           bus_pos$trigger_timestamp <- Sys.time()
           dbWriteTable(pg,"bus_pos",bus_pos,row.names = FALSE, overwrite = TRUE, append = FALSE)   
           log <- data.frame(
             RouteID = 999,
             Source = "bus_pos",
             rows_added = nrow(bus_pos),
             StartTime = start_time,
             EndTime = end_time,
             trigger_timestamp = Sys.time())
           dbWriteTable(pg,"log",log, row.names = FALSE, append = TRUE)  
         }
        
        dbDisconnect(pg) 
        closeAllConnections()
    }
    
    suppressWarnings(rm(start_time,end_time,bus_incidents_sub,lines))
    bus_incidents <- bus_incidents[order(bus_incidents$RoutesAffected, decreasing = FALSE),]
    RouteID <- bus_incidents[!duplicated(bus_incidents$RoutesAffected),]
    RouteID <- subset(RouteID, select=c(RoutesAffected))
    names(RouteID)<-"RouteID"
    RouteID <- as.array(RouteID$RouteID)
    
    desc <- bus_incidents %>% group_by(RoutesAffected) %>%
                              arrange(desc(IncidentLastUpdated)) %>%
                              filter(row_number() == 1)
    desc <- desc[order(desc$RoutesAffected, decreasing = FALSE),]
    date <- as.character(str_sub(Sys.time(),1,10))
    r <- 1
    
    if (!dir.exists(paste0(path,date))) { dir.create(paste0(path,date)) }
    
    for (r in r:length(RouteID)) {
      {
        if (Sys.info()['sysname'] == "Linux") {
          setwd("/home/eab")
          wd <- "/home/eab"
          path <- "/home/eab/Projects/metrobus/routes/"
        }
        else if (Sys.info()['sysname'] == "Windows") {
          setwd("C:/Users/ebarbaro")
          wd <- "C:/Users/ebarbaro"
          path <- "C:/Users/ebarbaro/R/Sandbox/metrobus/routes/"
        }
      }
      #for (r in 1) {  
      if (!dir.exists(paste0(path,date,"/",RouteID[r]))) { dir.create(paste0(path,date,"/",RouteID[r])) }
        if (!dir.exists(paste0("/home/eab/Projects/metrobus/logs/",date))) { dir.create(paste0("/home/eab/Projects/metrobus/logs/",date)) }

      closeAllConnections()
      input <- data.frame(
        input1 = as.character(paste0("setwd(",'"',wd,'"',")")),
        input2 = as.character(paste0("print(getwd())")),
        input3 = as.character(paste0("print(environment())")),
        input4 = as.character("rm(list = ls(all.names = TRUE))"),
        input5 = as.character(paste0("RouteID <- ",'"',RouteID[r],'"')))
      
      input$input <- paste(input$input1,input$input2,input$input3,input$input4,input$input5,sep = "\n")
      input <- as.character(input$input)
      cat(input, file = paste0(path,"input.txt"),sep = "\n")
      {
        if (Sys.info()['sysname'] == "Linux") {
          files <- list.files(path,pattern = "\\input.txt$|\\linux.txt$")
        }
        else if (Sys.info()['sysname'] == "Windows") {
          files <- list.files(path,pattern = "\\input.txt$|\\windows.txt$")
        }
      }
      files <- as.data.frame(files)
      files <- files[order(-xtfrm(files$files), decreasing = FALSE),]
      outFile <- file(paste0(path,date,"/",RouteID[r],"/",RouteID[r],".R"), "w")
      f <- 1
      
      for (f in 1:length(files)){
        x <- suppressWarnings(readLines(paste0(path,files[f])))
        writeLines(x, outFile)
        f <- f+1
      }
      close(outFile)
      rm(outFile,x,f,files,input) 

      file.copy(paste0(path,date,"/",RouteID[r],"/",RouteID[r],".R"),"/home/eab/Projects/metrobus/logs/Errors", overwrite = TRUE) 

    {
        if (Sys.info()['sysname'] == "Linux") {

          infile <- paste0('"',"/home/eab/Projects/metrobus/bus-position-saver.R",'"')
          outfile <- paste0('"',"/home/eab/Projects/metrobus/logs/",date,"/","bus-position-saver.log",'"')
          cmd <- as.character(paste("/opt/R/release/lib/R/bin/Rscript",infile,">>",outfile,"2>&1"))
          system(cmd,wait = TRUE)
          closeAllConnections()
          suppressWarnings(rm(cmd,infile,outfile)) 

          infile <- paste0('"',path,date,"/",RouteID[r],"/",RouteID[r],".R",'"')
          outfile <- paste0('"',"/home/eab/Projects/metrobus/logs/",date,"/",RouteID[r],"_updateR.log",'"')
          cmd <- as.character(paste("/opt/R/release/lib/R/bin/Rscript",infile,">>",outfile,"2>&1"))
          system(cmd,wait = TRUE)
          closeAllConnections()
          suppressWarnings(rm(cmd,infile,outfile)) 
        }
        else if (Sys.info()['sysname'] == "Windows") {
          infile <- paste0('"',path,date,"/",RouteID[r],"/",RouteID[r],".R",'"')
          outfile <- paste0('"',path,date,"/",RouteID[r],"/",RouteID[r],".log",'"')
          cmd <- as.character(paste("cmd","/c","C:/PROGRA~1/R/R-44~1.1/bin/Rscript.exe",infile,">>",outfile,"2>&1"))
          system(cmd,wait = TRUE)
          closeAllConnections()
          suppressWarnings(rm(cmd,infile,outfile)) 
        }
      }
      
      print(paste0(Sys.time(),": ",r," of ",length(RouteID)," // ",desc$IncidentShortDescription[r]))
      save.image()
      gc()
      r <- r+1
    }

    }

    else if ((nrow(bus_pos) > 0) & (nrow(bus_incidents) <= 0)) {
      
      if (Sys.info()['sysname'] == "Linux") {
        pg <- dbConnect(RPostgres::Postgres()
                        , host=Sys.getenv("pg_host")
                        , port=Sys.getenv("pg_port")
                        , dbname="wmata"
                        , user=Sys.getenv("pg_user")
                        , password=Sys.getenv("pg_password"))
        
        if (nrow(bus_pos) > 0) {
          
          bus_pos$trigger_timestamp <- Sys.time()
          dbWriteTable(pg,"bus_pos",bus_pos,row.names = FALSE, overwrite = TRUE, append = FALSE)   
          log <- data.frame(
            RouteID = 999,
            Source = "bus_pos",
            rows_added = nrow(bus_pos),
            StartTime = start_time,
            EndTime = end_time,
            trigger_timestamp = Sys.time())
          dbWriteTable(pg,"log",log, row.names = FALSE, append = TRUE)  
        }
        
        dbDisconnect(pg) 
        closeAllConnections()
    }
   }
 }

}
