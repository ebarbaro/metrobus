rm(list = ls(all.names = TRUE)) 
cmd <- paste0("if ps -p ",Sys.getpid()," > /dev/null; then    echo '",Sys.getpid()," is running'; else     echo 'Restarting process...';  /opt/R/release/lib/R/bin/Rscript '/home/eab/Projects/metrobus/schedule-update.R' > '/home/eab/Projects/metrobus/logs/schedule-updateR.log' 2>&1 & fi")
write.table(cmd,"/home/eab/Projects/metrobus/gitignore/inputs/sys_cmd.txt")
suppressWarnings(rm(cmd))
closeAllConnections()
start_time <- Sys.time()
print(paste0(Sys.getpid()," - Start time: ",start_time))
date <- as.character(str_sub(Sys.time(),1,10))

{
  if (Sys.info()['sysname'] == "Linux") {
  setwd("/home/eab")
  wd <- "/home/eab"
  path <- "/home/eab/Projects/metrobus/"
  if (file.exists(paste0('"',"/home/eab/Projects/metrobus/logs/schedule-updateR.log",'"'))) {
       file.remove(paste0('"',"/home/eab/Projects/metrobus/logs/schedule-updateR.log",'"'))
  }
  }
  else if (Sys.info()['sysname'] == "Windows") {
    setwd("C:/Users/ebarbaro")
    wd <- "C:/Users/ebarbaro"
    path <- "C:/Users/ebarbaro/R/Sandbox/metrobus/"
  }
}

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
bus_incidents$IncidentLastUpdated <- with_tz(bus_incidents$IncidentLastUpdated, tzone = 'UTC')
bus_incidents_sub <- subset(bus_incidents,!(Description %like% "operator availability"))

  {
      inc <- subset(bus_incidents_sub,select=c(IncidentID))
      inc <- inc[!duplicated(inc$IncidentID),]
      inc$IncidentID <- paste0("'",inc$IncidentID,"',")
      inc_n <- split(inc, (seq(nrow(inc))-1) %/% 1)
      inc_n <- as.data.frame(inc_n)
      
      if (nrow(bus_incidents_sub)>1){
        inc_n$IncidentID <- apply( inc_n[ , names(inc_n) ] , 1 , paste , collapse = "" )
      }
      inc_n <- select(inc_n,
                      IncidentID)
      
      inc_n$IncidentID <- str_sub(inc_n$IncidentID, end = -2)
  }

###
pg <- dbConnect(RPostgres::Postgres()
                      , host=Sys.getenv("pg_host")
                      , port=Sys.getenv("pg_port")
                      , dbname="wmata"
                      , user=Sys.getenv("pg_user")
                      , password=Sys.getenv("pg_password"))

bi_db_x <- dbGetQuery(pg,paste0('select distinct b.* 
                                      from public.bus_incidents a
                                                    inner join public.bus_incidents b on a."IncidentID" = b."IncidentID" and a."DirectionAffected" = b."DirectionAffected" and b."trigger_timestamp" = (select b1."trigger_timestamp"
                                                                                                                    from public.bus_incidents b1
                                                                                                                    where b1."IncidentID" = b."IncidentID"
                                                                                                                        and b1."DirectionAffected" = b."DirectionAffected"
                                                                                                                   order by b1."trigger_timestamp" desc
                                                                                                                    limit 1)
                                      where b."IncidentID" in (',inc_n$IncidentID,')                                                                
                                      order by b."trigger_timestamp" desc'))

dbDisconnect(pg) 
closeAllConnections()
bi_db <- bi_db_x[!duplicated(bi_db_x$IncidentID),]

suppressWarnings(rm(inc,inc_n,pg,bi_db_x))
end_time <- Sys.time()

###
bi_api <- subset(bus_incidents_sub,select=c(IncidentID,IncidentLastUpdated))

not_updated_x <- inner_join(bi_db,bi_api,by="IncidentID")
not_updated <- subset(not_updated_x, IncidentLastUpdated.y == IncidentLastUpdated.x)
#print(paste0("bi_db$IncidentLastUpdated: ",bi_db$IncidentLastUpdated))
#print(paste0("not_updated$IncidentLastUpdated.y: ",not_updated$IncidentLastUpdated.y))
rm(not_updated_x)

{
  if (length(unique(not_updated$IncidentID))==nrow(bus_incidents_sub)) {

    update_stmt <- subset(not_updated,select=c(IncidentID))
    update_stmt$IncidentID <- paste0("'",update_stmt$IncidentID,"'")
    update_stmt$EndTime <- paste0("'",with_tz(end_time,tzone = 'UTC'),"'")
    update_stmt$trigger_timestamp <- paste0("'",with_tz(Sys.time(),tzone = 'UTC'),"'")
    u <- 1

    pg <- dbConnect(RPostgres::Postgres()
                      , host=Sys.getenv("pg_host")
                      , port=Sys.getenv("pg_port")
                      , dbname="wmata"
                      , user=Sys.getenv("pg_user")
                      , password=Sys.getenv("pg_password"))

    for (u in u:nrow(update_stmt)) {
      uu <- suppressWarnings(dbSendQuery(pg,paste0('update public."bus_incidents"
                                        set "EndTime" = ',update_stmt$EndTime[u],',
                                            "trigger_timestamp" = ',update_stmt$trigger_timestamp[u],'
                                   where "IncidentID" = ',update_stmt$IncidentID[u])))

      dbClearResult(uu)
      rm(uu)
      u <- u + 1
    }
    
    log <- data.frame(
             RouteID = 999,
             Source = "bus_incidents",
             rows_added = length(unique(not_updated$RoutesAffected))*-1,
             StartTime = start_time,
             EndTime = end_time,
             trigger_timestamp = Sys.time())
    dbWriteTable(pg,"log",log, row.names = FALSE, overwrite = FALSE, append = TRUE)  
    suppressWarnings(rm(log,start_time,end_time))

    print(paste0(Sys.time(),": Updated ",length(unique(not_updated$IncidentID))," existing incidents: "))
    print(not_updated$IncidentShortDescription)
    start_time <- Sys.time()

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

    if (nrow(bus_pos) > 0) {
      bus_pos$DateTime <- gsub("T"," ",bus_pos$DateTime)
      bus_pos$DateTime <- ymd_hms(bus_pos$DateTime, tz = 'UTC')
      bus_pos$TripStartTime <- gsub("T"," ",bus_pos$TripStartTime)
      bus_pos$TripStartTime <- ymd_hms(bus_pos$TripStartTime, tz = 'UTC')
      bus_pos$TripEndTime <- gsub("T"," ",bus_pos$TripEndTime)
      bus_pos$TripEndTime <- ymd_hms(bus_pos$TripEndTime, tz = 'UTC')
      bus_pos <- bus_pos %>%
                 st_as_sf(coords = c("Lon","Lat")) %>%
                 st_set_crs(4326)
      bus_pos$geometry <- gsub("POINT \\(","",bus_pos$geometry)
      bus_pos$geometry <- trimws(bus_pos$geometry)
      bus_pos <- as.data.frame(bus_pos)
      bus_pos$trigger_timestamp <- Sys.time()
      end_time <- Sys.time() 
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
    print(paste0(Sys.time(),": Fetched ",nrow(bus_pos)," updated bus positions. Process done in ",ceiling(difftime(Sys.time(),start_time,units = "secs"))," secs"))
    invisible(gc())
  }

  else if ((nrow(bi_db)==0)|(length(unique(not_updated$IncidentID))<(nrow(bus_incidents_sub)))) {

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

  {
    if (nrow(bus_pos) > 0) {
      bus_pos$DateTime <- gsub("T"," ",bus_pos$DateTime)
      bus_pos$DateTime <- ymd_hms(bus_pos$DateTime, tz = 'UTC')
      bus_pos$TripStartTime <- gsub("T"," ",bus_pos$TripStartTime)
      bus_pos$TripStartTime <- ymd_hms(bus_pos$TripStartTime, tz = 'UTC')
      bus_pos$TripEndTime <- gsub("T"," ",bus_pos$TripEndTime)
      bus_pos$TripEndTime <- ymd_hms(bus_pos$TripEndTime, tz = 'UTC')
      bus_pos <- bus_pos %>%
                 st_as_sf(coords = c("Lon","Lat")) %>%
                 st_set_crs(4326) 
      bus_pos$geometry <- gsub("POINT \\(","",bus_pos$geometry)
      bus_pos$geometry <- trimws(bus_pos$geometry)
      bus_pos <- as.data.frame(bus_pos)        
      bus_pos$trigger_timestamp <- Sys.time()
      end_time <- Sys.time()

      pg <- dbConnect(RPostgres::Postgres()
                        , host=Sys.getenv("pg_host")
                        , port=Sys.getenv("pg_port")
                        , dbname="wmata"
                        , user=Sys.getenv("pg_user")
                        , password=Sys.getenv("pg_password"))   

      dbWriteTable(pg,"bus_pos",bus_pos,row.names = FALSE, overwrite = TRUE, append = FALSE)   
      log <- data.frame(
             RouteID = 999,
             Source = "bus_pos",
             rows_added = nrow(bus_pos),
             StartTime = start_time,
             EndTime = end_time,
             trigger_timestamp = Sys.time())
       dbWriteTable(pg,"log",log, row.names = FALSE, append = TRUE)  
      suppressWarnings(rm(log)) 
      dbDisconnect(pg) 
      closeAllConnections()
      }
  }

    lines <- subset(bus_pos,select=c(RouteID,DirectionText))
    lines <- lines[!duplicated(lines[,c("RouteID","DirectionText")]),]

    ##
    bus_incidents_sub_j <- left_join(bus_incidents_sub,lines,by=c("RoutesAffected"="RouteID"),relationship = "many-to-many")
    rm(bus_incidents_sub,bus_incidents)
    bus_incidents_sub_x <- bus_incidents_sub_j %>%
                                group_by(RoutesAffected) %>%
                                filter(!n()>1)
    
    {                           
      if (nrow(bus_incidents_sub_x)>=1) {
      bus_incidents_sub_x$DirectionText <- ifelse((bus_incidents_sub_x$DirectionText == "NORTH"),"SOUTH",
                                            ifelse((bus_incidents_sub_x$DirectionText == "SOUTH"),"NORTH",
                                             ifelse((bus_incidents_sub_x$DirectionText == "EAST"),"WEST", 
                                              ifelse((bus_incidents_sub_x$DirectionText == "WEST"),"EAST",NA))))
      
      bus_incidents_sub <- rbind(bus_incidents_sub_j,bus_incidents_sub_x)
      rm(bus_incidents_sub_x,bus_incidents_sub_j)
      }
      else {
        bus_incidents_sub <- bus_incidents_sub_j
        rm(bus_incidents_sub_x,bus_incidents_sub_j)
      }
     }
    
    bus_incidents_sub$DirectionAffected <- ifelse((grepl("Northbound|northbound",bus_incidents_sub$Description)&!grepl("NORTH",bus_incidents_sub$DirectionText)),NA,
                                            ifelse((grepl("Southbound|southbound",bus_incidents_sub$Description)&!grepl("SOUTH",bus_incidents_sub$DirectionText)),NA,
                                             ifelse((grepl("Eastbound|eastbound",bus_incidents_sub$Description)&!grepl("EAST",bus_incidents_sub$DirectionText)),NA,
                                              ifelse((grepl("Westbound|westbound",bus_incidents_sub$Description)&!grepl("WEST",bus_incidents_sub$DirectionText)),NA,bus_incidents_sub$DirectionText))))
    bus_incidents_sub$normDirectionAffected <- ifelse((!is.na(bus_incidents_sub$DirectionAffected)),bus_incidents_sub$DirectionAffected,
                                                ifelse((is.na(bus_incidents_sub$DirectionAffected)&grepl("Northbound|northbound",bus_incidents_sub$Description)),"NORTH",
                                                 ifelse((is.na(bus_incidents_sub$DirectionAffected)&grepl("Southbound|southbound",bus_incidents_sub$Description)),"SOUTH",
                                                  ifelse((is.na(bus_incidents_sub$DirectionAffected)&grepl("Eastbound|eastbound",bus_incidents_sub$Description)),"EAST",
                                                   ifelse((is.na(bus_incidents_sub$DirectionAffected)&grepl("Westbound|westbound",bus_incidents_sub$Description)),"WEST",NA)))))
    bus_incidents_sub$IncidentType <- ifelse((bus_incidents_sub$Description %like% "operator availability"),"Notice",
                                       ifelse((bus_incidents_sub$Description %like% "detour"),paste0("Detour (",str_sub(bus_incidents_sub$normDirectionAffected,1,1),"B)"),
                                        ifelse((bus_incidents_sub$Description %like% "delay"),paste0("Delay (",str_sub(bus_incidents_sub$normDirectionAffected,1,1),"B)"),bus_incidents_sub$IncidentType)))
    bus_incidents_sub$IncidentDescription <- bus_incidents_sub$Description
    bus_incidents_sub$IncidentShortDescription <- paste0(gsub("\\..*","",bus_incidents_sub$Description),".") 
    bus_incidents_sub <- bus_incidents_sub[!is.na(bus_incidents_sub$normDirectionAffected),]
    bus_incidents_sub <- bus_incidents_sub[!duplicated(bus_incidents_sub[,c("normDirectionAffected","IncidentID")]),]
    bus_incidents <- subset(bus_incidents_sub,select=c(RoutesAffected,normDirectionAffected,IncidentType,IncidentShortDescription,IncidentDescription,IncidentID,IncidentLastUpdated))
    names(bus_incidents)[2]<-"DirectionAffected"

    existing <- inner_join(bi_db,bi_api,by="IncidentID")
    new_incidents <- anti_join(bus_incidents,existing,by="IncidentID") 

    {
      if (nrow(new_incidents)>0) {
        pg <- dbConnect(RPostgres::Postgres()
                          , host=Sys.getenv("pg_host")
                          , port=Sys.getenv("pg_port")
                          , dbname="wmata"
                          , user=Sys.getenv("pg_user")
                          , password=Sys.getenv("pg_password"))
        end_time <- Sys.time()
        new_incidents$StartTime <- start_time
        new_incidents$EndTime <- end_time
        new_incidents$trigger_timestamp <- Sys.time()
        dbWriteTable(pg,"bus_incidents",new_incidents,row.names = FALSE, overwrite = FALSE, append = TRUE) 
        log <- data.frame(
             RouteID = 999,
             Source = "bus_incidents",
             rows_added = length(unique(new_incidents$RoutesAffected)),
             StartTime = start_time,
             EndTime = end_time,
             trigger_timestamp = Sys.time())
        dbWriteTable(pg,"log",log, row.names = FALSE, overwrite = FALSE, append = TRUE)  
        suppressWarnings(rm(log))
      }
      else {
        pg <- dbConnect(RPostgres::Postgres()
                          , host=Sys.getenv("pg_host")
                          , port=Sys.getenv("pg_port")
                          , dbname="wmata"
                          , user=Sys.getenv("pg_user")
                          , password=Sys.getenv("pg_password"))        
      }
    }

    update_stmt <- subset(existing,select=c(IncidentID))
    update_stmt <- inner_join(update_stmt,bus_incidents,by="IncidentID")
    update_stmt$IncidentID <- paste0("'",update_stmt$IncidentID,"'")
    update_stmt$RoutesAffected <- paste0("'",update_stmt$RoutesAffected,"'")
    update_stmt$DirectionAffected <- paste0("'",update_stmt$DirectionAffected,"'")
    update_stmt$IncidentType <- paste0("'",update_stmt$IncidentType,"'")
    update_stmt$IncidentShortDescription <- paste0("'",update_stmt$IncidentShortDescription,"'")
    update_stmt$IncidentDescription <- paste0("'",update_stmt$IncidentDescription,"'")
    update_stmt$IncidentLastUpdated <- paste0("'",update_stmt$IncidentLastUpdated,"'")
    update_stmt$EndTime <- paste0("'",end_time,"'")
    update_stmt$trigger_timestamp <- paste0("'",Sys.time(),"'")
    u <- 1

    {
      for (u in u:nrow(update_stmt)) {
        uu <- suppressWarnings(dbSendQuery(pg,paste0('update public."bus_incidents"
                                          set "RoutesAffected" = ',update_stmt$RoutesAffected[u],',
                                              "DirectionAffected" = ',update_stmt$DirectionAffected[u],',
                                              "IncidentType" = ',update_stmt$IncidentType[u],',
                                              "IncidentShortDescription" = ',update_stmt$IncidentShortDescription[u],',
                                              "IncidentDescription" = ',update_stmt$IncidentDescription[u],',
                                              "IncidentLastUpdated" = ',update_stmt$IncidentLastUpdated[u],',
                                              "EndTime" = ',update_stmt$EndTime[u],',
                                              "trigger_timestamp" = ',update_stmt$trigger_timestamp[u],'
                                     where "IncidentID" = ',update_stmt$IncidentID[u])))
        dbClearResult(uu)
        rm(uu)
        u <- u + 1
      }
    }

    print(paste0(Sys.time(),": ",length(unique(existing$IncidentID))," record(s) updated: "))
    print(existing$IncidentShortDescription)
    suppressWarnings(rm(log)) 
    dbDisconnect(pg) 
    closeAllConnections()

    updated <- subset(existing, IncidentLastUpdated.y > IncidentLastUpdated.x)
    u_routes <- bind_rows(new_incidents,updated)
    xu_routes <- anti_join(u_routes,not_updated,by=c("IncidentID","DirectionAffected"))
    rm(u_routes)
    xu_routes <- xu_routes[order(xu_routes$RoutesAffected, decreasing = FALSE),]
    RouteID <- xu_routes[!duplicated(xu_routes$RoutesAffected),]
    RouteID <- subset(RouteID, select=c(RoutesAffected))
    names(RouteID)<-"RouteID"
    RouteID <- as.array(RouteID$RouteID)
    
    desc <- xu_routes %>% group_by(RoutesAffected) %>%
                              arrange(desc(IncidentLastUpdated)) %>%
                              filter(row_number() == 1)
    desc <- desc[order(desc$RoutesAffected, decreasing = FALSE),]
    date <- as.character(str_sub(Sys.time(),1,10))

    suppressWarnings(rm(bi_api,bi_db,bus_incidents,bus_incidents_sub,bus_pos,existing,lines,new_incidents,not_updated,pg,xu_routes,updated))

    r <- 1

if (!dir.exists(paste0("/home/eab/Projects/metrobus/logs/",date))) { dir.create(paste0("/home/eab/Projects/metrobus/logs/",date)) }

  for (r in r:length(RouteID)) {
     {
        if (Sys.info()['sysname'] == "Linux") {
          setwd("/home/eab")
          wd <- "/home/eab"
          path <- "/home/eab/Projects/metrobus/"
        }
        else if (Sys.info()['sysname'] == "Windows") {
          setwd("C:/Users/ebarbaro")
          wd <- "C:/Users/ebarbaro"
          path <- "C:/Users/ebarbaro/R/Sandbox/metrobus/"
        }
      }
 
      if (!dir.exists(paste0(path,"/logs/Errors/",date))) { dir.create(paste0(path,"/logs/Errors/",date)) }

      closeAllConnections()
      input <- data.frame(
        input1 = as.character(paste0("setwd(",'"',wd,'"',")")),
        input2 = as.character(paste0("print(Sys.time())")),
        input3 = as.character(paste0("print(Sys.getpid())")),
        input6 = as.character(paste0("print('schedule-update.R')")),
        input4 = as.character("rm(list = ls(all.names = TRUE))"),
        input5 = as.character(paste0("RouteID <- ",'"',RouteID[r],'"')))
      
      input$input <- paste(input$input1,input$input2,input$input3,input$input6,input$input4,input$input5,sep = "\n")
      input <- as.character(input$input)
      cat(input, file = paste0(path,"inputs/input.txt"),sep = "\n")
      {
        if (Sys.info()['sysname'] == "Linux") {
          files <- list.files(paste0(path,"inputs"),pattern = "\\input.txt$|\\linux.txt$")
        }
        else if (Sys.info()['sysname'] == "Windows") {
          files <- list.files(paste0(path,"inputs"),pattern = "\\input.txt$|\\windows.txt$")
        }
      }
      files <- as.data.frame(files)
      files <- files[order(-xtfrm(files$files), decreasing = FALSE),]
      outFile <- file(paste0(path,"/logs/Errors/",date,"/",RouteID[r],".R"), "w")
      f <- 1
      
      for (f in 1:length(files)){
        x <- suppressWarnings(readLines(paste0(path,"inputs/",files[f])))
        writeLines(x, outFile)
        f <- f+1
      }
      close(outFile)
      rm(outFile,x,f,files,input) 

      file.copy(paste0(path,"/logs/Errors/",date,"/",RouteID[r],".R"),"/home/eab/Projects/metrobus/logs/Errors", overwrite = TRUE) 

      {
        if (Sys.info()['sysname'] == "Linux") {

          infile <- paste0('"',"/home/eab/Projects/metrobus/bus-position-save.R",'"')
          outfile <- paste0('"',"/home/eab/Projects/metrobus/logs/bus-position-saveR.log",'"')
          cmd <- as.character(paste0("/opt/R/release/lib/R/bin/Rscript ",infile," > ",outfile," 2>&1"))
          system(cmd,wait = TRUE)
          closeAllConnections()
          suppressWarnings(rm(cmd,infile,outfile)) 

          infile <- paste0('"',path,"/logs/Errors/",date,"/",RouteID[r],".R",'"')
          outfile <- paste0('"',path,"/logs/Errors/",date,"/",RouteID[r],".log",'"')
          cmd <- as.character(paste0("/opt/R/release/lib/R/bin/Rscript ",infile," >> ",outfile," 2>&1"))
          system(cmd,wait = TRUE)
          closeAllConnections()
          suppressWarnings(rm(cmd,infile,outfile)) 
        }
        else if (Sys.info()['sysname'] == "Windows") {
          infile <- paste0('"',path,"/logs/Errors/",date,"/",RouteID[r],".R",'"')
          outfile <- paste0('"',path,"/logs/Errors/",date,"/",RouteID[r],".log",'"')
          cmd <- as.character(paste0("cmd","/c","C:/PROGRA~1/R/R-44~1.1/bin/Rscript.exe ",infile," > ",outfile," 2>&1 &"))
          system(cmd,wait = TRUE)
          closeAllConnections()
          suppressWarnings(rm(cmd,infile,outfile)) 
        }
      }
      print(paste0(Sys.time(),": ",r," of ",length(RouteID)," // ",desc$IncidentShortDescription[r]))
      save.image()
      invisible(gc())
      r <- r+1            
    }
    closeAllConnections()
  }
} 
