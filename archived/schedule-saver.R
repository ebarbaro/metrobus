rm(list = ls(all.names = TRUE)) 
closeAllConnections()
start_time <- Sys.time()
print(paste0("Start time: ",start_time))

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

## Routes (Returns a list of all bus route variants. For example, the 10A and 10Av1 are the same route, but may stop at slightly different locations.) # nolint
routes <- GET("https://api.wmata.com/Bus.svc/json/jRoutes", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()   # nolint

#
route <- data.frame()
i <- 1

{
  if (Sys.info()['sysname'] == "Linux") {
    for (i in i:length(routes[["Routes"]][["RouteID"]])) {
      routes_stg <- data.frame(
        RouteID = coalesce(routes[["Routes"]][["RouteID"]][[i]],NA),
        Name = coalesce(routes[["Routes"]][["Name"]][[i]],NA),
        LineDescription = coalesce(routes[["Routes"]][["LineDescription"]][[i]],NA))
      
      route <- rbind(route,routes_stg)
      rm(routes_stg)
    }
  }
  else if (Sys.info()['sysname'] == "Windows") {
    for (i in i:length(routes$Routes)) {
      routes_stg <- data.frame(
        RouteID = coalesce(routes[["Routes"]][[i]][["RouteID"]],NA), # nolint
        Name = coalesce(routes[["Routes"]][[i]][["Name"]],NA),
        LineDescription = coalesce(routes[["Routes"]][[i]][["LineDescription"]],NA))
      
      route <- rbind(route,routes_stg)
      rm(routes_stg)
    }
  }
}

suppressWarnings(rm(i))

route$LineKey <- sub("\\-.*", "", route$Name)
route$LineKey <- trimws(route$LineKey)
route$Dir <- gsub(".*[-]([^.]+)[-].*", "\\1", route$Name)
route$Dir <- trimws(route$Dir)

## bus position (Returns bus positions for the given route. If no parameters are specified, all bus positions are returned. Bus positions are refreshed approximately every 7 to 10 seconds.)
bus_pos <- GET("https://api.wmata.com/Bus.svc/json/jBusPositions", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  

{
  if (Sys.info()['sysname'] == "Linux") {
    bus_pos <- suppressWarnings(rbindlist(bus_pos,fill = TRUE)) 
    bus_pos <- bus_pos[!grepl("EMPLOYEE SHUTTLE",bus_pos$TripHeadsign),]
    
    missing_routes <- anti_join(bus_pos,route,by=c("RouteID"))
    
    if (nrow(missing_routes) > 0) {
      error_log <- data.frame(
        RouteID = missing_routes$RouteID,
        Direction = missing_routes$DirectionText,
        TripHeadsign = missing_routes$TripHeadsign,
        Source = "missing_routes",
        trigger_timestamp = Sys.time())
      
      bus_pos <- anti_join(bus_pos,missing_routes,by=c("RouteID"))
      
      pg <- dbConnect(RPostgres::Postgres()
                      , host=Sys.getenv("pg_host")
                      , port=Sys.getenv("pg_port")
                      , dbname="wmata"
                      , user=Sys.getenv("pg_user")
                      , password=Sys.getenv("pg_password"))
      dbWriteTable(pg,error_log,error_log,row.names = FALSE, append = TRUE)                       
      dbDisconnect(pg) 
    }
  }
  else if (Sys.info()['sysname'] == "Windows") {
    bus_pos <- suppressWarnings(rbindlist(bus_pos$BusPositions,fill = TRUE)) 
    bus_pos <- bus_pos[!grepl("EMPLOYEE SHUTTLE",bus_pos$TripHeadsign),]
    
    missing_routes <- anti_join(bus_pos,route,by=c("RouteID"))
    
    if (nrow(missing_routes) > 0) {
      bus_pos <- anti_join(bus_pos,missing_routes,by=c("RouteID"))
    }
  }
}

suppressWarnings(rm(missing_routes,route,routes))
bus_pos <- bus_pos[order(bus_pos$RouteID, decreasing = FALSE),]
RouteID <- bus_pos[!duplicated(bus_pos$RouteID),]
RouteID <- subset(RouteID, select=c(RouteID))
RouteID <- as.array(RouteID$RouteID)
date <- as.character(str_sub(Sys.time(),1,10))
r <- 1

if (!dir.exists(paste0(path,date))) { dir.create(paste0(path,date)) }
    if (!dir.exists(paste0("/home/eab/Projects/metrobus/logs/",date))) { dir.create(paste0("/home/eab/Projects/metrobus/logs/",date)) }

  
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
  
  {
    if (Sys.info()['sysname'] == "Linux") {
      infile <- paste0('"',"/home/eab/Projects/metrobus/bus-position-saver.R",'"')
      outfile <- paste0('"',"/home/eab/Projects/metrobus/logs/",date,"/","bus-position-saver.log",'"')
      cmd <- as.character(paste("/opt/R/release/lib/R/bin/Rscript",infile,">>",outfile,"2>&1"))
      system(cmd,wait = TRUE)
      closeAllConnections()
      suppressWarnings(rm(cmd,infile,outfile)) 
      infile <- paste0('"',path,date,"/",RouteID[r],"/",RouteID[r],".R",'"')
      outfile <- paste0('"',"/home/eab/Projects/metrobus/logs/",date,"/",RouteID[r],"_schedule-saveR.log",'"')
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
      Sys.sleep(1)
      closeAllConnections()
      suppressWarnings(rm(cmd,infile,outfile)) 
    }
  }
  
if (Sys.info()['sysname'] == "Linux") {    
  {
    if (r == 1) {
      rs <- paste0("public.",'"routes_',RouteID[r],'"')
      ss <- paste0("public.",'"stops_',RouteID[r],'"')
      scs <- paste0("public.",'"sched_',RouteID[r],'"')
      route_stmt <- as.data.frame(as.character(paste("select * from ",rs)))
      names(route_stmt)[1]<-"r_stmt"
      stops_stmt <- as.data.frame(as.character(paste("select * from ",ss)))
      names(stops_stmt)[1]<-"s_stmt"
      sched_stmt <- as.data.frame(as.character(paste("select * from ",scs)))
      names(sched_stmt)[1]<-"s_stmt"
      suppressWarnings(rm(rs,ss,scs))
    }
    else if (r > 1) {
      rs <- paste0("public.",'"routes_',RouteID[r],'"')
      ss <- paste0("public.",'"stops_',RouteID[r],'"')
      scs <- paste0("public.",'"sched_',RouteID[r],'"')
      route_stmt$r_stmt <- as.character(paste(route_stmt$r_stmt,"\n"," UNION ALL select * from ",rs))
      stops_stmt$s_stmt <- as.character(paste(stops_stmt$s_stmt,"\n"," UNION ALL select * from ",ss))
      sched_stmt$s_stmt <- as.character(paste(sched_stmt$s_stmt,"\n"," UNION ALL select * from ",scs))
      suppressWarnings(rm(rs,ss,scs))
    }
  }
}
 print(paste0(Sys.time(),": ",RouteID[r],": processed ",r," of ",length(RouteID)))
  if (r == length(RouteID)) {
    print(paste0(Sys.time(),": Saved ",length(RouteID)," routes in ",difftime(Sys.time(),start_time,units = "secs")," secs"))
    i <- 1
    for (i in i:length(RouteID)) {
      
      if (dir.exists(paste0(path,date,"/",RouteID[i]))) { unlink(paste0(path,date,"/",RouteID[i]), recursive = TRUE) }
      print(paste0("Removed ",path,date,"/",RouteID[i]))
      i <- i + 1
      
    }
    
    {
      if (Sys.info()['sysname'] == "Linux") {
        route_stmt <- as.character(route_stmt)
        stops_stmt <- as.character(stops_stmt)
        sched_stmt <- as.character(sched_stmt)
        cat(route_stmt, file = paste0(path,"route_stmt.sql"),sep = "\n")
        cat(stops_stmt, file = paste0(path,"stops_stmt.sql"),sep = "\n")
        cat(sched_stmt, file = paste0(path,"sched_stmt.sql"),sep = "\n")

        Sys.sleep(120)

        pg <- dbConnect(RPostgres::Postgres()
                      , host=Sys.getenv("pg_host")
                      , port=Sys.getenv("pg_port")
                      , dbname="wmata"
                      , user=Sys.getenv("pg_user")
                      , password=Sys.getenv("pg_password"))

        res <- dbGetQuery(pg, statement = read_file("/home/eab/Projects/metrobus/sql/bus_pos_check.sql"))
        dbDisconnect(pg) 
        closeAllConnections()
        if (difftime(Sys.time(),max(res$trigger_timestamp),units = "secs")>59) {
            infile <- paste0('"',"/home/eab/Projects/metrobus/schedule-updater.R",'"')
            outfile <- paste0('"',"/home/eab/Projects/metrobus/logs/",date,"/","schedule-updater.log",'"')
            cmd <- as.character(paste("/opt/R/release/lib/R/bin/Rscript",infile,">>",outfile,"2>&1"))
            system(cmd,wait = FALSE)
            closeAllConnections()
            suppressWarnings(rm(cmd,infile,outfile,res)) 
        }
        
      }
      else if (Sys.info()['sysname'] == "Windows") {
        folders <- list.files(paste0(path,"images"))
        folders <- subset(folders, folders != "workspaces")
        folders <- as.data.frame(folders)
        folders <- folders[order(-xtfrm(folders$folders), decreasing = FALSE),]
        folders <- as.data.frame(folders)
        
        pdr <- data.frame()
        sd  <- data.frame()
        ss <- data.frame()
        
        x <- 1
        i <- 1
        
        for (x in 1:nrow(folders)) {
          
          files <- list.files(paste0(path,"images/",folders[x,]))
          files <- as.data.frame(files)
          files <- files[order(-xtfrm(files$files), decreasing = FALSE),]
          files <- as.data.frame(files)
          
          for (i in 1:nrow(files)) {
           { 
             if (folders[x,] == "stops") {
             print(paste0("Binding ",i," to ",folders[x,]))   
             pdr <- bind_rows(pdr,path_deets_routes)
             rm(path_deets_routes)
             }
             if (folders[x,] == "schedule") {
               print(paste0("Binding ",i," to ",folders[x,]))   
               sd <- bind_rows(sd,scheduled_departures)
               rm(scheduled_departures)
             }
             if (folders[x,] == "routes") {
               print(paste0("Binding ",i," to ",folders[x,]))   
               ss <- bind_rows(ss,scheduled_stops)
               rm(scheduled_stops)
             }
            }
            i <- i + 1
              }
          i <- 1
          x <- x + 1
        }
      }
    }
    unlink(paste0(path,date), recursive = TRUE)
    suppressWarnings(rm(i))
  }
  save.image()
  gc()
  r <- r+1
}
