rm(list = ls(all.names = TRUE)) 
closeAllConnections()
start_time <- Sys.time()
print(paste0(Sys.getpid()," - Start time: ",start_time))
date <- as.character(str_sub(Sys.time(),1,10))

{
  if (Sys.info()['sysname'] == "Linux") {
  setwd("/home/eab")
  wd <- "/home/eab"
  path <- "/home/eab/Projects/metrobus/"
  if (file.exists(paste0('"',"/home/eab/Projects/metrobus/logs/schedule-saveR.log",'"'))) {
    file.remove(paste0('"',"/home/eab/Projects/metrobus/logs/schedule-saveR.log",'"'))
  }
 } 
  else if (Sys.info()['sysname'] == "Windows") {
    setwd("C:/Users/ebarbaro")
    wd <- "C:/Users/ebarbaro"
    path <- "C:/Users/ebarbaro/R/Sandbox/metrobus/"
  }
}


## Routes (Returns a list of all bus route variants. For example, the 10A and 10Av1 are the same route, but may stop at slightly different locations.)
routes <- GET("https://api.wmata.com/Bus.svc/json/jRoutes", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  

#
route <- data.frame()
i <- 1

{
  if (Sys.info()['sysname'] == "Linux") {
    
    for (i in i:nrow(routes$Routes)) {
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
        RouteID = coalesce(routes[["Routes"]][[i]][["RouteID"]],NA),
        Name = coalesce(routes[["Routes"]][[i]][["Name"]],NA),
        LineDescription = coalesce(routes[["Routes"]][[i]][["LineDescription"]],NA))
      
      route <- rbind(route,routes_stg)
      rm(routes_stg)
    }
  }
}

suppressWarnings(rm(i,routes))

route$LineKey <- sub("\\-.*", "", route$Name)
route$LineKey <- trimws(route$LineKey)
route$Dir <- gsub(".*[-]([^.]+)[-].*", "\\1", route$Name)
route$Dir <- trimws(route$Dir)
routes <- route[!grepl("C29|X3",route$RouteID),]
suppressWarnings(rm(route))
route <- routes

l <- 1
RouteID <- array()
not_added <- array()
for (l in l:nrow(route)) {
  schedule <- GET(paste0("https://api.wmata.com/Bus.svc/json/jRouteSchedule?RouteID=",route$RouteID[l],"&Date=",Sys.Date(),"&IncludingVariations=true"), add_headers("api_key" = Sys.getenv("wmata_key2"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()
  path_deets <- GET(paste0("https://api.wmata.com/Bus.svc/json/jRouteDetails?RouteID=",route$RouteID[l],"&Date=",Sys.Date()), add_headers("api_key" = Sys.getenv("wmata_key2"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON() 
{  
  if (is.list(path_deets)&is.list(schedule))
  {
    RouteID <- c(RouteID,route$RouteID[l])
  }
  else {
    not_added <- c(not_added,route$RouteID[l])
  }
  }
  rm(schedule,path_deets)
  #print(l) ## remove this print when you push this
l <- l+1
}

RouteID <- as.data.frame(RouteID)
sch_routes <- inner_join(route,RouteID,by="RouteID")

sch_routes <- sch_routes[!duplicated(sch_routes$LineKey),]
RouteID  <- subset(sch_routes,select=c("LineKey"))
names(RouteID)[1]<-"RouteID"
RouteID <- as.array(RouteID$RouteID)

suppressWarnings(rm(sch_routes,l,routes,not_added))

date <- as.character(str_sub(Sys.time(),1,10))
r <- 1

if (!dir.exists(paste0(path,"/logs/Errors/",date))) { dir.create(paste0(path,"/logs/Errors/",date)) }
 
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

  #for (r in 1) {  
  #if (!dir.exists(paste0(path,"/routes/",date,"/",RouteID[r]))) { dir.create(paste0(path,"/routes/",date,"/",RouteID[r])) }
  
     closeAllConnections()
      input <- data.frame(
        input1 = as.character(paste0("setwd(",'"',wd,'"',")")),
        input2 = as.character(paste0("print(Sys.time())")),
        input3 = as.character(paste0("print(Sys.getpid())")),
        input6 = as.character(paste0("print('schedule-save.R')")),
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
      cmd <- as.character(paste("/opt/R/release/lib/R/bin/Rscript",infile," > ",outfile,"2>&1"))
      system(cmd,wait = TRUE)
      closeAllConnections()
      suppressWarnings(rm(cmd,infile,outfile)) 
      infile <- paste0('"',path,"/logs/Errors/",date,"/",RouteID[r],".R",'"')
      outfile <- paste0('"',path,"/logs/Errors/",date,"/",RouteID[r],".log",'"')
      cmd <- as.character(paste("/opt/R/release/lib/R/bin/Rscript",infile," >> ",outfile,"2>&1"))
      system(cmd,wait = TRUE)
      closeAllConnections()
      suppressWarnings(rm(cmd,infile,outfile)) 
    }
    else if (Sys.info()['sysname'] == "Windows") {
      infile <- paste0('"',path,"/logs/Errors/",date,"/",RouteID[r],".R",'"')
      outfile <- paste0('"',path,"/logs/",RouteID[r],".log",'"')
      cmd <- as.character(paste("cmd","/c","C:/PROGRA~1/R/R-44~1.1/bin/Rscript.exe",infile,">",outfile,"2>&1"))
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
    print(paste0(Sys.time(),": Saved ",length(RouteID)," routes in ",ceiling(difftime(Sys.time(),start_time,units = "mins"))," mins"))
    i <- 1
    for (i in i:length(RouteID)) {
      
      if (file.exists(paste0(path,"/logs/Errors/",date,"/",RouteID[i],".R"))) { file.remove(paste0(path,"/logs/Errors/",date,"/",RouteID[i],".R")) }
      i <- i + 1
      
    }
    
      print(paste0(Sys.time(),": Removed ",i," of ",length(RouteID)," files"))

    {
      if (Sys.info()['sysname'] == "Linux") {
        route_stmt <- as.character(route_stmt)
        stops_stmt <- as.character(stops_stmt)
        sched_stmt <- as.character(sched_stmt)
        cat(route_stmt, file = paste0(path,"/gitignore/sql/route_stmt.sql"),sep = "\n")
        cat(stops_stmt, file = paste0(path,"/gitignore/sql/stops_stmt.sql"),sep = "\n")
        cat(sched_stmt, file = paste0(path,"/gitignore/sql/sched_stmt.sql"),sep = "\n")
        
      }
      else if (Sys.info()['sysname'] == "Windows") {
        folders <- list.files(paste0(path,"/gitignore/images"))
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
          
          files <- list.files(paste0(path,"/gitignore/images/",folders[x,]))
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
    #unlink(paste0(path,date), recursive = TRUE)
    suppressWarnings(rm(i))
  }
  save.image()
  invisible(gc())
  r <- r+1
}
