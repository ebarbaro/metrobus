rm(list = ls(all.names = TRUE)) 
closeAllConnections()
start_time <- Sys.time()
print(paste0(Sys.getpid()," - Start time: ",start_time))

###
bus_pos <- GET("https://api.wmata.com/Bus.svc/json/jBusPositions", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  
    
{
    if (Sys.info()['sysname'] == "Linux") {
        bus_pos <- suppressWarnings(rbindlist(bus_pos,fill = TRUE)) 
    }
    else if (Sys.info()['sysname'] == "Windows") {
        bus_pos <- suppressWarnings(rbindlist(bus_pos$BusPositions,fill = TRUE)) 
    }
}

###
new_vehicles <- bus_pos[!duplicated(bus_pos$VehicleID),]

{
    if (nrow(new_vehicles)>0) {
        new_vehicles$Extendo <- NA
        new_vehicles$SeenTimes <- 1
        new_vehicles$TripInfo <- paste0(new_vehicles$RouteID," - ",new_vehicles$TripHeadsign," (",str_sub(new_vehicles$DirectionText,1,1),"B)")
        new_vehicles$TripEndTime <- gsub("T"," ",new_vehicles$TripEndTime)
        new_vehicles$TripEndTime <- ymd_hms(new_vehicles$TripEndTime, tz = Sys.timezone())
        new_vehicles$FirstSeen <- Sys.time()
        new_vehicles$LastSeen <- Sys.time()
        new_vehicles <- subset(new_vehicles, select=c(VehicleID,Extendo,SeenTimes,TripID,TripInfo,TripEndTime,FirstSeen,LastSeen))
        new_vehicles$trigger_timestamp <- Sys.time()
        pg <- dbConnect(RPostgres::Postgres()
                      , host=Sys.getenv("pg_host")
                      , port=Sys.getenv("pg_port")
                      , dbname="wmata"
                      , user=Sys.getenv("pg_user")
                      , password=Sys.getenv("pg_password"))
        dbWriteTable(pg,"buses",new_vehicles, row.names = FALSE, overwrite = TRUE, append = FALSE) 
        end_time <- Sys.time()
        log <- data.frame(
             RouteID = as.character("999"),
             Source = "buses",
             rows_added = nrow(new_vehicles),
             StartTime = start_time,
             EndTime = end_time,
             trigger_timestamp = Sys.time())
         dbWriteTable(pg,"log",log, row.names = FALSE, overwrite = TRUE, append = FALSE) 
        dbDisconnect(pg) 
        closeAllConnections()
        suppressWarnings(rm(log))
}
}

invisible(gc())
print(paste0(Sys.time(),": ",nrow(new_vehicles)," vehicles added in ",round(difftime(Sys.time(),start_time,units = "secs"))," secs (AND i took the trash out)."))

###
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

            pg <- dbConnect(RPostgres::Postgres()
                          , host=Sys.getenv("pg_host")
                          , port=Sys.getenv("pg_port")
                          , dbname="wmata"
                          , user=Sys.getenv("pg_user")
                          , password=Sys.getenv("pg_password"))
        end_time <- Sys.time()
        bus_incidents$StartTime <- start_time
        bus_incidents$EndTime <- end_time
        bus_incidents$trigger_timestamp <- Sys.time()
        dbWriteTable(pg,"bus_incidents",bus_incidents,row.names = FALSE, overwrite = FALSE, append = TRUE) 
        log <- data.frame(
             RouteID = as.character("999"),
             Source = "bus_incidents",
             rows_added = length(unique(bus_incidents$RoutesAffected)),
             StartTime = start_time,
             EndTime = end_time,
             trigger_timestamp = Sys.time())
        dbWriteTable(pg,"log",log, row.names = FALSE, overwrite = FALSE, append = TRUE)  
        suppressWarnings(rm(log))

###        
cmd <- cron_rscript(
                "/home/eab/Projects/metrobus/metrobus-vehicleID-loggeR.R",
                 rscript_log = "/home/eab/Projects/metrobus/logs/metrobus-vehicleID-loggeR.log",
                 cmd = file.path(Sys.getenv("R_HOME"), "bin", "Rscript"),
                 log_append = FALSE,
                 log_timestamp = FALSE)
cron_add(cmd, frequency = '*/10 * * * *', id = "metrobus-vehicleID-loggeR", description = "every-ten-mins-metrobus-vehicleID-loggeR",ask = FALSE)

rm(list = ls(all.names = TRUE)) 
closeAllConnections()

cmd <- cron_rscript(
                "/home/eab/Projects/metrobus/logs/schedule-updateR-continuity.R",
                 rscript_log = "/home/eab/Projects/metrobus/logs/schedule-updateR-continuity.log",
                 cmd = file.path(Sys.getenv("R_HOME"), "bin", "Rscript"),
                 log_append = FALSE,
                 log_timestamp = FALSE)
cron_add(cmd, frequency = '*/1 * * * *', id = "x", description = "schedule-updateR-continuity",ask = FALSE)

rm(list = ls(all.names = TRUE)) 
closeAllConnections()

wmata6x <- cron_rscript(
                "/home/eab/Projects/metrobus/DRAFT-schedule-pull-loop.R",
                 rscript_log = "/home/eab/Projects/metrobus/logs/schedule-saveR.log",
                 cmd = file.path(Sys.getenv("R_HOME"), "bin", "Rscript"),
                 log_append = FALSE,
                 log_timestamp = FALSE)
cron_add(
       wmata6x,
       description = "wmata-6x-daily-scrape",
       frequency = "0 */4 * * *",
       id = "wmata6x",
       ask = FALSE
     )

rm(list = ls(all.names = TRUE)) 
closeAllConnections()

cmd <- cron_rscript(
                "/home/eab/Projects/metrobus/cleaning/DRAFT-daily-scrub.R",
                 rscript_log = "/home/eab/Projects/metrobus/logs/daily-scrub.log",
                 cmd = file.path(Sys.getenv("R_HOME"), "bin", "Rscript"),
                 log_append = FALSE,
                 log_timestamp = FALSE)
cron_add(cmd, frequency = '0 3 * * *', id = "scrubbR", description = "daily-scrub",ask = FALSE)
