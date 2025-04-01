rm(list = ls(all.names = TRUE)) 
closeAllConnections()
start_time <- Sys.time()
print(paste0(Sys.getpid()," - Start time: ",start_time))

###
pg <- dbConnect(RPostgres::Postgres()
                      , host=Sys.getenv("pg_host")
                      , port=Sys.getenv("pg_port")
                      , dbname="wmata"
                      , user=Sys.getenv("pg_user")
                      , password=Sys.getenv("pg_password"))
###
buses_db <- dbGetQuery(pg,'select distinct * from public.buses')
dbWriteTable(pg,"buses_tmp",buses_db, row.names = FALSE, overwrite = TRUE, append = FALSE)  
dbDisconnect(pg) 
closeAllConnections()

###
buses_db$TripEndTime <- as.POSIXct(buses_db$TripEndTime, tz = Sys.timezone())
buses_db$FirstSeen <- as.POSIXct(buses_db$FirstSeen, tz = Sys.timezone())
buses_db$LastSeen <- as.POSIXct(buses_db$LastSeen, tz = Sys.timezone())
buses_db <- buses_db[!duplicated(buses_db$VehicleID),]

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
vehicles <- bus_pos[!duplicated(bus_pos$VehicleID),]

##
existing_vehicles <- inner_join(vehicles,buses_db,by="VehicleID")
missing_vehicles <- anti_join(buses_db,vehicles,by="VehicleID")

{
    if (nrow(existing_vehicles)>0) {
        existing_vehicles$TripID <- existing_vehicles$TripID.x
        existing_vehicles$SeenTimes <- ifelse((existing_vehicles$TripID.x == existing_vehicles$TripID.y), existing_vehicles$SeenTimes, (existing_vehicles$SeenTimes +1))
        existing_vehicles$TripInfo <- paste0(existing_vehicles$RouteID," - ",existing_vehicles$TripHeadsign," (",str_sub(existing_vehicles$DirectionText,1,1),"B)")
        existing_vehicles$TripEndTime <- gsub("T"," ",existing_vehicles$TripEndTime.x)
        existing_vehicles$TripEndTime <- ymd_hms(existing_vehicles$TripEndTime, tz = Sys.timezone())
        existing_vehicles$LastSeen <- Sys.time()
        existing_vehicles <- subset(existing_vehicles, select=c(VehicleID,Extendo,SeenTimes,TripID,TripInfo,TripEndTime,FirstSeen,LastSeen))
        existing_vehicles$trigger_timestamp <- Sys.time()
        existing_vehicles_b <- bind_rows(existing_vehicles,missing_vehicles)
        rm(existing_vehicles)
        existing_vehicles <- existing_vehicles_b[!duplicated(existing_vehicles_b$VehicleID),]
        rm(existing_vehicles_b)
        pg <- dbConnect(RPostgres::Postgres()
                      , host=Sys.getenv("pg_host")
                      , port=Sys.getenv("pg_port")
                      , dbname="wmata"
                      , user=Sys.getenv("pg_user")
                      , password=Sys.getenv("pg_password"))
        dbWriteTable(pg,"buses",existing_vehicles, row.names = FALSE, overwrite = TRUE, append = FALSE)  
        end_time <- Sys.time()
        log <- data.frame(
             RouteID = 999,
             Source = "buses_tmp",
             rows_added = nrow(existing_vehicles),
             StartTime = start_time,
             EndTime = end_time,
             trigger_timestamp = Sys.time())
        dbDisconnect(pg) 
        closeAllConnections()
        suppressWarnings(rm(log))
}
    else {
        suppressWarnings(rm(existing_vehicles))
    }
}

new_vehicles <- anti_join(vehicles,existing_vehicles,by="VehicleID")

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
        dbWriteTable(pg,"buses",new_vehicles, row.names = FALSE, overwrite = FALSE, append = TRUE) 
        end_time <- Sys.time()
        log <- data.frame(
             RouteID = 999,
             Source = "buses",
             rows_added = nrow(new_vehicles),
             StartTime = start_time,
             EndTime = end_time,
             trigger_timestamp = Sys.time())
        dbDisconnect(pg) 
        closeAllConnections()
        suppressWarnings(rm(log))
}
}

invisible(gc())
print(paste0(Sys.time(),": ",nrow(new_vehicles)," vehicles added + ",nrow(existing_vehicles)," updated (",nrow(new_vehicles)+nrow(existing_vehicles)," tot.) in ",round(difftime(Sys.time(),start_time,units = "secs"))," secs (AND i took the trash out)."))
