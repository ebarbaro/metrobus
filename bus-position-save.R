setwd("/home/eab")
rm(list = ls(all.names = TRUE)) 
closeAllConnections()
start_time <- Sys.time()
print(paste0(Sys.getpid()," - Start time: ",start_time))
date <- as.character(str_sub(Sys.time(),1,10))

if (!dir.exists(paste0("/home/eab/Projects/metrobus/logs/",date))) { dir.create(paste0("/home/eab/Projects/metrobus/logs/",date)) }

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

## bus position (Returns bus positions for the given route. If no parameters are specified, all bus positions are returned. Bus positions are refreshed approximately every 7 to 10 seconds.)
bus_pos <- GET("https://api.wmata.com/Bus.svc/json/jBusPositions", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  
  
{
    if (Sys.info()['sysname'] == "Linux") {
        bus_pos <- suppressWarnings(rbindlist(bus_pos,fill = TRUE)) 
    }
    else if (Sys.info()['sysname'] == "Windows") {
        bus_pos <- suppressWarnings(rbindlist(bus_pos$BusPositions,fill = TRUE))    
    }
}

pg <- dbConnect(RPostgres::Postgres()
		                      , host=Sys.getenv("pg_host")
		                      , port=Sys.getenv("pg_port")
		                      , dbname="wmata"
		                      , user=Sys.getenv("pg_user")
		                      , password=Sys.getenv("pg_password"))

save.image("/home/eab/Projects/metrobus/gitingnore/images/workspaces/bus_pos.RData")

{
	if (nrow(bus_pos) > 0) {
	  bus_pos <- bus_pos[!grepl("EMPLOYEE SHUTTLE",bus_pos$TripHeadsign),]
	  bus_pos$DateTime <- gsub("T"," ",bus_pos$DateTime)
	  bus_pos$DateTime <- ymd_hms(bus_pos$DateTime, tz = Sys.timezone())
	  bus_pos$TripStartTime <- gsub("T"," ",bus_pos$TripStartTime)
	  bus_pos$TripStartTime <- ymd_hms(bus_pos$TripStartTime, tz = Sys.timezone())
	  bus_pos$TripEndTime <- gsub("T"," ",bus_pos$TripEndTime)
	  bus_pos$TripEndTime <- ymd_hms(bus_pos$TripEndTime, tz = Sys.timezone())
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
  	else if (nrow(bus_pos) <= 0) {
	    error_log <- data.frame(
		               RouteID = 999,
		               Source = "bus_pos",
		               rows_added = 0,
		               StartTime = start_time,
		               EndTime = end_time,
		               trigger_timestamp = Sys.time())
		dbWriteTable(pg,"log",error_log, row.names = FALSE, append = TRUE)  
		}                
    dbDisconnect(pg) 
    closeAllConnections()
    print(paste0(Sys.time(),": Fetched ",nrow(bus_pos)," bus positions in ",ceiling(difftime(Sys.time(),start_time,units = "secs"))," sec(s)"))
    invisible(gc())
 }	