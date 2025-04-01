library(sf)
library(rnaturalearth)
library(geosphere)

############################## parameters
RouteID <- "S9"
StopID <- "1002915"

############################## shapefiles
lines <- read_sf("C:/Users/ebarbaro/Downloads/Metro Bus Lines.geojson")
stops <- read_sf("C:/Users/ebarbaro/Downloads/Metro Bus Stops.geojson")

############################## api results

################## Bus Route and Stop Methods
## bus position (Returns bus positions for the given route. If no parameters are specified, all bus positions are returned. Bus positions are refreshed approximately every 7 to 10 seconds.)
bus_pos <- GET("https://api.wmata.com/Bus.svc/json/jBusPositions", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  
bus_pos <- suppressWarnings(rbindlist(bus_pos$BusPositions,fill = TRUE))

#
bus_pos_col_names <- as.data.frame(names(bus_pos))
names(bus_pos_col_names)[1] <- "col_names"
bus_pos_col_names$in_bus_pos <- "x"

## path details (returns the set of ordered latitude/longitude points along a route variant along with the list of stops served)
path_deets <- GET(paste0("https://api.wmata.com/Bus.svc/json/jRouteDetails?RouteID=",RouteID,""), add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  

##stops
dir0 <- suppressWarnings(rbindlist(path_deets[["Direction0"]][["Stops"]],fill = TRUE))
dir0$RouteID <- path_deets$RouteID
dir0$Name <- path_deets$Name
dir0$TripHeadsign <- path_deets[["Direction0"]][["TripHeadsign"]]
dir0$DirectionText <- path_deets[["Direction0"]][["DirectionText"]]
dir0$DirectionNum <- path_deets[["Direction0"]][["DirectionNum"]]
dir0 <- dir0 %>% relocate(RouteID,Name,TripHeadsign,DirectionText,DirectionNum, .before = StopID)
dir0_target <- subset(dir0, Routes == RouteID)
dir0_dups <- subset(dir0, Routes != RouteID)
dir0_dups <- dir0_dups %>% 
  group_by(StopID) %>% 
  summarise(Addtl_Routes = str_c(Routes, collapse = ", "))
dir0_target <- left_join(dir0_target,dir0_dups,by="StopID")
names(dir0_target)
dir0_target <- subset(dir0_target,select=c(StopID,Lon,Lat,RouteID,DirectionText,DirectionNum,Name,TripHeadsign,Routes,Addtl_Routes))

#
dir1 <- suppressWarnings(rbindlist(path_deets[["Direction1"]][["Stops"]],fill = TRUE))
dir1$RouteID <- path_deets$RouteID
dir1$Name <- path_deets$Name
dir1$TripHeadsign <- path_deets[["Direction1"]][["TripHeadsign"]]
dir1$DirectionText <- path_deets[["Direction1"]][["DirectionText"]]
dir1$DirectionNum <- path_deets[["Direction1"]][["DirectionNum"]]
dir1 <- dir1 %>% relocate(RouteID,Name,TripHeadsign,DirectionText,DirectionNum, .before = StopID)
dir1_target <- subset(dir1, Routes == RouteID)
dir1_dups <- subset(dir1, Routes != RouteID)
dir1_dups <- dir1_dups %>% 
  group_by(StopID) %>% 
  summarise(Addtl_Routes = str_c(Routes, collapse = ", "))
dir1_target <- left_join(dir1_target,dir1_dups,by="StopID")

path_deets_stops <- bind_rows(dir0_target,dir1_target)
suppressWarnings(rm(dir0,dir0_dups,dir0_target,dir0_target_x,dir1,dir1_dups,dir1_target))

#
path_deets_stops_col_names <- as.data.frame(names(path_deets_stops))
names(path_deets_stops_col_names)[1] <- "col_names"
path_deets_stops_col_names$in_path_deets_stops <- "x"

col_names <- full_join(bus_pos_col_names,path_deets_stops_col_names,by=c("col_names"))

##route
dir0 <- data.frame()
i <- 1

for (i in i:length(path_deets[["Direction0"]][["Shape"]])) {
  dir0_stg <- data.frame(
              RouteID = path_deets$RouteID,
              Name = path_deets$Name,
              TripHeadsign = path_deets[["Direction0"]][["TripHeadsign"]],
              DirectionText = path_deets[["Direction0"]][["DirectionText"]],
              DirectionNum = path_deets[["Direction0"]][["DirectionNum"]],
              Lat = path_deets[["Direction0"]][["Shape"]][[i]][["Lat"]],
              Lon = path_deets[["Direction0"]][["Shape"]][[i]][["Lon"]],
              SeqNum = path_deets[["Direction0"]][["Shape"]][[i]][["SeqNum"]])
  
  dir0 <- rbind(dir0,dir0_stg)
  print(paste0("processed seq ",i))
  rm(dir0_stg)
}

#
dir1 <- data.frame()
i <- 1

for (i in i:length(path_deets[["Direction1"]][["Shape"]])) {
  dir1_stg <- data.frame(
    RouteID = path_deets$RouteID,
    Name = path_deets$Name,
    TripHeadsign = path_deets[["Direction1"]][["TripHeadsign"]],
    DirectionText = path_deets[["Direction1"]][["DirectionText"]],
    DirectionNum = path_deets[["Direction1"]][["DirectionNum"]],
    Lat = path_deets[["Direction1"]][["Shape"]][[i]][["Lat"]],
    Lon = path_deets[["Direction1"]][["Shape"]][[i]][["Lon"]],
    SeqNum = path_deets[["Direction1"]][["Shape"]][[i]][["SeqNum"]])
  
  dir1 <- rbind(dir1,dir1_stg)
  print(paste0("processed seq ",i))
  rm(dir1_stg)
}

path_deets_routes <- bind_rows(dir0,dir1)
suppressWarnings(rm(dir0,dir1,i))

#
path_deets_routes_col_names <- as.data.frame(names(path_deets_routes))
names(path_deets_routes_col_names)[1] <- "col_names"
path_deets_routes_col_names$in_path_deets_routes <- "x"

col_names <- full_join(col_names,path_deets_routes_col_names,by=c("col_names"))

## Routes (Returns a list of all bus route variants. For example, the 10A and 10Av1 are the same route, but may stop at slightly different locations.)
routes <- GET("https://api.wmata.com/Bus.svc/json/jRoutes", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  

#
route <- data.frame()
i <- 1

for (i in i:length(routes$Routes)) {
  routes_stg <- data.frame(
    RouteID = coalesce(routes[["Routes"]][[i]][["RouteID"]],NA),
    Name = coalesce(routes[["Routes"]][[i]][["Name"]],NA),
    LineDescription = coalesce(routes[["Routes"]][[i]][["LineDescription"]],NA))
  
  route <- rbind(route,routes_stg)
  print(paste0("processed route ",i))
  rm(routes_stg)
}

suppressWarnings(rm(i))

#
routes_col_names <- as.data.frame(names(route))
names(routes_col_names)[1] <- "col_names"
routes_col_names$in_routes <- "x"

col_names <- full_join(col_names,routes_col_names,by=c("col_names"))

## Schedule (Returns schedules for a given route variant for a given date.)
schedule <- GET(paste0("https://api.wmata.com/Bus.svc/json/jRouteSchedule?RouteID=",RouteID,""), add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  

##departures
dir0 <- data.frame()
i <- 1

for (i in i:length(schedule[["Direction0"]])) {
  dir0_stg <- data.frame(
    TripID = coalesce(schedule[["Direction0"]][[i]][["TripID"]],NA),
    RouteID = coalesce(schedule[["Direction0"]][[i]][["RouteID"]],NA),
    Name = schedule[["Name"]],
    TripHeadsign = coalesce(schedule[["Direction0"]][[i]][["TripHeadsign"]],NA),
    DirectionText = coalesce(schedule[["Direction0"]][[i]][["TripDirectionText"]],NA),
    DirectionNum = coalesce(schedule[["Direction0"]][[i]][["DirectionNum"]],NA),
    StartTime = coalesce(schedule[["Direction0"]][[i]][["StartTime"]],NA),
    EndTime = coalesce(schedule[["Direction0"]][[i]][["EndTime"]],NA))
  
  dir0 <- rbind(dir0,dir0_stg)
  print(paste0("processed trip ",i))
  rm(dir0_stg)
}

#
dir1 <- data.frame()
i <- 1

for (i in i:length(schedule[["Direction1"]])) {
  dir1_stg <- data.frame(
    TripID = coalesce(schedule[["Direction1"]][[i]][["TripID"]],NA),
    RouteID = coalesce(schedule[["Direction1"]][[i]][["RouteID"]],NA),
    Name = schedule[["Name"]],
    TripHeadsign = coalesce(schedule[["Direction1"]][[i]][["TripHeadsign"]],NA),
    DirectionText = coalesce(schedule[["Direction1"]][[i]][["TripDirectionText"]],NA),
    DirectionNum = coalesce(schedule[["Direction1"]][[i]][["DirectionNum"]],NA),
    StartTime = coalesce(schedule[["Direction1"]][[i]][["StartTime"]],NA),
    EndTime = coalesce(schedule[["Direction1"]][[i]][["EndTime"]],NA))
  
  dir1 <- rbind(dir1,dir1_stg)
  print(paste0("processed trip ",i))
  rm(dir1_stg)
}

scheduled_departures <- bind_rows(dir0,dir1)
suppressWarnings(rm(dir0,dir1,i))

#
scheduled_departures_col_names <- as.data.frame(names(scheduled_departures))
names(scheduled_departures_col_names)[1] <- "col_names"
scheduled_departures_col_names$in_scheduled_departures <- "x"

col_names <- full_join(col_names,scheduled_departures_col_names,by=c("col_names"))

##stops
dir0 <- data.frame()
x <- 1
i <- 1
for (x in x:length(schedule[["Direction0"]])) {
  
  for (i in i:length(schedule[["Direction0"]][[i]][["StopTimes"]])){
    dir0_stg <- data.frame(
      TripID = coalesce(schedule[["Direction0"]][[x]][["TripID"]],NA),
      RouteID = coalesce(schedule[["Direction0"]][[x]][["RouteID"]],NA),
      Name = schedule[["Name"]],
      TripHeadsign = coalesce(schedule[["Direction0"]][[x]][["TripHeadsign"]],NA),
      DirectionText = coalesce(schedule[["Direction0"]][[x]][["TripDirectionText"]],NA),
      DirectionNum = coalesce(schedule[["Direction0"]][[x]][["DirectionNum"]],NA),
      StopID = coalesce(schedule[["Direction0"]][[i]][["StopTimes"]][[i]][["StopID"]],NA),
      StopName = coalesce(schedule[["Direction0"]][[i]][["StopTimes"]][[i]][["StopName"]],NA),
      StopSeq = coalesce(schedule[["Direction0"]][[i]][["StopTimes"]][[i]][["StopSeq"]],NA),
      Time = coalesce(schedule[["Direction0"]][[i]][["StopTimes"]][[i]][["Time"]],NA))
    
    dir0 <- rbind(dir0,dir0_stg)
    print(paste0("processed stop ",i))
    rm(dir0_stg)
    i <- i + 1
  }
    print(paste0("######### processed trip ",x))
    x <- x + 1
    i <- 1
}

#
dir1 <- data.frame()
x <- 1
i <- 1
for (x in x:length(schedule[["Direction1"]])) {
  
  for (i in i:length(schedule[["Direction1"]][[i]][["StopTimes"]])){
    dir1_stg <- data.frame(
      TripID = coalesce(schedule[["Direction1"]][[x]][["TripID"]],NA),
      RouteID = coalesce(schedule[["Direction1"]][[x]][["RouteID"]],NA),
      Name = schedule[["Name"]],
      TripHeadsign = coalesce(schedule[["Direction1"]][[x]][["TripHeadsign"]],NA),
      DirectionText = coalesce(schedule[["Direction1"]][[x]][["TripDirectionText"]],NA),
      DirectionNum = coalesce(schedule[["Direction1"]][[x]][["DirectionNum"]],NA),
      StopID = coalesce(schedule[["Direction1"]][[i]][["StopTimes"]][[i]][["StopID"]],NA),
      StopName = coalesce(schedule[["Direction1"]][[i]][["StopTimes"]][[i]][["StopName"]],NA),
      StopSeq = coalesce(schedule[["Direction1"]][[i]][["StopTimes"]][[i]][["StopSeq"]],NA),
      Time = coalesce(schedule[["Direction1"]][[i]][["StopTimes"]][[i]][["Time"]],NA))
    
    dir1 <- rbind(dir1,dir1_stg)
    print(paste0("processed stop ",i))
    rm(dir1_stg)
    i <- i + 1
  }
  print(paste0("######### processed trip ",x))
  x <- x + 1
  i <- 1
}

scheduled_stops <- bind_rows(dir0,dir1)
suppressWarnings(rm(dir0,dir1,i))

#
scheduled_stops_col_names <- as.data.frame(names(scheduled_stops))
names(scheduled_stops_col_names)[1] <- "col_names"
scheduled_stops_col_names$in_scheduled_stops <- "x"

col_names <- full_join(col_names,scheduled_stops_col_names,by=c("col_names"))

## Schedule at stop (Bus stop information, route and schedule data, and bus positions.)
schedule_at_stop <- GET(paste0("https://api.wmata.com/Bus.svc/json/jStopSchedule?StopID=",StopID,""), add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  

#
stop_schedule <- data.frame()
i <- 1

for (i in i:length(schedule_at_stop$ScheduleArrivals)) {
  stop_schedule_stg <- data.frame(
    StopID = coalesce(schedule_at_stop[["Stop"]][["StopID"]],NA),
    TripID = coalesce(schedule_at_stop[["ScheduleArrivals"]][[i]][["TripID"]],NA),
    RouteID = coalesce(schedule_at_stop[["ScheduleArrivals"]][[i]][["RouteID"]],NA),
    ScheduleTime = coalesce(schedule_at_stop[["ScheduleArrivals"]][[i]][["ScheduleTime"]],NA),
    StartTime = coalesce(schedule_at_stop[["ScheduleArrivals"]][[i]][["StartTime"]],NA),
    EndTime = coalesce(schedule_at_stop[["ScheduleArrivals"]][[i]][["EndTime"]],NA),
    Lat = coalesce(schedule_at_stop[["Stop"]][["Lat"]],NA),
    Lon = coalesce(schedule_at_stop[["Stop"]][["Lon"]],NA),
    DirectionNum = coalesce(schedule_at_stop[["ScheduleArrivals"]][[i]][["DirectionNum"]],NA),
    DirectionText = coalesce(schedule_at_stop[["ScheduleArrivals"]][[i]][["TripDirectionText"]],NA),
    StopName = coalesce(schedule_at_stop[["Stop"]][["Name"]],NA),
    TripHeadsign = coalesce(schedule_at_stop[["ScheduleArrivals"]][[i]][["TripHeadsign"]],NA) )
  
  stop_schedule <- rbind(stop_schedule,stop_schedule_stg)
  print(paste0("processed stop_schedule ",i))
  rm(stop_schedule_stg)
}

suppressWarnings(rm(i))

#
schedule_at_stop_col_names <- as.data.frame(names(stop_schedule))
names(schedule_at_stop_col_names)[1] <- "col_names"
schedule_at_stop_col_names$in_schedule_at_stop <- "x"

col_names <- full_join(col_names,schedule_at_stop_col_names,by=c("col_names"))

## Stop search (Lookup of bus stops)
stop_search <- GET("https://api.wmata.com/Bus.svc/json/jStops", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  
stop_search <- suppressWarnings(rbindlist(stop_search$Stops,fill = TRUE))

#
stop_search_col_names <- as.data.frame(names(stop_search))
names(stop_search_col_names)[1] <- "col_names"
stop_search_col_names$in_stop_search <- "x"

col_names <- full_join(col_names,stop_search_col_names,by=c("col_names"))

################## Incidents
## Bus Incidents (Returns a set of reported bus incidents/delays for a given Route. Omit the Route to return all reported items. Bus incidents/delays are refreshed once every 20 to 30 seconds approximately.)
bus_incidents <- GET("https://api.wmata.com/Incidents.svc/json/BusIncidents", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON() 
bus_incidents <- suppressWarnings(rbindlist(bus_incidents$BusIncidents,fill = TRUE))

#
bus_incidents_col_names <- as.data.frame(names(bus_incidents))
names(bus_incidents_col_names)[1] <- "col_names"
bus_incidents_col_names$in_bus_incidents <- "x"

col_names <- full_join(col_names,bus_incidents_col_names,by=c("col_names"))

################## Real-Time Bus Predictions
## Next Bus (Next bux infoboard data)
bus_predictions <- GET(paste0("https://api.wmata.com/NextBusService.svc/json/jPredictions?StopID=",StopID,""), add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON() 
bus_predictions <- suppressWarnings(rbindlist(bus_predictions$Predictions,fill = TRUE))

#
bus_predictions_col_names <- as.data.frame(names(bus_predictions))
names(bus_predictions_col_names)[1] <- "col_names"
bus_predictions_col_names$in_bus_predictions <- "x"

col_names <- full_join(col_names,bus_predictions_col_names,by=c("col_names"))
