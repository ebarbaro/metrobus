################## Bus Route and Stop Methods
## bus position (Returns bus positions for the given route. If no parameters are specified, all bus positions are returned. Bus positions are refreshed approximately every 7 to 10 seconds.)

# buses_mapped <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
#   filter(name %in% c("District of Columbia"))

############################## parameters
RouteID <- "S9"
StopID <- "1002915"

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

## Stop search (Lookup of bus stops)
stop_search <- GET("https://api.wmata.com/Bus.svc/json/jStops", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  
stop_search <- suppressWarnings(rbindlist(stop_search$Stops,fill = TRUE))

##
lines <- path_deets_routes %>%
  st_as_sf(coords = c("Lon","Lat"), crs = 4326) %>%
  group_by(DirectionText) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

stops_pts <- stop_search %>%
  st_as_sf(coords = c("Lon","Lat")) %>%
  st_set_crs(4326)

stops_pts$geometry

sched_stops_coords <- inner_join(scheduled_stops,stop_search,by=c("StopID","RouteID"="Routes"))

# ggplot() + 
#   geom_sf(data = lines, aes(color = DirectionText)) +
#   geom_sf(data = lines, aes(color = DirectionText)) +
#   geom_sf(data = buses_mapped, fill = NA) 

sched_stops_coords_dd <- sched_stops_coords[!duplicated(sched_stops_coords[,c("RouteID","DirectionText","StopSeq")]),] 
sched_stops_coords_dd <- sched_stops_coords_dd %>%
  st_as_sf(coords = c("Lon","Lat")) %>%
  st_set_crs(4326)

while (1==1) {
print(paste0(substring(as_hms(Sys.time()),1,8),": Fetching data..."))  
bus_pos <- GET("https://api.wmata.com/Bus.svc/json/jBusPositions", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()
bus_pos <- suppressWarnings(rbindlist(bus_pos$BusPositions,fill = TRUE))

buses <- inner_join(bus_pos,sched_stops_coords,by=c("TripID"))

#View(buses)

buses <- buses %>%
  mutate(dist_km = distGeo(
    p1 = cbind(Lon.x, Lat.x),
    p2 = cbind(Lon.y, Lat.y)) / 1000
  ) 

buses_sub <- subset(buses,((buses$DirectionText.x == "NORTH") & (buses$Lat.x < buses$Lat.y))|((buses$DirectionText.x == "SOUTH") & (buses$Lat.x > buses$Lat.y)))

#nrow(buses)
#nrow(buses_sub)

buses_sub <- buses_sub[order(-xtfrm(buses_sub$dist_km),buses_sub$StopSeq, decreasing = TRUE),]

buses_dd <- buses_sub[!duplicated(buses_sub[,c("TripID")]),]

print(paste0(substring(as_hms(Sys.time()),1,8),": Records collected...")) 
buses_dd <- buses_dd[order(-xtfrm(buses_dd$DirectionText.x),-xtfrm(buses_dd$StopSeq),buses_dd$DateTime, decreasing = TRUE),]
print(as.data.frame(subset(buses_dd,select=c(DateTime,dist_km,StopSeq,VehicleID,TripID,DirectionText.x,StopID,StopName,Lon.x,Lat.x,Lon.y,Lat.y))))

buses_dd <- buses_dd %>%
  st_as_sf(coords = c("Lon.x","Lat.x")) %>%
  st_set_crs(4326)
 
print(paste0(substring(as_hms(Sys.time()),1,8),": Refreshing map...")) 
#dev.off(dev.list()["RStudioGD"])
map <- ggplot() +
        geom_sf(data = sched_stops_coords_dd, aes(color = DirectionText, shape = DirectionText), size = 3) +
        #geom_sf(data = lines, aes(color = DirectionText)) +
        #geom_sf(data = buses_mapped, fill = NA) +
        #geom_sf_label(data = buses_dd, aes(label = paste0(str_sub(buses_dd$DirectionText.x,1,1),"B ",buses_dd$RouteID.x," - ",round(buses_dd$dist_km, digits = 2)," km away from stop ",buses_dd$StopSeq," at ",str_sub(buses_dd$DateTime,-8)), color = DirectionText.x)) +
        geom_sf_label(data = buses_dd, aes(label = paste0(buses_dd$VehicleID," - ",round(buses_dd$dist_km, digits = 2)," km away from stop ",buses_dd$StopSeq," at ",str_sub(buses_dd$DateTime,-8)), color = DirectionText.x)) +
        coord_sf(xlim = c(-77.07, -77.0), ylim = c(38.8999, 38.998))
plot(map)
save.image()
rm(bus_pos,buses,buses_dd,buses_sub)
Sys.sleep(7)
}

ls()
