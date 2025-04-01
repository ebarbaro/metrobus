library(sf)
library(rnaturalearth)
library(geosphere)
library(ggiraph)

rm(list = ls(all.names = TRUE)) 

## Stop search (Lookup of bus stops)
stop_search <- GET("https://api.wmata.com/Bus.svc/json/jStops", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  
stop_search <- suppressWarnings(rbindlist(stop_search$Stops,fill = TRUE))

#
stops_pts <- stop_search %>%
  st_as_sf(coords = c("Lon","Lat")) %>%
  st_set_crs(4326)

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

  suppressWarnings(rm(i))

  route$LineKey <- sub("\\-.*", "", route$Name)
  route$LineKey <- trimws(route$LineKey)
  route$Dir <- gsub(".*[-]([^.]+)[-].*", "\\1", route$Name)
  route$Dir <- trimws(route$Dir)

################## Bus Route and Stop Methods
## bus position (Returns bus positions for the given route. If no parameters are specified, all bus positions are returned. Bus positions are refreshed approximately every 7 to 10 seconds.)
bus_pos <- GET("https://api.wmata.com/Bus.svc/json/jBusPositions", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  
bus_pos <- suppressWarnings(rbindlist(bus_pos$BusPositions,fill = TRUE))

# bus_pos_j <- left_join(bus_pos,route,by=c("RouteID"="LineKey","TripHeadsign"="Dir"))
# bus_pos_j$RouteID.x <- ifelse((bus_pos_j$RouteID == bus_pos_j$RouteID.y),bus_pos_j$RouteID,bus_pos_j$RouteID.y)
# bus_pos_j$RouteID.x <- coalesce(bus_pos_j$RouteID.x,bus_pos_j$RouteID)
# bus_pos_j <- bus_pos_j[order(bus_pos_j$RouteID.x, decreasing = FALSE),]
# RouteID <- bus_pos_j[!duplicated(bus_pos_j$RouteID.x),]
# RouteID <- subset(RouteID, select=c(RouteID.x))
bus_pos <- bus_pos[order(bus_pos$RouteID, decreasing = FALSE),]
RouteID <- bus_pos[!duplicated(bus_pos$RouteID),]
RouteID <- subset(RouteID, select=c(RouteID))

## path details (returns the set of ordered latitude/longitude points along a route variant along with the list of stops served)
RouteID <- as.array(RouteID$RouteID)
date <- as.character(str_sub(Sys.time(),1,10))
r <- 1
path_deets_routes <- data.frame()
path_deets_stops <- data.frame()
scheduled_departures  <- data.frame()
scheduled_stops <- data.frame()

for (r in r:length(RouteID)) {
  path_deets <- GET(paste0("https://api.wmata.com/Bus.svc/json/jRouteDetails?RouteID=",RouteID[r],"&Date=",date,""), add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()  
  
  if (is.list(path_deets)) {
    ##stops
    dir0 <- suppressWarnings(rbindlist(path_deets[["Direction0"]][["Stops"]],fill = TRUE))
    dir0$RouteID <- path_deets$RouteID
    dir0$Name <- path_deets$Name
    dir0$TripHeadsign <- path_deets[["Direction0"]][["TripHeadsign"]]
    dir0$DirectionText <- path_deets[["Direction0"]][["DirectionText"]]
    dir0$DirectionNum <- path_deets[["Direction0"]][["DirectionNum"]]
    
    { 
      if ((length(path_deets[["Direction0"]][["Stops"]])>0)&(length(path_deets[["Direction1"]][["Stops"]])>0)) {
        dir0 <- dir0 %>% relocate(RouteID,Name,TripHeadsign,DirectionText,DirectionNum, .before = StopID)
        dir0_target <- subset(dir0, Routes == RouteID)
        dir0_dups <- subset(dir0, Routes != RouteID)
        dir0_dups <- dir0_dups %>% 
          group_by(StopID) %>% 
          summarise(Addtl_Routes = str_c(Routes, collapse = ", "))
        dir0_target <- left_join(dir0_target,dir0_dups,by="StopID")
        names(dir0_target)
        dir0_target <- subset(dir0_target,select=c(StopID,Lon,Lat,RouteID,DirectionText,DirectionNum,Name,TripHeadsign,Routes,Addtl_Routes))
        
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
        
        path_deets_stops_stg <- bind_rows(dir0_target,dir1_target)
        path_deets_stops <- rbind(path_deets_stops,path_deets_stops_stg)
      }
      
      else if ((length(path_deets[["Direction0"]][["Stops"]])==0)&(length(path_deets[["Direction1"]][["Stops"]])==0)) {
        suppressWarnings(rm(dir0))
      }
      else if ((length(path_deets[["Direction0"]][["Stops"]])>0)&(length(path_deets[["Direction1"]][["Stops"]])==0)) {
        dir0 <- dir0 %>% relocate(RouteID,Name,TripHeadsign,DirectionText,DirectionNum, .before = StopID)
        dir0_target <- subset(dir0, Routes == RouteID)
        dir0_dups <- subset(dir0, Routes != RouteID)
        dir0_dups <- dir0_dups %>% 
          group_by(StopID) %>% 
          summarise(Addtl_Routes = str_c(Routes, collapse = ", "))
        dir0_target <- left_join(dir0_target,dir0_dups,by="StopID")
        names(dir0_target)
        dir0_target <- subset(dir0_target,select=c(StopID,Lon,Lat,RouteID,DirectionText,DirectionNum,Name,TripHeadsign,Routes,Addtl_Routes))
        
        path_deets_stops_stg <- dir0_target
        path_deets_stops <- rbind(path_deets_stops,path_deets_stops_stg)
        
      }
      else if ((length(path_deets[["Direction0"]][["Stops"]])==0)&(length(path_deets[["Direction1"]][["Stops"]])>0)) {
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
        
        path_deets_stops_stg <- dir1_target
        path_deets_stops <- rbind(path_deets_stops,path_deets_stops_stg)
        
      }
    }  
    
    ##route
    { 
      if ((length(path_deets[["Direction0"]][["Shape"]])>0)&(length(path_deets[["Direction1"]][["Shape"]])>0)) {
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
          #print(paste0(RouteID[r],": processed seq ",i))
          rm(dir0_stg)
        }
        
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
          #print(paste0(RouteID[r],": processed seq ",i))
          rm(dir1_stg)
        }
        path_deets_routes_stg <- bind_rows(dir0,dir1)
        path_deets_routes <- rbind(path_deets_routes,path_deets_routes_stg)
        suppressWarnings(rm(dir0,dir0_dups,dir0_target,dir0_target_x,dir1,dir1_dups,dir1_target,path_deets_stops_stg,path_deets_routes_stg,i,x,path_deets))
      }
      
      else if ((length(path_deets[["Direction0"]][["Shape"]])==0)&(length(path_deets[["Direction1"]][["Shape"]])==0)) {
        suppressWarnings(rm(dir0,dir0_dups,dir0_target,dir0_target_x,dir1,dir1_dups,dir1_target,path_deets_stops_stg,path_deets_routes_stg,i,x,path_deets))
      }
      else if ((length(path_deets[["Direction0"]][["Shape"]])>0)&(length(path_deets[["Direction1"]][["Shape"]])==0)) {
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
          #print(paste0(RouteID[r],": processed seq ",i))
          rm(dir0_stg)
        }
        path_deets_routes_stg <- dir0
        path_deets_routes <- rbind(path_deets_routes,path_deets_routes_stg)
        suppressWarnings(rm(dir0,dir0_dups,dir0_target,dir0_target_x,dir1,dir1_dups,dir1_target,path_deets_stops_stg,path_deets_routes_stg,i,x,path_deets))
      }
      else if ((length(path_deets[["Direction0"]][["Shape"]])==0)&(length(path_deets[["Direction1"]][["Shape"]])>0)) {
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
          #print(paste0(RouteID[r],": processed seq ",i))
          rm(dir1_stg)
        }
        path_deets_routes_stg <- dir1
        path_deets_routes <- rbind(path_deets_routes,path_deets_routes_stg)
        suppressWarnings(rm(dir0,dir0_dups,dir0_target,dir0_target_x,dir1,dir1_dups,dir1_target,path_deets_stops_stg,path_deets_routes_stg,i,x,path_deets))
      }
    }  
  }
  
  ## Schedule (Returns schedules for a given route variant for a given date.)
  schedule <- GET(paste0("https://api.wmata.com/Bus.svc/json/jRouteSchedule?RouteID=",RouteID[r],"&Date=",date,"&IncludingVariations=true"), add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON() 
  
  if (is.list(schedule)) {
    ##departures
    { 
      if ((length(schedule[["Direction0"]])>0)&(length(schedule[["Direction1"]]))>0) {
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
          #print(paste0(RouteID[r],": processed departures ",i))
          rm(dir0_stg)
        }
        
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
          #print(paste0(RouteID[r],": processed departures ",i))
          rm(dir1_stg)
        }
        
        scheduled_departures_stg <- bind_rows(dir0,dir1)
        
        scheduled_departures <- rbind(scheduled_departures,scheduled_departures_stg)
        suppressWarnings(rm(dir0,dir1,i,scheduled_departures_stg))
      }
      
      else if ((length(schedule[["Direction0"]])==0)&(length(schedule[["Direction1"]]))==0) {
        suppressWarnings(rm(dir0,dir1,i,scheduled_departures_stg))
      }
      else if ((length(schedule[["Direction0"]])>0)&(length(schedule[["Direction1"]]))==0) {
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
          #print(paste0(RouteID[r],": processed departures ",i))
          rm(dir0_stg)
        }
        
        scheduled_departures_stg <- dir0
        
        scheduled_departures <- rbind(scheduled_departures,scheduled_departures_stg)
        suppressWarnings(rm(dir0,dir1,i,scheduled_departures_stg))
        
      }
      else if ((length(schedule[["Direction0"]])==0)&(length(schedule[["Direction1"]]))>0) {
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
          #print(paste0(RouteID[r],": processed departures ",i))
          rm(dir1_stg)
        }
        
        scheduled_departures_stg <- dir1
        
        scheduled_departures <- rbind(scheduled_departures,scheduled_departures_stg)
        suppressWarnings(rm(dir0,dir1,i,scheduled_departures_stg))
        
      }
    }
    ##stops
    { 
      if ((length(schedule[["Direction0"]])>0)&(length(schedule[["Direction1"]]))>0) {
        dir0 <- data.frame()
        x <- 1
        i <- 1
        
        for (x in 1:length(schedule[["Direction0"]])) {
          
          for (i in 1:length(schedule[["Direction0"]][[x]][["StopTimes"]])) {
            { 
              if (i > length(schedule[["Direction0"]][[x]][["StopTimes"]])) {
                rm(i)
                break
              }
              else {
                dir0_stg <- data.frame(
                  TripID = coalesce(schedule[["Direction0"]][[x]][["TripID"]],NA),
                  RouteID = coalesce(schedule[["Direction0"]][[x]][["RouteID"]],NA),
                  Name = schedule[["Name"]],
                  TripHeadsign = coalesce(schedule[["Direction0"]][[x]][["TripHeadsign"]],NA),
                  DirectionText = coalesce(schedule[["Direction0"]][[x]][["TripDirectionText"]],NA),
                  DirectionNum = coalesce(schedule[["Direction0"]][[x]][["DirectionNum"]],NA),
                  StopID = coalesce(schedule[["Direction0"]][[x]][["StopTimes"]][[i]][["StopID"]],NA),
                  StopName = coalesce(schedule[["Direction0"]][[x]][["StopTimes"]][[i]][["StopName"]],NA),
                  StopSeq = coalesce(schedule[["Direction0"]][[x]][["StopTimes"]][[i]][["StopSeq"]],NA),
                  Time = coalesce(schedule[["Direction0"]][[x]][["StopTimes"]][[i]][["Time"]],NA))
                
                dir0 <- rbind(dir0,dir0_stg)
                #print(paste0("processed stop ",i))
                rm(dir0_stg)
                i <- i + 1
              } 
              
            }
          }
          #print(paste0(RouteID[r],": processed trip ",x))
          i <- 1
          x <- x + 1
        }
        
        #
        
        dir1 <- data.frame()
        x <- 1
        i <- 1
        for (x in 1:length(schedule[["Direction1"]])) {
          
          for (i in 1:length(schedule[["Direction1"]][[x]][["StopTimes"]])){
            { 
              if (i > length(schedule[["Direction1"]][[x]][["StopTimes"]])) {
                rm(i)
                break
              }
              else {
                dir1_stg <- data.frame(
                  TripID = coalesce(schedule[["Direction1"]][[x]][["TripID"]],NA),
                  RouteID = coalesce(schedule[["Direction1"]][[x]][["RouteID"]],NA),
                  Name = schedule[["Name"]],
                  TripHeadsign = coalesce(schedule[["Direction1"]][[x]][["TripHeadsign"]],NA),
                  DirectionText = coalesce(schedule[["Direction1"]][[x]][["TripDirectionText"]],NA),
                  DirectionNum = coalesce(schedule[["Direction1"]][[x]][["DirectionNum"]],NA),
                  StopID = coalesce(schedule[["Direction1"]][[x]][["StopTimes"]][[i]][["StopID"]],NA),
                  StopName = coalesce(schedule[["Direction1"]][[x]][["StopTimes"]][[i]][["StopName"]],NA),
                  StopSeq = coalesce(schedule[["Direction1"]][[x]][["StopTimes"]][[i]][["StopSeq"]],NA),
                  Time = coalesce(schedule[["Direction1"]][[x]][["StopTimes"]][[i]][["Time"]],NA))
                
                dir1 <- rbind(dir1,dir1_stg)
                #print(paste0("processed stop ",i))
                rm(dir1_stg)
                i <- i + 1
              } 
              
            }
          }
          #print(paste0(RouteID[r],": processed trip ",x))
          i <- 1
          x <- x + 1
        }
        
        scheduled_stops_stg <- bind_rows(dir0,dir1)
        scheduled_stops <- rbind(scheduled_stops,scheduled_stops_stg)
        suppressWarnings(rm(dir0,dir1,i,path_deets,schedule,scheduled_stops_stg))
      }
      
      else if ((length(schedule[["Direction0"]])==0)&(length(schedule[["Direction1"]]))==0) {
        suppressWarnings(rm(dir0,dir1,i,path_deets,schedule,scheduled_stops_stg))
      }
      
      else if ((length(schedule[["Direction0"]])>0)&(length(schedule[["Direction1"]]))==0) {
        dir0 <- data.frame()
        x <- 1
        i <- 1
        
        for (x in 1:length(schedule[["Direction0"]])) {
          
          for (i in 1:length(schedule[["Direction0"]][[x]][["StopTimes"]])) {
            { 
              if (i > length(schedule[["Direction0"]][[x]][["StopTimes"]])) {
                rm(i)
                break
              }
              else {
                dir0_stg <- data.frame(
                  TripID = coalesce(schedule[["Direction0"]][[x]][["TripID"]],NA),
                  RouteID = coalesce(schedule[["Direction0"]][[x]][["RouteID"]],NA),
                  Name = schedule[["Name"]],
                  TripHeadsign = coalesce(schedule[["Direction0"]][[x]][["TripHeadsign"]],NA),
                  DirectionText = coalesce(schedule[["Direction0"]][[x]][["TripDirectionText"]],NA),
                  DirectionNum = coalesce(schedule[["Direction0"]][[x]][["DirectionNum"]],NA),
                  StopID = coalesce(schedule[["Direction0"]][[x]][["StopTimes"]][[i]][["StopID"]],NA),
                  StopName = coalesce(schedule[["Direction0"]][[x]][["StopTimes"]][[i]][["StopName"]],NA),
                  StopSeq = coalesce(schedule[["Direction0"]][[x]][["StopTimes"]][[i]][["StopSeq"]],NA),
                  Time = coalesce(schedule[["Direction0"]][[x]][["StopTimes"]][[i]][["Time"]],NA))
                
                dir0 <- rbind(dir0,dir0_stg)
                #print(paste0("processed stop ",i))
                rm(dir0_stg)
                i <- i + 1
              } 
              
            }
          }
          #print(paste0(RouteID[r],": processed trip ",x))
          i <- 1
          x <- x + 1
        }
        
        scheduled_stops_stg <- dir0
        scheduled_stops <- rbind(scheduled_stops,scheduled_stops_stg)
        suppressWarnings(rm(dir0,dir1,i,path_deets,schedule,scheduled_stops_stg))
        
      }
      
      else if ((length(schedule[["Direction0"]])==0)&(length(schedule[["Direction1"]]))>0) {
        dir1 <- data.frame()
        x <- 1
        i <- 1
        for (x in 1:length(schedule[["Direction1"]])) {
          
          for (i in 1:length(schedule[["Direction1"]][[x]][["StopTimes"]])){
            { 
              if (i > length(schedule[["Direction1"]][[x]][["StopTimes"]])) {
                rm(i)
                break
              }
              else {
                dir1_stg <- data.frame(
                  TripID = coalesce(schedule[["Direction1"]][[x]][["TripID"]],NA),
                  RouteID = coalesce(schedule[["Direction1"]][[x]][["RouteID"]],NA),
                  Name = schedule[["Name"]],
                  TripHeadsign = coalesce(schedule[["Direction1"]][[x]][["TripHeadsign"]],NA),
                  DirectionText = coalesce(schedule[["Direction1"]][[x]][["TripDirectionText"]],NA),
                  DirectionNum = coalesce(schedule[["Direction1"]][[x]][["DirectionNum"]],NA),
                  StopID = coalesce(schedule[["Direction1"]][[x]][["StopTimes"]][[i]][["StopID"]],NA),
                  StopName = coalesce(schedule[["Direction1"]][[x]][["StopTimes"]][[i]][["StopName"]],NA),
                  StopSeq = coalesce(schedule[["Direction1"]][[x]][["StopTimes"]][[i]][["StopSeq"]],NA),
                  Time = coalesce(schedule[["Direction1"]][[x]][["StopTimes"]][[i]][["Time"]],NA))
                
                dir1 <- rbind(dir1,dir1_stg)
                #print(paste0("processed stop ",i))
                rm(dir1_stg)
                i <- i + 1
              } 
              
            }
          }
          #print(paste0(RouteID[r],": processed trip ",x))
          i <- 1
          x <- x + 1
        }
        
        
        scheduled_stops_stg <- dir1
        scheduled_stops <- rbind(scheduled_stops,scheduled_stops_stg)
        suppressWarnings(rm(dir0,dir1,i,path_deets,schedule,scheduled_stops_stg))
        
      }
    }
  }
  
  suppressWarnings(rm(dir0,dir1,i,path_deets,schedule,scheduled_stops_stg))
  print(paste0(RouteID[r],": processed line ",r," of ",length(RouteID)))
  save.image()
  r <- r+1
}   


#
lines <- path_deets_routes %>%
  st_as_sf(coords = c("Lon","Lat"), crs = 4326) %>%
  group_by(RouteID,DirectionText) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

suppressWarnings(rm(r,x,date))

############################## boundaries
buses_mapped <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("District of Columbia","Maryland","Virginia"))

################## Incidents
## Bus Incidents (Returns a set of reported bus incidents/delays for a given Route. Omit the Route to return all reported items. Bus incidents/delays are refreshed once every 20 to 30 seconds approximately.)
bus_incidents <- GET("https://api.wmata.com/Incidents.svc/json/BusIncidents", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON() 
bus_incidents <- suppressWarnings(rbindlist(bus_incidents$BusIncidents,fill = TRUE))

# ggplot() + 
#   geom_sf(data = lines, aes(color = DirectionText)) +
#   geom_sf(data = lines, aes(color = DirectionText)) +
#   geom_sf(data = buses_mapped, fill = NA) 
stop_search_dd <- subset(stop_search,select=-c(Routes))
stop_search_dd <- stop_search_dd[!duplicated(stop_search_dd$StopID),]

sched_stops_coords <- inner_join(scheduled_stops,stop_search_dd,by=c("StopID"))
names(sched_stops_coords)[3] <- "RouteName"
sched_stops_coords <- subset(sched_stops_coords,select = -c(Name.y))
sched_stops_coords$Time <- gsub("T"," ",sched_stops_coords$Time)
sched_stops_coords$Time <- ymd_hms(sched_stops_coords$Time, tz = "EST")
sched_stops_coords$RouteIDc <- sub("\\ -.*", "", sched_stops_coords$RouteName)
sched_stops_coords$Legend <- paste0(sched_stops_coords$RouteIDc," - ",str_sub(sched_stops_coords$DirectionText,1,1),"B")


# sched_stops_coords_dd <- sched_stops_coords_dd %>%
#   st_as_sf(coords = c("Lon","Lat")) %>%
#   st_set_crs(4326)

ls()
rm(list=setdiff(ls(), c("buses_mapped","lines","path_deets_routes","path_deets_stops","route","RouteID","routes","sched_stops_coords","scheduled_departures","scheduled_stops","stop_search","stops_pts")))
gc()

while (1==1) {
  print(paste0(substring(as_hms(Sys.time()),1,8),": Fetching data..."))  
  
  ## get bus positions
  bus_pos <- GET("https://api.wmata.com/Bus.svc/json/jBusPositions", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON()
  bus_pos <- suppressWarnings(rbindlist(bus_pos$BusPositions,fill = TRUE))
  bus_pos$DateTime <- gsub("T"," ",bus_pos$DateTime)
  bus_pos$DateTime <- ymd_hms(bus_pos$DateTime, tz = "EST")
  bus_pos$TripStartTime <- gsub("T"," ",bus_pos$TripStartTime)
  bus_pos$TripStartTime <- ymd_hms(bus_pos$TripStartTime, tz = "EST")
  bus_pos$TripEndTime <- gsub("T"," ",bus_pos$TripEndTime)
  bus_pos$TripEndTime <- ymd_hms(bus_pos$TripEndTime, tz = "EST")

  
  bus_incidents <- GET("https://api.wmata.com/Incidents.svc/json/BusIncidents", add_headers("api_key" = Sys.getenv("wmata_key"))) %>% content(as="text", encoding = "UTF-8") %>% fromJSON() 
  bus_incidents <- suppressWarnings(rbindlist(bus_incidents$BusIncidents,fill = TRUE))
  bus_incidents$IncidentLastUpdated <- gsub("T"," ",bus_incidents$DateUpdated)
  bus_incidents$IncidentLastUpdated <- ymd_hms(bus_incidents$IncidentLastUpdated, tz = "EST")
  bus_incidents_sub <- inner_join(bus_incidents,lines,by=c("RoutesAffected"="RouteID"))
  bus_incidents_sub$DirectionAffected <- ifelse((grepl("northbound",bus_incidents_sub$Description)&!grepl("NORTH",bus_incidents_sub$DirectionText)),NA,
                                          ifelse((grepl("southbound",bus_incidents_sub$Description)&!grepl("SOUTH",bus_incidents_sub$DirectionText)),NA,
                                           ifelse((grepl("eastbound",bus_incidents_sub$Description)&!grepl("EAST",bus_incidents_sub$DirectionText)),NA,
                                            ifelse((grepl("westbound",bus_incidents_sub$Description)&!grepl("WEST",bus_incidents_sub$DirectionText)),NA,bus_incidents_sub$DirectionText))))
  bus_incidents_sub <- bus_incidents_sub[!is.na(bus_incidents_sub$DirectionAffected),]
  bus_incidents_sub <- bus_incidents_sub[!duplicated(bus_incidents_sub[,c("RoutesAffected","Description")]),]
  bus_incidents_sub$IncidentType <- ifelse((bus_incidents_sub$Description %like% "operator availability"),"Notice",
                                     ifelse((bus_incidents_sub$Description %like% "detour"),paste0("Detour (",str_sub(bus_incidents_sub$DirectionAffected,1,1),"B)"),
                                      ifelse((bus_incidents_sub$Description %like% "delay"),paste0("Delay (",str_sub(bus_incidents_sub$DirectionAffected,1,1),"B)"),bus_incidents_sub$IncidentType)))
  bus_incidents_sub$IncidentDescription <- bus_incidents_sub$Description
  bus_incidents_sub <- subset(bus_incidents_sub,select=c(RoutesAffected,DirectionAffected,IncidentType,IncidentDescription,IncidentLastUpdated))
  suppressWarnings(rm(bus_incidents))
  
  buses <- left_join(bus_pos,bus_incidents_sub,by=c("RouteID"="RoutesAffected","DirectionText"="DirectionAffected"))
  
  buses <- buses[grepl("S2|S9",buses$RouteID)&(buses$DirectionText == "NORTH"),]
  lines_sub <- lines[grepl("^S2$|^S9$",lines$RouteID),]
  ##buses <- buses[grepl("S2|S9|52|54|X2|90|96|L2",buses$RouteID),]
  #lines_sub <- lines[grepl("^S2$|^S9$|^52$|^54$|^X2$|^90$|^96$|^L2$",lines$RouteID),]
  
  bus_trip <- subset(buses,select=c(RouteID,TripID,Deviation,DateTime))
  bus_trip <- bus_trip[!duplicated(bus_trip$TripID),]
  sched_stops_coords_dd <- inner_join(bus_trip,sched_stops_coords,by=c("RouteID"="RouteIDc","TripID"))
  sched_stops_coords_dd$Tooltip <- paste0(sched_stops_coords_dd$Legend," stop #",sched_stops_coords_dd$StopSeq,": ",sched_stops_coords_dd$StopName)

  rm(bus_trip)
  
  buses_geo <- inner_join(buses,sched_stops_coords,by=c("RouteID"="RouteIDc","TripID"))
  buses_geo$DiffTime <- round(difftime((buses_geo$Time + minutes(buses_geo$Deviation)),buses_geo$DateTime,units = "secs"), digits = 0)
  buses_geo <- buses_geo %>% relocate(DiffTime, .after = Deviation)
  buses_geo$BusLat <- buses_geo$Lat.x
  buses_geo$BusLon <- buses_geo$Lon.x
  buses_geo$StopLat <- buses_geo$Lat.y
  buses_geo$StopLon <- buses_geo$Lon.y
  buses_geo$DirectionNum <- buses_geo$DirectionNum.x
  buses_geo$DirectionText <- buses_geo$DirectionText.x
  buses_geo$TripHeadsign <- buses_geo$TripHeadsign.x
  buses_geo$PosDateTime <- buses_geo$DateTime
  buses_geo$SchedStopTime <- buses_geo$Time
  
  buses_geo <- buses_geo %>%
    mutate(dist_km = distGeo(
      p1 = cbind(Lon.x, Lat.x),
      p2 = cbind(Lon.y, Lat.y)) / 1000
    ) 
  
  buses_geo <- subset(buses_geo,select=c(TripID,VehicleID,RouteID,Legend,StopID,StopSeq,PosDateTime,SchedStopTime,dist_km,DiffTime,Deviation,DirectionText,IncidentType,BusLon,StopLon,BusLat,StopLat,RouteName,TripHeadsign,StopName,BlockNumber,DirectionNum,TripStartTime,TripEndTime,IncidentLastUpdated,IncidentDescription))
  
  buses_sub <- subset(buses_geo, (DirectionText == "NORTH" & BusLat < StopLat)|(DirectionText == "SOUTH" & BusLat > StopLat)|(DirectionText == "EAST" & BusLon < StopLon)|(DirectionText == "WEST" & BusLon > StopLon))
  
  ### scheduled arrivals
  buses_sub$BusArrivalOrder <- ave(buses_sub$dist_km, buses_sub$StopID, FUN = seq_along)
  
  ### change this to 'next_bus'
  buses_dd <- buses_sub %>% 
                group_by(VehicleID) %>% 
                filter(abs(DiffTime) == min(abs(DiffTime))) %>%
                ungroup()
  rm(buses_sub)

  print(paste0(substring(as_hms(Sys.time()),1,8),": Records collected...")) 
  
  buses_dd <- buses_dd[order(-xtfrm(buses_dd$Legend),-xtfrm(buses_dd$StopSeq),buses_dd$PosDateTime, decreasing = TRUE),]

  print(as.data.frame(subset(buses_dd,select=c(PosDateTime,Legend,VehicleID,DiffTime,dist_km,StopSeq,StopName,IncidentType))))

  buses_dd$Tooltip <- paste0(buses_dd$VehicleID," (",buses_dd$Legend,") - ",round(buses_dd$dist_km, digits = 2)," km from stop ",buses_dd$StopSeq," (+",round(difftime(Sys.time(),buses_dd$PosDateTime,units = "secs"), digits = 0),"s)")
  
  int <- interval(min(buses_geo$TripStartTime),max(buses_geo$TripEndTime + max(minutes(buses_geo$Deviation))))
  buses_x <- subset(buses_geo,select=c(TripID,RouteID,StopID,VehicleID,Deviation,DirectionText,PosDateTime,BusLon,BusLat))
  #buses_x <- subset(buses_x, PosDateTime %within% int)
  buses_x <- buses_x[!duplicated(buses_x[,c("TripID","RouteID","StopID","DirectionText")]),]
  
  #next_arrival <- subset(sched_stops_coords_dd, (sched_stops_coords_dd$Time + minutes(next_arrival$Deviation)) > PosDateTime)
  next_arrival <- left_join(buses_x,sched_stops_coords_dd,by=c("TripID"))
  #next_arrival <- subset(next_arrival, (next_arrival$Time + minutes(next_arrival$Deviation)) %within% int)
  next_arrival <- subset(next_arrival, (Time + minutes(Deviation.x)) > PosDateTime)
  next_arrival <- next_arrival[!duplicated(next_arrival[,c("TripID","RouteID.y","StopID.y","DirectionText.y")]),]
  next_arrival <- next_arrival %>%
    mutate(dist_km = distGeo(
      p1 = cbind(BusLon, BusLat),
      p2 = cbind(Lon, Lat)) / 1000
    ) 
  next_arrival$DiffTime <- round(difftime((next_arrival$Time + minutes(next_arrival$Deviation.x)),Sys.time(),units = "secs"), digits = 0)
  next_arrival$DiffTime <- as.numeric(gsub("([0-9]+).*$", "\\1", next_arrival$DiffTime))
  next_arrival <- subset(next_arrival, DiffTime >= 60)
  next_arrival <- next_arrival %>% group_by(StopID.y,DiffTime) %>%
    arrange(!desc(dist_km)) %>%
    filter(row_number() == 1)
  
  next_arrival <- next_arrival %>% 
    group_by(StopID.y) %>% 
    filter(DiffTime == min(DiffTime)) %>%
    ungroup()
  next_arrival$DiffTime <- ifelse((next_arrival$DiffTime < 0),paste0(round(abs(next_arrival$DiffTime)/60, digits = 0)," mins ago"),paste0("in ",round(abs(next_arrival$DiffTime)/60, digits = 0)," mins"))
  next_arrival$Tooltip <- paste0(next_arrival$StopName,": ",next_arrival$RouteID.x," - ",next_arrival$VehicleID," ",next_arrival$DiffTime)
  next_arrival$Legend <- paste0(next_arrival$Legend," - ",next_arrival$VehicleID) 
  rm(buses_x,buses_geo)                          
  buses_dd <- buses_dd %>%
    st_as_sf(coords = c("BusLon","BusLat")) %>%
    st_set_crs(4326)
  
 sched_stops_coords_dd <- sched_stops_coords_dd %>%
    st_as_sf(coords = c("Lon","Lat")) %>%
    st_set_crs(4326) 
 
 next_arrival <- next_arrival %>%
   st_as_sf(coords = c("Lon","Lat")) %>%
   st_set_crs(4326) 
 
  print(paste0(substring(as_hms(Sys.time()),1,8),": Refreshing map...")) 
  dev.off(dev.list()["RStudioGD"])
  
  map <- ggplot() +
    geom_sf(data = buses_mapped, fill = NA) +
    #geom_sf(data = lines_sub, aes(color = RouteID)) +
    geom_sf(data = sched_stops_coords_dd, aes(color = RouteID), size = 2) +
    #geom_sf_label(data = buses_dd, aes(label = Tooltip, color = RouteID), size = 3) +
    geom_sf_label(data = next_arrival, aes(label = Tooltip, color = Legend), size = 3) +
    coord_sf(xlim = c(-77.1,-76.9888), ylim = c(38.889, 39.0))
  
  plot(map)
  save.image()
  rm(list=setdiff(ls(), c("buses_mapped","lines","path_deets_routes","path_deets_stops","route","RouteID","routes","sched_stops_coords","scheduled_departures","scheduled_stops","stop_search","stops_pts","map")))
  gc()
  Sys.sleep(7)
}

