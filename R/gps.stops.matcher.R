require(dplyr)
require(lubridate)
require(geosphere)

#' Checks if the location of two geographical points match
#'
#'
#'
#' @param p1.lat point 1 latitude
#' @param p1.lon point 1 longitude
#' @param p2.df point 2 dataframe with latitude,longitude
#' @param threshold maximum distance for matching
#'
#' @return distance between point 1 and point 2
#'
#' @examples
#' p2 <- data.frame(59.39,29.53)
#' points.match(51.09,25.94,p2,50)
#'
#' @export
points.match <- function(p1.lat, p1.lon, p2.df, threshold) {
  p1.df <- data.frame(long=p1.lon, lat=p1.lat)
  dist <- distHaversine(p1.df,p2.df)
  return(dist < threshold)
}

#' Returns trips initial points
#'
#'
#'
#' @param locations.df GPS locations data frame
#' @param initial.pt.df initial point (longitude,latitude) dataframe
#' @param thresh point 2 dataframe with latitude,longitude
#' @param threshold maximum distance for points matching
#'
#' @return distance between point 1 and point 2
#'
#' @examples
#' locations <- data.frame(51.24,20.53)
#' init.pt <- data.frame(59.39,29.53)
#' get.trips.initial.points(locations,init.pt,30)
#'
#' @export
get.trips.initial.points <- function(locations.df, initial.pt.df, thresh = 50) {
  match.initial.point <- points.match(locations.df$latitude, locations.df$longitude, initial.pt.df, thresh)
  trip.initial.points <- filter(locations.df, match.initial.point)
  return(trip.initial.points)
}

#' Returns point trip number
#'
#' Matches point to trip according to point timestamp
#'
#' @param point.timestamp GPS point timestamp
#' @param trips.inits.df trips initial point (longitude,latitude,timestamp) dataframe
#'
#' @return point trip number
#'
#' @examples
#'
#' @export
get.trip.number <- function(point.timestamp, trips.inits.df) {
  t <- trips.inits.df$timestamp
  next.trips <- which(t > point.timestamp)
  res <- ifelse(length(next.trips) == 0, as.numeric(nrow(trips.inits.df)), min(next.trips) -1)
  return(res)
}

#' Returns distance between stop and GPS location points
#'
#' 
#'
#' @param locations.df GPS locations data frame
#' @param stops.df stops locations data frame
#' @param stop.row row number of stop in stops.df to be analyzed
#' @param location.row row number of location in locations.df to be analyzed
#'
#' @return distance between stop at row stop.row and location at row location.row
#'
#' @examples
#'
#' @export
get.stop.location.dist <- function(locations.df, stops.df, stop.row, location.row) {
  distHaversine(c(locations.df[location.row,]$longitude, locations.df[location.row,]$latitude),
                c(stops.df[stop.row,]$stop_lon, stops.df[stop.row,]$stop_lat))
}

#' Refines a given stop-location match using a surrounding location interval range
#'
#' 
#'
#' @param locations.df GPS locations data frame
#' @param stops.df stops locations data frame
#' @param st.row matched stop row number in stop in stops.df 
#' @param loc.row matched location row number in locations.df
#'
#' @return the best match for the given stop among the given locations interval
#'
#' @examples
#'
#' @export
refine.match <- function(locations.df, stops.df, st.row, loc.row, interval.range) {
  #cat("Initial Match between location row #",loc.row,"and stop row #",st.row,"with distance:",
      #get.stop.location.dist(locations.df, stops.df, st.row, loc.row),"\n")
  
  interval.begin <- ifelse((loc.row - interval.range/2) < 1, 1,(loc.row - interval.range/2))
  interval.end <- ifelse((loc.row + interval.range/2) > nrow(locations.df), nrow(locations.df),(loc.row + interval.range/2))
  interval.range <- interval.end - interval.begin + 1
  
  #cat("Interval chosen - begin:",interval.begin,"end:",interval.end,"range:",interval.range,"\n")
  stop.dist <- data.frame(loc.row=interval.begin:interval.end, 
                          dist=rep(.Machine$double.xmax,interval.range),
                          lon=rep(0,interval.range),
                          lat=rep(0,interval.range),
                          time=rep(ymd("1996-06-08"),interval.range))
  #cat("Locations df size:",nrow(locations.df),"Stop Dist df size:",nrow(stop.dist),"\n")
  
  for (i in interval.begin:interval.end) {
    stop.dist[stop.dist$loc.row == i,] <- c(i,get.stop.location.dist(locations.df, stops.df, st.row, i),
                                              locations.df[i,c("longitude","latitude","timestamp")])
  }
  
  stop.dist <- stop.dist %>% arrange(dist)
  #cat("Final Match between location row #", stop.dist[1,c("loc.row")],"and stop row #",
      #st.row,"with distance:",stop.dist[1,c("dist")],"m\n")
  #print(stop.dist)
  return(stop.dist)
}

# estimate.stop.time <- function(matching.interval,stops.df,stop.row,estimation.range) {
#   matching.loc.row <- matching.interval[1,c("loc.row")]
#   trip.interval <- arrange(matching.interval,loc.row)
#   
#   est.interval.begin <- ifelse((matching.loc.row - estimation.range + 1) < 1,1,(matching.loc.row - estimation.range + 1))
#   est.interval.end <- ifelse((matching.loc.row + estimation.range - 1) > nrow(matching.interval),nrow(matching.interval),
#                              (matching.loc.row + estimation.range - 1))
#   
#   delta.space <- distHaversine(trip.interval[est.interval.begin,c("longitude","latitude")],
#                               trip.interval[est.interval.end,c("longitude","latitude")])
#   delta.time <- difftime(trip.interval[est.interval.begin,c("timestamp")],
#                          trip.interval[est.interval.end,c("timestamp")],
#                          units="secs")
#   
#   mean.speed <- delta.space/delta.time
#   
#   stop.dist <- distHaversine(trip.interval[matching.loc.row,c("longitude","latitude")],
#                              stops.df[stop.row,c("stop_lon","stop_lat")])
# }

#' Matches stops to GPS locations for a trip 
#'
#' 
#'
#' @param trip.locations.df data frame with GPS locations for a single trip 
#' @param stops.locations.df line stops locations data frame
#' @param init.stop.seq number of initial stop sequence for trip
#' @param verbose if TRUE then debugging logs are printed during processing 
#'
#' @return data frame with matched stops and GPS locations data
#'
#' @examples
#'
#' @export
match.trip.locations.stops <- function(trip.locations.df, stops.locations.df, init.stop.seq, verbose=FALSE) {
  cat("\nMatching GPS and GTFS data for trip#",trip.locations.df[1,]$trip.num,"\n")
  
  stops.locations.df$location.match <- NA
  max.optimal.dist.threshold <- 30
  max.acceptable.dist.threshold <- 100
  location.row <- 1
  #indice <- 1
  
  trip.locations.df <- ungroup(trip.locations.df)
  
  for (stop.row.num in 1:nrow(stops.locations.df)) {
    stop.row <- ((stop.row.num + (init.stop.seq - 2)) %% nrow(stops.locations.df)) + 1
    if (location.row >= nrow(trip.locations.df)) {
      break;
    }
    
    next.locs.dist <- data.frame(row.num = 1:nrow(trip.locations.df),dist=rep(.Machine$double.xmax,nrow(trip.locations.df)))
    curr.dist <- get.stop.location.dist(trip.locations.df, stops.locations.df, stop.row, location.row)
    
    while(curr.dist > max.optimal.dist.threshold) {
      next.locs.dist[location.row,] <- c(location.row,curr.dist)
      #indice <- location.row
      if (location.row >= nrow(trip.locations.df)) {
        break
      }
      
      location.row <- location.row + 1
      curr.dist <- get.stop.location.dist(trip.locations.df, stops.locations.df, stop.row, location.row)
    }
    
    if (curr.dist > max.optimal.dist.threshold) {
      if (verbose) cat("Could not find matching location to stop#",stop.row,"with dist < 30\n")
      next.locs.dist <- arrange(next.locs.dist,dist)
      #if (verbose) cat("Last row compared:",indice,"num.location.rows:",nrow(trip.locations.df),"\n")
      if (verbose) cat("Last row compared:",location.row,"num.location.rows:",nrow(trip.locations.df),"\n")
      #indice <- next.locs.dist[1,1]
      #location.row <- indice
      location.row <- next.locs.dist[1,1]
      curr.dist <- next.locs.dist[1,2]
      if (verbose) {
        cat("Ordered location distances:\n")
        print(next.locs.dist[1:5,1:2])
      }
      
      if (curr.dist > max.acceptable.dist.threshold) {
        if (verbose) cat("Distance to stop #",stop.row,"=",curr.dist,"above acceptable threshold.\n")
        print("It seems the trip is shorter than usual. Check the data tables for more details.")
        print("Exiting matching!")
        break
      }
    }
    
    matching.interval <-refine.match(locations.df = trip.locations.df, stops.df = stops.locations.df, 
                       loc.row = location.row, st.row = stop.row, interval.range = 20)
    if (verbose) cat("Match between stop#",stop.row,"and location#",location.row,"with distance=",
                     get.stop.location.dist(trip.locations.df,stops.locations.df,stop.row,location.row),"m\n")
    stops.locations.df[stop.row, c("location.match")] <- matching.interval[1,c("loc.row")]
  }
  
  trip.locations.df$location.id <- 1:nrow(trip.locations.df)
  matched.stops <- merge(x = stops.locations.df,y = trip.locations.df,by.x = "location.match", by.y="location.id")
  
  select(matched.stops, stop_id, stop_sequence, stop_name, stop_lat, stop_lon, arrival_time, route_short_name, route_long_name,
         codveiculo, latitude, longitude, timestamp, trip.num)
}

#' Returns initial stop sequence for GPS bus trajectory
#'
#' This function compares bus trajectory points to line stops points to find the initial point of the trajectory. If the initial point matches more than one stop, a disambiguation process is performed.
#'
#' @param trajectory.location.data data frame with GPS locations for a bus trajectory 
#' @param line.stops.df line stops locations data frame
#' @param verbose if TRUE then debugging logs are printed during processing 
#'
#' @return first stop-matched point in trajectory
#'
#' @examples
#'
#' @export
find.trip.initial.stop.seq <- function(trajectory.location.data, line.stops.df, verbose=FALSE) {
  trip.stops <- line.stops.df
  initial.stop.seq <- numeric()
  
  if (verbose) {
    print(initial.stop.seq)
    cat("#Location Rows:",nrow(trajectory.location.data),"\n")
  }
  
  location.row <- 1
  while(length(initial.stop.seq) == 0) {
    if (location.row > nrow(trajectory.location.data)) {
      break
    }
    
    trip.stops$match.initial.point <- points.match(p1.lat = trip.stops$stop_lat,
                                                   p1.lon = trip.stops$stop_lon,
                                                   p2.df = trajectory.location.data[location.row,c("longitude","latitude")],
                                                   threshold = 30)
    if (verbose) {
      cat("Location Row#",location.row,"Stop Sequence = ", trip.stops[trip.stops$match.initial.point,]$stop_sequence,"\n")
    }
    initial.stop.seq <- trip.stops[trip.stops$match.initial.point,c("stop_sequence")]
    
    if (length(initial.stop.seq) > 1) {
      same.stop <- TRUE
      for (i in 1:(length(initial.stop.seq)-1)) {
        same.stop <- same.stop & (trip.stops[trip.stops$stop_seq == initial.stop.seq[i],"stop_id"] ==
                                    trip.stops[trip.stops$stop_seq == initial.stop.seq[(i+1)],"stop_id"])
      }
      
      if (same.stop) {
        if (verbose) cat("Initial stops are the same (have the same stop id). Keeping the first one.")
        initial.stop.seq <- initial.stop.seq[1]
      } else {
        if (verbose) cat("Initial stops are not the same (have different stop ids). Starting disambiguation.")
        initial.stop.seq <- disambiguate.matched.stop(location.row,initial.stop.seq,trajectory.location.data,trip.stops)
      }
    }
    location.row <- location.row + 1
  }
  
  return(initial.stop.seq)
}

#' Disambiguates matched-stop locations
#'
#' This function analyzes points which matched the same stop to find which one best fits the line trip
#'
#' @param location.row matched location row
#' @param initial.stops.sequence vector with matched stops
#' @param trip.locations.df data frame with GPS locations
#' @param trips.stops.df data frame with stops points
#'
#' @return best-matching point
#'
#' @examples
#'
#' @export
disambiguate.matched.stop <- function(location.row, initial.stops.sequence, trip.locations.df, trips.stops.df) {
  curr.location.row <- location.row + 1
  num.comparisons <- 3
  max.optimal.dist.threshold <- 30
  num.matches.df <- data.frame(initial.stop.seq=numeric(),num.matches=numeric())
  for (init.stop.seq in initial.stops.sequence) {
    num.matches <- 0
    for (stop.row.num in 1:num.comparisons) {
      stop.row <- ((stop.row.num + (init.stop.seq - 1)) %% nrow(trips.stops.df)) + 1
      
      curr.dist <- get.stop.location.dist(trip.locations.df, trips.stops.df, stop.row, curr.location.row)
      
      while(curr.dist > max.optimal.dist.threshold) {
        
        if (curr.location.row >= nrow(trip.locations.df)) {
          break
        }
        
        curr.location.row <- curr.location.row + 1
        curr.dist <- get.stop.location.dist(trip.locations.df, trips.stops.df, stop.row, curr.location.row)
      }
      
      if (curr.dist <= max.optimal.dist.threshold) {
        num.matches <- num.matches + 1
      }
    }
    num.matches.df <- rbind(num.matches.df,data.frame(init.stop.seq,num.matches))
  }
  
  return(num.matches.df[which.max(num.matches.df$num.matches),c("initial.stop.seq")])
}

#' Matches stops to GPS locations for a bus 
#'
#' This function splits bus GPS locations by trip and finds trips initial point to call match.trip.locations.stops
#'
#' @param bus.locations.df data frame with GPS locations for a single bus 
#' @param line.stops.df line stops locations data frame
#' @param verbose if TRUE then debugging logs are printed during processing 
#'
#' @return data frame with matched stops and GPS locations data
#'
#' @examples
#'
#' @export
match.bus.locations.stops <- function(bus.locations.df,line.stops.df,verbose=FALSE) {
  cat("\n\nMatching locations for bus:",as.character(bus.locations.df[1,]$codveiculo),"\n\n")
  
  matched.stops <- data.frame()
  
  if (nrow(bus.locations.df) < 100) {
    cat("Current bus has too few GPS observations:", nrow(bus.locations.df))
    return(data.frame())
  }
  
  initial.stop.seq <- find.trip.initial.stop.seq(bus.locations.df,line.stops.df,verbose)
  
  if (length(initial.stop.seq) == 0) return(matched.stops)
  
  #Retrieving Trip Initial Points
  trip.initial.point <- data.frame(longitude=line.stops.df[initial.stop.seq,]$stop_lon, latitude=line.stops.df[initial.stop.seq,]$stop_lat)
  trip.initial.points <- get.trips.initial.points(bus.locations.df, trip.initial.point)

  #Eliminating repeated points at the same location (stop)
  trip.initial.points <- mutate(trip.initial.points, dist.between.trips = difftime(timestamp, lag(timestamp), units = "mins"))
  trip.initial.points <- filter(trip.initial.points, (is.na(dist.between.trips) | (dist.between.trips > 30)))
  
  if (verbose) cat("#Trips:",nrow(trip.initial.points))
  
  #Assigning trip number to each trip
  trip.initial.points$trip.num<-seq.int(nrow(trip.initial.points))
  
  #Assigning trip number to each location
  bus.locations.df <- bus.locations.df %>% rowwise() %>% mutate(trip.num = get.trip.number(timestamp, trip.initial.points))
  
  #Matching trip stops
  matched.stops <- bus.locations.df %>% ungroup() %>% group_by(trip.num) %>%
    do(match.trip.locations.stops(trip.locations.df = .,
                                  stops.locations.df = line.stops.df,
                                  init.stop.seq = initial.stop.seq,verbose)) %>%
    rbind(matched.stops,.)
  
  n.matched.trips <- length(unique(matched.stops$trip.num))
  n.trips <- length(unique(bus.locations.df$trip.num))
  
  matched.stops$num.trips <- rep(n.trips,nrow(matched.stops))
  matched.stops$num.matched.trips <- rep(n.matched.trips,nrow(matched.stops))
  
  return(matched.stops)
}

#' Returns longest trip among line trips
#'
#'
#'
#' @param line line code
#' @param stops.df all lines stops data frame
#'
#' @return trip with the greatest number of stops among line trips
#'
#' @examples
#'
#' @export
get.line.longest.trip <- function(line,stops.df) {
  line.stops.detailed <- filter(stops.df, route_short_name == line)
  
  longest.trip <- line.stops.detailed %>% group_by(trip_id) %>% summarise(num.stops = n()) %>%
    arrange(num.stops) %>% filter(row_number() == n())
  
  line.stops.detailed <- filter(line.stops.detailed, trip_id == longest.trip$trip_id)
  
  return(line.stops.detailed)
}

#' Matches stops to GPS locations for a line 
#'
#' This function splits line GPS locations by bus and filters line stops to call match.bus.locations.stops. Finally, matched trips with size shorter than 1/3 of longest trip are removed and rows are sorted by bus code, trip number and timestamp.
#'
#' @param line.location.data data frame with GPS locations for a single line
#' @param stops.df stops locations data frame
#' @param verbose if TRUE then debugging logs are printed during processing 
#'
#' @return data frame with matched stops and GPS locations data
#'
#' @examples
#'
#' @export
match.line.locations.stops <- function(line.location.data,stops.df,verbose=FALSE) {
  line <- as.character(line.location.data[1,]$codlinha)
  
  line.matched.stops <- data.frame()
  
  cat("\n\nMatching locations for line:",line,"\n\n")
  
  line.stops.detailed <- filter(stops.df, route_short_name == line)
  
  line.trip <- get.line.longest.trip(line,stops.df)
  if(nrow(line.trip) == 0) {
    cat("\nThere are no trip records for line",line,"in GTFS.\nExiting matching.\n")
    return(line.matched.stops)
  }
  
  line.matched.stops <- line.location.data %>% group_by(codveiculo) %>%
    do(match.bus.locations.stops(bus.locations.df = ., line.stops.df=line.trip,verbose)) %>%
    rbind(line.matched.stops,.)
  
  line.matched.stops <- line.matched.stops %>% group_by(codveiculo, trip.num) %>%
    mutate(num.matched.stops = n()) %>% filter(num.matched.stops >= nrow(line.trip)/3)
  
  line.matched.stops <- line.matched.stops %>% group_by(codveiculo, trip.num) %>% arrange(codveiculo,trip.num,timestamp)
  
  return(line.matched.stops)
}


#' Matches stops to GPS locations 
#'
#' This function splits GPS locations by line and orders the locations by timestamp to call match.line.locations.stops.
#'
#' @param location.data data frame with all GPS locations
#' @param stops.df stops locations data frame
#' @param verbose if TRUE then debugging logs are printed during processing 
#'
#' @return data frame with matched stops and GPS locations data
#'
#' @examples
#'
#' @export
match.locations.stops <- function(location.data,stops.df,verbose=FALSE) {
  matches <- location.data %>% group_by(codlinha) %>% 
    arrange(timestamp) %>%
    do(match.line.locations.stops(.,stops.df,verbose))
  return(matches)
}

#' Reads and pre-processes GPS data
#'
#' This function reads GPS data from a csv file and parses the timestamps into POSIXct date-time objects
#'
#' @param bus.gps.csv.file.path GPS data csv file path
#'
#' @return data frame with GPS data
#'
#' @examples
#'
#' @export
prepare.gps.data <- function(bus.gps.csv.file.path) {
  location.data <- read.csv(bus.gps.csv.file.path)
  location.data$timestamp <- parse_date_time(location.data$data, "ymd HMS", tz = "GMT-3")
  
  return(location.data)
}

#' Reads and pre-processes stops data
#'
#' This function reads GTFS data from a csv file, assembles the necessary information and organizes it into a data frame for simpler usage.
#'
#' @param gtfs.folder.path GTFS folder path
#'
#' @return data frame with stops data
#'
#' @examples
#'
#' @export
prepare.stops.data <- function(gtfs.folder.path) {
  #Reading, assembling and organizing GTFS data
  stop.times <- read.csv(paste(gtfs.folder.path,"stop_times.txt",sep="/"))
  trips <- read.csv(paste(gtfs.folder.path,"trips.txt",sep="/"))
  routes <- read.csv(paste(gtfs.folder.path,"routes.txt",sep="/"))
  stops <- read.csv(paste(gtfs.folder.path,"stops.txt",sep="/"))
  
  stops.detailed <- merge(stop.times,trips)
  stops.detailed <- merge(stops.detailed,routes)
  stops.detailed <- merge(stops.detailed,stops)
  
  stops.detailed <- stops.detailed %>%
    select(trip_id, arrival_time, stop_id, stop_sequence, service_id, trip_headsign, direction_id, route_id, route_short_name, route_long_name,
           route_type, route_color, stop_name, stop_lat, stop_lon) %>%
    arrange(route_short_name, trip_id, stop_sequence)
  
  return(stops.detailed)
}