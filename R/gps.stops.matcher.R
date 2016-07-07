require(dplyr)
require(lubridate)
require(geosphere)
require(lazyeval)

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
  
  if (verbose) cat("Number of locations for trip: ",nrow(trip.locations.df),"\n")
  
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
      if (verbose) cat("Last row compared:",location.row,"num.location.rows:",nrow(trip.locations.df),"\n")
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
         bus.code, latitude, longitude, timestamp, trip.num)
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
find.trip.initial.stop.seq <- function(trajectory.location.data, initial.stops.df, line.stops.df, verbose=FALSE) {
  trip.initial.stops <- initial.stops.df
  initial.stop.seq <- numeric()
  
  if (verbose) {
    cat("#Location Rows:",nrow(trajectory.location.data),"\n")
  }
  
  #print(line.stops.df[,c("stop_id","stop_sequence")])
  
#   print("Trip Initial Stops:")
#   print(trip.initial.stops)
  
  location.row <- 1
  while(length(initial.stop.seq) == 0) {
    if (location.row > nrow(trajectory.location.data)) {
      break
    }
    
    trip.initial.stops$point.dist <- distHaversine(trip.initial.stops[,c("stop_lon","stop_lat")],
                                           trajectory.location.data[location.row,c("longitude","latitude")])
    
    
    
    trip.initial.stops <- trip.initial.stops %>% ungroup() %>% arrange(point.dist)
    
    initial.stop.id <- trip.initial.stops[1,]$stop_id
    initial.stop.seq <- (line.stops.df %>% filter(stop_id == initial.stop.id) %>% select(stop_sequence))$stop_sequence
    # initial.stop.seq <- trip.stops[trip.stops$match.initial.point,c("stop_sequence")]
    
    if (verbose) {
      #print(trajectory.location.data[location.row,c("longitude","latitude")])
      #print(trip.initial.stops)
      # cat("Location Row#",location.row,"Stop Sequence = ", trip.stops[trip.stops$match.initial.point,]$stop_sequence,"\n")
      cat("Location Row#",location.row,"Stop Sequence = ", initial.stop.seq,"\n")
    }
    
    if (length(initial.stop.seq) > 1) {
      same.stop <- TRUE
      for (i in 1:(length(initial.stop.seq)-1)) {
        same.stop <- same.stop & (line.stops.df[line.stops.df$stop_seq == initial.stop.seq[i],"stop_id"] ==
                                    line.stops.df[line.stops.df$stop_seq == initial.stop.seq[(i+1)],"stop_id"])
      }
      
      if (same.stop) {
        if (verbose) cat("Initial stops are the same (have the same stop id). Keeping the first one.")
        initial.stop.seq <- initial.stop.seq[1]
      } else {
        if (verbose) cat("Initial stops are not the same (have different stop ids). Starting disambiguation.")
        initial.stop.seq <- disambiguate.matched.stop(location.row,initial.stop.seq,trajectory.location.data,line.stops.df)
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
match.bus.locations.stops <- function(bus.locations.df,line.stops.df,line.initial.stops.df,verbose=FALSE) {
  cat("\n\nMatching locations for bus:",as.character(bus.locations.df[1,]$bus.code),"\n\n")
  
  matched.stops <- data.frame()
  
  if (nrow(bus.locations.df) < 100) {
    cat("Current bus has too few GPS observations:", nrow(bus.locations.df))
    return(matched.stops)
  }
  
  initial.stop.seq <- find.trip.initial.stop.seq(bus.locations.df,line.initial.stops.df,line.stops.df,verbose)
  
  if (length(initial.stop.seq) == 0) return(matched.stops)
  
  #Retrieving Trip Initial Points
  trip.initial.point <- data.frame(longitude=line.stops.df[initial.stop.seq,]$stop_lon, latitude=line.stops.df[initial.stop.seq,]$stop_lat)
  trip.initial.points <- get.trips.initial.points(bus.locations.df, trip.initial.point)
  if (nrow(trip.initial.points) == 0) {
    #trip.initial.points <- bus.locations.df[1,]
    cat("\nCould not find initial stop in bus trips. Stopping matching for bus.\n")
    return(matched.stops)
  }

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
  line <- as.character(line.location.data[1,]$line.code)
  
  line.matched.stops <- data.frame()
  
  cat("\n\nMatching locations for line:",line,"\n\n")
  
  line.stops.detailed <- filter(stops.df, route_short_name == line)
  
  line.trip <- get.line.longest.trip(line,stops.df)
  if(nrow(line.trip) == 0) {
    cat("\nThere are no trip records for line",line,"in GTFS.\nExiting matching.\n")
    return(line.matched.stops)
  }
  
  line.initial.stops <- get.line.initial.stops(stops.df, line=line)
  
  line.matched.stops <- line.location.data %>% 
    group_by(bus.code) %>%
    do(match.bus.locations.stops(bus.locations.df = ., 
                                 line.stops.df=line.trip,
                                 line.initial.stops.df=line.initial.stops,
                                 verbose)) %>%
    rbind(line.matched.stops,.)
  
  if (nrow(line.matched.stops) == 0) {
    cat("\nNo matches were found for line",line,"\n") 
    return(line.matched.stops)
  }
  
  line.matched.stops <- line.matched.stops %>% group_by(bus.code, trip.num) %>%
    mutate(num.matched.stops = n())
  # %>% filter(num.matched.stops >= nrow(line.trip)/3)
  
  line.matched.stops <- line.matched.stops %>% group_by(bus.code, trip.num) %>% arrange(bus.code,trip.num,timestamp)
  
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
  matches <- location.data %>% group_by(line.code) %>% 
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
prepare.gps.data.old <- function(bus.gps.csv.file.path) {
  location.data <- read.csv(bus.gps.csv.file.path)
  names(location.data) <- c("bus.code","latitude","longitude","timestamp","line.code")
  location.data$timestamp <- parse_date_time(location.data$timestamp, "ymd HMS", tz = "GMT-3")
  
  return(location.data)
}

#' pre-processes GPS data
#'
#' This function parses GPS data timestamps into POSIXct date-time objects
#'
#' @param gps.data GPS observations data frame
#'
#' @return data frame with GPS data
#'
#' @examples
#'
#' @export
prepare.gps.data <- function(gps.data) {
  location.data <- gps.data
  location.data$timestamp <- parse_date_time(location.data$timestamp, "ymd HMS", tz = "GMT-3")
  
  return(location.data)
}

fix.timestamp <- function(timestamp) {
  splitted.time <- strsplit(timestamp,":")[[1]]
  splitted.time[1] <- ifelse(splitted.time[1] == "24","00",splitted.time[1])
  return(paste(splitted.time,collapse=":"))
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
    select(trip_id, arrival_time, departure_time, stop_id, stop_sequence, service_id, trip_headsign, direction_id, route_id, route_short_name, route_long_name,
           route_type, route_color, stop_name, stop_lat, stop_lon) %>%
    arrange(route_short_name, trip_id, stop_sequence)
  
  return(stops.detailed)
}

#' Retrieve initial stop data of each trip for each line
#'
#'  This function groups the stops by route_id and trip_id, and then returns the data of the first stop in each trip 
#'
#' @param stops.df GTFS stop times data frame
#'
#' @return data frame with initial stop data for each trip 
#'
#' @examples
#'
#' @export
get.trips.initial.stops <- function(stops.df, s_id = 1) {
  trips.initial.stops <- stops.df %>% 
    group_by(route_short_name, trip_id) %>% 
    mutate(num.stops = n()) %>%
    filter(stop_sequence == 1 & service_id == s_id) %>%
    mutate(departure_time = fix.timestamp(as.character(departure_time)))
  
  return(trips.initial.stops)
}

get.line.initial.stops <- function(stops.df, line, s_id = 1) {
  trips.initial.stops <- stops.df %>% 
    filter(route_short_name == line & (stop_sequence == 1 & service_id == s_id)) %>%
    group_by(stop_id,stop_lon,stop_lat) %>%
    summarise(count = n())
  return(trips.initial.stops)
}

set.column.name.prefix <- function(column.name, prefix) {
  new.column.name <- paste(prefix,str_replace(column.name,"_","."),sep=".")
}

match.scheduled.trip <- function(scheduled.trip, observed.trips, matching.type=1, verbose=FALSE)  {
  if (verbose) cat("Matching scheduled trip # ",scheduled.trip$sch.trip.id,"\n")
  obs.trips.comparison <- observed.trips
  
  if (matching.type == 1) {
    obs.trips.comparison <- obs.trips.comparison %>% 
      rowwise() %>%
      mutate(same.initial.stop = (obs.stop.id == scheduled.trip$sch.stop.id),
             time.diff = abs(difftime(obs.timestamp, scheduled.trip$sch.departure.time, units = "mins"))) %>%
      filter(same.initial.stop & time.diff < 30 & (!obs.paired))
  } else if (matching.type == 2) {
    obs.trips.comparison <- obs.trips.comparison %>% 
      rowwise() %>%
      mutate(time.diff = abs(difftime(obs.timestamp, scheduled.trip$sch.departure.time, units = "mins"))) %>%
      filter(time.diff < 30 & (!obs.paired))
  } else {
    return(data.frame())
  }
  
  obs.trips.comparison <- obs.trips.comparison %>%
    ungroup() %>%
    arrange(obs.timestamp) %>%
    filter(row_number() == 1)
}

match.line.trips <- function(line.observed.trips, line.scheduled.trips, matching.type=1, verbose=FALSE) {
  curr.line <- as.character(line.observed.trips[1,]$line.code)
  
  cat("Matching trips for line:",curr.line,"\n")
  
  obs.trips <- line.observed.trips
  sch.trips <- line.scheduled.trips %>% filter(as.character(route_short_name) == curr.line)
  obs.trips$trip.id <- 1:nrow(obs.trips)
  obs.trips$paired <- FALSE
  sch.trips$paired <- FALSE
  
  names(obs.trips) <- set.column.name.prefix(names(obs.trips),"obs")
  names(sch.trips) <- set.column.name.prefix(names(sch.trips),"sch")
  
  matched.trips <- data.frame()
  #Optimized Test
  for (i in seq(1:nrow(sch.trips))) {
    match <- match.scheduled.trip(sch.trips[i,],obs.trips,matching.type,verbose)
    if (nrow(match) == 1) {
      obs.trips[obs.trips$obs.trip.id == match$obs.trip.id,c("obs.paired")] = TRUE
      sch.trips[i,c("sch.paired")] = TRUE
      matched.trips <- rbind(matched.trips,cbind(match,sch.trips[i,]))
    }
  }
  
  unmatched.sch.trips <- sch.trips %>% ungroup %>% filter(!sch.paired)
  unmatched.obs.trips <- obs.trips %>% ungroup %>% filter(!obs.paired)
  
  matched.trips <- plyr::rbind.fill(matched.trips,unmatched.sch.trips)
  matched.trips <- plyr::rbind.fill(matched.trips,unmatched.obs.trips)
  
  return(matched.trips)
}

match.trips <- function(scheduled.trips.initial.stops, observed.trips.initial.stops,matching.type=1,verbose=FALSE) {
  matched.trips <- data.frame()
  matched.trips <- observed.trips.initial.stops %>% 
    ungroup() %>%
    group_by(line.code) %>%
    do(match.line.trips(.,scheduled.trips.initial.stops,matching.type,verbose)) %>%
    ungroup() %>%
    rbind(matched.trips,.)
}

#' Builds a map with gps points locations for a specified line and bus
#'
#'  This function filters the input GPS data points, selecting observations with the specified line and bus codes and plots these points in a Google Map.
#'
#' @param city.name City name where points are located
#' @param gps.data GPS data frame with GPS points observations
#' @param lcode line code whose observations should be plotted
#' @param bcode bus code whose observations should be plotted
#' @param num.points number of points to be plotted (in ascending order of appearance)
#'
#' @return plot with selected line, bus and number of points GPS data
#'
#' @examples
#'
#' @export
plot.gps.data <- function(city.name, gps.data, lcode, bcode, num.points=NULL) {
  selected.gps.data <- gps.data %>% filter(line.code == lcode & bus.code == bcode)
  if (!missing(num.points)) {
    selected.gps.data <- selected.gps.data %>% head(num.points)
  }
  map <- qmap(city.name, zoom = 12, maptype = 'hybrid')
  plot <- map + geom_point(data = selected.gps.data, aes(x = longitude, y = latitude), color="blue", size=3, alpha=0.5)
  return(plot)
}


plot.stops.data <- function(city.name, stops.data, lcode, trip.id, num.points=NULL) {
  selected.stops.data <- stops.data %>% filter(route_short_name == lcode & trip_id == trip.id)
  if (!missing(num.points)) {
    selected.gps.data <- selected.gps.data %>% head(num.points)
  }
  map <- qmap(city.name, zoom = 12, maptype = 'hybrid')
  plot <- map + geom_point(data = selected.stops.data, aes(x = stop_lon, y = stop_lat), color="blue", size=3, alpha=0.5)
  return(plot)
}