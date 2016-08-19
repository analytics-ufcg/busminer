require(dplyr)
require(lubridate)
require(geosphere)
require(lazyeval)
require(ggmap)
require(reshape2)
require(stringr)

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
  # print(plot.gps.data("Curitiba",trip.locations.df,"022","BL301") +
  #         geom_point(data=stops.locations.df, aes(x=stop_lon, y=stop_lat), color="green", size=3, alpha=0.5) +
  #         geom_text(data=stops.locations.df, aes(x=stop_lon, y=stop_lat, label=stop_sequence), color="black", fontface="bold", size=3))
  
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
        if (verbose) {
          cat("Distance to stop #",stop.row,"=",curr.dist,"above acceptable threshold.\n")
          cat("Jumping to next stop...\n")
        }
        next
      }
    }
    
    matching.interval <-refine.match(locations.df = trip.locations.df, stops.df = stops.locations.df, 
                       loc.row = location.row, st.row = stop.row, interval.range = 20)
    if (verbose) cat("Match between stop#",stops.locations.df[stop.row, c("stop_id")],"(row#",
                     stop.row,") and location#",location.row,"(timestamp=",format(trip.locations.df[location.row,]$timestamp,"%H:%M:%S"),
                     ") with distance=",get.stop.location.dist(trip.locations.df,stops.locations.df,stop.row,location.row),"m\n")
    stops.locations.df[stop.row, c("location.match")] <- matching.interval[1,c("loc.row")]
  }
  
  trip.locations.df$location.id <- 1:nrow(trip.locations.df)
  matched.stops <- merge(x = stops.locations.df,y = trip.locations.df,by.x = "location.match", by.y="location.id")
  matched.stops$matching.dist <- round(distHaversine(matched.stops[,c("longitude","latitude")],
                                                     matched.stops[,c("stop_lon","stop_lat")]))
  
  cat("Num matched stops:",nrow(matched.stops),"\n")
  
  # map <- qmap('Curitiba', zoom = 12, maptype = 'hybrid') +
  #   geom_point(data = matched.stops, aes(x = longitude, y = latitude), color="blue", size=3, alpha=0.5) +
  #   geom_point(data = matched.stops, aes(x = stop_lon, y = stop_lat), color="green", size=3, alpha=0.5) +
  #   geom_text(data = matched.stops, aes(x = stop_lon, y = stop_lat, label=matching.dist), color="black", fontface="bold", size=4, hjust=-0.5)
  # print(map)
  
  matched.stops <- matched.stops %>% select(stop_id, stop_sequence, stop_name, stop_lat, stop_lon, route_short_name, 
                                            route_long_name, bus.code, latitude, longitude, timestamp, trip.num,matching.dist)
  
  return(matched.stops)
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

find.trip.initial.stop.seq2 <- function(trajectory.location.data, initial.stops.df, line.stops.df, verbose=FALSE) {
  trip.initial.stops <- line.stops.df
  initial.stop.seq <- numeric()
  
  if (verbose) {
    cat("#Location Rows:",nrow(trajectory.location.data),"\n")
    # print(trip.initial.stops)
  }
  
  location.row <- 1
  while(length(initial.stop.seq) == 0) {
    if (location.row > nrow(trajectory.location.data)) {
      break
    }
    
    trip.initial.stops$point.dist <- distHaversine(trip.initial.stops[,c("stop_lon","stop_lat")],
                                                   trajectory.location.data[location.row,c("longitude","latitude")])
    
    trip.initial.stops <- trip.initial.stops %>% ungroup() %>% arrange(point.dist)
    
    if (trip.initial.stops[1,]$point.dist < 30) {
      initial.stop.seq <- trip.initial.stops[1,]$stop_sequence
    }
    
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
  
  # initial.stop.seq <- find.trip.initial.stop.seq(bus.locations.df,line.initial.stops.df,line.stops.df,verbose)
  initial.stop.seq <- find.trip.initial.stop.seq2(bus.locations.df,line.initial.stops.df,line.stops.df,verbose)
  
  if (length(initial.stop.seq) == 0) return(matched.stops)
  cat("Initial stop id:",line.stops.df[initial.stop.seq,]$stop_id,"\n")
  
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
  matched.stops <- bus.locations.df %>% 
    ungroup() %>% 
    group_by(trip.num) %>% 
    filter(trip.num >= 1) %>%
    do(match.trip.locations.stops(trip.locations.df = .,
                                  stops.locations.df = line.stops.df,
                                  init.stop.seq = initial.stop.seq,verbose)) %>%
    ungroup() %>%
    rbind(matched.stops,.)
  
  # n.matched.trips <- length(unique(matched.stops$trip.num))
  # n.trips <- length(unique(bus.locations.df$trip.num))
  # matched.stops$num.trips <- rep(n.trips,nrow(matched.stops))
  # matched.stops$num.matched.trips <- rep(n.matched.trips,nrow(matched.stops))
  
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
get.line.longest.trip <- function(line,stops.df,shape.id=NULL) {
  line.stops.detailed <- filter(stops.df, as.character(route_short_name) == as.character(line))
  if (!missing(shape.id)) line.stops.detailed <- line.stops.detailed %>% filter(shape_id == shape.id)
  
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
    ungroup() %>%
    rbind(line.matched.stops,.)
  
  if (nrow(line.matched.stops) == 0) {
    cat("\nNo matches were found for line",line,"\n") 
    return(line.matched.stops)
  }
  
  line.matched.stops <- line.matched.stops %>%
    ungroup() %>%
    group_by(bus.code, trip.num)
  # %>% mutate(num.matched.stops = n())
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
  fixed.timestamp <- gsub("24:","00:",timestamp)
  return(fixed.timestamp)
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
           route_type, route_color, stop_name, stop_lat, stop_lon, shape_id) %>%
    mutate(arrival_time = fix.timestamp(arrival_time),
           departure_time = fix.timestamp(departure_time)) %>%
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
get.trips.initial.stops <- function(stops.df) {
  trips.initial.stops <- stops.df %>% 
    group_by(route_short_name, trip_id) %>% 
    mutate(num.stops = n()) %>%
    filter(stop_sequence == 1) %>%
    mutate(departure_time = fix.timestamp(as.character(departure_time)))
  
  return(trips.initial.stops)
}

get.line.initial.stops <- function(stops.df, line) {
  trips.initial.stops <- stops.df %>% 
    filter(route_short_name == line & (stop_sequence == 1)) %>%
    group_by(stop_id,stop_lon,stop_lat) %>%
    summarise(count = n())
  return(trips.initial.stops)
}

get.observed.trips.initial.stops <- function(observed.trips) {
  observed.trips.initial.stops <- observed.trips %>% 
    group_by(line.code, bus.code, trip.num) %>% 
    mutate(num.stops = n()) %>%
    arrange(timestamp)  %>%
    filter(row_number() == 1)
  return(observed.trips.initial.stops)
}

get.obs.trips <- function(observed.trips) {
  observed.trips.ids <- observed.trips %>%
    group_by(line.code, bus.code, trip.num) %>% 
    summarise(trip.size = n()) %>%
    ungroup() %>%
    mutate(trip.id = row_number())
}

set.column.name.prefix <- function(column.name, prefix) {
  new.column.name <- paste(prefix,str_replace_all(column.name,"_","."),sep=".")
}

match.trips.by.time <- function(observed.trips, scheduled.trip) {
  obs.trips.comparison <- observed.trips %>% 
    rowwise() %>%
    mutate(time.diff = abs(difftime(obs.timestamp, scheduled.trip$sch.departure.time, units = "mins"))) %>%
    filter(time.diff < 30 & (!obs.paired)) %>%
    ungroup() %>%
    arrange(time.diff)
}

match.trips.by.time.initial.stop.1 <- function(observed.trips, scheduled.trip, dist.threshold = 400, time.diff.threshold = 30) {
  obs.trips.comparison <- observed.trips %>% 
    rowwise() %>%
    mutate(initial.stop.dist = distHaversine(c(obs.longitude,obs.latitude),
                                             scheduled.trip[1,c("sch.stop.lon","sch.stop.lat")]),
           time.diff = abs(difftime(obs.timestamp, scheduled.trip$sch.departure.time, units = "mins"))) %>%
    filter((initial.stop.dist < dist.threshold & time.diff < time.diff.threshold) & (!obs.paired)) %>%
    ungroup() %>%
    arrange(initial.stop.dist,time.diff)
}

match.trips.by.time.initial.stop.2 <- function(observed.trips, scheduled.trip) {
  obs.trips.comparison <- observed.trips %>% 
    rowwise() %>%
    mutate(same.initial.stop = (obs.stop.id == scheduled.trip$sch.stop.id),
           time.diff = abs(difftime(obs.timestamp, scheduled.trip$sch.departure.time, units = "mins"))) %>%
    filter(same.initial.stop & time.diff < 30 & (!obs.paired)) %>%
    ungroup() %>%
    arrange(time.diff)
}

match.trips.by.time.num.stops <- function(observed.trips, scheduled.trip) {
  obs.trips.comparison <- observed.trips %>% 
    rowwise() %>%
    mutate(number.of.stops.diff = abs(obs.trip.size - scheduled.trip$sch.num.stops),
           time.diff = abs(difftime(obs.timestamp, scheduled.trip$sch.departure.time, units = "mins"))) %>%
    filter(time.diff < 30 & (!obs.paired)) %>%
    ungroup() %>%
    arrange(number.of.stops.diff,time.diff)
}

match.trips.by.time.initial.stop.num.stops <- function(observed.trips, scheduled.trip) {
  obs.trips.comparison <- observed.trips %>% 
    rowwise() %>%
    mutate(initial.stop.dist = distHaversine(c(obs.longitude,obs.latitude),
                                             scheduled.trip[1,c("sch.stop.lon","sch.stop.lat")]),
           number.of.stops.diff = abs(obs.trip.size - scheduled.trip$sch.num.stops),
           time.diff = abs(difftime(obs.timestamp, scheduled.trip$sch.departure.time, units = "mins"))) %>%
    filter(initial.stop.dist < 400 & time.diff < 30 & (!obs.paired)) %>%
    ungroup() %>%
    arrange(initial.stop.dist,number.of.stops.diff,time.diff)
}

match.scheduled.trip <- function(scheduled.trip, observed.trips, matching.type=1, verbose=FALSE)  {
  if (verbose) cat("Matching scheduled trip # ",scheduled.trip$sch.trip.id,"\n")
  
  if (matching.type == 1) {
    obs.trips.comparison <- match.trips.by.time(observed.trips,scheduled.trip)
  } else if (matching.type == 2) {
    obs.trips.comparison <- match.trips.by.time.initial.stop.2(observed.trips,scheduled.trip)
  } else if (matching.type == 3) {
    obs.trips.comparison <- match.trips.by.time.initial.stop.1(observed.trips,scheduled.trip)
  } else if (matching.type == 4) {
    obs.trips.comparison <- match.trips.by.time.num.stops(observed.trips,scheduled.trip)
  } else if (matching.type == 5) {
    obs.trips.comparison <- match.trips.by.time.initial.stop.num.stops(observed.trips,scheduled.trip)
  }else {
    return(data.frame())
  }
  
  # print(obs.trips.comparison[1:10,c("initial.stop.dist","time.diff")])
  obs.trips.comparison <- obs.trips.comparison %>% filter(row_number() == 1)
}

match.line.trips <- function(line.observed.trips, line.scheduled.trips, matching.type=1, verbose=FALSE) {
  curr.line <- as.character(line.observed.trips[1,]$line.code)
  
  cat("Matching trips for line:",curr.line,"\n")
  
  obs.trips <- line.observed.trips
  sch.trips <- line.scheduled.trips %>% filter(as.character(route_short_name) == curr.line)
  
  obs.trips$paired <- rep(FALSE,nrow(obs.trips))
  sch.trips$paired <- rep(FALSE,nrow(sch.trips))
  
  names(obs.trips) <- set.column.name.prefix(names(obs.trips),"obs")
  names(sch.trips) <- set.column.name.prefix(names(sch.trips),"sch")
  
  matched.trips <- data.frame()
  #Optimized Test
  for (i in seq(1:nrow(sch.trips))) {
    match <- match.scheduled.trip(sch.trips[i,],obs.trips,matching.type,verbose)
    if (nrow(match) == 1) {
      obs.trips[obs.trips$obs.trip.id == match$obs.trip.id,c("obs.paired")] = TRUE
      sch.trips[i,c("sch.paired")] = TRUE
      matched.trips <- rbind(matched.trips,
                             data.frame(
                               obs.trip.id=match[,c("obs.trip.id")],
                               sch.trip.id=sch.trips[i,c("sch.trip.id")]))
    }
  }
  
  unmatched.sch.trips <- sch.trips %>% ungroup %>% filter(!sch.paired)
  unmatched.obs.trips <- obs.trips %>% ungroup %>% filter(!obs.paired)
  
  matched.trips <- rbind(matched.trips,data.frame(obs.trip.id=rep(NA,nrow(unmatched.sch.trips)),
                                                  sch.trip.id=unmatched.sch.trips$sch.trip.id))
  matched.trips <- rbind(matched.trips,data.frame(obs.trip.id=unmatched.obs.trips$obs.trip.id,
                                                  sch.trip.id=rep(NA,nrow(unmatched.obs.trips))))
  
  return(matched.trips)
}

match.trips.initial.stops <- function(observed.trips.initial.stops, scheduled.trips.initial.stops, matching.type=1,verbose=FALSE) {
  day.service.id <- observed.trips.initial.stops[1,c("service_id")]
  if(verbose) cat("day.service.id:",day.service.id,"\n")
  scheduled.trips <- scheduled.trips.initial.stops %>% filter(service_id == day.service.id)
  
  if(verbose) cat("Scheduled trips size:",nrow(scheduled.trips),"\n")
  
  matched.trips <- data.frame()
  matched.trips <- observed.trips.initial.stops %>% 
    ungroup() %>%
    group_by(line.code) %>%
    do(match.line.trips(.,scheduled.trips,matching.type,verbose)) %>%
    ungroup() %>%
    rbind(matched.trips,.)
  
  return(matched.trips)
}

build.service.ids.df <- function(gtfs.folder.path) {
  file.path <- paste(gtfs.folder.path,"calendar.txt",sep="/")
  service.ids <- melt(read.csv(file.path), id.vars = c("service_id","start_date","end_date")) %>%
    filter(value == 1 & service_id != 4) %>% 
    select(variable, service_id)
  return(service.ids)
}

align.trips <- function(stop.matches,stops.gtfs.data,obs.trip.id,sch.trip.id,verbose=FALSE) {
  aligned.trips <- data.frame()
  obs.trip <- stop.matches %>% filter(trip.id == obs.trip.id)
  sch.trip <- stops.gtfs.data %>% filter(trip_id == sch.trip.id)
  
  obs.trip.row <- 1
  sch.trip.row <- 1
  
  if(verbose) cat("\nAligning obs trip #",obs.trip.id,"(size =",nrow(obs.trip),
                  ") to sch trip",sch.trip.id,"(size =",nrow(sch.trip),").\n")
  while(obs.trip.row <= nrow(obs.trip) & sch.trip.row <= nrow(sch.trip)) {
    aligned.trips <- rbind.data.frame(aligned.trips,
                           cbind.data.frame(obs.trip[obs.trip.row,c("line.code","stop_id","stop_name",
                                                                    "bus.code","trip.id","timestamp",
                                                                    "latitude","longitude")],
                                 sch.trip[sch.trip.row,c("arrival_time","stop_sequence","service_id","stop_lat","stop_lon")]))
    obs.trip.row <- obs.trip.row + 1
    sch.trip.row <- sch.trip.row + 1
  }
  
  return(aligned.trips)
}

match.trips <- function(stop.matches,stops.gtfs.data,service.ids,matching.type=1,verbose=FALSE) {
  match <- list()
  
  if(verbose) print("Parsing timestamp...")
  
  observed.stops <- stop.matches
  observed.stops$timestamp <- parse_date_time(observed.stops$timestamp, "ymd HMS", tz = "GMT-3")
  
  if(verbose) print("Adding service_id to observed trips...")
  
  observed.trips <- get.obs.trips(observed.stops)
  observed.stops <- merge(observed.stops,observed.trips,c("line.code","bus.code","trip.num"))
  
  observed.trips.initial.stops <- observed.stops %>%
    group_by(trip.id) %>% 
    arrange(timestamp) %>% 
    filter(row_number() == 1)
  observed.trips.initial.stops$weekday <- tolower(wday(observed.trips.initial.stops$timestamp, label=TRUE, abbr=FALSE))
  observed.trips.initial.stops <- merge(observed.trips.initial.stops, service.ids, by.x="weekday",by.y="variable")
  
  if(verbose) print("Retrieving scheduled trips initial stops...")
  
  scheduled.stops <- stops.gtfs.data
  scheduled.trips.initial.stops <- get.trips.initial.stops(scheduled.stops)
  
  if(verbose) print("Setting date...")
  
  observed.data.date <- strsplit(as.character(observed.trips.initial.stops[1,]$timestamp)," ")[[1]][1]
  scheduled.trips.initial.stops$departure_time <- parse_date_time(paste(
      observed.data.date, 
      as.character(scheduled.trips.initial.stops$departure_time)),
    "ymd HMS", tz = "GMT-3")
  
  if(verbose) print("Matching trips...")
  
  match$trip.matches <- match.trips.initial.stops(observed.trips.initial.stops,scheduled.trips.initial.stops,matching.type,verbose)
  
  if(verbose) print("Aligning matched trips stops...")
  
  match$stops.matches <- match$trip.matches %>% 
     filter(!(is.na(obs.trip.id) | is.na(sch.trip.id))) %>%
     rowwise() %>%
     do(align.trips(observed.stops,scheduled.stops,.$obs.trip.id,.$sch.trip.id,verbose))
  
  match$matching.type <- matching.type
  
  return(match)
}

get.city.map <- function(city.name) {
  qmap(city.name, zoom = 12, maptype = 'hybrid')
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
plot.gps.data <- function(city.map, gps.data, lcode, bcode, map.zoom=12, range=NULL, point.color="blue") {
  selected.gps.data <- gps.data %>% filter(line.code == lcode & bus.code == bcode)
  if (!missing(range)) {
    selected.gps.data <- selected.gps.data %>% 
      ungroup() %>%
      filter(row_number() %in% range)
  }

  map <- city.map +
    geom_point(data = selected.gps.data, aes(x = longitude, y = latitude), color=point.color, size=3, alpha=0.5) +
    geom_text(data = selected.gps.data, aes(x = longitude, y = latitude, label = format(timestamp,"%H:%M:%S")), color="black", size = 3, vjust = 0, hjust = -0.2)
  return(map)
}


plot.stops.data <- function(city.map, stops.data, lcode, trip.id, num.points=NULL, map.zoom=12) {
  selected.stops.data <- stops.data %>% filter(route_short_name == lcode & trip_id == trip.id)
  if (!missing(num.points)) {
    selected.stops.data <- selected.stops.data %>% head(num.points)
  }
  map <- city.map +
    geom_point(data = selected.stops.data, aes(x = stop_lon, y = stop_lat), color="blue", size=3, alpha=0.5) +
    geom_text(data = selected.stops.data, aes(x = stop_lon, y = stop_lat, label = stop_id), color="black", size = 4, fontface="bold", vjust = 0, hjust = -0.5)
  return(map)
}

plot.stop.matches.data <- function(city.map, matches.data, lcode, bcode, tnum, num.points=NULL, map.zoom=12) {
  selected.stop.matches.data <- matches.data %>% filter(route_short_name == lcode & bus.code == bcode & trip.num == tnum)
  if (!missing(num.points)) {
    selected.stop.matches.data <- selected.stop.matches.data %>% head(num.points)
  }
  
  map <- city.map +
    geom_point(data = selected.stop.matches.data, aes(x = longitude, y = latitude), color="red", size=3, alpha=0.5) +
    geom_point(data = selected.stop.matches.data, aes(x = stop_lon, y = stop_lat), color="blue", size=3, alpha=0.5) +
    geom_text(data = selected.stop.matches.data, aes(x = stop_lon, y = stop_lat, label = stop_id), color="black", size = 4, fontface="bold", vjust = -0.5, hjust = 0) +
    geom_text(data = selected.stop.matches.data, aes(x = longitude, y = latitude, label = format(timestamp,"%H:%M:%S")), color="white", size = 3, fontface="bold", vjust = +0.5, hjust = 0)
  return(map)
}

plot.shape.data <- function(city.map, shapes.data, shape.id, range=NULL, map.zoom=12) {
  selected.shapes.data <- shapes.data %>% filter(shape_id == shape.id)
  if (!missing(range)) {
    selected.shapes.data <- selected.shapes.data %>% filter(row_number() %in% range)
  }
  map <- city.map +
    geom_point(data = selected.shapes.data, aes(x = shape_pt_lon, y = shape_pt_lat), color="blue", size=3, alpha=0.5)
  return(map)
}

plot.estimated.stops.times.data <- function(city.map, matches.data, lcode, bcode, tnum, range=NULL, map.zoom=12) {
  selected.stop.matches.data <- matches.data %>% filter(line.code == lcode & bus.code == bcode & trip.num == tnum)
  if (!missing(range)) {
    selected.stop.matches.data <- selected.stop.matches.data %>% filter(row_number() %in% range)
  }
  
  map <- city.map +
    # geom_point(data = selected.stop.matches.data, aes(x = longitude, y = latitude), color="red", size=3, alpha=0.5) +
    geom_point(data = selected.stop.matches.data, aes(x = stop_lon, y = stop_lat), color="blue", size=3, alpha=0.5) +
    geom_text(data = selected.stop.matches.data, aes(x = stop_lon, y = stop_lat, label = stop_id), color="black", size = 4, fontface="bold", vjust = -0.5, hjust = 0) +
    geom_text(data = selected.stop.matches.data, aes(x = stop_lon, y = stop_lat, label = format(arrival_time,"%H:%M:%S")), color="white", size = 3, fontface="bold", vjust = +0.5, hjust = 0)
  return(map)
}

set.stops.times.date <- function(stops.df, date.str) {
  stops.df.with.date <- stops.df %>% 
    mutate(arrival_time = parse_date_time(paste(date.str, as.character(arrival_time)),"ymd HMS", tz = "GMT-3"),
           departure_time = parse_date_time(paste(date.str, as.character(departure_time)),"ymd HMS", tz = "GMT-3"))
}