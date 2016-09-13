#source("gps.stops.matcher.R")

# Constants

MIN_TRIP_SIZE = 10
ACCEPTABLE_DIST_THRESHOLD = 50
MAX_TIMEDIFF_BETWEEN_POINTS = 1800

# Functions

get.closest.point.in.line <- function(point,line, lon.col.index, lat.col.index) {
    distances <- data.frame(shape_pt_sequence=line$shape_pt_sequence, dist=distHaversine(c(point[[lon.col.index]],point[[lat.col.index]]),line[,c("shape_pt_lon","shape_pt_lat")]))
    closest.point <- distances %>% 
        arrange(dist) %>% 
        head(1)
    
    return(cbind(point,closest.point))
}

project.gps.to.shape <- function(gps.data,shape.data, lon.col.index, lat.col.index) {
    projection <- gps.data %>% 
        rowwise() %>%
        do(get.closest.point.in.line(.,shape.data, lon.col.index, lat.col.index))
    return(projection)
}

get.secs.since.midnight <- function(time) {
    secs.since.midnight <- 3600*hour(time) + 60*minute(time) + second(time)
}

secs.to.timestamp <- function(secs,date) {
    days <- floor(secs/86400)
    hours <- floor(secs/3600) %% 24
    minutes <- floor((secs%%3600)/60) %% 60
    seconds <- ((secs%%3600)%%60) %% 60
    
    timestamp.str <- paste(paste(year(date),month(date),day(date),sep="-"),
                           paste(hours,minutes,seconds,sep=":"))
    
    timestamp <- ymd_hms(timestamp.str,tz = tz(date)) + days(days)
    
    return(timestamp)
}

get.column.index <- function(df, col.name) {
    return (which(names(df)==col.name))
}

get.line.shapes <- function(stops.data, line.code) {
    line.shapes <- stops.data %>%
        filter(route_short_name == line.code) %>%
        distinct(shape_id)
    return(line.shapes)
}

match.gps.trajectory.to.shape <- function(shapes.data,line.shapes.ids,gps.trajectory, gps.data.row, strict.matching=FALSE) {
    DEF_MAXIMUM_DIST <- 200
    
    gps.trip <- gps.trajectory %>% arrange(timestamp)
    
    shapes.initial.points <- shapes.data %>%
        filter(shape_id %in% line.shapes.ids$shape_id) %>%
        group_by(shape_id) %>%
        filter(row_number() == 1) 
    
    closest.shape <- data.frame(point.dist = NA, shape_id = -1)
    max.dist <- ifelse(strict.matching,DEF_MAXIMUM_DIST,.Machine$double.xmax)
    
    while (gps.data.row <= nrow(gps.trajectory) & (is.na(closest.shape$point.dist) | first(closest.shape$point.dist) > max.dist)){
        
        shapes.initial.points$point.dist <- distHaversine(shapes.initial.points[,c("shape_pt_lon","shape_pt_lat")],
                                                          gps.trajectory[gps.data.row,c("longitude","latitude")])
        
        closest.shape <- shapes.initial.points %>% 
            ungroup() %>% 
            arrange(point.dist) %>%
            head(1)
        
        gps.data.row <- gps.data.row +1
    }
    
    return(list(shape.id = closest.shape$shape_id, gps.row = gps.data.row-1))
}

estimate.trajectory.time <- function(trip.projected.gps.data,line.shape,verbose=FALSE) {
    enhanced.line.shape <- left_join(line.shape,trip.projected.gps.data,by=c("shape_pt_sequence" = "shape.pt.match")) %>% 
        group_by(shape_pt_sequence) %>% 
        arrange(matching.dist) %>%
        filter(row_number() == 1) %>%
        ungroup()  %>%
        arrange(shape_pt_sequence) %>%
        mutate(timestamp.secs = get.secs.since.midnight(timestamp),
               bus.code = as.character(bus.code))
    
    # last.trip.timestamp <- max(enhanced.line.shape$timestamp, na.rm = TRUE)
    
    if(verbose) print("Creating regression model")
    
    enhanced.line.shape.train <- enhanced.line.shape %>%
        filter(!is.na(bus.code))
    
    enhanced.line.shape.test <- enhanced.line.shape %>%
        filter(is.na(bus.code))
    
    enhanced.line.model <- lm(timestamp.secs ~ shape_dist_traveled, data = enhanced.line.shape.train)
    # summary(enhanced.line.model)
    enhanced.line.shape.test$timestamp.secs <- predict.lm(object=enhanced.line.model,newdata=enhanced.line.shape.test)
    
    enhanced.line.est.shape <- rbind(enhanced.line.shape.train,enhanced.line.shape.test) %>% 
        arrange(shape_pt_sequence)
}

estimate.trip.arrival.times <- function(trip.projected.gps.data,shapes.data,stops.data,trip.shape.matches, verbose=FALSE) {
    curr.trip.num <- head(trip.projected.gps.data,1)$trip.num
    if(verbose) cat("\nEstimating trip arrival times for trip#",curr.trip.num,"\n")
    trip.metadata <- trip.shape.matches %>% filter(trip.num == curr.trip.num)
    
    line.shape <- shapes.data %>% filter(shape_id == trip.metadata$shape.id)
    longest.line.trip <- stops.data %>% filter(trip_id == trip.metadata$longest.trip.id)
    trip.gps.date <- first(trip.projected.gps.data$timestamp)
    trip.bus.code <- first(trip.projected.gps.data$bus.code)
    trip.line.code <- first(trip.projected.gps.data$line.code)
    
    estimated.trajectory.time <- estimate.trajectory.time(trip.projected.gps.data,line.shape)
    
    if(verbose) print("Projecting stops data")
    
    projected.stops.data <- project.gps.to.shape(longest.line.trip,estimated.trajectory.time, 
                                                 get.column.index(longest.line.trip, "stop_lon"),  
                                                 get.column.index(longest.line.trip, "stop_lat") ) 
    
    projected.stops.data[1,]$shape_pt_sequence <- estimated.trajectory.time[1,]$shape_pt_sequence
    
    projected.stops.data <- projected.stops.data %>%
        select(stop_id,stop_lon,stop_lat,stop_sequence,stop_name,shape_pt_sequence) %>%
        merge(estimated.trajectory.time) %>%
        mutate(arrival_time = secs.to.timestamp(timestamp.secs,trip.gps.date),
               trip.num = curr.trip.num,
               bus.code = trip.bus.code,
               line.code = trip.line.code) %>%
        # filter(arrival_time <= last.trip.timestamp) %>%
        arrange(arrival_time)
    
    return (projected.stops.data)
}

get.prev.var <- function(bus.gps.data,curr.row,trip.first.row,curr.trip.num,var.name) {
    prev.var <- NA
    if ((curr.row == 1) | (curr.row == trip.first.row)) {
        prev.var <- -1
    } else if (bus.gps.data[curr.row-1,]$trip.num < curr.trip.num) {
        prev.var <- -1
    }  else {
        prev.var <- unname(unlist(bus.gps.data[curr.row-1,get.column.index(bus.gps.data,var.name)]))
    }
    
    return(prev.var)
}

match.point.to.shape <- function(bus.gps.data,line.shape,curr.row,trip.first.row) {
    matching.line.shape <- NA
    
    if (curr.row == trip.first.row) {
        matching.line.shape <- line.shape[1,]
    } else {
        matching.line.shape <- line.shape
    }
    
    curr.pt.shape.match <- get.closest.point.in.line(bus.gps.data[curr.row,],
                                                     matching.line.shape,
                                                     get.column.index(bus.gps.data, "longitude"),  
                                                     get.column.index(bus.gps.data, "latitude"))
}

count.anomalous.points <- function(bus.gps.data,line.shape,gps.data.row,prev.seq,checking.range,acc.dist.threshold) {
    num.going.backwards.pts <- 0
    num.inconsistent.pts <- 0
    
    for(i in (gps.data.row+1):(gps.data.row+checking.range)) {
        if (i > nrow(bus.gps.data)) {
            found.new.trip = FALSE
        } else {
            closest.shape.pt <- match.point.to.shape(bus.gps.data,line.shape,i,i-1)
            
            pt.seq <- closest.shape.pt$shape_pt_sequence
            pt.matching.dist <- closest.shape.pt$dist
            
            if ((pt.seq - prev.seq) < 0) num.going.backwards.pts <- num.going.backwards.pts + 1
            # cat("\nCurr Pt:",pt.seq,"; Prev Pt:",prev.seq,"Diff:",pt.seq - prev.seq,"Row#",i,"\n")
            
            if (pt.matching.dist > acc.dist.threshold) {
                num.inconsistent.pts <- num.inconsistent.pts + 1
            }
        }
    }
    
    return(list(num.going.backwards.pts=num.going.backwards.pts,num.inconsistent.pts=num.inconsistent.pts))
}

check.point.range <- function(bus.gps.data,line.shape,gps.data.row,prev.seq,checking.range=10,acc.dist.threshold=50,verbose=FALSE) {
    found.new.trip <- FALSE
    consistent.pt <- TRUE
    
    anomalous.pts <- count.anomalous.points(bus.gps.data,line.shape,gps.data.row,prev.seq,checking.range,acc.dist.threshold)
    num.going.backwards.pts <- anomalous.pts$num.going.backwards.pts
    num.inconsistent.pts <- anomalous.pts$num.inconsistent.pts
    
    if ((num.going.backwards.pts == checking.range) | (num.inconsistent.pts == checking.range)) { #I'm sure this trip ended
        if(verbose) print("Found new trip - number of going-back/inconsistent points exceeded!")
        found.new.trip <- TRUE
    } else { #Inconsistent GPS occurrence found
        consistent.pt <- FALSE
    }
    
    return(list(found.new.trip=found.new.trip,consistent.pt=consistent.pt))
}

delimitate.current.trip <- function(bus.gps.data,line.shape,trip.first.row,curr.trip.num,verbose=FALSE) {
    found.new.trip <- FALSE
    gps.data.row <- trip.first.row
    
    while((gps.data.row <= nrow(bus.gps.data)) & (!found.new.trip)) {
        consistent.pt <- TRUE
        
        curr.pt.shape.match <- match.point.to.shape(bus.gps.data,line.shape,gps.data.row,trip.first.row)
        
        prev.seq <- get.prev.var(bus.gps.data,gps.data.row,trip.first.row,curr.trip.num,"shape.pt.match")
        prev.time <- get.prev.var(bus.gps.data,gps.data.row,trip.first.row,curr.trip.num,"timestamp")
        
        curr.seq <- curr.pt.shape.match$shape_pt_sequence
        curr.time <-  bus.gps.data[gps.data.row,]$timestamp
        
        if((prev.time != -1) & (curr.time - prev.time > MAX_TIMEDIFF_BETWEEN_POINTS)){
            if(verbose) print("Found new trip - time limit between consecutive points exceeded!")
            found.new.trip = TRUE
        } else {
            if (((curr.seq - prev.seq) <= 0)) { #Maybe found new trip
                pt.range.checking <- check.point.range(bus.gps.data,line.shape,gps.data.row,prev.seq,verbose = verbose)
                found.new.trip <- pt.range.checking$found.new.trip
                consistent.pt <- pt.range.checking$consistent.pt
            }
            
            if (curr.pt.shape.match$dist > ACCEPTABLE_DIST_THRESHOLD) { consistent.pt <- FALSE }
            if (!consistent.pt) {
                bus.gps.data[gps.data.row,]$consistent.point <- FALSE
                curr.seq <- prev.seq
            }
        }
        
        if (!found.new.trip) {
            bus.gps.data[gps.data.row,c("shape.pt.match","matching.dist","trip.num")] <- 
                c(curr.seq,curr.pt.shape.match$dist,curr.trip.num)
            gps.data.row <- gps.data.row + 1
        }
    }
    trip.last.row <- gps.data.row-1
    if(verbose) cat("\nClosed trip in range: ",trip.first.row,"-",trip.last.row,"size:",trip.last.row-trip.first.row+1,"\n")
    
    return(list(gps.data=bus.gps.data[trip.first.row:trip.last.row,],trip.first.row=trip.first.row,trip.last.row=trip.last.row))
}

identify.bus.trips <- function(bus.gps.data,shapes.data,stops.data,verbose=FALSE) {
    
    curr.line.code <- as.character(first(bus.gps.data$line.code))
    line.shapes <- get.line.shapes(stops.data,curr.line.code)
    
    if (nrow(line.shapes) == 0) {
        print(paste("Found no shapes for current line:",curr.line.code))
        bus.gps.data=data.frame()
        trip.shape.matches=data.frame()
    } else {
        gps.data.row <- 1
        bus.gps.data <- bus.gps.data %>%
            mutate(shape.pt.match = NA,
                   matching.dist = NA,
                   trip.num = NA,
                   consistent.point = TRUE)
        curr.trip.num = 1
        trip.shape.matches <- data.frame(trip.num=numeric(),shape.id=numeric(),longest.trip.id=numeric())
        
        if(verbose) print("Finding trips...")
        
        while(gps.data.row <= nrow(bus.gps.data)){
            trip.shape.match <- match.gps.trajectory.to.shape(shapes.data,line.shapes,bus.gps.data, gps.data.row,
                                                              strict.matching = (curr.trip.num == 1))
            
            gps.data.row <- trip.shape.match$gps.row
            line.shape.id <- trip.shape.match$shape.id
            trip.first.row <- gps.data.row
            
            cat("\nNext Trip: GPS Row:",gps.data.row,"; Line Shape: ",line.shape.id,"\n")
            
            longest.line.trip <- get.line.longest.trip(curr.line.code,stops.data,line.shape.id)
            trip.shape.matches <- rbind(trip.shape.matches,data.frame(trip.num=curr.trip.num,shape.id=line.shape.id,
                                                                      longest.trip.id=first(longest.line.trip$trip_id)))
            line.shape <- shapes.data %>% filter(shape_id == line.shape.id)
            
            trip.gps.data <- delimitate.current.trip(bus.gps.data,line.shape,trip.first.row,curr.trip.num,verbose)
            bus.gps.data[trip.gps.data$trip.first.row:trip.gps.data$trip.last.row,] <- trip.gps.data$gps.data
            curr.trip.num <- curr.trip.num + 1
            gps.data.row <- trip.gps.data$trip.last.row + 1
        }
    }
    
    return(list(bus.gps.data=bus.gps.data,trip.shape.matches=trip.shape.matches))
}

estimate.bus.arrival.times <- function(stops.data, shapes.data, bus.gps.data, verbose=FALSE) {
    cat("\nEstimating arrival times for vehycle",as.character(first(bus.gps.data$bus.code)),":\n")
    
    bus.trips <- identify.bus.trips(bus.gps.data,shapes.data,stops.data,verbose)
    if (nrow(bus.trips$bus.gps.data) == 0) {
        print(paste("Found no trips for bus",first(bus.gps.data$bus.code)))
        return(data.frame())
    }
    
    projected.bus.gps.data <- bus.trips$bus.gps.data %>%
        filter(consistent.point & (!is.na(trip.num)))
    
    # last.trajectory.timestamp <- last(projected.bus.gps.data$timestamp)
    
    selected.projected.bus.gps.data <- projected.bus.gps.data %>%
        group_by(trip.num) %>%
        filter(n() >= MIN_TRIP_SIZE)
    
    if (nrow(selected.projected.bus.gps.data) == 0) {
        print("Bus trips have too few observations.")
        return(data.frame())
    }
    
    # return(selected.projected.bus.gps.data)
    
    projected.bus.stops.data <- selected.projected.bus.gps.data %>%
        do(estimate.trip.arrival.times(.,shapes.data,stops.data,bus.trips$trip.shape.matches, verbose))
    
    #   projected.bus.stops.data <- projected.bus.stops.data %>%
    #     ungroup() %>%
    #     filter(arrival_time <= last.trajectory.timestamp)
    
    return(projected.bus.stops.data)
}

describe.estimated.bat <- function(estimated.bat) {
    est.bat.desc <- estimated.bat  %>%
        group_by(line.code,bus.code,trip.num) %>%
        summarise(trip.size = n(),
                  trip.duration = difftime(last(arrival_time),first(arrival_time),units = "mins"),
                  matched.shape = first(shape_id),
                  trip.departure.time = first(arrival_time),
                  trip.arrival.time = last(arrival_time))
}