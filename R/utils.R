library(lubridate)

get.group.N.min <- function(time,N) {
    mins.since.midnight <- 60*hour(time) + minute(time)
    return(round(mins.since.midnight/N))
}