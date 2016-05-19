# gps-stops-matcher
An R package to help matching GPS bus trajectories and GTFS bus stops. 



## Usage
Notice: It is expected that the GPS data table will be formatted like the example below:  

| bus.code| latitude  | longitude | date                | line.code |
| --------|----------:| ---------:| :------------------:| --------: |
| AA028   | -25.40741 | -49.25299 | 2015-10-19 07:36:28 |       211 |
| AA028   | -25.40733 | -49.25303 | 2015-10-19 07:36:26 |       211 |
| AA028   | -25.40704 | -49.25291 | 2015-10-19 07:36:20 |       211 |

Notice: It is expected that the GTFS folder contains the following files: 

* stops.txt  
* routes.txt  
* stop_times.txt  
* trips.txt  
  
```
location.data <- prepare.gps.data(bus.gps.csv.file.path = "<your-gps-data-file-path>")
stops.data <- prepare.stops.data(gtfs.folder.path = "<gps-folder-path>")
matches <- match.locations.stops(location.data,stops.data,verbose=TRUE)
```

## Installation
```
library(devtools)
install_github("analytics-ufcg/gps.stops.matcher")
library(gps.stops.matcher)
```
