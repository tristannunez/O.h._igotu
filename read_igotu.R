library(raster)
library(rgdal)
library(lubridate)

setwd("F:/deer_igotu_github/github/O.h._igotu") # set your local working directory
wd<- getwd()
collarfiles<-paste(wd, "./raw_csv_data/", dir("./raw_csv_data"), sep="")

# read in the deer files
hop.deer.csvs<-lapply(collarfiles, FUN=read.csv)

ExtractCollarData <- function (X){
  id<- "B2" #  # hard-coding in for now; this could vary based on logger file name or other
  unit.time <-X[,"Time"]
  unit.date <-X[,"Date"]
  unit.datetime <- paste(unit.date, unit.time, sep="")
  # translates into POSIXct times
  unit.time.POSIXct<- as.POSIXct(strptime(unit.datetime, "%Y/%m/%d %H:%M:%S"), tz="America/Los_Angeles") # what timezone are igotu units recording in?
  # rename lat and long
  lat <-X[,"Latitude"]
  lon <-X[,"Longitude"]
  # spit out as a dataframe
  data.frame(id=id, unit.time.POSIXct=unit.time.POSIXct, lat=lat, lon=lon)
}

# generate list of extracted collar data
extracted.list <- lapply(hop.deer.csvs, FUN=ExtractCollarData)

# pull out from list
B2_0<-extracted.list[[1]]
B2_1 <-extracted.list[[2]]

# bind these together because they're from the same animal ... presumably theres's some data in here that's not on-animal data?
B2 <- rbind(B2_0, B2_1)

# remove points that are from before the tag was deployed (start time is estimated ... whatis it really?)
start_time <- as.POSIXct(strptime("2017-02-14 09:30:00", "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"), tz="America/Los_Angeles")
index<- B2$unit.time.POSIXct > start_time
B2_on <- B2[index,]

# transform into an spdf
B2.spdf <- B2_on # copy
coordinates(B2.spdf) <- ~lon+lat # creat spdf
proj4string(B2.spdf) <- CRS("+proj=longlat +ellps=WGS84")

# check, takes a while to plot this many points.
plot(B2.spdf, pch=".")

# if you want to write this out as a shapefile ... 
writeOGR(B2.spdf, dsn="./shapefiles" ,layer="B2_points", driver="ESRI Shapefile")

# to turn the movement track into a long line
B2.sl<-SpatialLines(list(Lines(list(Line(coordinates(B2.spdf))),"X"))) 
# have to turn into a spatial lines data frame to write out as shapefile
B2.sldf<-SpatialLinesDataFrame(B2.sl, data=data.frame(animalid="B2"), match.ID=F)
# write out as shapefile
writeOGR(B2.sldf, dsn="./shapefiles" ,layer="B2_line", driver="ESRI Shapefile")

