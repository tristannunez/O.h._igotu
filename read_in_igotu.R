library(raster)
library(rgdal)
library(lubridate)

setwd("F:/Chp3_trails/deer_tracks/reb2track")
collarfiles<-dir()
cdfw.deer.csvs<-lapply(collarfiles, FUN=read.csv)

str(cdfw.deer.csvs)

names(cdfw.deer.csvs[[1]])
cdfw.deer.csvs[[1]][1,]

# function to pull out collar ids from each csv
#ListCollarIds<- function (X){
#  unique(X[,2])
#}
# make sure in this table that each collar id is only represented once
#table(unlist(lapply(cdfw.deer.csvs, FUN=ListCollarIds)))
#length(unique(unlist(lapply(cdfw.deer.csvs, FUN=ListCollarIds)))) # should be 27 (only downloaded collars with over 300 points)

X<-cdfw.deer.csvs[[1]]

Sys.timezone()

CollarFixRates <- function(X){
  unit.time <-X[,"Time"]
  unit.date <-X[,"Date"]
  unit.datetime <- paste(unit.date, unit.time, sep="")
  unit.time.POSIXct<- as.POSIXct(strptime(unit.datetime, "%Y/%m/%d %H:%M:%S"), tz="UTC") # what timezone are igotu units recording in?
  difftimes <-difftime(unit.time.POSIXct[-1],unit.time.POSIXct[-length(unit.time.POSIXct)], units="secs")
  data.frame(median_diff=median(difftimes), min_diff=min(difftimes), max_diff=max(difftimes))
}

durations<-unlist(lapply(cdfw.deer.csvs, FUN=CollarFixRates)) # gives fix rates
durations

ExtractCollarData <- function (X){
  id<- "B2" # X[,"Device.ID"]  # hard-coding in for now
  study.name<- "Hopland Igotu" #X[,"Device.Name"]
  unit.time <-X[,"Time"]
  unit.date <-X[,"Date"]
  unit.datetime <- paste(unit.date, unit.time, sep="")
  unit.time.POSIXct<- as.POSIXct(strptime(unit.datetime, "%Y/%m/%d %H:%M:%S"), tz="UTC") # what timezone are igotu units recording in?
  lat <-X[,"Latitude"]
  lon <-X[,"Longitude"]
  #lotek.fix.status <- X[,"Fix.Status"]
  #lotek.dop <- X[,"DOP"]
  data.frame(id=id, unit.time.POSIXct=unit.time.POSIXct, lat=lat, lon=lon)
}

extracted.list <- lapply(cdfw.deer.csvs, FUN=ExtractCollarData)

B2_021417start_0<-extracted.list[[1]]
B2_021417start_1 <-extracted.list[[2]]

B2_021417start_0$lat
B2_021417start_0$lon

B2_0<- B2_021417start_0
B2_1<-B2_021417start_1 

head(B2_0)

?coordinates

coordinates(B2_0) <- ~lon+lat
proj4string(B2_0) <- CRS("+proj=longlat +ellps=WGS84")



plot(B2_0)
index<- B2_0$unit.time.POSIXct > as.POSIXct(strptime("2017-02-14 09:30:00", "%Y-%m-%d %H:%M:%S", tz="UTC"), tz="UTC")
B2_0_s <- B2_0[index,]
plot(B2_0_s, add=T, col="red")

coordinates(B2_1) <- ~lon+lat
proj4string(B2_1) <- CRS("+proj=longlat +ellps=WGS84")


blue.ras<-raster("F:/hopland_imagery/GE_test/B2_test1.tif", band=3)
plot(blue.ras)
plot(B2_0_s, add=T, col="green", pch=".")
plot(B2_1, add=T, col="blue", pch=".")

plot(B2_1, pch=".")
plot(blue.ras, add=T)
plot(B2_1, add=T)
plot(B2_0, add=T, col="red")


names(B2_0)
class(B2_0)
B2_0$pointid<-1:length(B2_0)

?Lines

# create list of lines of water movements. this is 11 lists of lines corresponding to the 11 hippos with in water movements.
Lines_list<-list()
Lines_data<- data.frame(num=1:length(unique(startpoints$id)), id=B2_0$id)
for(id in unique(startpoints$id)){
  start_subset <-startpoints[startpoints$id==id,]
  end_subset <- endpoints[endpoints$id==id,]
  list1 <- list()
  for (i in 1:length(start_subset$x)){
    x<- c(start_subset$x[i], end_subset$x[i])
    y<- c(start_subset$y[i], end_subset$y[i])
    list1[[length(list1)+1]] <- Line(cbind(x,y))
  }
  Lines_list[[length(Lines_list)+1]]<- Lines(list1, ID=id)
}
# create spatial lines objects
water_lines <- SpatialLines(Lines_list,proj4string=CRS("+proj=utm +north +zone=37 +ellps=WGS84"))

# create spatial lines dataframe
Lines_data<- data.frame(row.names=row.names(water_lines),num=1:length(unique(startpoints$id)), id=unique(startpoints$id) )
water_lines.sldf <- SpatialLinesDataFrame(water_lines, data=Lines_data)















plot(Lines(list(Line(coordinates(B2_1))),"z"))
?SpatialLinesDataFrame






B2_1_L<-SpatialLines(list(Lines(list(Line(coordinates(B2_1))),"X"))) 
plot(B2_1_L)

B2_0_L<-SpatialLines(list(Lines(list(Line(coordinates(B2_0_s))),"X"))) 
plot(B2_0_L)


plot(blue.ras)
plot(B2_1_L, add=T)
plot(B2_0_L, add=T)
plot(blue.ras, add=T)
plot(B2_1, add=T)
plot(B2_0, add=T, col="red")

dir("../")

B2_1_L_sldf<-SpatialLinesDataFrame(B2_1_L, data=data.frame(fakedata="fake"), match.ID=F)
writeOGR(B2_1_L_sldf, dsn="../tracks" ,layer="../B2_1_line", driver="ESRI Shapefile")

B2_0_L_sldf<-SpatialLinesDataFrame(B2_0_L, data=data.frame(fakedata="fake"), match.ID=F)
crs(B2_0_L_sldf) <- crs(B2_0)
writeOGR(B2_0_L_sldf, dsn="../tracks" ,layer="../B2_0_line6", driver="ESRI Shapefile")


coordinates(B2_0_s)


readback<-readOGR("../B2_0_line2.shp")
plot(readback, col="red")





