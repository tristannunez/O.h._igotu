library(raster)
library(rgdal)
library(lubridate)
library(bcpa)

setwd("F:/deer_igotu_github/github/O.h._igotu") # set your local working directory
wd<- getwd()
collarfiles<-paste(wd, "./raw_csv_data/", dir("./raw_csv_data"), sep="")

# read in the deer files
options(digits=15) # this ensures that all the decimal places are read in from lat/long readings
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

# bind these together because they're from the same animal ... presumably theres's some data in here that's not on-animal data? I'm not sure why the B2 data is in 2 csv files
B2 <- rbind(B2_0, B2_1)

# time tag was deployed
# this clearly includes points prior to deployment if use "2017-02-14 07:58:00" as the start time
# keeping it at 09:30 until clarify the time zone issue
# need to confirm the timezone in which the units are recording
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
writeOGR(B2.spdf, dsn="./shapefiles" ,layer="B2_points", driver="ESRI Shapefile", overwrite_layer = T)
# to turn the movement track into a long line
B2.sl<-SpatialLines(list(Lines(list(Line(coordinates(B2.spdf))),"X"))) 
# have to turn into a spatial lines data frame to write out as shapefile
B2.sldf<-SpatialLinesDataFrame(B2.sl, data=data.frame(animalid="B2"), match.ID=F)
# write out as shapefile
writeOGR(B2.sldf, dsn="./shapefiles" ,layer="B2_line", driver="ESRI Shapefile", overwrite_layer = T)


# Begin to look at movement metrics; this code heavily borrows from Eli Gurarie's wiki 
# project to UTM; units are now meters rather than decimal degrees
deer.utm <- spTransform(B2.spdf, CRS("+proj=utm +north +zone=10 +ellps=WGS84")) # need to confirm UTM zone
m<-deer.utm
m$local.time <- m$unit.time.POSIXct
m.coords<- coordinates(m)
X <- m.coords[,1] # gives the x coordinates
Y <- m.coords[,2]# gives y coordinates

dX<-diff(X) # change in longitude
min(dX[dX>0]) # smallest movement without no movement
median(dX[dX>0]) # median movement that wasn't 0
max(dX[dX>0]) #  B2 moved 35 meters in 5 seconds! 
sum(dX==0)/length(dX) # number of no-movement fixes

dY <- diff(Y) # change in latitude
sum(dY==0)/length(dY)
min(dY[dY>0])
median(dY[dY>0])
max(dY[dY>0])

sum(dY==0)==sum(dX==0) # this returns True, which doesn't make sense to me. It means that between fixes, if there was a change in x there was always a change in y.
plot(X[1:200], Y[1:200])
plot(X[dX==0], Y[dY==0]) # this is a plot of all the places where there was no change in x or y from the previous fix; highlights resting locations

Time <- m$local.time # timestamp
mytrack <- MakeTrack(X,Y,Time) # special format from BCPA package
plot(mytrack)
Simp <- mytrack
Z <- complex(re=Simp$X, im=Simp$Y) # creates complex numbers 

dZ <- diff(Z) # change in trajectory
Phi <- Arg(dZ) # forget what this is; absolute direction? 
Theta <- diff(Phi) #this gives turn angles
absTheta <- abs(Theta) # this gives absolute turn angles 
stTheta<-absTheta # creat copy
stTheta[which(stTheta>pi)]<-pi-(stTheta[stTheta>pi]-pi)#standardize turn angle to between 0 and pi
S <- Mod(dZ) # this gives step length

S <- Mod(dZ)
sum(S==0)/length(S) # according to this, 79% of fixes had no change in location ... the fact that this number is the same as 
min(S[S>0])
median(S[S>0])
median(S)
max(S[S>0])

dT <- as.numeric(diff(Simp$T))*60 # this gives change in time with each step 
V <- S/dT # this gives velocity 

#  comparing time of day 

deer.utm$day <- day(deer.utm$unit.time.POSIXct)
deer.utm$hour <- hour(deer.utm$unit.time.POSIXct)
# general relationship of velocity to time of day
plot(log(V[V!=0])~ deer.utm$hour[-1][V!=0])

# set up two-panel graphic device
par(mfrow=c(1,2))
# plot by day of month
plot(deer.utm, col=as.numeric(deer.utm$day), pch=".")

# plot by time of day (quick way of doing this) 
deer.utm.night <- deer.utm[deer.utm$hour>= 20 & deer.utm$hour <=24,]
deer.utm.earlyam <- deer.utm[deer.utm$hour>= 0 & deer.utm$hour <= 5,]
deer.utm.am <- deer.utm[deer.utm$hour>=6  & deer.utm$hour <=8,]
deer.utm.day <- deer.utm[deer.utm$hour>=9 & deer.utm$hour <=16,]
deer.utm.eve <- deer.utm[deer.utm$hour>=17 & deer.utm$hour <=19,]

plot(deer.utm, col="white")
plot(deer.utm.night, add=T, pch=".")
plot(deer.utm.earlyam, add=T, col="green", pch=".")
plot(deer.utm.am, add=T, col="blue", pch=".")
plot(deer.utm.day, add=T, col="orange", pch=".")
plot(deer.utm.eve, add=T, col="purple", pch=".")
# turn off graphic device
dev.off()

# classify time of day in the spdf
deer.utm$tod<-rep("unclassified", length(deer.utm$unit.time.POSIXct))
deer.utm$tod[deer.utm$hour>= 20 & deer.utm$hour <=24]<- "night_20_24"
deer.utm$tod[deer.utm$hour>= 0 & deer.utm$hour <= 5] <- "earlyam_0_5"
deer.utm$tod[deer.utm$hour>=6  & deer.utm$hour <=8] <- "dawn_6_8"
deer.utm$tod[deer.utm$hour>=9 & deer.utm$hour <=16] <- "day_9_16"
deer.utm$tod[deer.utm$hour>=17 & deer.utm$hour <=19]<- "dusk_17_19"

# write out as shapefile - note this is in utm unlike the other shapefiles
writeOGR(deer.utm, dsn="./shapefiles" ,layer="B2_utm_tod", driver="ESRI Shapefile", overwrite_layer = T)



