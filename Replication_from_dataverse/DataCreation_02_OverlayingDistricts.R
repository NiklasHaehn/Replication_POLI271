###################################################################################################
# Far From Home                      ###############################################
# Jesse Crosson and Jaclyn Kaslovsky ###############################################
###################################################################################################

#This file takes the list of legislator home towns created in DataCreation_01
#and overlays them on top of shapefiles. The result is a dataframe that matches
#EVERY legislator to the districts in each congress, noting which district they were born in
#We then match the legislator to the appropriate congress in the next file, DataCreation_03.

###################################################################################################
# Working Directory and Packages ##################################################################
###################################################################################################

setwd("~/Dropbox/Local Roots/Replication Package New/Intermediate Data/")

library(leaflet)
library(tigris)
library(maptools)
library(sp)
library(rgdal)
library(shapefiles)
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)
library(foreign)
library(rgeos)
library(readstata13)
library(sf)
library(mapview)
library(raster)
library(rgeos)

###################################################################################################
# Load in the data  ###################################################################
###################################################################################################

data <- read.csv("hometowns_allcongresses_updated.csv")
data <- data[!is.na(data$lon | data$lat),]
data2 <- data
coordinates(data2)<-~lon+lat 


#Run a loop to load in all of the shapefiles and output which districts members were from 
all_data <- data.frame()
for (i in 1:116){

    shape_dat <- unzip(paste("Shapefiles/districts0",i,".zip",sep=""))
  
  if (i<10) {
    shape_file <- readOGR(paste("districtShapes/districts00",i,".shp",sep=""))
  } else if (i>9 & i <100) {
    shape_file <- readOGR(paste("districtShapes/districts0",i,".shp",sep=""))
  } else if (i>=100 & i <115) {
    shape_file <- readOGR(paste("districtShapes/districts",i,".shp",sep=""))
  } else if (i==115) {
    shape_file <- readOGR("districtShapes/cb_2017_us_cd115_500k.shp")
  } else if (i==116) {
    shape_file <- readOGR("districtShapes/cb_2019_us_cd116_500k.shp")
  } 

###################################################################################################
# Make the shapefiles have the same coordinate systems  ###########################################
###################################################################################################
  

  proj4string(data2)<- CRS("+proj=longlat +datum=WGS84")
  shape_file <- spTransform(shape_file, CRS("+proj=longlat +datum=WGS84"))
  
###################################################################################################
# Use "over" to overlay the coordinates onto districts  ############################################
###################################################################################################
  
  if (i>=115) {
    names(shape_file)[names(shape_file) == "GEOID"] <- "ID"
  } 
  
  
  districts_towns <- over(shape_file,data2, returnList = TRUE)
  names(districts_towns) <-as.character(shape_file$ID)
  
  total  <- ldply(districts_towns, data.frame)
  colnames(total)[1] <- "ID"
  total$ID <- as.character(total$ID)
  shape_file$ID <- as.character(shape_file$ID)
  districts <- as.data.frame(shape_file)
  
  if (i>=115) {
      districts <- districts[,c(1,2,4)]
  } 
  
  
  districts <- districts[,1:3]
  
  total <- inner_join(total,districts, by=c("ID"))
  final <- left_join(data,total)
  final$cong <- i
  all_data <- bind_rows(all_data,final)
}

all_data$DISTRICT[all_data$cong==115] <- as.numeric(as.character(all_data$CD115FP[all_data$cong==115]))
all_data$DISTRICT[all_data$cong==116] <- as.numeric(as.character(all_data$CD116FP[all_data$cong==116]))

save(all_data, file = "Overlayed_Districts.RData")


