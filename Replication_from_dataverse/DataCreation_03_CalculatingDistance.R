###################################################################################################
# Far From Home                      ###############################################
# Jesse Crosson and Jaclyn Kaslovsky ###############################################
###################################################################################################

#This file loads in the data file that has already matched legislators' home towns to congressional districts and 
#writes a function to calculate the closest distance to any point on the district's border. First, however, it matches
#the data to a list of congressional members from voteview, so that we only look at members in the appropriate congress.
#We also have to fix some overlapping polygons in the shapefiles. 

###################################################################################################
# Working Directory and Packages ##################################################################
###################################################################################################

setwd("~/Dropbox/Local Roots/Replication Package New/Intermediate Data/")
library(lfe)
library(haven)
library(dplyr)
library(leaflet)
library(tigris)
library(maptools)
library(sp)
library(rgdal)
library(shapefiles)
library(reshape2)
library(foreign)
library(rgeos)
library(readstata13)
library(sf)
library(mapview)
library(raster)
library(rgeos)
library(geosphere)
library(measurements)
library(geosphere)

#load in the Data
load("Overlayed_Districts.RData")

#fix some column names so we are clear on what is the representing district and what is the born district
colnames(all_data)[14] <- "congress"
colnames(all_data)[13] <- "district_born"

#make the district born variable numeric
all_data$district_born <- as.numeric(as.character(all_data$district_born))

#remove the district overlap variables that are just for 115th and 116th
#congress - already replaced these in the district column
all_data <- all_data[,-c(16:17)]

###################################################################################################
# Fix Duplicates  ###################################################################
###################################################################################################

#remove observations which matched members to the at large district,
#even if they represented a different one
all_data <- all_data %>% 
  group_by(bioguide_id,congress) %>% 
  mutate(n=n(),
         minval = min(district_born))
all_data <- all_data[!(all_data$minval==0 & all_data$n>1 & all_data$district_born==0),]


#fix the overlapping polygons by removing incorrect duplicate observations 

#Tennessee 
all_data <- all_data[!(all_data$STATENAME=="Tennessee" & (all_data$congress==97 | all_data$congress==96 | all_data$congress==95) &
                         (all_data$hometown_final=="Memphis, Shelby County, TN" |  
                            all_data$hometown_final=="Collierville, Shelby County, TN"|
                            all_data$hometown_final=="Memphis, TN" |
                            all_data$hometown_final=="Memphis, Lauderdale County, TN"  |
                            all_data$hometown_final=="Shelby County, TN")
                       & all_data$district_born!=8),] 

#South Carolina
all_data <- all_data[!((all_data$congress<=92 & all_data$congress>=89) &
                         (all_data$hometown_final=="Westminster, Oconee County, SC" |  
                            all_data$hometown_final=="Walhalla, SC"|
                            all_data$hometown_final=="Fair Forest Creek, SC")
                       & all_data$STATENAME!="South Carolina"),] 

#MA
all_data <- all_data[!((all_data$congress<=48 & all_data$congress>=38) &
                         (all_data$hometown_final=="Fall River, MA" |  
                            all_data$hometown_final=="Fall River, Bristol County, MA")
                       & all_data$STATENAME!="Massachusetts"),] 

#Note that in new york in the 1806 elections it seems like new york had two seats for one district and counted
# it as two districts - the 2nd and 3rd https://en.wikipedia.org/wiki/1806_United_States_House_of_Representatives_elections_in_New_York

#Kentucky
all_data$kentucky <- sapply("KY|Kentucky|Ky", grepl, all_data$hometown_final)
all_data <- all_data[!(all_data$kentucky==TRUE & all_data$congress==2 & all_data$STATENAME=="Virginia"),]

#Virginia
all_data$virginia <- sapply("VA", grepl, all_data$hometown_final)
all_data <- all_data[!(all_data$virginia==TRUE & all_data$congress==2 & all_data$STATENAME=="Kentucky"),]

#fix CARLIN, Charles Creighton	who was incorrectly matched to the 9th district in KY
all_data <- all_data[!(all_data$bioguide_id=="C000151" & all_data$district_born==9),]


#add in statenames for the later congresses
data(fips_codes)
fips_codes <-unique(fips_codes[c("state_name", "state_code")])
colnames(all_data)[15] <- "state_code"

all_data <- left_join(all_data, fips_codes, by="state_code")
all_data$STATENAME[is.na(all_data$STATENAME)] <- all_data$state_name[is.na(all_data$STATENAME)]
all_data <- all_data[,-20]

###################################################################################################
# Load and Merge in Controls  ###################################################################
###################################################################################################

library(tidyverse)
voteview <- read_csv("Hall_members.csv")
voteview <- voteview[voteview$chamber=="House",]
voteview <- voteview[voteview$congress<117,]

#merge the data 
full <- left_join(voteview, all_data,by=c("congress","bioguide_id"))

#need to mark at large districts as 0 instead of 1 but also want to leave room in case someone took over someone elses seat
full <- full %>% 
  group_by(congress,state_abbrev) %>% 
  dplyr::mutate(n_dist=n(),
                maxval = max(district_code))

#note that 2 is the max value of n_dist
full$district_code[full$maxval==1  & full$n_dist<=2] <- 0
full$district_code[full$district_code==98] <- 0
full$district_code[full$district_code==99] <- 0

#get rid of unecessary columns - "state_code", "n", "minval", "kentucky",  "virginia",  "n_dist", "maxval"                       
full<- full[,-c(35:41)]
colnames(full)[33] <- "state_born"

#replace -1 in georgia to be statewide ticket
full$district_code[full$district_code==0 & full$state_abbrev=="GA" & full$congress==20] <- -1

#before we geocode, lets check if anyone from abroad was matched incorrectly 
#stateab <- paste(unique(state_name$state_abbrev),collapse="|")
#statename <- paste(unique(state_name$state.name),collapse="|")
#check <- full[!grepl("County|county", full$hometown_2) & !grepl(stateab, full$hometown_2) & !grepl(statename, full$hometown_2) & !grepl(statename, full$hometown_final) & !grepl(stateab, full$hometown_final) & is.na(full$state),]

###################################################################################################
# Calculate Distances  ###################################################################
###################################################################################################

#subsetting to members not born in the district they represent - want to calculate distance 
full$state_represent <- state.name[match(full$state_abbrev,state.abb)]
full$born_code <- paste(full$district_born, full$state_born, sep="")
full$represent_code  <- paste(full$district_code, full$state_represent, sep="")

not_born <- full[!(full$born_code==full$represent_code & !is.na(full$district_born)),]
not_born <- not_born[!(is.na(not_born$lon) | is.na(not_born$lat)),]

#load in fips codes and match to the dataset 
data(fips_codes)
fips_codes <-unique(fips_codes[c("state_name", "state_code")])
state_name <- data.frame(state.name)
state_name$state_abbrev <- state.abb
colnames(fips_codes)[1] <- "state.name"
state_name <- left_join(state_name, fips_codes)

not_born <- left_join(not_born,state_name)

#create a new id variable that is just the state name and district variable
not_born$newid <- paste(not_born$state.name, not_born$district_code, sep=",")
not_born$newid[not_born$congress>114] <- paste(not_born$state_code[not_born$congress>114], sprintf("%02d",not_born$district_code[not_born$congress>114]), sep="")

#need to load in all shapefiles into a list
dir <- "~/Dropbox/Local Roots/Replication Package/Intermediate Data/districtShapes/"
shps <- list.files(dir, pattern="\\.shp$", full.names=TRUE)
shplist <- lapply(shps, readOGR) 

#need a loop to turn each district into a polygon and overlay with the congressional district
output_distances <- data.frame()
output_distances_final <- data.frame()

#replaced someone who has no district in jeff lewis data
not_born <- not_born[not_born$bioname!="GILMER, George Rockingham",]

for (i in 1:nrow(not_born)){
  
  #subset to the legislator of interest
  legislator <- data.frame(not_born[i,])
  
  #subset to the appropriate congress in the list of shapefiles and load in that shapefile
  k <- not_born$congress[i] 
  shape_file <- shplist[[k]]
  
  #make a newid variable that will help us with identifying the district in the shapefile
  if (k<115) {
    shape_file$newid <- paste(shape_file$STATENAME, shape_file$DISTRICT, sep=",")
  }else {  
    names(shape_file)[names(shape_file) == "GEOID"] <- "newid"
  } 
  
  #limit to the correct state
  if (k<115){
  shape_file <- shape_file[shape_file@data$STATENAME==legislator$state_represent,]
  }else {  
    shape_file <- shape_file[shape_file@data$STATEFP==legislator$state_code,]
  } 
  
  #get a district variable for the later congresses
  if (k<115){
    shape_file <- shape_file
  }else {  
    shape_file$DISTRICT <-as.numeric(str_sub(shape_file@data$newid,-2,-1))
  } 
  
  #if in an at large district make sure looking at the whole state
  if(legislator$district_code==0 & min(shape_file@data$DISTRICT)==-1){
    shape_file <- shape_file 
  }else{
    shape_file <- shape_file[shape_file@data$newid==legislator$newid,]
  }  
  
  #get the coordinates of the district from the shapefile
  shape_file <- spTransform(shape_file, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #select the group of polygons that's larger
  results_intermediate <- list()
  results_main <- list()
  for(j in 1:length(shape_file@polygons)){
    for(l in 1:length(shape_file@polygons[[j]]@Polygons))
    {
      results <- list()
      results[[l]] <- shape_file@polygons[[j]]@Polygons[[l]]@coords
      results <- as.data.frame(Reduce(rbind, results))
      results_intermediate <- bind_rows(results_intermediate, results)
    }
    results_main <-  bind_rows(results_main, results_intermediate)
  }
  
  dist<-distGeo(legislator[, c("lon", "lat")], results_main[, c("V1", "V2")])/1609.35
  
  output_distances <- data.frame(min(dist))
  
  
  output_distances_final <- bind_rows(output_distances_final, output_distances)
  print(i)
}  

#recombine the people not born in the district with the outputted distances and the entire dataset 
not_born <- bind_cols(not_born, output_distances_final)
full$min.dist1 <- 0
full <- left_join(full, not_born)

#give people born in the district a 0 for the distance measure. However, account for people who couldnt be geocoded
full$min.dist.[full$min.dist1==0 & is.na(full$min.dist.) & !is.na(full$lon) & !is.na(full$lat)] <- 0
full <- full[,-c(36:37)]

#save the dataset
save(full, file = "FinalOutput_WithContinuous.RData")

