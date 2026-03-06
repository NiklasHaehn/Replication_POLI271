###################################################################################################
# Far From Home                      ###############################################
# Jesse Crosson and Jaclyn Kaslovsky ###############################################
###################################################################################################

#This file takes the data including the shortest distance from the member's birth place to their district and 
#adds in necessary controls, including party unity data, volden and wiseman data on member characteristics, 
#presidential vote from gary jacobson, and info from the jeff lewis shapefiles on how long the district existed. 

###################################################################################################
# Working Directory and Packages ##################################################################
###################################################################################################

setwd("~/Dropbox/Local Roots/Replication Package New/Intermediate Data/")


require(dplyr)
require(ggplot2)
require(reshape2)
require(tidyr)
require(foreign)
require(rgeos)
require(sf)
require(mapview)
require(usmap)
require(grid)
require(ggpubr)
require(lemon)
require(tidycensus)
require(haven)
require(zoo)
require(lfe)
require(stargazer)
require(rgdal)
require(readr)
require(readxl)

######################################
# Load in the Main data  #############
######################################

load("FinalOutput_WithContinuous.RData")

################################################
# Load in the party unity scores and merge ####
################################################

## Party Unity from VoteView legacy page through 2014 and then created using vote data and function
pu <- read_excel("House_Party_Unity_35-113.xls")

pu2 <- read.csv("newpartyunityscores.csv")
pu <- pu %>% dplyr::select(congress, ICPSR, party_unity)
pu2 <- pu2 %>% dplyr::select(congress, ICPSR, party_unity)
pu <- bind_rows(pu, pu2)
names(pu)[names(pu) == "ICPSR"] <- "icpsr"

#joins by congress and icpsr
full <- left_join(full, pu)

#adjust for states like rhode island that at the time were MA
full$state_born2 <- state.abb[match(full$state_born,state.name)]


#fix members who represent at large districts 
full$min.dist.[full$state_abbrev==full$state_born2 & full$district_code==0] <- 0

#lets get rid of the new addition of state_born2
full <- full %>% select(-c(state_born2))

#####################################
# Load in Member Level Controls  ####
#####################################

les <- read_dta("CELHouse93to116Reduced.dta")

#create a variable for ideological extremism 
full$absextreme <- abs(full$nokken_poole_dim1)


################################################
# Load in Data to Create District Indicators  ##
################################################

#load in the jeff lewis data to get district data 
data_shape_full <- data.frame()
for (i in 1:114){
  
  shape_dat <- unzip(paste("Shapefiles/districts0",i,".zip",sep=""))
  
  if (i<10) {
    shape_file <- readOGR(paste("districtShapes/districts00",i,".shp",sep=""))
  } else if (i>9 & i <100) {
    shape_file <- readOGR(paste("districtShapes/districts0",i,".shp",sep=""))
  } else if (i>=100 & i <115) {
    shape_file <- readOGR(paste("districtShapes/districts",i,".shp",sep=""))
  } 
  
  data_shape <- shape_file@data
  data_shape$congress <- i
  data_shape_full <- bind_rows(data_shape_full, data_shape)
}
data_shape_full <- data_shape_full %>%
  dplyr::select(congress, STATENAME, DISTRICT,STARTCONG,ENDCONG)
data_shape_full <- data_shape_full[data_shape_full$congress>=93,]


#########################
# Merge the data  #######
#########################
#note that merging with controls leads to a smaller sample
full2 <- left_join(les, full, by=c("icpsr", "congress", "party_code"))

#load in a dataset of statenames so that we can ensure they are in the same format across datasets
state_name <- data.frame(state.name)
state_name$state_abbrev <- state.abb
names(state_name)[names(state_name) == "state.name"] <- "STATENAME"

data_shape_full <- inner_join(data_shape_full, state_name)
names(data_shape_full)[names(data_shape_full) == "DISTRICT"] <- "cd"
data_shape_full$cd <- as.numeric(as.character(data_shape_full$cd))
data_shape_full$cd[data_shape_full$cd==0] <- 1

#fix some incorrect districts
full2$cd[full2$congress==97 & full2$icpsr==14879] <- 30
full2$cd[full2$congress==115 & full2$icpsr==20959] <- 22
full2$cd[full2$congress==115 & full2$icpsr==21321] <- 21

full2 <- left_join(full2, data_shape_full,by = c("congress", "cd", "state_abbrev"))

#Adjust for later congresses and the states in those later congresses that redistricted between censuses 
full2$STARTCONG[full2$congress>114] <- 113
full2$STARTCONG[(full2$state_abbrev=="AK" | full2$state_abbrev=="DE" | full2$state_abbrev=="MT" | full2$state_abbrev=="ND" | full2$state_abbrev=="VT" | full2$state_abbrev=="WY") & full2$congress>114] <- 103
full2$STARTCONG[full2$state_abbrev=="SD" & full2$congress>114] <- 108

full2$ENDCONG[full2$ENDCONG==114] <- 116
full2$ENDCONG[full2$congress>114] <- 116

full2$STARTCONG[full2$congress>114 & full2$state_abbrev=="FL"] <- 115
full2$STARTCONG[full2$congress>114 & full2$state_abbrev=="VA"] <- 115
full2$STARTCONG[full2$congress>114 & full2$state_abbrev=="NC"] <- 115
full2$STARTCONG[full2$congress>115 & full2$state_abbrev=="PA"] <- 116

#create the district indicator
full2$districtID <- paste(full2$state_abbrev, full2$cd, full2$STARTCONG, full2$ENDCONG, sep="_")
full2$distidcheck <- grepl("NA_", full2$districtID)
full2$districtID[full2$distidcheck==TRUE] <- NA

#remove some duplicate variables
full2 <- subset(full2, select=-c(bioname.y, bioname.x, bioguide_id.y,bioguide_id.x, born.y, born.x, died.y, died.x, year))


#########################################
# Load in Presidential Vote Share ####
#########################################

pres_vote <- read_dta("presidential_voteshare.dta")
pres_vote$year <- pres_vote$year + 1

full2$district <- paste(full2$st_name, full2$cd, sep="")
full2$year <- 2*full2$congress + 1787

full2 <- left_join(full2, pres_vote, by=c("year", "district"))
full2$dpres <- full2$dpres/100

### Make "dpres" directional toward legislator's party

full2$inpres <- ifelse(full2$party_code == 200, 1-full2$dpres, full2$dpres)

#####################################################
# Create and Load in New Bipartisanship Measure ####
#####################################################

## Convert ICPSR to Bioguide

cw <- read.csv("HSall_members.csv")
cw <- cw %>% dplyr::select(congress, icpsr, bioguide_id) %>% distinct()

full2 <- left_join(full2, cw)

## Merge with cospon data

cospon <- read.csv("cospon_totals_93-117.csv")
full2 <- left_join(full2, cospon)

## Create in-/outparty cospon variable

full2$inpart_cospon <- ifelse(full2$party_code == 100, full2$d_cospon / full2$total_cospon,
                              ifelse(full2$party_code == 200,  full2$r_cospon / full2$total_cospon,
                                     NA))

######################################################
# Create a Distance from the Party Median Measure ####
######################################################

full2<- full2 %>%
  group_by(congress,party_code) %>%
  dplyr::mutate(median_nokken       = median(nokken_poole_dim1, na.rm=TRUE), 
                meddist2            = abs(nokken_poole_dim1 - median_nokken))


#########################
# Save the Files ########
#########################


save(full2, file = "TotalData2.RData")
save(full, file = "AllCongresses.RData")


#####################################################
# Add in Staff Data and Percent Foreign Born ########
#####################################################
library(stringr)

#limit to legislators with a lat long for birth place 
full2 <- full2[!is.na(full2$min.dist.),]

full2$dist <- log(full2$min.dist. + 1)
full2$binary <- 0
full2$binary[full2$min.dist.==0] <- 1

#Load in the staff data
staff <- read_csv("Member_Staff_Data_hill_1212020.csv")
total <- left_join(full2, staff,by=c("thomas_name", "congress", "st_name", "cd", "dem","elected", "female","votepct", "deleg_size", "speaker", "chair", "subchr", "seniority", "power", "majority"))

total$pct_constituencystaff <- (total$est_constit_staff_size/total$est_avg_office_size)*100

summary(total$pct_constituencystaff)

#Load in the bernhard and sulkin data - going to remove the variable from the total dataset so that we also have it for the 109th congress
leg_style <- read_csv("congicpsrstyle.csv")%>%
  rename(congress=cong)

total <- total %>%
  select(-style)

total <- left_join(total, leg_style, by=c("icpsr", "congress"))


total$style2 <- ifelse(total$style == "District Advocate", "District Focused", 
                       ifelse(total$style %in% c("Party Soldier", "Party Builder"), "Party Focused",
                              "Policy Focused"))


total$style3 <- factor(total$style2, levels = c("Party Focused", "District Focused", "Policy Focused"))


#prep the congressional district demographics data set downloaded from here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CI2EPI
cd_data <- read.csv("allCongressDataPublishV2.csv")

cd_data <- cd_data %>%
  rename(congress = congNum)%>%
  select(icpsr, congress, prcntForeignBorn,medianIncome,prcntBA,
         prcntUnemp, prcntWhiteAll)

#remove duplicates- its OK to randomly keep one because only keeping the demographic variables
cd_data <- cd_data%>%
  group_by(congress, icpsr) %>%
  distinct() 

total <- left_join(total, cd_data, by = c("icpsr", "congress")) 


total$inpart_cospon2 <- ifelse(total$party_code == 100, total$d_cospon,
                               ifelse(total$party_code == 200,  total$r_cospon,
                                      NA))

total <- total %>% dplyr::select(congress, year, thomas_num, thomas_name, icpsr,bioguide_id, st_name, cd, districtID,
                                 inpart_cospon, inpart_cospon2, total_cospon, party_unity, dist, dem, seniority, majority, votepct, power, chair, female, inpres,
                                 hometown_final, state_born, district_born, state_represent, min.dist., 
                                 binary, pct_constituencystaff, pct_MRA_constit_spending, style2, style3, prcntForeignBorn, medianIncome, prcntBA,
                                 prcntUnemp, prcntWhiteAll, meddist2)

#load in more recent census data I downloaded from the ACS
cd_data2 <- read_dta("census_variables_complete.dta")
data("fips_codes")
cd_data2 <- cd_data2  %>% rename(state_code = state_fips)
fips_codes$state_code <- as.numeric(as.character(fips_codes$state_code))
fips_codes <- fips_codes%>%
  distinct(state, state_code) %>%
  rename(st_name = state)
cd_data2 <- left_join(cd_data2, fips_codes)

total <- left_join(total, cd_data2, by = c("congress", "st_name", "cd")) 

require(httr)
require(data.table)
require(magrittr)


inflation_adjust2 <- function(base_year=NA){  
  require(dplyr)
  require(xts)
  if (nchar(base_year) == 4){
    #Load file from BLS servers
    cu_main <-  fread('cu.data.1.AllItems.txt')
    #Get rid of annual time periods and add real dates.
    cu_main <- subset(cu_main,series_id=="CUSR0000SA0")
    cu_main <- subset (cu_main,period!="M13")
    cu_main <- subset (cu_main,period!="S01")
    cu_main <- subset (cu_main,period!="S02")
    cu_main <- subset (cu_main,period!="S03")
    cu_main$date <-as.Date(paste(cu_main$year, cu_main$period,"01",sep="-"),"%Y-M%m-%d")
    cu_main <- cu_main[,c('date','value')]
    cu_main <- xts(cu_main$value, order.by=cu_main$date)
    cu_main <- round(cu_main, 1)
    # Get average yearly CPI.
    avg.cpi <- apply.yearly(cu_main, mean)
    avg.cpi <- round(avg.cpi, 1)
    # Formula for calculating inflation example: $1.00 * (1980 CPI/ 2014 CPI) = 1980 price
    cf <- avg.cpi/as.numeric(avg.cpi[as.character(base_year)])
    colnames(cf) <- "adj_value"
    #cf <- round(cf, 2)
    dat <- merge(avg.cpi, cf)
    # Xts object to data frame
    dat <- data.frame(date=index(dat), coredata(dat))
    dat$base.year <- as.character(base_year)
    dat$pct_increase <- (1-dat$adj_value) * -100
    # Round dollar amounts
    dat$adj_value <- round(dat$adj_value, 2)
    # Reorder cols in a more usable fassion
    dat <- dat[c('date','base.year', 'adj_value', 'pct_increase')]
    return(dat)
  }
  else {(message(
    "***************************************************************************************
        Please input a valid four digit year without quotes. For example: 2015.
    ***************************************************************************************", appendLF = TRUE))
  }
}

infl <- inflation_adjust2(2020)
infl$year <- year(infl$date)
total <- left_join(total, infl, by = "year")
total$medianIncome <- total$medianIncome / total$adj_value

#replace the census variables for the newer congresses 
total$medianIncome[total$congress>113] <- total$medianincome[total$congress>113]
total$prcntBA[total$congress>113] <- total$per_bachelors[total$congress>113]
total$prcntWhiteAll[total$congress>113] <- total$per_whitealone[total$congress>113]
total$prcntForeignBorn[total$congress>113] <- total$per_foreign[total$congress>113]


total <- total %>% dplyr::select(congress, year, thomas_num, thomas_name, icpsr, bioguide_id, st_name, cd, districtID,
                                 inpart_cospon, inpart_cospon2, total_cospon, party_unity, dist, dem, seniority, majority, votepct, power, chair, female, inpres,
                                 hometown_final, state_born, district_born, state_represent, min.dist., 
                                 binary, pct_constituencystaff, pct_MRA_constit_spending, style2, style3, prcntForeignBorn, medianIncome, prcntBA,
                                 prcntWhiteAll, meddist2)
View(full2)


##########################################################
# CREATING EXPECTED CF SCORES FOR 'MISMATCH' ANALYSIS ####
##########################################################

total$ICPSR <- total$icpsr

## DIME Data (CFscores through 2018)
cf <- read.csv("dime_recipients_1979_2018_winners.csv")
names(cf)[names(cf) == "cycle"] <- "year"
cf$year <- cf$year + 1
cf <- cf %>% select(-state, -ICPSR)
cf$ICPSR <- as.numeric(as.character(cf$ICPSR2))

cf <- cf %>% group_by(year, ICPSR)  %>%  filter(n() == 1)

total <- left_join(total, cf)
summary(total$recipient.cfscore)

total <- total %>% dplyr::select(!c(ICPSR, ICPSR2,name,lname,ffname,
                                    fname,mname,seat,chamber,district,
                                    Incum.Chall, recipient.cfscore.dyn,irt.cfscore,
                                    total.disbursements,total.receipts,total.indiv.contrib,
                                    total.pac.contribs,gen.elec.stat,Cand.ID,FEC.ID,
                                    NID))


### Create New Variables ###

## "Predicted" CFscore by District Partisanship

# Regress CFscore on District Partisanship
cfmod <- lm(recipient.cfscore ~ district.partisanship + factor(congress), data = total)
summary(cfmod)

cfmod2 <- lm(recipient.cfscore ~ district.pres.vs + factor(congress), data = total)
summary(cfmod2)

cfmod3 <- lm(recipient.cfscore ~ district.pres.vs + district.partisanship + factor(congress), data = total)
summary(cfmod3)

# Generate Predictions
cfmod3_pred <- data.frame(predict(cfmod3))
names(cfmod3_pred) <- "predicted_CF_Score"
cfmod3_pred$rowid <- row.names(cfmod3_pred)

# Merge back to original dataframe
total$rowid <- row.names(total)
total <- left_join(total, cfmod3_pred)

plot(total$recipient.cfscore, total$predicted_CF_Score)

## Party-Ideology "Mismatch"

# Ideo/Party mismatch more moderate --> legislator is more MODERATE than their district partisanship would predict
total$mismatch_moderate <- ifelse(total$party == 100, 
                                  total$recipient.cfscore - total$predicted_CF_Score,
                                  total$predicted_CF_Score - total$recipient.cfscore)

# Ideo/Party mismatch more extreme --> legislator is more EXTREME than their district partisanship would predict

total$mismatch_extreme <- ifelse(total$party == 100, 
                                 total$predicted_CF_Score - total$recipient.cfscore,
                                 total$recipient.cfscore - total$predicted_CF_Score)

# Absolute Ideo/Party 

total$mismatch_absolute <- abs(total$recipient.cfscore - total$predicted_CF_Score)

# Absolute district partisanship

total$abs_district.partisanship <- abs(total$district.partisanship)

# Load in and merge data on percent moved to a different county
load("Census_MobilityData.RData")

#make a census variable in our main dataset to match
total$census[total$year>=1983 & total$year<1993] <-"census 1980"
total$census[total$year>=1993 & total$year<2003] <-"census 1990"
total$census[total$year>=2003 & total$year<=2006] <-"census 2000"
total$census[total$year==2007] <-"acs 2007 1 year"
total$census[total$year==2009] <-"acs 2009 1 year"
total$census[total$year==2011] <-"acs 2011 1 year"
total$census[total$year==2013] <-"acs 2013 1 year"
total$census[total$year==2015] <-"acs 2015 1 year"
total$census[total$year==2017] <-"acs 2017 1 year"
total$census[total$year==2019] <-"acs 2019 1 year"

total <- left_join(total,total_mobility, by = c("census", "st_name", "cd"))

total$districtID[total$st_name=="AK"] <- "AK_1_93_116"
total$districtID[total$st_name=="DE"] <- "DE_1_93_116"
total$districtID[total$st_name=="ND"] <- "ND_1_93_116"
total$districtID[total$st_name=="VT"] <- "VT_1_93_116"
total$districtID[total$st_name=="WY"] <- "WY_1_93_116"
total$districtID[total$st_name=="MT" & total$congress>=103] <- "MT_1_103_116"
total$districtID[total$st_name=="SD" & total$congress>=98] <- "SD_1_98_116"
total$districtID[total$st_name=="NV" & total$congress<98] <- "NV_1_93_97"


# Add in Warshaw and Coughey estimates of ideology
load("aip_cd_ideology_v2022a.RData")
table$cd <- str_sub(table$cd_fips, start= -2)
table$cd <- as.numeric(table$cd)
table$cd[table$cd==0] <- 1
table$st_name <- state.abb[match(table$state,state.name)]
table <- table %>% dplyr::select(congress, st_name, cd, mrp_ideology, mrp_ideology_se) %>% rename(congress_ideo = congress)

# before merging with ideo scores create a merging variable to account for correct congresses
total$congress_ideo[total$congress==111] <- 111
total$congress_ideo[total$congress==112] <- 111
total$congress_ideo[total$congress==113] <- 114
total$congress_ideo[total$congress==114] <- 114
total$congress_ideo[total$congress==115] <- 116
total$congress_ideo[total$congress==116] <- 116
total$congress_ideo[total$congress==117] <- 117
total$congress_ideo[total$congress==118] <- 118
#drop pennsylvania for 115 to 116 so that we dont average over states where districts changed from one session to another
total$congress_ideo[total$congress==115 & total$st_name=="PA"] <- NA


total <- left_join(total, table, by=c("congress_ideo", "st_name","cd"))

#### WRITE OUT UPDATED DATA ####
save(total, file="Dataset_ForAnalysis.RData")

##################################
# Cleaning files for final dataset
##################################

load("Dataset_ForAnalysis.RData")


total$state_match <- 0 
total$state_match[total$state_born==total$state_represent] <- 1

df <- total %>% ungroup() %>% select(-c(party_code, district_born, style2, fecyear,election,party,dwdime,rowid,
                                        census, congress_ideo, mismatch_absolute,abs_district.partisanship))%>%
  rename(logged_distance = dist,
         distance_miles=min.dist.,
         meddist=meddist2)


hunt <- read_dta("hfa_replication.dta")
hunt <- hunt  %>%
  rename(bioguide_id= inc_bioguide_id,
         st_name=state) %>%
  mutate(congress = ((elect_yr + 1) - 1787)/2)%>%
  select(congress, bioguide_id, st_name, cd, inc_hs_indist,inc_born_indist, inc_roots_0220)

# Limit to our years of analysis
hunt <- hunt[hunt$congress>92 & hunt$congress<117,]

# Make sure at large districts are marked the same way
hunt$cd[hunt$cd==0] <- 1

# Merge the data
df <- left_join(df, hunt, by=c("bioguide_id", "congress","st_name", "cd"))

df$newbinary <- df$binary
df$newbinary[df$inc_hs_indist==1] <- 1
df$newbinary[is.na(df$inc_hs_indist)] <-NA

df$newind <- df$binary + df$inc_hs_indist
df$newind[is.na(df$inc_hs_indist)] <-NA

df <- df %>% select(-c(inc_hs_indist))%>%
  rename(alt_binary= newbinary,
         alt_index=newind)

write.dta(df, "Dataset_ForMainAnalysis.dta")

load("AllCongresses.RData")
full <- full[!is.na(full$lat) & !is.na(full$lon),]
full$dist <- log(full$min.dist. + 1)
full$binary <- 0
full$binary[full$min.dist.==0] <- 1
df_overtime <- full %>% ungroup() %>% select(-c(party_code, chamber,state_icpsr, district_born, hometown_1, hometown_2,hometown_3,
                                                hometown_4,state,lon,lat,state.name,state_code,newid,party_unity,absextreme, bioguide_id,
                                                occupancy, last_means,born,died,nominate_dim1,nominate_dim2,nominate_log_likelihood,
                                                nominate_geo_mean_probability, nominate_number_of_votes,nominate_number_of_errors,
                                                conditional, nokken_poole_dim1,nokken_poole_dim2,county,ID,min.dist1, bioname))%>%
  rename(st_name=state_abbrev,
         cd=district_code,
         logged_distance = dist,
         distance_miles=min.dist.)


write.dta(df_overtime,file="Dataset_OvertimeComparisons.dta")
