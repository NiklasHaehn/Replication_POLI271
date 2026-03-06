###################################################################################################
# Far From Home                      ###############################################
# Jesse Crosson and Jaclyn Kaslovsky ###############################################
###################################################################################################

##### SCRAPING COSPONSORSHIP DATA AND CREATING BIPARTISANSHIP MEASURES #####

setwd("~/Dropbox/Local Roots/Replication Package New/Intermediate Data/")

require("rjson")
require("downloader")
require("tidyr")

#### Unzip, and Extract JSON from Downloaded Propublica Files####

for(i in 112:117){
  unzip(paste(i, ".zip", sep = ""))
}

## Create data files and bind together

# HR

dataset <- NA
for(i in 112:117){
  temp_wd <- paste(getwd(), "/congress/data/", i, "/bills/hr", sep = "")
  loop_folders <- list.files(temp_wd)
  for(j in loop_folders){
    skip_to_next <- FALSE
    tryCatch(obj <- fromJSON(file = paste(temp_wd, "/", j, "/data.json", sep = "")),
                      error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }
    cospon <- obj$cosponsors
    obj$cosponsors
    if(length(cospon)>0){
      cospon_out <- matrix(NA, ncol = 6, nrow = length(cospon))
      for(k in 1:length(cospon)){
       cospon_out[k,1] <- ifelse(length(cospon[[k]]$thomas_id)>0,cospon[[k]]$thomas_id, NA)
       cospon_out[k,2] <- ifelse(length(cospon[[k]]$bioguide_id)>0,cospon[[k]]$bioguide_id, NA)
       cospon_out[k,3] <- cospon[[k]]$sponsored_at
       cospon_out[k,4] <- ifelse(length(obj$sponsor$thomas_id)>0,obj$sponsor$thomas_id, NA)
       cospon_out[k,5] <- ifelse(length(obj$sponsor$bioguide_id)>0,obj$sponsor$bioguide_id, NA)
       cospon_out[k,6] <- obj$bill_id
      }
      dataset <- rbind(dataset, cospon_out)}else{
      NA
      }
    }
  }

# HRES

for(i in 112:117){
  temp_wd <- paste(getwd(), "/congress/data/", i, "/bills/hres", sep = "")
  loop_folders <- list.files(temp_wd)
  for(j in loop_folders){
    skip_to_next <- FALSE
    tryCatch(obj <- fromJSON(file = paste(temp_wd, "/", j, "/data.json", sep = "")),
             error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }
    cospon <- obj$cosponsors
    obj$cosponsors
    if(length(cospon)>0){
      cospon_out <- matrix(NA, ncol = 6, nrow = length(cospon))
      for(k in 1:length(cospon)){
        cospon_out[k,1] <- ifelse(length(cospon[[k]]$thomas_id)>0,cospon[[k]]$thomas_id, NA)
        cospon_out[k,2] <- ifelse(length(cospon[[k]]$bioguide_id)>0,cospon[[k]]$bioguide_id, NA)
        cospon_out[k,3] <- cospon[[k]]$sponsored_at
        cospon_out[k,4] <- ifelse(length(obj$sponsor$thomas_id)>0,obj$sponsor$thomas_id, NA)
        cospon_out[k,5] <- ifelse(length(obj$sponsor$bioguide_id)>0,obj$sponsor$bioguide_id, NA)
        cospon_out[k,6] <- obj$bill_id
      }
      dataset <- rbind(dataset, cospon_out)}else{
        NA
      }
  }
}

# HJRES

for(i in 112:117){
  temp_wd <- paste(getwd(), "/congress/data/", i, "/bills/hjres", sep = "")
  loop_folders <- list.files(temp_wd)
  for(j in loop_folders){
    skip_to_next <- FALSE
    tryCatch(obj <- fromJSON(file = paste(temp_wd, "/", j, "/data.json", sep = "")),
             error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }
    cospon <- obj$cosponsors
    obj$cosponsors
    if(length(cospon)>0){
      cospon_out <- matrix(NA, ncol = 6, nrow = length(cospon))
      for(k in 1:length(cospon)){
        cospon_out[k,1] <- ifelse(length(cospon[[k]]$thomas_id)>0,cospon[[k]]$thomas_id, NA)
        cospon_out[k,2] <- ifelse(length(cospon[[k]]$bioguide_id)>0,cospon[[k]]$bioguide_id, NA)
        cospon_out[k,3] <- cospon[[k]]$sponsored_at
        cospon_out[k,4] <- ifelse(length(obj$sponsor$thomas_id)>0,obj$sponsor$thomas_id, NA)
        cospon_out[k,5] <- ifelse(length(obj$sponsor$bioguide_id)>0,obj$sponsor$bioguide_id, NA)
        cospon_out[k,6] <- obj$bill_id
      }
      dataset <- rbind(dataset, cospon_out)}else{
        NA
      }
  }
}

# HCONRES

for(i in 112:117){
  temp_wd <- paste(getwd(), "/congress/data/", i, "/bills/hconres", sep = "")
  loop_folders <- list.files(temp_wd)
  for(j in loop_folders){
    skip_to_next <- FALSE
    tryCatch(obj <- fromJSON(file = paste(temp_wd, "/", j, "/data.json", sep = "")),
             error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }
    cospon <- obj$cosponsors
    obj$cosponsors
    if(length(cospon)>0){
      cospon_out <- matrix(NA, ncol = 6, nrow = length(cospon))
      for(k in 1:length(cospon)){
        cospon_out[k,1] <- ifelse(length(cospon[[k]]$thomas_id)>0,cospon[[k]]$thomas_id, NA)
        cospon_out[k,2] <- ifelse(length(cospon[[k]]$bioguide_id)>0,cospon[[k]]$bioguide_id, NA)
        cospon_out[k,3] <- cospon[[k]]$sponsored_at
        cospon_out[k,4] <- ifelse(length(obj$sponsor$thomas_id)>0,obj$sponsor$thomas_id, NA)
        cospon_out[k,5] <- ifelse(length(obj$sponsor$bioguide_id)>0,obj$sponsor$bioguide_id, NA)
        cospon_out[k,6] <- obj$bill_id
      }
      dataset <- rbind(dataset, cospon_out)}else{
        NA
      }
  }
}

dataset <- data.frame(dataset)
names(dataset) <- c("thomas_id_cospon", "bioguide_id_cospon", "cospon_date", "thomas_id_spon", "bioguide_id_spon", "bill_id")
dataset <- unique(dataset)
dataset <- dataset[2:nrow(dataset),]
dataset <- subset(dataset, as.Date(cospon_date) > "2011-01-01")

# Fix variable types

dat_clean <- dataset
dat_clean$thomas_id_cospon <- as.numeric(dat_clean$thomas_id_cospon)
dat_clean$thomas_id_spon <- as.numeric(dat_clean$thomas_id_spon)
dat_clean$bioguide_id_cospon <- as.character(dat_clean$bioguide_id_cospon)
dat_clean$bioguide_id_spon <- as.character(dat_clean$bioguide_id_spon)

## Merge in Partisanship Information

# Read in legislator data

legislators <- read.csv("legislators-historical.csv")
legislators2 <- read.csv("legislators-current.csv")
legislators <- dplyr::bind_rows(legislators, legislators2)

# Split by cospon identifier; merge

leg_partisanship <- legislators %>% select(thomas_id, bioguide_id, party)
leg_partisanship <- unique(leg_partisanship)


dataset_thomas <- subset(dat_clean, is.na(thomas_id_cospon) == F)
dataset_thomas <- merge(dataset_thomas, leg_partisanship, by.x = "thomas_id_cospon", by.y = "thomas_id", all.x = T)
dataset_thomas$bioguide_id_cospon <- dataset_thomas$bioguide_id

dataset_bio <- subset(dat_clean, is.na(bioguide_id_cospon) == F)
dataset_bio <- merge(dataset_bio, leg_partisanship, by.x = "bioguide_id_cospon", by.y = "bioguide_id", all.x = T)

dat_clean <- bind_rows(dataset_thomas, dataset_bio)
dat_clean <- dat_clean %>% select(-thomas_id, -bioguide_id)
names(dat_clean)[names(dat_clean) == "party"] <- "party_cospon"


# Now with the sponsors

dataset_thomas <- subset(dat_clean, is.na(thomas_id_spon) == F)
dataset_thomas <- merge(dataset_thomas, leg_partisanship, by.x = "thomas_id_spon", by.y = "thomas_id", all.x = T)
dataset_thomas$bioguide_id_spon <- dataset_thomas$bioguide_id

dataset_bio <- subset(dat_clean, is.na(bioguide_id_spon) == F)
dataset_bio <- merge(dataset_bio, leg_partisanship, by.x = "bioguide_id_spon", by.y = "bioguide_id", all.x = T)

dat_clean2 <- bind_rows(dataset_thomas, dataset_bio)
dat_clean2 <- dat_clean2 %>% select(-thomas_id, -bioguide_id)
names(dat_clean2)[names(dat_clean2) == "party"] <- "party_spon"

dat_clean2$party_cospon[dat_clean2$thomas_id_cospon == 1793] <- "Republican"
dat_clean2$bioguide_id_cospon[dat_clean2$thomas_id_cospon == 1793] <- "F000449"
dat_clean2$party_spon[dat_clean2$thomas_id_spon == 1793] <- "Republican"
dat_clean2$bioguide_id_spon[dat_clean2$thomas_id_spon == 1793] <- "F000449"

alldat <- dat_clean2 %>% select(-thomas_id_spon, -thomas_id_cospon)


## Generate Congress variable

require(stringi)
alldat$congress <- as.numeric(stri_sub(alldat$bill_id,-3))

# Investigate missingness; fix

alldat$party_cospon[alldat$bioguide_id_cospon %in% c("F000449")] <- "Republican"
alldat$party_cospon[alldat$bioguide_id_cospon %in% c("V000132")] <- "Democrat"

alldat$party_spon[alldat$bioguide_id_spon %in% c("F000449")] <- "Republican"
alldat$party_spon[alldat$bioguide_id_spon %in% c("V000132")] <- "Democrat"

alldat$party_spon[alldat$bioguide_id_spon %in% c("A000367") & alldat$congress < 116] <- "Republican"
alldat$party_cospon[alldat$bioguide_id_cospon %in% c("A000367") & alldat$congress < 116] <- "Republican"

alldat$party_spon[alldat$bioguide_id_spon %in% c("V000133") & alldat$congress < 117] <- "Democrat"
alldat$party_cospon[alldat$bioguide_id_cospon %in% c("V000133") & alldat$congress < 117] <- "Democrat"

alldat$party_spon[alldat$bioguide_id_spon %in% c("M001201") & alldat$congress < 117] <- "Republican"
alldat$party_cospon[alldat$bioguide_id_cospon %in% c("M001201") & alldat$congress < 117] <- "Republican"

#### GENERATE MEMBER-LEVEL PARTISANSHIP MEASURES ####

## Loop through sponsors; table by party of attracted cosponsors

final <- NA
for(i in 112:117){
  df <- subset(alldat, congress == i)
  outcon <- NA
  for(j in na.omit(unique(df$bioguide_id_spon))){
    df2 <- subset(df, bioguide_id_spon == j)
    spon_part <- data.frame(table(df2$party_cospon))
    spon_part <- subset(spon_part, Var1 %in% c("Republican", "Democrat"))
    total_cospon <- sum(spon_part$Freq)
    r_cospon <- ifelse(length(subset(spon_part, Var1 == "Republican")$Freq) > 0,
                       subset(spon_part, Var1 == "Republican")$Freq,
                       0)
    d_cospon <- ifelse(length(subset(spon_part, Var1 == "Democrat")$Freq) > 0,
                       subset(spon_part, Var1 == "Democrat")$Freq,
                       0)
    outcon <- rbind(outcon, c(j, total_cospon, r_cospon, d_cospon, i))
  }
  outcon <- outcon[2:nrow(outcon),]
  final <- rbind(final, outcon)
}
final <- data.frame(final[2:nrow(final),])
names(final) <- c("bioguide_id", "total_cospon", "r_cospon", "d_cospon", "congress")

# write.csv(final, "cospon_112-117.csv", row.names = F)


#### READ IN FOWLER DATA AND PROCESS ####

## Data in separate docs

require(splitstackshape)

bill_id <- read.delim("bills.txt", header = F)
names(bill_id) <- "bill_id"
bill_id <- as.character(bill_id$bill_id)

spon <- read.delim("sponsors.txt", header = F)
names(spon) <- "spon"

cospons <- data.frame(read.delim("Cosponsors.txt", header = F))
cospons <- cSplit(cospons, "V1", " ")

# Join

data <- cbind(bill_id, cospons, spon)
data <- data.frame(data)

rm(cospons)
rm(bill_id)

# Make into long data

require(reshape2)

data_long <- melt(data, id.vars = c("bill_id", "spon"))
data_long <- data_long %>% dplyr::select(-variable)

rm(data)

data_long <- subset(data_long, is.na(value) == F)

## Join with legislator data for cosponsor party

leg_partisanship <- read.csv("HSall_members.csv")
names(leg_partisanship)[names(leg_partisanship) == "icpsr"] <- "icpsr_id"
leg_partisanship <- subset(leg_partisanship, chamber != "President" & congress %in% 93:108)
leg_partisanship <- leg_partisanship %>% dplyr::select(icpsr_id, bioguide_id, party_code, congress)
leg_partisanship <- unique(leg_partisanship)

# Check for dupes; fix party-switchers

dupes <- data.frame(table(leg_partisanship$bioguide_id, leg_partisanship$congress))
dupes <- subset(dupes, Freq > 1 & Var2 %in% c(93:108))
names(dupes) <- c("bioguide_id", "congress", "Freq")

leg_partisanship[leg_partisanship$congress == 104 & leg_partisanship$bioguide_id == "C000077" & leg_partisanship$party_code == 100, "drop"] <- "drop" 
leg_partisanship[leg_partisanship$congress == 104 & leg_partisanship$bioguide_id == "D000168" & leg_partisanship$party_code == 100, "drop"] <- "drop" 
leg_partisanship[leg_partisanship$congress == 104 & leg_partisanship$bioguide_id == "H000390" & leg_partisanship$party_code == 100, "drop"] <- "drop"
leg_partisanship[leg_partisanship$congress == 104 & leg_partisanship$bioguide_id == "L000119" & leg_partisanship$party_code == 100, "drop"] <- "drop"
leg_partisanship[leg_partisanship$congress == 104 & leg_partisanship$bioguide_id == "P000066" & leg_partisanship$party_code == 100, "drop"] <- "drop"
leg_partisanship[leg_partisanship$congress == 104 & leg_partisanship$bioguide_id == "T000058" & leg_partisanship$party_code == 100, "drop"] <- "drop"
leg_partisanship[leg_partisanship$congress == 106 & leg_partisanship$bioguide_id == "F000257" & leg_partisanship$party_code == 200, "drop"] <- "drop"
leg_partisanship[leg_partisanship$congress == 106 & leg_partisanship$bioguide_id == "G000280", "drop"] <- "drop"
leg_partisanship[leg_partisanship$congress == 106 & leg_partisanship$bioguide_id == "M000206" & leg_partisanship$party_code == 100, "drop"] <- "drop"
leg_partisanship[leg_partisanship$congress == 107 & leg_partisanship$bioguide_id == "G000280", "drop"] <- "drop"
leg_partisanship[leg_partisanship$congress == 107 & leg_partisanship$bioguide_id == "J000072", "drop"] <- "drop"
leg_partisanship[leg_partisanship$congress == 108 & leg_partisanship$bioguide_id == "H000067" & leg_partisanship$party_code == 100, "drop"] <- "drop"

leg_partisanship <- subset(leg_partisanship, is.na(drop)) %>% dplyr::select(-drop)

# Create "congress" variable for merging

require(stringr)

data_long$congress <- str_extract(data_long$bill_id, "\\_([^_]+)\\_")
data_long$congress <- as.numeric(str_remove_all(data_long$congress,"\\_"))

names(data_long)[3] <- "icpsr_id"
head(data_long)
data_long <- dplyr::left_join(data_long, leg_partisanship)

names(data_long)[2:6] <- c("icpsr_id_spon", "icpsr_id_cospon", "congress", 
                          "bioguide_id_cospon", "party_cospon")

# Fix missing ICPSRs

party <- read.delim("party.txt", header = F)
names(party) <- "party"

spon_part <- cbind(spon, party)
spon_part <- unique(spon_part) # CW to parties
names(spon_part) <- c("icpsr_id_cospon", "party") # not actually the cosponsnor; using this to grab the missing partisanship


missing <- subset(data_long, is.na(party_cospon))
missing <- dplyr::left_join(missing, spon_part)
missing$party_cospon <- missing$party

# De-dupe the missing cases (from party-switching)

dupes <- data.frame(table(missing$icpsr_id_cospon, missing$congress, missing$bill_id))
dupes <- subset(dupes, Freq > 1)
dupes <- dupes %>% dplyr::select(-Var3)
dupes <- unique(dupes)

missing[missing$congress == 104 & missing$icpsr_id_cospon == 14679 & missing$party == 100, "drop"] <- "drop" 
missing[missing$congress == 104 & missing$icpsr_id_cospon == 15611 & missing$party == 100, "drop"] <- "drop" 
missing[missing$congress == 104 & missing$icpsr_id_cospon == 15617 & missing$party == 100, "drop"] <- "drop" 
missing[missing$congress == 104 & missing$icpsr_id_cospon == 29342 & missing$party == 100, "drop"] <- "drop" 
missing[missing$congress == 104 & missing$icpsr_id_cospon == 15418 & missing$party == 100, "drop"] <- "drop" 
missing[missing$congress == 106 & missing$icpsr_id_cospon == 14879 & missing$party == 100, "drop"] <- "drop" 
missing[missing$congress == 106 & missing$icpsr_id_cospon == 29767 & missing$party == 100, "drop"] <- "drop" 

missing <- subset(missing, is.na(drop))
missing <- missing %>% dplyr::select(-party, -drop)

data_long <- subset(data_long, is.na(party_cospon) == F)
data_long <- dplyr::bind_rows(data_long, missing)
data_long <- unique(data_long)

data_long$party_cospon <- ifelse(data_long$party_cospon == 100, "Democrat",
                               ifelse(data_long$party_cospon == 200, "Republican",
                                      "Independent"))

## Add in missing Bioguides (will help to match to final data)

missing_bio <- subset(data_long, is.na(bioguide_id_cospon))
cw <- read.csv("missing_cw.csv")

missing_bio <- dplyr::left_join(missing_bio, cw)
missing_bio$bioguide_id_cospon <- missing_bio$bioguide_id
missing_bio <- missing_bio %>% dplyr::select(-bioguide_id)

data_long <- subset(data_long, is.na(bioguide_id_cospon) == F)
data_long <- dplyr::bind_rows(data_long, missing_bio)

## Add in sponsor bioguide

bios <- leg_partisanship %>% dplyr::select(icpsr_id, bioguide_id)
bios <- unique(bios)
data_long <- subset(data_long, is.na(spon) == F)

data_long <- merge(data_long, bios, by.x = "icpsr_id_spon", by.y = "icpsr_id", all.x = T)
data_long$bioguide_id_spon <- data_long$bioguide_id
data_long <- data_long %>% dplyr::select(-bioguide_id)
View(data_long)

missing_bio <- subset(data_long, is.na(bioguide_id_spon))
cw <- read.csv("missing_cw.csv")
names(cw)[1] <- "spon"

missing_bio <- merge(missing_bio, cw, by.x = "icpsr_id_spon", by.y = "spon")
missing_bio$bioguide_id_spon <- missing_bio$bioguide_id
missing_bio <- missing_bio %>% dplyr::select(-bioguide_id)

data_long <- subset(data_long, is.na(bioguide_id_spon) == F)
data_long <- dplyr::bind_rows(data_long, missing_bio)

## FINALLY, run the loop to create the output file for 93-forward

final2 <- NA
for(i in 93:108){
  df <- subset(data_long, congress == i)
  outcon <- NA
  for(j in na.omit(unique(df$bioguide_id_spon))){
    df2 <- subset(df, bioguide_id_spon == j)
    spon_part <- data.frame(table(df2$party_cospon))
    spon_part <- subset(spon_part, Var1 %in% c("Republican", "Democrat"))
    total_cospon <- sum(spon_part$Freq)
    r_cospon <- ifelse(length(subset(spon_part, Var1 == "Republican")$Freq) > 0,
                       subset(spon_part, Var1 == "Republican")$Freq,
                       0)
    d_cospon <- ifelse(length(subset(spon_part, Var1 == "Democrat")$Freq) > 0,
                       subset(spon_part, Var1 == "Democrat")$Freq,
                       0)
    outcon <- rbind(outcon, c(j, total_cospon, r_cospon, d_cospon, i))
  }
  outcon <- outcon[2:nrow(outcon),]
  final2 <- rbind(final2, outcon)
}

final2 <- data.frame(final2[2:nrow(final2),])
names(final2) <- c("bioguide_id", "total_cospon", "r_cospon", "d_cospon", "congress")

# write.csv(final2, "cospon_93-108.csv", row.names = F)


#### GOVTRACK COSPON DATA FOR 109-111TH CONGRESSES ####

gov1 <- read.csv("govtrack_cosponsor_data_109_congress.csv")
gov2 <- read.csv("govtrack_cosponsor_data_110_congress.csv")
gov3 <- read.csv("govtrack_cosponsor_data_111_congress.csv")

gov <- dplyr::bind_rows(gov1,gov2,gov3)

## Congress variable

require(stringr)
gov <- gov %>% 
  dplyr::mutate(congress = str_sub(bill_number, -3) %>% as.numeric) 

## ICPSRs

require("humaniformat")
gov <- gov %>% 
  dplyr::mutate(clean_name = format_reverse(name))

gov$district[is.na(gov$district)] <- 1

require("purrr")
name.df <- purrr::map_df(unique(gov$clean_name), ~humaniformat::parse_names(.))
gov <- dplyr::left_join(gov, name.df, by = c("clean_name" = "full_name"))
gov[gov$name == "Carson, Julia", "last_name"] <- "Carson, J." # addressing "double Carson" issue
vw <- read.csv("cel_cw.csv")

merge.df <- vw[,c(3, 4, 6, 7, 2)]
names(merge.df) <- c("icpsr", "congress", "state", "district", "name")

merge.df <- merge.df %>% 
  dplyr::mutate(clean_name = humaniformat::format_reverse(name)) %>% 
  dplyr::left_join(purrr::map_df(unique(.$clean_name), ~humaniformat::parse_names(.)), by = c("clean_name" = "full_name")) %>%
  dplyr::select(icpsr:district, first_name, last_name) # drop unnecessary columns

merge.df <- subset(merge.df, is.na(icpsr) == F) %>% dplyr::select(-first_name)

# Fix the "multiple Carsons" problem
merge.df[merge.df$icpsr == 29720, "last_name"] <- "Carson, J."

check <- dplyr::left_join(gov, merge.df) 
View(check)

## Keep the necessary variables

gt_cleaned <- check %>% dplyr::select(icpsr, bill_number, sponsor, congress)

sponsors <- subset(gt_cleaned, sponsor == T)
cosponsors <- subset(gt_cleaned, sponsor == F)
sponsors <- sponsors %>% dplyr::select(-sponsor)
cosponsors <- cosponsors %>% dplyr::select(-sponsor)

## Add in partisanship

leg_partisanship <- unique(read.csv("party_bios_109-111.csv"))

cosponsors <- dplyr::left_join(cosponsors, leg_partisanship)
sponsors <- dplyr::left_join(sponsors, leg_partisanship)

cosponsors$party_cospon <- ifelse(cosponsors$party_code == 100, "Democrat",
                                  ifelse(cosponsors$party_code == 200, "Republican",
                                         "Independent"))

sponsors$party_spon <- ifelse(sponsors$party_code == 100, "Democrat",
                                  ifelse(sponsors$party_code == 200, "Republican",
                                         "Independent"))

cosponsors <- subset(cosponsors, is.na(icpsr) == F) %>% dplyr::select(bill_number, congress, bioguide_id, party_cospon)
sponsors <- subset(sponsors, is.na(icpsr) == F)%>% dplyr::select(bill_number, congress, bioguide_id, party_spon)

## Join together cosponsors and sponsors

names(cosponsors)[3] <- "bioguide_id_cospon"
names(sponsors)[3] <- "bioguide_id_spon"

gt_cleaned <- dplyr::left_join(cosponsors, sponsors)

## Now tabulate as before

final3 <- NA
for(i in 109:111){
  df <- subset(gt_cleaned, congress == i)
  outcon <- NA
  for(j in na.omit(unique(df$bioguide_id_spon))){
    df2 <- subset(df, bioguide_id_spon == j)
    spon_part <- data.frame(table(df2$party_cospon))
    spon_part <- subset(spon_part, Var1 %in% c("Republican", "Democrat"))
    total_cospon <- sum(spon_part$Freq)
    r_cospon <- ifelse(length(subset(spon_part, Var1 == "Republican")$Freq) > 0,
                       subset(spon_part, Var1 == "Republican")$Freq,
                       0)
    d_cospon <- ifelse(length(subset(spon_part, Var1 == "Democrat")$Freq) > 0,
                       subset(spon_part, Var1 == "Democrat")$Freq,
                       0)
    outcon <- rbind(outcon, c(j, total_cospon, r_cospon, d_cospon, i))
  }
  outcon <- outcon[2:nrow(outcon),]
  final3 <- rbind(final3, outcon)
}

final3 <- data.frame(final3[2:nrow(final3),])
names(final3) <- c("bioguide_id", "total_cospon", "r_cospon", "d_cospon", "congress")

# write.csv(final3, "cospon_109-111 2.csv", row.names = F)

#### NOW JOIN TOGETHER ALL DATA FRAMES ####

final <- read.csv("cospon_112-117.csv")
final2 <- read.csv("cospon_93-108.csv")
final3 <- read.csv("cospon_109-111 2.csv")
final_out <- dplyr::bind_rows(final, final2, final3)

# write.csv(final_out, "cospon_totals_93-117.csv", row.names = F)