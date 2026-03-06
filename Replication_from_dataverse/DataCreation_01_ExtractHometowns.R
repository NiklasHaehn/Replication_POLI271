###################################################################################################
# Far From Home                      ###############################################
# Jesse Crosson and Jaclyn Kaslovsky ###############################################
###################################################################################################

#This file creates a list of legislator home towns 

###################################################################################################
# Working Directory and Packages ##################################################################
###################################################################################################

setwd("~/Dropbox/Local Roots/Replication Package New/Intermediate Data/")

require(jsonlite)
require(dplyr)
require(devtools)

## Read in most recent version of historical legislators from Congress GitHub

members <- read.csv("https://theunitedstates.io/congress-legislators/legislators-historical.csv")
members_c <- read.csv("https://theunitedstates.io/congress-legislators/legislators-current.csv")
members <- bind_rows(members, members_c)

## Cycle through all bios; pull out profile text

out <- NA

for(i in unique(members$bioguide_id)){
  bioguide <- i
  profileText <- as.character(fromJSON(paste("https://bioguide.congress.gov/search/bio/",i,".json", sep = ""))$data$profileText)
  out <- rbind(out, c(bioguide,profileText))
}
out <- data.frame(out[2:nrow(out),])
names(out) <- c("bioguide_id", "profileText")
row.names(out) <- NULL


## Trim profile text to "born in" portion

# Remove stuff before "born in", after died in

out$profileText <- gsub(".*born\\s","",out$profileText)
out$profileText <- gsub("ied\\s*.","",out$profileText)
out$profileText <- gsub("St\\.","Saint",out$profileText)
out$profileText <- gsub("Mt\\.","Mount",out$profileText)


# Split according to semi-colon delimiters, comma'd sections

require("splitstackshape")
out2 <- cSplit(out, "profileText", sep = ";")
out2 <- cSplit(out2, "profileText_01", sep = ", ")

# Process text within each column; pull out town, state.

require("stringr")
out2$location1 <- str_extract(as.character(out2$profileText_01_01), "([A-Z][a-z]+\\s?)+")

states <- read.csv("states.csv")

for(i in 1:nrow(out2)){
  ifelse(grepl("\\.", out2$profileText_01_02[i]) | out2$profileText_01_02[i] %in% states$name, 
    out2$state[i] <- as.character(out2$profileText_01_02[i]), 
      out2$state[i] <-  NA)
}

out2$profileText_01_02 <- ifelse(is.na(out2$state)==F, NA, as.character(out2$profileText_01_02))

for(i in 1:nrow(out2)){
  ifelse(is.na(out2$state[i]) & (grepl("\\.", out2$profileText_01_03[i]) | out2$profileText_01_03[i] %in% states$name), 
         out2$state[i] <- as.character(out2$profileText_01_03[i]), 
         out2$state[i] <-   out2$state[i])
}

out2$profileText_01_03 <- ifelse(is.na(out2$state)==F, NA, as.character(out2$profileText_01_3))

for(i in 1:nrow(out2)){
  ifelse(is.na(out2$state[i]) & (grepl("\\.", out2$profileText_01_04[i]) | out2$profileText_01_04[i] %in% states$name), 
         out2$state[i] <- as.character(out2$profileText_01_04[i]), 
         out2$state[i] <-   out2$state[i])
}

out2$profileText_01_04 <- ifelse(is.na(out2$state)==F, NA, as.character(out2$profileText_01_04))

for(i in 1:nrow(out2)){
  ifelse(is.na(out2$state[i]) & (grepl("\\.", out2$profileText_01_05[i]) | out2$profileText_01_05[i] %in% states$name), 
         out2$state[i] <- as.character(out2$profileText_01_05[i]), 
         out2$state[i] <-   out2$state[i])
}

out2$profileText_01_05 <- ifelse(is.na(out2$state)==F, NA, as.character(out2$profileText_01_05))

for(i in 1:nrow(out2)){
  ifelse(is.na(out2$state[i]) & (grepl("\\.", out2$profileText_01_06[i]) | out2$profileText_01_06[i] %in% states$name), 
         out2$state[i] <- as.character(out2$profileText_01_06[i]), 
         out2$state[i] <-   out2$state[i])
}


# Clean State

#clean state names

out2$state <-gsub(" then a part of", "", out2$state)

out2$state <- ifelse(is.na(out2$state) & grepl("Maine", out2$profileText_01_03), "ME", out2$state)
out2$state <-gsub("\\bAlabama\\b", "AL", out2$state)
out2$state <-gsub("\\bAlaska\\b", "AK", out2$state)
out2$state <-gsub("\\bArizona\\b", "AZ", out2$state)
out2$state <-gsub("\\bArkansas\\b", "AR", out2$state)
out2$state <-gsub("\\bAmerican Samoa\\b", "AS", out2$state)
out2$state <-gsub("\\bCalifornia\\b", "CA", out2$state)
out2$state <-gsub("\\bColorado\\b", "CO", out2$state)
out2$state <-gsub("\\bConnecticut\\b", "CT", out2$state)
out2$state <-gsub("\\bDistrict of Columbia\\b", "DC", out2$state)
out2$state <-gsub("\\bDelaware\\b", "DE", out2$state)
out2$state <-gsub("\\bFlorida\\b", "FL", out2$state)
out2$state <-gsub("\\bGeorgia\\b", "GA", out2$state)
out2$state <-gsub("\\bIowa\\b", "IA", out2$state)
out2$state <-gsub("\\bIllinois\\b", "IL", out2$state)
out2$state <-gsub("\\bIndiana\\b", "IN", out2$state)
out2$state <-gsub("\\bKansas\\b", "KS", out2$state)
out2$state <-gsub("\\bKentucky\\b", "KY", out2$state)
out2$state <-gsub("\\bLouisiana\\b", "LA", out2$state)
out2$state <-gsub("\\bMassachusetts\\b", "MA", out2$state)
out2$state <-gsub("\\bMaryland\\b", "MD", out2$state)
out2$state <-gsub("\\bHawaii\\b", "HI", out2$state)
out2$state <-gsub("\\bMaine\\b", "ME", out2$state)
out2$state <-gsub("\\bMichigan\\b", "MI", out2$state)
out2$state <-gsub("\\bMinnesota\\b", "MN", out2$state)
out2$state <-gsub("\\bMissouri\\b", "MO", out2$state)
out2$state <-gsub("\\bMississippi\\b", "MS", out2$state)
out2$state <-gsub("\\bMontana\\b", "MT", out2$state)
out2$state <-gsub("\\bNorth Carolina\\b", "NC", out2$state)
out2$state <-gsub("\\bNorth Dakota\\b", "ND", out2$state)
out2$state <-gsub("\\bNebraska\\b", "NE", out2$state)
out2$state <-gsub("\\bNew Jersey\\b", "NJ", out2$state)
out2$state <-gsub("\\bNew Mexico\\b", "NM", out2$state)
out2$state <-gsub("\\bNevada\\b", "NV", out2$state)
out2$state <-gsub("\\bNew York\\b", "NY", out2$state)
out2$state <-gsub("\\bOhio\\b", "OH", out2$state)
out2$state <-gsub("\\bPennsylvania\\b", "PA", out2$state)
out2$state <-gsub("\\bPuerto Rico\\b", "PR", out2$state)
out2$state <-gsub("\\bRhode Island\\b", "RI", out2$state)
out2$state <-gsub("\\bSouth Carolina\\b", "SC", out2$state)
out2$state <-gsub("\\bSouth Dakota\\b", "SD", out2$state)
out2$state <-gsub("\\bTexas\\b", "TX", out2$state)
out2$state <-gsub("\\bTennessee\\b", "TN", out2$state)
out2$state <-gsub("Va\\. \\(now West Virginia\\)", "WV", out2$state)
out2$state <-gsub("\\bWest Virginia\\b", "WV", out2$state)
out2$state <-gsub("\\bVirginia\\b", "VA", out2$state)
out2$state <-gsub("\\bVermont\\b", "VT", out2$state)
out2$state <-gsub("\\bWashington\\b", "WA", out2$state)
out2$state <-gsub("\\bWisconsin\\b", "WI", out2$state)
out2$state <-gsub("\\bWyoming\\b", "WY", out2$state)
out2$state <-gsub("\\bNew Hampshire\\b", "NH", out2$state)
out2$state <-gsub("\\bIdaho\\b", "ID", out2$state)
out2$state <-gsub("\\bUtah\\b", "UT", out2$state)
out2$state <-gsub("\\bPrussia\\b", "Germany", out2$state)

out2$state <-gsub(" on January|February|March|April|May|June|July|August|September|October|November|December", "", out2$state)

out2$state <-gsub("Ala\\. \\(then a part of GA\\)", "AL", out2$state)
out2$state <-gsub("Va \\(now West Virginia\\)", "WV", out2$state)
out2$state <-gsub("Va\\. \\(now West Virginia\\)", "WV", out2$state)
out2$state <-gsub("Va\\. \\(now W\\.Va\\.\\)", "WV", out2$state)
out2$state <-gsub("Va\\. \\(now West Virginia in 1777\\)", "WV", out2$state)
out2$state <-gsub("Va\\. \\(later West Virginia\\)", "WV", out2$state)
out2$state <-gsub("Dak\\. \\(now South Dakota\\)", "SD", out2$state)
out2$state <-gsub("Fla\\. \\(then a Spanish colony\\)", "FL", out2$state)
out2$state <-gsub("Ill\\. \\(then a portion of the Northwest Territory\\)", "IL", out2$state)
out2$state <-gsub("Ill\\. January 31", "IL", out2$state)
out2$state <-gsub("Kans\\.  18", "KS", out2$state)
out2$state <-gsub("Ky\\.  29", "KY", out2$state)
out2$state <-gsub("Ky\\. \\(then a part of Virginia\\)", "KY", out2$state)
out2$state <-gsub("Va\\. \\(now Kentucky\\)", "KY", out2$state)
out2$state <-gsub("Mass\\. \\(now Maine\\)", "MA", out2$state)
out2$state <-gsub("Md\\. \\(now a part of Washington", "MD", out2$state)
out2$state <-gsub("Md\\. \\(now the District of Columbia\\)", "MD", out2$state)
out2$state <-gsub("Mich\\. \\(near Sault Ste\\. Marie\\)", "MI", out2$state)
out2$state <-gsub("N\\. Dak\\.", "ND", out2$state)
out2$state <-gsub("N\\.Dak\\.", "ND", out2$state)
out2$state <-gsub("N\\. Mex\\.", "NM", out2$state)
out2$state <-gsub("N\\.Mex\\. \\(then in the Republic of Mexico\\)", "NM", out2$state)
out2$state <-gsub("N\\.Mex\\.", "NM", out2$state)
out2$state <-gsub("N\\.C\\. \\(now TX\\)", "TX", out2$state)
out2$state <-gsub("N\\.Y\\.  16", "NY", out2$state)
out2$state <-gsub("N\\.Y\\.  8", "NY", out2$state)
out2$state <-gsub("Okla\\.)", "OK", out2$state)
out2$state <-gsub("R\\.I\\. on  26", "RI", out2$state)
out2$state <-gsub("S\\. Dak\\. on  9", "SD", out2$state)
out2$state <-gsub("S\\. Dak\\.", "SD", out2$state)
out2$state <-gsub("S\\.Dak\\.", "SD", out2$state)
out2$state <-gsub("S\\.Dak\\.", "SD", out2$state)
out2$state <-gsub("S\\.C\\.  19", "Sc", out2$state)
out2$state <-gsub("Tenn\\. \\(then North Carolina\\)", "TN", out2$state)
out2$state <-gsub("Va\\. \\(now Theodore Roosevelt Island", "VA", out2$state)
out2$state <-gsub("Va\\. \\(then included in the District of Columbia\\)", "VA", out2$state)
out2$state <-gsub("Va\\.\\,", "VA", out2$state)
out2$state <-gsub("Vt\\.  4", "VT", out2$state)
out2$state <-gsub("W\\.Va\\.", "WV", out2$state)
out2$state <-gsub("W\\. Va\\.", "WV", out2$state)
out2$state <-gsub("W\\.Va\\. \\(then Virginia\\)", "WV", out2$state)
out2$state <-gsub("WV \\(then Virginia\\)", "WV", out2$state)
out2$state <-gsub("WV in 1777", "WV", out2$state)

out2$state <-gsub("\\bArk\\b", "AR", out2$state)
out2$state <-gsub("\\bMass\\b", "MA", out2$state)
out2$state <-gsub("\\bConn\\b", "CT", out2$state)
out2$state <-gsub("\\bMiss\\b", "MS", out2$state)
out2$state <-gsub("\\bMinn\\b", "MN", out2$state)
out2$state <-gsub("\\bInd\\b", "IN", out2$state)
out2$state <-gsub("\\bDel\\b", "DE", out2$state)
out2$state <-gsub("\\bMaine\\b", "ME", out2$state)
out2$state <-gsub("\\bCalif\\b", "CA", out2$state)
out2$state <-gsub("\\bTex\\b", "TX", out2$state)
out2$state <-gsub("\\bIll\\b", "IL", out2$state)
out2$state <-gsub("\\bOhio\\b", "OH", out2$state)
out2$state <-gsub("\\bOkla\\b", "OK", out2$state)
out2$state <-gsub("\\bUtah\\b", "UT", out2$state)
out2$state <-gsub("\\bOreg\\b", "OR", out2$state)
out2$state <-gsub("\\bAriz\\b", "AZ", out2$state)
out2$state <-gsub("\\bIowa\\b", "IA", out2$state)
out2$state <-gsub("\\bAla\\b", "AL", out2$state)
out2$state <-gsub("\\bWash\\b", "WA", out2$state)
out2$state <-gsub("\\bNMex\\b", "NM", out2$state)
out2$state <-gsub("\\bN Mex\\b", "NM", out2$state)
out2$state <-gsub("\\bTenn\\b", "TN", out2$state)
out2$state <-gsub("\\bColo\\b", "CO", out2$state)
out2$state <-gsub("\\bOre\\b", "OR", out2$state)
out2$state <-gsub("\\bFla\\b", "FL", out2$state)
out2$state <-gsub("\\bWyo\\b", "WY", out2$state)
out2$state <-gsub("\\bMich\\b", "MI", out2$state)
out2$state <-gsub("\\bKans\\b", "KS", out2$state)
out2$state <-gsub("\\bWis\\b", "WI", out2$state)
out2$state <-gsub("\\bNebr\\b", "NE", out2$state)
out2$state <-gsub("\\bWVa\\b", "WV", out2$state)
out2$state <-gsub("\\bSDak\\b", "SD", out2$state)
out2$state <-gsub("\\bMont\\b", "MT", out2$state)
out2$state <-gsub("\\bNev\\b", "NV", out2$state)
out2$state <-gsub("\\bPenn\\b", "PA", out2$state)

out2$state <- toupper(out2$state)

# Now pull out county

out2$county <- str_extract(as.character(out2$profileText_01_01), "([A-Z].+\\s?)+County")
out2$county <- str_extract(as.character(out2$state), "([A-Z].+\\s?)+County")
out2$county <- ifelse(is.na(out2$county), str_extract(as.character(out2$profileText_01_02), "([A-Z].+\\s?)+County"), out2$county)
out2$county <- ifelse(is.na(out2$county), str_extract(as.character(out2$profileText_01_03), "([A-Z].+\\s?)+County"), out2$county)
out2$county <- ifelse(is.na(out2$county), str_extract(as.character(out2$profileText_01_04), "([A-Z].+\\s?)+County"), out2$county)
out2$county <- ifelse(is.na(out2$county), str_extract(as.character(out2$profileText_01_05), "([A-Z].+\\s?)+County"), out2$county)

out2$state <- gsub("*\\s*", "", out2$state)
out2$state <- gsub("*\\.*", "", out2$state)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
out2$state <- substrRight(out2$state, 2)


# Final Cleaning

out2$location1 <-gsub("January|February|March|April|May|June|July|August|September|October|November|December", "", out2$location1)
out2$location1 <- gsub("Representative", "", out2$location1)
out2$location1 <- gsub("State", "", out2$location1)


## Pull Locations All Together

# Standardize Sections

out2 <- out2 %>% dplyr::select(bioguide_id, location1, state, county, profileText_01_01, profileText_01_02, profileText_01_03, profileText_01_04, profileText_01_05,profileText_01_06,  profileText_02)
out2$hometown_1 <- ifelse(out2$location1 != out2$county | is.na(out2$location1 != out2$county), out2$location1, NA)
out2$hometown_2 <- ifelse(out2$location1 != out2$county & out2$profileText_01_02 != out2$county & is.na(out2$location1 != out2$county) == F & is.na(out2$profileText_01_02 != out2$county) == F, out2$profileText_01_02,
                          ifelse(is.na(out2$county) == F, out2$county,
                                 ifelse(is.na(out2$state), out2$profileText_01_02, out2$state)))
out2$hometown_3 <- ifelse(out2$location1 != out2$county & out2$profileText_01_02 != out2$county & out2$profileText_01_03 != out2$county & is.na(out2$location1 != out2$county) == F & is.na(out2$profileText_01_02 != out2$county) == F & is.na(out2$profileText_01_03 != out2$county) == F, 
                          out2$profileText_01_03, out2$state)
out2$hometown_3 <- ifelse(out2$hometown_3 == out2$hometown_2, NA, out2$hometown_3)
out2$hometown_4 <- ifelse(grepl("[A-Z][A-Z]", out2$hometown_3) | is.na(out2$hometown_3), NA, out2$state)


# Remove all numbers

out2$hometown_1 <- ifelse(grepl("[0-9]",out2$hometown_1), NA, out2$hometown_1)
out2$hometown_2 <- ifelse(grepl("[0-9]",out2$hometown_2), NA, out2$hometown_2)
out2$hometown_3 <- ifelse(grepl("[0-9]",out2$hometown_3), NA, out2$hometown_3)
out2$hometown_4 <- ifelse(grepl("[0-9]",out2$hometown_4), NA, out2$hometown_4)

# Remove "near"

out2$hometown_1 <- ifelse(grepl("near\\s",out2$hometown_1), NA, out2$hometown_1)
out2$hometown_2 <- ifelse(grepl("near\\s",out2$hometown_2), NA, out2$hometown_2)
out2$hometown_3 <- ifelse(grepl("near\\s",out2$hometown_3), NA, out2$hometown_3)
out2$hometown_4 <- ifelse(grepl("near\\s",out2$hometown_4), NA, out2$hometown_4)

## Touch-Up cleaning

# Pull in missing states

out2$state <- ifelse(is.na(out2$state) & out2$hometown_2 == "Ohio", "OH", out2$state)
out2$state <- ifelse(is.na(out2$state) & (out2$hometown_2 == "Maine" | out2$hometown_2 == "Maine (then a district of Massachusetts)" | out2$hometown_2 == "Maine (then a part of Massachusetts)"), "ME", out2$state)
out2$state <- ifelse(is.na(out2$state) & out2$hometown_2 == "Tennessee", "TN", out2$state)
out2$state <- ifelse(is.na(out2$state) & out2$hometown_2 == "Iowa", "IA", out2$state)
out2$state <- ifelse(is.na(out2$state) & out2$hometown_2 == "Ky", "KY", out2$state)
out2$state <- ifelse(is.na(out2$state) & out2$hometown_2 == "Oregon", "OR", out2$state)
out2$state <- ifelse(is.na(out2$state) & (out2$hometown_1 %in% c("New York","New York City", "New York City ", "New York City  ")), "NY", out2$state)
out2$state[out2$state == "EB"] <- "NE"

out2$hometown_2 <- ifelse(out2$hometown_2 == "birth date unknown", NA, out2$hometown_2)
out2$hometown_2 <- ifelse(out2$hometown_2 == " ", NA, out2$hometown_2)
out2$hometown_1 <- ifelse(out2$hometown_1 == " ", NA, out2$hometown_1)

# Fix Odd States, Counties

out2$hometown_1[out2$bioguide_id %in% c("W000187", "S001057")] <- "St. Louis"
out2$state[out2$bioguide_id %in% c("W000187", "S001057")] <- "MO"
out2$hometown_1[out2$bioguide_id %in% c("D000265")] <- "Ville Platte"
out2$county[out2$bioguide_id %in% c("D000265")] <- "Evangeline Parish"
out2$state[out2$bioguide_id %in% c("D000265")] <- "LA"
out2$hometown_1[out2$bioguide_id %in% c("E000254")] <- "Christiansted"
out2$hometown_2[out2$bioguide_id %in% c("E000254")] <- "St. Croix"
out2$state[out2$bioguide_id %in% c("E000254")] <- "VI"
out2$hometown_1[out2$bioguide_id %in% c("B000212")] <- "Greeneville"
out2$state[out2$bioguide_id %in% c("B000212")] <- "TN"
out2$hometown_1[out2$bioguide_id %in% c("S001215")] <- "Rochester Hills"
out2$county[out2$bioguide_id %in% c("S001215")] <- "Oakland County"
out2$state[out2$bioguide_id %in% c("S001215")] <- "MI"
out2$hometown_1[out2$bioguide_id %in% c("K000158")] <- "Leonardtown"
out2$county[out2$bioguide_id %in% c("K000158")] <- "St. Mary's County"
out2$state[out2$bioguide_id %in% c("K000158")] <- "MD"
out2$hometown_1[out2$bioguide_id %in% c("S000474")] <- "England"
out2$state[out2$bioguide_id %in% c("S000474")] <- NA
out2$hometown_1[out2$bioguide_id %in% c("T000171")] <- "Shepherdstown"
out2$state[out2$bioguide_id %in% c("T000171")] <- "WV"
out2$hometown_1[out2$bioguide_id %in% c("H000834")] <- "Lexington"
out2$hometown_1[out2$bioguide_id %in% c("H000517")] <- "Henrys Mills"
out2$hometown_1[out2$bioguide_id %in% c("F000221")] <- "Louisville"
out2$hometown_1[out2$bioguide_id %in% c("C000360")] <- "Saint Albans"
out2$county[out2$bioguide_id %in% c("H000517")] <- "Scott County"
out2$county[out2$bioguide_id %in% c("F000221")] <- "Jefferson County"
out2$county[out2$bioguide_id %in% c("C000360")] <- "Kanawha County"
out2$state[out2$bioguide_id %in% c("H000834", "H000517", "F000221")] <- "KY"
out2$state[out2$bioguide_id %in% c("C000360")] <- "WV"
out2$hometown_1[out2$bioguide_id %in% c("G000474")] <- "Sioux Falls"
out2$state[out2$bioguide_id %in% c("G000474")] <- "SD"
out2$hometown_1[out2$bioguide_id %in% c("P000250")] <- "Georgetown"
out2$hometown_2[out2$bioguide_id %in% c("P000250")] <- "Washington"
out2$state[out2$bioguide_id %in% c("P000250")] <- "DC"
out2$county[out2$bioguide_id %in% c("S000965")] <- "Sevier County"
out2$state[out2$bioguide_id %in% c("S000965")] <- "TN"
out2$hometown_1[out2$bioguide_id %in% c("T000104")] <- "Alexandria"
out2$state[out2$bioguide_id %in% c("T000104")] <- "VA"
out2$hometown_1[out2$bioguide_id %in% c("C001033")] <- "Cutts Island"
out2$state[out2$bioguide_id %in% c("C001033")] <- "ME"
out2$hometown_1[out2$bioguide_id %in% c("H000891")] <- "Wheeling"
out2$state[out2$bioguide_id %in% c("H000891")] <- "WV"
out2$hometown_1[out2$bioguide_id %in% c("N000158")] <- "Danville"
out2$county[out2$bioguide_id %in% c("M000467")] <- "Fayette County"
out2$state[out2$bioguide_id %in% c("M000467", "N000158")] <- "KY"
out2$hometown_1[out2$bioguide_id %in% c("S001142")] <- "Pendleton"
out2$state[out2$bioguide_id %in% c("S001142")] <- "OR"

out2$state <- ifelse(is.na(out2$state) & grepl("Maine", out2$hometown_2), "ME", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("Pennsyl", out2$hometown_2), "PA", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("Ohio", out2$hometown_2), "OH", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("Ohio", out2$profileText_01_03), "OH", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("New Mexico", out2$hometown_2), "NM", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("Pickens District", out2$hometown_1), "SC", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("Indiana", out2$profileText_01_03), "IN", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("North Carolina", out2$hometown_2), "CA", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("Oklahoma", out2$hometown_2), "OK", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("Oklahoma", out2$profileText_01_03), "OK", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("South Dakota", out2$profileText_01_03), "SD", out2$state)
out2$state <- ifelse(is.na(out2$state) & grepl("Baltimore Md", out2$hometown_1), "MD", out2$state)
out2$hometown_1 <- ifelse(is.na(out2$state) & grepl("Baltimore Md", out2$hometown_1), "Baltimore", out2$hometown_1)
out2$state <- ifelse(is.na(out2$state) & grepl("Hawai", out2$hometown_2), "HI", out2$state)
out2$hometown_2 <- ifelse(is.na(out2$state) & grepl("Saint Mary", out2$hometown_2), "Saint Marys County", out2$hometown_2)
out2$state <- ifelse(is.na(out2$state) & grepl("Saint Mary", out2$hometown_2), "MD", out2$state)

# Pull in missing counties

out2$county <- ifelse(is.na(out2$county) & grepl(" County", out2$hometown_2), out2$hometown_2, out2$county)

subset(out2, bioguide_id == "W000509")$hometown_1
View(out[out$bioguide_id == "W000509",])
View(subset(out2, is.na(state)))

# Paste final names together
# County if no hometown; cycle through all hometown before county

out2$hometown_1 <- ifelse(nchar(out2$hometown_1) < 3, NA, out2$hometown_1)
out2$hometown_1 <- ifelse(grepl("Fond", out2$hometown_1), "Fond du Lac", out2$hometown_1)
out2$hometown_1 <- ifelse(grepl("n Island", out2$hometown_1), "Staten Island", out2$hometown_1)

out2$hometown_final <- ifelse(is.na(out2$hometown_1) == F & is.na(out2$hometown_2) == F & is.na(out2$hometown_3) & is.na(out2$hometown_4),
                              paste(out2$hometown_1, out2$hometown_2, sep = ", "),
                              ifelse(is.na(out2$hometown_1) == F & is.na(out2$hometown_2) == F & is.na(out2$hometown_3) == F & is.na(out2$hometown_4),
                                     paste(out2$hometown_1, out2$hometown_2, out2$hometown_3, sep = ", "), 
                                     ifelse(is.na(out2$hometown_1) == F & is.na(out2$hometown_2) == F & is.na(out2$hometown_3) == F & is.na(out2$hometown_4) == F,
                                            paste(out2$hometown_1, out2$hometown_2, out2$hometown_3, out2$hometown_4, sep = ", "),
                                            ifelse(is.na(out2$hometown_1) & is.na(out2$hometown_2) == F & is.na(out2$hometown_3) == F & is.na(out2$hometown_4) == F,
                                                   paste(out2$hometown_2, out2$hometown_3, out2$hometown_4, sep = ", "),
                                                   ifelse(is.na(out2$hometown_1) == F & is.na(out2$hometown_2) & is.na(out2$hometown_3) & is.na(out2$hometown_4), out2$hometown_1,
                                                          ifelse(is.na(out2$hometown_1) & is.na(out2$hometown_2) == F & is.na(out2$hometown_3) & is.na(out2$hometown_4),
                                                                paste(out2$county, out2$state, sep = ", "),
                                                                        ifelse(is.na(out2$hometown_1) == F & is.na(out2$hometown_2) & is.na(out2$hometown_3) == F & is.na(out2$hometown_4) == F,
                                                                              paste(out2$hometown_1, out2$hometown_3, out2$hometown_4,sep = ", "),
                                                                                    ifelse(is.na(out2$hometown_1) == F & is.na(out2$hometown_2) == F & is.na(out2$hometown_3) & is.na(out2$hometown_4) == F,
                                                                                          paste(out2$hometown_1, out2$hometown_2, out2$hometown_4, sep = ", "),
                                                                                          ifelse(is.na(out2$hometown_1) == F & is.na(out2$hometown_2)& is.na(out2$hometown_3) == F & is.na(out2$hometown_4),
                                                                                                 paste(out2$hometown_1, out2$hometown_3, sep = ", "),
                                                                                                 paste(out2$hometown_2, out2$hometown_3, sep = ", ")
                                     )))))))))
out2$hometown_final <- ifelse(out2$hometown_final == "NA, NA", NA, out2$hometown_final)

# Final strange cases

out2$hometown_final[out2$bioguide_id == "V000083"] <- "Worsham, VA"
out2$hometown_final[out2$hometown_1 == "a County"] <- "Augusta County, VA"
out2$hometown_final[out2$bioguide_id == "C001033"] <- "Cutts Island, ME"
out2$hometown_final[out2$bioguide_id == "M000496"] <- "Lexington, Rock Bridge County, VA"
out2$hometown_final[out2$hometown_final == "NA, GA"] <- "Augusta, GA"
out2$hometown_final[out2$bioguide_id == "B001288"] <- "Washington, DC"
out2$hometown_final[out2$hometown_final == "NA, CA"] <- "NC"
out2$hometown_final[out2$bioguide_id == "C001088"] <- "Greenwich, CT"
out2$hometown_final[out2$bioguide_id == "C000811"] <- "Tipton, Cedar County, IA"
out2$hometown_final[out2$bioguide_id == "F000442"] <- "Elgin, IL"
out2$hometown_final[out2$bioguide_id == "C001041"] <- "Chicago, IL"
out2$hometown_final[out2$bioguide_id == "C001042"] <- "Taylorville, Christian County, IL"
out2$hometown_final[out2$bioguide_id == "B000819"] <- "South Bend, IN"
out2$hometown_final[out2$bioguide_id == "M000836"] <- "New Palestine, IN"
out2$hometown_final[out2$bioguide_id == "G000165"] <- "Versailles, Woodford County, KY"
out2$hometown_final[out2$bioguide_id == "D000254"] <- "Charles County, MD"
out2$hometown_final[out2$bioguide_id == "H000617"] <- "Baltimore, MD"
out2$hometown_final[out2$bioguide_id == "P000465"] <- "Augusta, ME"
out2$hometown_final[out2$bioguide_id == "B001268"] <- "Kittery, ME"
out2$hometown_final[out2$bioguide_id == "B001237"] <- "Annandale, MN"
out2$hometown_final[out2$bioguide_id == "J000293"] <- "Mankato, MN"
out2$hometown_final[out2$bioguide_id == "K000388"] <- "Union, MS"
out2$hometown_final[out2$bioguide_id == "W000818"] <- "Butte, MT"
out2$hometown_final[out2$bioguide_id == "N000180"] <- "McCook, Red Willow County, NE"
out2$hometown_final[out2$bioguide_id == "C000928"] <- "Andover, Ashtabula County, OH"
out2$hometown_final[out2$bioguide_id == "B001261"] <- "Reading, PA"
out2$hometown_final[out2$bioguide_id == "F000435"] <- "Guayama, PR"
out2$hometown_final[out2$bioguide_id == "C000350"] <- "Rocky River, Abbeville County, SC"
out2$hometown_final[out2$bioguide_id == "M001183"] <- "Marion County, WV"
out2$hometown_final[out2$bioguide_id == "W000181"] <- "Cedar Hill, TN"
out2$hometown_final[out2$bioguide_id == "T000368"] <- "Bedford, VA"
out2$hometown_final[out2$bioguide_id == "M000495"] <- "Rockbridge County, VA"
out2$hometown_final[out2$bioguide_id == "T000027"] <- "Fredericksburg, VA"
out2$hometown_final[out2$bioguide_id == "G000006"] <- "Augusta, WV"
out2$hometown_final[out2$bioguide_id == "R000410"] <- "Buckhannon, WV"
out2$hometown_final[out2$bioguide_id == "F000045"] <- "Martinsburg, WV"
out2$hometown_final[out2$bioguide_id == "G000561"] <- "Mt. Alto, WV"
out2$hometown_final[out2$bioguide_id == "E000008"] <- "Landrum, Spartanburg County, SC"
out2$hometown_final[out2$bioguide_id == "C000436"] <- "Bedford, VA"
out2$hometown_final[out2$bioguide_id == "S000493"] <- "NJ"
out2$hometown_final[out2$bioguide_id == "D000151"] <- "VA"
out2$hometown_final[out2$bioguide_id == "B000482"] <- "NY"
out2$hometown_final[out2$bioguide_id == "L000593"] <- "Inglewood, CA"
out2$hometown_final[out2$bioguide_id == "T000050"] <- "NC"
out2$hometown_final[out2$bioguide_id == "C000226"] <- "SC"
out2$hometown_final[out2$bioguide_id == "C000510"] <- "Hartford County, MD"
out2$hometown_final[out2$bioguide_id == "L000290"] <- "VA"
out2$hometown_final[out2$bioguide_id == "H000371"] <- "NY"
out2$hometown_final[out2$bioguide_id == "M001110"] <- "PA"
out2$hometown_final[out2$bioguide_id == "W000509"] <- "MD"
out2$hometown_final[out2$bioguide_id == "H000125"] <- "Ireland"
out2$hometown_final[out2$bioguide_id == "C000250"] <- "Island of Madeira"
out2$hometown_final[out2$bioguide_id == "S000347"] <- "NY"
out2$hometown_final[out2$bioguide_id == "M000331"] <- "Dresden, Saxony"
out2$hometown_final[out2$bioguide_id == "W000312"] <- "London, England"
out2$hometown_final[out2$bioguide_id == "O000073"] <- "Ireland"
out2$hometown_final[out2$bioguide_id == "C000510"] <- "Hartford County, MD"
out2$hometown_final[out2$bioguide_id == "K000222"] <- "Buttevant, County Cork, Ireland"
out2$hometown_final[out2$bioguide_id == "J000038"] <- "Schleswig-Holstein"
out2$hometown_final[out2$bioguide_id == "H000741"] <- "Molnbacka, Varmland"
out2$hometown_final[out2$bioguide_id == "W000714"] <- "British Columbia, Canada"
out2$hometown_final[out2$bioguide_id == "M000045"] <- "Livonia, MO"
out2$hometown_final[out2$bioguide_id == "J000303"] <- "Birmingham, AL"
out2$hometown_final[out2$bioguide_id == "C001102"] <- "Tacoma, WA"
out2$hometown_final[out2$bioguide_id == "S001164"] <- "Cincinnati, OH"
out2$hometown_final[out2$bioguide_id == "H001087"] <- "Abilene, TX"
out2$hometown_final[out2$bioguide_id == "M001201"] <- "Boston, MA"
out2$hometown_final[out2$bioguide_id == "C001123"] <- "Los Angeles, CA"
out2$hometown_final[out2$bioguide_id == "L000588"] <- "Washington, DC"
out2$hometown_final[out2$bioguide_id == "H001082"] <- "Belton, MO"
out2$hometown_final[out2$bioguide_id == "W000826"] <- "Wiesbaden Air Force Base, Germany"
out2$hometown_final[out2$bioguide_id == "G000591"] <- "Woodbury, NJ"
out2$hometown_final[out2$bioguide_id == "M001204"] <- "Babylon, NY"
out2$hometown_final[out2$bioguide_id == "R000610"] <- "Pittsburgh, PA"
out2$hometown_final[out2$bioguide_id == "B001311"] <- "Charlotte, NC"
out2$hometown_final[out2$bioguide_id == "M001210"] <- "Raleigh, NC"
out2$hometown_final[out2$bioguide_id == "M001212"] <- "Enterprise, AL"
out2$hometown_final[out2$bioguide_id == "B001312"] <- "Roanoke, VA"
out2$hometown_final[out2$bioguide_id == "F000470"] <- "Woodbury, MN"
out2$hometown_final[out2$bioguide_id == "M001135"] <- "Detroit, MI"
out2$hometown_final[out2$bioguide_id == "G000594"] <- "San Antonio, TX"
out2$hometown_final[out2$bioguide_id == "R000435"] <- "Havana, Cuba"
out2$hometown_final[out2$bioguide_id == "B001231"] <- "New York, NY"
out2$hometown_final[out2$bioguide_id == "M000797"] <- "Maui County, HI"
out2$hometown_final[out2$bioguide_id == "K000307"] <- "McDonald, OH"
out2$hometown_final[out2$bioguide_id == "B001220"] <- "Baltimore, MD"
out2$hometown_final[out2$bioguide_id == "H000747"] <- "Birmingham, AL"
out2$hometown_final[out2$bioguide_id == "H000174"] <- "Multnomah County, OR"
out2$hometown_final[out2$bioguide_id == "G000471"] <- "Lawrence County, MO"
out2$hometown_final[out2$bioguide_id == "S000764"] <- "Bridgnorth, England"
out2$hometown_final[out2$bioguide_id == "L000252"] <- "Detroit, Wayne County, MI"
out2$hometown_final[out2$bioguide_id == "N000052"] <- "Tillamook County, OR"
out2$hometown_final[out2$bioguide_id == "B000407"] <- "Nieder Rebbach, Austria-Hungary"
out2$hometown_final[out2$bioguide_id == "M000216"] <- "Theodore Roosevelt Island, Washington, DC"
out2$hometown_final[out2$bioguide_id == "J000087"] <- "Caernarvon Township, Lancaster County, Pennsylvania"
out2$hometown_final[out2$bioguide_id == "H000967"] <- "Caernarvon Township, Lancaster County, Pennsylvania"
out2$hometown_final[out2$bioguide_id == "P000386"] <- "Powhatan County, VA"
out2$hometown_final[out2$bioguide_id == "T000017"] <- "Bedford County, VA"
out2$hometown_final[out2$bioguide_id == "B000258"] <- "Wellington Estates, Wicomico County, MD"
out2$hometown_final[out2$bioguide_id == "R000181"] <- "County Londonderry, Ireland"
out2$hometown_final[out2$hometown_final == "Berkeley County, VA"] <- "Berkeley County, WV"
out2$hometown_final[out2$bioguide_id == "B000234"] <- "Greene, Androscoggin County, ME"
out2$hometown_final[out2$bioguide_id == "G000278"] <- "Charlotte County, VA"
out2$hometown_final[out2$bioguide_id == "M000219"] <- "Clear Spring, Washington County, MD"
out2$hometown_final[out2$bioguide_id == "H000395"] <- "Fairmont, WV"
out2$hometown_final[out2$bioguide_id == "M000216"] <- "Theodore Roosevelt Island, Washington, DC"
out2$hometown_final[out2$bioguide_id == "O000104"] <- "Craytonville, SC"
out2$hometown_final[out2$hometown_final == "Cabell County, VA"] <- "Cabell County, WV"
out2$hometown_final[out2$bioguide_id == "S000627"] <- "Marengo, VA"
out2$hometown_final[out2$bioguide_id == "M000017"] <- "Marion, GA"
out2$hometown_final[out2$bioguide_id %in% c("B001078")] <- "Conneaut Lake, PA"
out2$hometown_final[out2$hometown_final == "Greenbrier County, VA"] <- "Greenbrier County, WV"
out2$hometown_final[out2$hometown_final == "Hillsborough, Jasper County, GA"] <- "Hillsboro, Jasper County, GA"
out2$hometown_final[out2$bioguide_id == "S000249"] <- "Koeln, Germany"
out2$hometown_final[out2$bioguide_id == "D000053"] <- "Darlington, Darlington County, SC"
out2$hometown_final[out2$bioguide_id == "T000426"] <- "Ballard County, KY"
out2$hometown_final[out2$bioguide_id == "K000265"] <- "St. John,  Missouri"
out2$hometown_final[out2$bioguide_id == "S000666"] <- "Amsterdam, NY"
out2$hometown_final[out2$bioguide_id == "H000379"] <- "Monroe, Benton County, OR"
out2$hometown_final[out2$bioguide_id == "H000824"] <- "Formoso, Jewell County, KS"
out2$hometown_final[out2$bioguide_id == "M000106"] <- "Bethel Township, Fulton County, PA"
out2$hometown_final[out2$bioguide_id == "S000865"] <- "Carleton County, New Brunswick, Canada"
out2$hometown_final[out2$bioguide_id == "S000865"] <- "Carleton County, New Brunswick, Canada"
out2$hometown_final[out2$hometown_final == "Odessa, Russia"] <- "Odessa, Ukraine"
out2$hometown_final[out2$hometown_final == "Vilna, Russia"] <- "Vilnius, Lithuania"
out2$hometown_final[out2$hometown_final == "Grand Duchy"] <- "Luxembourg"
out2$hometown_final[out2$hometown_final == "Grand Duchy "] <- "Luxembourg"
out2$hometown_final[out2$hometown_final == "Unagh, Ireland"] <- "Cookstown, Ireland"
out2$hometown_final[out2$hometown_final == "Unagh"] <- "Cookstown, Ireland"
out2$hometown_final[out2$bioguide_id == "C000114"] <- "Moores Hill, IN"
out2$hometown_final[out2$hometown_final == "Kety, Galicia"] <- "Kety, Poland"
out2$hometown_final[out2$hometown_final == "Gnesen"] <- "Gniezno, Poland"
out2$hometown_final[out2$bioguide_id == "K000332"] <- NA
out2$hometown_final[out2$bioguide_id == "I000039"] <- "Argentina"
out2$hometown_final[out2$bioguide_id == "A000018"] <- "Bronx, NY"
out2$hometown_final[out2$bioguide_id == "B001158"] <- "Bialystock, Poland"

# Fixing individuals whose names got placed into their hometown
out2$hometown_final[out2$bioguide_id == "R000184"] <- "Beaufort, SC"
out2$hometown_final[out2$bioguide_id == "G000435"] <- "Boone County, KY"
out2$hometown_final[out2$bioguide_id == "K000040"] <- "Radford, Montgomery County, Va"
out2$hometown_final[out2$bioguide_id == "G000368"] <- "Easton, Northampton County, PA"
out2$hometown_final[out2$bioguide_id == "W000256"] <- "Chicago, IL"
out2$hometown_final[out2$bioguide_id == "M000273"] <- "Yakima, WA"
out2$hometown_final[out2$bioguide_id == "B000064"] <- "Sevierville, Sevier County, TN"
out2$hometown_final[out2$bioguide_id == "T000178"] <- "Nacogdoches, TX"
out2$hometown_final[out2$bioguide_id == "C000371"] <- "Brooklyn, Kings County, NY"
out2$hometown_final[out2$bioguide_id == "A000205"] <- "Geneva, AL"
out2$hometown_final[out2$bioguide_id == "H000566"] <- "South Boston, MA"
out2$hometown_final[out2$bioguide_id == "C000634"] <- "St. Louis, MO"
out2$hometown_final[out2$bioguide_id == "P000273"] <- "Mountain View, Santa Clara County, CA"
out2$hometown_final[out2$bioguide_id == "K000162"] <- "Hutchinson, Reno County, KA"
out2$hometown_final[out2$bioguide_id == "S000622"] <- "Randolph, Fremont County, IA"
out2$hometown_final[out2$bioguide_id == "L000381"] <- "Fort Smith, Sebastian County, AR"
out2$hometown_final[out2$bioguide_id == "S000663"] <- "Augusta, Kennebec County, ME"
out2$hometown_final[out2$bioguide_id == "M000195"] <- "Chicago, Cook County, IL"
out2$hometown_final[out2$bioguide_id == "H000058"] <- "Mound Bayou, Bolivar County, MS"
out2$hometown_final[out2$bioguide_id == "A000220"] <- "Cincinnati, Hamilton County, OH"
out2$hometown_final[out2$bioguide_id == "S000136"] <- "Clairton, PA"
out2$hometown_final[out2$bioguide_id == "B000711"] <- "Brooklyn, Kings County, NY"
out2$hometown_final[out2$bioguide_id == "V000124"] <- "Camp Dix, New Jersey"
out2$hometown_final[out2$bioguide_id == "P000197"] <- "Baltimore, MD"
out2$hometown_final[out2$bioguide_id == "S000480"] <- "Harlan County, KY"
out2$hometown_final[out2$bioguide_id == "U000017"] <- "Corvallis, Benton County, OR"
out2$hometown_final[out2$bioguide_id == "B001228"] <- "Cleveland, Cuyahoga County, OH"
out2$hometown_final[out2$bioguide_id == "M000309"] <- "Brooklyn, Kings County, NY"
out2$hometown_final[out2$bioguide_id == "B000607"] <- "Cleveland, OH"
out2$hometown_final[out2$bioguide_id == "S000248"] <- "Mayaguez, PR"
out2$hometown_final[out2$bioguide_id == "M000687"] <- "Baltimore, MD"
out2$hometown_final[out2$bioguide_id == "K000332"] <- "Russia"
out2$hometown_final[out2$bioguide_id == "S000590"] <- "Skowhegan, Somerset County, Maine"
out2$hometown_final[out2$bioguide_id == "K000370"] <- "Washington DC"
out2$hometown_final[out2$bioguide_id == "A000097"] <- "Texas Valley, Cortland County, NY"
out2$hometown_final[out2$bioguide_id == "G000093"] <- "Richland County, SC"
out2$hometown_final[out2$bioguide_id == "M001051"] <- "Duchamp, St. Martin Parish, LA"
out2$hometown_final[out2$bioguide_id == "O000018"] <- "Barbour County, VA"
out2$hometown_final[out2$bioguide_id == "K000085"] <- "Amite, St. Helena Parish, LA"
out2$hometown_final[out2$bioguide_id == "R000474"] <- "Aurora, Dearborn County, IN"
out2$hometown_final[out2$bioguide_id == "B000712"] <- "Laurel, Sussex County, DE"
out2$hometown_final[out2$bioguide_id == "M001060"] <- "Charles County, MD"
out2$hometown_final[out2$bioguide_id == "P000051"] <- "Tuskegee, Macon County, AL"
out2$hometown_final[out2$bioguide_id == "H000891"] <- "Wheeling, VA"
out2$hometown_final[out2$bioguide_id == "H000724"] <- "Aurora, Dearborn County, IN"
out2$hometown_final[out2$bioguide_id == "H000680"] <- "Smyrna, DE"
out2$hometown_final[out2$bioguide_id == "B000896"] <- "New Iberia, Iberia Parish, LA"
out2$hometown_final[out2$bioguide_id == "E000189"] <- "Charles Town, WV"

out2$hometown_final[out2$bioguide_id == "M000294"] <- "Ulster, Ireland"
out2$hometown_final[out2$bioguide_id == "B000552"] <- "Ulster, Ireland"
out2$hometown_final[out2$bioguide_id == "S000648"] <- "Ulster, Ireland"
out2$hometown_final[out2$bioguide_id == "P000316"] <- "Ulster, Ireland"

out2$hometown_final[out2$bioguide_id == "A000292"] <- "Farmington, St. Francois County, MO"
out2$hometown_final[out2$bioguide_id == "M001059"] <- "Charles County, MD"
out2$hometown_final[out2$bioguide_id == "B000924"] <- "Dillsboro, Dearborn County, IN"
out2$hometown_final[out2$bioguide_id == "P000568"] <- "Budapest, Hungary"
out2$hometown_final[out2$bioguide_id == "E000270"] <- "West Chester, West Whiteland Township, Chester County, PA"
out2$hometown_final[out2$bioguide_id == "O000005"] <- "Troy, Pike County, AL"
out2$hometown_final[out2$bioguide_id == "R000225"] <- "Sumter, Sumter County, SC"
out2$hometown_final[out2$bioguide_id == "W000399"] <- "Fort Wayne, IN"
out2$hometown_final[out2$bioguide_id == "C000417"] <- "Farmington, MO"
out2$hometown_final[out2$bioguide_id == "W000753"] <- "McArthur, OH"
out2$hometown_final[out2$bioguide_id == "T000406"] <- "Aurora, Dearborn County, IN"
out2$hometown_final[out2$bioguide_id == "S000568"] <- "Dinwiddie Court House, Dinwiddie County, VA"
out2$hometown_final[out2$bioguide_id == "R000340"] <- "Unionville Center, Union County, OH"
out2$hometown_final[out2$bioguide_id == "P000038"] <- "Manchester, Dearborn County, IN"
out2$hometown_final[out2$bioguide_id == "A000312"] <- "Gillett, AR"
out2$hometown_final[out2$bioguide_id == "F000095"] <- "Mackinac Island, MI"
out2$hometown_final[out2$bioguide_id == "C000559"] <- "Skowhegan, ME"
out2$hometown_final[out2$bioguide_id == "P000435"] <- "Lawrenceburg, Dearborn County, IN"
out2$hometown_final[out2$bioguide_id == "H000732"] <- "Aurora, Dearborn County, IN"
out2$hometown_final[out2$bioguide_id == "T000152"] <- "Thibodeaux, Terrebonne Parish, LA"
out2$hometown_final[out2$bioguide_id == "J000222"] <- "King and Queen County, VA"
out2$hometown_final[out2$bioguide_id == "H000971"] <- "Guyana"
out2$hometown_final[out2$bioguide_id == "W000357"] <- "Vienna, ME"
out2$hometown_final[out2$bioguide_id == "G000449"] <- "Moffat, Dumfriesshire, Scotland"
out2$hometown_final[out2$bioguide_id == "D000547"] <- "New York City"
out2$hometown_final[out2$bioguide_id == "D000245"] <- "Worcester County, MD"
out2$hometown_final[out2$bioguide_id == "S000729"] <- "Talbot County, MD"
out2$hometown_final[out2$bioguide_id == "B000023"] <- "Peru, MA"
out2$hometown_final[out2$bioguide_id == "L000483"] <- "Jacksonborough, St. Bartholomew's parish, SC"
out2$hometown_final[out2$bioguide_id == "R000291"] <- "King William County, VA"
out2$hometown_final[out2$bioguide_id == "L000290"] <- "Virginia"
out2$hometown_final[out2$bioguide_id == "D000151"] <- "Virginia"
out2$hometown_final[out2$bioguide_id == "D000243"] <- "Worcester County, MD"
out2$hometown_final[out2$bioguide_id == "J000005"] <- "Waxhaw Settlement in South Carolina"
out2$hometown_final[out2$bioguide_id == "P000065"] <- "Wight County, VA"
out2$hometown_final[out2$bioguide_id == "J000017"] <- "Devonshire, England"



#fix Roger Nelson
out2$hometown_final[out2$hometown_final == "Point , MD"] <- "Frederick, MD"

# Write out!!

final <- out2 %>% dplyr::select(bioguide_id, county, state, hometown_1, hometown_2, hometown_3, hometown_4, hometown_final)

final$hometown_final[final$hometown_final=="Panhandle "] <- "Panhandle, VA"

write.csv(final, "hometowns_allcongresses.csv", row.names = F)


## Connect to coordinates
require("ggmap")
require(tidyverse)

final <- read.csv("hometowns_allcongresses.csv")

#You need a google key to run this; note that we geocoded our data on Feb. 22, 2022; any changes to Google Maps will be reflected if you re-run the code
#register_google(key = "")
latlong <- geocode(unique(subset(final, is.na(hometown_final) == F)$hometown_final),key="")
latlong <- cbind(latlong, unique(subset(final, is.na(hometown_final) == F)$hometown_final))
names(latlong)[3] <- "hometown_final"

final <- left_join(final, latlong)
View(subset(final, is.na(lat)))

# Final fixes!
final$lat[final$bioguide_id == "G000279"] <- 36.6168101
final$lon[final$bioguide_id == "G000279"] <- -78.2083291
final$lat[final$bioguide_id == "O000122"] <- 38.040278
final$lon[final$bioguide_id == "O000122"] <- -97.811389
final$lat[final$bioguide_id == "S000643"] <- 34.2556504
final$lon[final$bioguide_id == "S000643"] <- -84.3413126
final$lat[final$bioguide_id == "B000407"] <- 11.43936
final$lon[final$bioguide_id == "B000407"] <- 47.17628
final$lat[final$bioguide_id %in% c("R000373", "M001198")] <- 37.81724
final$lon[final$bioguide_id %in% c("R000373", "M001198")] <- -96.8622524

final$hometown_final[final$bioguide_id=="B001102"] <- "Los Angeles, California"
final$lat[final$bioguide_id == "B001102"] <- 34.1
final$lon[final$bioguide_id == "B001102"] <- -118

final$hometown_final[final$bioguide_id=="P000215"] <- "Albert Lea, Freeborn County, Minnesota"
final$lat[final$bioguide_id == "P000215"] <- 43.6
final$lon[final$bioguide_id == "P000215"] <- -93.4


#fix a missing value for george mahon
final$county[final$bioguide_id=="M000065"] <- "Claiborne Parish"
final$state[final$bioguide_id=="M000065"] <- "LA"
final$hometown_final[final$bioguide_id=="M000065"] <- "Mahon, Claiborne Parish, LA"
final$lon[final$bioguide_id=="M000065"] <- -93.0
final$lat[final$bioguide_id=="M000065"] <- 32.8


#HUNGERFORD, John Pratt
final$lat[final$bioguide_id=="H000967"] <- 38.11273
final$lon[final$bioguide_id=="H000967"] <- -76.77982	
final$hometown_final[final$bioguide_id=="H000967"] <- "Westmoreland County, Virginia"  

#ALEXANDER, John	
final$lat[final$bioguide_id=="A000096"] <- 34.94957
final$lon[final$bioguide_id=="A000096"] <-  -81.93205	
final$hometown_final[final$bioguide_id=="A000096"] <- "Spartanburg District, South Carolina"  

#MILLER, Stephen Decatur	
final$lat[final$bioguide_id=="M000755"] <- 34.72534
final$lon[final$bioguide_id=="M000755"] <- -80.67708	
final$hometown_final[final$bioguide_id=="M000755"] <- "Lancaster County, South Carolina"  

#BURROWS, Daniel		
final$lat[final$bioguide_id=="B001140"] <- 41.34619
final$lon[final$bioguide_id=="B001140"] <- -72.02409	
final$hometown_final[final$bioguide_id=="B001140"] <- "Fort Hill, Groton, Connecticut"  

#BLAIR, James		
final$lat[final$bioguide_id=="B000526"] <- 34.72534
final$lon[final$bioguide_id=="B000526"] <- -80.67708	
final$hometown_final[final$bioguide_id=="B000526"] <- "Lancaster County, South Carolina"  

#HENRY, Robert Pryor			
final$lat[final$bioguide_id=="H000517"] <- 38.31724
final$lon[final$bioguide_id=="H000517"] <--84.56415	
final$hometown_final[final$bioguide_id=="H000517"] <- "Scott County, Kentucky"  

#BLAIR, John	
final$lat[final$bioguide_id=="B000528"] <- 36.29427
final$lon[final$bioguide_id=="B000528"] <- -82.47348		
final$hometown_final[final$bioguide_id=="B000528"] <- "Jonesboro, Washington County, Tennesse"  

#STODDERT, John Truman		
final$lat[final$bioguide_id=="S000944"] <- 38.45561
final$lon[final$bioguide_id=="S000944"] <- -77.21529		
final$hometown_final[final$bioguide_id=="S000944"] <- "Nanjemoy, Charles County, Maryland"  

#WHITE, John			
final$lat[final$bioguide_id=="W000380"] <- 36.60726
final$lon[final$bioguide_id=="W000380"] <- -83.71428			
final$hometown_final[final$bioguide_id=="W000380"] <- "Middlesboro, Kentucky"  

#PENNYBACKER, Isaac Samuels	
final$lat[final$bioguide_id=="P000216"] <- 38.6479
final$lon[final$bioguide_id=="P000216"] <- -78.67141			
final$hometown_final[final$bioguide_id=="P000216"] <- "Newmarket, Shenandoah County, VA"  

#ELLIOTT, John Milton	
final$lat[final$bioguide_id=="E000125"] <- 36.71262
final$lon[final$bioguide_id=="E000125"] <- -82.65321					
final$hometown_final[final$bioguide_id=="E000125"] <- "Clinch River, Scott County, VA"  

#WORTENDYKE, Jacob Reynier	
final$lat[final$bioguide_id=="W000746"] <- 40.88593
final$lon[final$bioguide_id=="W000746"] <- -74.04347						
final$hometown_final[final$bioguide_id=="W000746"] <- "Hackensack, Bergen County, NJ"  

#AHL, John Alexander		
final$lat[final$bioguide_id=="A000060"] <- 39.96483
final$lon[final$bioguide_id=="A000060"] <- -77.70641						

#DUNLAP, George Washington	
final$lat[final$bioguide_id=="D000542"] <- 38.06065
final$lon[final$bioguide_id=="D000542"] <- -84.48026						
final$hometown_final[final$bioguide_id=="D000542"] <- "Lexington, Fayette County, KY"  

#WINANS, James January	
final$lat[final$bioguide_id=="W000626"] <- 38.64119
final$lon[final$bioguide_id=="W000626"] <- -83.74437							
final$hometown_final[final$bioguide_id=="W000626"] <- "Maysville, KY"  

#ROGERS, John		
final$lat[final$bioguide_id=="R000397"] <- 43.42618
final$lon[final$bioguide_id=="R000397"] <- -73.71234									
final$hometown_final[final$bioguide_id=="R000397"] <- "Lake George, Warren County, NY"  

#SPENCER, William Brainerd	
final$lat[final$bioguide_id=="S000731"] <- 31.70531
final$lon[final$bioguide_id=="S000731"] <- -91.90992										
final$hometown_final[final$bioguide_id=="S000731"] <- "Catahoula Parish, LA"  

#FINDLAY, John Van Lear	
final$lat[final$bioguide_id=="F000121"] <- 39.60065
final$lon[final$bioguide_id=="F000121"] <- -77.82055											
final$hometown_final[final$bioguide_id=="F000121"] <- "Williamsport, Washington County, MD"  

#FRENCH, Carlos	
final$lat[final$bioguide_id=="F000376"] <- 41.39715
final$lon[final$bioguide_id=="F000376"] <- -73.07475											
final$hometown_final[final$bioguide_id=="F000376"] <- "Seymour, CT"  

#McCLELLAN, Charles A. O.	
final$lat[final$bioguide_id=="M000329"] <- 40.86867
final$lon[final$bioguide_id=="M000329"] <- -82.31822												
final$hometown_final[final$bioguide_id=="M000329"] <- "Ashland, Ohio"  

#PRICE, Andrew	
final$lat[final$bioguide_id=="P000521"] <- 29.79604
final$lon[final$bioguide_id=="P000521"] <- -91.5015													
final$hometown_final[final$bioguide_id=="P000521"] <- "Franklin, St. Mary Parish, LA"  

#REILLY, James Bernard	
final$lat[final$bioguide_id=="R000153"] <- 40.63298
final$lon[final$bioguide_id=="R000153"] <- -76.06681													
final$hometown_final[final$bioguide_id=="R000153"] <- "Pinedale, West Brunswick Township, Schuylkill County, PA"  

#HILBORN, Samuel Greeley	
final$lat[final$bioguide_id=="H000583"] <- 44.15632
final$lon[final$bioguide_id=="H000583"] <- -70.34712														
final$hometown_final[final$bioguide_id=="H000583"] <- "Minot, Maine"  

#FLOOD, Henry De La Warr	
final$lat[final$bioguide_id=="F000210"] <- 37.40249
final$lon[final$bioguide_id=="F000210"] <- -78.79295															
final$hometown_final[final$bioguide_id=="F000210"] <- "Appomattox County, VA"  

#ASWELL, James Benjamin	
final$lat[final$bioguide_id=="A000321"] <- 32.38876
final$lon[final$bioguide_id=="A000321"] <- -92.57154																
final$hometown_final[final$bioguide_id=="A000321"] <- "Vernon, Jackson Parish, LA"  

#DICKSON, David
final$lat[final$bioguide_id=="D000328"] <- 32.16562
final$lon[final$bioguide_id=="D000328"] <- -82.90008																	
final$hometown_final[final$bioguide_id=="D000328"] <- "Georgia, United States"  

#SHIELDS, Ebenezer J
final$lat[final$bioguide_id=="S000360"] <- 32.16562
final$lon[final$bioguide_id=="S000360"] <- -82.90008																	
final$hometown_final[final$bioguide_id=="S000360"] <- "Georgia, United States"  

#HACKETT, Thomas C
final$lat[final$bioguide_id=="H000005"] <- 32.16562
final$lon[final$bioguide_id=="H000005"] <- -82.90008																	
final$hometown_final[final$bioguide_id=="H000005"] <- "Georgia, United States"  


write.csv(final, "hometowns_allcongresses_final.csv", row.names = F)

######################################
# Fixing Women with Maiden Names
######################################

## Connect to coordinates
require("ggmap")
require("tidyverse")
require("readxl")

#load in our corrected dataset
tofix <- read_csv("icpsrs_tofix.csv")

#load in voteview so that we can get the bioguide id 
voteview <- read_csv("Hall_members.csv")
voteview <- voteview[voteview$chamber=="House",]
voteview <- voteview[voteview$congress<117,]
voteview <- voteview %>% distinct(icpsr,bioguide_id) 

total_new <- inner_join(tofix,voteview)

#geocode
register_google(key = "")
latlong_new <- geocode(total_new$hometown_final,key="")
latlong_new <- cbind(total_new,latlong_new)

#limit to necessary columns
latlong_new <- latlong_new %>% rename(lat_new=lat,lon_new=lon,hometown_final_new=hometown_final) %>% select(-icpsr)

#combine with original file 
final <- read.csv("hometowns_allcongresses_final.csv")
final <- left_join(final,latlong_new)
final$lat[!is.na(final$lat_new)] <- final$lat_new[!is.na(final$lat_new)]
final$lon[!is.na(final$lon_new)] <- final$lon_new[!is.na(final$lon_new)]
final$hometown_final[!is.na(final$hometown_final_new)] <- final$hometown_final_new[!is.na(final$hometown_final_new)]

final <- final %>% select(-c(lat_new,lon_new,hometown_final_new))

final$hometown_final[final$hometown_final=="field, Graves County, KY"] <- "Mayfield, Graves County, KY"
final$lat[final$hometown_final=="Mayfield, Graves County, KY"] <- 36.74172
final$lon[final$hometown_final=="Mayfield, Graves County, KY"] <- -88.63672

final$hometown_final[final$hometown_final=="field, Fulton County, NY"] <- "Mayfield, Fulton County, NY"
final$lat[final$hometown_final=="Mayfield, Fulton County, NY"] <- 43.10452
final$lon[final$hometown_final=="Mayfield, Fulton County, NY"] <- -74.26486


final$hometown_final[final$hometown_final=="Pas Negras, Mexico"] <- "Piedras Negras, Mexico"
final$lat[final$hometown_final=="Piedras Negras, Mexico"] <- 28.69162
final$lon[final$hometown_final=="Piedras Negras, Mexico"] <--100.5409

final$hometown_final[final$hometown_final=="sboro, Bulloch County, GA"] <- "Statesboro, Bulloch County, GA"
final$lat[final$hometown_final=="Statesboro, Bulloch County, GA"] <- 32.44879
final$lon[final$hometown_final=="Statesboro, Bulloch County, GA"] <--81.78317

final$hometown_final[final$hometown_final=="Cape , NJ"] <- "Cape May, NJ"
final$lat[final$hometown_final=="Cape May, NJ"] <- 38.93511
final$lon[final$hometown_final=="Cape May, NJ"] <--74.90601

final$hometown_final[final$hometown_final=="sville, Mason County, KY"] <- "Maysville, Mason County, KY"
final$lat[final$hometown_final=="Maysville, Mason County, KY"] <- 38.64119
final$lon[final$hometown_final=="Maysville, Mason County, KY"] <--83.74437

final$hometown_final[final$hometown_final=="City , VA"] <- "Richmond, VA"
final$lat[final$hometown_final=="Richmond, VA"] <- 37.54072
final$lon[final$hometown_final=="Richmond, VA"] <- -77.43605

final$hometown_final[final$hometown_final=="Head , Cecil County, MD"] <- "Elkton, Cecil County, MD"
final$lat[final$hometown_final=="Elkton, Cecil County, MD"] <- 39.60678
final$lon[final$hometown_final=="Elkton, Cecil County, MD"] <- -75.83327

final$hometown_final[final$hometown_final=="Island , Ireland"] <- "Island of Rathlin, Ireland"
final$lat[final$hometown_final=="Island of Rathlin, Ireland"] <- 55.29795
final$lon[final$hometown_final=="Island of Rathlin, Ireland"] <- -6.207279

final$hometown_final[final$hometown_final=="Pont, Greenville County, SC"] <- "Piedmont, Greenville County, SC"
final$lat[final$hometown_final=="Piedmont, Greenville County, SC"] <- 34.7023389
final$lon[final$hometown_final=="Piedmont, Greenville County, SC"] <- -82.4645714

final$hometown_final[final$hometown_final=="Pont, Calhoun County, AL"] <- "Piedmont, Calhoun County, AL"
final$lat[final$hometown_final=="Piedmont, Calhoun County, AL"] <- 33.9245454
final$lon[final$hometown_final=="Piedmont, Calhoun County, AL"] <- -85.6113501


write.csv(final, "hometowns_allcongresses_updated.csv", row.names = F)





