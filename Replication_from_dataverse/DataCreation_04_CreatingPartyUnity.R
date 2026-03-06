###################################################################################################
# Far From Home                      ###############################################
# Jesse Crosson and Jaclyn Kaslovsky ###############################################
###################################################################################################

#This file is our party unity function to create scores for the missing congresses in the legacy file

library(tidyr)
library(tidyverse)
library(reshape2)
library(miceadds)

#before running this file, place the member and votes files into a subfolder called Party Unity

setwd("~/Dropbox/Local Roots/Replication Package New/Intermediate Data/Party Unity")
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))

for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}


#merge together
member_votes_114 <- inner_join(H114_members, H114_votes, by=c("congress", "chamber", "icpsr"))
member_votes_115 <- inner_join(H115_members, H115_votes, by=c("congress", "chamber", "icpsr"))
member_votes_116 <- inner_join(H116_members, H116_votes, by=c("congress", "chamber", "icpsr"))

# data has to have a column called "party_code" and only have members of the two parties in the data

party_unity_score <- function(rc, threshold=0.5){
  
  #remove members who were not a member of the chamber when the vote was taken
  rc<-  rc[rc$cast_code!=0,]
  
  #mark everything but "yea" and "nay" as missing
  rc$cast_code[rc$cast_code!=1 & rc$cast_code!=6] <-  NA
  
  #mark nay as 0 instead of 6
  rc$cast_code[rc$cast_code!=1] <-  0
  
  #go from long to wide 
  rc <- dcast(rc, congress + chamber + icpsr + party_code + state_abbrev + district_code ~ rollnumber, value.var="cast_code")
  
  library(dplyr)
  
  #get column names for all the votes
  votenames <- colnames(rc[ , grepl( "[0-9]" , names( rc ) ) ])
  
  party1 <- sort(unique(rc$party_code))[1]
  party2 <- sort(unique(rc$party_code))[2]
  
  #get the members of each party 
  party_one_rows <- which(rc$party_code==party1)
  party_two_rows <- which(rc$party_code==party2)
  
  #subset to just party1 and take the average support for each vote
  party_one <- rc[party_one_rows,]
  party_one_votes <- party_one[ , votenames ]
  
  #subset to just party2 and take the average support for each vote
  party_two <- rc[party_two_rows,]
  party_two_votes <- party_two[ , votenames ]
  
  #calculate party support
  party_one_support <- colMeans(party_one_votes,na.rm=T)
  party_two_support <- colMeans(party_two_votes,na.rm=T)
  
  #this is when dems vote in favor
  party_vote1 <- party_one_support>=(threshold+.00000001) & party_two_support<=((1-threshold)-.00000001)
  
  #this is when reps vote in favor
  party_vote2 <- party_one_support<=((1-threshold)-.00000001) & party_two_support>=(threshold+.00000001)
  
  #get party unity scores for dems; do so by turning negatives into positives for the vote in which 
  #their party voted majority against 
  
  party_one_unity_neg <- as.data.frame(party_one_votes[,party_vote2])
  
  if(nrow(party_one_unity_neg)>0){
    party_one_unity_neg <- as.data.frame(apply(party_one_unity_neg, 2, function(x) ifelse(x<1, 2, x)))
    party_one_unity_neg <- as.data.frame(apply(party_one_unity_neg, 2, function(x) ifelse(x<2, 0, x)))
    party_one_unity_neg <- as.data.frame(apply(party_one_unity_neg, 2, function(x) ifelse(x>1, 1, x)))
  }
  party_one_unity_pos <- as.data.frame(party_one_votes[,party_vote1])
  
  if(nrow(party_one_unity_pos)>0 & nrow(party_one_unity_neg)>0){
    
    party_one_unity <- cbind(party_one_unity_pos,party_one_unity_neg)
    
  } else if(nrow(party_one_unity_pos)==0){
    
    party_one_unity <- party_one_unity_neg
    
  } else{party_one_unity <- party_one_unity_pos}
  
  party_one_unity <-data.frame(lapply(party_one_unity,as.numeric))
  party_one_unityscore <- rowMeans(party_one_unity,na.rm=T)
  party_one_unityvotes <- rowSums(!is.na(party_one_unity))
  if(length(party_one_unityscore)>0){
    party_one$partyunity <- party_one_unityscore
  } else{party_one$partyunity <- NA}
  if(length(party_one_unityvotes)>0){
    party_one$partyunityvotes <- party_one_unityvotes
  } else{party_one$partyunityvotes <- NA}
  #get party unity scores for reps; do so by turning negatives into positives for the vote in which 
  #their party voted majority against 
  party_two_unity_neg <- as.data.frame(party_two_votes[,party_vote1])
  if(nrow(party_two_unity_neg)>0){
    party_two_unity_neg <- as.data.frame(apply(party_two_unity_neg, 2, function(x) ifelse(x<1, 2, x)))
    party_two_unity_neg <- as.data.frame(apply(party_two_unity_neg, 2, function(x) ifelse(x<2, 0, x)))
    party_two_unity_neg <- as.data.frame(apply(party_two_unity_neg, 2, function(x) ifelse(x>1, 1, x)))
  }
  party_two_unity_pos <- as.data.frame(party_two_votes[,party_vote2])
  
  if(nrow(party_two_unity_pos)>0 & nrow(party_two_unity_neg)>0){
    
    party_two_unity <- cbind(party_two_unity_pos,party_two_unity_neg)
    
  } else if(nrow(party_two_unity_pos)==0){
    
    party_two_unity <- party_two_unity_neg
    
  } else{party_two_unity <- party_two_unity_pos}
  
  
  party_two_unity <-data.frame(lapply(party_two_unity,as.numeric))
  party_two_unityscore <- rowMeans(party_two_unity,na.rm=T)
  party_two_unityvotes <- rowSums(!is.na(party_two_unity))
  if(length(party_two_unityscore)>0){
    party_two$partyunity <- party_two_unityscore
  } else{party_two$partyunity <- NA}
  if(length(party_two_unityvotes)>0){
    party_two$partyunityvotes <- party_two_unityvotes
  } else{party_two$partyunityvotes <- NA}
  total <- as.data.frame(rbind(party_one, party_two))
  total <- total[, c("congress", "chamber", "icpsr", "partyunity", "partyunityvotes", "district_code", "state_abbrev")]

  return(total)
}

total_114 <- party_unity_score(member_votes_114, threshold = 0.5)
total_115 <- party_unity_score(member_votes_115, threshold = 0.5)
total_116 <- party_unity_score(member_votes_116, threshold = 0.5)
total <- bind_rows(total_114, total_115,total_116)
names(total)[names(total) == "icpsr"] <- "ICPSR"
total$partyunity <- round2(total$partyunity*100,3)
names(total)[names(total) == "partyunity"] <- "party_unity"
names(total)[names(total) == "district_code"] <- "district_num"
names(total)[names(total) == "state_abbrev"] <- "state"
write_csv(total, "~/Dropbox/Local Roots/Replication Package New/Intermediate Data/newpartyunityscores.csv")
