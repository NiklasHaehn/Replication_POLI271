library(estimatr)
library(haven)
library(tidyverse)
library(purrr)
library(fixest)
library(nnet)

df_final <- read_dta('data/fmt/Dataset_Final.dta')
df_wdd <- read_dta('data/fmt/Dataset_WithDeathDistance.dta')
df_wdp <- read_dta('data/fmt/Dataset_WithDeathplaces.dta')

# ------ Explore Data ------

# Of the legislators born in their district, how many also died there vs. how
# many died elsewhere? Bar graph
plot_local <- df_final %>%
  filter(binary == 1, !is.na(death_binary)) %>% 
  ggplot(aes(x = factor(death_binary))) + # factor() makes it treat 0 and 1 as categories
  geom_bar(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Bar Plot of Death Places for Legislators Born in Their District",
       x = "Outside District vs. Inside District", y = "Count")

print(plot_local)

# Of the legislators not born in their district, how many died in their district
# vs. died elsewhere? Bar graph
plot_nonlocal <- df_final %>%
  filter(binary == 0, !is.na(death_binary)) %>% 
  ggplot(aes(x = factor(death_binary))) + # factor() makes it treat 0 and 1 as categories
  geom_bar(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Bar Plot of Death Places for Legislators Not Born in Their District",
       x = "Outside District vs. Inside District", y = "Count")

print(plot_nonlocal)

# Plotting birthplace distance vs death place distance 
plot_distance <- 
  ggplot(df_final, aes(x = distance_miles, y = death_dist_miles)) + 
  geom_point() + 
  labs(title = "Distance of Birth Place to District vs. Distance of Death Place to District",
       x = "Distance between legislator's birth place and their district",
       y = "Distance between legislator's death place and their district")

print(plot_distance)
# Many legislators were born and died in (or close) to their district. But there are 
# are clusters of legislators who were born in their district but died farther away,
# and clusters of legislators who were born farther away but died in their 
# district. 

# I tried a log_distance plot but it was not as useful

df_close <- subset(df_final, distance_miles<=100)
df_far <- subset(df_final, distance_miles > 100)

#Count of Legislators by Distance from District of Death (similar to the histograms
# in the paper)
hist(df_close$death_dist_miles, main="Born <= 100 miles from District",
     xlab="Miles from District of Death Place", ylab="Count") 
hist(df_far$death_dist_miles, main="Born >100 miles from District",
     xlab="Miles from District of Death Place", ylab="Count") 

# ------ Setup for Logit Replications with Death Data ------

# remove rows without style values (the DV in these two models)
df_final_clean <- df_final %>% drop_na(style3, dem, seniority, majority, power, chair, female, inpres, congress)
df_final_clean$style3 <- as.factor(df_final_clean$style3)
df_final_clean$congress <- as.factor(df_final_clean$congress)

# create style factor variable with numbers and labels 
style_levels <- c(1, 2, 3)
style_labels <- c("Party Focused", "District Focused", "Policy Focused")

df_final_clean$style_labeled <- factor(df_final_clean$style3,
                                 levels = style_levels,
                                 labels = style_labels)

#head(df_final_clean[, c("style3", "style_labeled")], n=20)

# ------ Replications with Death Data ------

# ------ Local Roots and Legislative Style (Logit) ------
# Original models: 
orig_style_bin <- multinom(style_labeled ~ binary + dem + seniority + majority + power + chair + female + inpres + congress, 
                            data=df_final_clean)
coefs_style_bin <- coefficients(orig_style_bin)
round(coefs_style_bin, 3) # matches Table 1 in paper

orig_style_log <- multinom(style_labeled ~ logged_distance + dem + seniority + majority + power + chair + female + inpres + congress, 
                           data=df_final_clean)
coefs_style_log <- coefficients(orig_style_log)
round(coefs_style_log, 3) # matches Table 1

# Models with death place variables
dp_style_bin <- multinom(style_labeled ~ binary + death_binary + dem + seniority + majority + power + chair + female + inpres + congress, 
                         data=df_final_clean)
coefs_dp_style_bin <- coefficients(dp_style_bin) 
round(coefs_dp_style_bin, 3) 

dp_style_log <- multinom(style_labeled ~ logged_distance + death_logged_dist + dem + seniority + majority + power + chair + female + inpres + congress, 
                         data=df_final_clean)
coefs_dp_style_log <- coefficients(dp_style_log) 
round(coefs_dp_style_log, 3) 

#df_final_cleaner <- df_final %>% drop_na(death_binary)

# ------ Local Roots and Constituency Staff Allocation ------
# Original Models
orig_const_bin <- feols(pct_constituencystaff ~ binary + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                          data = df_final,
                          cluster = ~districtID)
summary(orig_const_bin) # matches Table 2

orig_const_log <- feols(pct_constituencystaff ~ logged_distance + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                       data = df_final,
                       cluster = ~districtID)
summary(orig_const_log) # matches Table 2

# Models with Death Place Variables
dp_const_bin <- feols(pct_constituencystaff ~ binary + death_binary + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                         data = df_final,
                         cluster = ~districtID)
summary(dp_const_bin)

dp_const_log <- feols(pct_constituencystaff ~ logged_distance + death_logged_dist + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                         data = df_final,
                         cluster = ~districtID)
summary(dp_const_log)
# very few observations because most of the constituency staff data is more recent, 
# so these legislators are mostly still alive

# ------ Local Roots and Party Unity ------
# Original Models
orig_party_log <- feols(party_unity ~ binary + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                   data=df_final,
                   cluster = ~districtID)
summary(orig_party_log)

orig_party_bin <- feols(party_unity ~ logged_distance + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                        data=df_final,
                        cluster = ~districtID)
summary(orig_party_bin)

# Models with Death Place Variables
dp_party_bin <- feols(party_unity ~ binary + death_binary + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                   data=df_final,
                   cluster = ~districtID)
summary(dp_party_bin)
# opposite signs on binary and death_binary coefficients

dp_party_log <- feols(party_unity ~ logged_distance + death_logged_dist + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                   data=df_final,
                   cluster = ~districtID)
summary(dp_party_log)
# opposite signs on logged_distance and death_logged_dist coefficients

# ------ Local Roots and Cosponsors ------
# Original Models
orig_cosp_bin <- feols(inpart_cospon ~ binary + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                      data=df_final,
                      cluster = ~districtID)
summary(orig_cosp_bin) 

orig_cosp_log <- feols(inpart_cospon ~ logged_distance + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                      data=df_final,
                      cluster = ~districtID)
summary(orig_cosp_log)

# Models with Death Place Variables
dp_cosp_bin <- feols(inpart_cospon ~ binary + death_binary + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                      data=df_final,
                      cluster = ~districtID)
summary(dp_cosp_bin) 

dp_cosp_log <- feols(inpart_cospon ~ logged_distance + death_logged_dist + dem + seniority + majority + power + chair + female + inpres | districtID + congress, 
                      data=df_final,
                      cluster = ~districtID)
summary(dp_cosp_log)

