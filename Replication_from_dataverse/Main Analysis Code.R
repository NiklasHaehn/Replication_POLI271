###################################################################################################
# Far From Home                         ###############################################
# Jesse Crosson and Jaclyn Kaslovsky    ###############################################
# Descriptive Graphs and Analysis       ###############################################
###################################################################################################

###################################################################################################
# Working Directory and Packages ##################################################################
###################################################################################################

setwd("~/Dropbox/Local Roots/Replication Package New/Data/")

require(dplyr)
require(stargazer)
require(starpolishr)
require(ggplot2)
require(ggpubr)
require(forcats)
require(scales)
require(cowplot)
require(lfe)
require(readstata13)

################################
# Load the Data  #########
################################

#Load in the main data for analyses
df <- read.dta13("Dataset_ForMainAnalysis.dta")

#load in the complete data for graphics - this goes back to 1st congress
df_overtime <- read.dta13("Dataset_OvertimeComparisons.dta")

#summarize our binary variable
summary(df_overtime$binary)

#get unique number of legislators 
unique_leg <- unique(df_overtime$icpsr[!is.na(df_overtime$distance_miles)])

################################################
# Histogram of distance born from the district #
################################################


fig1_justborn <- df_overtime[df_overtime$distance_miles <= 100,] %>%
  ggplot(aes(distance_miles)) +
  geom_histogram(fill="grey", col="black") + xlab("Miles from District") + ylab("Count") +
  theme_bw()+ ggtitle("Born Less than or Equal to 100 Miles from the District")+
  theme(plot.title = element_text(hjust = 0.5))

fig1_justborn2 <- df_overtime[df_overtime$distance_miles > 100,] %>%
  ggplot(aes(distance_miles)) +
  geom_histogram(fill="grey", col="black", boundary = 100) + xlab("Miles from District") + ylab("Count") +
  theme_bw()+
  ggtitle("Born More than 100 Miles from the District")  +
  theme(plot.title = element_text(hjust = 0.5))

combined <-  ggarrange(fig1_justborn, fig1_justborn2 + rremove("ylab"), ncol = 2, nrow = 1)



ggsave("~/Dropbox/Local Roots/Replication Package New/Paper/Figure1.pdf",combined,width = 10, height = 5, dpi=600) 


################################################
## Basic summary stats for main text
################################################

# How many observations fall into left panel of figure 1
cat(round((nrow(df_overtime[df_overtime$distance_miles <= 100,])/nrow(df_overtime))*100,0), sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Paper/left_panel.tex")


# All observations: over 51 percent of all observations and 48 percent of unique
# legislators are either zero (legislator was born in the district) or
# under 10 miles from the nearest district border.

inneardist <- nrow(subset(df_overtime, logged_distance < 2.302585))
cat(round((inneardist/nrow(df_overtime))*100,0), sep = '\n', file = "../Paper/withinten.tex")


# Unique legislator-districts

uniq_leg_dat <- df_overtime %>% group_by(icpsr, st_name, cd) %>% summarize(logged_distance = mean(logged_distance, na.rm = T))
inneardist <- nrow(subset(uniq_leg_dat, logged_distance < 2.302585))
cat(round((inneardist/nrow(uniq_leg_dat))*100,0), sep = '\n', file = "../Paper/withinten_unique.tex")

# Did you ever represent your home?

local <- NA
for(i in unique(df_overtime$icpsr)){
  vec <- subset(df_overtime, icpsr == i)$logged_distance
  val <- ifelse(any(ifelse(vec < 2.302585, 1, 0)),1,0)
  local <- c(local, val)
}

cat(round((sum(local, na.rm = T)/length(unique_leg))*100,0), sep = '\n', file = "../Paper/ever_represent.tex")


# Did you ever represent your home state?

localst <- NA
for(i in unique(df_overtime$icpsr)){
  vec <- subset(df_overtime, icpsr == i)
  vec$instate <- ifelse(vec$state_born == vec$state_represent, 1, 0)
  val <- ifelse(sum(vec$instate, na.rm = T) > 0, 1, 0)
  localst <- c(localst, val)
}

cat(round((sum(localst, na.rm = T)/length(unique_leg))*100,0), sep = '\n', file = "../Paper/ever_represent_st.tex")


# Looking at the 114th congress to compare to British MPs
df_overtime$state_match <- 0 
df_overtime$state_match[df_overtime$state_born==df_overtime$state_represent] <- 1
cat(round((mean(df_overtime$state_match[df_overtime$congress==114]))*100,0), sep = '\n', file = "../Paper/114_cong.tex")


#comparing house localism in 1970 to the US census
seventies <- subset(df_overtime, congress == 91)
inneardist_seventies <- nrow(subset(seventies, logged_distance < 2.302585))
cat(round((inneardist_seventies/nrow(seventies))*100,0), sep = '\n', file = "../Paper/1970s_local.tex")
 # over 61% of congress in 1970 are 0 or <10 miles
cat(round((mean(df_overtime$state_match[df_overtime$congress==91]))*100,0), sep = '\n', file = "../Paper/1970s_local_st.tex")


# Serve in state of birth as of 2019
cat(round((mean(df_overtime$state_match[df_overtime$congress==116]))*100,0), sep = '\n', file = "../Paper/2019_local.tex")

# Comparing the most recent congress to 2000 for in the conclusion

modern <- subset(df_overtime, congress == 116)
inneardist2 <- nrow(subset(modern, logged_distance < 2.302585))
cat(round((inneardist2/nrow(modern))*100,0), sep = '\n', file = "../Paper/116th_local.tex") # over 42% of 116th congress are 0 or <10 miles


older <- subset(df_overtime, congress == 106)
inneardist3 <- nrow(subset(older, logged_distance < 2.302585))
cat(round((inneardist3/nrow(older))*100,0), sep = '\n', file = "../Paper/106th_local.tex") # over 50% of 106th congress are 0 or <10 miles



######################################################################################
# Graph of distance born from the district over time with transportation innovations #
######################################################################################

#some useful functions for all of the following graphs
just_nums <- function(n){
  
  suff <- case_when(n %in% c(11,12,13) ~ "th",
                    n %% 10 == 1 ~ 'st',
                    n %% 10 == 2 ~ 'nd',
                    n %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(n, suff)
}


every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

#Collapse the complete data so we can show over time trends for distance born from district
collapsed_cong <- df_overtime %>%
  group_by(congress) %>%
  dplyr::summarize(avg_born = mean(distance_miles, na.rm=TRUE),
                   median_born = median(distance_miles, na.rm=TRUE))

collapsed_cong$year <- 2*collapsed_cong$congress + 1787
collapsed_cong$cong2 <- just_nums(collapsed_cong$congress)
collapsed_cong$term <- paste(collapsed_cong$cong2, " Congress, ", collapsed_cong$year)
collapsed_cong <- collapsed_cong %>%
  mutate(term = fct_reorder(term, congress))

collapsed_cong$transportation <- ""
collapsed_cong$transportation[collapsed_cong$term=="20th  Congress,  1827"] <- "Railroads"
collapsed_cong$transportation[collapsed_cong$term=="53rd  Congress,  1893"] <- "Gasoline Powered Car"
collapsed_cong$transportation[collapsed_cong$term=="58th  Congress,  1903"] <- "Airplane"
collapsed_cong$transportation[collapsed_cong$term=="60th  Congress,  1907"] <- "Model T Car"
collapsed_cong$transportation[collapsed_cong$term=="85th  Congress,  1957"] <- "Federal Highway Act"

fig2_avg <- ggplot(collapsed_cong, aes(x=as.factor(term), y=avg_born, group=1)) +
  geom_line() +  
  xlab("")  + theme_classic() + theme(legend.position="bottom",legend.title = element_blank()) + 
  scale_x_discrete(breaks = every_nth(19), labels = wrap_format(10))+ ylim(0,700)+
  scale_color_manual(values = colors)+ geom_vline(xintercept=21,linetype=3) +
  annotate("text", x=22, label="Railroads", y=600,angle=90)  + geom_vline(xintercept=52,linetype=3) +
  annotate("text", x=53, label="Gasoline Car", y=600,angle=90) + geom_vline(xintercept=58,linetype=3) +
  annotate("text", x=59, label="Airplane", y=600,angle=90) + geom_vline(xintercept=60,linetype=3) +
  annotate("text", x=61, label="Model T Car", y=600,angle=90) + geom_vline(xintercept=84,linetype=3) +
  annotate("text", x=85, label="Federal Highway Act", y=600,angle=90) + ylab("Average Distance Born from the District (Miles)")+ theme(axis.text.x = element_blank(),
                                                                                                                                       axis.ticks.x = element_blank(),
                                                                                                                                       axis.title.x = element_blank())

fig2_median <- ggplot(collapsed_cong, aes(x=as.factor(term), y=median_born, group=1)) +
  geom_line() +  
  xlab("")  + theme_classic() + theme(legend.position="bottom",legend.title = element_blank()) + 
  scale_x_discrete(breaks = every_nth(19), labels = wrap_format(10))+
  scale_color_manual(values = colors)+ geom_vline(xintercept=21,linetype=3) +
  annotate("text", x=22, label="", y=60,angle=90)  + geom_vline(xintercept=52,linetype=3) +
  annotate("text", x=53, label="", y=60,angle=90) + geom_vline(xintercept=58,linetype=3) +
  annotate("text", x=59, label="", y=60,angle=90) + geom_vline(xintercept=60,linetype=3) +
  annotate("text", x=61, label="", y=60,angle=90) + geom_vline(xintercept=84,linetype=3) +
  annotate("text", x=85, label="", y=60,angle=90) + ylab("Median Distance Born from the District (Miles)")


aligned <- plot_grid(fig2_avg, fig2_median,ncol=1, align = "v")

ggsave("~/Dropbox/Local Roots/Replication Package New/Paper/Figure2.pdf",aligned,width = 10, height = 8, dpi=600) 


#######################################################
################### Main Regressions ##################
#######################################################

mean(df$pct_constituencystaff, na.rm=TRUE)

# Local Staff Regression

df$distance <- df$binary
reg1 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg1)


df$distance <- df$logged_distance
reg2 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg2)


reg2_table_pct <- stargazer(reg1, reg2,
                            add.lines=list(c("District Fixed Effects", "\\checkmark", "\\checkmark"),
                                           c("Congress Fixed Effects",  "\\checkmark","\\checkmark")),
                            omit=c("Constant"),
                            notes.append = FALSE,notes.label = "",
                            report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                            omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                            column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                            dep.var.labels = "Percent of Staff Allocated Towards Constituency Service",column.sep.width="0pt",
                            covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                               "Member is a Woman", "Same Party Presidential Vote Share"),
                            notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                            label="tab3_pct2",
                            digits=3,
                            digits.extra = 0,
                            title="Local Roots and Percent of Staff that Focus on Constituency Service, 1993 to 2014")
cat(reg2_table_pct, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Paper/Table2.tex")



# Party Unity Regression 

df$distance <- df$binary
reg3 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg3)

df$distance <- df$logged_distance
reg4 <- felm(party_unity  ~ distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df)
summary(reg4)


reg1_table <- stargazer(reg3,reg4,
                        add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark"),
                                       c("Congress Fixed Effects","\\checkmark","\\checkmark")),
                        omit=c("Constant"),
                        notes.append = FALSE,notes.label = "",
                        report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                        omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                        column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                        dep.var.labels = "Party Unity Score",column.sep.width="0pt",
                        covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                           "Member is a Woman", "Same Party Presidential Vote Share"),
                        notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                        label="tab1",
                        digits=3,
                        digits.extra = 0,
                        title="Member Birth Place and Party Unity, 1973 to 2020")

cat(reg1_table, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Paper/Table3.tex")


# Bipartisanship in Cosponsoring Regression 

df$distance <- df$binary
reg5 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg5)

df$distance <- df$logged_distance
reg6 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df)
summary(reg6)

reg1_table <- stargazer(reg5,reg6,
                        add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark"),
                                       c("Congress Fixed Effects","\\checkmark","\\checkmark")),
                        omit=c("Constant"),
                        notes.append = FALSE,notes.label = "",
                        report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                        omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                        column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                        dep.var.labels = "Percent of Cosponsors that are Copartisans",column.sep.width="0pt",
                        covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                           "Member is a Woman", "Same Party Presidential Vote Share"),
                        notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                        label="tab2",
                        digits=3,
                        digits.extra = 0,
                        title="Member Birth Place and Cosponsorship Attraction, 1973 to 2020")

cat(reg1_table, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Paper/Table4.tex")


#######################################################
###### CONDITIONAL DISTRICT-FEATURES ANALYSIS #########
#######################################################

#### Percent BA

df$distance <- df$binary

reg7a1 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data=subset(df, prcntBA > median(df$prcntBA, na.rm = T)))
summary(reg7a1)

reg7b1 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data= subset(df, prcntBA > median(df$prcntBA, na.rm = T)))
summary(reg7b1)

reg7c1 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres  | congress + districtID | 0 | districtID, data=subset(df, prcntBA > median(df$prcntBA, na.rm = T)))
summary(reg7c1)

reg7d2 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data=subset(df, prcntBA < median(df$prcntBA, na.rm = T)))
summary(reg7d2)

reg7e2 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data= subset(df, prcntBA < median(df$prcntBA, na.rm = T)))
summary(reg7e2)

reg7f2 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres  | congress + districtID | 0 | districtID, data=subset(df, prcntBA < median(df$prcntBA, na.rm = T)))
summary(reg7f2)

##

df$distance <- df$logged_distance

reg8a1 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data=subset(df, prcntBA > median(df$prcntBA, na.rm = T)))
summary(reg8a1)

reg8b1 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data= subset(df, prcntBA > median(df$prcntBA, na.rm = T)))
summary(reg8b1)

reg8c1 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres  | congress + districtID | 0 | districtID, data=subset(df, prcntBA > median(df$prcntBA, na.rm = T)))
summary(reg8c1)

reg8d2 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data=subset(df, prcntBA < median(df$prcntBA, na.rm = T)))
summary(reg8d2)

reg8e2 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data= subset(df, prcntBA < median(df$prcntBA, na.rm = T)))
summary(reg8e2)

reg8f2 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres  | congress + districtID | 0 | districtID, data=subset(df, prcntBA < median(df$prcntBA, na.rm = T)))
summary(reg8f2)

#### Lived in a different state a year ago - limiting to 2007 and beyond since before this time it was lived in a different state FIVE years ago

df$distance <- df$binary

reg9a1 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data=subset(df[df$year>=2007,], state_mobility > median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg9a1)

reg9b1 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data= subset(df[df$year>=2007,], state_mobility > median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg9b1)

reg9c1 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres  | congress + districtID | 0 | districtID, data=subset(df[df$year>=2007,], state_mobility > median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg9c1)

reg9d2 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data=subset(df[df$year>=2007,], state_mobility < median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg9d2)

reg9e2 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data= subset(df[df$year>=2007,], state_mobility < median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg9e2)

reg9f2 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres  | congress + districtID | 0 | districtID, data=subset(df[df$year>=2007,], state_mobility < median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg9f2)

##

df$distance <- df$logged_distance

reg10a1 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data=subset(df[df$year>=2007,], state_mobility > median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg10a1)

reg10b1 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data= subset(df[df$year>=2007,], state_mobility > median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg10b1)

reg10c1 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres  | congress + districtID | 0 | districtID, data=subset(df[df$year>=2007,], state_mobility > median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg10c1)

reg10d2 <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data=subset(df[df$year>=2007,], state_mobility < median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg10d2)

reg10e2 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres | congress + districtID | 0 | districtID, data= subset(df[df$year>=2007,], state_mobility < median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg10e2)

reg10f2 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + 
                 inpres  | congress + districtID | 0 | districtID, data=subset(df[df$year>=2007,], state_mobility < median(df$state_mobility[df$year>=2007], na.rm = T)))
summary(reg10f2)


reg1_table_belowmeds <- stargazer(reg7d2, reg7e2, reg7f2, reg8d2, reg8e2, reg8f2,
                                     omit=c("Constant"),
                                     notes.append = FALSE,notes.label = "",
                                     report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                     omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                     column.labels=c("Staff", "Cospon.", "Party Unity", "Staff", "Cospon.", "Party Unity"),
                                  dep.var.labels = c("","","","","",""), dep.var.caption=c("Born in District (0/1)  \\hspace{1.5cm} Log(Miles Born from District + 1)"),
                                  covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                        "Member is a Woman", "Same Party Presidential Vote Share"),
                                     notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                     label="tab_bonus1",
                                     digits=3,
                                     digits.extra = 0,
                                     title="Member Birth Place and Differential Findings by \\% Bachelor's Degree")

reg1_table_abovemeds <- stargazer(reg7a1, reg7b1, reg7c1, reg8a1, reg8b1, reg8c1,
                                  add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                                                 c("Congress Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                                  omit=c("Constant"),
                                  notes.append = FALSE,notes.label = "",
                                  report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                  omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                  column.labels=c("Staff", "Cospon.", "Party Unity", "Staff", "Cospon.", "Party Unity"),
                                  dep.var.labels = c("","","","","",""), dep.var.caption=c("Born in District (0/1)  \\hspace{1.5cm} Log(Miles Born from District + 1)"),
                                  covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                     "Member is a Woman", "Same Party Presidential Vote Share"),
                                  notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                  label="tab_bonus1",
                                  digits=3,
                                  digits.extra = 0)

star.panel.out_bonus <- star_panel(reg1_table_belowmeds,reg1_table_abovemeds ,panel.label.fontface="bold",
                                   panel.names = c("Below Median","Above Median"), same.summary.stats = FALSE)
cat(star.panel.out_bonus, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Paper/Table5.tex")

###


reg1_table_belowmeds <- stargazer(reg9d2, reg9e2, reg9f2, reg10d2, reg10e2, reg10f2,
                                  omit=c("Constant"),
                                  notes.append = FALSE,notes.label = "",
                                  report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                  omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                  column.labels=c("Staff", "Cospon.", "Party Unity", "Staff", "Cospon.", "Party Unity"),
                                  dep.var.labels = c("","","","","",""), dep.var.caption=c("Born in District (0/1)  \\hspace{1.5cm} Log(Miles Born from District + 1)"),
                                  column.sep.width="0pt",
                                  covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                     "Member is a Woman", "Same Party Presidential Vote Share"),
                                  notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$. Note that the data begins in 2007.}",
                                  label="tab_bonus2",
                                  digits=3,
                                  digits.extra = 0,
                                  title="Member Birth Place and Differential Findings by Interstate Constituent Mobility")

reg1_table_abovemeds <- stargazer(reg9a1, reg9b1, reg9c1, reg10a1, reg10b1, reg10c1,
                                  add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                                                 c("Congress Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                                  omit=c("Constant"),
                                  notes.append = FALSE,notes.label = "",
                                  report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                  omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                  column.labels=c("Staff", "Cospon.", "Party Unity", "Staff", "Cospon.", "Party Unity"),
                                  dep.var.labels = c("","","","","",""), dep.var.caption=c("Born in District (0/1)  \\hspace{1.5cm} Log(Miles Born from District + 1)"),
                                  covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                     "Member is a Woman", "Same Party Presidential Vote Share"),
                                  notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$. Note that the data begins in 2007.}",
                                  label="tab_bonus2",
                                  digits=3,
                                  digits.extra = 0)

star.panel.out_bonus <- star_panel(reg1_table_belowmeds,reg1_table_abovemeds ,panel.label.fontface="bold",
                                   panel.names = c("Below Median","Above Median"), same.summary.stats = FALSE)
cat(star.panel.out_bonus, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Paper/Table6.tex")



###########################################
######## Interpreting Effect Sizes ########
###########################################
set.seed(02138)

######### Staffers #########

#get the fixed effects from the regression to include
fes <- getfe(reg2)
fes_district <- data.frame(fes$effect[11:length(fes$effect)])
fes_congress <- data.frame(fes$effect[1:10])

#get coefficient values
coefs <- reg2$coefficients

#calculate the value when everything is held at the median
p75 <- coefs[1]*quantile(df$logged_distance[!is.na(df$pct_constituencystaff)], probs = .75, na.rm = T) + coefs[2]*median(df$dem[!is.na(df$pct_constituencystaff)]) +
   coefs[3]*median(df$seniority[!is.na(df$pct_constituencystaff)], na.rm = T)  + coefs[4]*median(df$majority[!is.na(df$pct_constituencystaff)], na.rm = T) + coefs[5]*median(df$power[!is.na(df$pct_constituencystaff)], na.rm = T) + 
   coefs[6]*median(df$chair[!is.na(df$pct_constituencystaff)], na.rm = T) + coefs[7]*median(df$female[!is.na(df$pct_constituencystaff)], na.rm = T) + coefs[8]*median(df$inpres[!is.na(df$pct_constituencystaff)], na.rm = T) +
   median(fes_district$fes.effect.11.length.fes.effect..)  + median(fes_congress$fes.effect.1.10.)
 
p25 <- coefs[1]*quantile(df$logged_distance[!is.na(df$pct_constituencystaff)], probs = .25, na.rm = T) + coefs[2]*median(df$dem[!is.na(df$pct_constituencystaff)]) +
   coefs[3]*median(df$seniority[!is.na(df$pct_constituencystaff)], na.rm = T)  + coefs[4]*median(df$majority[!is.na(df$pct_constituencystaff)], na.rm = T) + coefs[5]*median(df$power[!is.na(df$pct_constituencystaff)], na.rm = T) + 
   coefs[6]*median(df$chair[!is.na(df$pct_constituencystaff)], na.rm = T) + coefs[7]*median(df$female[!is.na(df$pct_constituencystaff)], na.rm = T) + coefs[8]*median(df$inpres[!is.na(df$pct_constituencystaff)], na.rm = T) +
   median(fes_district$fes.effect.11.length.fes.effect..)  + median(fes_congress$fes.effect.1.10.)
 
(p75 -  p25)/(p25) * 100
p75-p25
# equals -4.46  percent change and -1.77 percent fewer staffers
 
 
 
# ######### Party Unity #########
 
#get the fixed effects from the regression to include
fes <- getfe(reg4)
fes_district <- data.frame(fes$effect[25:length(fes$effect)])
fes_congress <- data.frame(fes$effect[1:24])

#get coefficient values
coefs <- reg4$coefficients
 
#calculate the value when everything is held at the median
p75 <- coefs[1]*quantile(df$logged_distance, probs = .75, na.rm = T) + coefs[2]*median(df$dem) +
   coefs[3]*median(df$seniority, na.rm = T)  + coefs[4]*median(df$majority) + coefs[5]*median(df$power) + 
   coefs[6]*median(df$chair) + coefs[7]*median(df$female) + coefs[8]*median(df$inpres, na.rm = T) +
   median(fes_district$fes.effect.25.length.fes.effect..)  + median(fes_congress$fes.effect.1.24.)
 
p25 <- coefs[1]*quantile(df$logged_distance, probs = .25, na.rm = T) + coefs[2]*median(df$dem) +
   coefs[3]*median(df$seniority, na.rm = T)  + coefs[4]*median(df$majority) + coefs[5]*median(df$power) + 
   coefs[6]*median(df$chair) + coefs[7]*median(df$female) + coefs[8]*median(df$inpres, na.rm = T) +
   median(fes_district$fes.effect.25.length.fes.effect..)  + median(fes_congress$fes.effect.1.24.)
 
(p75 -  p25)/(p25) * 100
p75-p25
# equals 1.98 percent change and 1.79 higher party unity score
 
 
# ######### Cosponsorship #########
 
#get the fixed effects from the regression to include
fes <- getfe(reg6)
fes_district <- data.frame(fes$effect[25:length(fes$effect)])
fes_congress <- data.frame(fes$effect[1:24])

#get coefficient values
coefs <- reg6$coefficients

#calculate the value when everything is held at the median
p75 <- coefs[1]*quantile(df$logged_distance, probs = .75, na.rm = T) + coefs[2]*median(df$dem) +
   coefs[3]*median(df$seniority, na.rm = T)  + coefs[4]*median(df$majority) + coefs[5]*median(df$power) + 
   coefs[6]*median(df$chair) + coefs[7]*median(df$female) + coefs[8]*median(df$inpres, na.rm = T) +
   median(fes_district$fes.effect.25.length.fes.effect..)  + median(fes_congress$fes.effect.1.24.)
 
p25 <- coefs[1]*quantile(df$logged_distance, probs = .25, na.rm = T) + coefs[2]*median(df$dem) +
   coefs[3]*median(df$seniority, na.rm = T)  + coefs[4]*median(df$majority) + coefs[5]*median(df$power) + 
   coefs[6]*median(df$chair) + coefs[7]*median(df$female) + coefs[8]*median(df$inpres, na.rm = T) +
   median(fes_district$fes.effect.25.length.fes.effect..)  + median(fes_congress$fes.effect.1.24.)
 
(p75 -  p25)/(p25) * 100
(p75-p25) * 100
# equals 3.14 percent change and 2.44 percent more in-party cosponsors

