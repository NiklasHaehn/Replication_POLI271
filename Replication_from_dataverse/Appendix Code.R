###################################################################################################
# Far From Home                         ###############################################
# Jesse Crosson and Jaclyn Kaslovsky    ###############################################
# Appendix Analyses                     ###############################################
###################################################################################################

###################################################################################################
# Working Directory and Packages ##################################################################
###################################################################################################

setwd("~/Dropbox/Local Roots/Replication Package New/Data/")

require(dplyr)
require(ggplot2)
require(ggthemes)
require(doBy)
require(qwraps2)
require(cowplot)
require(gridExtra)
require(ggridges)
require(tidyr)
require(grid)
require(ggpubr)
require(lemon)
require(haven)
require(lfe)
require(stargazer)
require(starpolishr)
require(broom)
require(memisc)
require(lme4)
require(readstata13)

################################
# Load the Data  #########
################################

#Load in the main data for analyses
df <- read.dta13("Dataset_ForMainAnalysis.dta")


#############################################
# Controlling for other kinds of local roots
#############################################

# Testing Born in the Same State

reg1 <- felm(party_unity ~ state_match + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg1)

reg2 <- felm(inpart_cospon ~ state_match + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg2)

reg3 <- felm(pct_constituencystaff ~ state_match + dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg3)


reg1_table <- stargazer(reg1,reg2,reg3,
                        add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark"),
                                       c("Congress Fixed Effects","\\checkmark","\\checkmark", "\\checkmark")),
                        omit=c("Constant"),
                        notes.append = FALSE,notes.label = "",
                        report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                        omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                        dep.var.labels = c("Party Unity","Cosponsorship Attraction", "Pct. Staff Constituency Service"),column.sep.width="0pt",
                        covariate.labels=c("Born in State (0/1)", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                           "Member is a Woman", "Same Party Presidential Vote Share"),
                        notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                        label="tab_state",
                        digits=3,
                        digits.extra = 0,
                        title="State Roots and Measures of Representational Style")

cat(reg1_table, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/table_state.tex")

##################################
# Replicating born in the district
##################################

#party unity 
reg1_born <- felm(party_unity ~ inc_born_indist + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg1_born )

#cosponsoring
reg2_born <- felm(inpart_cospon ~ inc_born_indist + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg2_born)

#pct staff
reg3_born <- felm(pct_constituencystaff ~ inc_born_indist + dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg3_born)


table1_born <- stargazer(reg1_born,reg2_born,reg3_born,
                    add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark"),
                                   c("Congress Fixed Effects","\\checkmark","\\checkmark", "\\checkmark")),
                    omit=c("Constant"),
                    notes.append = FALSE,notes.label = "",
                    report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                    omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                    dep.var.labels = c("Party Unity","Cosponsorship Attraction", "Pct. Staff Constituency Service"),column.sep.width="0pt",
                    covariate.labels=c("Born in District (0/1)","Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                       "Member is a Woman", "Same Party Presidential Vote Share"),
                    notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                    label="tab_born2",
                    digits=3,
                    digits.extra = 0,
                    title="Hunt's Measure of Born in the District and Measures of Representational Style")

cat(table1_born, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/table_newborn.tex")



#########################################################
# Using the local roots index with Hunt's specification
#########################################################


mix.int_index1 <- lmer(party_unity ~ inc_roots_0220 + dem + seniority + majority + power + chair + female + inpres + (1 | icpsr), data = df)

mix.int_index2 <- lmer(inpart_cospon ~ inc_roots_0220 + dem + seniority + majority + power + chair + female + inpres + (1 | icpsr), data = df)

mix.int_index3 <- lmer(pct_constituencystaff ~ inc_roots_0220 + dem + seniority + majority + power + chair + female + inpres + (1 | icpsr), data = df)



main_index2 <- stargazer(mix.int_index1,mix.int_index2,mix.int_index3,
                         add.lines=list(c("Member Random Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                         omit=c("Constant"),
                         notes.append = FALSE,notes.label = "",
                         report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                         omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                         dep.var.labels = c("Party Unity","Cosponsorship Attraction", "Pct. of Staff Constituency Service"),column.sep.width="0pt",
                         covariate.labels=c("Home Field Advantage Index", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                            "Member is a Woman", "Same Party Presidential Vote Share"),
                         notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}:The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                         label="tab_homefield",
                         digits=3,
                         digits.extra = 0,
                         title="Alternative Measures of Local Roots from Home Field Advantage and Measures of Representational Style")

cat(main_index2, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/Table_replication2.tex")

########################################################################
# Adding high school and born in the district using Hunt's specification
########################################################################

mix.int1 <- lmer(party_unity ~ alt_binary + dem + seniority + majority + power + chair + female + inpres + (1 | icpsr), data = df)

mix.int2 <- lmer(party_unity ~ alt_index + dem + seniority + majority + power + chair + female + inpres + (1 | icpsr), data = df)


mix.int3 <- lmer(inpart_cospon ~ alt_binary + dem + seniority + majority + power + chair + female + inpres + (1 | icpsr), data = df)

mix.int4 <- lmer(inpart_cospon ~ alt_index + dem + seniority + majority + power + chair + female + inpres + (1 | icpsr), data = df)


mix.int5 <- lmer(pct_constituencystaff ~ alt_binary + dem + seniority + majority + power + chair + female + inpres + (1 | icpsr), data = df)

mix.int6 <- lmer(pct_constituencystaff ~ alt_index + dem + seniority + majority + power + chair + female + inpres + (1 | icpsr), data = df)



replication1 <- stargazer(mix.int1, mix.int3, mix.int5,
                          add.lines=list(c("Member Random Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                          omit=c("Constant"),
                          notes.append = FALSE,notes.label = "",
                          report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                          omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,dep.var.labels = c("Party Unity","Cosponsorship Attraction", "Pct. of Staff Constituency Service"),column.sep.width="0pt",
                          covariate.labels=c("Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                             "Member is a Woman", "Same Party Presidential Vote Share"),  
                          notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}:The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                          label="tab_homefieldnew",
                          digits=3,
                          digits.extra = 0,
                          title="Binary Measures of Local Roots updated with Highschool Measure from Home Field Advantage and Measures of Representational Style")

replication2 <- stargazer(mix.int2, mix.int4, mix.int6,
                          add.lines=list(c("Member Random Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                          omit=c("Constant"),
                          notes.append = FALSE,notes.label = "",
                          report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                          omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                          covariate.labels=c("Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                             "Member is a Woman", "Same Party Presidential Vote Share"), 
                          notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}:The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                          label="tab_homefieldnew",
                          digits=3,
                          digits.extra = 0,
                          title="Binary Measures of Local Roots updated with Highschool Measure from Home Field Advantage and Measures of Representational Style")



star.panel.out <- star_panel(replication1, replication2 ,panel.label.fontface="bold",
                             panel.names = c("Born or Went to High School in District (0/1)","Index of Born and High School in District (0,1,2)"), same.summary.stats = FALSE)
cat(star.panel.out, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/Table_replication1.tex")

#################################################
# Quadratic Specification of Local Roots  #######
#################################################

df$minsquared <- df$distance_miles^2

reg1_quad <- felm(party_unity  ~ distance_miles + minsquared  + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df)
summary(reg1_quad)

reg2_quad <- felm(inpart_cospon ~  distance_miles + minsquared  + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df)
summary(reg2_quad)

reg3_quad <- felm(pct_constituencystaff ~ distance_miles + minsquared  + dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg3_quad)



regquad_table <- stargazer(reg1_quad,reg2_quad,reg3_quad,
                           add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark"),
                                          c("Congress Fixed Effects","\\checkmark","\\checkmark", "\\checkmark")),
                           omit=c("Constant"),
                           notes.append = FALSE,notes.label = "",
                           report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                           omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                           dep.var.labels = c("Party Unity","Cosponsorship Attraction", "Pct. Staff Constituency Service"),column.sep.width="0pt",
                           covariate.labels=c("Distance Born from District", "Distance Born from District Squared","Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                              "Member is a Woman", "Same Party Presidential Vote Share"),
                           notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                           label="tab_quad",
                           digits=5,
                           digits.extra = 0,
                           title="Local Roots and Measures of Representational Style, Using a Quadratic Function")

cat(regquad_table, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/table_quad.tex")

##########################################################################
# Replacing Presidential Vote Share with the Member's Previous Vote Share
##########################################################################
# Party Unity: Controlling for Previous Vote Share 

df$distance <- df$binary
reg1_votepct <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + votepct | congress + districtID | 0 | districtID, data=df)
summary(reg1_votepct)

df$distance <- df$logged_distance
reg2_votepct <- felm(party_unity  ~ distance + dem + seniority + majority + power + chair + female + votepct  | congress + districtID | 0 | districtID, data=df)
summary(reg2_votepct)


reg1_table_votepct <- stargazer(reg1_votepct,reg2_votepct,
                                add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark"),
                                               c("Congress Fixed Effects","\\checkmark","\\checkmark")),
                                omit=c("Constant"),
                                notes.append = FALSE,notes.label = "",
                                report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                                dep.var.labels = "Party Unity Score",column.sep.width="0pt",
                                covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                   "Member is a Woman", "Previous Vote Share"),
                                notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                label="tab1_votepct",
                                digits=3,
                                digits.extra = 0,
                                title="Member Birth Place and Party Unity with Previous General Election Vote Share, 1973 to 2020")

cat(reg1_table_votepct, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_table_new_votepct.tex")

# Cosponsorship: Controlling for Previous Vote Share

df$distance <- df$binary
reg3_votepct <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + votepct | congress + districtID | 0 | districtID, data=df)
summary(reg3_votepct)

df$distance <- df$logged_distance
reg4_votepct <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + votepct  | congress + districtID | 0 | districtID, data=df)
summary(reg4_votepct)

reg2_table_votepct <- stargazer(reg3_votepct,reg4_votepct,
                                add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark"),
                                               c("Congress Fixed Effects","\\checkmark","\\checkmark")),
                                omit=c("Constant"),
                                notes.append = FALSE,notes.label = "",
                                report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                                dep.var.labels = "Percent of Cosponsors that are Copartisans",column.sep.width="0pt",
                                covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                   "Member is a Woman", "Previous Vote Share"),
                                notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                label="tab2_votepct",
                                digits=3,
                                digits.extra = 0,
                                title="Member Birth Place and Cosponsorship Attraction with Previous General Election Vote Share, 1973 to 2020")

cat(reg2_table_votepct, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_table_cospon_votepct.tex")

# Constituency Service: Controlling for Previous Vote Share

df$distance <- df$binary
reg5_votepct <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power +chair + female + votepct | congress + districtID | 0 | districtID, data=df)
summary(reg5_votepct)


df$distance <- df$logged_distance
reg6_votepct <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power +chair + female + votepct | congress + districtID | 0 | districtID, data=df)
summary(reg6_votepct)


reg3_table_votepct <- stargazer(reg5_votepct, reg6_votepct,
                                add.lines=list(c("District Fixed Effects", "\\checkmark", "\\checkmark"),
                                               c("Congress Fixed Effects",  "\\checkmark","\\checkmark")),
                                omit=c("Constant"),
                                notes.append = FALSE,notes.label = "",
                                report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                                dep.var.labels = "Percent of Staff Allocated Towards Constituency Service",column.sep.width="0pt",
                                covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                   "Member is a Woman", "Previous Vote Share"),
                                notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                label="tab3_votepct",
                                digits=3,
                                digits.extra = 0,
                                title="Local Roots and Percent of Staff that Focus on Constituency Service with Previous General Election Vote Share, 1993 to 2014")
cat(reg3_table_votepct, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg3_table_votepct.tex")

###########################################################
# Replacing Fixed Effects with a Linear Time Trend  #######
###########################################################

# Party Unity with Linear Time Trend

df$distance <- df$binary
reg1_linear <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + inpres + congress | districtID | 0 | districtID, data=df)
summary(reg1_linear)

df$distance <- df$logged_distance
reg2_linear <- felm(party_unity  ~ distance + dem + seniority + majority + power + chair + female + inpres + congress | districtID | 0 | districtID, data=df)
summary(reg2_linear)


reg1_table_linear <- stargazer(reg1_linear,reg2_linear,
                               add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark")),
                               omit=c("Constant"),
                               notes.append = FALSE,notes.label = "",
                               report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                               omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                               column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                               dep.var.labels = "Party Unity Score",column.sep.width="0pt",
                               covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                  "Member is a Woman", "Same Party Presidential Vote Share", "Congress"),
                               notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                               label="tab1_linear",
                               digits=3,
                               digits.extra = 0,
                               title="Member Birth Place and Party Unity with Linear Time Trend, 1973 to 2020")

cat(reg1_table_linear, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_table_new_linear.tex")

# Cosponsorship with Linear Time Trend

df$distance <- df$binary
reg3_linear <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres + congress | districtID | 0 | districtID, data=df)
summary(reg3_linear)

df$distance <- df$logged_distance
reg4_linear <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres + congress | districtID| 0 | districtID, data=df)
summary(reg4_linear)

reg2_table_linear <- stargazer(reg3_linear,reg4_linear,
                               add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark")),
                               omit=c("Constant"),
                               notes.append = FALSE,notes.label = "",
                               report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                               omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                               column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                               dep.var.labels = "Percent of Cosponsors that are Copartisans",column.sep.width="0pt",
                               covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                  "Member is a Woman", "Same Party Presidential Vote Share", "Congress"),
                               notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                               label="tab2_linear",
                               digits=3,
                               digits.extra = 0,
                               title="Member Birth Place and Cosponsorship Attraction with Linear Time Trend, 1973 to 2020")

cat(reg2_table_linear, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_table_cospon_linear.tex")

# Staffing with Linear Time Trend

df$distance <- df$binary
reg5_linear <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power +chair + female + inpres + congress | districtID | 0 | districtID, data=df)
summary(reg5_linear)


df$distance <- df$logged_distance
reg6_linear <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power +chair + female + inpres + congress | districtID | 0 | districtID, data=df)
summary(reg6_linear)


reg3_table_linear <- stargazer(reg5_linear, reg6_linear,
                               add.lines=list(c("District Fixed Effects", "\\checkmark", "\\checkmark")),
                               omit=c("Constant"),
                               notes.append = FALSE,notes.label = "",
                               report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                               omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                               column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                               dep.var.labels = "Percent of Staff Allocated Towards Constituency Service",column.sep.width="0pt",
                               covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                  "Member is a Woman", "Same Party Presidential Vote Share", "Congress"),
                               notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                               label="tab3_linear",
                               digits=3,
                               digits.extra = 0,
                               title="Local Roots and Percent of Staff that Focus on Constituency Service with Linear Time Trend, 1993 to 2014")
cat(reg3_table_linear, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg3_table_linear.tex")

############################################################
## Results with Legislator Clustered Standard Errors #######
############################################################

# Party Unity with legislator-clustered standard errors

df$distance <- df$binary
reg1_altse <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | icpsr, data=df)
summary(reg1_altse)

df$distance <- df$logged_distance
reg2_altse <- felm(party_unity  ~ distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | icpsr, data=df)
summary(reg2_altse)


reg1_table_altse <- stargazer(reg1_altse,reg2_altse,
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
                              notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with member-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                              label="tab1_altse",
                              digits=3,
                              digits.extra = 0,
                              title="Member Birth Place and Party Unity with Legislator Clustered Standard Errors, 1973 to 2020")

cat(reg1_table_altse, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_table_new_altse.tex")

# Cosponsorship with legislator-clustered standard errors

df$distance <- df$binary
reg3_altse <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | icpsr, data=df)
summary(reg3_altse)

df$distance <- df$logged_distance
reg4_altse <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | icpsr, data=df)
summary(reg4_altse)

reg2_table_altse <- stargazer(reg3_altse,reg4_altse,
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
                              notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with member-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                              label="tab2_altse",
                              digits=3,
                              digits.extra = 0,
                              title="Member Birth Place and Cosponsorship Attraction with Legislator Clustered Standard Errors, 1973 to 2020")

cat(reg2_table_altse, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_table_cospon_altse.tex")

# Staffing with legislator-clustered standard errors

df$distance <- df$binary
reg5_altse <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | icpsr, data=df)
summary(reg5_altse)


df$distance <- df$logged_distance
reg6_altse <- felm(pct_constituencystaff ~ distance + dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | icpsr, data=df)
summary(reg6_altse)


reg3_table_altse <- stargazer(reg5_altse, reg6_altse,
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
                              notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with member-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                              label="tab3_altse",
                              digits=3,
                              digits.extra = 0,
                              title="Local Roots and Percent of Staff that Focus on Constituency Service with Legislator Clustered Standard Errors, 1993 to 2014")
cat(reg3_table_altse, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg3_table_altse.tex")

######################################
# Local Staff Over Time Figure  ######
######################################

#Collapse the data so we can show over time trends
collapsed_cong <- df %>%
  group_by(congress) %>%
  dplyr::summarize(avg_born = mean(distance_miles, na.rm=TRUE))

collapsed_cong2<- left_join(df, collapsed_cong, by="congress")


fig2 <- ggplot(collapsed_cong2, aes(x = pct_constituencystaff, y = congress, height=..density.., group=avg_born))+
  geom_density_ridges_gradient(aes(fill = avg_born), scale = 3, size = 0.3) +
  scale_fill_gradientn(
    colours = c("grey","grey25"),
    name = "Avg. Distance Born from District (Miles)")+ theme_minimal() + ylab("Congress") +
  xlab("Percent of Staff Allocated Towards Constituency Service") + xlim(0, 100) +  theme(legend.position="bottom")
ggsave("~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/graph_overtime_staff.png",fig2,width = 9, height = 7) 

################################
# Results  by Decade  ####
################################
# Party Unity by Decade 

#1970s - doesn't include majority because dem's in majority the entire time
reg_70spu <- felm(party_unity  ~ logged_distance + dem + seniority + power + chair + female + inpres  |  congress + districtID | 0 | districtID, data=df[df$year<1981,])
summary(reg_70spu)
reg_70s2pu <- felm(party_unity  ~ binary + dem + seniority +  power + chair + female + inpres  |congress +  districtID | 0 | districtID, data=df[df$year<1981,])
summary(reg_70s2pu)

#1980s - doesn't include majority because dem's in majority the entire time
reg_80spu <- felm(party_unity  ~ logged_distance + dem + seniority +  power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=1981 & df$year<1991,])
summary(reg_80spu)
reg_80s2pu <- felm(party_unity  ~ binary + dem + seniority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=1981 & df$year<1991,])
summary(reg_80s2pu)

#1990s
reg_90spu <- felm(party_unity  ~ logged_distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=1991 & df$year<2001,])
summary(reg_90spu)
reg_90s2pu <- felm(party_unity  ~ binary + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=1991 & df$year<2001,])
summary(reg_90s2pu)

#2000s
reg_00spu <- felm(party_unity  ~ logged_distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2001 & df$year<2011,])
summary(reg_00spu)
reg_00s2pu <- felm(party_unity  ~ binary + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2001 & df$year<2011,])
summary(reg_00s2pu)

#2010s
reg_10spu <- felm(party_unity  ~ logged_distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2010,])
summary(reg_10spu)
reg_10s2pu <- felm(party_unity  ~ binary + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2010,])
summary(reg_10s2pu)

#table version for appendix 
reg1_table_decade <- stargazer(reg_70spu, reg_80spu, reg_90spu, reg_00spu, reg_10spu,
                               add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                                              c("Congress Fixed Effects","\\checkmark","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                               omit=c("Constant"),
                               notes.append = FALSE,notes.label = "",
                               report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                               omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                               column.labels=c("1970s", "1980s", "1990s", "2000s", "2010s"),
                               dep.var.labels = "",column.sep.width="0pt",
                               covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                  "Member is a Woman", "Same Party Presidential Vote Share"),
                               notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                               label="tab_decade",
                               digits=3,
                               digits.extra = 0,
                               title="Member Birth Place and Party Unity by Decade")

reg1_table_decade2 <- stargazer(reg_70s2pu, reg_80s2pu, reg_90s2pu, reg_00s2pu, reg_10s2pu,
                                add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                                               c("Congress Fixed Effects","\\checkmark","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                                omit=c("Constant"),
                                notes.append = FALSE,notes.label = "",
                                report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                column.labels=c("1970s", "1980s", "1990s", "2000s", "2010s"),
                                dep.var.labels = "",column.sep.width="0pt",
                                covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                   "Member is a Woman", "Same Party Presidential Vote Share"),
                                notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                label="tab_decade",
                                digits=3,
                                digits.extra = 0,
                                title="Member Birth Place and Party Unity by Decade")

star.panel.out <- star_panel(reg1_table_decade2,reg1_table_decade ,panel.label.fontface="bold",
                             panel.names = c("Born in District (0/1)","Log(Miles Born from District + 1)"), same.summary.stats = FALSE)
cat(star.panel.out, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/Table_decade.tex")



# Copsonsorship by decade

#1970s - doesn't include majority because dem's in majority the entire time
reg_70s <- felm(inpart_cospon  ~ logged_distance + dem + seniority + power + chair + female + inpres  |  congress + districtID | 0 | districtID, data=df[df$year<1981,])
reg_70s2 <- felm(inpart_cospon  ~ binary + dem + seniority +  power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year<1981,])

#1980s - doesn't include majority because dem's in majority the entire time
reg_80s <- felm(inpart_cospon  ~ logged_distance + dem + seniority +  power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=1981 & df$year<1991,])
reg_80s2 <- felm(inpart_cospon  ~ binary + dem + seniority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=1981 & df$year<1991,])

#1990s
reg_90s <- felm(inpart_cospon  ~ logged_distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=1991 & df$year<2001,])
reg_90s2 <- felm(inpart_cospon  ~ binary + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=1991 & df$year<2001,])

#2000s
reg_00s<- felm(inpart_cospon  ~ logged_distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2001 & df$year<2011,])
reg_00s2 <- felm(inpart_cospon ~ binary + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2001 & df$year<2011,])

#2010s
reg_10s <- felm(inpart_cospon  ~ logged_distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2010,])
reg_10s2 <- felm(inpart_cospon  ~ binary + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2010,])

#table version for appendix 
reg1_table_decade_cosponsor <- stargazer(reg_70s, reg_80s, reg_90s, reg_00s, reg_10s,
                                         add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                                                        c("Congress Fixed Effects","\\checkmark","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                                         omit=c("Constant"),
                                         notes.append = FALSE,notes.label = "",
                                         report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                         omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                         column.labels=c("1970s", "1980s", "1990s", "2000s", "2010s"),
                                         dep.var.labels = "",column.sep.width="0pt",
                                         covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                            "Member is a Woman", "Same Party Presidential Vote Share"),
                                         notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                         label="tab_decade_cosponsor",
                                         digits=3,
                                         digits.extra = 0,
                                         title="Member Birth Place and Cosponsorship Attraction by Decade")

reg1_table_decade2_cosponsor <- stargazer(reg_70s2, reg_80s2, reg_90s2, reg_00s2, reg_10s2,
                                          add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                                                         c("Congress Fixed Effects","\\checkmark","\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                                          omit=c("Constant"),
                                          notes.append = FALSE,notes.label = "",
                                          report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                          omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                          column.labels=c("1970s", "1980s", "1990s", "2000s", "2010s"),
                                          dep.var.labels = "",column.sep.width="0pt",
                                          covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                             "Member is a Woman", "Same Party Presidential Vote Share"),
                                          notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                          label="tab_decade_cosponsor",
                                          digits=3,
                                          digits.extra = 0,
                                          title="Member Birth Place and Cosponsorship Attraction by Decade")

star.panel.out_cosponsor <- star_panel(reg1_table_decade2_cosponsor,reg1_table_decade_cosponsor ,panel.label.fontface="bold",
                                       panel.names = c("Born in District (0/1)","Log(Miles Born from District + 1)"), same.summary.stats = FALSE)
cat(star.panel.out_cosponsor, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/Table_decade_cosponsor.tex")



# Staffing by decade   


#1990s
reg_90s_staff <- felm(pct_constituencystaff  ~ logged_distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year<2001,])
reg_90s2_staff <- felm(pct_constituencystaff  ~ binary + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year<2001,])

#2000s and on - not separating out the 2010s because there would only be two for this analysis
reg_00s_staff <- felm(pct_constituencystaff  ~ logged_distance + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2001,])
reg_00s2_staff <- felm(pct_constituencystaff ~ binary + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df[df$year>=2001,])

#table version for appendix 
reg1_table_decade_staff <- stargazer(reg_90s_staff, reg_00s_staff,
                                     add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark"),
                                                    c("Congress Fixed Effects","\\checkmark", "\\checkmark")),
                                     omit=c("Constant"),
                                     notes.append = FALSE,notes.label = "",
                                     report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                     omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                     column.labels=c("1993-2000", "2001-2014"),
                                     dep.var.labels = "",column.sep.width="0pt",
                                     covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                        "Member is a Woman", "Same Party Presidential Vote Share"),
                                     notes="\\parbox[t]{.6\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                     label="tab_decade_staff",
                                     digits=3,
                                     digits.extra = 0,
                                     title="Member Birth Place and Percent of Staff that Focus on Constituency Service by Decade")

reg1_table_decade2_staff <- stargazer(reg_90s2_staff, reg_00s2_staff,
                                      add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark"),
                                                     c("Congress Fixed Effects","\\checkmark", "\\checkmark")),
                                      omit=c("Constant"),
                                      notes.append = FALSE,notes.label = "",
                                      report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                      omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                      column.labels=c("1993-2000", "2001-2014"),
                                      dep.var.labels = "",column.sep.width="0pt",
                                      covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                         "Member is a Woman", "Same Party Presidential Vote Share"),
                                      notes="\\parbox[t]{.6\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                      label="tab_decade_staff",
                                      digits=3,
                                      digits.extra = 0,
                                      title="Member Birth Place and Percent of Staff that Focus on Constituency Service by Decade")

star.panel.out_staff <- star_panel(reg1_table_decade2_staff,reg1_table_decade_staff ,panel.label.fontface="bold",
                                   panel.names = c("Born in District (0/1)","Log(Miles Born from District + 1)"), same.summary.stats = FALSE)
cat(star.panel.out_staff, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/Table_decade_staff.tex")

#############################################################
# Alternative Measures of Staff Allocation  #########
#############################################################

# Proportion constituency spending

df$distance <- df$binary
reg1_pct <- felm(pct_MRA_constit_spending ~ distance + dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, data=df)

df$distance <- df$logged_distance
reg2_pct <- felm(pct_MRA_constit_spending ~ distance + dem + seniority + majority + power +chair + female  + inpres| congress + districtID | 0 | districtID, data=df)


reg2_table <- stargazer(reg1_pct,reg2_pct,
                        add.lines=list(c("District Fixed Effects", "\\checkmark", "\\checkmark"),
                                       c("Congress Fixed Effects","\\checkmark","\\checkmark")),
                        omit=c("Constant"),
                        notes.append = FALSE,notes.label = "",
                        report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                        omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                        column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                        dep.var.labels = "\\%MRA Spent on Constituency Staff",column.sep.width="0pt",
                        covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                           "Member is a Woman", "Same Party Presidential Vote Share"),
                        notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                        label="tab3_pct",
                        digits=3,
                        digits.extra = 0,
                        title="Local Roots and Percent of MRA Spent on Staff, 1993 to 2014")
cat(reg2_table, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg2_table_pct.tex")

#######################################
## Testing Alternative Theories #######
#######################################

# Mismatch Ideology Analysis

#### Mismatch: Party Unity ####
df$distance <- df$binary
regA1 <- felm(party_unity ~ distance + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, 
              data=subset(df, mismatch_moderate > 0))
summary(regA1) # note that dem is dropped because matrix is rank-deficient

regA2 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID,
              data=subset(df, mismatch_extreme > 0))
summary(regA2)

df$distance <- df$logged_distance
regB1 <- felm(party_unity ~ distance + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, 
              data=subset(df, mismatch_moderate > 0))
summary(regB1) # note that dem is dropped because matrix is rank-deficient

regB2 <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID,
              data=subset(df, mismatch_extreme > 0))
summary(regB2)

regMIS_table_PU <- stargazer(regA1, regA2, regB1, regB2,
                             add.lines=list(c("'Moderate' Mismatch", "\\checkmark", "", "\\checkmark", ""),
                                            c("'Extreme' Mismatch", "", "\\checkmark", "", "\\checkmark"),
                                            c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                                            c("Congress Fixed Effects",  "\\checkmark","\\checkmark", "\\checkmark", "\\checkmark")),
                             omit=c("Constant"),
                             notes.append = FALSE,notes.label = "",
                             report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                             omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                             column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),column.separate=c(2,2),
                             dep.var.labels = "Party Unity Score",column.sep.width="0pt",
                             covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                "Member is a Woman", "Same Party Presidential Vote Share"),
                             notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                             label="tab_PU",
                             digits=3,
                             digits.extra = 0,
                             title="Local Roots, Ideological Mismatch, and Party-Line Roll Call Voting")
cat(regMIS_table_PU, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/regMIS_table_PU.tex")



#### Mismatch: Cosponsorship Measure ####

df$distance <- df$binary
regA3 <- felm(inpart_cospon ~ distance + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, 
              data=subset(df, mismatch_moderate > 0))
summary(regA3) # note that dem is dropped because matrix is rank-deficient

regA4 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID,
              data=subset(df, mismatch_extreme > 0))
summary(regA4)

# Result is negative in both subgroups, but significant only in the moderate one

df$distance <- df$logged_distance
regB3 <- felm(inpart_cospon ~ distance + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, 
              data=subset(df, mismatch_moderate > 0))
summary(regB3)

regB4 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID,
              data=subset(df, mismatch_extreme > 0))
summary(regB4)

regMIS_table_CS <- stargazer(regA3, regA4, regB3, regB4,
                             add.lines=list(c("'Moderate' Mismatch", "\\checkmark", "", "\\checkmark", ""),
                                            c("'Extreme' Mismatch", "", "\\checkmark", "", "\\checkmark"),
                                            c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                                            c("Congress Fixed Effects",  "\\checkmark","\\checkmark", "\\checkmark", "\\checkmark")),
                             omit=c("Constant"),
                             notes.append = FALSE,notes.label = "",
                             report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                             omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                             column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),column.separate=c(2,2),
                             dep.var.labels = "Cosponsorship Attraction",column.sep.width="0pt",
                             covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                "Member is a Woman", "Same Party Presidential Vote Share"),
                             notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                             label="tab_MS",
                             digits=3,
                             digits.extra = 0,
                             title="Local Roots, Ideological Mismatch, and Attraction of Co-partisan Cosponsors")
cat(regMIS_table_CS, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/regMIS_table_CS.tex")


# Party unity subset by district safety

df$distance <- df$binary
reg1a <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female | congress + districtID | 0 | districtID, data=subset(df, inpres > 0.5749))
summary(reg1a)

reg1b <- felm(party_unity ~ distance + dem + seniority + majority + power + chair + female | congress + districtID | 0 | districtID, data=subset(df, inpres < 0.5749))
summary(reg1b)


df$distance <- df$logged_distance
reg2a <- felm(party_unity  ~ distance + dem + seniority + majority + power + chair + female  | congress + districtID | 0 | districtID, data=subset(df, inpres > 0.5749))
summary(reg2a)


reg2b <- felm(party_unity  ~ distance + dem + seniority + majority + power + chair + female  | congress + districtID | 0 | districtID, data=subset(df, inpres < 0.5749))
summary(reg2b)

reg_table_safety <- stargazer(reg1a, reg1b, reg2a, reg2b,
                              add.lines=list(c("Safe District","\\checkmark", "","\\checkmark", ""),
                                             c("District Fixed Effects","\\checkmark", "\\checkmark","\\checkmark", "\\checkmark"),
                                             c("Congress Fixed Effects","\\checkmark","\\checkmark", "\\checkmark", "\\checkmark")),
                              omit=c("Constant"),
                              notes.append = FALSE,notes.label = "",
                              report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                              omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                              column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),column.separate=c(2,2),
                              dep.var.labels = "Party Unity Score",column.sep.width="0pt",
                              covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                 "Member is a Woman", "Same Party Presidential Vote Share"),
                              notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                              label="tab1_safety",
                              digits=3,
                              digits.extra = 0,
                              title="Member Birth Place and Party Unity Subset by District Safety, 1973 to 2020")

cat(reg_table_safety, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg_table_safety.tex")

# Cosponsorship subset by district safety

df$distance <- df$binary
reg1a_cospon <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female | congress + districtID | 0 | districtID, data=subset(df, inpres > 0.5749))
summary(reg1a_cospon)

reg1b_cospon <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female | congress + districtID | 0 | districtID, data=subset(df, inpres < 0.5749))
summary(reg1b_cospon)


df$distance <- df$logged_distance
reg2a_cospon <- felm(inpart_cospon  ~ distance + dem + seniority + majority + power + chair + female  | congress + districtID | 0 | districtID, data=subset(df, inpres > 0.5749))
summary(reg2a_cospon)


reg2b_cospon <- felm(inpart_cospon  ~ distance + dem + seniority + majority + power + chair + female  | congress + districtID | 0 | districtID, data=subset(df, inpres < 0.5749))
summary(reg2b_cospon)

reg_table_safety_cospon <- stargazer(reg1a_cospon, reg1b_cospon, reg2a_cospon, reg2b_cospon,
                                     add.lines=list(c("Safe District","\\checkmark", "","\\checkmark", ""),
                                                    c("District Fixed Effects","\\checkmark", "\\checkmark","\\checkmark", "\\checkmark"),
                                                    c("Congress Fixed Effects","\\checkmark","\\checkmark", "\\checkmark", "\\checkmark")),
                                     omit=c("Constant"),
                                     notes.append = FALSE,notes.label = "",
                                     report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                     omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                     column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),column.separate=c(2,2),
                                     dep.var.labels = "Percent of Cosponsors that are Copartisans",column.sep.width="0pt",
                                     covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                        "Member is a Woman", "Same Party Presidential Vote Share"),
                                     notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                     label="tab1_safety_cospon",
                                     digits=3,
                                     digits.extra = 0,
                                     title="Member Birth Place and Cosponsorship Attraction Subset by District Safety, 1973 to 2020")

cat(reg_table_safety_cospon, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg_table_safety_cospon.tex")


#############################################################
# Alternative Measures of Cosponsorship Attraction  #########
#############################################################

#raw outcome while controlling for df cosponsorships
df$distance <- df$binary
reg3a <- felm(inpart_cospon2 ~ distance + total_cospon + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID, data=df)
summary(reg3a)

df$distance <- df$logged_distance
reg4a <- felm(inpart_cospon2 ~ distance + total_cospon + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df)
summary(reg4a)

reg1_table2 <- stargazer(reg3a,reg4a,
                         add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark"),
                                        c("Congress Fixed Effects","\\checkmark","\\checkmark")),
                         omit=c("Constant"),
                         notes.append = FALSE,notes.label = "",
                         report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                         omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                         column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                         dep.var.labels = "Number of Cosponsors that are Copartisans",column.sep.width="0pt",
                         covariate.labels=c("Measure of Local Roots","Total Cosponsorships", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                            "Member is a Woman", "Same Party Presidential Vote Share"),
                         notes="\\parbox[t]{.8\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                         label="tab2_2",
                         digits=3,
                         digits.extra = 0,
                         title="Member Birth Place and Cosponsorship Attraction, Alternative Measure, 1973 to 2020")

cat(reg1_table2, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_table_cospon2.tex")


#Controlling for ideological extremism instead of previous presidential vote share

df$distance <- df$binary
reg3_absextreme <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + meddist | congress + districtID | 0 | districtID, data=df)
summary(reg3_absextreme)

df$distance <- df$logged_distance
reg4_absextreme <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + meddist  | congress + districtID | 0 | districtID, data=df)
summary(reg4_absextreme)

reg1_table_absextreme <- stargazer(reg3_absextreme,reg4_absextreme,
                                   add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark"),
                                                  c("Congress Fixed Effects","\\checkmark","\\checkmark")),
                                   omit=c("Constant"),
                                   notes.append = FALSE,notes.label = "",
                                   report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                   omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                   column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                                   dep.var.labels = "Percent of Cosponsors that are Copartisans",column.sep.width="0pt",
                                   covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                      "Member is a Woman", "Absolute Distance from Median Party Member"),
                                   notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                   label="tab2_extreme",
                                   digits=3,
                                   digits.extra = 0,
                                   title="Member Birth Place and Cosponsorship Attraction Replacing Presidential Vote Share with Member Ideology, 1973 to 2020")

cat(reg1_table_absextreme, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_table_cospon_absextreme.tex")

# Controlling for ideological extremism AND presidential voteshare 

df$distance <- df$binary
reg3_absextreme2 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres + meddist | congress + districtID | 0 | districtID, data=df)
summary(reg3_absextreme2)

df$distance <- df$logged_distance
reg4_absextreme2 <- felm(inpart_cospon ~ distance + dem + seniority + majority + power + chair + female + inpres + meddist  | congress + districtID | 0 | districtID, data=df)
summary(reg4_absextreme2)

reg1_table_absextreme2 <- stargazer(reg3_absextreme2,reg4_absextreme2,
                                    add.lines=list(c("District Fixed Effects","\\checkmark", "\\checkmark"),
                                                   c("Congress Fixed Effects","\\checkmark","\\checkmark")),
                                    omit=c("Constant"),
                                    notes.append = FALSE,notes.label = "",
                                    report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                                    omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                                    column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),
                                    dep.var.labels = "Percent of Cosponsors that are Copartisans",column.sep.width="0pt",
                                    covariate.labels=c("Measure of Local Roots", "Democrat", "Seniority", "Majority Party","Member of Power Committees", "Committee Chair",
                                                       "Member is a Woman", "Same Party Presidential Vote Share", "Absolute Distance from Median Party Member"),
                                    notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                                    label="tab2_extreme2",
                                    digits=3,
                                    digits.extra = 0,
                                    title="Member Birth Place and Cosponsorship Attraction Including both Presidential Vote Share and Member Ideology, 1973 to 2020")

cat(reg1_table_absextreme2, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_table_cospon_absextreme2.tex")


################################
# Create a Balance Table  ######
################################

library("arsenal") 
library(gtools)

my_labels <- list(
  binary="Born in State (0/1)",
  dem = "Democrat",
  seniority="Seniority",
  majority="Majority Party",
  power ="Member of Power Committees",
  chair ="Committee Chair",
  female="Member is a Woman",
  inpres="Same Party Presidential Vote Share",
  prcntForeignBorn = "Percent Foreign Born",
  medianIncome= "Median Income",
  prcntBA = "Percent with BA",
  prcntWhiteAll = "Percent White"
)

require("stats")


#### Within and Between District Variation for All Variables

fundat <- df %>% ungroup()%>% 
  dplyr::select(dem, seniority, majority, power, chair, female, inpres,prcntForeignBorn,medianIncome, prcntBA,
                prcntWhiteAll, state_mobility, born_instate, districtID)


btw_win <- function(x, ...){
  if(class(x) == "numeric"){
    districtID <- factor(fundat[["districtID"]])
    fit = lm(x ~ districtID)
    within <- anova(fit)["Residuals", "Mean Sq"]
    between <- anova(fit)["districtID", "Mean Sq"]
    all <- within + between
    out <- c(within/all, between/all)}else{
      out <- NA
    }
  return(out)
}

output <- NA #note that warnings are due to adding columns; values are manually entered in to the .tex code for Table L.1
for(i in seq_along(fundat)){ # takes a *long* time but does finish
  output <- cbind(output, c(names(fundat)[i], btw_win(fundat[[i]])))
}

# write.csv(output, "win_btw_vars.csv", row.names = F)

##### 

table_one <- tableby(quant ~ ., data = df %>% ungroup() %>%
                       mutate(quant = quantcut(logged_distance, q = 4, na.rm = TRUE))%>%
                       dplyr::select(quant, dem, seniority, majority, power, chair, female, inpres,prcntForeignBorn,medianIncome, prcntBA,
                                     prcntWhiteAll, state_mobility,born_instate),
                     numeric.stats=c("mean","sd","range"),test=FALSE, df=FALSE,digits=2) 
des_table<-summary(table_one, title = "Comparing Local and Non-Local Legislators",  labelTranslations = my_labels,
                   text="latex")

require(data.table)
require("mvbutils")

write_clip(tableby)
writeClipboard()

##################################################
# Including District Level Demographics   #######
##################################################

df$logincome <- log(df$medianIncome+1)

# Party Unity with district level demographics

df$distance <- df$binary
reg1_n <- felm(party_unity ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power + chair + female + inpres | congress | 0 | districtID, data=df)

reg1_dist <- felm(party_unity ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID, data=df)

reg1_ideo <- felm(party_unity ~ distance + born_instate + logincome + prcntBA + prcntWhiteAll + mrp_ideology +  dem + seniority + majority + power + chair + female + inpres | congress | 0 | districtID, data=df)

df$distance <- df$logged_distance
reg2_n <- felm(party_unity  ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power + chair + female + inpres  | congress  | 0 | districtID, data=df)

reg2_dist <- felm(party_unity  ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df)

reg2_ideo <- felm(party_unity  ~ distance + born_instate + logincome + prcntBA + prcntWhiteAll + mrp_ideology + dem + seniority + majority + power + chair + female + inpres  | congress  | 0 | districtID, data=df)


reg1_table_fb <- stargazer(reg1_n, reg1_dist, reg1_ideo, reg2_n, reg2_dist,reg2_ideo, 
                           add.lines=list(c("District Fixed Effects","","\\checkmark","","","\\checkmark", ""),
                                          c("Congress Fixed Effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark"),
                                          c("Time Period","1983 to 2020","1983 to 2020","2009 to 2020","1983 to 2020","1983 to 2020","2009 to 2020")),
                           omit=c("Constant"),
                           notes.append = FALSE,notes.label = "",
                           report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                           omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                           column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),column.separate = c(3,3),
                           dep.var.labels = "Party Unity Score",column.sep.width="0pt",
                           covariate.labels=c("Measure of Local Roots", "Percent Born in State","Log(Median Income+1)", "Percent with BA", "Percent White",
                                              "MRP District Ideology Estimate", "Democrat", "Seniority", "Majority Party","Member of Power Committees",
                                              "Committee Chair", "Member is a Woman", "Same Party Presidential Vote Share"),
                           notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                           label="tab1_fb",
                           digits=3,
                           digits.extra = 0,
                           title="Member Birth Place and Party Unity Controlling for District Level Characteristics")

cat(reg1_table_fb, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg1_fb.tex")


# Cosponsorship with district level demographics

df$distance <- df$binary
reg3_n <- felm(inpart_cospon ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power + chair + female + inpres | congress  | 0 | districtID, data=df)

reg3_dist <- felm(inpart_cospon ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power + chair + female + inpres | congress + districtID | 0 | districtID, data=df)

reg3_ideo <- felm(inpart_cospon ~ distance  + born_instate + logincome + prcntBA + prcntWhiteAll + mrp_ideology + dem + seniority + majority + power + chair + female + inpres | congress  | 0 | districtID, data=df)

df$distance <- df$logged_distance
reg4_n <- felm(inpart_cospon ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power + chair + female + inpres  | congress  | 0 | districtID, data=df)

reg4_dist <- felm(inpart_cospon ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power + chair + female + inpres  | congress + districtID | 0 | districtID, data=df)

reg4_ideo <- felm(inpart_cospon ~ distance + born_instate + logincome + prcntBA + prcntWhiteAll + mrp_ideology  +  dem + seniority + majority + power + chair + female + inpres | congress  | 0 | districtID, data=df)


reg2_table_fb <- stargazer(reg3_n, reg3_dist, reg3_ideo, reg4_n, reg4_dist,reg4_ideo, 
                           add.lines=list(c("District Fixed Effects","","\\checkmark","","","\\checkmark", ""),
                                          c("Congress Fixed Effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark"),
                                          c("Time Period","1983 to 2020","1983 to 2020","2009 to 2020","1983 to 2020","1983 to 2020","2009 to 2020")),
                           omit=c("Constant"),
                           notes.append = FALSE,notes.label = "",
                           report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                           omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                           column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),column.separate = c(3,3),
                           dep.var.labels = "Percent of Cosponsors that are Copartisans",column.sep.width="0pt",
                           covariate.labels=c("Measure of Local Roots", "Percent Born in State", "Log(Median Income+1)", "Percent with BA", "Percent White",
                                              "MRP District Ideology Estimate", "Democrat", "Seniority", "Majority Party","Member of Power Committees",
                                              "Committee Chair","Member is a Woman", "Same Party Presidential Vote Share"),
                           notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                           label="tab2_fb",
                           digits=3,
                           digits.extra = 0,
                           title="Member Birth Place and Cosponsorship Attraction Controlling for District Level Characteristics")

cat(reg2_table_fb, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg2_fb.tex")

# Staffing with district level demographics

df$distance <- df$binary
reg5_n <- felm(pct_constituencystaff ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll +  dem + seniority + majority + power +chair + female + inpres | congress | 0 | districtID, data=df)

reg5_dist <- felm(pct_constituencystaff ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll +  dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, data=df)

reg5_ideo <- felm(pct_constituencystaff ~ distance + born_instate + logincome + prcntBA + prcntWhiteAll + mrp_ideology + dem + seniority + majority + power +chair + female + inpres  | congress | 0 | districtID, data=df)

df$distance <- df$logged_distance
reg6_n <- felm(pct_constituencystaff ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power +chair + female + inpres | congress  | 0 | districtID, data=df)

reg6_dist <- felm(pct_constituencystaff ~ distance +  born_instate + logincome + prcntBA + prcntWhiteAll + dem + seniority + majority + power +chair + female + inpres | congress + districtID | 0 | districtID, data=df)

reg6_ideo <- felm(pct_constituencystaff ~ distance + born_instate + logincome + prcntBA + prcntWhiteAll + mrp_ideology  +  dem + seniority + majority + power +chair + female + inpres| congress | 0 | districtID, data=df)



reg3_table_fb <- stargazer(reg5_n, reg5_dist,reg5_ideo, reg6_n, reg6_dist,reg6_ideo, 
                           add.lines=list(c("District Fixed Effects","","\\checkmark","","","\\checkmark", ""),
                                          c("Congress Fixed Effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark"),
                                          c("Time Period","1993 to 2014","1993 to 2014","2009-2014","1993 to 2014","1993 to 2014", "2009-2014")),
                           omit=c("Constant"),
                           notes.append = FALSE,notes.label = "",
                           report="vc*s",star.char=c("*","**"),star.cutoffs = c(0.10,0.05),no.space = TRUE,
                           omit.stat = c("rsq", "ser","f"),font.size = "footnotesize", model.numbers = FALSE,
                           column.labels=c("Born in District (0/1)","Log(Miles Born from District + 1)"),column.separate = c(3,3),
                           dep.var.labels = "Percent of Staff Allocated Towards Constituency Service",column.sep.width="0pt",
                           covariate.labels=c("Measure of Local Roots", "Percent Born in State", "Log(Median Income+1)", "Percent with BA", "Percent White",
                                              "MRP District Ideology Estimate", "Democrat", "Seniority", "Majority Party","Member of Power Committees",
                                              "Committee Chair","Member is a Woman", "Same Party Presidential Vote Share"),
                           notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Estimates are from OLS regressions with district-clustered standard errors in parentheses. 
                        The unit of observation is the legislator-Congress. $^{**}p<0.05$, $^*p<0.10$.}",
                           label="tab3_fb",
                           digits=3,
                           digits.extra = 0,
                           title="Local Roots and Percent of Staff that Focus on Constituency Service Controlling for District Level Characteristics")

cat(reg3_table_fb, sep = '\n', file = "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/reg3_fb.tex")


