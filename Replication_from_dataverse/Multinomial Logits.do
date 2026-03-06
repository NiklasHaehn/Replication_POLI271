
use "~/Dropbox/Local Roots/Replication Package New/Data/Dataset_ForMainAnalysis.dta", clear

label var dem "Democrat"
label var seniority "Seniority"
label var majority "Majority"
label var power "Member of Powerful Committees"
label var chair "Committee Chair"
label var female "Member is a Woman"
label var inpres "Same Party Presidential Vote Share"
label var style3 "Dependent Variable: Representational Style"

**************
* Table 1
**************

gen distance = binary 
label var distance "Measure of Local Roots"
mlogit style3  distance  dem seniority  majority  power chair female inpres i.congress, cluster(districtID)
eststo mlogit1a

drop distance 
gen distance = logged_distance
label var distance "Measure of Local Roots"
mlogit style3  distance  dem seniority  majority  power chair female inpres i.congress, cluster(districtID) 
eststo mlogit1b


estadd local hasdist "\multicolumn{2}{c}\checkmark": mlogit1a mlogit1b
local numbers "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}\begin{tabular}{l*{4}{c}}\hline\hline& \multicolumn{2}{c}{Born in District (0/1)} & \multicolumn{2}{c}{Log(Miles Born from the District + 1)}\\ \cline{2-5} \\"
local numbers2 "\hline Congress Indicators & \checkmark  & \checkmark & \checkmark  & \checkmark \\ Observations & 4273  & 4273  & 4273  & 4273\\ Pseudo R-squared &   0.109  &   0.109  &  0.108   &  0.108 \\"


esttab mlogit1a mlogit1b using "~/Dropbox/Local Roots/Replication Package New/Paper/Table1.tex", ///
 nonumber noobs replace prehead("`numbers'") prefoot("`numbers2'") nobaselevels se label drop(_cons *.congress) noomitted ///
 sfmt(3 0) star(* 0.10 ** 0.05) unstack b(3) nogaps 

  
 
**************
*Table 5
**************

drop distance
gen distance = binary 
label var distance "Measure of Local Roots"
mlogit style3  distance  dem seniority  majority  power chair female inpres i.congress, cluster(districtID) baseoutcome(2)
eststo mlogit2a

drop distance 
gen distance = logged_distance
label var distance "Measure of Local Roots"
mlogit style3  distance  dem seniority  majority  power chair female inpres i.congress, cluster(districtID) baseoutcome(2)
eststo mlogit2b


local numbers "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}\begin{tabular}{@{\extracolsep{0pt}}lcc} \\[-1.8ex]\hline \hline \\[-1.8ex]  &  \multicolumn{2}{c}{\textit{Dependent variable:}} \\ \cline{2-3} \\[-1.8ex] & \multicolumn{2}{c}{Policy v. District Focus} \\ \\[-1.8ex] & Born in District (0/1) & Log(Miles Born from District + 1)  \\ \hline \\[-1.8ex] \\"
local numbers2 "\hline Congress Indicators & \checkmark  & \checkmark  \\ Observations & 4273 & 4273\\"


esttab mlogit2a mlogit2b using "~/Dropbox/Local Roots/Replication Package New/Supplementary Materials/mlogit2.tex", ///
 nonumber replace prehead("`numbers'") prefoot("`numbers2'") nobaselevels se label nomtitle drop(_cons *.congress) noomitted ///
 noobs sfmt(3 0) star(* 0.10 ** 0.05) b(3) nogaps
