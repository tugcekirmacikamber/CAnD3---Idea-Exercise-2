*** Analytical sample ****
rename age_group_synth age_group
keep if age_group <= 9
tab age_group // From 19 to 64


*** Rename and recode all the target variables ***

** Condition of dwelling **

tab rpair_synth // 1 - no repair; 2 - minor repair; 3 - major repair
generate con_dwelling = .
replace con_dwelling = 0 if rpair_synth == 1 // 0 No repair is needed
replace con_dwelling = 1 if rpair_synth == 2 | rpair_synth == 3 //1 - Repair is needed
tab con_dwelling
tab rpair_synth

** Immigrant status **
tab immder_synth
gen immigrant = .
replace immigrant = 1 if immder_synth == 1 | immder_synth == 2
replace immigrant = 0 if immder_synth == 3 

tab immigrant // 0 - non-immgrants; 1 - immigrants
tab immder_synth

** Visible minority status **
tab dvismin_synth
gen vis_min = .
replace vis_min = 0 if dvismin_synth == 14
replace vis_min = 1 if dvismin_synth == 1 | dvismin_synth == 2 | dvismin_synth == 3 | dvismin_synth == 4 | dvismin_synth == 5 | dvismin_synth == 6 | dvismin_synth == 7 | dvismin_synth == 8 | dvismin_synth == 9 | dvismin_synth == 10 | dvismin_synth == 11 | dvismin_synth == 12 | dvismin_synth == 13

tab vis_min // 1 - Visible minority; 0 - Non-visible minority

** Parental status **
tab kid_group_synth

gen parent = .
replace parent = 0 if kid_group_synth == 1
replace parent = 1 if kid_group_synth == 2 | kid_group_synth == 3

tab kid_group_synth
tab parent // 1 - Parent; 0 - Non-parent

** Marital status ** 
tab marst_synth
gen marital_status = .
replace marital_status = 0 if marst_synth == 1 | marst_synth == 4 | marst_synth == 5 // Single
replace marital_status = 1 if marst_synth == 2 | marst_synth == 3 // Married

tab marst_synth // 0 = single; 1 - married
tab marital_status

** Gender **
tab sex_synth
gen gender = .
replace gender = 0 if sex_synth == 1 // 0 =  female
replace gender = 1 if sex_synth == 2 // 1 = male 

tab gender // 0 - female; 1 - male
tab sex_synth

** Income **
tab loinca_synth

gen income = .
replace income = 1 if loinca_synth == 1 // non-low income
replace income = 0 if loinca_synth == 2 // low income

tab income // 0 - low incone; 1 - non-low income
tab loinca_synth

** Education **
tab hcdd_synth
gen education = .
replace education = 1 if hcdd_synth == 1 | hcdd_synth == 2 // high school or less
replace education = 2 if hcdd_synth == 3 | hcdd_synth == 4 | hcdd_synth == 5 | hcdd_synth == 6 | hcdd_synth == 7 // trades /college
replace education = 3 if hcdd_synth == 8 | hcdd_synth == 9 // Bachelor degree
replace education = 4 if hcdd_synth == 10 | hcdd_synth == 11 | hcdd_synth == 12 | hcdd_synth == 13 // Graduate

tab education
tab hcdd_synth

*** INTERSECTION # 1 -- Immigrant status X visible minority status ***

gen imm_vis = .
replace imm_vis = 1 if immigrant == 0 & vis_min == 0  // White natives
replace imm_vis = 2 if immigrant == 1 & vis_min == 0 // White immigrants
replace imm_vis = 3 if immigrant == 0 & vis_min == 1 // Non-white natives
replace imm_vis = 4 if immigrant == 1 & vis_min == 1 // Non-white immigrants

tab imm_vis

label define imm_vis_labels ///
1 "White natives" /// 
2 "White immigrants" /// 
3 "Non-white natives" /// 
4 "Non-white immigrants"

label values imm_vis imm_vis_labels

tab imm_vis

*** Regression - Imm status * Visible minority ***

logistic con_dwelling i.imm_vis i.parent i.marital_status i.gender i.income i.education i.age_group
margins imm_vis
marginsplot


*** INTERSECTION # 2 - MARITAL STATUS AND CHILDREN ***

gen child_marriage = .
replace child_marriage = 1 if marital_status == 0 & parent == 0 // Single with no children
replace child_marriage = 2 if marital_status == 1 & parent == 0 // Married with no children
replace child_marriage = 3 if marital_status == 0 & parent == 1 // Single with children
replace child_marriage = 4 if marital_status == 1 & parent == 1 // Married with children

tab child_marriage 

label define child_marriage_labelss ///
1 "Single with no children" /// 
2 "Married with no children" /// 
3 "Single with children" /// 
4 "Married with children"

label values child_marriage child_marriage_labelss

tab child_marriage

*** Regression - Marital status * Parenthood status ***

logistic con_dwelling i.child_marriage i.immigrant i.vis_min i.gender i.income i.education i.age_group
margins child_marriage
marginsplot

*** INTERSECTION # 3 - GENDER and MARITAL STATUS ***

gen gender_marriage = .
replace gender_marriage = 1 if gender == 0 & marital_status == 0 // Single women
replace gender_marriage = 2 if gender == 1 & marital_status == 0 // Single men
replace gender_marriage = 3 if gender == 0 & marital_status == 1 // Married women
replace gender_marriage = 4 if gender == 1 & marital_status == 1 // Married men

tab gender_marriage

label define gender_marriage_label ///
1 "Single women" ///
2 "Single men" ///
3 "Married women" ///
4 "Married men" 

label values gender_marriage gender_marriage_label

tab gender_marriage

*** Regression - Gender * marital status ****

logistic con_dwelling i.gender_marriage i.immigrant i.vis_min i.parent i.income i.education
margins gender_marriage
marginsplot















