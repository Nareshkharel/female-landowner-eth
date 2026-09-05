clear

cd"/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1"

use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect1_hh_w5.dta"

gen age = s1q03a  
gen male = 1 if s1q02 == 1
replace male = 0 if missing(male)
gen head =1 if s1q01  == 1

gen male_head = 1 if head == 1 & male == 1
replace male_head = 0 if missing(male_head)

label variable male "1= male, 0= female"

gen married = 1 if (s1q09 ==2 | s1q09 == 3)
replace married = 0 if missing(married)

label variable married "married = 1 , not married =0"

bysort household_id: egen household_size = count(individual_id)

tab household_size

gen child = 1 if age < 15
gen old = 1 if age > 64
replace child = 0 if missing(child) 
replace old = 0 if missing(old)
gen working_age = 1 if (age > 14 & age < 65)


bysort household_id : egen dependent = sum(child + old)
bysort household_id: egen independent = sum(working_age)

gen dependency_ratio = dependent/ independent


label variable dependency_ratio "dr = dependent / independent"


merge 1:1 household_id individual_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect2_hh_w5.dta"

gen basic_educ = s2q03
replace basic_educ = 0 if basic_educ == 2


gen school_attended = s2q04 
replace school_attended = 0 if school_attended == 2
drop if _merge == 1
drop _merge

//// merging the labour data

merge 1:1 household_id individual_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect4_hh_w5.dta"
drop _merge


bysort household_id: egen psnp_labour= total(s4q46)

keep if head == 1 

distinct household_id

// merging with fies dataset

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect8_hh_w5.dta"

drop if _merge == 2
drop _merge

drop s1q05-s1q37
drop s2q07-s2q19
drop s4q02-s4q54
drop s2q00-s2q06
drop s8q01-s8q08a

save merged_w_fies.dta, replace

///////////////
use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect9_hh_w5.dta", replace

gen drought = 1 if shock_type ==6 & s9q01==1
replace drought = 0 if missing(drought)
tab drought

bysort household_id: egen drought_shock = max(drought)

gen shock_1=1 if s9q01 ==1
replace shock_1=0 if s9q01==2
bysort household_id: egen shock=total(shock_1)


gen shock_faced =0 if shock==0
replace shock_faced=1 if missing(shock_faced)

duplicates drop household_id, force

distinct household_id

tab shock_faced

label variable shock_faced "shock faced=1 , shock not faced=0"
keep household_id shock_faced drought_shock
save hh_9_w5.dta, replace

///// section 12 = NFE
use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect12a_hh_w5.dta", replace
describe

tab s12aq01_1 
gen non_farm_enterprise= 1 if s12aq01_1 ==1
replace non_farm_enterprise = 0 if missing(non_farm_enterprise)
label variable non_farm_enterprise "owned NFE=1, not owned=0"

keep household_id non_farm_enterprise 

save hh_12a_w5.dta, replace

/* USING SECTION 14 (ASSISTANCE)
*/

use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect14_hh_w5.dta", replace

gen assistance = 1 if s14q01==1
replace assistance =0 if missing(assistance)

bysort household_id: egen household_assist = total(assistance)
replace household_assist = 0 if missing(household_assist)


gen assistance_received = 0 if household_assist==0
replace assistance_received =1 if missing(assistance_received)


label variable assistance_received "1= assistance received, 0=assistance not received"

tab assistance_received

////PSNP variable
gen psnp_received = 1 if assistance_cd==1 & s14q01==1
replace psnp_received=0 if missing(psnp_received)

bysort household_id: egen psnp= total(psnp_received)
replace psnp=0 if missing(psnp)

gen psnp_assistance = 0 if psnp==0
replace psnp_assistance = 1 if missing(psnp_assistance)

label variable psnp_assistance "0= not received,  1= received"

/// free food
gen free_food = 1 if assistance_cd==2 & s14q01==1
replace free_food=0 if missing(free_food)


bysort household_id: egen food_help= total(free_food)
replace food_help=0 if missing(food_help)

gen free_food_assist = 0 if food_help==0
replace free_food_assist = 1 if missing(free_food_assist)

duplicates drop household_id, force

save hh_14.dta, replace



////  //     merging     ///////


use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/merged_w_fies.dta", replace

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/hh_9_w5.dta"
drop if _merge < 3
drop _merge


merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/hh_12a_w5.dta"
drop if _merge < 3
drop _merge


merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/hh_14.dta"

drop if _merge < 3
drop _merge

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/hh11_w5_pca.dta"

drop if _merge < 3
drop _merge

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/fies_dta.dta"

drop if _merge < 3
drop _merge

gen fies_score = worried + healthy + fewfoods + skipped + ateless + wholeday + ranout + hungry
tab fies_score

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/female_ownership.dta"
drop if _merge < 3
drop _merge

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/household_geographical.dta"
drop if _merge < 3
drop _merge

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/hdds_ethiopia.dta"
drop if _merge < 3
drop _merge

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/area_ethiopia.dta"

drop if _merge < 3
drop _merge

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/agri_practices.dta"
drop if _merge < 3
drop _merge

merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/improved_maize.dta"

drop _merge



destring saq06, generate(kebele)
drop s6cq00-s6cq33 





/////////////////////////////////////////////////////////////////////////
///           Regression study
//////////////////// agriculture practices. ////////


drop if fies_score >8
drop if missing(dependency_ratio)
drop if missing(soil_fertility)
drop if missing(sfi)

list if missing(chemical_fertilizer) & (farm_type == 1 | farm_type == 3)

logit chemical_fertilizer female_landowner sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)

outreg2 using ownership_practices3.doc, replace ctitle("Chemical fertlizer use") dec(3)
margins, dydx (*) post
outreg2 using marginal_practices3.doc, replace ctitle("Chemical fertilizer use") dec(3)



logit crop_rotation female_landowner sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)
outreg2 using ownership_practices3.doc, append ctitle("Crop rotation") dec(3)
margins, dydx (*) post
outreg2 using marginal_practices3.doc, append ctitle("Crop rotation") dec(3)


logit improved_maize female_landowner sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)
outreg2 using ownership_practices3.doc, append ctitle("improved maize variety") dec(3)
margins, dydx (*) post
outreg2 using marginal_practices3.doc, append ctitle("Improved maize variety") dec(3)




/////// with sole and joint ownership  (ag practices)

logit chemical_fertilizer sole_female_ownership joint_ownership sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)

outreg2 using agri_practices3.doc, replace ctitle("Synthetic fertlizer") dec(3)
margins, dydx (*) post
outreg2 using marginal_agripractices3.doc, replace ctitle("Synthetic fertlizer") dec(3)


logit crop_rotation sole_female_ownership joint_ownership sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)

outreg2 using agri_practices3.doc, append ctitle("Crop rotation") dec(3)
margins, dydx (*) post
outreg2 using marginal_agripractices3.doc, append ctitle("Crop rotation") dec(3)

logit improved_maize sole_female_ownership joint_ownership sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)
outreg2 using agri_practices3.doc, append ctitle("improved maize") dec(3)
margins, dydx (*) post
outreg2 using marginal_agripractices3.doc, append ctitle("Improved maize variety") dec(3)





/////////////// FIES ////////////////

drop if fies_score >8
gen fies_category = 1 if fies_score <4
replace fies_category = 3 if fies_score >6
replace fies_category = 2 if fies_score > 3 & fies_score < 7

gen fies_dummy = 1 if fies_category >1
replace fies_dummy=0 if missing(fies_dummy)

reg fies_score female_landowner sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock married i.saq01, vce(robust)

outreg2 using food_security.doc, replace ctitle("FIES")


////////// logit model for food security /////


logit fies_dummy female_landowner sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)

outreg2 using logit_fies3.doc, replace ctitle ("FIES") dec(3)
margins, dydx (*) post
outreg2 using marginal_logitfies3.doc, replace ctitle("FIES") dec(3)

//// joint and sole ownership 

logit fies_dummy sole_female_ownership joint_ownership sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)

outreg2 using logit_fies3.doc, append ctitle ("FIES") dec(3)

margins, dydx (*) post
outreg2 using marginal_logitfies3.doc, append ctitle("FIES") dec(3)

////////// severe FI ////

gen severe_fi = 1 if fies_score >6
replace severe_fi=0 if missing(severe_fi)
gen age_sq = age^2

tab fies_category

logit severe_fi female_landowner sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)

outreg2 using logit_fies3.doc, append ctitle("SFI") dec(3)
margins, dydx (*) post
outreg2 using marginal_sfi3.doc, replace ctitle("SFI") dec(3)


logit severe_fi sole_female_ownership joint_ownership sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)

outreg2 using logit_fies3.doc, append ctitle("SFI") dec(3)
margins, dydx (*) post
outreg2 using marginal_sfi3.doc, append ctitle("SFI") dec(3)








///////////////////////////  robustness test   ///////
//////////   PCA /////////
ssc install factortest


global xlist worried-hungry
corr $xlist


factor $xlist, mineigen(1)
screeplot, yline(1)

predict pc1

estat kmo

//// kmo = 0.899. so sufficient 

factortest $xlist

summarize pc1

reg pc1 female_landowner sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)
outreg2 using fies_factor3.doc, replace ctitle("FIES_factor") dec(3)

reg pc1 sole_female_ownership joint_ownership sfi age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq dist_road  soil_fertility drought_shock i.saq01, vce(robust)

outreg2 using fies_factor3.doc, append ctitle("FIES_factor") dec(3)

reg hdds_household sfi age age_sq basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index female_landowner soil_fertility shock_faced married total_land livestock_hh dist_admhq dist_road i.saq01, vce(robust)





////// descriptive statistics ///

foreach i of varlist worried-hungry {
    count if `i' == 1
}

/// summary stats ///
foreach i of var chemical_fertilizer improved_maize crop_rotation age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq female_landowner dist_road  soil_fertility drought_shock {
	summarize `i'
}

outreg2 using summary_stats_new.doc, replace sum(log) keep(chemical_fertilizer improved_maize crop_rotation age basic_educ male_head dependency_ratio non_farm_enterprise  wealth_index dist_admhq female_landowner dist_road  soil_fertility drought_shock sfi joint_ownership sole_female_ownership sole_male_ownership)








