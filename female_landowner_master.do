*===============================================================================
* female_landowner_master.do
*
* Single, self-contained replacement for:
*     ethiopia_landowner.do        (Part 1 below)
*     female landowner analysis.do (Part 2 and Part 3 below)
*
* Running this one file reproduces exactly the same intermediate .dta files and
* the same regression output as running those two files in sequence. Every
* command from the two originals is kept verbatim and in its original order.
* The only edits are (a) hard-coded paths replaced by the $root global and
* (b) `ssc install` wrapped in `capture` so a re-run does not stop on
* "already installed".
*
* Added on top of the original analysis:
*     Part 0  ea_id and pw_w5 are captured up front and merged back at the end,
*             so they always survive into the analysis file.
*     Part 4  Every model re-estimated with the pw_w5 sampling weight and
*             standard errors clustered at the enumeration area (ea_id).
*     Part 5  Formal tests of whether the disaggregated ownership model
*             (sole_female_ownership + joint_ownership) outperforms the pooled
*             model (female_landowner).
*
* BEFORE RUNNING
*   1. Set $root below to the folder that holds the ESPS wave 5 data.
*   2. The path must not contain spaces (Stata `cd`/`use` handle them, but the
*      original scripts assume a space-free path).
*   3. These datasets are produced by other scripts and must already exist:
*        Post_planting/hh_landarea.dta
*        hh11_w5_pca.dta            <- Pca_wealthindex.do
*        fies_dta.dta               <- FIES/Rasch step (fies_rasch.R)
*        household_geographical.dta
*        hdds_ethiopia.dta
*      The prerequisite check in Part 0 stops with a clear message if any of
*      them is missing.
*===============================================================================

clear all
set more off

global root "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1"
global hh   "$root/Household"
global pp   "$root/Post_planting"

* Fail here, rather than several commands later, if $root is not set correctly.
capture cd "$root"
if _rc {
    display as error "Cannot open the data folder: $root"
    display as error "Edit the \$root global at the top of this do-file."
    exit 170
}

capture log close _all
log using "$root/female_landowner_master.log", replace text

* Community commands used below. `capture` keeps a re-run from stopping on
* "already installed", and keeps a machine with no internet from stopping here.
capture ssc install outreg2
capture ssc install factortest
capture ssc install distinct

* Without these the run would die part-way through, after writing some tables.
foreach cmd in outreg2 factortest distinct {
    capture which `cmd'
    if _rc {
        display as error "Required community command `cmd' is not installed."
        display as error "Install it on a machine with internet: ssc install `cmd'"
        exit 199
    }
}


*===============================================================================
* PART 0. PREREQUISITES AND SURVEY DESIGN VARIABLES
*===============================================================================

*-------------------------------------------------------------------------------
* 0.1 Stop early, with a readable message, if an input file is missing.
*-------------------------------------------------------------------------------
local req1 "$pp/hh_landarea.dta"
local req2 "$root/hh11_w5_pca.dta"
local req3 "$root/fies_dta.dta"
local req4 "$root/household_geographical.dta"
local req5 "$root/hdds_ethiopia.dta"

local nmiss = 0
forvalues k = 1/5 {
    capture confirm file "`req`k''"
    if _rc {
        display as error "MISSING PREREQUISITE FILE: `req`k''"
        local nmiss = `nmiss' + 1
    }
}
if `nmiss' > 0 {
    display as error ""
    display as error "`nmiss' prerequisite file(s) not found. Build them first,"
    display as error "or correct the \$root global at the top of this do-file."
    exit 601
}

*-------------------------------------------------------------------------------
* 0.2 Household-level survey design file: ea_id (PSU) and pw_w5 (weight).
*
* These are pulled out now and merged back onto the analysis file at the end of
* Part 2. The merge pipeline below drops variables by range (e.g.
* `drop s1q05-s1q37`), which depends on variable order and can silently remove
* ea_id or pw_w5. Capturing them separately makes them safe from that.
*-------------------------------------------------------------------------------
local src1 "$hh/sect_cover_hh_w5.dta"
local src2 "$hh/sect1_hh_w5.dta"
local src3 "$pp/sect_cover_pp_w5.dta"
local src4 "$hh/sect2_hh_w5.dta"

local designdone = 0
forvalues k = 1/4 {
    if `designdone' continue
    capture confirm file "`src`k''"
    if _rc continue

    quietly use "`src`k''", clear
    capture confirm variable ea_id
    if _rc continue
    capture confirm variable pw_w5
    if _rc continue

    keep household_id ea_id pw_w5
    quietly duplicates drop household_id, force
    label variable ea_id "enumeration area (PSU) identifier"
    label variable pw_w5 "household sampling weight, ESPS wave 5"
    save "$root/survey_design.dta", replace
    display as text "Survey design variables taken from: `src`k''"
    local designdone = 1
}
if `designdone' == 0 {
    display as error "Could not find a source file containing both ea_id and pw_w5."
    display as error "Add the correct file to the src1-src4 list in Part 0.2."
    exit 111
}


*===============================================================================
* PART 1. LAND OWNERSHIP, LAND AREA AND AGRICULTURAL PRACTICES
*         (was: ethiopia_landowner.do)
*===============================================================================

clear
cd "$hh"


//// parcel data  ///
use "$pp/sect2_pp_w5.dta"

describe

drop if s2q01c==2

gen farm_type = saq15

label variable farm_type "1=crop only, 2 = livestock only , 3 = both, 4 = none"


keep household_id parcel_id s2q01c s2q05 s2q06 s2q07_1 s2q07_2 s2q04b_1 s2q04b_2 s2q04b_3 s2q04b_4 s2q17 s2q07_1 s2q07_2 s2q06 farm_type

bysort household_id: egen number_plots = count(parcel_id)


gen soil_fertile = 1 if s2q17== 3
replace soil_fertile =2 if s2q17== 2
replace soil_fertile =3 if s2q17== 1

tab soil_fertile

bysort household_id: egen soil_fertility = mean(soil_fertile)
tab parcel_id

save "$hh/parcel.dta", replace



///// s2q12_1 and s2q12_2  are land owners

use "$hh/sect1_hh_w5.dta", replace

keep household_id individual_id s1q02 s1q01 s1q03a  

joinby household_id using "$hh/parcel.dta"

distinct household_id

tab parcel_id

/* generating variable for female land ownership */

tostring individual_id, generate(individual_id_str)
tostring parcel_id, generate(parcel_id_str)



egen unique_id = concat(household_id individual_id_str parcel_id_str), p("_")
egen unique_parcel = concat(household_id parcel_id_str), p("_")

distinct unique_parcel

duplicates drop unique_id, force

drop individual_id_str parcel_id_str unique_id

/////////////////////////////////////
 
 
 

gen indfemale_landowner = 0   
foreach var in s2q04b_1 s2q04b_2 s2q04b_3 s2q04b_4    {
    by household_id: replace indfemale_landowner = 1 if `var' == individual_id  & s1q02  == 2
}

bysort household_id: egen total_female_owned = sum(indfemale_landowner)
gen number_plots_female = total_female_owned

gen female_landowner = 1 if total_female_owned >0
replace female_landowner = 0 if missing(female_landowner)



tab female_landowner

drop s1q01 s1q03a 
label variable female_landowner "at least one female in household owns land"
label variable  number_plots_female "number of plots owned by female in household"



/////// joint female landowner and sole female landowner ///////

* Step 1: Generating a temporary variable to track male and female ownership for each household
gen male_owner = 0
gen female_owner = 0


* Step 2: Looping through each land ownership variable

foreach var in s2q04b_1 s2q04b_2 s2q04b_3 s2q04b_4 {
    * Loop through all individuals in the household
     foreach i in individual_id {
        * Check if the current value matches any individual_id and the individual is male
       replace male_owner = 1 if `var' == `i' & s1q02[_n] == 1
        
        * Check if the current value matches any individual_id and the individual is female
        replace female_owner = 1 if `var' == `i' & s1q02[_n] == 2
    }
}

///// for sole and joint ownership
bysort household_id: egen male_owner1= max(male_owner)
bysort household_id: egen female_owner1= max(female_owner)

tab female_owner1

* Step 3: Creating the joint land ownership variable
gen joint_land_ownership = (male_owner1 == 1 & female_owner1 == 1)
bysort household_id: egen joint_ownership = max (joint_land_ownership)

gen sole_female_landowner = (male_owner1 == 0 & female_owner1 == 1)
bysort household_id: egen sole_female_ownership = min(sole_female_landowner)

gen sole_male_landowner = (male_owner1== 1 & female_owner1 == 0)
bysort household_id: egen sole_male_ownership = min(sole_male_landowner)

////// female land ownership calculated earlier is good to go

* Step 4: Cleaning up temporary variables
drop male_owner female_owner indfemale_landowner  total_female_owned  male_owner1 female_owner1 sole_female_landowner sole_male_landowner joint_land_ownership 



distinct unique_parcel

duplicates drop unique_parcel, force


tab s2q05 if sole_female_ownership== 1
tab s2q05 if joint_ownership==1
tab s2q05 if sole_male_ownership==1

tab s2q05


label variable sole_female_ownership "1= female is sole owner of all plots, 0=otherwise"
label variable joint_ownership "1= plots are jointly owned, 0=otherwise"

duplicates drop household_id, force

///merging for land area
merge 1:1 household_id using "$pp/hh_landarea.dta"

drop if _merge <3
drop _merge

save "$hh/female_ownership.dta", replace



///// sfi 

cd "$root"
use "$pp/sect3_pp_w5.dta", replace


keep household_id-s3q08 s3q34   

count if missing(s3q08)

describe


gen plot_area_ha = s3q08 * 1 / 10000


bysort household_id:egen sum_area_numerator = sum(plot_area_ha^2)

bysort household_id:egen sum_denominator = sum(plot_area_ha)
bysort household_id:gen sum_denominator_squared = sum_denominator^2


bysort household_id: gen sfi = 1- sum_area_numerator / sum_denominator_squared

bysort household_id: egen total_land = sum(plot_area_ha)


duplicates drop household_id, force

keep household_id sfi total_land legume_crop
label variable total_land "land area of household in ha"


save "$root/area_ethiopia.dta", replace


/// improved agriculture practices
cd "$root"
use "$pp/sect7_pp_w5.dta", replace

bysort household_id: egen crop_rotation = min(s7q01)
replace crop_rotation = 0 if crop_rotation > 1 & crop_rotation != .


bysort household_id: egen chemical_fertilizer = min(s7q02)
replace chemical_fertilizer= 0 if chemical_fertilizer > 1 & chemical_fertilizer!= .

bysort household_id: egen watershed_participation = min(s7q29 )
replace watershed_participation = 0 if watershed_participation> 1 & chemical_fertilizer!= .


duplicates drop household_id, force

keep household_id chemical_fertilizer crop_rotation watershed_participation

save "$root/agri_practices.dta", replace


// improved seeds use

use "$pp/sect5_pp_w5.dta", replace


bysort household_id: gen improved_maize = 1 if s5q0B == 2 & s5q01a == 2
bysort household_id: replace improved_maize = 0 if s5q0B == 2 & s5q01a == 1

tab improved_maize

replace improved_maize = . if missing(improved_maize) 

tab improved_maize

bysort household_id: egen improved_seed = max(s5q01a)
replace improved_seed = 0 if missing(improved_seed)
tab improved_seed

replace improved_seed = 0 if improved_seed == 1
replace improved_seed = 1 if improved_seed == 2

label variable improved_seed "1= use improved seed for at least one crop, 0= use traditional seed"

duplicates drop household_id, force

keep household_id improved_maize improved_seed
save "$root/improved_maize.dta", replace


*===============================================================================
* PART 2. HOUSEHOLD CHARACTERISTICS, SHOCKS, ASSISTANCE AND THE FINAL MERGE
*         (was: female landowner analysis.do, up to the merges)
*===============================================================================

clear

cd "$root"

use "$hh/sect1_hh_w5.dta"

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


merge 1:1 household_id individual_id using "$hh/sect2_hh_w5.dta"

gen basic_educ = s2q03
replace basic_educ = 0 if basic_educ == 2


gen school_attended = s2q04 
replace school_attended = 0 if school_attended == 2
drop if _merge == 1
drop _merge

//// merging the labour data

merge 1:1 household_id individual_id using "$hh/sect4_hh_w5.dta"
drop _merge


bysort household_id: egen psnp_labour= total(s4q46)

keep if head == 1 

distinct household_id

// merging with fies dataset

merge 1:1 household_id using "$hh/sect8_hh_w5.dta"

drop if _merge == 2
drop _merge

drop s1q05-s1q37
drop s2q07-s2q19
drop s4q02-s4q54
drop s2q00-s2q06
drop s8q01-s8q08a

save "$root/merged_w_fies.dta", replace


///////////////
use "$hh/sect9_hh_w5.dta", replace

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
save "$root/hh_9_w5.dta", replace

///// section 12 = NFE
use "$hh/sect12a_hh_w5.dta", replace
describe

tab s12aq01_1 
gen non_farm_enterprise= 1 if s12aq01_1 ==1
replace non_farm_enterprise = 0 if missing(non_farm_enterprise)
label variable non_farm_enterprise "owned NFE=1, not owned=0"

keep household_id non_farm_enterprise 

save "$root/hh_12a_w5.dta", replace

/* USING SECTION 14 (ASSISTANCE)
*/

use "$hh/sect14_hh_w5.dta", replace

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

save "$root/hh_14.dta", replace



////  //     merging     ///////


use "$root/merged_w_fies.dta", replace

merge 1:1 household_id using "$root/hh_9_w5.dta"
drop if _merge < 3
drop _merge


merge 1:1 household_id using "$root/hh_12a_w5.dta"
drop if _merge < 3
drop _merge


merge 1:1 household_id using "$root/hh_14.dta"

drop if _merge < 3
drop _merge

merge 1:1 household_id using "$root/hh11_w5_pca.dta"

drop if _merge < 3
drop _merge

merge 1:1 household_id using "$root/fies_dta.dta"

drop if _merge < 3
drop _merge

gen fies_score = worried + healthy + fewfoods + skipped + ateless + wholeday + ranout + hungry
tab fies_score

merge 1:1 household_id using "$hh/female_ownership.dta"
drop if _merge < 3
drop _merge

merge 1:1 household_id using "$root/household_geographical.dta"
drop if _merge < 3
drop _merge

merge 1:1 household_id using "$root/hdds_ethiopia.dta"
drop if _merge < 3
drop _merge

merge 1:1 household_id using "$root/area_ethiopia.dta"

drop if _merge < 3
drop _merge

merge 1:1 household_id using "$root/agri_practices.dta"
drop if _merge < 3
drop _merge

merge 1:1 household_id using "$root/improved_maize.dta"

drop _merge



destring saq06, generate(kebele)
drop s6cq00-s6cq33 


*-------------------------------------------------------------------------------
* 2.1 Restore the survey design variables (ea_id, pw_w5).
*
* `keep(master match)` adds no observations and removes none, so every model in
* Part 3 runs on exactly the sample it ran on before.
*-------------------------------------------------------------------------------
capture drop ea_id
capture drop pw_w5
merge 1:1 household_id using "$root/survey_design.dta", keep(master match) nogenerate

quietly count if missing(ea_id)
display as text "Households with missing ea_id: " as result r(N)
quietly count if missing(pw_w5)
display as text "Households with missing pw_w5: " as result r(N)


*===============================================================================
* PART 3. ORIGINAL REGRESSIONS  (unchanged, unweighted, vce(robust))
*         (was: female landowner analysis.do, regression section)
*===============================================================================

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
capture ssc install factortest


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


* The analysis file, with ea_id and pw_w5 retained. `order` runs here rather
* than at the merge so it cannot disturb the variable-order ranges that Part 3
* relies on (worried-hungry, s6cq00-s6cq33).
order household_id ea_id pw_w5
save "$root/female_landowner_analysis.dta", replace


*===============================================================================
* PART 4. SURVEY-WEIGHTED MODELS, ERRORS CLUSTERED AT THE ENUMERATION AREA
*
* svyset declares ea_id as the sampling unit and pw_w5 as the probability
* weight, so every `svy:` estimate below is weighted by pw_w5 and its standard
* errors are clustered at ea_id. This is equivalent to
*     logit y x [pw=pw_w5], vce(cluster ea_id)
* but it also enables the design-based Wald tests used in Part 5.
*
* Point estimates change relative to Part 3 (they are now population-weighted);
* standard errors change because clustering at ea_id accounts for the
* correlation between households in the same enumeration area.
*
* Output goes to *_svy.doc so the Part 3 tables are not overwritten.
*===============================================================================

*-------------------------------------------------------------------------------
* 4.1 Cluster identifier.
*
* ea_id is stored as a string in the ESPS files and svyset needs a numeric
* sampling unit, so cluster on a numeric recoding of it. `egen group()` gives
* one value per distinct ea_id, so the clusters are exactly the enumeration
* areas. ea_id itself stays in the data.
*-------------------------------------------------------------------------------
capture drop ea_cluster
capture confirm variable ea_id
if _rc {
    display as error "ea_id is not in the data, so Part 4 and Part 5 cannot run."
    exit 111
}

capture confirm string variable ea_id
local ea_is_string = (_rc == 0)
if `ea_is_string' {
    egen ea_cluster = group(ea_id)
}
else {
    gen ea_cluster = ea_id
}
label variable ea_cluster "enumeration area, numeric (clustering unit)"

quietly count if missing(pw_w5) | missing(ea_cluster)
local ndropped = r(N)
display as text "Observations dropped from the weighted models: " as result `ndropped'

quietly count if pw_w5 <= 0 & !missing(pw_w5)
local nbadweight = r(N)
if `nbadweight' > 0 {
    display as error "`nbadweight' observations have a non-positive pw_w5; svyset will reject them."
}

quietly count if !missing(pw_w5)
local nweighted = r(N)
if `nweighted' == 0 {
    display as error "pw_w5 is missing for every household, so Part 4 and Part 5 cannot run."
    exit 2000
}

svyset ea_cluster [pweight = pw_w5]
svydescribe

global XLOGIT "sfi age basic_educ male_head dependency_ratio non_farm_enterprise wealth_index dist_admhq dist_road soil_fertility drought_shock i.saq01"
global XFIES  "sfi age basic_educ male_head dependency_ratio non_farm_enterprise wealth_index dist_admhq dist_road soil_fertility drought_shock married i.saq01"


//////////////////// agriculture practices ////////////////////

svy: logit chemical_fertilizer female_landowner $XLOGIT
outreg2 using ownership_practices3_svy.doc, replace ctitle("Chemical fertlizer use") dec(3)
margins, dydx(*) post
outreg2 using marginal_practices3_svy.doc, replace ctitle("Chemical fertilizer use") dec(3)

svy: logit crop_rotation female_landowner $XLOGIT
outreg2 using ownership_practices3_svy.doc, append ctitle("Crop rotation") dec(3)
margins, dydx(*) post
outreg2 using marginal_practices3_svy.doc, append ctitle("Crop rotation") dec(3)

svy: logit improved_maize female_landowner $XLOGIT
outreg2 using ownership_practices3_svy.doc, append ctitle("improved maize variety") dec(3)
margins, dydx(*) post
outreg2 using marginal_practices3_svy.doc, append ctitle("Improved maize variety") dec(3)


/////// with sole and joint ownership  (ag practices)

svy: logit chemical_fertilizer sole_female_ownership joint_ownership $XLOGIT
outreg2 using agri_practices3_svy.doc, replace ctitle("Synthetic fertlizer") dec(3)
margins, dydx(*) post
outreg2 using marginal_agripractices3_svy.doc, replace ctitle("Synthetic fertlizer") dec(3)

svy: logit crop_rotation sole_female_ownership joint_ownership $XLOGIT
outreg2 using agri_practices3_svy.doc, append ctitle("Crop rotation") dec(3)
margins, dydx(*) post
outreg2 using marginal_agripractices3_svy.doc, append ctitle("Crop rotation") dec(3)

svy: logit improved_maize sole_female_ownership joint_ownership $XLOGIT
outreg2 using agri_practices3_svy.doc, append ctitle("improved maize") dec(3)
margins, dydx(*) post
outreg2 using marginal_agripractices3_svy.doc, append ctitle("Improved maize variety") dec(3)


/////////////// FIES ////////////////

svy: regress fies_score female_landowner $XFIES
outreg2 using food_security_svy.doc, replace ctitle("FIES")

svy: regress fies_score sole_female_ownership joint_ownership $XFIES
outreg2 using food_security_svy.doc, append ctitle("FIES")

svy: logit fies_dummy female_landowner $XLOGIT
outreg2 using logit_fies3_svy.doc, replace ctitle("FIES") dec(3)
margins, dydx(*) post
outreg2 using marginal_logitfies3_svy.doc, replace ctitle("FIES") dec(3)

svy: logit fies_dummy sole_female_ownership joint_ownership $XLOGIT
outreg2 using logit_fies3_svy.doc, append ctitle("FIES") dec(3)
margins, dydx(*) post
outreg2 using marginal_logitfies3_svy.doc, append ctitle("FIES") dec(3)


////////// severe FI ////

svy: logit severe_fi female_landowner $XLOGIT
outreg2 using logit_fies3_svy.doc, append ctitle("SFI") dec(3)
margins, dydx(*) post
outreg2 using marginal_sfi3_svy.doc, replace ctitle("SFI") dec(3)

svy: logit severe_fi sole_female_ownership joint_ownership $XLOGIT
outreg2 using logit_fies3_svy.doc, append ctitle("SFI") dec(3)
margins, dydx(*) post
outreg2 using marginal_sfi3_svy.doc, append ctitle("SFI") dec(3)


//////////   PCA robustness /////////

svy: regress pc1 female_landowner $XLOGIT
outreg2 using fies_factor3_svy.doc, replace ctitle("FIES_factor") dec(3)

svy: regress pc1 sole_female_ownership joint_ownership $XLOGIT
outreg2 using fies_factor3_svy.doc, append ctitle("FIES_factor") dec(3)

svy: regress hdds_household sfi age age_sq basic_educ male_head dependency_ratio non_farm_enterprise wealth_index female_landowner soil_fertility shock_faced married total_land livestock_hh dist_admhq dist_road i.saq01
outreg2 using hdds_svy.doc, replace ctitle("HDDS") dec(3)


*===============================================================================
* PART 5. WHICH OWNERSHIP SPECIFICATION PERFORMS BETTER?
*
* Model A (pooled):        y = b1*female_landowner + controls
* Model B (disaggregated): y = b1*sole_female_ownership + b2*joint_ownership
*                              + controls
*
* female_landowner equals sole_female_ownership + joint_ownership: a household
* with a female landowner is either the sole owner or a joint owner, and the two
* categories are mutually exclusive. Model A is therefore Model B with the
* restriction b1 = b2 imposed, which makes the two models NESTED and lets us
* test them against each other directly.
*
* The tests reported for each outcome:
*   TEST 1  Adjusted Wald test of b(sole) = b(joint), under the survey design.
*           This is the headline test. Rejecting it means the restriction that
*           Model A imposes is false, so Model B is the better specification.
*   TEST 1b lincom of the sole-joint gap: how large the difference is, in which
*           direction, and with what confidence interval.
*   TEST 2  Joint significance of the two ownership terms in Model B.
*   TEST 3  AIC / BIC for both models. Lower is better.
*   TEST 4  Likelihood-ratio test of A against B.
*   TEST 5  For binary outcomes, a DeLong test comparing the two ROC areas,
*           i.e. whether Model B actually classifies households better.
*           For continuous outcomes, adjusted R-squared.
*
* TEST 1, 1b and 2 use the weighted, ea_cluster-clustered design from Part 4.
* TEST 3 to TEST 5 are likelihood- and prediction-based, and neither the
* likelihood nor AIC/BIC is defined for pseudo-likelihood (weighted)
* estimation, so those are computed unweighted. TEST 3 and TEST 5 still cluster
* at ea_cluster; TEST 4 requires plain maximum likelihood, so it is unclustered.
*===============================================================================

capture program drop fl_horserace
program define fl_horserace
    syntax varname(numeric), ESTimator(string) Controls(string) [ TItle(string) ]

    local y "`varlist'"
    if `"`title'"' == "" local title "`y'"

    display as text _n(2) "{hline 79}"
    display as result `"MODEL COMPARISON: `title'   (outcome: `y')"'
    display as text "{hline 79}"

    * Confirm the nesting that the tests below rely on.
    quietly count if !missing(female_landowner, sole_female_ownership, joint_ownership) ///
        & female_landowner != (sole_female_ownership + joint_ownership)
    local nmismatch = r(N)
    display as text "Households where female_landowner != sole + joint: " as result `nmismatch'
    if `nmismatch' > 0 {
        display as error "  The two specifications are not exactly nested;"
        display as error "  read TEST 1 and TEST 4 as approximate."
    }

    * Stored under per-outcome names so that after the run you can line the two
    * specifications up side by side, e.g. estimates table A_fies_dummy B_fies_dummy
    display as text _n "--- Model A: pooled female_landowner (weighted, clustered) ---"
    svy: `estimator' `y' female_landowner `controls'
    estimates store A_`y'

    display as text _n "--- Model B: sole + joint ownership (weighted, clustered) ---"
    svy: `estimator' `y' sole_female_ownership joint_ownership `controls'
    estimates store B_`y'

    display as text _n "TEST 1  Adjusted Wald test, H0: b(sole) = b(joint)"
    display as text "        p < 0.05 favours Model B; otherwise Model A is adequate."
    test sole_female_ownership = joint_ownership

    display as text _n "TEST 1b Size and direction of the gap between the two"
    lincom sole_female_ownership - joint_ownership
    if _b[sole_female_ownership] > _b[joint_ownership] {
        display as text "        Sole female ownership has the larger coefficient."
    }
    else {
        display as text "        Joint ownership has the larger coefficient."
    }

    display as text _n "TEST 2  Joint significance of the ownership terms in Model B"
    testparm sole_female_ownership joint_ownership

    display as text _n "TEST 3  AIC / BIC (unweighted, clustered at ea_cluster). Lower is better."
    quietly `estimator' `y' female_landowner `controls', vce(cluster ea_cluster)
    estimates store icA
    quietly `estimator' `y' sole_female_ownership joint_ownership `controls', vce(cluster ea_cluster)
    estimates store icB
    capture noisily estimates stats icA icB
    local rc = _rc
    if `rc' display as error "        Information criteria unavailable (rc = `rc')."

    display as text _n "TEST 4  Likelihood-ratio test of Model A against Model B"
    quietly `estimator' `y' female_landowner `controls'
    estimates store lrA
    quietly `estimator' `y' sole_female_ownership joint_ownership `controls'
    estimates store lrB
    capture noisily lrtest lrA lrB
    local rc = _rc
    if `rc' display as error "        Likelihood-ratio test unavailable (rc = `rc')."

    if "`estimator'" == "logit" {
        display as text _n "TEST 5  ROC areas, Model A vs Model B (DeLong test of equality)"
        capture drop _phatA
        capture drop _phatB
        quietly logit `y' female_landowner `controls', vce(cluster ea_cluster)
        quietly predict double _phatA if e(sample), pr
        quietly logit `y' sole_female_ownership joint_ownership `controls', vce(cluster ea_cluster)
        quietly predict double _phatB if e(sample), pr
        capture noisily roccomp `y' _phatA _phatB
        local rc = _rc
        if `rc' display as error "        roccomp unavailable (rc = `rc')."
        capture drop _phatA
        capture drop _phatB
    }
    else {
        display as text _n "TEST 5  Adjusted R-squared (unweighted, clustered at ea_cluster)"
        quietly regress `y' female_landowner `controls', vce(cluster ea_cluster)
        display as text "        Model A adjusted R2 = " as result %9.4f e(r2_a)
        quietly regress `y' sole_female_ownership joint_ownership `controls', vce(cluster ea_cluster)
        display as text "        Model B adjusted R2 = " as result %9.4f e(r2_a)
    }
end


* Controls are passed unquoted so `syntax` receives the bare variable list.
fl_horserace chemical_fertilizer, estimator(logit)   controls($XLOGIT) title(Synthetic fertilizer use)
fl_horserace crop_rotation,       estimator(logit)   controls($XLOGIT) title(Crop rotation)
fl_horserace improved_maize,      estimator(logit)   controls($XLOGIT) title(Improved maize variety)
fl_horserace fies_dummy,          estimator(logit)   controls($XLOGIT) title(Moderate or severe food insecurity)
fl_horserace severe_fi,           estimator(logit)   controls($XLOGIT) title(Severe food insecurity)
fl_horserace fies_score,          estimator(regress) controls($XFIES)  title(FIES raw score)
fl_horserace pc1,                 estimator(regress) controls($XLOGIT) title(FIES factor score)


display as text _n(2) "{hline 79}"
display as result "HOW TO READ PART 5"
display as text "{hline 79}"
display as text "TEST 1 is the decisive one. Model A forces sole and joint female"
display as text "ownership to have the same effect. If TEST 1 rejects that restriction,"
display as text "the pooled female_landowner model is misspecified and the sole/joint"
display as text "model should be reported; TEST 1b then says which of the two forms of"
display as text "ownership carries the larger effect. If TEST 1 does not reject, the"
display as text "pooled model is the more parsimonious choice, and TEST 3 (lower AIC/BIC)"
display as text "and TEST 5 (no gain in ROC area) should agree with that conclusion."

log close
