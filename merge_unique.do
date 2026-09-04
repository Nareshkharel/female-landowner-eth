clear
capture cd "C:\Users\nk11022\Desktop\ETH_2021_ESPS-W5_v01_M_Stata (new)\Household"
if _rc != 0 {
    display "Primary directory not found. Remaining in the current directory."
}
else {
    display "Primary directory set successfully."
}


use sect1_hh_w5.dta, replace

////   Section 1  //////
/* creating a unique id for the variable
*/


tostring household_id, gen(household_id_str) // household_id is already string
tostring individual_id, gen(individual_id_str)

gen unique_id = household_id + individual_id_str

ssc install distinct

distinct unique_id

save sect1_hh_w5_unique.dta,replace

/*  2 Importing section hh_2    
*/
use sect2_hh_w5.dta, replace

tostring individual_id, gen(individual_id_str)
tostring household_id, gen(household_id_str) 

gen unique_id = household_id + individual_id_str

distinct unique_id

save sect2_hh_w5_unique.dta, replace

/* Using the first dataset as master dataset the data was merged
*/
use sect2_hh_w5_unique.dta, replace
use sect1_hh_w5_unique.dta, replace
merge 1:1 unique_id using sect2_hh_w5_unique.dta

drop if _merge==1

distinct household_id

tab s1q01

///    MERGING 1 2 4 AND 6C DATASETS ///////////////
save merge_hh_12.dta, replace
use merge_hh_12.dta,replace
tab _merge
drop _merge
save merge_hh_12.dta, replace

/* 4 labor and labor use section 
*/
use sect4_hh_w5.dta, replace

describe

tab s4q46


bysort household_id: egen psnp_labour= total(s4q46)


tab psnp_labour


// creating unique identifier ///
tostring household_id, gen(household_id_str) //household_id is already string
tostring individual_id, gen(individual_id_str)

gen unique_id = household_id + individual_id_str

distinct unique_id

save hh_4_w5.dta, replace




/// Merging the dataset 1 and 2 with 4   ///////////
/* use the method abobe to first merge dataset 1 and 2 and then merge it with the saved dataset 4 */

use merge_hh_12.dta, replace

merge 1:1 unique_id using hh_4_w5.dta,force
tab _merge  // all  the observations were merged

drop _merge

distinct household_id

save merged_1_2_4.dta, replace

/* Keeping only the household head dataset
*/
tab s1q01
use merged_1_2_4.dta,replace
keep if s1q01==1

distinct household_id

save merged_hh_1_2_4.dta, replace

/* Now merging for the food insecurity dataset sec8_FIES
*/
use sect8_hh_w5.dta, replace

describe

distinct household_id

/* household_id is not unique for around 800 observations so trying the region,zone,wareds,city, subcity,kebele, ea and household to see if a unique identifier can be generated
*/
foreach var in saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 {
    tostring `var', replace
}

/// The first one is backtick and the secon one is single apostrphi



egen unique_household_id = concat(saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08),p("")

distinct unique_household_id

save hh8_w5.dta,replace


///////   SECOND METHOD FOR GENERATING UNIQUE IDENTIFIER  /////////////////////////////
/*
use merged_hh_1_2_4.dta, replace
foreach var in saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 {
    tostring `var', replace
}
*/
/* Converting to numeric form worked so using the destring function to convert it to numeric form 
*/

use merged_hh_1_2_4.dta, replace

foreach var in saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 {
    destring `var', replace
} 

egen unique_household_id = concat(saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08),p("")

distinct unique_household_id

save merged_1234_concated.dta, replace


/* Merging the merged file section 1 and 2 with the section 8
*/

use merged_1234_concated.dta, replace

distinct household_id
tab s1q21

describe


merge 1:1 unique_household_id using hh8_w5.dta,force

list if _merge==2

drop if _merge==2
drop _merge
save merged_1248.dta, replace

/* List observations where the unique ID matched but the household ID did not
*/
/*list unique_household_id hh8_w5.dta(household_id) merged_1234_concated.dta(household_id) if _merge == 3 & household_id != household_id

TO DO THIS WORK
*/


/* 9 USING THE SECTION 9
*/

use sect9_hh_w5.dta, replace

describe
distinct household_id ///4959

tab s9q01
gen shock_1=1 if s9q01 ==1
replace shock_1=0 if s9q01==2
bysort household_id: egen shock=total(shock_1)

bysort household_id: summarize shock

gen shock_faced =0 if shock==0
replace shock_faced=1 if missing(shock_faced)

duplicates drop household_id, force

distinct household_id

tab shock_faced

label variable shock_faced "shock faced=1 , shock not faced=0"
save hh_9_w5.dta, replace

/* USING SECTION 12
*/
use sect12a_hh_w5.dta, replace
describe

tab s12aq01_1 
gen non_farm_enterprise= 1 if s12aq01_1 ==1
replace non_farm_enterprise = 0 if missing(non_farm_enterprise)
label variable non_farm_enterprise "owned NFE=1, not owned=0"

save hh_12a_w5.dta, replace


/* USING SECTION 14 (ASSISTANCE)
*/
use sect14_hh_w5.dta, replace

describe

tab s14q01

gen assistance = 1 if s14q01==1
replace assistance =0 if missing(assistance)

bysort household_id: egen household_assist = total(assistance)
replace household_assist = 0 if missing(household_assist)

bysort household_id: tab household_assist

gen assistance_received = 0 if household_assist==0
replace assistance_received =1 if missing(assistance_received)


label variable assistance_received "1= assistance received, 0=assistance not received"

tab assistance_received

distinct household_id

/// psnp variable
bysort assistance_cd:tab s14q01

tab s14q01

gen psnp_received = 1 if assistance_cd==1 & s14q01==1
replace psnp_received=0 if missing(psnp_received)

bysort household_id: egen psnp= total(psnp_received)
replace psnp=0 if missing(psnp)

gen psnp_assistance = 0 if psnp==0
replace psnp_assistance = 1 if missing(psnp_assistance)

label variable psnp_assistance "0= not received,  1= received"

///free food variable

gen free_food = 1 if assistance_cd==2 & s14q01==1
replace free_food=0 if missing(free_food)

bysort assistance_cd:tab s14q01
tab free_food

bysort household_id: egen food_help= total(free_food)
replace food_help=0 if missing(food_help)

gen free_food_assist = 0 if food_help==0
replace free_food_assist = 1 if missing(free_food_assist)

describe
tab s14q03

duplicates drop household_id, force

distinct household_id

tab psnp_assistance
tab free_food_assist

label variable free_food_assist "0= not received,  1= received"

tab assistance_received

save hh_14_w5.dta, replace

/* USING SECTION 13 SALES OF ASSETS
*/
use sect13_hh_w5.dta,replace

describe

bysort household_id: egen income_asset_sales= total(s13q02)

gen agric_asset_sales=1 if source_cd== 112 & s13q01==1
replace agric_asset_sales = 0 if missing(agric_asset_sales)

tab agric_asset_sales

bysort household_id: egen sales_asset_agri= total(agric_asset_sales)
replace sales_asset_agri =0 if missing(sales_asset_agri)

gen agriculture_asset_sale = 0 if sales_asset_agri==0
replace agriculture_asset_sale = 1 if missing(agriculture_asset_sale)

label variable agriculture_asset_sale "1= agriculture asset sold,  0 - agriculture asset not sold "	

distinct household_id

duplicates drop household_id, force

save hh_13_w5.dta, replace

/* USING SECTION 12C
only has 363 households so going to use the pp or ph dataset */
use sect12c_hh_w5.dta, replace
describe

distinct household_id

/* Merging dataset 
*/
use merged_1248.dta, replace

distinct household_id

save merged_1248.dta, replace

/// Merging section 9

use hh_9_w5.dta, replace

foreach var in saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 {
    destring `var', replace
} 

egen unique_household_id = concat(saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08),p("")

distinct unique_household_id

save hh_9_w5.dta,replace

use merged_1248.dta, replace

merge 1:1 unique_household_id using hh_9_w5.dta, force

drop if _merge==2

drop _merge
save merged_12489.dta, replace


/* Merging section  12
*/
use hh_12a_w5.dta,replace

foreach var in saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 {
    destring `var', replace
} 

egen unique_household_id = concat(saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08),p("")

distinct unique_household_id

save hh_12a_w5.dta,replace

use merged_12489.dta, replace

merge 1:1 unique_household_id using hh_12a_w5.dta, force
drop if _merge==2
drop _merge

save merged_12489_12a.dta, replace

/* Merging section 13
*/

use hh_13_w5.dta, replace
foreach var in saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 {
    destring `var', replace
} 

egen unique_household_id = concat(saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08),p("")

distinct unique_household_id

save hh_13_w5.dta, replace

use merged_12489_12a.dta, replace 

merge 1:1 unique_household_id using hh_13_w5.dta, force 
drop if _merge==2

drop _merge 
save merged_12489_12a_13.dta, replace


/* merging section  14
*/
use hh_14_w5.dta, replace

foreach var in saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 {
    destring `var', replace
} 

egen unique_household_id = concat(saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08),p("")

distinct unique_household_id

save hh_14_w5.dta, replace

use merged_12489_12a_13.dta, replace 

merge 1:1 unique_household_id using hh_14_w5.dta, force
list if _merge==2

drop if _merge==2

drop _merge 

save merged_12489_12a_13_14.dta, replace

use merged_12489_12a_13_14.dta, replace

describe

tab s2q06

count if missing(s2q06)

/* USING SECTION 11 ASSETS
*/

use sect11_hh_w5.dta, replace
describe

//access to electricity
gen has_electric_asset = 0
replace has_electric_asset = 1 if asset_cd == 3 & s11q00 == 1
replace has_electric_asset = 1 if asset_cd == 9 & s11q00 == 1
replace has_electric_asset = 1 if asset_cd == 21 & s11q00 == 1
replace has_electric_asset = 1 if asset_cd == 37 & s11q00 == 1

bysort household_id: egen access_elctricity= max(has_electric_asset)

tab access_elctricity


// access to water 

gen water_asset = 0

bysort household_id: gen water_access = 1 if asset_cd==34 & s11q00==1


foreach var in saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 {
    destring `var', replace
} 

egen unique_household_id = concat(saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08),p("")

distinct unique_household_id

distinct household_id
duplicates drop household_id, force

save hh_11_w5.dta, replace

/* Merging section 11
*/
use merged_12489_12a_13_14.dta, replace

merge 1:1 unique_household_id using hh_11_w5.dta, force

drop if _merge==2

drop _merge

save merged_hh.dta, replace // 1,2,4,8,9,11,12a,13,14 merged


describe

/* using some variable to run regression
*/

use merged_hh.dta, replace

describe
gen male_head=0
replace male_head=1 if s1q02==1

label variable male_head "1=male, 0=female"
tab s1q02
tab male_head

tab saq14

gen rural=0
replace rural = 1 if saq14==1
tab rural
label variable rural "1= rural, 0=urban"

tab s2q03
gen basic_education=0
replace basic_education=1 if s2q03==1
label variable basic_education "1= person can read/write, 0=person cant read/write"
tab basic_education
describe

tab s4q33b
gen payment_work=0
replace payment_work= 1 if s4q33b==1

tab payment_work
label variable payment_work "1= worked for payment in the last 12 months, 0= not worked for payment in last 12 months"

gen household_size=saq09

tab saq09
tab household_size

describe


tab saq01

tab saq01, gen(region_dummy)

gen Afar= region_dummy1
gen Amhara= region_dummy2
gen Oromia=region_dummy3
gen Somali= region_dummy4
generate Benishangul= region_dummy5
gen SNNP = region_dummy6
gen Gambela = region_dummy7
gen Harar = region_dummy8
gen Addis_ababa= region_dummy9
gen Dire_dawa = region_dummy10

tab saq01
tab Afar 
tab Amhara 
tab Oromia 
tab Somali

tab saq01
tab Benishangul 
tab SNNP 
tab Gambela
tab Harar

tab saq01 
tab Addis_ababa 
tab Dire_dawa



describe

drop region_dummy1-region_dummy10

describe

tab s1q09

gen married=1 if (s1q09==2| s1q09==3)
replace married=0 if missing(married)

tab married

save merged_hh.dta, replace

/* REGRESSION PART
*/

use merged_hh.dta, replace

ssc install outreg2


tab access_elctricity

describe

//Harar is excluded for multicollinearity

logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size shock_faced access_elctricity

vif, uncentered
 
outreg2 using foodsafety.doc, replace ctitle("Model 2")


margins, dydx(*)

outreg2 using marginal.doc, append ctitle("Model 2")

tab Afar
tab s2q03
tab s2q04

summarize household_foodworry 
summarize age_head 
summarize male_head 
summarize Amhara


/* Merging the livestock part
*/
use merged_hh.dta, replace
tab s1q01

merge 1:1 unique_household_id using merged_ls_1_2.dta, force 

drop if _merge==2

drop _merge

save merged_hh_livestock.dta, replace

use merged_hh_livestock.dta, replace

gen livestock_only=1 if saq15==2
replace livestock_only=0 if missing(livestock_only)

save merged_hh_livestock.dta, replace

use merged_hh_livestock.dta, replace

logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_received agriculture_asset_sale non_farm_enterprise payment_work household_size shock_faced access_elctricity livestock_only

outreg2 using foodsafety.doc, append ctitle("Model 1")

vif, uncentered

/* PSNP labour participation
*/

drop if missing(household_foodworry)

save merged_hh_livestock.dta, replace
use merged_hh_livestock.dta, replace

tab psnp_labour

logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size shock_faced access_elctricity livestock_only married

outreg2 using foodsafety.doc, replace ctitle("Coefficient estimate")

* Store the number of parameters (including intercept)
local k = e(df_m) + 1

* Store the log likelihood
local logL = e(ll)

* Store the number of observations
local n = e(N)

* Calculate AIC
local AIC = 2 * `k' - 2 * `logL'

* Display the AIC
display "AIC: " `AIC'

* Calculate BIC
local BIC = `k' * log(`n') - 2 * `logL'

* Display the BIC
display "BIC: " `BIC'

/* Marginal effect
*/
margins, dydx(*) post

outreg2 using foodsafety.doc, append ctitle("marginal effect")


quietly logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size shock_faced access_elctricity livestock_only married
estat classification


/* regression without using shock effect */
use merged_hh_livestock.dta, replace

logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size access_elctricity livestock_only married

outreg2 using foodsafety_lessshcok.doc, replace ctitle("Coefficient estimate")

/* Calculating AIC
*/

* Store the number of parameters (including intercept)
local k = e(df_m) + 1

* Store the log likelihood
local logL = e(ll)

* Store the number of observations
local n = e(N)

* Calculate AIC
local AIC = 2 * `k' - 2 * `logL'

* Display the AIC
display "AIC: " `AIC'

* Calculate BIC
local BIC = `k' * log(`n') - 2 * `logL'

* Display the BIC
display "BIC: " `BIC'

/// Marginal effect

margins, dydx(*) post

outreg2 using foodsafety_lessshcok.doc, append ctitle("marginal effect")

quietly logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size access_elctricity livestock_only married
estat classification

/* Descriptive statistics  
*/
* Install estout package
ssc install estout
ssc install outreg2

* Generate summary statistics and export to an Excel file
estpost summarize household_foodworry age_head male_head Amhara Harar Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size shock_faced access_elctricity livestock_only married


esttab using summary_stats.csv, cells("mean sd min max") replace

/* Using the probit method for regression for without shock model
*/

/* regression without using shock effect */
use merged_hh_livestock.dta, replace

ssc install outreg2
probit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size access_elctricity livestock_only married

outreg2 using foodsafety_probit.doc, replace ctitle("Coefficient estimate")

/* Calculating AIC
*/

* Store the number of parameters (including intercept)
local k = e(df_m) + 1

* Store the log likelihood
local logL = e(ll)

* Store the number of observations
local n = e(N)

* Calculate AIC
local AIC = 2 * `k' - 2 * `logL'

* Display the AIC
display "AIC: " `AIC'

* Calculate BIC
local BIC = `k' * log(`n') - 2 * `logL'

* Display the BIC
display "BIC: " `BIC'

/// Marginal effect

margins, dydx(*) post

outreg2 using foodsafety_probit.doc, append ctitle("marginal effect")

quietly logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size access_elctricity livestock_only married
estat classification


/* PSNp labour is actually the temporary labour. So Psnp received may also be included in the model */

use merged_hh_livestock.dta, replace

logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size access_elctricity livestock_only married psnp_received

outreg2 using foodsafety_psnpreceived.doc, replace ctitle("coefficient estimate")

/* Calculating AIC
*/

* Store the number of parameters (including intercept)
local k = e(df_m) + 1

* Store the log likelihood
local logL = e(ll)

* Store the number of observations
local n = e(N)

* Calculate AIC
local AIC = 2 * `k' - 2 * `logL'

* Display the AIC
display "AIC: " `AIC'

* Calculate BIC
local BIC = `k' * log(`n') - 2 * `logL'

* Display the BIC
display "BIC: " `BIC'

/// Marginal effect

margins, dydx(*) post

outreg2 using foodsafety_psnpreceived.doc, append ctitle("marginal effect")

quietly logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size access_elctricity livestock_only married psnp_received
estat classification

// Removing the outliers
use merged_hh_livestock.dta, replace

tab age_head

drop if age_head < 18

count if age_head > 85

drop if age_head >90

ssc install outreg2

logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size access_elctricity livestock_only married

outreg2 using foodsafety_noutliers.doc, replace ctitle("Coefficient estimate") 

/* Calculating AIC
*/

* Store the number of parameters (including intercept)
local k = e(df_m) + 1

* Store the log likelihood
local logL = e(ll)

* Store the number of observations
local n = e(N)

* Calculate AIC
local AIC = 2 * `k' - 2 * `logL'

* Display the AIC
display "AIC: " `AIC'

* Calculate BIC
local BIC = `k' * log(`n') - 2 * `logL'

* Display the BIC
display "BIC: " `BIC'

/// Marginal effect

margins, dydx(*) post

outreg2 using foodsafety_noutliers.doc, append ctitle("marginal effect")

quietly logit household_foodworry age_head male_head Amhara Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size access_elctricity livestock_only married
estat classification

/* Descriptive statistics  
*/
* Install estout package
ssc install estout
ssc install outreg2

* Generate summary statistics and export to an Excel file
estpost summarize household_foodworry age_head male_head Amhara Harar Oromia Somali Benishangul SNNP Gambela Afar Addis_ababa Dire_dawa basic_education psnp_labour agriculture_asset_sale non_farm_enterprise payment_work household_size access_elctricity livestock_only married


esttab using summary_stats.csv, cells("mean sd min max") replace



