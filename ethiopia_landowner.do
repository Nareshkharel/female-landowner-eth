clear
cd"/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household"


//// parcel data  ///
use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Post_planting/sect2_pp_w5.dta"

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

save parcel.dta, replace



///// s2q12_1 and s2q12_2  are land owners

use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect1_hh_w5.dta", replace

keep household_id individual_id s1q02 s1q01 s1q03a  

joinby household_id using parcel.dta

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
merge 1:1 household_id using "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Post_planting/hh_landarea.dta"

drop if _merge <3
drop _merge

save female_ownership.dta, replace



///// sfi 

cd"/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1"
use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Post_planting/sect3_pp_w5.dta", replace


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


save area_ethiopia.dta, replace


/// improved agriculture practices
cd"/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1"
use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Post_planting/sect7_pp_w5.dta", replace

bysort household_id: egen crop_rotation = min(s7q01)
replace crop_rotation = 0 if crop_rotation > 1 & crop_rotation != .


bysort household_id: egen chemical_fertilizer = min(s7q02)
replace chemical_fertilizer= 0 if chemical_fertilizer > 1 & chemical_fertilizer!= .

bysort household_id: egen watershed_participation = min(s7q29 )
replace watershed_participation = 0 if watershed_participation> 1 & chemical_fertilizer!= .


duplicates drop household_id, force

keep household_id chemical_fertilizer crop_rotation watershed_participation

save agri_practices.dta, replace


// improved seeds use

use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Post_planting/sect5_pp_w5.dta", replace


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
save improved_maize.dta, replace





