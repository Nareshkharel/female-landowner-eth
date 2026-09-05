
clear

use "/Users/nareshkharel/Desktop/Thesis/data/TZA_2020_NPS-R5_v02_M_STATA14/Household/hh_sec_j3.dta", replace

/*
local dummies "Cereals Roots_tubers Vegetables Fruits Meat_poultry_offal Eggs Fish_seafood Pulses_legumes_nuts Milk_products Oil_fats Sugar_honey Miscellaneous"

foreach varname of local dummies {
    generate `varname' = 0
}

*/

describe
tab itemcode
rename itemcode item_cd
rename hh_j01 s6aq01


/* now we give the values to the dummy variables
*/
//1. Cereals
bysort y5_hhid: gen staples = (item_cd==0101 & s6aq01==1 | item_cd == 0102 & s6aq01==1 | item_cd==0103 & s6aq01==1 | item_cd==0104 & s6aq01==1 | item_cd==0105 & s6aq01==1 | item_cd==0106 & s6aq01==1 | item_cd==0107 & s6aq01==1 | item_cd==01081 & s6aq01==1 | item_cd==01082 & s6aq01==1 |item_cd==0109 & s6aq01==1| item_cd==0110 & s6aq01==1 |item_cd==0111 & s6aq01==1 |item_cd==0112 & s6aq01==1 | item_cd==0201 & s6aq01==1 | item_cd == 0202 & s6aq01==1 | item_cd==0203 & s6aq01==1 | item_cd==0204 & s6aq01==1 | item_cd==0205 & s6aq01==1 | item_cd==0206 & s6aq01==1 | item_cd==0207 & s6aq01==1)


bysort y5_hhid: egen main_staples = max(staple)


//2. pulses and nuts

bysort y5_hhid: gen pulses_nuts = (item_cd==0401 & s6aq01==1 | item_cd==0501 & s6aq01==1 | item_cd == 0502 & s6aq01==1 | item_cd==0503 & s6aq01==1 | item_cd==0504 & s6aq01==1)

bysort y5_hhid: egen pulses = max(pulses_nuts)


// 3. vegetables

 gen vegetables_consumed = (item_cd==0601 & s6aq01==1 | item_cd == 0602 & s6aq01==1 | item_cd==0603 & s6aq01==1 )

 bysort y5_hhid: egen vegetables = max(vegetables_consumed)

 tab vegetables


//3.  oils and fats

gen oils_fats = (item_cd==301 & s6aq01==1 | item_cd == 302 & s6aq01==1 | item_cd==303 & s6aq01==1 | item_cd==304 & s6aq01==1 | item_cd==305 & s6aq01==1 | item_cd==708 & s6aq01==1)

bysort household_id: egen oils_fats_cons = max(oils_fats)

tab oils_fats_cons

// 7. meat and fish

 gen meat_fishcons = (item_cd==0801 & s6aq01==1 | item_cd == 0802 & s6aq01==1 | item_cd==0803 & s6aq01==1| item_cd==0804 & s6aq01==1 | item_cd == 0805 & s6aq01==1 | item_cd==0806 & s6aq01==1 | item_cd==0807 & s6aq01==1 | item_cd==0808 & s6aq01==1 | item_cd==0809 & s6aq01==1 | item_cd==0810 & s6aq01==1 )
 
bysort y5_hhid: egen meat_fish = max(meat_fishcons)
tab meat_fish

tab item_cd if s6aq01==1

tab item_cd

distinct y5_hhid



 
 // 5. fruits
 gen fruits = (item_cd==501 & s6aq01==1 | item_cd == 502 & s6aq01==1 | item_cd==503 & s6aq01==1 | item_cd==504 & s6aq01==1 | item_cd==505 & s6aq01==1 | item_cd==506 & s6aq01==1)
 
bysort household_id: egen fruits_consumed = max(fruits)
tab fruits_consumed
 
 // 6.  roots and tubers
 
gen roots_tubers = (item_cd==601 & s6aq01==1 | item_cd == 602 & s6aq01==1 | item_cd==603 & s6aq01==1 | item_cd==604 & s6aq01==1 | item_cd==605 & s6aq01==1 | item_cd==606 & s6aq01==1 | item_cd==607 & s6aq01==1 | item_cd==608 & s6aq01==1 | item_cd==609 & s6aq01==1 |item_cd==610 & s6aq01==1 )

bysort household_id: egen roots_tubers_cons = max(roots_tubers)

tab roots_tubers_cons


//8.  Eggs

gen eggs = (item_cd== 709 & s6aq01==1)

bysort household_id: egen egg_consumed = max(eggs)

tab egg_consumed

//9. Fish and seafood
gen fish_seafood = (item_cd==704 & s6aq01==1)

bysort household_id: egen fish_consumed = max(fish_seafood)

tab fish_consumed  //ethiopia is land_locked so no other seafood

//10. milk and milk products

gen milk_milkprod = (item_cd==705 & s6aq01==1 | item_cd == 706 & s6aq01==1 | item_cd==707 & s6aq01==1 )

bysort household_id: egen milk_products_consumed = max(milk_milkprod)

tab milk_products_consumed

//11. sugar and honey

gen sugar_honey = (item_cd==710 & s6aq01==1 | item_cd == 711 & s6aq01==1)
bysort household_id: egen sugar_honey_consumed = max(sugar_honey)
tab sugar_honey_consumed

//12. miscellaneous

gen miscellaneous = (item_cd==801 & s6aq01==1 | item_cd == 802 & s6aq01==1 | item_cd==803 & s6aq01==1| item_cd==804 & s6aq01==1 | item_cd == 806 & s6aq01==1 | item_cd==807 & s6aq01==1 | item_cd== 904 & s6aq01==1 )

bysort household_id: egen miscellaneous_consumed = max(miscellaneous)

tab miscellaneous_consumed

/* constructing HDDS score
*/
gen hdds_score = cereals_consumed + pulse_nuts_cons + oils_fats_cons + veg_consumed + fruits_consumed + roots_tubers_cons + meat_poultry_consumed + egg_consumed + fish_consumed + milk_products_consumed + sugar_honey_consumed + miscellaneous_consumed

tab hdds_score

ssc install distinct
distinct household_id

duplicates drop household_id, force

tab hdds_score

save hdds_household.dta, replace

use hdds_household.dta, replace
distinct household_id

/* Using the fies data
*/
use sect8_hh_w5.dta, replace
distinct household_id

describe

gen worried=1 if s8q01==1
gen healthy=1 if s8q02==1
gen fewfoods=1 if s8q03==1
gen skipped=1 if s8q04==1
gen ateless=1 if s8q05==1
gen wholeday=1 if s8q06==1
gen ranout=1 if s8q07==1
gen hungry=1 if s8q08==1

replace worried = 0 if missing(worried)
replace healthy = 0 if missing(healthy)
replace fewfoods = 0 if missing(fewfoods)
replace skipped = 0 if missing(skipped)
replace ateless = 0 if missing(ateless)
replace wholeday = 0 if missing(wholeday)
replace ranout = 0 if missing(ranout)
replace hungry= 0 if missing(hungry)

replace worried = . if missing(s8q01)
replace healthy = . if missing(s8q02)
replace fewfoods = . if missing(s8q03)
replace skipped = . if missing(s8q04)
replace ateless = . if missing(s8q05)
replace wholeday = . if missing(s8q06)
replace ranout = . if missing(s8q07)
replace hungry= . if missing(s8q08)


summarize worried healthy fewfoods skipped ateless wholeday ranout hungry 


tab s8q08


egen fies_score = rowtotal(worried healthy fewfoods skipped ateless wholeday ranout hungry) 

tab fies_score


merge 1:1 household_id using hdds_household.dta

reg hdds_score fies_score

correlate hdds_score fies_score

save hdds_fies.dta, replace

drop _merge

