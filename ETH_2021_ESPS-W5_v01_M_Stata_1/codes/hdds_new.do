
clear
cd"/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1"

use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect6c_hh_w5.dta"


gen staples = 1 if (s6cq03 ==1 |s6cq04 ==1 | s6cq15 ==1 | s6cq27  ==1 |s6cq33  ==1 )

gen pulses_nuts = 1 if (s6cq06 ==1 | s6cq25 ==1 )

gen vegetables = 1 if (s6cq07 ==1 |s6cq08 == 1|s6cq09 ==1 |  s6cq10 ==1)

gen oils_fats = 1 if (s6cq28 ==1 | s6cq26  ==1 | s6cq22 ==1)

gen meat_poultry =1 if (s6cq20 == 1| s6cq21 == 1 | s6cq22 ==1 | s6cq23 == 1)

gen fruits = 1 if (s6cq11 ==1| s6cq12==1 | s6cq13 ==1 | s6cq14 == 1|s6cq31 ==1)

gen roots_tubers = 1 if (s6cq05 ==1| s6cq28 == 1 | s6cq26 ==1)

gen eggs = 1 if (s6cq17 ==1)

gen fish = 1 if (s6cq24 ==1)

gen milk_milkprod = 1 if (s6cq29 ==1 | s6cq19 ==1 | s6cq18 ==1)

gen sugar_honey = 1 if (s6cq32  ==1 |s6cq16 ==1| s6cq30 ==1)

gen miscellaneous = 1 if (s6cq28 ==1)

foreach var of varlist staples-miscellaneous {
    replace `var' = 0 if missing(`var')
}


gen ind_hdds = staples + pulses_nuts + vegetables  + oils_fats + meat_poultry +  fruits + roots_tubers + eggs  + fish + milk_milkprod + sugar_honey + miscellaneous

tab ind_hdds

bysort household_id: egen hdds_household = mean(ind_hdds)

tab hdds_household

duplicates drop household_id, force

save hdds_ethiopia.dta, replace
