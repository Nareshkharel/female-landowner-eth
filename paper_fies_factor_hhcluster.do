********************************************************************************
* paper_fies_factor_hhcluster.do
*
* Robustness check from the paper code:
*   factor worried-hungry, mineigen(1)
*   predict pc1
*   regress pc1 ... , vce(robust)
*
* This file keeps that unweighted regression (to match fies_factorpaper.txt)
* and adds the same models with:
*   svyset hh_cluster [pweight=pw_w5]
* i.e. survey weight and errors clustered at the household.
*
* Sample: the paper N = 1,826 file (paper_sample_1826.dta), or the same
* merges as paper_fies_1826_easyform.do.
*
* Run from the Stata Command window:
*   do "C:\full\path\to\paper_fies_factor_hhcluster.do"
********************************************************************************

clear all
set more off
capture log close
log using "paper_fies_factor_hhcluster.log", replace text

global eth_root "C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"
global analysis_dta ""

capture confirm file "$analysis_dta"
if _rc {
    capture confirm file "$eth_root/paper_sample_1826.dta"
    if !_rc global analysis_dta "$eth_root/paper_sample_1826.dta"
}
capture confirm file "$analysis_dta"
if _rc {
    capture confirm file "paper_sample_1826.dta"
    if !_rc global analysis_dta "paper_sample_1826.dta"
}

capture confirm file "$analysis_dta"
if !_rc {
    display as text "Loading saved sample: $analysis_dta"
    use "$analysis_dta", clear
}
else {
    display as text "No saved sample. Building it from: $eth_root"
    capture confirm file "$eth_root/merged_w_fies.dta"
    if _rc {
        display as error "Cannot find merged_w_fies.dta. Edit eth_root in this do-file."
        exit 601
    }
    use "$eth_root/merged_w_fies.dta", clear
    merge 1:1 household_id using "$eth_root/hh_9_w5.dta",     keep(match) nogenerate
    merge 1:1 household_id using "$eth_root/hh_12a_w5.dta",   keep(match) nogenerate
    merge 1:1 household_id using "$eth_root/hh_14.dta",       keep(match) nogenerate
    merge 1:1 household_id using "$eth_root/hh11_w5_pca.dta", keep(match) nogenerate
    merge 1:1 household_id using "$eth_root/fies_dta.dta",    keep(match) nogenerate
    capture confirm file "$eth_root/Household/female_ownership.dta"
    if !_rc {
        merge 1:1 household_id using "$eth_root/Household/female_ownership.dta", ///
            keepusing(female_landowner sole_female_ownership joint_ownership ///
                      soil_fertility farm_type) keep(match) nogenerate
    }
    else {
        merge 1:1 household_id using "$eth_root/female_ownership.dta", ///
            keepusing(female_landowner sole_female_ownership joint_ownership ///
                      soil_fertility farm_type) keep(match) nogenerate
    }
    capture confirm file "$eth_root/Household_geographical.dta"
    if !_rc {
        merge 1:1 household_id using "$eth_root/Household_geographical.dta", ///
            keepusing(dist_admhq dist_road) keep(match) nogenerate
    }
    else {
        merge 1:1 household_id using "$eth_root/household_geographical.dta", ///
            keepusing(dist_admhq dist_road) keep(match) nogenerate
    }
    merge 1:1 household_id using "$eth_root/hdds_ethiopia.dta", keep(match) nogenerate
    merge 1:1 household_id using "$eth_root/area_ethiopia.dta", ///
        keepusing(sfi total_land) keep(match) nogenerate
    merge 1:1 household_id using "$eth_root/agri_practices.dta", keep(match) nogenerate

    capture confirm numeric variable fies_score
    if _rc {
        gen fies_score = worried + healthy + fewfoods + skipped ///
            + ateless + wholeday + ranout + hungry
    }
    drop if missing(fies_score) | fies_score > 8
    capture confirm numeric variable livestock_hh
    if _rc gen livestock_hh = (farm_type == 1 | farm_type == 3) if !missing(farm_type)
}

capture confirm numeric variable fies_dummy
if _rc gen fies_dummy = (fies_score >= 4) if !missing(fies_score)
capture confirm numeric variable severe_fi
if _rc gen severe_fi = (fies_score > 6) if !missing(fies_score)
capture confirm numeric variable livestock_hh
if _rc gen livestock_hh = (farm_type == 1 | farm_type == 3) if !missing(farm_type)

global x_paper sfi age basic_educ male_head dependency_ratio ///
    non_farm_enterprise wealth_index soil_fertility drought_shock ///
    married total_land livestock_hh

foreach v in $x_paper female_landowner sole_female_ownership joint_ownership saq01 {
    capture confirm variable `v'
    if !_rc drop if missing(`v')
}

count
if r(N) != 1826 display as error "Expected N = 1,826, got " r(N)
else            display as text "Paper sample N = 1,826"

capture confirm numeric variable pw_w5
if _rc {
    display as error "pw_w5 not found."
    exit 111
}

* Household cluster: one observation per household
capture drop hh_cluster
egen hh_cluster = group(household_id)

********************************************************************************
* FIES factor score (same as the uploaded paper robustness check)
********************************************************************************

display as text _n "===== FIES factor: worried-hungry, mineigen(1) ====="
factor worried healthy fewfoods skipped ateless wholeday ranout hungry, mineigen(1)
estat kmo
capture drop pc1
predict pc1
summarize pc1
label var pc1 "FIES principal-factor score (higher = more insecure)"

********************************************************************************
* Unweighted robust OLS  (should match fies_factorpaper.txt)
********************************************************************************

display as text _n "===== Unweighted robust OLS (uploaded paper tables) ====="
regress pc1 female_landowner $x_paper i.saq01, vce(robust)
estimates store fac_unw_a
regress pc1 sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store fac_unw_b
test sole_female_ownership = joint_ownership

********************************************************************************
* Survey weight + household cluster
* Three specs, matching the latest FI / Sev FI tables:
*   (1) ownership only
*   (2) household controls, no region
*   (3) household controls + i.saq01
********************************************************************************

display as text _n "===== Survey-weighted, household-clustered OLS ====="
svyset hh_cluster [pweight=pw_w5], singleunit(centered)

display as text _n "Spec A (1): ownership only"
svy: regress pc1 female_landowner
estimates store fac_svy_a1

display as text _n "Spec A (2): household controls, no region"
svy: regress pc1 female_landowner $x_paper
estimates store fac_svy_a2

display as text _n "Spec A (3): household controls + region"
svy: regress pc1 female_landowner $x_paper i.saq01
estimates store fac_svy_a3

display as text _n "Spec B (1): ownership only"
svy: regress pc1 sole_female_ownership joint_ownership
estimates store fac_svy_b1
test sole_female_ownership = joint_ownership

display as text _n "Spec B (2): household controls, no region"
svy: regress pc1 sole_female_ownership joint_ownership $x_paper
estimates store fac_svy_b2
test sole_female_ownership = joint_ownership

display as text _n "Spec B (3): household controls + region"
svy: regress pc1 sole_female_ownership joint_ownership $x_paper i.saq01
estimates store fac_svy_b3
test sole_female_ownership = joint_ownership

display as text _n "===== Ownership coefficients ====="
estimates table fac_unw_a fac_svy_a1 fac_svy_a2 fac_svy_a3, ///
    keep(female_landowner) star stats(N r2)
estimates table fac_unw_b fac_svy_b1 fac_svy_b2 fac_svy_b3, ///
    keep(sole_female_ownership joint_ownership) star stats(N r2)

log close
display as text "Log: paper_fies_factor_hhcluster.log"
