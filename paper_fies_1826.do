********************************************************************************
* Paper food-security sample (N = 1,826) and all models on that sample
* Ethiopia ESPS Wave 5
*
* This is the specification in:
*   ETH_2021_ESPS-W5_v01_M_Stata_1/codes/paper code/merge_ownership.do
*   spec_paper.txt / specpaper_coef.txt / sample_output_food.docx
*
* What it does
*   1. Builds the 1,826-household sample (roster dependency ratio).
*   2. Summary statistics (should match summary_paper.txt).
*   3. Unweighted robust logits (paper table): Spec A any female owner,
*      Spec B sole vs joint, for moderate/severe FI and severe FI.
*      After every Spec B: Wald test H0: sole = joint, and lincom.
*      After every logit: margins, dydx(*).
*   4. Unweighted robust OLS on the FIES score (same controls).
*   5. Survey-weighted, EA-clustered versions of the same models
*      (pw_w5, cluster ea_id) on the households that have a weight.
*
* How to run (from the repository root, in Stata)
*   do paper_fies_1826.do
*
* Change eth_root if the ETH folder is somewhere else.
********************************************************************************

clear all
set more off
capture log close
log using "paper_fies_1826.log", replace text

* Folder that holds merged_w_fies.dta, female_ownership.dta, area_ethiopia.dta, ...
global eth_root "ETH_2021_ESPS-W5_v01_M_Stata_1"

capture confirm file "$eth_root/merged_w_fies.dta"
if _rc {
    display as error "Cannot find $eth_root/merged_w_fies.dta"
    display as error "Set eth_root to the ETH_2021_ESPS-W5_v01_M_Stata_1 folder."
    exit 601
}


*------------------------------------------------------------------------------
* Sample: same inner merges and drops as the paper do-file
*------------------------------------------------------------------------------
use "$eth_root/merged_w_fies.dta", clear

merge 1:1 household_id using "$eth_root/hh_9_w5.dta",     keep(match) nogenerate
merge 1:1 household_id using "$eth_root/hh_12a_w5.dta",   keep(match) nogenerate
merge 1:1 household_id using "$eth_root/hh_14.dta",       keep(match) nogenerate
merge 1:1 household_id using "$eth_root/hh11_w5_pca.dta", keep(match) nogenerate
merge 1:1 household_id using "$eth_root/fies_dta.dta",    keep(match) nogenerate

merge 1:1 household_id using "$eth_root/Household/female_ownership.dta", ///
    keepusing(female_landowner sole_female_ownership joint_ownership ///
              sole_male_ownership soil_fertility farm_type) ///
    keep(match) nogenerate

capture confirm file "$eth_root/Household_geographical.dta"
if !_rc {
    merge 1:1 household_id using "$eth_root/Household_geographical.dta", ///
        keepusing(dist_admhq dist_road) keep(match) nogenerate
}
else {
    merge 1:1 household_id using "$eth_root/household_geographical.dta", ///
        keepusing(dist_admhq dist_road) keep(match) nogenerate
}

merge 1:1 household_id using "$eth_root/hdds_ethiopia.dta",  keep(match) nogenerate
merge 1:1 household_id using "$eth_root/area_ethiopia.dta", ///
    keepusing(sfi total_land) keep(match) nogenerate
merge 1:1 household_id using "$eth_root/agri_practices.dta", keep(match) nogenerate

capture confirm numeric variable fies_score
if _rc {
    gen fies_score = worried + healthy + fewfoods + skipped ///
                   + ateless + wholeday + ranout + hungry
}

drop if missing(fies_score) | fies_score > 8
drop if missing(dependency_ratio)
drop if missing(soil_fertility)
drop if missing(sfi)

gen fies_dummy = (fies_score >= 4) if !missing(fies_score)
gen severe_fi  = (fies_score > 6)  if !missing(fies_score)
* Paper coding: crop-only or mixed (farm_type 1 or 3), not livestock-only.
gen livestock_hh = (farm_type == 1 | farm_type == 3) if !missing(farm_type)

label var fies_dummy   "1 = moderate or severe FI (FIES >= 4)"
label var severe_fi    "1 = severe FI (FIES > 6)"
label var livestock_hh "1 = farm_type crop-only or mixed (paper coding)"

* Same regressors as spec_paper (no distances).
global x_paper sfi age basic_educ male_head dependency_ratio ///
    non_farm_enterprise wealth_index soil_fertility drought_shock ///
    married total_land livestock_hh

foreach v in $x_paper female_landowner sole_female_ownership joint_ownership saq01 {
    drop if missing(`v')
}

count
if r(N) != 1826 {
    display as error "Expected N = 1,826, got " r(N)
}
else {
    display as text "Paper sample N = 1,826"
}

capture confirm numeric variable ea_id
if !_rc {
    egen psu = group(ea_id)
}
else {
    egen psu = group(saq01 saq02 saq03 saq04 saq05 saq06)
}

global wt ""
foreach v in pw_w5 svwt {
    capture confirm numeric variable `v'
    if !_rc & "$wt" == "" global wt `v'
}


*------------------------------------------------------------------------------
* 1. Summary statistics (summary_paper.txt)
*------------------------------------------------------------------------------
display as text _n "SUMMARY (target: summary_paper.txt)"
summarize fies_dummy severe_fi sfi age basic_educ male_head ///
    dependency_ratio non_farm_enterprise wealth_index female_landowner ///
    soil_fertility drought_shock married total_land livestock_hh


*------------------------------------------------------------------------------
* 2. Paper logits: unweighted, vce(robust)  -- specpaper_coef / spec_paper
*------------------------------------------------------------------------------
display as text _n "PAPER LOGITS: unweighted Huber-White SEs, N = " _N

logit fies_dummy female_landowner $x_paper i.saq01, vce(robust)
estimates store pap_fi_a
margins, dydx(*)

logit fies_dummy sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store pap_fi_b
test sole_female_ownership = joint_ownership
lincom sole_female_ownership - joint_ownership
margins, dydx(*)

logit severe_fi female_landowner $x_paper i.saq01, vce(robust)
estimates store pap_sev_a
margins, dydx(*)

logit severe_fi sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store pap_sev_b
test sole_female_ownership = joint_ownership
lincom sole_female_ownership - joint_ownership
margins, dydx(*)

display as text _n "Paper logits: coefficients (should match specpaper_coef.txt)"
estimates table pap_fi_a pap_fi_b pap_sev_a pap_sev_b, ///
    keep(female_landowner sole_female_ownership joint_ownership $x_paper) ///
    star stats(N)


*------------------------------------------------------------------------------
* 3. OLS on the FIES score, same paper controls
*------------------------------------------------------------------------------
display as text _n "PAPER OLS: FIES score, vce(robust)"

regress fies_score female_landowner $x_paper i.saq01, vce(robust)
estimates store pap_ols_a

regress fies_score sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store pap_ols_b
test sole_female_ownership = joint_ownership
lincom sole_female_ownership - joint_ownership


*------------------------------------------------------------------------------
* 4. Survey-weighted, EA-clustered models on the same 1,826 (minus missing wt)
*------------------------------------------------------------------------------
if "$wt" != "" {
    quietly count if missing($wt) | missing(psu)
    display as text _n "NEW (survey): dropping " r(N) " households missing $wt or PSU"
    preserve
    drop if missing($wt, psu)
    svyset psu [pweight=$wt], singleunit(centered)
    display as text "survey sample N = " _N ", weight = $wt"

    svy: regress fies_score female_landowner $x_paper i.saq01
    estimates store svy_ols_a
    svy: regress fies_score sole_female_ownership joint_ownership $x_paper i.saq01
    estimates store svy_ols_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership

    svy: logit fies_dummy female_landowner $x_paper i.saq01
    estimates store svy_fi_a
    margins, dydx(*)
    svy: logit fies_dummy sole_female_ownership joint_ownership $x_paper i.saq01
    estimates store svy_fi_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership
    margins, dydx(*)

    svy: logit severe_fi female_landowner $x_paper i.saq01
    estimates store svy_sev_a
    margins, dydx(*)
    svy: logit severe_fi sole_female_ownership joint_ownership $x_paper i.saq01
    estimates store svy_sev_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership
    margins, dydx(*)

    display as text _n "Paper robust vs survey: ownership coefficients"
    estimates table pap_ols_a svy_ols_a pap_ols_b svy_ols_b, ///
        keep(female_landowner sole_female_ownership joint_ownership *.saq01) ///
        star stats(N)
    estimates table pap_fi_a svy_fi_a pap_fi_b svy_fi_b, ///
        keep(female_landowner sole_female_ownership joint_ownership *.saq01) ///
        star stats(N)
    estimates table pap_sev_a svy_sev_a pap_sev_b svy_sev_b, ///
        keep(female_landowner sole_female_ownership joint_ownership *.saq01) ///
        star stats(N)
    restore
}
else {
    display as text "no pw_w5/svwt in the file; survey block skipped"
}

display as text _n "Done. Log: paper_fies_1826.log"
log close
