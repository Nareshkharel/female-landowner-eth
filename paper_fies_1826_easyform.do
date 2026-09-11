********************************************************************************
* paper_fies_1826_easyform.do
*
* Easy-to-read version of paper_fies_1826.do
* Same sample, same models, same numbers. Only the layout and comments changed.
* The original file is left as-is.
*
* Ethiopia ESPS Wave 5: food insecurity and female land ownership
* Paper sample: 1,826 households
*
* ---------------------------------------------------------------------------
* WHAT THE FILE DOES
*   Step 1  Set the folder that holds your ETH data
*   Step 2  Load the 1,826-household sample (or build it from the ETH files)
*   Step 3  Make outcomes, the control list, the PSU, and the weight
*   Step 4  Summary statistics  (should match summary_paper.txt)
*   Step 5  PREVIOUS models: unweighted, Huber-White SEs  (vce(robust))
*           Spec A = any female landowner
*           Spec B = sole female vs joint, plus Wald test sole = joint
*           Outcomes: moderate/severe FI, severe FI, FIES raw score
*   Step 6  NEW models: survey weight pw_w5 and cluster at the enumeration area
*   Step 7  Print the ownership coefficients in one short list
*
* ---------------------------------------------------------------------------
* HOW TO RUN (do this from the Stata Command window)
*
*   do "C:\full\path\to\paper_fies_1826_easyform.do"
*
* The do-file editor often runs a Temp copy, so relative paths fail.
* If Step 1 is wrong, Stata will stop and tell you.
*
* ---------------------------------------------------------------------------
* SHORT GLOSSARY
*   fies_score              raw FIES count, 0 to 8
*   fies_dummy              1 if FIES >= 4  (moderate or severe food insecurity)
*   severe_fi               1 if FIES > 6   (severe food insecurity)
*   female_landowner        1 if any woman on the household owns land (Spec A)
*   sole_female_ownership   1 if a woman is the only owner
*   joint_ownership         1 if a woman owns jointly with someone else
*   livestock_hh            1 if farm is crop-only or mixed (paper coding)
*   saq01                   region  (i.saq01 adds region dummies)
*   ea_id / psu             enumeration area  (cluster)
*   pw_w5                   Wave 5 household survey weight
*   vce(robust)             Huber-White standard errors (unclustered)
*   svyset                  survey design: weight + EA cluster
*   margins, dydx(*)        average marginal effects for every regressor
*   test A = B              Wald test that two coefficients are equal
*
* OUTPUTS (written to Stata's current working directory)
*   paper_fies_1826_easyform.log
*   paper_sample_1826.dta     (created if the sample has to be rebuilt)
********************************************************************************

clear all
set more off
capture log close
log using "paper_fies_1826_easyform.log", replace text

display as text "Stata working directory: `c(pwd)'"
display as text "Windows user name:       `c(username)'"


********************************************************************************
* STEP 1.  Folder that holds your data
*
* Edit this one line if your ETH folder is somewhere else.
* The folder must contain merged_w_fies.dta
********************************************************************************

global eth_root "C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"

* Optional: point straight at a saved analysis sample.
* Leave empty to let the do-file look in eth_root and the working directory.
global analysis_dta ""
* global analysis_dta "C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1\paper_sample_1826.dta"


********************************************************************************
* STEP 2.  Load the sample
*
* Prefer paper_sample_1826.dta if it already exists (faster).
* Otherwise merge the ETH files the same way as paper_fies_1826.do
********************************************************************************

* Look for a ready-made sample in three places
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
        display as error "Cannot find $eth_root/merged_w_fies.dta"
        display as error "Edit Step 1 so eth_root is the folder that contains merged_w_fies.dta."
        display as error "Example:"
        display as error `"  global eth_root "C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1""'
        exit 601
    }

    * Keep only households that appear in every file (inner merge)
    * merged_w_fies: household heads, roster dependency ratio, education, FIES items
    use "$eth_root/merged_w_fies.dta", clear

    * drought_shock
    merge 1:1 household_id using "$eth_root/hh_9_w5.dta",     keep(match) nogenerate
    * non_farm_enterprise
    merge 1:1 household_id using "$eth_root/hh_12a_w5.dta",   keep(match) nogenerate
    merge 1:1 household_id using "$eth_root/hh_14.dta",       keep(match) nogenerate
    * wealth_index
    merge 1:1 household_id using "$eth_root/hh11_w5_pca.dta", keep(match) nogenerate
    * FIES score
    merge 1:1 household_id using "$eth_root/fies_dta.dta",    keep(match) nogenerate

    * Ownership only (names such as s2q05 collide with education variables)
    capture confirm file "$eth_root/Household/female_ownership.dta"
    if !_rc {
        merge 1:1 household_id using "$eth_root/Household/female_ownership.dta", ///
            keepusing(female_landowner sole_female_ownership joint_ownership ///
                      sole_male_ownership soil_fertility farm_type) ///
            keep(match) nogenerate
    }
    else {
        merge 1:1 household_id using "$eth_root/female_ownership.dta", ///
            keepusing(female_landowner sole_female_ownership joint_ownership ///
                      sole_male_ownership soil_fertility farm_type) ///
            keep(match) nogenerate
    }

    * Distances are merged so the sample matches the paper, but they are not used
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
    * sfi and total_land
    merge 1:1 household_id using "$eth_root/area_ethiopia.dta", ///
        keepusing(sfi total_land) keep(match) nogenerate
    merge 1:1 household_id using "$eth_root/agri_practices.dta", keep(match) nogenerate

    * FIES raw score = count of the 8 yes/no items
    capture confirm numeric variable fies_score
    if _rc {
        gen fies_score = worried + healthy + fewfoods + skipped ///
                       + ateless + wholeday + ranout + hungry
    }

    * Same drops as the paper
    drop if missing(fies_score) | fies_score > 8
    drop if missing(dependency_ratio)
    drop if missing(soil_fertility)
    drop if missing(sfi)

    save "paper_sample_1826.dta", replace
    display as text "Saved paper_sample_1826.dta in the working directory"
}

count
display as text "Households now in memory: " r(N)


********************************************************************************
* STEP 3.  Outcomes, controls, cluster, weight
********************************************************************************

* Moderate or severe food insecurity: FIES score 4 or higher
capture confirm numeric variable fies_dummy
if _rc gen fies_dummy = (fies_score >= 4) if !missing(fies_score)

* Severe food insecurity: FIES score 7 or 8
capture confirm numeric variable severe_fi
if _rc gen severe_fi = (fies_score > 6) if !missing(fies_score)

* Paper coding: crop-only (1) or mixed (3). This is not livestock-only.
capture confirm numeric variable livestock_hh
if _rc gen livestock_hh = (farm_type == 1 | farm_type == 3) if !missing(farm_type)

label var fies_dummy   "1 = moderate or severe FI (FIES >= 4)"
label var severe_fi    "1 = severe FI (FIES > 6)"
label var livestock_hh "1 = crop-only or mixed farm (paper coding)"

* Controls used in every regression. Region dummies are added as i.saq01
global x_paper sfi age basic_educ male_head dependency_ratio ///
    non_farm_enterprise wealth_index soil_fertility drought_shock ///
    married total_land livestock_hh

* Drop remaining missing values on the regressors
foreach v in $x_paper female_landowner sole_female_ownership joint_ownership saq01 {
    capture confirm variable `v'
    if !_rc drop if missing(`v')
}

count
if r(N) == 1826 {
    display as text "Paper sample N = 1,826  (this is the correct paper N)"
}
else {
    display as error "Expected N = 1,826, got " r(N)
}

display as text _n "Who owns land in this sample?"
tab female_landowner
tab sole_female_ownership joint_ownership
quietly count if female_landowner == 1 & sole_female_ownership == 1
display as text "Sole female owners: " r(N)
quietly count if female_landowner == 1 & joint_ownership == 1
display as text "Joint owners:       " r(N)

* Enumeration area = PSU for clustering
capture confirm numeric variable psu
if _rc {
    capture confirm numeric variable ea_id
    if !_rc egen psu = group(ea_id)
    else    egen psu = group(saq01 saq02 saq03 saq04 saq05 saq06)
}

* Survey weight: Wave 5 household weight
global wt ""
capture confirm numeric variable pw_w5
if !_rc global wt pw_w5
if "$wt" == "" {
    capture confirm numeric variable svwt
    if !_rc global wt svwt
}
if "$wt" != "" display as text "Weight variable: $wt"
else           display as text "No pw_w5/svwt found; survey models will be skipped"


********************************************************************************
* STEP 4.  Summary statistics
*          Compare this block to summary_paper.txt
********************************************************************************

display as text _n "===== STEP 4. Summary statistics ====="
summarize fies_dummy severe_fi sfi age basic_educ male_head ///
    dependency_ratio non_farm_enterprise wealth_index female_landowner ///
    soil_fertility drought_shock married total_land livestock_hh


********************************************************************************
* STEP 5.  PREVIOUS MODELS
*          Unweighted logit / OLS with Huber-White SEs:  vce(robust)
*          These numbers match specpaper_coef.txt and spec_paper.txt
*
*          Spec A: one dummy, female_landowner
*          Spec B: two dummies, sole_female_ownership and joint_ownership
*                  then Wald test  H0: sole = joint
********************************************************************************

display as text _n "===== STEP 5. Previous models: vce(robust) ====="

* ----- 5a. Moderate or severe FI, Spec A (any female owner) -----
display as text _n "5a. Logit, moderate/severe FI, any female owner"
logit fies_dummy female_landowner $x_paper i.saq01, vce(robust)
estimates store pap_fi_a
margins, dydx(*)

* ----- 5b. Moderate or severe FI, Spec B (sole vs joint) -----
display as text _n "5b. Logit, moderate/severe FI, sole vs joint"
logit fies_dummy sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store pap_fi_b
test sole_female_ownership = joint_ownership
lincom sole_female_ownership - joint_ownership
margins, dydx(*)

* ----- 5c. Severe FI, Spec A -----
display as text _n "5c. Logit, severe FI, any female owner"
logit severe_fi female_landowner $x_paper i.saq01, vce(robust)
estimates store pap_sev_a
margins, dydx(*)

* ----- 5d. Severe FI, Spec B -----
display as text _n "5d. Logit, severe FI, sole vs joint"
logit severe_fi sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store pap_sev_b
test sole_female_ownership = joint_ownership
lincom sole_female_ownership - joint_ownership
margins, dydx(*)

* ----- 5e. FIES score (0-8), OLS -----
display as text _n "5e. OLS, FIES score, any female owner"
regress fies_score female_landowner $x_paper i.saq01, vce(robust)
estimates store pap_ols_a

display as text _n "5f. OLS, FIES score, sole vs joint"
regress fies_score sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store pap_ols_b
test sole_female_ownership = joint_ownership
lincom sole_female_ownership - joint_ownership

* Compact tables: Spec A and Spec B separately
* (mixing them in one keep() list is hard to read; region dummies omitted)
display as text _n "Previous models, Spec A (any female owner + controls)"
estimates table pap_fi_a pap_sev_a pap_ols_a, ///
    keep(female_landowner $x_paper) star stats(N)

display as text _n "Previous models, Spec B (sole vs joint + controls)"
estimates table pap_fi_b pap_sev_b pap_ols_b, ///
    keep(sole_female_ownership joint_ownership $x_paper) star stats(N)


********************************************************************************
* STEP 6.  NEW MODELS
*          Same 1,826 households
*          svyset: cluster at the EA (psu) and weight with pw_w5
********************************************************************************

display as text _n "===== STEP 6. New models: survey weight + EA cluster ====="

if "$wt" == "" {
    display as text "No weight variable. Skipping survey models."
}
else {
    quietly count if missing($wt) | missing(psu)
    display as text "Households missing weight or PSU: " r(N)

    preserve
    drop if missing($wt, psu)
    svyset psu [pweight=$wt], singleunit(centered)
    display as text "Survey N = " _N "   weight = $wt"

    * OLS on FIES score
    display as text _n "6a. Survey OLS, FIES score, Spec A"
    svy: regress fies_score female_landowner $x_paper i.saq01
    estimates store svy_ols_a
    display as text _n "6b. Survey OLS, FIES score, Spec B"
    svy: regress fies_score sole_female_ownership joint_ownership $x_paper i.saq01
    estimates store svy_ols_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership

    * Moderate or severe FI
    display as text _n "6c. Survey logit, moderate/severe FI, Spec A"
    svy: logit fies_dummy female_landowner $x_paper i.saq01
    estimates store svy_fi_a
    margins, dydx(*)
    display as text _n "6d. Survey logit, moderate/severe FI, Spec B"
    svy: logit fies_dummy sole_female_ownership joint_ownership $x_paper i.saq01
    estimates store svy_fi_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership
    margins, dydx(*)

    * Severe FI
    display as text _n "6e. Survey logit, severe FI, Spec A"
    svy: logit severe_fi female_landowner $x_paper i.saq01
    estimates store svy_sev_a
    margins, dydx(*)
    display as text _n "6f. Survey logit, severe FI, Spec B"
    svy: logit severe_fi sole_female_ownership joint_ownership $x_paper i.saq01
    estimates store svy_sev_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership
    margins, dydx(*)

    * Ownership only. Do not use keep(*.saq01); that pattern can error.
    display as text _n "Previous (robust) vs new (survey): ownership coefficients"
    estimates table pap_ols_a svy_ols_a, keep(female_landowner) star stats(N)
    estimates table pap_ols_b svy_ols_b, keep(sole_female_ownership joint_ownership) star stats(N)
    estimates table pap_fi_a svy_fi_a, keep(female_landowner) star stats(N)
    estimates table pap_fi_b svy_fi_b, keep(sole_female_ownership joint_ownership) star stats(N)
    estimates table pap_sev_a svy_sev_a, keep(female_landowner) star stats(N)
    estimates table pap_sev_b svy_sev_b, keep(sole_female_ownership joint_ownership) star stats(N)
    restore
}


********************************************************************************
* STEP 7.  Key ownership numbers (coefficient and standard error)
*
* Look at the full logit output above for p-values and average marginal effects.
* Compact-table stars use  * p<0.05  ** p<0.01  *** p<0.001
* The paper tables use     * p<0.10  ** p<0.05  *** p<0.01
********************************************************************************

display as text _n "===== STEP 7. Key ownership results  N = " _N " ====="

foreach est in pap_ols_a pap_fi_a pap_sev_a {
    quietly estimates restore `est'
    display as text "`est'  any female: " %7.3f _b[female_landowner] ///
        " (" %6.3f _se[female_landowner] ")"
}
foreach est in pap_ols_b pap_fi_b pap_sev_b {
    quietly estimates restore `est'
    display as text "`est'  sole: " %7.3f _b[sole_female_ownership] ///
        " (" %6.3f _se[sole_female_ownership] ")" ///
        "   joint: " %7.3f _b[joint_ownership] ///
        " (" %6.3f _se[joint_ownership] ")"
}
if "$wt" != "" {
    foreach est in svy_ols_a svy_fi_a svy_sev_a {
        quietly estimates restore `est'
        display as text "`est'  any female: " %7.3f _b[female_landowner] ///
            " (" %6.3f _se[female_landowner] ")"
    }
    foreach est in svy_ols_b svy_fi_b svy_sev_b {
        quietly estimates restore `est'
        display as text "`est'  sole: " %7.3f _b[sole_female_ownership] ///
            " (" %6.3f _se[sole_female_ownership] ")" ///
            "   joint: " %7.3f _b[joint_ownership] ///
            " (" %6.3f _se[joint_ownership] ")"
    }
}

display as text _n "Done."
display as text "Log:    paper_fies_1826_easyform.log"
display as text "Sample: paper_sample_1826.dta  (if the sample was rebuilt)"
display as text "Original do-file (unchanged): paper_fies_1826.do"
log close
