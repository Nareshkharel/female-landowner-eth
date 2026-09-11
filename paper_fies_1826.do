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
* How to run
*   In the Stata Command window, first cd to the folder that holds
*   merged_w_fies.dta, OR set eth_root below to that full Windows path.
*   Then Do this file (Do not rely on a relative ETH_... path; the
*   do-file editor often runs a Temp copy, so relative paths fail).
*
*   cd "C:\Users\YOURNAME\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"
*   do "C:\path\to\female-landowner-eth\paper_fies_1826.do"
*
* Outputs
*   paper_fies_1826.log          full Stata log (in the working directory)
*   paper_sample_1826.dta        the analysis sample (N = 1,826)
********************************************************************************

clear all
set more off
capture log close
log using "paper_fies_1826.log", replace text

display as text "Working directory is: `c(pwd)'"
display as text "Windows user:         `c(username)'"

*------------------------------------------------------------------------------
* >>> EDIT if auto-search misses your data <<<
* Folder that contains merged_w_fies.dta (full path, in quotes).
* Leave empty to search common locations.
*------------------------------------------------------------------------------
global eth_root ""
* global eth_root "C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"

* Already-built analysis sample (optional). If ETH data are missing but
* this file exists, the script skips the merges and runs the models.
global analysis_dta ""
* global analysis_dta "C:\Users\nk11022\Desktop\paper analysis\paper_sample_1826.dta"

local p1 `"$eth_root"'
local p2 `"`c(pwd)'"'
local p3 `"`c(pwd)'/ETH_2021_ESPS-W5_v01_M_Stata_1"'
local p4 `"ETH_2021_ESPS-W5_v01_M_Stata_1"'
local p5 `"C:\Users\`c(username)'\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"'
local p6 `"C:\Users\`c(username)'\Desktop\ETH_2021_ESPS-W5_v01_M_Stata_1"'
local p7 `"C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"'

global found_eth 0
forvalues i = 1/7 {
    if `"`p`i''"' != "" {
        capture confirm file `"`p`i''/merged_w_fies.dta"'
        if !_rc {
            global eth_root `"`p`i''"'
            global found_eth 1
            continue, break
        }
    }
}

local a1 `"$analysis_dta"'
local a2 `"`c(pwd)'/paper_sample_1826.dta"'
local a3 `"paper_sample_1826.dta"'
global found_sample 0
forvalues i = 1/3 {
    if `"`a`i''"' != "" {
        capture confirm file `"`a`i''"'
        if !_rc {
            global analysis_dta `"`a`i''"'
            global found_sample 1
            continue, break
        }
    }
}

if $found_eth {
    display as text "Using ETH folder: $eth_root"
}
else if $found_sample {
    display as text "ETH folder not found; using saved sample: $analysis_dta"
}
else {
    display as error "Cannot find merged_w_fies.dta"
    display as error "Stata working directory is: `c(pwd)'"
    display as error "The do-file editor runs a Temp copy, so ETH_2021_... as a"
    display as error "relative path will not work. Do ONE of these:"
    display as error "  1. In the Command window:  cd to the ETH folder, then Do again"
    display as error `"  2. Edit this file:  global eth_root "C:\full\path\ETH_2021_ESPS-W5_v01_M_Stata_1""'
    display as error "The ETH folder is the one that contains merged_w_fies.dta"
    display as error "and Household\female_ownership.dta."
    exit 601
}


* Same regressors as spec_paper (no distances).
global x_paper sfi age basic_educ male_head dependency_ratio ///
    non_farm_enterprise wealth_index soil_fertility drought_shock ///
    married total_land livestock_hh

if $found_eth {
*------------------------------------------------------------------------------
* Sample: same inner merges and drops as the paper do-file
*------------------------------------------------------------------------------
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
                  sole_male_ownership soil_fertility farm_type) ///
        keep(match) nogenerate
}
else {
    merge 1:1 household_id using "$eth_root/female_ownership.dta", ///
        keepusing(female_landowner sole_female_ownership joint_ownership ///
                  sole_male_ownership soil_fertility farm_type) ///
        keep(match) nogenerate
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

capture confirm numeric variable fies_dummy
if _rc {
    gen fies_dummy = (fies_score >= 4) if !missing(fies_score)
}
capture confirm numeric variable severe_fi
if _rc {
    gen severe_fi  = (fies_score > 6)  if !missing(fies_score)
}
* Paper coding: crop-only or mixed (farm_type 1 or 3), not livestock-only.
capture confirm numeric variable livestock_hh
if _rc {
    gen livestock_hh = (farm_type == 1 | farm_type == 3) if !missing(farm_type)
}

label var fies_dummy   "1 = moderate or severe FI (FIES >= 4)"
label var severe_fi    "1 = severe FI (FIES > 6)"
label var livestock_hh "1 = farm_type crop-only or mixed (paper coding)"

foreach v in $x_paper female_landowner sole_female_ownership joint_ownership saq01 {
    drop if missing(`v')
}
} else {
    use "$analysis_dta", clear
    display as text "Loaded saved analysis sample"
}

count
if r(N) != 1826 {
    display as error "Expected N = 1,826, got " r(N)
}
else {
    display as text "Paper sample N = 1,826"
}

display as text _n "OWNERSHIP COUNTS"
tab female_landowner
tab sole_female_ownership
tab joint_ownership
tab sole_female_ownership joint_ownership
quietly count if female_landowner == 1 & sole_female_ownership == 1
display as text "sole among any-female: " r(N)
quietly count if female_landowner == 1 & joint_ownership == 1
display as text "joint among any-female: " r(N)

capture confirm numeric variable psu
if _rc {
    capture confirm numeric variable ea_id
    if !_rc {
        egen psu = group(ea_id)
    }
    else {
        egen psu = group(saq01 saq02 saq03 saq04 saq05 saq06)
    }
}

global wt ""
foreach v in pw_w5 svwt {
    capture confirm numeric variable `v'
    if !_rc & "$wt" == "" global wt `v'
}

if $found_eth {
    save "paper_sample_1826.dta", replace
    display as text "saved paper_sample_1826.dta"
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

display as text _n "============================================================"
display as text "KEY OWNERSHIP RESULTS  N = " _N
display as text "============================================================"
foreach est in pap_ols_a pap_fi_a pap_sev_a {
    quietly estimates restore `est'
    display as text "`est' any female: " %7.3f _b[female_landowner] ///
        " (" %6.3f _se[female_landowner] ")"
}
foreach est in pap_ols_b pap_fi_b pap_sev_b {
    quietly estimates restore `est'
    display as text "`est' sole: " %7.3f _b[sole_female_ownership] ///
        " (" %6.3f _se[sole_female_ownership] ")" ///
        "  joint: " %7.3f _b[joint_ownership] ///
        " (" %6.3f _se[joint_ownership] ")"
}
if "$wt" != "" {
    foreach est in svy_ols_a svy_fi_a svy_sev_a {
        quietly estimates restore `est'
        display as text "`est' any female: " %7.3f _b[female_landowner] ///
            " (" %6.3f _se[female_landowner] ")"
    }
    foreach est in svy_ols_b svy_fi_b svy_sev_b {
        quietly estimates restore `est'
        display as text "`est' sole: " %7.3f _b[sole_female_ownership] ///
            " (" %6.3f _se[sole_female_ownership] ")" ///
            "  joint: " %7.3f _b[joint_ownership] ///
            " (" %6.3f _se[joint_ownership] ")"
    }
}

display as text _n "Done. Log: paper_fies_1826.log"
display as text "Sample file: paper_sample_1826.dta"
log close
