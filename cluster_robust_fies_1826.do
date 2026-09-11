********************************************************************************
* cluster_robust_fies_1826.do
* Compare three variance estimators on the paper food-security sample (N=1826)
*
*   1. Previous:  logit/regress , vce(robust)          Huber-White, no cluster
*   2. Cluster:   logit/regress , vce(cluster psu)     EA cluster-robust SEs
*   3. Survey:    svyset psu [pweight=pw_w5] : logit   weight + EA cluster
*
* Spec A = any female landowner
* Spec B = sole female + joint ownership  (Wald: sole = joint)
*
* How to run (Stata Command window)
*   cd to the folder that contains paper_sample_1826.dta
*   OR the ETH folder that contains merged_w_fies.dta, then:
*     do "C:\full\path\to\cluster_robust_fies_1826.do"
*
* If the do-file editor uses a Temp copy, set this path first:
*   global eth_root "C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"
*
* Outputs (in the working directory)
*   cluster_robust_fies_1826.log
********************************************************************************

clear all
set more off
capture log close
log using "cluster_robust_fies_1826.log", replace text

display as text "Working directory: `c(pwd)'"
display as text "Windows user:      `c(username)'"

*------------------------------------------------------------------------------
* Paths: saved sample first, then ETH folder
*------------------------------------------------------------------------------
global eth_root ""
* global eth_root "C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"
global analysis_dta ""
* global analysis_dta "C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1\paper_sample_1826.dta"

local s1 `"$analysis_dta"'
local s2 `"`c(pwd)'/paper_sample_1826.dta"'
local s3 `"paper_sample_1826.dta"'
local s4 `"C:\Users\`c(username)'\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1\paper_sample_1826.dta"'
local s5 `"C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1\paper_sample_1826.dta"'

global found_sample 0
forvalues i = 1/5 {
    if `"`s`i''"' != "" {
        capture confirm file `"`s`i''"'
        if !_rc {
            global analysis_dta `"`s`i''"'
            global found_sample 1
            continue, break
        }
    }
}

local p1 `"$eth_root"'
local p2 `"`c(pwd)'"'
local p3 `"`c(pwd)'/ETH_2021_ESPS-W5_v01_M_Stata_1"'
local p4 `"ETH_2021_ESPS-W5_v01_M_Stata_1"'
local p5 `"C:\Users\`c(username)'\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"'
local p6 `"C:\Users\nk11022\Desktop\paper analysis\ETH_2021_ESPS-W5_v01_M_Stata_1"'

global found_eth 0
forvalues i = 1/6 {
    if `"`p`i''"' != "" {
        capture confirm file `"`p`i''/merged_w_fies.dta"'
        if !_rc {
            global eth_root `"`p`i''"'
            global found_eth 1
            continue, break
        }
    }
}

global x_paper sfi age basic_educ male_head dependency_ratio ///
    non_farm_enterprise wealth_index soil_fertility drought_shock ///
    married total_land livestock_hh

if $found_sample {
    display as text "Using saved sample: $analysis_dta"
    use "$analysis_dta", clear
}
else if $found_eth {
    display as text "Building sample from ETH folder: $eth_root"
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
                      sole_male_ownership soil_fertility farm_type) keep(match) nogenerate
    }
    else {
        merge 1:1 household_id using "$eth_root/female_ownership.dta", ///
            keepusing(female_landowner sole_female_ownership joint_ownership ///
                      sole_male_ownership soil_fertility farm_type) keep(match) nogenerate
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
    drop if missing(dependency_ratio)
    drop if missing(soil_fertility)
    drop if missing(sfi)
    capture confirm numeric variable fies_dummy
    if _rc gen fies_dummy = (fies_score >= 4) if !missing(fies_score)
    capture confirm numeric variable severe_fi
    if _rc gen severe_fi = (fies_score > 6) if !missing(fies_score)
    capture confirm numeric variable livestock_hh
    if _rc gen livestock_hh = (farm_type == 1 | farm_type == 3) if !missing(farm_type)
    foreach v in $x_paper female_landowner sole_female_ownership joint_ownership saq01 {
        drop if missing(`v')
    }
    save "paper_sample_1826.dta", replace
}
else {
    display as error "Cannot find paper_sample_1826.dta or merged_w_fies.dta"
    display as error "cd to the ETH folder, or set eth_root / analysis_dta at the top."
    exit 601
}

count
display as text "N = " r(N)
capture confirm numeric variable psu
if _rc {
    capture confirm numeric variable ea_id
    if !_rc egen psu = group(ea_id)
    else egen psu = group(saq01 saq02 saq03 saq04 saq05 saq06)
}
quietly egen _npsu = group(psu)
quietly summarize _npsu
display as text "Number of EAs / PSUs = " r(max)
drop _npsu

global wt ""
foreach v in pw_w5 svwt {
    capture confirm numeric variable `v'
    if !_rc & "$wt" == "" global wt `v'
}
if "$wt" == "" {
    display as error "No pw_w5/svwt — survey block will be skipped"
}
else {
    svyset psu [pweight=$wt], singleunit(centered)
}


*------------------------------------------------------------------------------
* Models: robust, cluster-robust, survey
*------------------------------------------------------------------------------
display as text _n "========== 1. PREVIOUS: vce(robust) =========="

logit fies_dummy female_landowner $x_paper i.saq01, vce(robust)
estimates store rob_fi_a
margins, dydx(female_landowner)

logit fies_dummy sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store rob_fi_b
test sole_female_ownership = joint_ownership
margins, dydx(sole_female_ownership joint_ownership)

logit severe_fi female_landowner $x_paper i.saq01, vce(robust)
estimates store rob_sev_a
margins, dydx(female_landowner)

logit severe_fi sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store rob_sev_b
test sole_female_ownership = joint_ownership
margins, dydx(sole_female_ownership joint_ownership)

regress fies_score female_landowner $x_paper i.saq01, vce(robust)
estimates store rob_ols_a
regress fies_score sole_female_ownership joint_ownership $x_paper i.saq01, vce(robust)
estimates store rob_ols_b
test sole_female_ownership = joint_ownership


display as text _n "========== 2. CLUSTER-ROBUST: vce(cluster psu)  EA =========="

logit fies_dummy female_landowner $x_paper i.saq01, vce(cluster psu)
estimates store cl_fi_a
margins, dydx(female_landowner)

logit fies_dummy sole_female_ownership joint_ownership $x_paper i.saq01, vce(cluster psu)
estimates store cl_fi_b
test sole_female_ownership = joint_ownership
margins, dydx(sole_female_ownership joint_ownership)

logit severe_fi female_landowner $x_paper i.saq01, vce(cluster psu)
estimates store cl_sev_a
margins, dydx(female_landowner)

logit severe_fi sole_female_ownership joint_ownership $x_paper i.saq01, vce(cluster psu)
estimates store cl_sev_b
test sole_female_ownership = joint_ownership
margins, dydx(sole_female_ownership joint_ownership)

regress fies_score female_landowner $x_paper i.saq01, vce(cluster psu)
estimates store cl_ols_a
regress fies_score sole_female_ownership joint_ownership $x_paper i.saq01, vce(cluster psu)
estimates store cl_ols_b
test sole_female_ownership = joint_ownership


display as text _n "========== 3. SURVEY: pw_w5 + cluster EA =========="

if "$wt" == "" {
    display as text "survey skipped (no weight variable)"
}
else {

svy: logit fies_dummy female_landowner $x_paper i.saq01
estimates store svy_fi_a
margins, dydx(female_landowner)

svy: logit fies_dummy sole_female_ownership joint_ownership $x_paper i.saq01
estimates store svy_fi_b
test sole_female_ownership = joint_ownership
margins, dydx(sole_female_ownership joint_ownership)

svy: logit severe_fi female_landowner $x_paper i.saq01
estimates store svy_sev_a
margins, dydx(female_landowner)

svy: logit severe_fi sole_female_ownership joint_ownership $x_paper i.saq01
estimates store svy_sev_b
test sole_female_ownership = joint_ownership
margins, dydx(sole_female_ownership joint_ownership)

svy: regress fies_score female_landowner $x_paper i.saq01
estimates store svy_ols_a
svy: regress fies_score sole_female_ownership joint_ownership $x_paper i.saq01
estimates store svy_ols_b
test sole_female_ownership = joint_ownership
}


*------------------------------------------------------------------------------
* Side-by-side ownership coefficients
*------------------------------------------------------------------------------
display as text _n "========== OWNERSHIP COEFFICIENTS: robust | cluster EA | survey =========="
display as text "Moderate/severe FI Spec A"
if "$wt" != "" {
    estimates table rob_fi_a cl_fi_a svy_fi_a, ///
        keep(female_landowner) b(%8.3f) se(%8.3f) star stats(N)
}
else {
    estimates table rob_fi_a cl_fi_a, ///
        keep(female_landowner) b(%8.3f) se(%8.3f) star stats(N)
}
display as text "Moderate/severe FI Spec B"
if "$wt" != "" {
    estimates table rob_fi_b cl_fi_b svy_fi_b, ///
        keep(sole_female_ownership joint_ownership) b(%8.3f) se(%8.3f) star stats(N)
}
else {
    estimates table rob_fi_b cl_fi_b, ///
        keep(sole_female_ownership joint_ownership) b(%8.3f) se(%8.3f) star stats(N)
}
display as text "Severe FI Spec A"
if "$wt" != "" {
    estimates table rob_sev_a cl_sev_a svy_sev_a, ///
        keep(female_landowner) b(%8.3f) se(%8.3f) star stats(N)
}
else {
    estimates table rob_sev_a cl_sev_a, ///
        keep(female_landowner) b(%8.3f) se(%8.3f) star stats(N)
}
display as text "Severe FI Spec B"
if "$wt" != "" {
    estimates table rob_sev_b cl_sev_b svy_sev_b, ///
        keep(sole_female_ownership joint_ownership) b(%8.3f) se(%8.3f) star stats(N)
}
else {
    estimates table rob_sev_b cl_sev_b, ///
        keep(sole_female_ownership joint_ownership) b(%8.3f) se(%8.3f) star stats(N)
}
display as text "FIES score OLS Spec A"
if "$wt" != "" {
    estimates table rob_ols_a cl_ols_a svy_ols_a, ///
        keep(female_landowner) b(%8.3f) se(%8.3f) star stats(N)
}
else {
    estimates table rob_ols_a cl_ols_a, ///
        keep(female_landowner) b(%8.3f) se(%8.3f) star stats(N)
}
display as text "FIES score OLS Spec B"
if "$wt" != "" {
    estimates table rob_ols_b cl_ols_b svy_ols_b, ///
        keep(sole_female_ownership joint_ownership) b(%8.3f) se(%8.3f) star stats(N)
}
else {
    estimates table rob_ols_b cl_ols_b, ///
        keep(sole_female_ownership joint_ownership) b(%8.3f) se(%8.3f) star stats(N)
}

display as text _n "Note: estimates table stars are * p<0.05, ** p<0.01, *** p<0.001"
display as text "Paper / outreg2 stars are * p<0.10, ** p<0.05, *** p<0.01"
display as text "Read p-values from the logit output above."
display as text "Expected: Spec A any-female is ** under robust and not significant under cluster EA."
display as text _n "Done. Log: cluster_robust_fies_1826.log"
log close
