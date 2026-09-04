********************************************************************************
* Food security and female land ownership
* Ethiopia ESPS Wave 5 -- runs on fies_household.dta (one row per household)
*
* Spec A: any female ownership          (female_landowner)
* Spec B: sole vs joint female ownership (sole_female_ownership, joint_ownership)
*
* Survey design
*   Weight:  svwt if present, otherwise pw_w5 (the household weight shipped
*            with the survey).
*   Cluster: ea_id, the sampling PSU. saq06 is only a kebele *code* (40 values
*            that repeat across regions), so it is not a usable cluster on its
*            own; kebele below is built from saq01-saq06 and can be swapped in.
*   Do not add vce(robust) or vce(cluster ...) on the estimation command;
*   svyset already clusters.
*
* Formal test (run after every Spec B model)
*   H0: _b[sole_female_ownership] = _b[joint_ownership]
*   Spec A is Spec B with that restriction, because female_landowner is the
*   union of sole and joint. A likelihood-ratio test is not valid here
*   (survey weights + clustered SEs). Use the Wald test from -test- / -lincom-.
*
* How to read the test
*   Fail to reject H0 (p >= 0.05): sole and joint are not statistically
*     different. Spec A is adequate; report "any female ownership."
*   Reject H0 (p < 0.05): do not pool. Use Spec B and interpret sole and
*     joint against the omitted group (no female owner) separately.
*   lincom reports the same contrast as a difference with SE and CI.
*
* fies_household.dta carries the outcome and most controls but no ownership
* variables. Set ownership_dta to the female_ownership.dta built by
* ethiopia_landowner.do to estimate Spec A and Spec B.
*
* Two designs on the same households:
*   PREVIOUS -- unweighted, vce(robust), matching female landowner analysis.do
*               (OLS on fies_score also includes married).
*   NEW      -- svyset with pweight and PSU cluster.
********************************************************************************

set more off

* Outputs of ethiopia_landowner.do. Leave empty to run without them.
*   ownership_dta = female_ownership.dta -> the ownership dummies
*   area_dta      = area_ethiopia.dta    -> sfi and total_land
global analysis_dta  "fies_household.dta"
global ownership_dta "female_ownership.dta"
global area_dta      "area_ethiopia.dta"

* ethiopia_landowner.do codes a missing owner as "no female owner", so the
* reference group mixes male-owned households with households whose owner
* fields are all blank. Set to 1 to keep only households with an owner on
* record, making sole-male ownership the reference.
global owner_identified_only 0

use "$analysis_dta", clear

* keepusing() is required, not optional: female_ownership.dta also carries
* s2q05, s2q06 and s2q17 from the post-planting parcel roster, and
* fies_household.dta already holds different variables under those same
* names from the household education section.
if `"$ownership_dta"' != "" {
    merge 1:1 household_id using "$ownership_dta", ///
        keepusing(female_landowner sole_female_ownership joint_ownership ///
                  sole_male_ownership number_plots_female soil_fertility) ///
        keep(match) nogenerate
}

if `"$area_dta"' != "" {
    merge 1:1 household_id using "$area_dta", ///
        keepusing(sfi total_land) keep(match) nogenerate
}

if $owner_identified_only == 1 {
    capture confirm numeric variable female_landowner
    if !_rc {
        capture confirm numeric variable sole_male_ownership
        if !_rc {
            drop if female_landowner == 0 & sole_male_ownership == 0
        }
    }
}


*------------------------------------------------------------------------------
* Design identifiers
*------------------------------------------------------------------------------
* Dropped first so the file can be re-run, or run block by block, without
* tripping "already defined".
capture drop kebele psu fies_dummy severe_fi

egen kebele = group(saq01 saq02 saq03 saq04 saq05 saq06)
label var kebele "kebele (from geographic codes; saq06 alone is not unique)"

capture confirm variable ea_id
if !_rc {
    egen psu = group(ea_id)
}
else {
    gen psu = kebele
}

global wt ""
foreach v in svwt pw_w5 {
    capture confirm numeric variable `v'
    if !_rc & "$wt" == "" global wt `v'
}
if "$wt" == "" {
    display as error "no survey weight (svwt or pw_w5) found"
    exit 111
}
display as text "weight = $wt"


*------------------------------------------------------------------------------
* Outcomes and controls
*------------------------------------------------------------------------------
capture confirm numeric variable fies_score
if _rc {
    gen fies_score = worried + healthy + fewfoods + skipped ///
                   + ateless + wholeday + ranout + hungry
}

capture confirm numeric variable basic_educ
if _rc {
    gen basic_educ = basic_education
}

* Working-age members are the denominator, so households with none are missing.
capture confirm numeric variable dependency_ratio
if _rc {
    gen dependency_ratio = (household_size - active_hh_member) / active_hh_member ///
        if active_hh_member > 0
}

drop if fies_score > 8

gen fies_dummy = (fies_score >= 4) if !missing(fies_score)
gen severe_fi  = (fies_score > 6)  if !missing(fies_score)

label var fies_dummy "1 = moderate or severe FI (FIES >= 4)"
label var severe_fi  "1 = severe FI (FIES > 6)"

* sfi and soil_fertility come from the post-planting files and are absent from
* fies_household.dta, so controls are assembled from what the data holds.
global x ""
foreach v in sfi age basic_educ male_head dependency_ratio non_farm_enterprise ///
             wealth_index dist_admhq dist_road soil_fertility drought_shock {
    capture confirm numeric variable `v'
    if !_rc {
        global x $x `v'
    }
    else {
        display as text "note: control `v' not in data, omitted"
    }
}

* Casewise sample, so every model below is estimated on the same households.
foreach v in $x {
    drop if missing(`v')
}
drop if missing(fies_score, $wt, psu)

svyset psu [pweight=$wt], singleunit(centered)


*------------------------------------------------------------------------------
* Do the ownership variables exist?
*------------------------------------------------------------------------------
global has_own 1
foreach v in female_landowner sole_female_ownership joint_ownership {
    capture confirm numeric variable `v'
    if _rc global has_own 0
}

if $has_own == 0 {

    display as error "{hline 70}"
    display as error "Ownership variables not found in $analysis_dta."
    display as error "Spec A, Spec B and the sole-vs-joint Wald test are skipped."
    display as error "Set ownership_dta to female_ownership.dta (built by"
    display as error "ethiopia_landowner.do) and run again."
    display as error "{hline 70}"

    svy: regress fies_score $x i.saq01
    svy: logit fies_dummy $x i.saq01
    svy: logit severe_fi  $x i.saq01

}
else {

    *--------------------------------------------------------------------------
    * PREVIOUS: unweighted, vce(robust) -- female landowner analysis.do
    * OLS includes married; logits do not.
    *--------------------------------------------------------------------------
    display as text _n "PREVIOUS: unweighted, Huber-White robust SEs"

    capture confirm numeric variable married
    if _rc {
        local married_ctrl
        display as text "note: married not in data, omitted from previous OLS"
    }
    else {
        local married_ctrl married
    }

    regress fies_score female_landowner $x `married_ctrl' i.saq01, vce(robust)
    estimates store old_ols_a

    regress fies_score sole_female_ownership joint_ownership $x `married_ctrl' i.saq01, vce(robust)
    estimates store old_ols_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership

    logit fies_dummy female_landowner $x i.saq01, vce(robust)
    estimates store old_logit_a
    margins, dydx(*)

    logit fies_dummy sole_female_ownership joint_ownership $x i.saq01, vce(robust)
    estimates store old_logit_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership
    margins, dydx(*)

    logit severe_fi female_landowner $x i.saq01, vce(robust)
    estimates store old_sfi_a
    margins, dydx(*)

    logit severe_fi sole_female_ownership joint_ownership $x i.saq01, vce(robust)
    estimates store old_sfi_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership
    margins, dydx(*)

    *--------------------------------------------------------------------------
    * NEW: survey weights + EA cluster
    *--------------------------------------------------------------------------
    display as text _n "NEW: $wt weights, SEs clustered on the PSU"

    svy: regress fies_score female_landowner $x i.saq01
    estimates store ols_a

    svy: regress fies_score sole_female_ownership joint_ownership $x i.saq01
    estimates store ols_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership

    svy: logit fies_dummy female_landowner $x i.saq01
    estimates store logit_a
    margins, dydx(*)

    svy: logit fies_dummy sole_female_ownership joint_ownership $x i.saq01
    estimates store logit_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership
    margins, dydx(*)

    svy: logit severe_fi female_landowner $x i.saq01
    estimates store sfi_a
    margins, dydx(*)

    svy: logit severe_fi sole_female_ownership joint_ownership $x i.saq01
    estimates store sfi_b
    test sole_female_ownership = joint_ownership
    lincom sole_female_ownership - joint_ownership
    margins, dydx(*)

    display as text _n "PREVIOUS (robust) vs NEW (survey) -- ownership and region"
    estimates table old_ols_a ols_a old_ols_b ols_b, ///
        keep(female_landowner sole_female_ownership joint_ownership *.saq01) ///
        star stats(N)
    estimates table old_logit_a logit_a old_logit_b logit_b, ///
        keep(female_landowner sole_female_ownership joint_ownership *.saq01) ///
        star stats(N)
    estimates table old_sfi_a sfi_a old_sfi_b sfi_b, ///
        keep(female_landowner sole_female_ownership joint_ownership *.saq01) ///
        star stats(N)

}
