********************************************************************************
* Food security and female land ownership
* Ethiopia ESPS Wave 5
*
* Spec A: any female ownership          (female_landowner)
* Spec B: sole vs joint female ownership (sole_female_ownership, joint_ownership)
*
* Survey design
*   Weight:  svwt   (kebele-level survey weight)
*   Cluster: kebele (PSU). saq06 is used if kebele is not already in the data.
*   Do not add vce(robust) or vce(cluster ...) on the estimation command;
*   svyset already clusters at kebele.
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
********************************************************************************

set more off

* Leave empty to use the merged household data already in memory.
* Otherwise point to the saved analysis .dta.
global analysis_dta ""

if `"$analysis_dta"' != "" {
    use "$analysis_dta", clear
}


*------------------------------------------------------------------------------
* Identifiers, outcomes, sample
*------------------------------------------------------------------------------
capture confirm variable kebele
if _rc {
    destring saq06, generate(kebele)
}

capture confirm variable fies_score
if _rc {
    gen fies_score = worried + healthy + fewfoods + skipped ///
                   + ateless + wholeday + ranout + hungry
}

drop if fies_score > 8
drop if missing(dependency_ratio, soil_fertility, sfi, svwt, kebele)

capture drop fies_dummy severe_fi
gen fies_dummy = (fies_score >= 4) if !missing(fies_score)
gen severe_fi  = (fies_score > 6)  if !missing(fies_score)

label var fies_dummy "1 = moderate or severe FI (FIES >= 4)"
label var severe_fi  "1 = severe FI (FIES > 6)"


*------------------------------------------------------------------------------
* Survey design: PSU = kebele, weight = svwt
*------------------------------------------------------------------------------
svyset kebele [pweight=svwt], singleunit(centered)

global x sfi age basic_educ male_head dependency_ratio ///
         non_farm_enterprise wealth_index dist_admhq dist_road ///
         soil_fertility drought_shock


*------------------------------------------------------------------------------
* 1. FIES score (linear)
*------------------------------------------------------------------------------
svy: regress fies_score female_landowner $x i.saq01
estimates store ols_a

svy: regress fies_score sole_female_ownership joint_ownership $x i.saq01
estimates store ols_b
test sole_female_ownership = joint_ownership
lincom sole_female_ownership - joint_ownership


*------------------------------------------------------------------------------
* 2. Moderate or severe food insecurity (logit)
*------------------------------------------------------------------------------
svy: logit fies_dummy female_landowner $x i.saq01
estimates store logit_a
margins, dydx(*)

svy: logit fies_dummy sole_female_ownership joint_ownership $x i.saq01
estimates store logit_b
test sole_female_ownership = joint_ownership
lincom sole_female_ownership - joint_ownership
margins, dydx(*)


*------------------------------------------------------------------------------
* 3. Severe food insecurity (logit)
*------------------------------------------------------------------------------
svy: logit severe_fi female_landowner $x i.saq01
estimates store sfi_a
margins, dydx(*)

svy: logit severe_fi sole_female_ownership joint_ownership $x i.saq01
estimates store sfi_b
test sole_female_ownership = joint_ownership
lincom sole_female_ownership - joint_ownership
margins, dydx(*)


estimates table ols_a ols_b logit_a logit_b sfi_a sfi_b, star stats(N)
