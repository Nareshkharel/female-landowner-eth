# Food security and female land ownership
# Ethiopia ESPS Wave 5 -- runs on fies_household.dta (one row per household)
#
# Spec A: any female ownership           (female_landowner)
# Spec B: sole vs joint female ownership (sole_female_ownership, joint_ownership)
#
# Survey design
#   Weight:  svwt if present, otherwise pw_w5 (the household weight shipped
#            with the survey).
#   Cluster: ea_id, the sampling PSU. saq06 is only a kebele *code* (40 values
#            that repeat across regions), so it is not a usable cluster on its
#            own; kebele below is built from saq01-saq06 and can be swapped in.
#
# Formal test (run after every Spec B model)
#   H0: beta_sole = beta_joint
#   Spec A is Spec B with that restriction, because female_landowner is the
#   union of sole and joint. A likelihood-ratio test is not valid here
#   (survey weights + clustered SEs). Use the Wald test below.
#
# How to read the test
#   Fail to reject H0 (p >= 0.05): sole and joint are not statistically
#     different. Spec A is adequate; report "any female ownership."
#   Reject H0 (p < 0.05): do not pool. Use Spec B and interpret sole and
#     joint against the omitted group (no female owner) separately.
#   The printed difference is the same contrast with SE, t, and 95% CI.
#
# fies_household.dta carries the outcome and most controls but no ownership
# variables. Set ownership_dta to the female_ownership.dta built by
# ethiopia_landowner.do to estimate Spec A and Spec B.
#
# Packages: haven, survey

library(haven)
library(survey)

# Outputs of ethiopia_landowner.do. Leave NULL to run without them.
#   ownership_dta = female_ownership.dta -> the ownership dummies
#   area_dta      = area_ethiopia.dta    -> sfi and total_land
analysis_dta  <- "fies_household.dta"
ownership_dta <- "female_ownership.dta"
area_dta      <- "area_ethiopia.dta"

# ethiopia_landowner.do codes a missing owner as "no female owner", so the
# reference group mixes male-owned households with households whose owner
# fields are all blank. TRUE keeps only households with an owner on record,
# making sole-male ownership the reference.
owner_identified_only <- FALSE

d <- as.data.frame(zap_labels(read_dta(analysis_dta)))

# Columns are whitelisted, not merged wholesale: female_ownership.dta also
# carries s2q05, s2q06 and s2q17 from the post-planting parcel roster, and
# fies_household.dta already holds different variables under those same names
# from the household education section.
merge_keep <- function(d, path, vars) {
  if (is.null(path)) return(d)
  u <- as.data.frame(zap_labels(read_dta(path)))
  merge(d, u[, intersect(c("household_id", vars), names(u))], by = "household_id")
}

d <- merge_keep(d, ownership_dta,
                c("female_landowner", "sole_female_ownership", "joint_ownership",
                  "sole_male_ownership", "number_plots_female", "soil_fertility"))
d <- merge_keep(d, area_dta, c("sfi", "total_land"))

if (owner_identified_only && all(c("female_landowner", "sole_male_ownership") %in% names(d))) {
  before <- nrow(d)
  d <- subset(d, female_landowner == 1 | sole_male_ownership == 1)
  message("owner_identified_only: dropped ", before - nrow(d),
          " households with no owner on record")
}

#------------------------------------------------------------------------------
# Design identifiers
#------------------------------------------------------------------------------
d$kebele <- as.integer(factor(with(
  d, paste(saq01, saq02, saq03, saq04, saq05, saq06, sep = "_")
)))

d$psu <- if ("ea_id" %in% names(d)) as.integer(factor(d$ea_id)) else d$kebele

wt <- intersect(c("svwt", "pw_w5"), names(d))[1]
if (is.na(wt)) stop("no survey weight (svwt or pw_w5) found")
d$.wt <- as.numeric(d[[wt]])
message("weight = ", wt)

#------------------------------------------------------------------------------
# Outcomes and controls
#------------------------------------------------------------------------------
if (!"fies_score" %in% names(d)) {
  d$fies_score <- with(
    d, worried + healthy + fewfoods + skipped + ateless + wholeday + ranout + hungry
  )
}

if (!"basic_educ" %in% names(d)) d$basic_educ <- d$basic_education

# Working-age members are the denominator, so households with none are missing.
if (!"dependency_ratio" %in% names(d)) {
  d$dependency_ratio <- ifelse(
    d$active_hh_member > 0,
    (d$household_size - d$active_hh_member) / d$active_hh_member,
    NA_real_
  )
}

d <- subset(d, fies_score <= 8)

d$fies_dummy <- as.integer(d$fies_score >= 4)
d$severe_fi  <- as.integer(d$fies_score > 6)

# sfi and soil_fertility come from the post-planting files and are absent from
# fies_household.dta, so controls are assembled from what the data holds.
candidates <- c("sfi", "age", "basic_educ", "male_head", "dependency_ratio",
                "non_farm_enterprise", "wealth_index", "dist_admhq",
                "dist_road", "soil_fertility", "drought_shock")
x <- intersect(candidates, names(d))
dropped <- setdiff(candidates, x)
if (length(dropped)) message("controls not in data, omitted: ",
                             paste(dropped, collapse = ", "))

# Casewise sample, so every model below is estimated on the same households.
d <- d[complete.cases(d[, c(x, "fies_score", ".wt", "psu", "saq01")]), ]
message("estimation sample: ", nrow(d), " households, ",
        length(unique(d$psu)), " clusters")

des <- svydesign(ids = ~psu, weights = ~.wt, data = d)

rhs <- function(own) paste(c(own, x, "factor(saq01)"), collapse = " + ")

# Wald test of H0: beta_sole = beta_joint, using the survey VCE and the same
# residual degrees of freedom that summary() reports.
wald_sole_eq_joint <- function(model) {
  b1 <- "sole_female_ownership"
  b2 <- "joint_ownership"
  b <- coef(model)
  V <- vcov(model)
  df <- df.residual(model)
  diff <- unname(b[b1] - b[b2])
  se <- sqrt(V[b1, b1] + V[b2, b2] - 2 * V[b1, b2])
  tstat <- diff / se
  out <- data.frame(
    difference = diff,
    se = se,
    t = tstat,
    df = df,
    p_value = 2 * pt(-abs(tstat), df),
    ci95_low = diff - qt(0.975, df) * se,
    ci95_high = diff + qt(0.975, df) * se
  )
  print(out)
  invisible(out)
}

#------------------------------------------------------------------------------
# Do the ownership variables exist?
#------------------------------------------------------------------------------
own_vars <- c("female_landowner", "sole_female_ownership", "joint_ownership")

if (!all(own_vars %in% names(d))) {
  message(strrep("-", 70))
  message("Ownership variables not found in ", analysis_dta, ".")
  message("Spec A, Spec B and the sole-vs-joint Wald test are skipped.")
  message("Set ownership_dta to female_ownership.dta (built by")
  message("ethiopia_landowner.do) and run again.")
  message(strrep("-", 70))

  print(summary(svyglm(as.formula(paste("fies_score ~", rhs(NULL))), design = des)))
  print(summary(svyglm(as.formula(paste("fies_dummy ~", rhs(NULL))),
                       design = des, family = quasibinomial())))
  print(summary(svyglm(as.formula(paste("severe_fi ~", rhs(NULL))),
                       design = des, family = quasibinomial())))
} else {

  #----------------------------------------------------------------------------
  # 1. FIES score (linear)
  #----------------------------------------------------------------------------
  ols_a <- svyglm(as.formula(paste("fies_score ~", rhs("female_landowner"))),
                  design = des)
  ols_b <- svyglm(
    as.formula(paste("fies_score ~",
                     rhs(c("sole_female_ownership", "joint_ownership")))),
    design = des
  )
  print(summary(ols_a))
  print(summary(ols_b))
  wald_sole_eq_joint(ols_b)

  #----------------------------------------------------------------------------
  # 2. Moderate or severe food insecurity (logit)
  #----------------------------------------------------------------------------
  logit_a <- svyglm(as.formula(paste("fies_dummy ~", rhs("female_landowner"))),
                    design = des, family = quasibinomial())
  logit_b <- svyglm(
    as.formula(paste("fies_dummy ~",
                     rhs(c("sole_female_ownership", "joint_ownership")))),
    design = des, family = quasibinomial()
  )
  print(summary(logit_a))
  print(summary(logit_b))
  wald_sole_eq_joint(logit_b)

  #----------------------------------------------------------------------------
  # 3. Severe food insecurity (logit)
  #----------------------------------------------------------------------------
  sfi_a <- svyglm(as.formula(paste("severe_fi ~", rhs("female_landowner"))),
                  design = des, family = quasibinomial())
  sfi_b <- svyglm(
    as.formula(paste("severe_fi ~",
                     rhs(c("sole_female_ownership", "joint_ownership")))),
    design = des, family = quasibinomial()
  )
  print(summary(sfi_a))
  print(summary(sfi_b))
  wald_sole_eq_joint(sfi_b)
}
