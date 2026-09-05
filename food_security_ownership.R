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
# Packages: haven, survey, sandwich, lmtest
#
# Two designs are estimated on the same households:
#   PREVIOUS -- unweighted, Huber-White SEs (the original
#               female landowner analysis.do block: regress/logit, vce(robust);
#               OLS on fies_score also includes married).
#   NEW      -- pw_w5 / svwt weights, SEs clustered on ea_id.
#
# N = 1,809 here (active_hh_member dependency on fies_household.dta).
# The paper table N = 1,826 is correct for
# ETH_2021_ESPS-W5_v01_M_Stata_1/codes/paper code/merge_ownership.do.
# Rebuild it with replicate_paper_fies.R.

library(haven)
library(survey)
library(sandwich)
library(lmtest)

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
# Not the same as female landowner analysis.do, which used
# dependent / independent on the full roster (ages <15 / 15-64 / >64)
# before keeping heads. fies_household.dta is heads only.
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
attrit <- function(v) sum(is.na(d[[v]]))
message("Attrition after ownership + area match (N = ", nrow(d), ")")
for (v in intersect(c("sfi", "dependency_ratio", "soil_fertility"), names(d))) {
  message("  missing ", v, ": ", attrit(v))
}
if (all(c("sfi", "dependency_ratio", "soil_fertility") %in% names(d))) {
  message("  SFI only: ",
          sum(is.na(d$sfi) & !is.na(d$dependency_ratio) & !is.na(d$soil_fertility)))
  message("  dependency only (active_hh_member==0): ",
          sum(!is.na(d$sfi) & is.na(d$dependency_ratio) & !is.na(d$soil_fertility)))
  message("  soil fertility only: ",
          sum(!is.na(d$sfi) & !is.na(d$dependency_ratio) & is.na(d$soil_fertility)))
}
message("  1,826 in the earlier food-security table used the roster ",
        "dependency ratio; this file cannot rebuild that measure.")

d <- d[complete.cases(d[, c(x, "fies_score", ".wt", "psu", "saq01")]), ]
message("estimation sample: ", nrow(d), " households, ",
        length(unique(d$psu)), " clusters")

des <- svydesign(ids = ~psu, weights = ~.wt, data = d)

rhs <- function(own) paste(c(own, x, "factor(saq01)"), collapse = " + ")

# Wald test of H0: beta_sole = beta_joint. Pass vcov so the same helper
# works for the previous robust models and the new survey models.
wald_sole_eq_joint <- function(model, V = vcov(model)) {
  b1 <- "sole_female_ownership"
  b2 <- "joint_ownership"
  b <- coef(model)
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

# Previous-spec helpers: unweighted, Stata vce(robust) = HC1 for OLS, HC0 for logit.
rhs_old_ols <- function(own) {
  extra <- if ("married" %in% names(d)) "married" else NULL
  paste(c(own, x, extra, "factor(saq01)"), collapse = " + ")
}

fit_old_ols <- function(own) {
  m <- lm(as.formula(paste("fies_score ~", rhs_old_ols(own))), data = d)
  print(coeftest(m, vcovHC(m, type = "HC1")))
  m
}

fit_old_logit <- function(y, own) {
  m <- glm(as.formula(paste(y, "~", rhs(own))), data = d, family = binomial())
  print(coeftest(m, vcovHC(m, type = "HC0")))
  m
}

own_row <- function(model, v, V = vcov(model)) {
  b <- unname(coef(model)[v])
  se <- sqrt(V[v, v])
  z <- b / se
  p <- 2 * pnorm(-abs(z))
  stars <- if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
  sprintf("%7.3f (%.3f)%-3s", b, se, stars)
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

  own_a <- "female_landowner"
  own_b <- c("sole_female_ownership", "joint_ownership")

  #--------------------------------------------------------------------------
  # PREVIOUS: unweighted, vce(robust) -- matches female landowner analysis.do
  # OLS includes married; logits do not.
  #--------------------------------------------------------------------------
  message(strrep("=", 70))
  message("PREVIOUS: unweighted, Huber-White robust SEs")
  message(strrep("=", 70))

  old_ols_a <- fit_old_ols(own_a)
  old_ols_b <- fit_old_ols(own_b)
  message("Wald (previous OLS)")
  wald_sole_eq_joint(old_ols_b, vcovHC(old_ols_b, type = "HC1"))

  old_logit_a <- fit_old_logit("fies_dummy", own_a)
  old_logit_b <- fit_old_logit("fies_dummy", own_b)
  message("Wald (previous moderate/severe FI)")
  wald_sole_eq_joint(old_logit_b, vcovHC(old_logit_b, type = "HC0"))
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    message("AME (previous moderate/severe FI, Spec A)")
    print(marginaleffects::avg_slopes(old_logit_a, vcov = vcovHC(old_logit_a, type = "HC0")))
    message("AME (previous moderate/severe FI, Spec B)")
    print(marginaleffects::avg_slopes(old_logit_b, vcov = vcovHC(old_logit_b, type = "HC0")))
  }

  old_sfi_a <- fit_old_logit("severe_fi", own_a)
  old_sfi_b <- fit_old_logit("severe_fi", own_b)
  message("Wald (previous severe FI)")
  wald_sole_eq_joint(old_sfi_b, vcovHC(old_sfi_b, type = "HC0"))
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    message("AME (previous severe FI, Spec A)")
    print(marginaleffects::avg_slopes(old_sfi_a, vcov = vcovHC(old_sfi_a, type = "HC0")))
    message("AME (previous severe FI, Spec B)")
    print(marginaleffects::avg_slopes(old_sfi_b, vcov = vcovHC(old_sfi_b, type = "HC0")))
  }

  #--------------------------------------------------------------------------
  # NEW: survey weights + EA cluster
  #--------------------------------------------------------------------------
  message(strrep("=", 70))
  message("NEW: ", wt, " weights, SEs clustered on ea_id")
  message(strrep("=", 70))

  ols_a <- svyglm(as.formula(paste("fies_score ~", rhs(own_a))), design = des)
  ols_b <- svyglm(as.formula(paste("fies_score ~", rhs(own_b))), design = des)
  print(summary(ols_a))
  print(summary(ols_b))
  message("Wald (new OLS)")
  wald_sole_eq_joint(ols_b)

  logit_a <- svyglm(as.formula(paste("fies_dummy ~", rhs(own_a))),
                    design = des, family = quasibinomial())
  logit_b <- svyglm(as.formula(paste("fies_dummy ~", rhs(own_b))),
                    design = des, family = quasibinomial())
  print(summary(logit_a))
  print(summary(logit_b))
  message("Wald (new moderate/severe FI)")
  wald_sole_eq_joint(logit_b)

  sfi_a <- svyglm(as.formula(paste("severe_fi ~", rhs(own_a))),
                  design = des, family = quasibinomial())
  sfi_b <- svyglm(as.formula(paste("severe_fi ~", rhs(own_b))),
                  design = des, family = quasibinomial())
  print(summary(sfi_a))
  print(summary(sfi_b))
  message("Wald (new severe FI)")
  wald_sole_eq_joint(sfi_b)

  #--------------------------------------------------------------------------
  # Side-by-side ownership coefficients
  #--------------------------------------------------------------------------
  cat("\n", strrep("=", 100), "\n", sep = "")
  cat("COMPARISON  n = ", nrow(d), " households. Previous = unweighted robust. New = ",
      wt, " + cluster ea_id.\n", sep = "")
  cat("OLS previous includes married (original do-file). Logits do not.\n")
  cat("Region dummies (factor(saq01), Afar omitted) are in every model.\n")
  cat(strrep("=", 100), "\n", sep = "")
  cat(sprintf("%-28s %-22s %-22s\n", "", "PREVIOUS (robust)", "NEW (survey)"))

  rows <- list(
    list("FIES score, any female",
         old_ols_a, "female_landowner", vcovHC(old_ols_a, type = "HC1"),
         ols_a, "female_landowner", vcov(ols_a)),
    list("FIES score, sole female",
         old_ols_b, "sole_female_ownership", vcovHC(old_ols_b, type = "HC1"),
         ols_b, "sole_female_ownership", vcov(ols_b)),
    list("FIES score, joint",
         old_ols_b, "joint_ownership", vcovHC(old_ols_b, type = "HC1"),
         ols_b, "joint_ownership", vcov(ols_b)),
    list("Mod/sev FI, any female",
         old_logit_a, "female_landowner", vcovHC(old_logit_a, type = "HC0"),
         logit_a, "female_landowner", vcov(logit_a)),
    list("Mod/sev FI, sole female",
         old_logit_b, "sole_female_ownership", vcovHC(old_logit_b, type = "HC0"),
         logit_b, "sole_female_ownership", vcov(logit_b)),
    list("Mod/sev FI, joint",
         old_logit_b, "joint_ownership", vcovHC(old_logit_b, type = "HC0"),
         logit_b, "joint_ownership", vcov(logit_b)),
    list("Severe FI, any female",
         old_sfi_a, "female_landowner", vcovHC(old_sfi_a, type = "HC0"),
         sfi_a, "female_landowner", vcov(sfi_a)),
    list("Severe FI, sole female",
         old_sfi_b, "sole_female_ownership", vcovHC(old_sfi_b, type = "HC0"),
         sfi_b, "sole_female_ownership", vcov(sfi_b)),
    list("Severe FI, joint",
         old_sfi_b, "joint_ownership", vcovHC(old_sfi_b, type = "HC0"),
         sfi_b, "joint_ownership", vcov(sfi_b))
  )
  for (r in rows) {
    cat(sprintf("%-28s %-22s %-22s\n", r[[1]],
                own_row(r[[2]], r[[3]], r[[4]]),
                own_row(r[[5]], r[[6]], r[[7]])))
  }
}
