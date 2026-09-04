# Food security and female land ownership
# Ethiopia ESPS Wave 5
#
# Spec A: any female ownership           (female_landowner)
# Spec B: sole vs joint female ownership (sole_female_ownership, joint_ownership)
#
# Survey design
#   Weight:  svwt   (kebele-level survey weight)
#   Cluster: kebele (PSU). saq06 is used if kebele is not already in the data.
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
#   The printed difference is the same contrast with SE, z, and 95% CI.
#
# Packages: haven, survey
#   install.packages(c("haven", "survey"))

library(haven)
library(survey)

# Path to the merged household analysis file. Edit before running.
analysis_dta <- "analysis_household.dta"

d <- zap_labels(read_dta(analysis_dta))
d <- as.data.frame(d)

#------------------------------------------------------------------------------
# Identifiers, outcomes, sample
#------------------------------------------------------------------------------
if (!"kebele" %in% names(d)) {
  d$kebele <- as.numeric(as.character(d$saq06))
}

if (!"fies_score" %in% names(d)) {
  d$fies_score <- with(
    d, worried + healthy + fewfoods + skipped + ateless + wholeday + ranout + hungry
  )
}

d <- subset(d, fies_score <= 8)
d <- d[
  complete.cases(d[, c(
    "dependency_ratio", "soil_fertility", "sfi", "svwt", "kebele"
  )]),
]

d$fies_dummy <- as.integer(d$fies_score >= 4)
d$severe_fi  <- as.integer(d$fies_score > 6)

#------------------------------------------------------------------------------
# Survey design: PSU = kebele, weight = svwt
#------------------------------------------------------------------------------
des <- svydesign(ids = ~kebele, weights = ~svwt, data = d)

rhs_a <- paste(
  "female_landowner",
  "sfi", "age", "basic_educ", "male_head", "dependency_ratio",
  "non_farm_enterprise", "wealth_index", "dist_admhq", "dist_road",
  "soil_fertility", "drought_shock", "factor(saq01)",
  sep = " + "
)
rhs_b <- sub("female_landowner",
             "sole_female_ownership + joint_ownership",
             rhs_a,
             fixed = TRUE)

# Wald test of H0: beta_sole = beta_joint (survey VCE)
wald_sole_eq_joint <- function(model) {
  b1 <- "sole_female_ownership"
  b2 <- "joint_ownership"
  b  <- coef(model)
  V  <- vcov(model)
  diff <- unname(b[b1] - b[b2])
  se   <- sqrt(V[b1, b1] + V[b2, b2] - 2 * V[b1, b2])
  z    <- diff / se
  p    <- 2 * pnorm(-abs(z))
  out  <- data.frame(
    difference = diff,
    se = se,
    z = z,
    p_value = p,
    ci95_low = diff - 1.96 * se,
    ci95_high = diff + 1.96 * se
  )
  print(out)
  invisible(out)
}

#------------------------------------------------------------------------------
# 1. FIES score (linear)
#------------------------------------------------------------------------------
ols_a <- svyglm(as.formula(paste("fies_score ~", rhs_a)), design = des)
ols_b <- svyglm(as.formula(paste("fies_score ~", rhs_b)), design = des)
summary(ols_a)
summary(ols_b)
wald_sole_eq_joint(ols_b)

#------------------------------------------------------------------------------
# 2. Moderate or severe food insecurity (logit)
#------------------------------------------------------------------------------
logit_a <- svyglm(
  as.formula(paste("fies_dummy ~", rhs_a)),
  design = des,
  family = quasibinomial()
)
logit_b <- svyglm(
  as.formula(paste("fies_dummy ~", rhs_b)),
  design = des,
  family = quasibinomial()
)
summary(logit_a)
summary(logit_b)
wald_sole_eq_joint(logit_b)

#------------------------------------------------------------------------------
# 3. Severe food insecurity (logit)
#------------------------------------------------------------------------------
sfi_a <- svyglm(
  as.formula(paste("severe_fi ~", rhs_a)),
  design = des,
  family = quasibinomial()
)
sfi_b <- svyglm(
  as.formula(paste("severe_fi ~", rhs_b)),
  design = des,
  family = quasibinomial()
)
summary(sfi_a)
summary(sfi_b)
wald_sole_eq_joint(sfi_b)
