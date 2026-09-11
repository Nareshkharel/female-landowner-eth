# cluster_robust_fies_1826.R
# Compare three variance estimators on the paper food-security sample (N=1826)
#
#   1. Previous: unweighted HC0          = Stata logit, vce(robust)
#   2. Cluster:  unweighted cluster EA   = Stata logit, vce(cluster psu)
#   3. Survey:   pw_w5 + cluster EA      = svyset psu [pweight=pw_w5]
#
# How to run (from the repo root, or after setting eth_root)
#   Rscript cluster_robust_fies_1826.R
#
# Packages: haven, sandwich, lmtest, survey, marginaleffects
#
# Outputs
#   cluster_robust_fies_1826_R.log
#   cluster_robust_fies_1826_results.csv

suppressPackageStartupMessages({
  library(haven)
  library(sandwich)
  library(lmtest)
  library(survey)
})

logf <- file("cluster_robust_fies_1826_R.log", open = "wt")
sink(logf, split = TRUE)
sink(logf, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(logf)
}, add = TRUE)

stars <- function(p) {
  if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}
fmt <- function(b, se, p) {
  sprintf("%s%.3f (%.3f)%s", if (b < 0) "-" else "", abs(b), se, stars(p))
}
own <- function(model, v, V) {
  b <- unname(coef(model)[v])
  se <- sqrt(V[v, v])
  p <- 2 * pnorm(-abs(b / se))
  c(coef = b, se = se, p = p, cell = fmt(b, se, p))
}

# Prefer the saved analysis sample; else rebuild from ETH.
cand_sample <- c(
  "paper_sample_1826.dta",
  file.path(getwd(), "paper_sample_1826.dta"),
  file.path("ETH_2021_ESPS-W5_v01_M_Stata_1", "paper_sample_1826.dta")
)
sample_path <- cand_sample[file.exists(cand_sample)][1]

if (!is.na(sample_path)) {
  cat("Using saved sample: ", sample_path, "\n", sep = "")
  d <- as.data.frame(zap_labels(read_dta(sample_path)))
} else {
  stop(
    "Cannot find paper_sample_1826.dta.\n",
    "Run paper_fies_1826.R first, or place paper_sample_1826.dta in the working directory."
  )
}

x_paper <- c("sfi", "age", "basic_educ", "male_head", "dependency_ratio",
             "non_farm_enterprise", "wealth_index", "soil_fertility",
             "drought_shock", "married", "total_land", "livestock_hh")
d$saq01 <- factor(d$saq01)
d$psu <- if ("psu" %in% names(d) && !all(is.na(d$psu))) {
  as.integer(d$psu)
} else {
  as.integer(factor(d$ea_id))
}
d$wt <- as.numeric(d[[intersect(c("pw_w5", "svwt"), names(d))[1]]])
cat("N = ", nrow(d), "  EAs = ", length(unique(d$psu)), "\n", sep = "")

rhs_a <- paste(c("female_landowner", x_paper, "saq01"), collapse = " + ")
rhs_b <- paste(c("sole_female_ownership", "joint_ownership", x_paper, "saq01"),
               collapse = " + ")

fit <- function(y, rhs) glm(as.formula(paste(y, "~", rhs)), data = d, family = binomial())
des <- svydesign(ids = ~psu, weights = ~wt, data = d)

rows <- list()
add_row <- function(outcome, spec, term, rob, Vrob, clV, svy_m) {
  r1 <- own(rob, term, Vrob)
  r2 <- own(rob, term, clV)
  r3 <- own(svy_m, term, vcov(svy_m))
  rows[[length(rows) + 1]] <<- data.frame(
    outcome = outcome, spec = spec, term = term,
    robust = r1["cell"], cluster_ea = r2["cell"], survey = r3["cell"],
    p_robust = as.numeric(r1["p"]),
    p_cluster = as.numeric(r2["p"]),
    p_survey = as.numeric(r3["p"]),
    stringsAsFactors = FALSE
  )
}

for (y in c("fies_dummy", "severe_fi")) {
  lab <- if (y == "fies_dummy") "Mod/sev FI" else "Severe FI"
  ma <- fit(y, rhs_a)
  mb <- fit(y, rhs_b)
  Va <- vcovHC(ma, type = "HC0")
  Vb <- vcovHC(mb, type = "HC0")
  Ca <- vcovCL(ma, cluster = d$psu, type = "HC0")
  Cb <- vcovCL(mb, cluster = d$psu, type = "HC0")
  sa <- svyglm(as.formula(paste(y, "~", rhs_a)), design = des, family = quasibinomial())
  sb <- svyglm(as.formula(paste(y, "~", rhs_b)), design = des, family = quasibinomial())
  add_row(lab, "A", "female_landowner", ma, Va, Ca, sa)
  add_row(lab, "B", "sole_female_ownership", mb, Vb, Cb, sb)
  add_row(lab, "B", "joint_ownership", mb, Vb, Cb, sb)
}

ols_a <- lm(as.formula(paste("fies_score ~", rhs_a)), data = d)
ols_b <- lm(as.formula(paste("fies_score ~", rhs_b)), data = d)
sva <- svyglm(as.formula(paste("fies_score ~", rhs_a)), design = des)
svb <- svyglm(as.formula(paste("fies_score ~", rhs_b)), design = des)
add_row("FIES score OLS", "A", "female_landowner",
        ols_a, vcovHC(ols_a, type = "HC1"),
        vcovCL(ols_a, cluster = d$psu, type = "HC1"), sva)
add_row("FIES score OLS", "B", "sole_female_ownership",
        ols_b, vcovHC(ols_b, type = "HC1"),
        vcovCL(ols_b, cluster = d$psu, type = "HC1"), svb)
add_row("FIES score OLS", "B", "joint_ownership",
        ols_b, vcovHC(ols_b, type = "HC1"),
        vcovCL(ols_b, cluster = d$psu, type = "HC1"), svb)

res <- do.call(rbind, rows)
print(res[, c("outcome", "spec", "term", "robust", "cluster_ea", "survey")],
      row.names = FALSE)
write.csv(res, "cluster_robust_fies_1826_results.csv", row.names = FALSE)
cat("\nWrote cluster_robust_fies_1826_results.csv\n")
cat("Stars: * p<0.10  ** p<0.05  *** p<0.01\n")
cat("Expected: Spec A any-female is ** under robust and ns under cluster EA.\n")
