# Paper food-security sample (N = 1,826) and all models on that sample
# Ethiopia ESPS Wave 5
#
# Same specification as:
#   ETH_2021_ESPS-W5_v01_M_Stata_1/codes/paper code/merge_ownership.do
#   spec_paper.txt / specpaper_coef.txt / sample_output_food.docx
#
# What it does
#   1. Builds the 1,826-household sample (roster dependency ratio).
#   2. Summary statistics (should match summary_paper.txt).
#   3. Unweighted robust logits (paper table): Spec A any female owner,
#      Spec B sole vs joint, for moderate/severe FI and severe FI.
#      After every Spec B: Wald test H0: sole = joint.
#      After every logit: average marginal effects (HC0).
#   4. Unweighted robust OLS on the FIES score (same controls).
#   5. Survey-weighted, EA-clustered versions of the same models
#      (pw_w5, cluster ea_id) on households that have a weight.
#
# How to run
#   Set eth_root below if auto-search misses the data, then:
#   Rscript paper_fies_1826.R
#
# The folder you want is the one that contains merged_w_fies.dta, e.g.
#   eth_root <- "C:/Users/YOURNAME/Desktop/paper analysis/ETH_2021_ESPS-W5_v01_M_Stata_1"
#
# Packages: haven, sandwich, lmtest, survey, marginaleffects
#
# Outputs
#   paper_fies_1826_R.log        full R log
#   paper_sample_1826.dta        the analysis sample (N = 1,826)
#   paper_fies_1826_results.csv  compact ownership table

suppressPackageStartupMessages({
  library(haven)
  library(sandwich)
  library(lmtest)
  library(survey)
})

logf <- file("paper_fies_1826_R.log", open = "wt")
sink(logf, split = TRUE)
sink(logf, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(logf)
}, add = TRUE)

# Leave "" to auto-search. Set a full path if R cannot find the ETH folder.
eth_root <- ""
# eth_root <- "C:/Users/nk11022/Desktop/paper analysis/ETH_2021_ESPS-W5_v01_M_Stata_1"

user <- Sys.getenv("USERNAME", unset = Sys.getenv("USER", unset = ""))
cand <- c(
  eth_root,
  "ETH_2021_ESPS-W5_v01_M_Stata_1",
  file.path(getwd(), "ETH_2021_ESPS-W5_v01_M_Stata_1"),
  getwd(),
  file.path("C:/Users", user, "Desktop/paper analysis/ETH_2021_ESPS-W5_v01_M_Stata_1"),
  file.path("C:/Users", user, "Desktop/ETH_2021_ESPS-W5_v01_M_Stata_1"),
  "C:/Users/nk11022/Desktop/paper analysis/ETH_2021_ESPS-W5_v01_M_Stata_1"
)
eth_root <- ""
for (p in unique(cand[nzchar(cand)])) {
  if (file.exists(file.path(p, "merged_w_fies.dta"))) {
    eth_root <- p
    break
  }
}
if (!nzchar(eth_root)) {
  stop(
    "Cannot find merged_w_fies.dta.\n",
    "Working directory is: ", getwd(), "\n",
    "Set eth_root to the ETH_2021_ESPS-W5_v01_M_Stata_1 folder ",
    "(the folder that contains merged_w_fies.dta)."
  )
}
cat("Using ETH folder: ", eth_root, "\n", sep = "")

rd <- function(...) {
  as.data.frame(zap_labels(read_dta(file.path(eth_root, ...))))
}

inner <- function(d, rel, extra) {
  u <- rd(rel)
  keep <- unique(c("household_id", extra))
  keep <- intersect(keep, names(u))
  before <- nrow(d)
  out <- merge(d, u[, keep, drop = FALSE], by = "household_id")
  message(sprintf("  merge %-40s %d -> %d", rel, before, nrow(out)))
  out
}

stars <- function(p) {
  if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}

fmt <- function(b, se, p) {
  sprintf("%s%.3f (%.3f)%s", if (b < 0) "-" else "", abs(b), se, stars(p))
}

wald_sole_eq_joint <- function(model, V = vcov(model)) {
  b1 <- "sole_female_ownership"
  b2 <- "joint_ownership"
  b <- coef(model)
  diff <- unname(b[b1] - b[b2])
  se <- sqrt(V[b1, b1] + V[b2, b2] - 2 * V[b1, b2])
  z <- diff / se
  out <- data.frame(
    difference = diff, se = se, z = z,
    p_value = 2 * pnorm(-abs(z)),
    ci95_low = diff - 1.96 * se,
    ci95_high = diff + 1.96 * se
  )
  print(out)
  invisible(out)
}

print_coeftest <- function(model, V, title) {
  message("\n", title)
  print(coeftest(model, V))
}

print_ames <- function(model, V, title) {
  if (!requireNamespace("marginaleffects", quietly = TRUE)) {
    message(title, ": install.packages(\"marginaleffects\") to print AMEs")
    return(invisible(NULL))
  }
  message("\n", title)
  print(marginaleffects::avg_slopes(model, vcov = V))
}

own_row <- function(model, v, V) {
  b <- unname(coef(model)[v])
  se <- sqrt(V[v, v])
  p <- 2 * pnorm(-abs(b / se))
  fmt(b, se, p)
}

#------------------------------------------------------------------------------
# Sample: same inner merges and drops as the paper do-file
#------------------------------------------------------------------------------
d <- rd("merged_w_fies.dta")
d <- inner(d, "hh_9_w5.dta", c("drought_shock", "shock_faced"))
d <- inner(d, "hh_12a_w5.dta", "non_farm_enterprise")
d <- inner(d, "hh_14.dta", "psnp_assistance")
d <- inner(d, "hh11_w5_pca.dta", "wealth_index")
d <- inner(d, "fies_dta.dta",
           c("worried", "healthy", "fewfoods", "skipped",
             "ateless", "wholeday", "ranout", "hungry"))
d <- inner(d, "Household/female_ownership.dta",
           c("female_landowner", "sole_female_ownership", "joint_ownership",
             "sole_male_ownership", "soil_fertility", "farm_type"))

geo <- if (file.exists(file.path(eth_root, "Household_geographical.dta"))) {
  "Household_geographical.dta"
} else {
  "household_geographical.dta"
}
d <- inner(d, geo, c("dist_admhq", "dist_road"))
d <- inner(d, "hdds_ethiopia.dta", "hdds_household")
d <- inner(d, "area_ethiopia.dta", c("sfi", "total_land"))
d <- inner(d, "agri_practices.dta", c("chemical_fertilizer", "crop_rotation"))

if (!"fies_score" %in% names(d)) {
  d$fies_score <- with(
    d, worried + healthy + fewfoods + skipped + ateless + wholeday + ranout + hungry
  )
}

d <- subset(d, !is.na(fies_score) & fies_score <= 8)
d <- subset(d, !is.na(dependency_ratio) & !is.na(soil_fertility) & !is.na(sfi))

d$fies_dummy <- as.integer(d$fies_score >= 4)
d$severe_fi <- as.integer(d$fies_score > 6)
# Paper coding: crop-only or mixed (farm_type 1 or 3), not livestock-only.
d$livestock_hh <- as.integer(d$farm_type %in% c(1, 3))
d$saq01 <- factor(d$saq01)

x_paper <- c("sfi", "age", "basic_educ", "male_head", "dependency_ratio",
             "non_farm_enterprise", "wealth_index", "soil_fertility",
             "drought_shock", "married", "total_land", "livestock_hh")
need <- c(x_paper, "female_landowner", "sole_female_ownership",
          "joint_ownership", "saq01", "fies_dummy", "severe_fi", "fies_score")
d <- d[complete.cases(d[, need]), ]

if (nrow(d) != 1826) {
  warning("Expected N = 1,826, got ", nrow(d))
} else {
  cat("Paper sample N = 1,826\n")
}

cat("\nOWNERSHIP COUNTS\n")
print(table(any_female = d$female_landowner, useNA = "ifany"))
print(table(sole = d$sole_female_ownership, joint = d$joint_ownership, useNA = "ifany"))
cat(sprintf("sole among any-female: %d\n",
            sum(d$female_landowner == 1 & d$sole_female_ownership == 1)))
cat(sprintf("joint among any-female: %d\n",
            sum(d$female_landowner == 1 & d$joint_ownership == 1)))

d$psu <- if ("ea_id" %in% names(d)) as.integer(factor(d$ea_id)) else NA_integer_
wt <- intersect(c("pw_w5", "svwt"), names(d))[1]
if (!is.na(wt)) d$.wt <- as.numeric(d[[wt]])

keep_cols <- unique(c(
  "household_id", "ea_id", "psu", "saq01", wt,
  "fies_score", "fies_dummy", "severe_fi",
  "female_landowner", "sole_female_ownership", "joint_ownership",
  "sole_male_ownership", x_paper
))
keep_cols <- intersect(keep_cols, names(d))
d_out <- d[, keep_cols]
if (is.factor(d_out$saq01)) {
  d_out$saq01 <- as.integer(as.character(d_out$saq01))
}
write_dta(d_out, "paper_sample_1826.dta")
cat("saved paper_sample_1826.dta\n")

#------------------------------------------------------------------------------
# 1. Summary statistics (summary_paper.txt)
#------------------------------------------------------------------------------
message("\nSUMMARY (target: summary_paper.txt)")
sum_vars <- c("fies_dummy", "severe_fi", "sfi", "age", "basic_educ", "male_head",
              "dependency_ratio", "non_farm_enterprise", "wealth_index",
              "female_landowner", "soil_fertility", "drought_shock",
              "married", "total_land", "livestock_hh")
print(sapply(d[, sum_vars], function(v) {
  c(n = sum(!is.na(v)), mean = mean(v, na.rm = TRUE), sd = sd(v, na.rm = TRUE),
    min = min(v, na.rm = TRUE), max = max(v, na.rm = TRUE))
}))

#------------------------------------------------------------------------------
# 2. Paper logits: unweighted, HC0 = Stata logit, vce(robust)
#------------------------------------------------------------------------------
rhs_a <- paste(c("female_landowner", x_paper, "saq01"), collapse = " + ")
rhs_b <- paste(c("sole_female_ownership", "joint_ownership", x_paper, "saq01"),
               collapse = " + ")

fit_logit <- function(y, rhs) {
  glm(as.formula(paste(y, "~", rhs)), data = d, family = binomial())
}

pap_fi_a <- fit_logit("fies_dummy", rhs_a)
pap_fi_b <- fit_logit("fies_dummy", rhs_b)
pap_sev_a <- fit_logit("severe_fi", rhs_a)
pap_sev_b <- fit_logit("severe_fi", rhs_b)

V_fi_a <- vcovHC(pap_fi_a, type = "HC0")
V_fi_b <- vcovHC(pap_fi_b, type = "HC0")
V_sev_a <- vcovHC(pap_sev_a, type = "HC0")
V_sev_b <- vcovHC(pap_sev_b, type = "HC0")

print_coeftest(pap_fi_a, V_fi_a, "PAPER logit FI Spec A (target female_landowner -0.264 (0.122))")
print_ames(pap_fi_a, V_fi_a, "AME FI Spec A (target female_landowner -0.052 (0.024))")

print_coeftest(pap_fi_b, V_fi_b, "PAPER logit FI Spec B")
message("Wald FI sole = joint")
wald_sole_eq_joint(pap_fi_b, V_fi_b)
print_ames(pap_fi_b, V_fi_b, "AME FI Spec B (target sole -0.010, joint -0.063)")

print_coeftest(pap_sev_a, V_sev_a, "PAPER logit severe FI Spec A (target -0.399 (0.187))")
print_ames(pap_sev_a, V_sev_a, "AME severe FI Spec A (target -0.037 (0.017))")

print_coeftest(pap_sev_b, V_sev_b, "PAPER logit severe FI Spec B")
message("Wald severe FI sole = joint")
wald_sole_eq_joint(pap_sev_b, V_sev_b)
print_ames(pap_sev_b, V_sev_b, "AME severe FI Spec B (target sole -0.006, joint -0.048)")

#------------------------------------------------------------------------------
# 3. OLS on the FIES score, same paper controls (HC1 = Stata regress, vce(robust))
#------------------------------------------------------------------------------
pap_ols_a <- lm(as.formula(paste("fies_score ~", rhs_a)), data = d)
pap_ols_b <- lm(as.formula(paste("fies_score ~", rhs_b)), data = d)
V_ols_a <- vcovHC(pap_ols_a, type = "HC1")
V_ols_b <- vcovHC(pap_ols_b, type = "HC1")
print_coeftest(pap_ols_a, V_ols_a, "PAPER OLS FIES score Spec A")
print_coeftest(pap_ols_b, V_ols_b, "PAPER OLS FIES score Spec B")
message("Wald OLS sole = joint")
wald_sole_eq_joint(pap_ols_b, V_ols_b)

#------------------------------------------------------------------------------
# 4. Survey-weighted, EA-clustered models
#------------------------------------------------------------------------------
if (!is.na(wt) && "psu" %in% names(d)) {
  ds <- d[!is.na(d$.wt) & !is.na(d$psu), ]
  message("\nNEW (survey): ", nrow(ds), " households, weight = ", wt)
  des <- svydesign(ids = ~psu, weights = ~.wt, data = ds)

  svy_ols_a <- svyglm(as.formula(paste("fies_score ~", rhs_a)), design = des)
  svy_ols_b <- svyglm(as.formula(paste("fies_score ~", rhs_b)), design = des)
  print(summary(svy_ols_a))
  print(summary(svy_ols_b))
  message("Wald survey OLS sole = joint")
  wald_sole_eq_joint(svy_ols_b)

  svy_fi_a <- svyglm(as.formula(paste("fies_dummy ~", rhs_a)),
                     design = des, family = quasibinomial())
  svy_fi_b <- svyglm(as.formula(paste("fies_dummy ~", rhs_b)),
                     design = des, family = quasibinomial())
  print(summary(svy_fi_a))
  print(summary(svy_fi_b))
  message("Wald survey FI sole = joint")
  wald_sole_eq_joint(svy_fi_b)
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    message("AME survey FI Spec A")
    print(marginaleffects::avg_slopes(svy_fi_a, newdata = ds, wts = ".wt"))
    message("AME survey FI Spec B")
    print(marginaleffects::avg_slopes(svy_fi_b, newdata = ds, wts = ".wt"))
  }

  svy_sev_a <- svyglm(as.formula(paste("severe_fi ~", rhs_a)),
                      design = des, family = quasibinomial())
  svy_sev_b <- svyglm(as.formula(paste("severe_fi ~", rhs_b)),
                      design = des, family = quasibinomial())
  print(summary(svy_sev_a))
  print(summary(svy_sev_b))
  message("Wald survey severe FI sole = joint")
  wald_sole_eq_joint(svy_sev_b)
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    message("AME survey severe FI Spec A")
    print(marginaleffects::avg_slopes(svy_sev_a, newdata = ds, wts = ".wt"))
    message("AME survey severe FI Spec B")
    print(marginaleffects::avg_slopes(svy_sev_b, newdata = ds, wts = ".wt"))
  }

  cat("\n", strrep("=", 90), "\n", sep = "")
  cat("OWNERSHIP COMPARISON  paper N = ", nrow(d),
      "  survey N = ", nrow(ds), "\n", sep = "")
  cat(sprintf("%-28s %-22s %-22s\n", "", "PAPER (robust)", "NEW (survey)"))
  rows <- list(
    list("FIES score, any female", pap_ols_a, "female_landowner", V_ols_a,
         svy_ols_a, "female_landowner", vcov(svy_ols_a)),
    list("FIES score, sole female", pap_ols_b, "sole_female_ownership", V_ols_b,
         svy_ols_b, "sole_female_ownership", vcov(svy_ols_b)),
    list("FIES score, joint", pap_ols_b, "joint_ownership", V_ols_b,
         svy_ols_b, "joint_ownership", vcov(svy_ols_b)),
    list("Mod/sev FI, any female", pap_fi_a, "female_landowner", V_fi_a,
         svy_fi_a, "female_landowner", vcov(svy_fi_a)),
    list("Mod/sev FI, sole female", pap_fi_b, "sole_female_ownership", V_fi_b,
         svy_fi_b, "sole_female_ownership", vcov(svy_fi_b)),
    list("Mod/sev FI, joint", pap_fi_b, "joint_ownership", V_fi_b,
         svy_fi_b, "joint_ownership", vcov(svy_fi_b)),
    list("Severe FI, any female", pap_sev_a, "female_landowner", V_sev_a,
         svy_sev_a, "female_landowner", vcov(svy_sev_a)),
    list("Severe FI, sole female", pap_sev_b, "sole_female_ownership", V_sev_b,
         svy_sev_b, "sole_female_ownership", vcov(svy_sev_b)),
    list("Severe FI, joint", pap_sev_b, "joint_ownership", V_sev_b,
         svy_sev_b, "joint_ownership", vcov(svy_sev_b))
  )
  for (r in rows) {
    cat(sprintf("%-28s %-22s %-22s\n", r[[1]],
                own_row(r[[2]], r[[3]], r[[4]]),
                own_row(r[[5]], r[[6]], r[[7]])))
  }

  coef_row <- function(block, outcome, spec, term, model, V) {
    b <- unname(coef(model)[term])
    se <- sqrt(V[term, term])
    p <- 2 * pnorm(-abs(b / se))
    data.frame(block = block, outcome = outcome, spec = spec, term = term,
               estimate = b, se = se, p = p, stringsAsFactors = FALSE)
  }
  ame_row <- function(block, outcome, spec, term, model, V, newdata = d, wts = NULL) {
    if (!requireNamespace("marginaleffects", quietly = TRUE)) {
      return(NULL)
    }
    sl <- if (is.null(wts)) {
      marginaleffects::avg_slopes(model, vcov = V, variables = term)
    } else {
      marginaleffects::avg_slopes(model, newdata = newdata, wts = wts, variables = term)
    }
    data.frame(block = block, outcome = outcome, spec = spec,
               term = paste0("AME:", term),
               estimate = sl$estimate[1], se = sl$std.error[1], p = sl$p.value[1],
               stringsAsFactors = FALSE)
  }
  wald_row <- function(block, outcome, spec, model, V) {
    w <- wald_sole_eq_joint(model, V)
    data.frame(block = block, outcome = outcome, spec = spec, term = "Wald sole=joint",
               estimate = w$difference, se = w$se, p = w$p_value,
               stringsAsFactors = FALSE)
  }

  res <- rbind(
    coef_row("paper", "FIES score", "A", "female_landowner", pap_ols_a, V_ols_a),
    coef_row("paper", "FIES score", "B", "sole_female_ownership", pap_ols_b, V_ols_b),
    coef_row("paper", "FIES score", "B", "joint_ownership", pap_ols_b, V_ols_b),
    wald_row("paper", "FIES score", "B", pap_ols_b, V_ols_b),
    coef_row("paper", "Mod/sev FI logit", "A", "female_landowner", pap_fi_a, V_fi_a),
    ame_row("paper", "Mod/sev FI AME", "A", "female_landowner", pap_fi_a, V_fi_a),
    coef_row("paper", "Mod/sev FI logit", "B", "sole_female_ownership", pap_fi_b, V_fi_b),
    coef_row("paper", "Mod/sev FI logit", "B", "joint_ownership", pap_fi_b, V_fi_b),
    ame_row("paper", "Mod/sev FI AME", "B", "sole_female_ownership", pap_fi_b, V_fi_b),
    ame_row("paper", "Mod/sev FI AME", "B", "joint_ownership", pap_fi_b, V_fi_b),
    wald_row("paper", "Mod/sev FI logit", "B", pap_fi_b, V_fi_b),
    coef_row("paper", "Severe FI logit", "A", "female_landowner", pap_sev_a, V_sev_a),
    ame_row("paper", "Severe FI AME", "A", "female_landowner", pap_sev_a, V_sev_a),
    coef_row("paper", "Severe FI logit", "B", "sole_female_ownership", pap_sev_b, V_sev_b),
    coef_row("paper", "Severe FI logit", "B", "joint_ownership", pap_sev_b, V_sev_b),
    ame_row("paper", "Severe FI AME", "B", "sole_female_ownership", pap_sev_b, V_sev_b),
    ame_row("paper", "Severe FI AME", "B", "joint_ownership", pap_sev_b, V_sev_b),
    wald_row("paper", "Severe FI logit", "B", pap_sev_b, V_sev_b),
    coef_row("survey", "FIES score", "A", "female_landowner", svy_ols_a, vcov(svy_ols_a)),
    coef_row("survey", "FIES score", "B", "sole_female_ownership", svy_ols_b, vcov(svy_ols_b)),
    coef_row("survey", "FIES score", "B", "joint_ownership", svy_ols_b, vcov(svy_ols_b)),
    wald_row("survey", "FIES score", "B", svy_ols_b, vcov(svy_ols_b)),
    coef_row("survey", "Mod/sev FI logit", "A", "female_landowner", svy_fi_a, vcov(svy_fi_a)),
    ame_row("survey", "Mod/sev FI AME", "A", "female_landowner", svy_fi_a, vcov(svy_fi_a), ds, ".wt"),
    coef_row("survey", "Mod/sev FI logit", "B", "sole_female_ownership", svy_fi_b, vcov(svy_fi_b)),
    coef_row("survey", "Mod/sev FI logit", "B", "joint_ownership", svy_fi_b, vcov(svy_fi_b)),
    ame_row("survey", "Mod/sev FI AME", "B", "sole_female_ownership", svy_fi_b, vcov(svy_fi_b), ds, ".wt"),
    ame_row("survey", "Mod/sev FI AME", "B", "joint_ownership", svy_fi_b, vcov(svy_fi_b), ds, ".wt"),
    wald_row("survey", "Mod/sev FI logit", "B", svy_fi_b, vcov(svy_fi_b)),
    coef_row("survey", "Severe FI logit", "A", "female_landowner", svy_sev_a, vcov(svy_sev_a)),
    ame_row("survey", "Severe FI AME", "A", "female_landowner", svy_sev_a, vcov(svy_sev_a), ds, ".wt"),
    coef_row("survey", "Severe FI logit", "B", "sole_female_ownership", svy_sev_b, vcov(svy_sev_b)),
    coef_row("survey", "Severe FI logit", "B", "joint_ownership", svy_sev_b, vcov(svy_sev_b)),
    ame_row("survey", "Severe FI AME", "B", "sole_female_ownership", svy_sev_b, vcov(svy_sev_b), ds, ".wt"),
    ame_row("survey", "Severe FI AME", "B", "joint_ownership", svy_sev_b, vcov(svy_sev_b), ds, ".wt"),
    wald_row("survey", "Severe FI logit", "B", svy_sev_b, vcov(svy_sev_b))
  )
  res$star <- ifelse(res$p < 0.01, "***",
                     ifelse(res$p < 0.05, "**",
                            ifelse(res$p < 0.1, "*", "")))
  write.csv(res, "paper_fies_1826_results.csv", row.names = FALSE)
  cat("\nCOMPACT RESULTS (also paper_fies_1826_results.csv)\n")
  print(res, row.names = FALSE, digits = 3)
} else {
  cat("no pw_w5/svwt or ea_id; survey block skipped\n")
}

cat("\nDone. Targets: N=1826; FI any-female logit -0.264 (0.122), AME -0.052 (0.024);\n")
cat("severe any-female logit -0.399 (0.187), AME -0.037 (0.017).\n")
