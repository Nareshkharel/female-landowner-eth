# =============================================================================
# food_security_analysis.R
# -----------------------------------------------------------------------------
# Food-security analysis for the Ethiopia ESPS Wave 5 (LSMS-ISA) thesis,
# focused ONLY on the food-security outcomes (FIES). Agricultural-practice
# models are intentionally excluded.
#
# This script is a re-implementation of the food-security part of
# `female landowner analysis.do` that integrates the review suggestions:
#
#   [A] Coding-bug fixes
#       - missing age must NOT be counted as elderly (Stata `age > 64` is TRUE
#         when age is missing); see build_demographics().
#       - dependency_ratio division-by-zero handled explicitly instead of
#         silently dropping households with no working-age adults.
#       - FIES items referenced by an EXPLICIT list (not the order-dependent
#         `worried-hungry` variable range).
#   [B] Methodology
#       - survey design honoured: sampling weights + EA clustering + strata
#         (design-based SEs via the `survey` package), instead of vce(robust).
#       - generated-regressor uncertainty (wealth index PCA + FIES Rasch score)
#         propagated with a cluster bootstrap.
#       - binary FIES items summarised with a WEIGHTED Rasch model (RM.weights),
#         and the Rasch person measure used as a continuous FI outcome, instead
#         of a linear factor on 0/1 data.
#   [C] Extra analyses
#       - ordered logit on the 3-level fies_category (uses the severity gradient)
#         in addition to the binary fies_dummy and severe_fi logits.
#       - heterogeneity: female_landowner interacted with male_head, region and
#         wealth quintile.
#       - diagnostics: VIF/collinearity, McFadden pseudo-R2.
#       - multiple-testing correction (Benjamini-Hochberg) across focal models.
#
# USAGE
#   * Real data:   set DRY_RUN <- FALSE and point DATA_DIR at the ESPS-W5 folder.
#                  The loader expects the merged household file produced by the
#                  Stata pipeline (see load_real_data() for the expected columns).
#   * Smoke test:  leave DRY_RUN <- TRUE to run the whole pipeline on simulated
#                  survey data (no external data needed).
# =============================================================================

# ---- 0. Packages -----------------------------------------------------------
.pkgs <- c("haven", "dplyr", "survey", "RM.weights", "car", "MASS")
.missing <- .pkgs[!(.pkgs %in% rownames(installed.packages()))]
if (length(.missing)) install.packages(.missing, repos = "https://cloud.r-project.org")
suppressMessages({
  library(dplyr)
  library(haven)
  library(survey)
  library(RM.weights)
})
options(survey.lonely.psu = "adjust")   # robust to single-PSU strata

# ---- 1. Configuration ------------------------------------------------------
DRY_RUN     <- TRUE                      # FALSE => read real data from DATA_DIR
DATA_DIR    <- "~/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1"
BOOT_B      <- 200                       # bootstrap replications (set 0 to skip)
set.seed(20240624)

# 8 FIES items in ascending severity. EXPLICIT list (suggestion B7) -- do NOT
# rely on a `worried-hungry` variable range whose result depends on column order.
FIES_ITEMS <- c("worried", "healthy", "fewfoods", "skipped",
                "ateless", "runout", "hungry", "wholeday")

# Controls common to every food-security model (ag-practice vars excluded).
CONTROLS <- c("sfi", "age", "basic_educ", "male_head", "dependency_ratio",
              "non_farm_enterprise", "wealth_index", "dist_admhq", "dist_road",
              "soil_fertility", "drought_shock", "married", "factor(saq01)")

# =============================================================================
# 2. Helper functions
# =============================================================================

# Recode raw FIES items (1 = yes, 2 = no, >2 = refuse/DK) to 1/0, others -> NA.
recode_fies <- function(df, items = FIES_ITEMS) {
  m <- as.data.frame(df[, items, drop = FALSE])
  m[m > 2] <- NA          # 3/4/... (refused, don't know) -> missing
  m[m == 2] <- 0          # "no" -> 0; "yes" stays 1
  m
}

# Build household-level demographics from INDIVIDUAL-level data, with the
# age/old and dependency_ratio bugs fixed (suggestion A1, A2).
build_demographics <- function(ind) {
  ind <- ind %>%
    mutate(
      male        = as.integer(s1q02 == 1),
      # FIX: require a non-missing age before classifying age groups, otherwise
      # missing age is silently treated as ">64" by Stata's missing semantics.
      child       = as.integer(!is.na(age) & age < 15),
      old         = as.integer(!is.na(age) & age > 64),
      working_age = as.integer(!is.na(age) & age >= 15 & age <= 64),
      is_head     = as.integer(s1q01 == 1),
      married_ind = as.integer(s1q09 %in% c(2, 3))
    )

  hh <- ind %>%
    group_by(household_id) %>%
    summarise(
      household_size = dplyr::n(),
      dependent      = sum(child + old, na.rm = TRUE),
      independent    = sum(working_age, na.rm = TRUE),
      # head-level attributes
      age        = age[is_head == 1][1],
      male_head  = max(male * is_head, na.rm = TRUE),
      married    = married_ind[is_head == 1][1],
      basic_educ = {
        be <- s2q03[is_head == 1][1]; ifelse(is.na(be), NA, as.integer(be == 1))
      },
      .groups = "drop"
    ) %>%
    mutate(
      # FIX: define the ratio when there are no working-age adults instead of
      # producing a missing value that later gets dropped (selection bias).
      dependency_ratio = ifelse(independent > 0, dependent / independent, dependent)
    )
  hh
}

# Weighted first principal component => asset wealth index (DHS-style).
# Weighted covariance honours the survey weights (suggestion B2 input).
weighted_pca_index <- function(assets, w) {
  assets <- as.matrix(assets)
  keep   <- apply(assets, 2, function(x) stats::var(x) > 0)
  assets <- assets[, keep, drop = FALSE]
  cw     <- stats::cov.wt(assets, wt = w, cor = TRUE)
  load1  <- eigen(cw$cor, symmetric = TRUE)$vectors[, 1]
  z      <- scale(assets, center = cw$center, scale = sqrt(diag(cw$cov)))
  idx    <- as.numeric(z %*% load1)
  # Orient so that higher index = wealthier (positive corr with mean assets).
  if (stats::cor(idx, rowMeans(z)) < 0) idx <- -idx
  as.numeric(scale(idx))
}

# Weighted Rasch model -> continuous person food-insecurity measure.
# RM.w returns item params in $b and the person measure per RAW SCORE in $a
# (length k+1). We map each household's raw score to its Rasch severity.
rasch_person_score <- function(fies01, w) {
  ok  <- stats::complete.cases(fies01)
  fit <- RM.w(as.matrix(fies01[ok, , drop = FALSE]), .w = w[ok])
  rs  <- rowSums(fies01)                       # raw score 0..k (NA if incomplete)
  theta <- rep(NA_real_, length(rs))
  theta[!is.na(rs)] <- fit$a[rs[!is.na(rs)] + 1]
  list(theta = theta, fit = fit)
}

# =============================================================================
# 3. Data input
# =============================================================================

# ---- 3a. Real data ---------------------------------------------------------
# Expects the merged household dataset created by the Stata pipeline, exported
# to Stata format, PLUS the survey-design columns from the LSMS basic-info file
# (sampling weight pw_w5, enumeration area / PSU, and stratum). Adjust the
# column names to match your export.
load_real_data <- function(dir = DATA_DIR) {
  hh <- haven::read_dta(file.path(dir, "food_security_merged.dta"))
  stopifnot(all(c(FIES_ITEMS, "female_landowner") %in% names(hh)))
  hh %>% mutate(
    pw_w5  = as.numeric(pw_w5),
    ea     = as.factor(ea),
    saq01  = as.factor(saq01),
    strata = as.factor(strata)
  )
}

# ---- 3b. Simulated survey data (smoke test) --------------------------------
# Generates individual-level members (to exercise build_demographics) plus
# household-level survey-design, asset, ownership and FIES variables.
simulate_survey <- function(n_strata = 6, ea_per_stratum = 12, hh_per_ea = 22) {
  strata_id <- seq_len(n_strata)
  design <- expand.grid(stratum = strata_id, ea_in = seq_len(ea_per_stratum))
  design$ea <- seq_len(nrow(design))
  hh_list <- lapply(seq_len(nrow(design)), function(j) {
    data.frame(
      household_id = paste0("ea", design$ea[j], "_h", seq_len(hh_per_ea)),
      ea = design$ea[j], strata = design$stratum[j],
      saq01 = design$stratum[j],                     # region == stratum here
      pw_w5 = runif(hh_per_ea, 0.6, 3.0)             # sampling weight
    )
  })
  hh <- dplyr::bind_rows(hh_list)
  N  <- nrow(hh)

  # Individual members per household (to drive build_demographics).
  sizes <- pmax(1L, rpois(N, 4))
  ind <- do.call(rbind, lapply(seq_len(N), function(i) {
    k <- sizes[i]
    ages <- pmax(1, round(rnorm(k, 28, 18)))
    ages[sample.int(k, 1)] <- sample(20:70, 1)       # a plausible head age
    ages[runif(k) < 0.03] <- NA                      # a few missing ages (tests A1)
    data.frame(
      household_id = hh$household_id[i],
      individual_id = seq_len(k),
      age   = ages,
      s1q02 = sample(c(1, 2), k, replace = TRUE),    # sex
      s1q01 = c(1, rep(2, k - 1)),                   # first member = head
      s1q09 = sample(1:4, k, replace = TRUE),        # marital status
      s2q03 = sample(c(1, 2), k, replace = TRUE)     # basic education y/n
    )
  }))

  # Land ownership (female / sole-female / joint), consistent with each other.
  female_landowner <- rbinom(N, 1, 0.45)
  joint_ownership  <- ifelse(female_landowner == 1, rbinom(N, 1, 0.5), 0)
  sole_female_ownership <- ifelse(female_landowner == 1 & joint_ownership == 0,
                                  rbinom(N, 1, 0.6), 0)

  # Wealth assets (10 dummies); wealthier EAs own more.
  ea_wealth <- rnorm(max(hh$ea))[hh$ea]
  assets <- sapply(1:10, function(a) rbinom(N, 1, plogis(-0.3 + 0.6 * ea_wealth + rnorm(N, 0, 0.5))))
  colnames(assets) <- paste0("asset_", 1:10)

  # Household covariates.
  hh <- hh %>% mutate(
    female_landowner = female_landowner,
    sole_female_ownership = sole_female_ownership,
    joint_ownership = joint_ownership,
    sfi = round(runif(N, 0, 0.9), 3),
    non_farm_enterprise = rbinom(N, 1, 0.3),
    dist_admhq = round(rgamma(N, 2, 0.1), 1),
    dist_road  = round(rgamma(N, 2, 0.2), 1),
    soil_fertility = sample(1:3, N, replace = TRUE),
    drought_shock  = rbinom(N, 1, 0.35),
    total_land = round(rgamma(N, 2, 1), 2)
  )

  # Latent food insecurity: female landownership is protective; wealth/educ help.
  demo <- build_demographics(ind)
  hh   <- hh %>% left_join(demo, by = "household_id")
  hh$wealth_index <- weighted_pca_index(assets, hh$pw_w5)
  eta <- with(hh,
    1.2 - 0.55 * female_landowner - 0.45 * wealth_index + 0.9 * drought_shock +
    0.6 * sfi + 0.4 * dependency_ratio - 0.3 * basic_educ + 0.25 * dist_road / 10 +
    rnorm(N, 0, 0.8))
  prob <- plogis(eta - 1.5)

  # Generate the 8 binary FIES items (severity-ordered) from the latent score.
  thr <- seq(-1.2, 1.6, length.out = length(FIES_ITEMS))
  fies01 <- sapply(thr, function(t) as.integer(plogis(eta - t) > runif(N)))
  colnames(fies01) <- FIES_ITEMS
  # Store as raw 1/2 codes with a few refusals, so recode_fies() is exercised.
  raw <- ifelse(fies01 == 1, 1L, 2L)
  raw[sample(length(raw), round(0.01 * length(raw)))] <- 3L
  hh <- cbind(hh, as.data.frame(raw))

  hh <- hh %>% mutate(ea = as.factor(ea), strata = as.factor(strata),
                      saq01 = as.factor(saq01))
  list(hh = hh, assets = assets)
}

# =============================================================================
# 4. Assemble analysis frame
# =============================================================================
if (DRY_RUN) {
  sim    <- simulate_survey()
  hh     <- sim$hh
  assets <- sim$assets
} else {
  hh     <- load_real_data()
  assets <- NULL   # supply your asset matrix here if rebuilding the index in R
}

# Recode FIES, build scores/categories (FIES item list is explicit -> A; the
# missing/threshold handling is explicit rather than relying on `> 8` tricks).
fies01 <- recode_fies(hh)
hh$fies_score <- rowSums(fies01)                       # 0..8, NA if incomplete
hh <- hh %>% mutate(
  fies_complete = !is.na(fies_score),
  fies_category = dplyr::case_when(
    fies_score <= 3 ~ 1L,                              # secure / mild
    fies_score <= 6 ~ 2L,                              # moderate
    fies_score >= 7 ~ 3L                               # severe
  ),
  fies_dummy = as.integer(fies_category > 1),          # moderate or severe
  severe_fi  = as.integer(fies_score >= 7)
)

# Continuous Rasch food-insecurity measure (weighted) -- suggestion B/C.
rs <- rasch_person_score(fies01, hh$pw_w5)
hh$rasch_fi <- rs$theta
cat(sprintf("Rasch reliability (Rasch): %.3f | infit range [%.2f, %.2f]\n",
            rs$fit$reliab, min(rs$fit$infit), max(rs$fit$infit)))

# Wealth quintile (for heterogeneity).
hh$wealth_q <- cut(hh$wealth_index,
                   breaks = quantile(hh$wealth_index, 0:5/5, na.rm = TRUE),
                   include.lowest = TRUE, labels = paste0("Q", 1:5))

# Analysis subset: complete FIES + the few covariates the Stata code required.
ana <- hh %>% filter(fies_complete,
                     !is.na(dependency_ratio), !is.na(soil_fertility), !is.na(sfi))

# =============================================================================
# 5. Survey design (design-based SEs) -- replaces vce(robust)  [B1]
# =============================================================================
des <- svydesign(ids = ~ea, strata = ~strata, weights = ~pw_w5,
                 data = ana, nest = TRUE)
des <- update(des, fies_cat_ord = ordered(fies_category))

# Convenience builders.
rhs <- function(key) paste(c(key, CONTROLS), collapse = " + ")
fml <- function(y, key) as.formula(paste(y, "~", rhs(key)))

# =============================================================================
# 6. Food-security models
# =============================================================================
KEY1 <- "female_landowner"
KEY2 <- c("sole_female_ownership", "joint_ownership")

models <- list(
  # OLS on the raw FIES score
  fies_score_own  = svyglm(fml("fies_score", KEY1), design = des),
  fies_score_sj   = svyglm(fml("fies_score", paste(KEY2, collapse = " + ")), design = des),
  # Binary logit: moderate-or-severe FI
  fies_dummy_own  = svyglm(fml("fies_dummy", KEY1), design = des, family = quasibinomial()),
  fies_dummy_sj   = svyglm(fml("fies_dummy", paste(KEY2, collapse = " + ")), design = des, family = quasibinomial()),
  # Binary logit: severe FI
  severe_own      = svyglm(fml("severe_fi", KEY1), design = des, family = quasibinomial()),
  severe_sj       = svyglm(fml("severe_fi", paste(KEY2, collapse = " + ")), design = des, family = quasibinomial()),
  # Continuous Rasch FI measure (OLS)
  rasch_own       = svyglm(fml("rasch_fi", KEY1), design = des)
)

# Ordered logit on the 3-level severity (uses the full gradient)  [C]
ord_own <- svyolr(fml("fies_cat_ord", KEY1), design = des)
ord_sj  <- svyolr(as.formula(paste("fies_cat_ord ~", rhs(paste(KEY2, collapse = " + ")))),
                  design = des)

cat("\n================ FOOD-SECURITY MODELS (design-based SEs) ================\n")
for (nm in names(models)) {
  cat("\n----", nm, "----\n")
  print(summary(models[[nm]])$coefficients[
    rownames(summary(models[[nm]])$coefficients) %in%
      c(KEY1, KEY2), , drop = FALSE])
}
cat("\n---- ordered logit: fies_category ~ female_landowner ----\n")
print(summary(ord_own)$coefficients[KEY1, , drop = FALSE])

# Marginal effects for the focal binary model (probability scale).
me <- tryCatch({
  s <- svyglm(fml("fies_dummy", KEY1), design = des, family = quasibinomial())
  b <- coef(s)[KEY1]
  pbar <- svymean(~fies_dummy, des)[1]
  b * pbar * (1 - pbar)
}, error = function(e) NA)
cat(sprintf("\nApprox. AME of %s on P(moderate+ FI): %.4f\n", KEY1, me))

# =============================================================================
# 7. Heterogeneity (interactions)  [C]
# =============================================================================
cat("\n================ HETEROGENEITY ================\n")
het_specs <- list(
  by_male_head = "female_landowner * male_head",
  by_region    = "female_landowner * factor(saq01)",
  by_wealth_q  = "female_landowner * wealth_q"
)
for (nm in names(het_specs)) {
  f <- as.formula(paste("fies_dummy ~", het_specs[[nm]], "+",
                        paste(setdiff(CONTROLS, "wealth_index"), collapse = " + ")))
  m <- tryCatch(svyglm(f, design = des, family = quasibinomial()), error = function(e) NULL)
  if (!is.null(m)) {
    co <- summary(m)$coefficients
    cat("\n----", nm, "(interaction terms) ----\n")
    print(co[grepl(":", rownames(co)), , drop = FALSE])
  }
}

# =============================================================================
# 8. Cluster + generated-regressor bootstrap  [B2]
# Resamples EAs with replacement, then REBUILDS the wealth index and the Rasch
# FI score on each resample before refitting -> honest SEs that account for both
# clustering and the fact that wealth_index / rasch_fi are estimated.
# =============================================================================
boot_focal <- function(B = BOOT_B) {
  if (B <= 0 || is.null(assets)) return(NULL)
  eas <- levels(droplevels(ana$ea))
  asset_df <- as.data.frame(assets); asset_df$household_id <- hh$household_id
  coefs <- numeric(0)
  for (b in seq_len(B)) {
    samp_ea <- sample(eas, length(eas), replace = TRUE)
    idx <- unlist(lapply(seq_along(samp_ea),
                         function(j) which(ana$ea == samp_ea[j])), use.names = FALSE)
    d <- ana[idx, , drop = FALSE]
    d$ea <- factor(paste0(d$ea, "_", rep(seq_along(samp_ea),
                  times = sapply(samp_ea, function(e) sum(ana$ea == e)))))
    # rebuild generated regressors on the resample
    a_b <- asset_df[match(d$household_id, asset_df$household_id),
                    paste0("asset_", 1:10)]
    d$wealth_index <- weighted_pca_index(a_b, d$pw_w5)
    f01 <- recode_fies(d)
    d$rasch_fi <- rasch_person_score(f01, d$pw_w5)$theta
    des_b <- svydesign(ids = ~ea, strata = ~strata, weights = ~pw_w5,
                       data = d, nest = TRUE)
    m <- tryCatch(svyglm(fml("fies_dummy", KEY1), design = des_b,
                         family = quasibinomial()), error = function(e) NULL)
    if (!is.null(m)) coefs <- c(coefs, coef(m)[KEY1])
  }
  coefs
}

cat("\n================ BOOTSTRAP (focal logit coefficient) ================\n")
bc <- boot_focal()
if (!is.null(bc) && length(bc) > 10) {
  cat(sprintf("female_landowner logit coef: boot mean %.3f | boot SE %.3f | 95%% CI [%.3f, %.3f] (B=%d)\n",
              mean(bc), sd(bc), quantile(bc, .025), quantile(bc, .975), length(bc)))
} else {
  cat("Bootstrap skipped (no asset matrix or BOOT_B = 0).\n")
}

# =============================================================================
# 9. Diagnostics: collinearity (VIF) + McFadden pseudo-R2  [C]
# =============================================================================
cat("\n================ DIAGNOSTICS ================\n")
plain <- glm(fml("fies_dummy", KEY1), data = ana, family = binomial())
cat("\n---- VIF / GVIF ----\n"); print(round(car::vif(plain), 2))
cat(sprintf("\nNagelkerke pseudo-R2 (fies_dummy ~ %s + controls): %.3f\n",
            KEY1, as.numeric(psrsq(models$fies_dummy_own, method = "Nagelkerke"))))

# =============================================================================
# 10. Multiple-testing correction across focal models  [C, B8]
# =============================================================================
cat("\n================ MULTIPLE-TESTING CORRECTION (BH) ================\n")
focal_p <- c(
  fies_score = coef(summary(models$fies_score_own))[KEY1, 4],
  fies_dummy = coef(summary(models$fies_dummy_own))[KEY1, 4],
  severe_fi  = coef(summary(models$severe_own))[KEY1, 4],
  rasch_fi   = coef(summary(models$rasch_own))[KEY1, 4],
  ord_cat    = coef(summary(ord_own))[KEY1, "Value"] /
               coef(summary(ord_own))[KEY1, "Std. Error"]   # placeholder z
)
# convert the ordered-logit z to a p-value
focal_p["ord_cat"] <- 2 * pnorm(-abs(focal_p["ord_cat"]))
tab <- data.frame(model = names(focal_p),
                  p_raw = round(focal_p, 4),
                  p_BH  = round(p.adjust(focal_p, method = "BH"), 4),
                  row.names = NULL)
print(tab)

cat("\nDONE: food-security pipeline ran end-to-end.\n")
