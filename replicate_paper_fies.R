# Rebuild codes/paper code/merge_ownership.do from the ETH folder
# and compare N=1826 / AMEs to the saved spec_paper table and the 1809 sample.
suppressPackageStartupMessages({
  library(haven)
  library(sandwich)
  library(lmtest)
  library(marginaleffects)
})

root <- if (dir.exists("ETH_2021_ESPS-W5_v01_M_Stata_1")) {
  "ETH_2021_ESPS-W5_v01_M_Stata_1"
} else if (dir.exists("../ETH_2021_ESPS-W5_v01_M_Stata_1")) {
  "../ETH_2021_ESPS-W5_v01_M_Stata_1"
} else {
  stop("ETH_2021_ESPS-W5_v01_M_Stata_1 folder not found")
}
rd <- function(p) as.data.frame(zap_labels(read_dta(p)))

fmt <- function(b, se) {
  p <- 2 * pnorm(-abs(b / se))
  stars <- if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
  sprintf("%s%.3f%s", if (b < 0) "-" else "", abs(b), stars)
}
fmtse <- function(se) sprintf("(%.3f)", se)

# --- roster dependency (Stata rules: missing age > 64 is TRUE) ---
s1 <- rd(file.path(root, "Household/sect1_hh_w5.dta"))
s1$age <- s1$s1q03a
s1$head <- as.integer(s1$s1q01 == 1)
s1$male <- as.integer(s1$s1q02 == 1)
s1$male[is.na(s1$male)] <- 0
s1$male_head <- as.integer(s1$head == 1 & s1$male == 1)
s1$male_head[is.na(s1$male_head)] <- 0
s1$married <- as.integer(s1$s1q09 %in% c(2, 3))
s1$married[is.na(s1$married)] <- 0
s1$child <- as.integer(!is.na(s1$age) & s1$age < 15)
# Stata: gen old=1 if age>64  → missing age counts as old
s1$old <- as.integer(is.na(s1$age) | s1$age > 64)
s1$working_age <- as.integer(!is.na(s1$age) & s1$age > 14 & s1$age < 65)

agg <- aggregate(
  cbind(household_size = 1, dependent = s1$child + s1$old, independent = s1$working_age) ~ household_id,
  data = s1, FUN = sum
)
agg$dependency_ratio <- ifelse(agg$independent > 0, agg$dependent / agg$independent, NA_real_)

heads <- s1[s1$head == 1, c("household_id", "individual_id", "age", "male_head", "married", "saq01")]
# if multiple heads, keep first
heads <- heads[!duplicated(heads$household_id), ]
heads <- merge(heads, agg, by = "household_id")
cat("Heads with roster dep:", nrow(heads),
    " missing dep:", sum(is.na(heads$dependency_ratio)), "\n")

# education from sect2 on the head
s2 <- rd(file.path(root, "Household/sect2_hh_w5.dta"))
s2$basic_educ <- s2$s2q03
s2$basic_educ[s2$basic_educ == 2] <- 0
heads <- merge(heads, s2[, c("household_id", "individual_id", "basic_educ")],
               by = c("household_id", "individual_id"))
cat("After sect2 head match:", nrow(heads), "\n")

# --- inner merges used by the paper do-file ---
inner <- function(d, path, extra = NULL) {
  u <- rd(path)
  keep <- unique(c("household_id", extra, intersect(names(u), names(d))))
  # avoid name clashes: take new-file columns that we need
  need <- unique(c("household_id", extra))
  u <- u[, intersect(need, names(u)), drop = FALSE]
  before <- nrow(d)
  d2 <- merge(d, u, by = "household_id")
  cat(sprintf("  merge %s: %d -> %d  (dropped %d)\n",
              basename(path), before, nrow(d2), before - nrow(d2)))
  d2
}

d <- heads
# shocks, NFE, assistance, wealth, fies, ownership, geo, hdds, area, agri
for (step in list(
  list(file.path(root, "hh_9_w5.dta"), c("drought_shock", "shock_faced")),
  list(file.path(root, "hh_12a_w5.dta"), c("non_farm_enterprise")),
  list(file.path(root, "hh_14.dta"), c("psnp_assistance")),
  list(file.path(root, "hh11_w5_pca.dta"), c("wealth_index")),
  list(file.path(root, "fies_dta.dta"),
       c("worried", "healthy", "fewfoods", "skipped", "ateless", "wholeday", "ranout", "hungry")),
  list(file.path(root, "Household/female_ownership.dta"),
       c("female_landowner", "sole_female_ownership", "joint_ownership",
         "soil_fertility", "farm_type")),
  list(file.path(root, "Household_geographical.dta"),
       c("dist_admhq", "dist_road")),
  list(file.path(root, "hdds_ethiopia.dta"), c("hdds_household")),
  list(file.path(root, "area_ethiopia.dta"), c("sfi", "total_land")),
  list(file.path(root, "agri_practices.dta"), c("chemical_fertilizer", "crop_rotation"))
)) {
  d <- inner(d, step[[1]], step[[2]])
}

d$fies_score <- with(d, worried + healthy + fewfoods + skipped + ateless + wholeday + ranout + hungry)
cat("After all merges:", nrow(d), " fies_score>8:", sum(d$fies_score > 8, na.rm = TRUE),
    " fies missing:", sum(is.na(d$fies_score)), "\n")

d <- subset(d, !is.na(fies_score) & fies_score <= 8)
cat("After fies_score<=8:", nrow(d), "\n")
cat("  missing dep:", sum(is.na(d$dependency_ratio)),
    " soil:", sum(is.na(d$soil_fertility)),
    " sfi:", sum(is.na(d$sfi)), "\n")

d0 <- d
d <- subset(d, !is.na(dependency_ratio) & !is.na(soil_fertility) & !is.na(sfi))
cat("PAPER SAMPLE N =", nrow(d), "\n")

d$fies_dummy <- as.integer(d$fies_score >= 4)
d$severe_fi <- as.integer(d$fies_score > 6)
d$livestock_hh <- as.integer(d$farm_type %in% c(1, 3))
d$livestock_true <- as.integer(d$farm_type %in% c(2, 3))
d$saq01 <- factor(d$saq01)

cat("\nSummary vs summary_paper.txt (target N=1826):\n")
for (v in c("age", "male_head", "married", "dependency_ratio", "basic_educ",
            "drought_shock", "non_farm_enterprise", "wealth_index", "soil_fertility",
            "female_landowner", "sfi", "total_land", "fies_dummy", "severe_fi",
            "livestock_hh")) {
  cat(sprintf("  %-22s n=%d  mean=%.4f  sd=%.3f\n",
              v, sum(!is.na(d[[v]])), mean(d[[v]], na.rm = TRUE), sd(d[[v]], na.rm = TRUE)))
}
cat("farm_type tab:\n"); print(table(d$farm_type, useNA = "ifany"))
cat("livestock_hh (type 1|3) mean:", mean(d$livestock_hh),
    " livestock true (2|3) mean:", mean(d$livestock_true), "\n")

# --- compare to 1809 sample ids ---
fies_h <- rd("/workspace/fies_household.dta")
own_w <- rd("/workspace/female_ownership.dta")
area_w <- rd("/workspace/area_ethiopia.dta")
w <- merge(fies_h, own_w[, intersect(c("household_id", "soil_fertility"), names(own_w))],
           by = "household_id")
w <- merge(w, area_w[, c("household_id", "sfi")], by = "household_id")
if (!"dependency_ratio" %in% names(w)) {
  w$dep_new <- ifelse(w$active_hh_member > 0,
                      (w$household_size - w$active_hh_member) / w$active_hh_member, NA)
} else w$dep_new <- w$dependency_ratio
w1809 <- w[!is.na(w$sfi) & !is.na(w$dep_new) & !is.na(w$soil_fertility) &
             !is.na(w$fies_score) & w$fies_score <= 8, ]
cat("\nWorkspace 1809-style N:", nrow(w1809), "\n")
cat("In paper, not in 1809:", sum(!d$household_id %in% w1809$household_id), "\n")
cat("In 1809, not in paper:", sum(!w1809$household_id %in% d$household_id), "\n")

extra <- d[!d$household_id %in% w1809$household_id, ]
miss1809 <- w1809[!w1809$household_id %in% d$household_id, ]
cat("\nExtra in paper (should be ~17 if N=1826):\n")
if (nrow(extra)) {
  m <- merge(extra[, c("household_id", "age", "household_size", "independent",
                       "dependency_ratio", "female_landowner")],
             w[, c("household_id", "active_hh_member")], by = "household_id", all.x = TRUE)
  print(data.frame(m, row.names = NULL))
  cat("  all extra have independent>0:", all(extra$independent > 0),
      "  active==0 among those in fies_h:",
      sum(m$active_hh_member == 0, na.rm = TRUE), "\n")
}

# --- fit paper logits and AMEs ---
rhs_a <- paste("female_landowner + sfi + age + basic_educ + male_head +",
               "dependency_ratio + non_farm_enterprise + wealth_index +",
               "soil_fertility + drought_shock + married + total_land +",
               "livestock_hh + saq01")
rhs_b <- sub("female_landowner", "sole_female_ownership + joint_ownership", rhs_a, fixed = TRUE)

need <- c("fies_dummy", "severe_fi", "female_landowner", "sole_female_ownership",
          "joint_ownership", "sfi", "age", "basic_educ", "male_head",
          "dependency_ratio", "non_farm_enterprise", "wealth_index",
          "soil_fertility", "drought_shock", "married", "total_land",
          "livestock_hh", "saq01")
cat("\nComplete-case on paper regressors:", sum(complete.cases(d[, need])), "\n")
dd <- d[complete.cases(d[, need]), ]
cat("Estimation N:", nrow(dd), "\n")

fit_ame <- function(y, rhs, label) {
  m <- glm(as.formula(paste(y, "~", rhs)), data = dd, family = binomial())
  s <- avg_slopes(m, vcov = vcovHC(m, type = "HC0"))
  cat("\n====", label, "N=", nobs(m), "====\n")
  keep <- c("female_landowner", "sole_female_ownership", "joint_ownership",
            "sfi", "age", "basic_educ", "male_head", "dependency_ratio",
            "non_farm_enterprise", "wealth_index", "soil_fertility",
            "drought_shock", "married", "total_land", "livestock_hh")
  s2 <- s[s$term %in% keep, ]
  for (i in seq_len(nrow(s2))) {
    cat(sprintf("  %-24s %s (%s)\n", s2$term[i],
                fmt(s2$estimate[i], s2$std.error[i]),
                sprintf("%.3f", s2$std.error[i])))
  }
  invisible(list(m = m, s = s))
}

a1 <- fit_ame("fies_dummy", rhs_a, "FI Spec A")
a2 <- fit_ame("fies_dummy", rhs_b, "FI Spec B")
a3 <- fit_ame("severe_fi", rhs_a, "Sev FI Spec A")
a4 <- fit_ame("severe_fi", rhs_b, "Sev FI Spec B")

# saved paper targets
cat("\n==== TARGET spec_paper.txt ====\n")
cat("female_landowner FI -0.052 (0.024)  Sev -0.037 (0.017)\n")
cat("joint            FI -0.063 (0.025)  Sev -0.048 (0.020)\n")
cat("sole             FI -0.010 (0.044)  Sev -0.006 (0.027)\n")
cat("N 1826\n")
