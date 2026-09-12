"""AIC/BIC for survey-weighted logits on the N=1826 paper sample.

Compares Spec A (female_landowner) with Spec B (sole + joint).
Clustering does not enter the likelihood, so household-clustered and
EA-clustered survey models share the same AIC/BIC. Weights do.

Two likelihood scales:
  unweighted  — matches Stata `logit ..., vce(robust)` log pseudolikelihood
  pweight_N   — pw_w5 rescaled to sum to N (Stata `logit, pweight` / AIC scale)

AIC = -2*LL + 2k
BIC = -2*LL + k*ln(N)
k includes the intercept.

Spec B is nested in Spec A when sole and joint partition female_landowner
(no overlap): Spec A is Spec B with b_sole = b_joint. The LR statistic is
2*(LL_B - LL_A) ~ chi2(1) under that restriction.
"""
from __future__ import annotations

from math import log
from pathlib import Path

import numpy as np

from estimate_hhcluster_ames import load_sample, logit_irls_w, make_X

OUT = Path(__file__).resolve().parent / "aic_bic_hhcluster_survey.txt"

# Stata Step 5 unweighted +region log pseudolikelihoods (validation)
STATA_UNW = {
    ("fies_dummy", "any", "region"): -1044.0852,
    ("fies_dummy", "decomp", "region"): -1043.4074,
    ("severe_fi", "any", "region"): -560.10096,
    ("severe_fi", "decomp", "region"): -559.0382,
}

OUTCOMES = [
    ("fies_dummy", "Moderate or severe FI"),
    ("severe_fi", "Severe FI"),
]
SPECS = [
    ("own", "Ownership only", False, False),
    ("noreg", "Survey + household controls, no region", True, False),
    ("region", "Survey + household controls + region", True, True),
]
OWN = {
    "any": ["female_landowner"],
    "decomp": ["sole_female_ownership", "joint_ownership"],
}


def loglik(y, p, w):
    p = np.clip(p, 1e-15, 1.0 - 1e-15)
    return float(np.sum(w * (y * np.log(p) + (1.0 - y) * np.log(1.0 - p))))


def chi2_sf_1(x):
    """Survival function of chi-square(1) = erfc(sqrt(x/2))."""
    from math import erfc, sqrt

    if x <= 0:
        return 1.0
    return float(erfc(sqrt(x / 2.0)))


def fit_ll(y, X, w):
    b, p, V = logit_irls_w(y, X, w)
    return loglik(y, p, w), int(X.shape[1]), b, V


def wald_sole_eq_joint(names, b, V):
    """Household-cluster Wald for H0: b_sole = b_joint (1 obs per HH)."""
    i = names.index("sole_female_ownership")
    j = names.index("joint_ownership")
    diff = float(b[i] - b[j])
    var = float(V[i, i] + V[j, j] - 2.0 * V[i, j])
    if var <= 0:
        return diff, 0.0, 1.0
    w = diff * diff / var
    # F(1, N-1) ≈ chi2(1) at N=1826; report chi2 p-value
    return diff, w, chi2_sf_1(w)


def main():
    d = load_sample()
    n = len(d["household_id"])
    pw = d["pw_w5"].astype(float)
    pw_n = pw * (n / pw.sum())
    ones = np.ones(n)

    sole = d["sole_female_ownership"].astype(float)
    joint = d["joint_ownership"].astype(float)
    anyf = d["female_landowner"].astype(float)
    overlap = int(np.sum((sole == 1) & (joint == 1)))
    mismatch = int(np.sum(((sole == 1) | (joint == 1)) != (anyf == 1)))

    lines = []

    def emit(s=""):
        print(s)
        lines.append(s)

    emit("AIC / BIC for the N=1826 paper sample")
    emit("Survey weight: pw_w5 (normalized to sum to N for AIC/BIC)")
    emit("Cluster: household (does not change AIC/BIC; same likelihood as EA cluster)")
    emit(f"N = {n}")
    emit(f"sum(pw_w5) = {pw.sum():.6g}")
    emit(f"sole=1: {int(sole.sum())}  joint=1: {int(joint.sum())}  any female=1: {int(anyf.sum())}")
    emit(f"sole and joint overlap: {overlap}   any != sole|joint: {mismatch}")
    emit("Spec A = female_landowner     Spec B = sole + joint")
    emit("AIC = -2*LL + 2k     BIC = -2*LL + k*ln(N)     k includes intercept")
    emit("")

    emit("===== Validation: unweighted +region LL vs Stata Step 5 =====")
    ok = True
    for (yname, own, spec), target in STATA_UNW.items():
        y = d[yname].astype(float)
        X, _ = make_X(d, OWN[own], controls=True, regions=True)
        ll, k, _, _ = fit_ll(y, X, ones)
        diff = abs(ll - target)
        flag = "OK" if diff < 5e-4 else "MISMATCH"
        if flag != "OK":
            ok = False
        emit(f"  {yname:12} {own:7} LL={ll:.6f}  Stata={target:.6f}  |d|={diff:.2e}  {flag}  k={k}")
    emit(f"Validation {'passed' if ok else 'FAILED'}")
    emit("")

    emit("===== Validation: hh-cluster Wald sole=joint vs Stata svy +region =====")
    stata_wald = {
        "fies_dummy": (0.0215554, 0.9524),
        "severe_fi": (0.6559951, 0.1484),
    }
    for yname, (stata_diff, stata_p) in stata_wald.items():
        y = d[yname].astype(float)
        Xb, names_b = make_X(d, OWN["decomp"], controls=True, regions=True)
        _, _, b_b, V_b = fit_ll(y, Xb, pw)
        diff, wald, p_wald = wald_sole_eq_joint(names_b, b_b, V_b)
        flag = "OK" if abs(diff - stata_diff) < 1e-4 and abs(p_wald - stata_p) < 0.02 else "CHECK"
        emit(
            f"  {yname:12} sole-joint={diff:.6f} (Stata {stata_diff:.6f})  "
            f"Wald chi2={wald:.3f} p={p_wald:.4f} (Stata p={stata_p:.4f})  {flag}"
        )
    emit("")

    rows = []
    for yname, ylabel in OUTCOMES:
        y = d[yname].astype(float)
        emit(f"===== {ylabel} ({yname}) =====")
        emit(
            f"{'spec':<42} {'kA':>3} {'kB':>3} "
            f"{'LL_A':>10} {'LL_B':>10} {'dLL':>8} "
            f"{'AIC_A':>10} {'AIC_B':>10} {'dAIC':>8} "
            f"{'BIC_A':>10} {'BIC_B':>10} {'dBIC':>8} "
            f"{'LR_chi2':>8} {'LR_p':>7} "
            f"{'Wald_p':>7} {'AIC pref':>10} {'BIC pref':>10}"
        )
        for spec, slabel, controls, regions in SPECS:
            Xa, _ = make_X(d, OWN["any"], controls=controls, regions=regions)
            Xb, names_b = make_X(d, OWN["decomp"], controls=controls, regions=regions)
            ll_a, k_a, _, _ = fit_ll(y, Xa, pw_n)
            ll_b, k_b, _, _ = fit_ll(y, Xb, pw_n)
            # Same point estimates with raw pw; VCE is the svy/hh-cluster sandwich
            _, _, b_b, V_b = fit_ll(y, Xb, pw)
            aic_a = -2 * ll_a + 2 * k_a
            aic_b = -2 * ll_b + 2 * k_b
            bic_a = -2 * ll_a + k_a * log(n)
            bic_b = -2 * ll_b + k_b * log(n)
            dll = ll_b - ll_a
            daic = aic_a - aic_b  # >0 => B better (lower AIC)
            dbic = bic_a - bic_b
            lr = 2.0 * dll
            p_lr = chi2_sf_1(lr)
            _, wald, p_wald = wald_sole_eq_joint(names_b, b_b, V_b)
            aic_pref = "sole+joint" if aic_b < aic_a - 1e-9 else (
                "tie" if abs(aic_b - aic_a) < 1e-9 else "female_own"
            )
            bic_pref = "sole+joint" if bic_b < bic_a - 1e-9 else (
                "tie" if abs(bic_b - bic_a) < 1e-9 else "female_own"
            )
            emit(
                f"{slabel:<42} {k_a:3d} {k_b:3d} "
                f"{ll_a:10.3f} {ll_b:10.3f} {dll:8.3f} "
                f"{aic_a:10.2f} {aic_b:10.2f} {daic:8.2f} "
                f"{bic_a:10.2f} {bic_b:10.2f} {dbic:8.2f} "
                f"{lr:8.3f} {p_lr:7.3f} "
                f"{p_wald:7.3f} {aic_pref:>10} {bic_pref:>10}"
            )
            rows.append(
                {
                    "outcome": ylabel,
                    "spec": slabel,
                    "k_a": k_a,
                    "k_b": k_b,
                    "ll_a": ll_a,
                    "ll_b": ll_b,
                    "aic_a": aic_a,
                    "aic_b": aic_b,
                    "bic_a": bic_a,
                    "bic_b": bic_b,
                    "lr": lr,
                    "p_lr": p_lr,
                    "p_wald": p_wald,
                    "aic_pref": aic_pref,
                    "bic_pref": bic_pref,
                }
            )
        emit("")

    emit("Notes")
    emit("  dLL  = LL(sole+joint) - LL(female_landowner); always >= 0 if nested.")
    emit("  dAIC = AIC_A - AIC_B; positive means sole+joint has lower AIC.")
    emit("  dBIC = BIC_A - BIC_B; positive means sole+joint has lower BIC.")
    emit("  Rule of thumb: |dAIC|<2 essentially equivalent; >10 strong preference.")
    emit("  BIC penalizes the extra sole/joint parameter by ln(N) = "
         f"{log(n):.3f}.")
    emit("  LR tests H0: b_sole = b_joint (Spec A nested in Spec B); ignores clustering.")
    emit("  Wald_p is the household-cluster sandwich test of the same restriction.")
    emit("  Household clustering changes SEs/Wald, not AIC/BIC.")

    OUT.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"\nWrote {OUT}")
    return rows


if __name__ == "__main__":
    main()
