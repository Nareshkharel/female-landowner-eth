"""Survey-weighted, household-clustered OLS for the FIES factor score.

The uploaded paper robustness check is in
  ETH_2021_ESPS-W5_v01_M_Stata_1/codes/paper code/merge_ownership.do
  and fies_factorpaper.txt / fiespaper_factor.txt:

    factor worried-hungry, mineigen(1)
    predict pc1
    regress pc1 ... i.saq01, vce(robust)

This script rebuilds that factor on the N=1826 sample, checks the
unweighted robust OLS against those tables, then estimates the same
models with pw_w5 and household-level linearized SEs
(svyset hh_cluster [pweight=pw_w5]).
"""
from __future__ import annotations

from math import erfc, sqrt
from pathlib import Path

import numpy as np

from estimate_hhcluster_ames import CONTROLS, REG_LEVELS, REG_NAMES, load_sample, make_X

ITEMS = [
    "worried",
    "healthy",
    "fewfoods",
    "skipped",
    "ateless",
    "wholeday",
    "ranout",
    "hungry",
]
OUT = Path(__file__).resolve().parent / "fies_factor_hhcluster_survey.txt"

# Uploaded unweighted robust OLS (paper factor tables)
PAPER_UNW = {
    "female_landowner": (-0.086, 0.046),
    "sole_female_ownership": (-0.000, 0.092),
    "joint_ownership": (-0.107, 0.048),
}


def stata_corr(X):
    """Pearson correlation matching Stata correlate (N-1)."""
    Z = X - X.mean(axis=0)
    n = X.shape[0]
    C = (Z.T @ Z) / (n - 1)
    sd = np.sqrt(np.diag(C))
    return C / np.outer(sd, sd)


def principal_factor_stata(X):
    """Stata `factor, pf mineigen(1)` then `predict` regression scores.

    Principal-factor method: SMC communalities on the diagonal of R,
    keep eigenvalues >= 1, regression scoring F = Z R^{-1} L.
    """
    n, p = X.shape
    sd = X.std(axis=0, ddof=1)
    mu = X.mean(axis=0)
    Z = (X - mu) / sd
    R = stata_corr(X)
    Rinv = np.linalg.pinv(R)
    smc = 1.0 - 1.0 / np.diag(Rinv)
    Rred = R.copy()
    np.fill_diagonal(Rred, smc)
    evals, evecs = np.linalg.eigh(Rred)
    order = np.argsort(evals)[::-1]
    evals = evals[order]
    evecs = evecs[:, order]
    keep = evals >= 1.0 - 1e-10
    if not np.any(keep):
        keep[0] = True
    L = evecs[:, keep] * np.sqrt(np.clip(evals[keep], 0, None))
    # Stata flips a column so the sum of loadings is positive
    for j in range(L.shape[1]):
        if L[:, j].sum() < 0:
            L[:, j] *= -1
    scores = Z @ Rinv @ L
    return scores[:, 0], evals, L[:, 0], smc


def pca_scores(X):
    """First principal component of the correlation matrix, positive loadings."""
    sd = X.std(axis=0, ddof=1)
    mu = X.mean(axis=0)
    Z = (X - mu) / sd
    R = stata_corr(X)
    evals, evecs = np.linalg.eigh(R)
    v = evecs[:, np.argmax(evals)]
    if v.sum() < 0:
        v = -v
    return Z @ v, evals[np.argmax(evals)], v


def wls_cluster(y, X, pw, k_adj=True):
    """Weighted OLS + household linearized VCE (1 obs per household).

    Matches svy: regress when svyset household_id [pweight=pw].
    Unweighted robust uses HC1 (n/(n-k)) when pw is all ones.
    """
    n, k = X.shape
    XtW = X.T * pw
    XtWX = XtW @ X
    try:
        b = np.linalg.solve(XtWX, XtW @ y)
    except np.linalg.LinAlgError:
        b = np.linalg.pinv(XtWX) @ (XtW @ y)
    e = y - X @ b
    u = X * (pw * e)[:, None]
    meat = u.T @ u
    XtWXinv = np.linalg.pinv(XtWX)
    # Survey linearized: n/(n-1). Unweighted robust HC1: n/(n-k).
    if np.allclose(pw, pw[0]) and k_adj:
        q = n / max(n - k, 1)
    else:
        q = n / max(n - 1, 1)
    V = q * (XtWXinv @ meat @ XtWXinv)
    se = np.sqrt(np.clip(np.diag(V), 0, None))
    t = np.divide(b, se, out=np.zeros_like(b), where=se > 0)
    p = np.array([erfc(abs(ti) / sqrt(2)) for ti in t])
    r2 = 1.0 - np.sum(pw * e * e) / np.sum(pw * (y - np.average(y, weights=pw)) ** 2)
    return b, se, p, float(r2), V


def stars(p):
    if p < 0.01:
        return "***"
    if p < 0.05:
        return "**"
    if p < 0.1:
        return "*"
    return ""


def fmt_row(name, b, se, p):
    return f"{name:<28} {b:8.3f}{stars(p):<3}  ({se:.3f})  p={p:.3f}"


def choose_score(d):
    X = np.column_stack([d[v].astype(float) for v in ITEMS])
    fac, evals, loadings, smc = principal_factor_stata(X)
    pca, pca_l, pca_v = pca_scores(X)
    return {
        "factor": (fac, evals, loadings, smc),
        "pca": (pca, pca_l, pca_v),
        "X": X,
    }


def estimate(y, d, own, controls, regions, pw, robust_unw=False):
    X, names = make_X(d, own, controls=controls, regions=regions)
    w = np.ones(len(y)) if robust_unw else pw
    b, se, p, r2, V = wls_cluster(y, X, w, k_adj=robust_unw)
    out = {nm: {"b": float(b[i]), "se": float(se[i]), "p": float(p[i])} for i, nm in enumerate(names)}
    return out, r2, names, b, V


def wald_sole_joint(names, b, V):
    i = names.index("sole_female_ownership")
    j = names.index("joint_ownership")
    diff = float(b[i] - b[j])
    var = float(V[i, i] + V[j, j] - 2.0 * V[i, j])
    w = diff * diff / var if var > 0 else 0.0
    p = float(erfc(sqrt(w / 2.0))) if w > 0 else 1.0
    return diff, w, p


def collect_models(d, y, pw):
    """Return models keyed like (outcome, family, spec) for the Word tables."""
    SPECS = [
        ("own", False, False),
        ("noreg", True, False),
        ("region", True, True),
    ]
    OWN = {
        "any": ["female_landowner"],
        "decomp": ["sole_female_ownership", "joint_ownership"],
    }
    models = {}
    for spec, controls, regions in SPECS:
        for family, owns in OWN.items():
            est, r2, names, b, V = estimate(
                y, d, owns, controls, regions, pw, robust_unw=False
            )
            models[("fies_factor", family, spec)] = est
            models[("fies_factor", family, spec)]["_r2"] = {"b": r2, "se": 0, "p": 1}
    return models


def main():
    d = load_sample()
    n = len(d["household_id"])
    pw = d["pw_w5"].astype(float)
    scored = choose_score(d)
    y_fac, evals, loadings, smc = scored["factor"]
    y_pca, pca_l, pca_v = scored["pca"]

    lines = []

    def emit(s=""):
        print(s)
        lines.append(s)

    emit("FIES factor score: survey-weighted, household-clustered OLS")
    emit("Sample: paper N=1826. Weight: pw_w5. Cluster: household (1 obs/HH).")
    emit("Outcome: Stata-style principal-factor score of the 8 FIES items")
    emit("  factor worried-hungry, mineigen(1)  ->  predict pc1")
    emit(f"N = {n}   sum(pw_w5) = {pw.sum():.6g}")
    emit(f"Factor eigenvalues (first 4): {', '.join(f'{e:.3f}' for e in evals[:4])}")
    emit(f"Kept factors with eigenvalue >= 1: {int(np.sum(evals >= 1 - 1e-10))}")
    emit("Loadings (factor 1): " + ", ".join(f"{a}={b:.3f}" for a, b in zip(ITEMS, loadings)))
    emit(f"KMO-like SMC mean = {smc.mean():.3f}  (paper reported KMO = 0.899)")
    emit(f"Factor score: mean={y_fac.mean():.4f}  sd={y_fac.std(ddof=1):.4f}")
    emit("")

    emit("===== Validation: unweighted robust OLS vs uploaded paper tables =====")
    specs_val = [
        ("female_landowner", ["female_landowner"], y_fac),
        ("decomp", ["sole_female_ownership", "joint_ownership"], y_fac),
    ]
    ok = True
    for label, owns, y in specs_val:
        est, r2, names, b, V = estimate(y, d, owns, True, True, pw, robust_unw=True)
        emit(f"  {label}  R2={r2:.3f}")
        for v in owns:
            e = est[v]
            target = PAPER_UNW[v]
            db = abs(e["b"] - target[0])
            ds = abs(e["se"] - target[1])
            flag = "OK" if db < 0.002 and ds < 0.003 else "CHECK"
            if flag != "OK":
                ok = False
            emit(
                f"    {v}: {e['b']:.3f} ({e['se']:.3f})  "
                f"paper {target[0]:.3f} ({target[1]:.3f})  "
                f"|db|={db:.4f} |dse|={ds:.4f}  {flag}"
            )
    emit(f"Unweighted validation {'passed' if ok else 'FAILED — see PCA fallback below'}")
    emit("")

    # If factor fails validation, show PCA unweighted as a diagnostic
    if not ok:
        emit("===== Diagnostic: unweighted OLS on 1st principal component =====")
        for label, owns, y in [
            ("female_landowner", ["female_landowner"], y_pca),
            ("decomp", ["sole_female_ownership", "joint_ownership"], y_pca),
        ]:
            est, r2, names, b, V = estimate(y, d, owns, True, True, pw, robust_unw=True)
            emit(f"  PCA {label} R2={r2:.3f}")
            for v in owns:
                e = est[v]
                emit(f"    {v}: {e['b']:.3f} ({e['se']:.3f})")
        emit("")

    SPECS = [
        ("own", "Ownership only", False, False),
        ("noreg", "Survey + household controls, no region", True, False),
        ("region", "Survey + household controls + region", True, True),
    ]
    OWN = {
        "any": ["female_landowner"],
        "decomp": ["sole_female_ownership", "joint_ownership"],
    }

    emit("===== Survey-weighted, household-clustered OLS (FIES factor) =====")
    emit("Higher factor score = more food-insecurity experience.")
    emit("")
    for slabel, slong, controls, regions in SPECS:
        emit(f"--- {slong} ---")
        for oname, owns in OWN.items():
            est, r2, names, b, V = estimate(y_fac, d, owns, controls, regions, pw, robust_unw=False)
            emit(f"  Spec {'A' if oname == 'any' else 'B'}  R2={r2:.3f}  k={len(names)}")
            for v in owns:
                e = est[v]
                emit(f"    {fmt_row(v, e['b'], e['se'], e['p'])}")
            if oname == "decomp":
                diff, wstat, pwald = wald_sole_joint(names, b, V)
                emit(f"    Wald sole=joint: chi2={wstat:.3f}  p={pwald:.3f}  (sole-joint={diff:.3f})")
            # print key controls in the two fuller specs
            if controls:
                for v in [
                    "male_head",
                    "wealth_index",
                    "drought_shock",
                    "total_land",
                    "livestock_hh",
                    "soil_fertility",
                    "dependency_ratio",
                    "basic_educ",
                    "sfi",
                    "non_farm_enterprise",
                ]:
                    if v in est:
                        e = est[v]
                        emit(f"    {fmt_row(v, e['b'], e['se'], e['p'])}")
            if regions:
                for lev in REG_LEVELS:
                    nm = REG_NAMES[lev]
                    if nm in est:
                        e = est[nm]
                        emit(f"    {fmt_row(nm, e['b'], e['se'], e['p'])}")
        emit("")

    emit("Notes")
    emit("  Paper uploaded tables are unweighted vce(robust) on the same factor.")
    emit("  Household clustering does not change coefficients relative to")
    emit("  survey-weighted OLS clustered at the EA; it changes SEs only.")
    emit("  One observation per household, so HH-cluster VCE = weighted HC.")
    emit("  Rebuild: python3 estimate_fies_factor_hhcluster.py")

    OUT.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"\nWrote {OUT}")


if __name__ == "__main__":
    main()
