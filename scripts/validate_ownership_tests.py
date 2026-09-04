#!/usr/bin/env python3
"""Validate the statistics added to female_landowner_master.do (Part 4 and 5).

Stata cannot run here, so the claims the new code rests on are checked directly
against simulated data with a from-scratch weighted logit, a cluster-robust
sandwich variance estimator, and the same Wald test the do-file reports.

Checks:
  A  female_landowner == sole_female_ownership + joint_ownership, exhaustively
     over the four (male_owner1, female_owner1) states the do-file can produce.
     This is what makes the pooled and disaggregated models nested.
  B  The pooled model is exactly the disaggregated model with b(sole)=b(joint):
     identical log-likelihood, and the unrestricted model never fits worse.
  C  Size: when sole and joint effects are truly equal, the Wald test of
     b(sole)=b(joint) rejects at about its nominal 5% rate.
  D  Power: when the effects genuinely differ, the same test rejects often.
  E  Clustering matters: with within-enumeration-area correlation, model-based
     confidence intervals under-cover while ea-clustered intervals stay near 95%.

Usage: python3 scripts/validate_ownership_tests.py
"""

from __future__ import annotations

import math

import numpy as np

NOMINAL = 0.05
Z_975 = 1.959963984540054


def fit_logit(X: np.ndarray, y: np.ndarray, w: np.ndarray) -> np.ndarray:
    """Weighted logistic MLE by Newton-Raphson."""
    beta = np.zeros(X.shape[1])
    for _ in range(200):
        p = 1.0 / (1.0 + np.exp(-(X @ beta)))
        gradient = X.T @ (w * (y - p))
        hessian = (X * (w * p * (1.0 - p))[:, None]).T @ X
        step = np.linalg.solve(hessian, gradient)
        beta += step
        if np.max(np.abs(step)) < 1e-11:
            break
    return beta


def log_likelihood(X: np.ndarray, y: np.ndarray, w: np.ndarray, beta: np.ndarray) -> float:
    eta = X @ beta
    return float(np.sum(w * (y * eta - np.logaddexp(0.0, eta))))


def vcov(
    X: np.ndarray,
    y: np.ndarray,
    w: np.ndarray,
    beta: np.ndarray,
    clusters: np.ndarray | None,
) -> np.ndarray:
    """Model-based variance, or the cluster-robust sandwich when clusters given."""
    p = 1.0 / (1.0 + np.exp(-(X @ beta)))
    bread = np.linalg.inv((X * (w * p * (1.0 - p))[:, None]).T @ X)
    if clusters is None:
        return bread

    scores = (w * (y - p))[:, None] * X
    meat = np.zeros_like(bread)
    unique = np.unique(clusters)
    for group in unique:
        total = scores[clusters == group].sum(axis=0)
        meat += np.outer(total, total)

    n_groups = len(unique)
    n_obs, n_par = X.shape
    correction = (n_groups / (n_groups - 1)) * ((n_obs - 1) / (n_obs - n_par))
    return bread @ (correction * meat) @ bread


def chi2_sf_1df(stat: float) -> float:
    """Upper tail of chi-squared with 1 degree of freedom."""
    return math.erfc(math.sqrt(stat / 2.0))


def simulate(rng, n_groups=80, per_group=20, b_sole=0.4, b_joint=0.4, sigma_cluster=0.0):
    n = n_groups * per_group
    clusters = np.repeat(np.arange(n_groups), per_group)

    male_owner = rng.binomial(1, 0.65, n)
    female_owner = rng.binomial(1, 0.45, n)
    sole = ((male_owner == 0) & (female_owner == 1)).astype(float)
    joint = ((male_owner == 1) & (female_owner == 1)).astype(float)
    pooled = (female_owner == 1).astype(float)

    x1 = rng.normal(size=n)
    x2 = rng.binomial(1, 0.4, n).astype(float)
    cluster_effect = rng.normal(0.0, sigma_cluster, n_groups)[clusters]

    eta = -0.3 + b_sole * sole + b_joint * joint + 0.5 * x1 - 0.4 * x2 + cluster_effect
    y = rng.binomial(1, 1.0 / (1.0 + np.exp(-eta))).astype(float)

    # Informative sampling weights, constant within an enumeration area.
    weights = np.exp(rng.normal(0.0, 0.4, n_groups))[clusters] * 100.0

    const = np.ones(n)
    x_pooled = np.column_stack([const, pooled, x1, x2])
    x_split = np.column_stack([const, sole, joint, x1, x2])
    return x_pooled, x_split, y, weights, clusters, sole, joint, pooled


def wald_sole_equals_joint(x_split, y, w, clusters) -> float:
    beta = fit_logit(x_split, y, w)
    covariance = vcov(x_split, y, w, beta, clusters)
    contrast = np.zeros(x_split.shape[1])
    contrast[1] = 1.0  # sole
    contrast[2] = -1.0  # joint
    difference = contrast @ beta
    variance = contrast @ covariance @ contrast
    return chi2_sf_1df(difference**2 / variance)


def check_a() -> bool:
    print("CHECK A  female_landowner == sole + joint, all four ownership states")
    ok = True
    for male in (0, 1):
        for female in (0, 1):
            sole = int(male == 0 and female == 1)
            joint = int(male == 1 and female == 1)
            pooled = int(female == 1)
            match = pooled == sole + joint
            ok &= match
            print(
                f"         male_owner1={male} female_owner1={female} -> "
                f"sole={sole} joint={joint} female_landowner={pooled} "
                f"{'ok' if match else 'MISMATCH'}"
            )
    print(f"         {'PASS' if ok else 'FAIL'}: the two specifications are nested\n")
    return bool(ok)


def check_b(rng) -> bool:
    print("CHECK B  pooled model == disaggregated model restricted to b(sole)=b(joint)")
    worst_gap = 0.0
    unrestricted_never_worse = True
    for _ in range(50):
        x_pooled, x_split, y, w, _, _, _, _ = simulate(rng)
        beta_pooled = fit_logit(x_pooled, y, w)
        beta_split = fit_logit(x_split, y, w)

        ll_restricted = log_likelihood(x_pooled, y, w, beta_pooled)
        ll_unrestricted = log_likelihood(x_split, y, w, beta_split)

        # Evaluate the disaggregated model at the restricted point: the sole and
        # joint columns collapse to female_landowner when their coefficients are
        # forced equal, so the two log-likelihoods must agree exactly.
        beta_forced = np.array(
            [beta_pooled[0], beta_pooled[1], beta_pooled[1], beta_pooled[2], beta_pooled[3]]
        )
        ll_forced = log_likelihood(x_split, y, w, beta_forced)

        worst_gap = max(worst_gap, abs(ll_forced - ll_restricted))
        unrestricted_never_worse &= ll_unrestricted >= ll_restricted - 1e-8

    ok = worst_gap < 1e-6 and unrestricted_never_worse
    print(f"         largest log-likelihood gap at the restricted point: {worst_gap:.3e}")
    print(f"         unrestricted fit never worse than restricted: {unrestricted_never_worse}")
    print(f"         {'PASS' if ok else 'FAIL'}: TEST 1 and TEST 4 are testing a real restriction\n")
    return ok


def check_c(rng, reps=500) -> bool:
    print("CHECK C  size of the Wald test when sole and joint effects are equal")
    rejections = 0
    for _ in range(reps):
        _, x_split, y, w, clusters, _, _, _ = simulate(rng, b_sole=0.4, b_joint=0.4)
        if wald_sole_equals_joint(x_split, y, w, clusters) < NOMINAL:
            rejections += 1
    rate = rejections / reps
    ok = 0.02 <= rate <= 0.09
    print(f"         rejection rate = {rate:.3f} over {reps} replications (nominal {NOMINAL})")
    print(f"         {'PASS' if ok else 'FAIL'}: the test does not over-reject\n")
    return ok


def check_d(rng, reps=500) -> bool:
    print("CHECK D  power of the Wald test when the effects genuinely differ")
    rejections = 0
    for _ in range(reps):
        _, x_split, y, w, clusters, _, _, _ = simulate(rng, b_sole=1.0, b_joint=0.0)
        if wald_sole_equals_joint(x_split, y, w, clusters) < NOMINAL:
            rejections += 1
    rate = rejections / reps
    ok = rate > 0.50
    print(f"         rejection rate = {rate:.3f} over {reps} replications")
    print(f"         {'PASS' if ok else 'FAIL'}: the test detects a real sole/joint gap\n")
    return ok


def simulate_spatial(rng, n_groups, per_group, sigma_u=1.1):
    """Enumeration areas where both ownership and the outcome are correlated.

    A single area-level factor shifts who owns land and how the outcome turns
    out, which is the situation that makes ea-level clustering necessary. When a
    regressor varies independently within an area, clustering correctly changes
    almost nothing, so a simulation without spatial correlation cannot say
    anything about whether clustering is needed.
    """
    n = n_groups * per_group
    clusters = np.repeat(np.arange(n_groups), per_group)
    area = rng.normal(0.0, sigma_u, n_groups)[clusters]

    female_owner = rng.binomial(1, 1.0 / (1.0 + np.exp(-1.6 * area)))
    male_owner = rng.binomial(1, 0.65, n)
    sole = ((male_owner == 0) & (female_owner == 1)).astype(float)
    joint = ((male_owner == 1) & (female_owner == 1)).astype(float)

    x1 = rng.normal(size=n) + 0.7 * area
    eta = -0.3 + 0.8 * sole + 0.2 * joint + 0.5 * x1 + 1.3 * area
    y = rng.binomial(1, 1.0 / (1.0 + np.exp(-eta))).astype(float)

    x_split = np.column_stack([np.ones(n), sole, joint, x1])
    return x_split, y, clusters


def check_e(rng, reps=400) -> bool:
    print("CHECK E  clustering at the enumeration area, coverage of 95% intervals")

    # Logistic coefficients are not collapsible, so the estimand is the
    # probability limit of the estimator rather than the 0.8 used to build the
    # data. Recover it from one very large sample of the same process.
    big_x, big_y, _ = simulate_spatial(rng, n_groups=20000, per_group=20)
    pseudo_true = fit_logit(big_x, big_y, np.ones(big_x.shape[0]))[1]
    print(f"         estimand (large-sample limit of b_sole) = {pseudo_true:.4f}")

    covered_model, covered_cluster = 0, 0
    se_model, se_cluster = [], []
    for _ in range(reps):
        x_split, y, clusters = simulate_spatial(rng, n_groups=60, per_group=25)
        weights = np.ones(x_split.shape[0])
        beta = fit_logit(x_split, y, weights)

        model_se = math.sqrt(vcov(x_split, y, weights, beta, None)[1, 1])
        cluster_se = math.sqrt(vcov(x_split, y, weights, beta, clusters)[1, 1])
        se_model.append(model_se)
        se_cluster.append(cluster_se)

        covered_model += abs(beta[1] - pseudo_true) <= Z_975 * model_se
        covered_cluster += abs(beta[1] - pseudo_true) <= Z_975 * cluster_se

    model_rate = covered_model / reps
    cluster_rate = covered_cluster / reps
    ok = cluster_rate > model_rate and 0.90 <= cluster_rate <= 0.98
    print(f"         mean model-based SE   = {np.mean(se_model):.4f}, coverage = {model_rate:.3f}")
    print(f"         mean ea-clustered SE  = {np.mean(se_cluster):.4f}, coverage = {cluster_rate:.3f}")
    print(f"         {'PASS' if ok else 'FAIL'}: clustering at ea_id corrects the under-coverage\n")
    return ok


def main() -> int:
    rng = np.random.default_rng(20260904)
    print("Validating the survey-weighted models and model-comparison tests\n")
    results = [check_a(), check_b(rng), check_c(rng), check_d(rng), check_e(rng)]
    if all(results):
        print("ALL CHECKS PASSED")
        return 0
    print("SOME CHECKS FAILED")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
