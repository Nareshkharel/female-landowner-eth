"""Unweighted vs survey-weighted FIES status by region (N=1826 paper sample).

Unweighted shares replicate the uploaded regional descriptive table.
Weighted shares use pw_w5. Weights correct for sampling probability;
they do not balance covariates across regions.
"""
from __future__ import annotations

from pathlib import Path

import numpy as np

from estimate_hhcluster_ames import load_sample

OUT = Path(__file__).resolve().parent / "region_fies_descriptives.txt"

# Paper table order
ORDER = [
    (2, "Afar"),
    (3, "Amhara"),
    (6, "Benishangul"),
    (15, "Dire Dawa"),
    (12, "Gambela"),
    (13, "Harar"),
    (4, "Oromia"),
    (7, "SNNPR"),
    (5, "Somali"),
]
PAPER = {
    "Afar": (74.55, 9.09, 16.36, 55),
    "Amhara": (74.62, 20.08, 5.30, 264),
    "Benishangul": (76.00, 20.00, 4.00, 100),
    "Dire Dawa": (66.41, 27.34, 6.25, 128),
    "Gambela": (79.50, 18.63, 1.86, 161),
    "Harar": (83.63, 13.45, 2.92, 171),
    "Oromia": (49.46, 28.53, 22.01, 368),
    "SNNPR": (50.72, 30.68, 18.60, 414),
    "Somali": (53.94, 26.67, 19.39, 165),
}


def shares(mask, secure, moderate, severe, pw=None):
    if pw is None:
        w = np.ones(int(mask.sum()))
    else:
        w = pw[mask]
    ws = float(w.sum())
    return (
        100.0 * float(np.sum(w * secure[mask]) / ws),
        100.0 * float(np.sum(w * moderate[mask]) / ws),
        100.0 * float(np.sum(w * severe[mask]) / ws),
        int(mask.sum()),
        ws,
    )


def main():
    d = load_sample()
    saq = d["saq01"].astype(float)
    score = d["fies_score"].astype(float)
    pw = d["pw_w5"].astype(float)
    secure = score < 4
    moderate = (score >= 4) & (score <= 6)
    severe = score > 6

    lines = []

    def emit(s=""):
        print(s)
        lines.append(s)

    emit("FIES status by region, paper sample N=1,826")
    emit("Food secure: FIES 0-3.  Moderate: 4-6.  Severe: 7-8.")
    emit("")
    emit("===== Unweighted (replicate uploaded table) =====")
    emit(f"{'Region':<14} {'Secure':>8} {'Moderate':>9} {'Severe':>8} {'N':>6}  match")
    ok_all = True
    for code, name in ORDER:
        sec, mod, sev, n, _ = shares(saq == code, secure, moderate, severe)
        t = PAPER[name]
        flag = (
            "OK"
            if abs(sec - t[0]) < 0.02
            and abs(mod - t[1]) < 0.02
            and abs(sev - t[2]) < 0.02
            and n == t[3]
            else "DIFF"
        )
        if flag != "OK":
            ok_all = False
        emit(f"{name:<14} {sec:8.2f} {mod:9.2f} {sev:8.2f} {n:6d}  {flag}")
    sec, mod, sev, n, _ = shares(np.ones(len(saq), bool), secure, moderate, severe)
    emit(f"{'All regions':<14} {sec:8.2f} {mod:9.2f} {sev:8.2f} {n:6d}")
    emit(f"Replication {'passed' if ok_all else 'FAILED'}")
    emit("")
    emit("===== Survey-weighted (pw_w5) =====")
    emit(
        f"{'Region':<14} {'Secure':>8} {'Moderate':>9} {'Severe':>8} "
        f"{'N':>6} {'Wt share':>9}"
    )
    wtot = float(pw.sum())
    for code, name in ORDER:
        sec, mod, sev, n, ws = shares(saq == code, secure, moderate, severe, pw)
        emit(f"{name:<14} {sec:8.2f} {mod:9.2f} {sev:8.2f} {n:6d} {100*ws/wtot:8.2f}%")
    sec, mod, sev, n, _ = shares(np.ones(len(saq), bool), secure, moderate, severe, pw)
    emit(f"{'All regions':<14} {sec:8.2f} {mod:9.2f} {sev:8.2f} {n:6d}   100.00%")
    emit("")
    emit("Notes")
    emit("  Categories match fies_dummy (FIES>=4) and severe_fi (FIES>6).")
    emit("  Weights rescale each region to its population share; they do not")
    emit("  balance household characteristics across regions.")
    emit("  Oromia + SNNPR are 42.8% of the sample but 72.2% of the weight.")
    OUT.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"\nWrote {OUT}")


if __name__ == "__main__":
    main()
