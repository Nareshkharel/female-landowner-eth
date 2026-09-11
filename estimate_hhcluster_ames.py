"""Rebuild the N=1826 sample and estimate survey-weighted AMEs
with household-level linearized SEs (svyset hh_cluster [pweight=pw_w5]).

Used to fill own-only and no-region columns that did not finish in
paper_fies_1826_hhclustered.log (that log died on a missing psu).
"""
from __future__ import annotations

import struct
from math import erfc
from pathlib import Path

import numpy as np

ETH = Path(__file__).resolve().parent / "ETH_2021_ESPS-W5_v01_M_Stata_1"
TYPE_MAP = {
    65526: ("<f8", 8),
    65527: ("<f4", 4),
    65528: ("<i4", 4),
    65529: ("<i2", 2),
    65530: ("<i1", 1),
}
CONTROLS = [
    "sfi",
    "age",
    "basic_educ",
    "male_head",
    "dependency_ratio",
    "non_farm_enterprise",
    "wealth_index",
    "soil_fertility",
    "drought_shock",
    "married",
    "total_land",
    "livestock_hh",
]
REG_LEVELS = [3, 4, 5, 6, 7, 12, 13, 15]
REG_NAMES = {
    3: "3. AMHARA",
    4: "4. OROMIA",
    5: "5. SOMALI",
    6: "6. BENISHANGUL GUMUZ",
    7: "7. SNNP",
    12: "12. GAMBELA",
    13: "13. HARAR",
    15: "15. DIRE DAWA",
}


def _tag(buf: bytes, name: str, start: int = 0):
    open_t = f"<{name}>".encode()
    close_t = f"</{name}>".encode()
    i = buf.find(open_t, start)
    j = buf.find(close_t, i + len(open_t))
    return buf[i + len(open_t) : j], j + len(close_t)


def _miss(arr, typ):
    a = arr.astype(np.float64, copy=True)
    if typ == 65530:
        a[arr > 100] = np.nan
    elif typ == 65529:
        a[arr > 32740] = np.nan
    elif typ == 65528:
        a[arr > 2147483620] = np.nan
    elif typ == 65527:
        a[a >= 1.0e30] = np.nan
        a[~np.isfinite(a)] = np.nan
    else:
        a[a >= 8.9884656743e307] = np.nan
        a[~np.isfinite(a)] = np.nan
    return a


def read_dta(path, columns):
    buf = Path(path).read_bytes()
    hdr, _ = _tag(buf, "header")
    kraw, _ = _tag(hdr, "K")
    nraw, _ = _tag(hdr, "N")
    K = struct.unpack("<H", kraw)[0]
    N = struct.unpack("<Q", nraw)[0]
    types_raw, _ = _tag(buf, "variable_types")
    types = struct.unpack("<" + "H" * K, types_raw)
    names_raw, _ = _tag(buf, "varnames")
    names = [
        names_raw[i * 129 : (i + 1) * 129].split(b"\x00", 1)[0].decode("utf-8", "replace")
        for i in range(K)
    ]
    data_raw, _ = _tag(buf, "data")
    widths = [t if t <= 2045 else TYPE_MAP[t][1] for t in types]
    rowsize = sum(widths)
    offsets = np.cumsum([0] + widths[:-1]).tolist()
    idx = {n: i for i, n in enumerate(names)}
    out = {}
    for col in columns:
        if col not in idx:
            raise KeyError(f"{path}: no {col}. names={names[:20]}")
        j = idx[col]
        t = types[j]
        off = offsets[j]
        if t <= 2045:
            sl = np.empty(N, dtype=object)
            for r in range(N):
                raw = data_raw[r * rowsize + off : r * rowsize + off + t]
                sl[r] = raw.split(b"\x00", 1)[0].decode("utf-8", "replace")
            out[col] = sl
        else:
            dt = np.dtype({"names": ["v"], "formats": [TYPE_MAP[t][0]], "offsets": [off], "itemsize": rowsize})
            arr = np.frombuffer(data_raw, dtype=dt, count=N)["v"]
            out[col] = _miss(np.ascontiguousarray(arr), t)
    return out


def to_index(ids):
    return {str(x): i for i, x in enumerate(ids)}


def inner_join(left, right, key="household_id"):
    idx = to_index(right[key])
    keep, rpos = [], []
    for i, hid in enumerate(left[key]):
        j = idx.get(str(hid))
        if j is not None:
            keep.append(i)
            rpos.append(j)
    keep = np.asarray(keep)
    rpos = np.asarray(rpos)
    out = {}
    nleft = len(left[key])
    for k, v in left.items():
        out[k] = v[keep] if len(v) == nleft else v
    nright = len(right[key])
    for k, v in right.items():
        if k == key:
            continue
        out[k] = v[rpos] if len(v) == nright else v
    return out


def load_sample():
    d = read_dta(
        ETH / "merged_w_fies.dta",
        ["household_id", "ea_id", "pw_w5", "saq01", "age", "basic_educ", "male_head",
         "dependency_ratio", "married"],
    )
    d = inner_join(d, read_dta(ETH / "hh_9_w5.dta", ["household_id", "drought_shock"]))
    d = inner_join(d, read_dta(ETH / "hh_12a_w5.dta", ["household_id", "non_farm_enterprise"]))
    d = inner_join(d, read_dta(ETH / "hh_14.dta", ["household_id"]))
    d = inner_join(d, read_dta(ETH / "hh11_w5_pca.dta", ["household_id", "wealth_index"]))
    d = inner_join(
        d,
        read_dta(
            ETH / "fies_dta.dta",
            ["household_id", "worried", "healthy", "fewfoods", "skipped",
             "ateless", "wholeday", "ranout", "hungry"],
        ),
    )
    d = inner_join(
        d,
        read_dta(
            ETH / "Household/female_ownership.dta",
            ["household_id", "female_landowner", "sole_female_ownership",
             "joint_ownership", "soil_fertility", "farm_type"],
        ),
    )
    geo = ETH / "Household_geographical.dta"
    if not geo.exists():
        geo = ETH / "household_geographical.dta"
    d = inner_join(d, read_dta(geo, ["household_id", "dist_admhq", "dist_road"]))
    d = inner_join(d, read_dta(ETH / "hdds_ethiopia.dta", ["household_id"]))
    d = inner_join(d, read_dta(ETH / "area_ethiopia.dta", ["household_id", "sfi", "total_land"]))
    d = inner_join(d, read_dta(ETH / "agri_practices.dta", ["household_id"]))

    score = (
        d["worried"] + d["healthy"] + d["fewfoods"] + d["skipped"]
        + d["ateless"] + d["wholeday"] + d["ranout"] + d["hungry"]
    )
    d["fies_score"] = score
    d["livestock_hh"] = np.where(
        np.isfinite(d["farm_type"]),
        ((d["farm_type"] == 1) | (d["farm_type"] == 3)).astype(float),
        np.nan,
    )
    d["fies_dummy"] = np.where(np.isfinite(score), (score >= 4).astype(float), np.nan)
    d["severe_fi"] = np.where(np.isfinite(score), (score > 6).astype(float), np.nan)

    n0 = len(d["household_id"])
    mask = np.isfinite(score) & (score <= 8)
    need = CONTROLS + [
        "female_landowner",
        "sole_female_ownership",
        "joint_ownership",
        "saq01",
        "pw_w5",
        "fies_dummy",
        "severe_fi",
    ]
    for v in need:
        mask &= np.isfinite(d[v].astype(float))
    for k in list(d):
        d[k] = d[k][mask]
    return d


def make_X(d, own_vars, *, controls: bool, regions: bool):
    n = len(d["fies_dummy"])
    cols = [np.ones(n)]
    names = ["_cons"]
    for v in own_vars:
        cols.append(np.asarray(d[v], dtype=float))
        names.append(v)
    if controls:
        for v in CONTROLS:
            cols.append(np.asarray(d[v], dtype=float))
            names.append(v)
    if regions:
        saq = np.asarray(d["saq01"], dtype=float)
        for lev in REG_LEVELS:
            cols.append((saq == lev).astype(float))
            names.append(REG_NAMES[lev])
    return np.column_stack(cols), names


def logit_irls_w(y, X, pw, maxiter=80, tol=1e-12):
    n, k = X.shape
    b = np.zeros(k)
    for _ in range(maxiter):
        xb = np.clip(X @ b, -30, 30)
        p = 1.0 / (1.0 + np.exp(-xb))
        var = np.clip(p * (1.0 - p), 1e-12, None)
        w = pw * var
        z = xb + (y - p) / var
        XtW = X.T * w
        H = XtW @ X
        try:
            b_new = np.linalg.solve(H, XtW @ z)
        except np.linalg.LinAlgError:
            b_new = np.linalg.pinv(H) @ (XtW @ z)
        if np.max(np.abs(b_new - b)) < tol:
            b = b_new
            break
        b = b_new
    xb = np.clip(X @ b, -30, 30)
    p = 1.0 / (1.0 + np.exp(-xb))
    var = np.clip(p * (1.0 - p), 1e-12, None)
    H = (X.T * (pw * var)) @ X
    Hinv = np.linalg.pinv(H)
    u = X * (pw * (y - p))[:, None]
    meat = u.T @ u
    nobs = len(y)
    V = Hinv @ meat @ Hinv * (nobs / (nobs - 1))
    return b, p, V


def ames(y, X, names, pw):
    """Stata margins, dydx(*) after svy: derivative AME = b_j * mean_w(p(1-p)).

    Region dummies created with i.saq01 use a discrete change from the base
    and are taken from the Stata log when present.
    """
    b, p, V = logit_irls_w(y, X, pw)
    wsum = float(np.sum(pw))
    dens = p * (1.0 - p)
    mean_dens = float(np.sum(pw * dens) / wsum)
    d_dens = (dens * (1.0 - 2.0 * p))[:, None] * X
    out = {}
    for j, name in enumerate(names):
        if name == "_cons":
            continue
        if name in REG_NAMES.values():
            X1 = X.copy()
            X0 = X.copy()
            for lev, rname in REG_NAMES.items():
                X1[:, names.index(rname)] = 1.0 if rname == name else 0.0
                X0[:, names.index(rname)] = 0.0
            p1 = 1.0 / (1.0 + np.exp(-np.clip(X1 @ b, -30, 30)))
            p0 = 1.0 / (1.0 + np.exp(-np.clip(X0 @ b, -30, 30)))
            ame = float(np.sum(pw * (p1 - p0)) / wsum)
            g = np.sum(
                pw[:, None] * ((p1 * (1 - p1))[:, None] * X1 - (p0 * (1 - p0))[:, None] * X0),
                axis=0,
            ) / wsum
        else:
            ame = float(b[j] * mean_dens)
            g = np.zeros(len(b))
            g[j] += mean_dens
            g += b[j] * np.sum(pw[:, None] * d_dens, axis=0) / wsum
        se = float(np.sqrt(max(g @ V @ g, 0.0)))
        t = ame / se if se > 0 else 0.0
        pval = float(erfc(abs(t) / np.sqrt(2)))
        out[name] = {"b": ame, "se": se, "p": pval}
    return out


def estimate_all():
    d = load_sample()
    n = len(d["household_id"])
    print(f"sample N={n}")
    if n != 1826:
        print("WARNING: expected 1826")
    y_fi = d["fies_dummy"].astype(float)
    y_sev = d["severe_fi"].astype(float)
    pw = d["pw_w5"].astype(float)
    specs = {
        ("fies_dummy", "any", "own"): (y_fi, ["female_landowner"], False, False),
        ("fies_dummy", "any", "noreg"): (y_fi, ["female_landowner"], True, False),
        ("fies_dummy", "any", "region"): (y_fi, ["female_landowner"], True, True),
        ("fies_dummy", "decomp", "own"): (y_fi, ["sole_female_ownership", "joint_ownership"], False, False),
        ("fies_dummy", "decomp", "noreg"): (y_fi, ["sole_female_ownership", "joint_ownership"], True, False),
        ("fies_dummy", "decomp", "region"): (y_fi, ["sole_female_ownership", "joint_ownership"], True, True),
        ("severe_fi", "any", "own"): (y_sev, ["female_landowner"], False, False),
        ("severe_fi", "any", "noreg"): (y_sev, ["female_landowner"], True, False),
        ("severe_fi", "any", "region"): (y_sev, ["female_landowner"], True, True),
        ("severe_fi", "decomp", "own"): (y_sev, ["sole_female_ownership", "joint_ownership"], False, False),
        ("severe_fi", "decomp", "noreg"): (y_sev, ["sole_female_ownership", "joint_ownership"], True, False),
        ("severe_fi", "decomp", "region"): (y_sev, ["sole_female_ownership", "joint_ownership"], True, True),
    }
    models = {}
    for key, (y, owns, controls, regions) in specs.items():
        X, names = make_X(d, owns, controls=controls, regions=regions)
        models[key] = ames(y, X, names, pw)
        own = "female_landowner" if "female_landowner" in models[key] else "joint_ownership"
        e = models[key][own]
        print(f"{key} {own} {e['b']:.6f} ({e['se']:.6f}) p={e['p']:.3f}")
    return models


if __name__ == "__main__":
    estimate_all()
