"""Word tables for survey-weighted, household-clustered FIES factor OLS.

Produces a 6-column table in the same layout as the paper Table 8/9
(Spec A in columns 1-3, Spec B in columns 4-6) plus two 3-column tables.
"""
from __future__ import annotations

import io
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET

from estimate_fies_factor_hhcluster import choose_score, collect_models, load_sample
from make_fies_ame_survey_tables import (
    CONTROLS,
    REGIONS,
    TEMPLATE,
    build_table,
    coef_cell,
    fmt3,
    lookup,
    make_cell,
    make_note,
    make_row,
    make_title,
    se_cell,
    w,
)

OUT = Path(__file__).resolve().parent / "fies_factor_hhcluster_tables.docx"

NOTE = (
    "Notes: OLS coefficients. The dependent variable is the first principal-factor "
    "score of the eight FIES items (Stata: factor worried-hungry, mineigen(1); predict pc1). "
    "A higher score means more food-insecurity experience. Survey-weighted (pw_w5), "
    "errors clustered at the household. Columns (1) and (4) include only the ownership "
    "variable. Columns (2) and (5) add household controls, without region dummies. "
    "Columns (3) and (6) add region dummies (saq01). Standard errors in parentheses. "
    "*** p<0.01, ** p<0.05, * p<0.1."
)

NOTE3 = (
    "Notes: OLS coefficients. The dependent variable is the first principal-factor "
    "score of the eight FIES items (Stata: factor worried-hungry, mineigen(1); predict pc1). "
    "A higher score means more food-insecurity experience. Survey-weighted (pw_w5), "
    "errors clustered at the household. Column (1) includes only the ownership variable. "
    "Column (2) adds household controls, without region dummies. Column (3) adds region "
    "dummies (saq01). Standard errors in parentheses. "
    "*** p<0.01, ** p<0.05, * p<0.1."
)

LABEL_W = 2800
NUM_W = 1100


def _data_rows(label: str, estimates: list[dict | None], kind: str = "none"):
    n = len(estimates)
    coef_row = make_row(
        [make_cell(label, LABEL_W, kind)]
        + [make_cell(coef_cell(e), NUM_W, kind, center=True) for e in estimates]
    )
    se_row = make_row(
        [make_cell("", LABEL_W, kind, empty_p=True)]
        + [make_cell(se_cell(e), NUM_W, kind, center=True) for e in estimates]
    )
    return [coef_row, se_row]


def _header_numbers(n: int):
    cells = [make_cell("", LABEL_W, "top", empty_p=True)]
    for i in range(1, n + 1):
        cells.append(make_cell(f"({i})", NUM_W, "top", center=True))
    return make_row(cells)


def _header_names(names: list[str]):
    cells = [make_cell("Variable name", LABEL_W, "header")]
    for name in names:
        cells.append(make_cell(name, NUM_W, "header", center=True))
    return make_row(cells)


def _regions_header(n: int):
    cells = [make_cell("Regions", LABEL_W, "none", bold=True)]
    for _ in range(n):
        cells.append(make_cell("", NUM_W, "none", center=True, empty_p=True))
    return make_row(cells)


def _obs_r2_rows(n: int, r2s: list[float | None]):
    obs = make_row(
        [make_cell("Observations", LABEL_W, "none")]
        + [make_cell("1,826", NUM_W, "none", center=True) for _ in range(n)]
    )
    r2_row = make_row(
        [make_cell("R-squared", LABEL_W, "bottom")]
        + [
            make_cell(fmt3(r) if r is not None else "", NUM_W, "bottom", center=True)
            for r in r2s
        ]
    )
    return [obs, r2_row]


def build_six_col(models: dict) -> ET.Element:
    keys = [
        ("fies_factor", "any", "own"),
        ("fies_factor", "any", "noreg"),
        ("fies_factor", "any", "region"),
        ("fies_factor", "decomp", "own"),
        ("fies_factor", "decomp", "noreg"),
        ("fies_factor", "decomp", "region"),
    ]
    cols = [models[k] for k in keys]
    n = 6
    tbl = ET.Element(w("tbl"))
    tblPr = ET.SubElement(tbl, w("tblPr"))
    tblW = ET.SubElement(tblPr, w("tblW"))
    tblW.set(w("w"), "0")
    tblW.set(w("type"), "auto")
    ET.SubElement(tblPr, w("jc")).set(w("val"), "center")
    ET.SubElement(tblPr, w("tblLayout")).set(w("type"), "fixed")
    mar = ET.SubElement(tblPr, w("tblCellMar"))
    left = ET.SubElement(mar, w("left"))
    left.set(w("w"), "75")
    left.set(w("type"), "dxa")
    right = ET.SubElement(mar, w("right"))
    right.set(w("w"), "75")
    right.set(w("type"), "dxa")
    look = ET.SubElement(tblPr, w("tblLook"))
    look.set(w("val"), "0000")
    look.set(w("firstRow"), "0")
    look.set(w("lastRow"), "0")
    look.set(w("firstColumn"), "0")
    look.set(w("lastColumn"), "0")
    look.set(w("noHBand"), "0")
    look.set(w("noVBand"), "0")
    grid = ET.SubElement(tbl, w("tblGrid"))
    for width in [LABEL_W] + [NUM_W] * n:
        gc = ET.SubElement(grid, w("gridCol"))
        gc.set(w("w"), str(width))

    tbl.append(_header_numbers(n))
    tbl.append(_header_names(["FIES factor"] * n))

    for row in _data_rows(
        "Female landowner",
        [lookup(cols[i], "female_landowner") if i < 3 else None for i in range(6)],
    ):
        tbl.append(row)
    for key, label in (
        ("sole_female_ownership", "Sole female ownership"),
        ("joint_ownership", "Joint ownership"),
    ):
        for row in _data_rows(
            label,
            [None if i < 3 else lookup(cols[i], key) for i in range(6)],
        ):
            tbl.append(row)

    for key, label in CONTROLS:
        ests = [
            None if i in (0, 3) else lookup(cols[i], key)
            for i in range(6)
        ]
        for row in _data_rows(label, ests):
            tbl.append(row)

    tbl.append(_regions_header(n))
    for key, label in REGIONS:
        ests = [
            lookup(cols[i], key) if i in (2, 5) else None
            for i in range(6)
        ]
        for row in _data_rows(label, ests):
            tbl.append(row)

    r2s = []
    for c in cols:
        r = c.get("_r2")
        r2s.append(r["b"] if r else None)
    for row in _obs_r2_rows(n, r2s):
        tbl.append(row)
    return tbl


def write_factor_docx(models: dict, dest: Path) -> Path:
    with zipfile.ZipFile(TEMPLATE) as zin:
        xml = zin.read("word/document.xml")
        other = {name: zin.read(name) for name in zin.namelist() if name != "word/document.xml"}

    root = ET.fromstring(xml)
    body = root.find(w("body"))
    sectPr = body.find(w("sectPr"))
    body.remove(sectPr)
    for child in list(body):
        body.remove(child)

    body.append(
        make_title(
            "Table. OLS estimates for the FIES factor score using two specifications "
            "accounting for women’s landownership"
        )
    )
    body.append(build_six_col(models))
    body.append(make_note(NOTE))

    # Also the two 3-column tables in the same style as the FI / Sev FI Word file
    body.append(make_title("Table 1. Female landowner and the FIES factor score"))
    body.append(build_table(models, "fies_factor", "any", "FIES factor"))
    body.append(make_note(NOTE3))
    body.append(make_title("Table 2. Sole and joint ownership and the FIES factor score"))
    body.append(build_table(models, "fies_factor", "decomp", "FIES factor"))
    body.append(make_note(NOTE3))
    body.append(sectPr)

    buf = io.BytesIO()
    ET.ElementTree(root).write(buf, encoding="UTF-8", xml_declaration=True)
    doc_xml = buf.getvalue().replace(
        b"<?xml version='1.0' encoding='UTF-8'?>",
        b'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    )
    with zipfile.ZipFile(dest, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for name, data in other.items():
            zout.writestr(name, data)
        zout.writestr("word/document.xml", doc_xml)
    return dest


def main():
    d = load_sample()
    y = choose_score(d)["factor"][0]
    pw = d["pw_w5"].astype(float)
    models = collect_models(d, y, pw)
    dest = write_factor_docx(models, OUT)
    print(f"Wrote {dest}")
    for family, spec, key in (
        ("any", "own", "female_landowner"),
        ("any", "noreg", "female_landowner"),
        ("any", "region", "female_landowner"),
        ("decomp", "own", "joint_ownership"),
        ("decomp", "noreg", "joint_ownership"),
        ("decomp", "region", "joint_ownership"),
    ):
        e = models[("fies_factor", family, spec)][key]
        print(f"{family} {spec} {key}: {e['b']:.3f} ({e['se']:.3f}) p={e['p']:.3f}")


if __name__ == "__main__":
    main()
