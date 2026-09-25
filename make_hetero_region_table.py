"""Word table of regional heterogeneity AMEs from the user's Stata log."""
from __future__ import annotations

import io
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET

from make_fies_ame_survey_tables import (
    TEMPLATE,
    coef_cell,
    make_cell,
    make_note,
    make_row,
    make_title,
    se_cell,
    w,
)

OUT = Path(__file__).resolve().parent / "fies_hetero_region_tables.docx"

# AMEs from the pasted Stata log (survey, hh cluster, margins dydx)
# high_fi_reg==1: Oromia + SNNPR, N=782
# high_fi_reg==0: other regions, N=1044
HIGH = {
    "female_landowner": {"b": -0.1049891, "se": 0.0504497, "p": 0.038},
    "sole_female_ownership": {"b": -0.1605057, "se": 0.1112751, "p": 0.150},
    "joint_ownership": {"b": -0.098897, "se": 0.0512815, "p": 0.054},
}
OTHER = {
    "female_landowner": {"b": 0.0499841, "se": 0.0540508, "p": 0.355},
    "sole_female_ownership": {"b": 0.0664338, "se": 0.0877631, "p": 0.449},
    "joint_ownership": {"b": 0.0438306, "se": 0.0571585, "p": 0.443},
}

LABEL_W = 3243
NUM_W = 2200

NOTE = (
    "Notes: Average marginal effects from survey-weighted logits (pw_w5), "
    "errors clustered at the household. Both specifications include household "
    "controls and region dummies (saq01). Columns (1) and (2) restrict the sample "
    "to Oromia and SNNPR (high_fi_reg==1). Columns (3) and (4) use the other "
    "regions (high_fi_reg==0). Female landowner is from Specification A; "
    "sole and joint ownership are from Specification B. "
    "The dependent variable is moderate or severe food insecurity (FIES >= 4). "
    "Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1."
)


def data_rows(label, estimates, kind="none"):
    coef = make_row(
        [make_cell(label, LABEL_W, kind)]
        + [make_cell(coef_cell(e), NUM_W, kind, center=True) for e in estimates]
    )
    se = make_row(
        [make_cell("", LABEL_W, kind, empty_p=True)]
        + [make_cell(se_cell(e), NUM_W, kind, center=True) for e in estimates]
    )
    return [coef, se]


def build_table():
    n = 4
    cols_a = [HIGH, None, OTHER, None]
    cols_b = [None, HIGH, None, OTHER]
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

    tbl.append(
        make_row(
            [make_cell("", LABEL_W, "top", empty_p=True)]
            + [make_cell(f"({i})", NUM_W, "top", center=True) for i in range(1, 5)]
        )
    )
    tbl.append(
        make_row(
            [make_cell("Variable name", LABEL_W, "header")]
            + [
                make_cell(h, NUM_W, "header", center=True)
                for h in (
                    "Oromia and SNNPR",
                    "Oromia and SNNPR",
                    "Other regions",
                    "Other regions",
                )
            ]
        )
    )
    for row in data_rows(
        "Female landowner",
        [c.get("female_landowner") if c else None for c in cols_a],
    ):
        tbl.append(row)
    for key, label in (
        ("sole_female_ownership", "Sole female ownership"),
        ("joint_ownership", "Joint ownership"),
    ):
        for row in data_rows(label, [c.get(key) if c else None for c in cols_b]):
            tbl.append(row)

    tbl.append(
        make_row(
            [make_cell("Observations", LABEL_W, "none")]
            + [
                make_cell(s, NUM_W, "none", center=True)
                for s in ("782", "782", "1,044", "1,044")
            ]
        )
    )
    tbl.append(
        make_row(
            [make_cell("Dependent variable", LABEL_W, "bottom")]
            + [make_cell("FI", NUM_W, "bottom", center=True) for _ in range(4)]
        )
    )
    return tbl


def main():
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
            "Table. Heterogeneity of female landownership AMEs for food insecurity: "
            "Oromia and SNNPR versus other regions"
        )
    )
    body.append(build_table())
    body.append(make_note(NOTE))
    body.append(sectPr)
    buf = io.BytesIO()
    ET.ElementTree(root).write(buf, encoding="UTF-8", xml_declaration=True)
    doc_xml = buf.getvalue().replace(
        b"<?xml version='1.0' encoding='UTF-8'?>",
        b'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    )
    dest = OUT
    with zipfile.ZipFile(dest, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for name, data in other.items():
            zout.writestr(name, data)
        zout.writestr("word/document.xml", doc_xml)
    print(f"Wrote {dest}")


if __name__ == "__main__":
    main()
