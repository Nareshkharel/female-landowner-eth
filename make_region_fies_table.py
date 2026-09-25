"""Word table of survey-weighted FIES status by region."""
from __future__ import annotations

import io
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET

from make_fies_ame_survey_tables import (
    TEMPLATE,
    make_cell,
    make_note,
    make_row,
    make_title,
    w,
)
from region_fies_descriptives import ORDER, shares
from estimate_hhcluster_ames import load_sample

OUT = Path(__file__).resolve().parent / "region_fies_descriptives.docx"

LABEL_W = 2400
NUM_W = 1800
N_W = 1600

NOTE = (
    "Notes: Percentages use the ESPS household survey weight (pw_w5). "
    "N is the unweighted estimation sample. "
    "Food secure = FIES score 0–3; moderate food insecurity = 4–6; "
    "severe food insecurity = 7–8. "
    "Weights restore each region’s population share. They do not balance "
    "household characteristics across regions."
)

HEADERS = [
    "Food secure (%)",
    "Moderate food insecurity (%)",
    "Severe food insecurity (%)",
    "Number of observations",
]


def header_row():
    cells = [make_cell("Region", LABEL_W, "header")]
    widths = [NUM_W, NUM_W, NUM_W, N_W]
    for name, width in zip(HEADERS, widths):
        cells.append(make_cell(name, width, "header", center=True))
    return make_row(cells)


def top_rule():
    cells = [make_cell("", LABEL_W, "top", empty_p=True)]
    for width in (NUM_W, NUM_W, NUM_W, N_W):
        cells.append(make_cell("", width, "top", empty_p=True))
    return make_row(cells)


def data_row(name, sec, mod, sev, n, kind="none"):
    return make_row(
        [
            make_cell(name, LABEL_W, kind),
            make_cell(f"{sec:.2f}", NUM_W, kind, center=True),
            make_cell(f"{mod:.2f}", NUM_W, kind, center=True),
            make_cell(f"{sev:.2f}", NUM_W, kind, center=True),
            make_cell(f"{n:,}", N_W, kind, center=True),
        ]
    )


def build_table(rows, total):
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
    for width in [LABEL_W, NUM_W, NUM_W, NUM_W, N_W]:
        gc = ET.SubElement(grid, w("gridCol"))
        gc.set(w("w"), str(width))
    tbl.append(top_rule())
    tbl.append(header_row())
    for i, row in enumerate(rows):
        tbl.append(data_row(*row, kind="none"))
    tbl.append(data_row(*total, kind="bottom"))
    return tbl


def write_docx(rows, total, dest: Path) -> Path:
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
            "Table. Food security status by region (survey-weighted percentages)"
        )
    )
    body.append(build_table(rows, total))
    body.append(make_note(NOTE))
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
    saq = d["saq01"].astype(float)
    score = d["fies_score"].astype(float)
    pw = d["pw_w5"].astype(float)
    secure = score < 4
    moderate = (score >= 4) & (score <= 6)
    severe = score > 6
    import numpy as np

    rows = []
    for code, name in ORDER:
        sec, mod, sev, n, _ = shares(saq == code, secure, moderate, severe, pw)
        rows.append((name, sec, mod, sev, n))
    tot = shares(np.ones(len(saq), dtype=bool), secure, moderate, severe, pw)
    total = ("All regions", tot[0], tot[1], tot[2], tot[3])
    dest = write_docx(rows, total, OUT)
    print(f"Wrote {dest}")
    for name, sec, mod, sev, n in rows + [total]:
        print(f"{name:<14} {sec:6.2f} {mod:6.2f} {sev:6.2f} {n:5d}")


if __name__ == "__main__":
    main()
