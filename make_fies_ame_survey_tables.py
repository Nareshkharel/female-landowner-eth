"""
Build Word AME tables from paper_fies_1826_easyform.log.

Layout matches format reference.docx:
  Times New Roman, centered fixed-width table, (1)/(2)/(3) headers,
  coefficient then SE in parentheses, 3 decimals, * p<0.1 ** p<0.05 *** p<0.01,
  top rule / header rule / bottom rule, Observations = 1,826.

Columns
  (1) survey-weighted, ownership only
  (2) survey-weighted, household controls, no region
  (3) survey-weighted, household controls and region dummies (saq01)

Four tables: female landowner x {FI, Sev FI}, sole/joint x {FI, Sev FI}.
"""
from __future__ import annotations

import io
import re
import zipfile
from decimal import ROUND_HALF_UP, Decimal
from pathlib import Path
from xml.etree import ElementTree as ET

ROOT = Path(__file__).resolve().parent
LOG_PATH = ROOT / "paper_fies_1826_easyform.log"
TEMPLATE = ROOT / "format reference.docx"
OUT_PATH = ROOT / "fies_ame_survey_tables.docx"

W = "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}"
W14 = "{http://schemas.microsoft.com/office/word/2010/wordml}"
NSMAP = {
    "wpc": "http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas",
    "cx": "http://schemas.microsoft.com/office/drawing/2014/chartex",
    "cx1": "http://schemas.microsoft.com/office/drawing/2015/9/8/chartex",
    "cx2": "http://schemas.microsoft.com/office/drawing/2015/10/21/chartex",
    "cx3": "http://schemas.microsoft.com/office/drawing/2016/5/9/chartex",
    "cx4": "http://schemas.microsoft.com/office/drawing/2016/5/10/chartex",
    "cx5": "http://schemas.microsoft.com/office/drawing/2016/5/11/chartex",
    "cx6": "http://schemas.microsoft.com/office/drawing/2016/5/12/chartex",
    "cx7": "http://schemas.microsoft.com/office/drawing/2016/5/13/chartex",
    "cx8": "http://schemas.microsoft.com/office/drawing/2016/5/14/chartex",
    "mc": "http://schemas.openxmlformats.org/markup-compatibility/2006",
    "m": "http://schemas.openxmlformats.org/officeDocument/2006/math",
    "aink": "http://schemas.microsoft.com/office/drawing/2016/ink",
    "am3d": "http://schemas.microsoft.com/office/drawing/2017/model3d",
    "oel": "http://schemas.microsoft.com/office/2019/extlst",
    "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
    "wp14": "http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing",
    "wp": "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing",
    "wpg": "http://schemas.microsoft.com/office/word/2010/wordprocessingGroup",
    "wpi": "http://schemas.microsoft.com/office/word/2010/wordprocessingInk",
    "wne": "http://schemas.microsoft.com/office/word/2006/wordml",
    "wps": "http://schemas.microsoft.com/office/word/2010/wordprocessingShape",
    "w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
    "w14": "http://schemas.microsoft.com/office/word/2010/wordml",
    "w15": "http://schemas.microsoft.com/office/word/2012/wordml",
    "w16cex": "http://schemas.microsoft.com/office/word/2018/wordml/cex",
    "w16cid": "http://schemas.microsoft.com/office/word/2016/wordml/cid",
    "w16": "http://schemas.microsoft.com/office/word/2018/wordml",
    "w16du": "http://schemas.microsoft.com/office/word/2023/wordml/word16du",
    "w16sdtdh": "http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash",
    "w16se": "http://schemas.microsoft.com/office/word/2015/wordml/symex",
}


def w(tag: str) -> str:
    return W + tag


for prefix, uri in NSMAP.items():
    ET.register_namespace(prefix, uri)


CONTROLS = [
    ("sfi", "SFI"),
    ("age", "Age"),
    ("basic_educ", "Basic education"),
    ("male_head", "Male head"),
    ("dependency_ratio", "Dependency ratio"),
    ("non_farm_enterprise", "Non-farm enterprise"),
    ("wealth_index", "Asset index"),
    ("soil_fertility", "Soil fertility"),
    ("drought_shock", "Drought "),
    ("married", "Married"),
    ("total_land", "Total land"),
    ("livestock_hh", "Livestock Ownership"),
]

REGIONS = [
    ("3. AMHARA", "Amhara "),
    ("4. OROMIA", "Oromia"),
    ("5. SOMALI", "Somali"),
    ("6. BENISHANGUL GUMUZ", "Benishangul"),
    ("7. SNNP", "SNNPR"),
    ("12. GAMBELA", "Gambela"),
    ("13. HARAR", "Harar"),
    ("15. DIRE DAWA", "Dire Dawa"),
]

REGION_ALIASES = {
    "3.saq01": "3. AMHARA",
    "4.saq01": "4. OROMIA",
    "5.saq01": "5. SOMALI",
    "6.saq01": "6. BENISHANGUL GUMUZ",
    "7.saq01": "7. SNNP",
    "12.saq01": "12. GAMBELA",
    "13.saq01": "13. HARAR",
    "15.saq01": "15. DIRE DAWA",
}


def fmt3(x: float) -> str:
    q = Decimal(str(x)).quantize(Decimal("0.001"), rounding=ROUND_HALF_UP)
    return f"{q:.3f}"


def stars(p: float) -> str:
    if p < 0.01:
        return "***"
    if p < 0.05:
        return "**"
    if p < 0.10:
        return "*"
    return ""


def coef_cell(est: dict | None) -> str:
    if not est:
        return ""
    return f"{fmt3(est['b'])}{stars(est['p'])}"


def se_cell(est: dict | None) -> str:
    if not est:
        return ""
    return f"({fmt3(est['se'])})"


NUM = r"(-?(?:\d+\.\d+|\.\d+))"
COEF_LINE = re.compile(
    rf"^\s*(?P<name>.+?)\s+\|\s+{NUM}\s+{NUM}\s+{NUM}\s+{NUM}"
)


def parse_ames(log_text: str) -> dict:
    """Last survey AME table for each (outcome, family, spec)."""
    lines = log_text.splitlines()
    models: dict[tuple[str, str, str], dict[str, dict]] = {}
    i = 0
    n = len(lines)
    while i < n:
        if not lines[i].lstrip().startswith("Average marginal effects"):
            i += 1
            continue
        end = min(i + 120, n)
        chunk = "\n".join(lines[i:end])
        if "Number of strata" not in chunk:
            i += 1
            continue
        if "Pr(fies_dummy)" in chunk:
            outcome = "fies_dummy"
        elif "Pr(severe_fi)" in chunk:
            outcome = "severe_fi"
        else:
            i += 1
            continue
        # Start of coefficient rows: line after the dy/dx header rule
        j = i
        while j < end and not re.search(r"dy/dx\s+std\. err\.", lines[j]):
            j += 1
        j += 1
        if j < n and set(lines[j].strip()) <= set("-"):
            j += 1
        coefs: dict[str, dict] = {}
        while j < n:
            line = lines[j]
            if line.startswith("Note:") or line.startswith(". "):
                break
            if set(line.strip()) <= set("-+"):
                if coefs:
                    break
                j += 1
                continue
            m = COEF_LINE.search(line)
            if m:
                name = re.sub(r"\s+", " ", m.group("name")).strip()
                name = REGION_ALIASES.get(name, name)
                coefs[name] = {
                    "b": float(m.group(2)),
                    "se": float(m.group(3)),
                    "p": float(m.group(5)),
                }
            j += 1
        if outcome and coefs:
            keys = set(coefs)
            if "3. AMHARA" in keys or any(k.endswith("saq01") for k in keys):
                spec = "region"
            elif "sfi" in keys:
                spec = "noreg"
            else:
                spec = "own"
            if "sole_female_ownership" in keys:
                family = "decomp"
            elif "female_landowner" in keys:
                family = "any"
            else:
                family = "other"
            models[(outcome, family, spec)] = coefs
        i = max(j, i + 1)
    return models


def make_pPr(center: bool) -> ET.Element:
    pPr = ET.Element(w("pPr"))
    for name in ("widowControl", "autoSpaceDE", "autoSpaceDN", "adjustRightInd"):
        el = ET.SubElement(pPr, w(name))
        el.set(w("val"), "0")
    if center:
        ET.SubElement(pPr, w("jc")).set(w("val"), "center")
    return pPr


def times_fonts(rPr: ET.Element) -> None:
    fonts = ET.SubElement(rPr, w("rFonts"))
    fonts.set(w("ascii"), "Times New Roman")
    fonts.set(w("hAnsi"), "Times New Roman")
    fonts.set(w("eastAsia"), "Times New Roman")


def make_run(text: str, *, bold: bool = False) -> ET.Element:
    r = ET.Element(w("r"))
    rPr = ET.SubElement(r, w("rPr"))
    times_fonts(rPr)
    if bold:
        ET.SubElement(rPr, w("b"))
        ET.SubElement(rPr, w("bCs"))
    t = ET.SubElement(r, w("t"))
    if text.startswith(" ") or text.endswith(" "):
        t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    t.text = text
    return r


def borders(kind: str) -> ET.Element:
    """kind: top, header, none, bottom"""
    tcBorders = ET.Element(w("tcBorders"))
    sides = ("top", "left", "bottom", "right")
    spec = {
        "top": ("single", "nil", "nil", "nil"),
        "header": ("nil", "nil", "single", "nil"),
        "none": ("nil", "nil", "nil", "nil"),
        "bottom": ("nil", "nil", "single", "nil"),
    }[kind]
    for side, val in zip(sides, spec):
        el = ET.SubElement(tcBorders, w(side))
        if val == "single":
            el.set(w("val"), "single")
            el.set(w("sz"), "6")
            el.set(w("space"), "0")
            el.set(w("color"), "auto")
        else:
            el.set(w("val"), "nil")
    return tcBorders


def make_cell(
    text: str,
    width: int,
    kind: str,
    *,
    center: bool = False,
    bold: bool = False,
    empty_p: bool = False,
) -> ET.Element:
    tc = ET.Element(w("tc"))
    tcPr = ET.SubElement(tc, w("tcPr"))
    tcW = ET.SubElement(tcPr, w("tcW"))
    tcW.set(w("w"), str(width))
    tcW.set(w("type"), "dxa")
    tcPr.append(borders(kind))
    p = ET.SubElement(tc, w("p"))
    p.append(make_pPr(center))
    if not empty_p and text != "":
        p.append(make_run(text, bold=bold))
    return tc


def make_row(cells: list[ET.Element]) -> ET.Element:
    tr = ET.Element(w("tr"))
    trPr = ET.SubElement(tr, w("trPr"))
    ET.SubElement(trPr, w("jc")).set(w("val"), "center")
    for c in cells:
        tr.append(c)
    return tr


LABEL_W = 3243
NUM_W = 1440


def data_rows(label: str, estimates: list[dict | None], kind: str = "none") -> list[ET.Element]:
    coef_row = make_row(
        [make_cell(label, LABEL_W, kind)]
        + [make_cell(coef_cell(e), NUM_W, kind, center=True) for e in estimates]
    )
    se_row = make_row(
        [make_cell("", LABEL_W, kind, empty_p=True)]
        + [make_cell(se_cell(e), NUM_W, kind, center=True) for e in estimates]
    )
    return [coef_row, se_row]


def header_row_numbers(n: int) -> ET.Element:
    cells = [make_cell("", LABEL_W, "top", empty_p=True)]
    for i in range(1, n + 1):
        cells.append(make_cell(f"({i})", NUM_W, "top", center=True))
    return make_row(cells)


def header_row_names(names: list[str]) -> ET.Element:
    cells = [make_cell("Variable name", LABEL_W, "header")]
    for name in names:
        cells.append(make_cell(name, NUM_W, "header", center=True))
    return make_row(cells)


def regions_header(n: int) -> ET.Element:
    cells = [make_cell("Regions", LABEL_W, "none", bold=True)]
    for _ in range(n):
        cells.append(make_cell("", NUM_W, "none", center=True, empty_p=True))
    return make_row(cells)


def obs_row(n: int) -> ET.Element:
    cells = [make_cell("Observations", LABEL_W, "bottom")]
    for _ in range(n):
        cells.append(make_cell("1,826", NUM_W, "bottom", center=True))
    return make_row(cells)


def lookup(model: dict | None, key: str) -> dict | None:
    if not model:
        return None
    if key in model:
        return model[key]
    return None


def three_cols(models: dict, outcome: str, family: str) -> list[dict | None]:
    return [
        models.get((outcome, family, "own")),
        models.get((outcome, family, "noreg")),
        models.get((outcome, family, "region")),
    ]


def build_table(models: dict, outcome: str, family: str, outcome_label: str) -> ET.Element:
    cols = three_cols(models, outcome, family)
    n = 3
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

    tbl.append(header_row_numbers(n))
    tbl.append(header_row_names([outcome_label] * n))

    if family == "any":
        ests = [lookup(c, "female_landowner") for c in cols]
        for row in data_rows("Female landowner", ests):
            tbl.append(row)
    else:
        for key, label in (
            ("sole_female_ownership", "Sole female ownership"),
            ("joint_ownership", "Joint ownership"),
        ):
            ests = [lookup(c, key) for c in cols]
            for row in data_rows(label, ests):
                tbl.append(row)

    for key, label in CONTROLS:
        # column 1 has no controls
        ests = [None, lookup(cols[1], key), lookup(cols[2], key)]
        for row in data_rows(label, ests):
            tbl.append(row)

    tbl.append(regions_header(n))
    for key, label in REGIONS:
        ests = [None, None, lookup(cols[2], key)]
        for row in data_rows(label, ests):
            tbl.append(row)

    tbl.append(obs_row(n))
    return tbl


def make_title(text: str) -> ET.Element:
    p = ET.Element(w("p"))
    pPr = ET.SubElement(p, w("pPr"))
    ET.SubElement(pPr, w("jc")).set(w("val"), "left")
    spacing = ET.SubElement(pPr, w("spacing"))
    spacing.set(w("before"), "240")
    spacing.set(w("after"), "120")
    r = ET.SubElement(p, w("r"))
    rPr = ET.SubElement(r, w("rPr"))
    fonts = ET.SubElement(rPr, w("rFonts"))
    fonts.set(w("ascii"), "Times New Roman")
    fonts.set(w("hAnsi"), "Times New Roman")
    fonts.set(w("eastAsia"), "Times New Roman")
    ET.SubElement(rPr, w("b"))
    sz = ET.SubElement(rPr, w("sz"))
    sz.set(w("val"), "24")
    t = ET.SubElement(r, w("t"))
    t.text = text
    return p


def make_note(text: str) -> ET.Element:
    p = ET.Element(w("p"))
    pPr = ET.SubElement(p, w("pPr"))
    spacing = ET.SubElement(pPr, w("spacing"))
    spacing.set(w("before"), "60")
    spacing.set(w("after"), "200")
    r = ET.SubElement(p, w("r"))
    rPr = ET.SubElement(r, w("rPr"))
    fonts = ET.SubElement(rPr, w("rFonts"))
    fonts.set(w("ascii"), "Times New Roman")
    fonts.set(w("hAnsi"), "Times New Roman")
    fonts.set(w("eastAsia"), "Times New Roman")
    sz = ET.SubElement(rPr, w("sz"))
    sz.set(w("val"), "20")
    t = ET.SubElement(r, w("t"))
    t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    t.text = text
    return p


NOTE = (
    "Notes: Average marginal effects from survey-weighted logits (pw_w5, clustered at the EA). "
    "Column (1) includes only the ownership variable. Column (2) adds household controls, without region dummies. "
    "Column (3) adds region dummies (saq01). Standard errors in parentheses. "
    "*** p<0.01, ** p<0.05, * p<0.1."
)

TABLES = [
    ("Table 1. Female landowner and moderate or severe food insecurity", "fies_dummy", "any", "FI"),
    ("Table 2. Female landowner and severe food insecurity", "severe_fi", "any", "Sev FI"),
    ("Table 3. Sole and joint ownership and moderate or severe food insecurity", "fies_dummy", "decomp", "FI"),
    ("Table 4. Sole and joint ownership and severe food insecurity", "severe_fi", "decomp", "Sev FI"),
]


def write_docx(models: dict) -> None:
    with zipfile.ZipFile(TEMPLATE) as zin:
        xml = zin.read("word/document.xml")
        other = {name: zin.read(name) for name in zin.namelist() if name != "word/document.xml"}

    root = ET.fromstring(xml)
    body = root.find(w("body"))
    sectPr = body.find(w("sectPr"))
    body.remove(sectPr)
    for child in list(body):
        body.remove(child)

    for title, outcome, family, olabel in TABLES:
        body.append(make_title(title))
        body.append(build_table(models, outcome, family, olabel))
        body.append(make_note(NOTE))
    body.append(sectPr)

    buf = io.BytesIO()
    ET.ElementTree(root).write(buf, encoding="UTF-8", xml_declaration=True)
    doc_xml = buf.getvalue().replace(
        b"<?xml version='1.0' encoding='UTF-8'?>",
        b'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    )
    with zipfile.ZipFile(OUT_PATH, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for name, data in other.items():
            zout.writestr(name, data)
        zout.writestr("word/document.xml", doc_xml)


def print_preview(models: dict) -> None:
    required = [
        ("fies_dummy", "any", "own"),
        ("fies_dummy", "any", "noreg"),
        ("fies_dummy", "any", "region"),
        ("fies_dummy", "decomp", "own"),
        ("fies_dummy", "decomp", "noreg"),
        ("fies_dummy", "decomp", "region"),
        ("severe_fi", "any", "own"),
        ("severe_fi", "any", "noreg"),
        ("severe_fi", "any", "region"),
        ("severe_fi", "decomp", "own"),
        ("severe_fi", "decomp", "noreg"),
        ("severe_fi", "decomp", "region"),
    ]
    missing = [k for k in required if k not in models]
    print("parsed models:", len(models))
    print("missing:", missing)
    for outcome, family, spec in required:
        m = models.get((outcome, family, spec), {})
        own = "female_landowner" if family == "any" else "joint_ownership"
        if own in m:
            e = m[own]
            print(
                f"{outcome:11} {family:6} {spec:6} {own:22} "
                f"{fmt3(e['b'])}{stars(e['p']):3}  ({fmt3(e['se'])})  p={e['p']:.3f}"
            )


def main() -> None:
    models = parse_ames(LOG_PATH.read_text(encoding="utf-8", errors="replace"))
    print_preview(models)
    write_docx(models)
    print("wrote", OUT_PATH)


if __name__ == "__main__":
    main()
