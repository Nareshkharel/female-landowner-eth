"""Word tables for survey-weighted, household-clustered FIES factor OLS."""
from pathlib import Path

from estimate_fies_factor_hhcluster import choose_score, collect_models, load_sample
from make_fies_ame_survey_tables import write_docx

OUT = Path(__file__).resolve().parent / "fies_factor_hhcluster_tables.docx"

TABLES = [
    (
        "Table 1. Female landowner and the FIES factor score",
        "fies_factor",
        "any",
        "FIES factor",
    ),
    (
        "Table 2. Sole and joint ownership and the FIES factor score",
        "fies_factor",
        "decomp",
        "FIES factor",
    ),
]

NOTE = (
    "Notes: OLS coefficients. The dependent variable is the first principal-factor "
    "score of the eight FIES items (Stata: factor worried-hungry, mineigen(1); predict pc1). "
    "A higher score means more food-insecurity experience. Survey-weighted (pw_w5), "
    "errors clustered at the household. Column (1) includes only the ownership variable. "
    "Column (2) adds household controls, without region dummies. Column (3) adds region "
    "dummies (saq01). Standard errors in parentheses. "
    "*** p<0.01, ** p<0.05, * p<0.1."
)


def main():
    d = load_sample()
    y = choose_score(d)["factor"][0]
    pw = d["pw_w5"].astype(float)
    models = collect_models(d, y, pw)
    dest = write_docx(models, out_path=OUT, note=NOTE, tables=TABLES)
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
