#!/usr/bin/env python3
"""Verify that female_landowner_master.do faithfully reproduces the originals.

The master do-file must run every command from ethiopia_landowner.do and
"female landowner analysis.do", in the same order, against the same files. This
script canonicalises all three do-files and checks that each original command
list is an ordered subsequence of the master's.

Canonicalisation removes comments and blank lines, expands the master's $root /
$hh / $pp globals, tracks `cd` so relative .dta references resolve to the same
absolute path on both sides, and ignores a leading `capture`.

Usage: python3 scripts/check_master_do.py [repo_root]
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from pathlib import Path

ROOT_PATH = "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1"
GLOBALS = {
    "$root": ROOT_PATH,
    "$hh": f"{ROOT_PATH}/Household",
    "$pp": f"{ROOT_PATH}/Post_planting",
}

BLOCK_COMMENT = re.compile(r"/\*.*?\*/", re.DOTALL)
DTA_TOKEN = re.compile(r'"([^"]*\.dta)"|(\S+\.dta)')


@dataclass
class Command:
    text: str
    lineno: int


def strip_comments(source: str) -> list[tuple[int, str]]:
    """Drop Stata comments, keeping 1-based line numbers of surviving lines."""
    # Replace block comments with an equal number of newlines to keep numbering.
    def blank_out(match: re.Match[str]) -> str:
        return "\n" * match.group(0).count("\n")

    source = BLOCK_COMMENT.sub(blank_out, source)

    kept: list[tuple[int, str]] = []
    for lineno, raw in enumerate(source.splitlines(), start=1):
        line = raw.split("//")[0]
        stripped = line.strip()
        if not stripped or stripped.startswith("*"):
            continue
        kept.append((lineno, stripped))
    return kept


def canonicalise(line: str, expand_globals: bool) -> str:
    text = line
    if expand_globals:
        for name, value in GLOBALS.items():
            text = text.replace(name, value)
    # `cd"path"` and `cd "path"` must compare equal.
    text = re.sub(r'^cd\s*"', 'cd "', text)
    # An added `capture` prefix does not change what the command does.
    text = re.sub(r"^capture\s+", "", text)
    return re.sub(r"\s+", " ", text).strip()


def resolve_paths(text: str, cwd: str) -> str:
    """Rewrite every .dta reference to an absolute path, then drop quotes."""

    def replace(match: re.Match[str]) -> str:
        raw = match.group(1) or match.group(2)
        # Keep any trailing punctuation that got swept into a bare token.
        suffix = ""
        while raw and not raw.endswith(".dta"):
            suffix = raw[-1] + suffix
            raw = raw[:-1]
        path = raw if raw.startswith("/") else f"{cwd}/{raw}"
        return path + suffix

    text = DTA_TOKEN.sub(replace, text)
    return text.replace('"', "")


def load(path: Path, expand_globals: bool) -> list[Command]:
    cwd = ROOT_PATH
    commands: list[Command] = []
    for lineno, raw in strip_comments(path.read_text()):
        text = canonicalise(raw, expand_globals)
        cd_match = re.match(r'^cd\s+"?([^",]+)"?\s*$', text)
        if cd_match:
            cwd = cd_match.group(1).rstrip("/")
        commands.append(Command(resolve_paths(text, cwd), lineno))
    return commands


def check_subsequence(original: list[Command], master: list[Command], name: str) -> bool:
    master_texts = [c.text for c in master]
    cursor = 0
    for cmd in original:
        try:
            cursor = master_texts.index(cmd.text, cursor) + 1
        except ValueError:
            print(f"FAIL [{name}] line {cmd.lineno} is missing or out of order:")
            print(f"       {cmd.text}")
            window = [t for t in master_texts[cursor : cursor + 5]]
            if window:
                print("     master file expected next one of:")
                for text in window:
                    print(f"       {text}")
            return False
    print(f"PASS [{name}] all {len(original)} commands present, in original order")
    return True


def check_balance(path: Path) -> bool:
    source = BLOCK_COMMENT.sub("", path.read_text())
    depth = 0
    ok = True
    for lineno, raw in enumerate(source.splitlines(), start=1):
        line = raw.split("//")[0]
        if line.strip().startswith("*"):
            continue
        if line.count('"') % 2:
            print(f"FAIL [braces] unbalanced double quote at line {lineno}: {raw.strip()}")
            ok = False
        depth += line.count("{") - line.count("}")
        if depth < 0:
            print(f"FAIL [braces] unmatched closing brace at line {lineno}")
            return False
    if depth != 0:
        print(f"FAIL [braces] {depth} unclosed brace(s) at end of file")
        return False
    if ok:
        print("PASS [braces] braces and double quotes are balanced")
    return ok


def check_programs(path: Path) -> bool:
    text = path.read_text()
    defines = len(re.findall(r"^program define\b", text, re.MULTILINE))
    ends = len(re.findall(r"^end\s*$", text, re.MULTILINE))
    if defines != ends:
        print(f"FAIL [program] {defines} `program define` vs {ends} `end`")
        return False
    print(f"PASS [program] {defines} program definition(s) closed with `end`")
    return True


def check_dta_order(master: list[Command]) -> bool:
    """Every .dta read must be produced earlier, or be external input data."""
    written: set[str] = set()
    ok = True
    for cmd in master:
        verb = cmd.text.split(" ", 1)[0]
        paths = re.findall(r"(\S+\.dta)", cmd.text)
        if not paths:
            continue
        if verb == "save":
            written.update(paths)
            continue
        for path in paths:
            name = path.rsplit("/", 1)[-1]
            derived = name in {
                "parcel.dta",
                "female_ownership.dta",
                "area_ethiopia.dta",
                "agri_practices.dta",
                "improved_maize.dta",
                "merged_w_fies.dta",
                "hh_9_w5.dta",
                "hh_12a_w5.dta",
                "hh_14.dta",
                "survey_design.dta",
            }
            if derived and path not in written:
                print(f"FAIL [order] line {cmd.lineno} reads {name} before it is saved")
                ok = False
    if ok:
        print("PASS [order] every derived dataset is saved before it is read")
    return ok


def main() -> int:
    repo = Path(sys.argv[1] if len(sys.argv) > 1 else ".").resolve()
    master_path = repo / "female_landowner_master.do"
    originals = {
        "ethiopia_landowner.do": repo / "ethiopia_landowner.do",
        "female landowner analysis.do": repo / "female landowner analysis.do",
    }

    for path in [master_path, *originals.values()]:
        if not path.exists():
            print(f"FAIL missing file: {path}")
            return 1

    master = load(master_path, expand_globals=True)
    print(f"Master file: {len(master)} commands\n")

    results = [
        check_subsequence(load(path, expand_globals=False), master, name)
        for name, path in originals.items()
    ]
    results.append(check_balance(master_path))
    results.append(check_programs(master_path))
    results.append(check_dta_order(master))

    print()
    if all(results):
        print("ALL CHECKS PASSED")
        return 0
    print("SOME CHECKS FAILED")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
