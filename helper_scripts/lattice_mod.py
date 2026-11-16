"""Write the modified Booster lattice, madx_files/<stem>_modified.madx: the lattice plus the elements it does not have yet
(unsurveyed extraction elements), the survey misalignments (on by default) and other edits. It runs with run_madx like
the lattice itself (same input and output files).

Not called "copy" since it is not an exact copy (user 2026-11-02); the module was lattice_copy until 2026-11-13.

Edits:
    elements   from a table (default madx_files/extra_elements.csv, made from the archived lattice by
               studies/archived_lattice/make_extra_elements.py): name, class, ref, offset_m, length_m, source.
               Each becomes <name>_UP (MARKER), <name>_CT (the class, e.g. MKS_KICKER, BEX2), <name>_DN (MARKER)
               at AT(ref) + offset_m and -/+ length_m / 2, like the surveyed L12 kickers; length_m 0 (thin, e.g. the
               old-style BPM MONITORs OLDSTYLE_BPM_*) gives one element <name> at AT(ref) + offset_m.
    classes    element class changes of existing sequence elements (CLASS_CHANGES: the empty L12 extraction
               kicker becomes an MKS_KICKER, user 2026-11-02).
    factors    kick calibration factors {element definition: factor}: the modified lattice defines
               <NAME>_CAL = factor and multiplies the definition's KICK by it (KICK_FACTORS, none now: since
               2026-11-02 the lattices give BEX2..4 their own currents I_BEX<n> and the calibration is in the
               B:BEX<n>S scale devices of the MISC files).
    rmatrix    TWISS with RMATRIX and RE11..RE44 in the twiss columns (pa.model_modes needs them).
    misalign   survey misalignments (2026-11-13): DX, DY, DS, DTHETA, DPHI, DPSI (MAD-X permanent misalignments)
               appended to the sequence lines of the misaligned elements, from a misalignment device file
               (helper_scripts/misalign.py, data_Booster/<date>_MISALIGN_<lattice>.json) and overrides
               ("BMAL3": 0, "BMAS12.dy": 0, "BMAS12.S": 0.5, "BMAS12.dx.O": 1e-3, misalign.batch(...)). Applied
               after the new elements (they are not surveyed, so not misaligned). Also writes the record file
               <out stem>_misalign.json (selection, applied offsets, element offsets), named in the header;
               run_madx reads it for the BPM offsets.
    outputs    the output files of WRITE / SURVEY / TRACK get "modified" in their names (MEV400_Booster_modified_twiss.outx,
               ...; 2026-11-14, user), so runs of the plain and the modified lattice keep their own tables.
    perm survey a second SURVEY with PERM_ALIGN_SURVEY=TRUE writing <survey file>_perm.outx (MAD-X's own misaligned
               survey; it applies the rotations about the global axes, see studies/survey_misalignments; kept to
               quantify the rotation effect, user 2026-11-13). On by default.
    track      keep the lattice's TRACK block (default) or remove it; track_start: an element to start the
               tracking at (SEQEDIT FLATTEN / CYCLE / USE just before TRACK, after TWISS and SURVEY, so their tables
               keep the lattice's start), e.g. TREATYPOINT (PIP-II) or L1_START (MEV400, from extra_elements.csv).

One modified lattice at a time (user 2026-11-13): make it with the chosen misalignments, then runs only change the
input files.

    import lattice_mod as lm
    path = lm.make_modified("madx_files/MEV400_Booster_lattice.madx")   # -> madx_files/MEV400_Booster_lattice_modified.madx
                                                                     #    (with the default misalignments)
    path = lm.make_modified("madx_files/MEV400_Booster_lattice.madx", misalign=None)   # without
    path = lm.make_modified("madx_files/MEV400_Booster_lattice.madx", rmatrix=True)
    path = lm.make_modified("madx_files/MEV400_Booster_lattice.madx",
                            misalign="data_Booster/11_13_26_MISALIGN_MEV400.json",
                            misalign_overrides={"BMAS12.dy": 0.0}, track_start="L1_START")

    python helper_scripts/lattice_mod.py madx_files/MEV400_Booster_lattice.madx [--out OUT] [--elements CSV]
        [--rmatrix] [--no-factors] [--misalign FILE | --no-misalign] [--mis-override NAME=V ..] [--mis-batch KIND[.COMP][.S|.O]=V ..]]
        [--no-perm-survey] [--same-output-names] [--no-track | --track-start ELEMENT]
"""
import argparse
import csv
import re
from pathlib import Path

import misalign as ma

ROOT = Path(__file__).resolve().parents[1]
ELEMENTS_FILE = ROOT / "madx_files/extra_elements.csv"
CLASS_CHANGES = {"EXTRACTION_KICKER_L12_D_CT": "MKS_KICKER"}
# per lattice file stem {definition: factor}; empty since the lattices have I_BEX3/I_BEX4 (2026-11-02; the
# bare-lattice closing ratios were BEX3 0.478403, BEX4 0.944484 x BEX2 for
# MEV400 and 0.522390, 0.923550 for PIP-II)
KICK_FACTORS: dict[str, dict[str, float]] = {}
SUFFIX = "_modified"
# default misalignment device file per lattice file stem (user 2026-11-13: the modified lattice has the calculated
# misalignments by default); a lattice not listed gets none
DEFAULT_MISALIGN = {"MEV400_Booster_lattice": ROOT / "data_Booster/11_13_26_MISALIGN_MEV400.json",
                    "PIP_II_Booster_lattice": ROOT / "data_Booster/11_13_26_MISALIGN_PIP_II.json"}
RE_COLUMNS = [f"RE{i}{j}" for i in range(1, 5) for j in range(1, 5)]   # = ping_analysis.RE_COLUMNS
SEQ_LINE = re.compile(r"^(\s*)(\w+)\s*:\s*(\w+)\s*,\s*AT\s*=\s*([-\d.eE+]+)(.*)$")
SEQ_LINE_END = re.compile(r"^(\s*)(\w+)\s*:\s*(\w+)\s*,\s*AT\s*=\s*([-\d.eE+]+)(.*?);(.*)$")
MIS_ATTRS = ("DX", "DY", "DS", "DTHETA", "DPHI", "DPSI")
RECORD_SUFFIX = "_misalign.json"


def read_elements(path: str | Path = ELEMENTS_FILE) -> list[dict]:
    """The element table: dicts with name, class, ref, offset_m [m], length_m [m] (floats), source."""
    with open(path, newline="") as f:
        rows = list(csv.DictReader(f))
    for r in rows:
        r["offset_m"], r["length_m"] = float(r["offset_m"]), float(r["length_m"])
    return rows


def sequence_span(lines: list[str]) -> tuple[int, int]:
    """Line indices of the (only) SEQUENCE statement and its ENDSEQUENCE."""
    start = [i for i, l in enumerate(lines) if re.search(r":\s*SEQUENCE\b", l, re.I)]
    end = [i for i, l in enumerate(lines) if re.match(r"\s*ENDSEQUENCE\s*;", l, re.I)]
    if len(start) != 1 or len(end) != 1:
        raise ValueError(f"expected one SEQUENCE ... ENDSEQUENCE, found {len(start)} / {len(end)}")
    return start[0], end[0]


def edit_lines(lines: list[str], elements: list[dict], class_changes: dict[str, str]) -> list[str]:
    """The lattice lines with the elements inserted in AT order and the class changes applied (inside the
    sequence). Raises ValueError for a ref or a class-change element not in the sequence, or a name that is
    already there."""
    i0, i1 = sequence_span(lines)
    body = lines[i0 + 1:i1]
    at = {}
    for k, l in enumerate(body):
        m = SEQ_LINE.match(l)
        if m:
            at[m.group(2)] = float(m.group(4))
            if m.group(2) in class_changes:
                body[k] = f"{m.group(1)}{m.group(2)}: {class_changes[m.group(2)]}, AT={m.group(4)}{m.group(5)}"
    if missing := [n for n in class_changes if n not in at]:
        raise ValueError(f"class change for elements not in the sequence: {missing}")
    indent = next((SEQ_LINE.match(l).group(1) for l in body if SEQ_LINE.match(l)), "\t")
    new = []
    for e in elements:
        if e["ref"] not in at:
            raise ValueError(f"{e['name']}: ref {e['ref']} not in the sequence")
        s = at[e["ref"]] + e["offset_m"]
        parts = (("UP", "MARKER", -0.5), ("CT", e["class"], 0.0), ("DN", "MARKER", 0.5)) if e["length_m"] else \
            ((None, e["class"], 0.0),)
        for part, cls, ds in parts:
            name = f"{e['name']}_{part}" if part else e["name"]
            if name in at:
                raise ValueError(f"{name} is already in the sequence")
            note = f" ! {e['source']}" if part in ("CT", None) else ""
            new.append((s + ds * e["length_m"], f"{indent}{name}: {cls}, AT={s + ds * e['length_m']!r};{note}"))
    out = []
    pending = sorted(new)
    for l in body:
        m = SEQ_LINE.match(l)
        while m and pending and pending[0][0] < float(m.group(4)):
            out.append(pending.pop(0)[1])
        out.append(l)
    out += [p[1] for p in pending]
    return lines[:i0 + 1] + out + lines[i1:]


def edit_factors(lines: list[str], factors: dict[str, float]) -> list[str]:
    """The lines with <NAME>_CAL = factor; before each element definition NAME (outside the sequence) and its KICK
    multiplied by <NAME>_CAL. Raises ValueError unless each NAME has exactly one definition with a KICK."""
    i0, i1 = sequence_span(lines)
    out = list(lines)
    for name, f in factors.items():
        hits = [i for i, l in enumerate(lines) if (i < i0 or i > i1) and re.match(rf"\s*{name}\s*:", l, re.I)]
        if len(hits) != 1 or not re.search(r"KICK\s*:?=", lines[hits[0]], re.I):
            raise ValueError(f"{name}: expected one definition with a KICK, found {len(hits)}")
        i = hits[0]
        out[i] = re.sub(r"(KICK\s*:?=\s*)", rf"\g<1>{name}_CAL*", out[i], count=1, flags=re.I)
        out[i] = f"{name}_CAL = {float(f)!r}; ! lattice_mod factor\n{out[i]}"
    return out


def edit_rmatrix(lines: list[str]) -> list[str]:
    """The lines with RMATRIX on every TWISS command and RE11..RE44 added to the twiss SELECT columns."""
    out = []
    for l in lines:
        if re.match(r"\s*select\s*,\s*flag\s*=\s*twiss\s*,.*column\s*=", l, re.I):
            l = re.sub(r"\s*;", "," + ",".join(c.lower() for c in RE_COLUMNS) + ";", l, count=1)
        elif re.match(r"\s*TWISS\s*,", l, re.I) and not re.search(r"\bRMATRIX\b", l, re.I):
            l = re.sub(r"(\s*TWISS)\s*,", r"\1,RMATRIX,", l, count=1, flags=re.I)
        out.append(l)
    if sum("RMATRIX" in l for l in out) == 0 or sum("re11" in l for l in out) == 0:
        raise ValueError("no TWISS / twiss SELECT line found for RMATRIX")
    return out


def edit_misalign(lines: list[str], offsets: dict[str, dict[str, float]]) -> list[str]:
    """The lines with DX..DPSI [m, rad] (offsets {element: {dx..dpsi}}, zeros left out) appended to the sequence
    lines of those elements. Raises ValueError for an element not in the sequence or already misaligned."""
    i0, i1 = sequence_span(lines)
    out, done = list(lines), set()
    for i in range(i0 + 1, i1):
        m = SEQ_LINE_END.match(lines[i])
        if not m or m.group(2) not in offsets:
            continue
        name, rest = m.group(2), m.group(5)
        if re.search(r",\s*D(X|Y|S|THETA|PHI|PSI)\s*=", rest, re.I):
            raise ValueError(f"{name} already has a misalignment attribute")
        off = offsets[name]
        attrs = "".join(f", {a}={float(off[a.lower()])!r}" for a in MIS_ATTRS if off.get(a.lower(), 0.0) != 0.0)
        out[i] = f"{m.group(1)}{name}: {m.group(3)}, AT={m.group(4)}{rest}{attrs};{m.group(6)}"
        done.add(name)
    if missing := sorted(set(offsets) - done):
        raise ValueError(f"misaligned elements not in the sequence: {missing[:10]}{' ...' if len(missing) > 10 else ''}")
    return out


def edit_outputs(lines: list[str], tag: str = "modified") -> list[str]:
    """The lines with the output files of WRITE, SURVEY and TRACK renamed, so the modified lattice does not overwrite
    the lattice's: <X>_Booster_<what> -> <X>_Booster_<tag>_<what> (e.g. MEV400_Booster_modified_twiss.outx), any other
    name -> <tag>_<name>. Input files (CALL) are not touched."""
    def rename(m: re.Match) -> str:
        name = m.group(2)
        new = name.replace("_Booster_", f"_Booster_{tag}_", 1) if "_Booster_" in name else f"{tag}_{name}"
        return f"{m.group(1)}{new}{m.group(3)}"
    out = []
    for l in lines:
        if re.match(r"\s*(WRITE|SURVEY|TRACK)\s*,", l, re.I):
            l = re.sub(r"(FILE\s*=\s*['\"])([^'\"]+)(['\"])", rename, l, count=1, flags=re.I)
        out.append(l)
    return out


def edit_perm_survey(lines: list[str]) -> list[str]:
    """The lines with a second SURVEY after each SURVEY: PERM_ALIGN_SURVEY=TRUE, FILE <name>_perm.outx."""
    out = []
    for l in lines:
        out.append(l)
        if re.match(r"\s*SURVEY\s*,", l, re.I):
            perm = re.sub(r"PERM_ALIGN_SURVEY\s*=\s*\w+", "PERM_ALIGN_SURVEY=TRUE", l, flags=re.I)
            if "PERM_ALIGN_SURVEY" not in perm.upper():
                perm = re.sub(r"\s*;", ", PERM_ALIGN_SURVEY=TRUE;", perm, count=1)
            perm = re.sub(r"(FILE\s*=\s*['\"])([^'\"]*?)(\.outx['\"])", r"\1\2_perm\3", perm, flags=re.I)
            out.append(perm)
    return out


def edit_track(lines: list[str], track: bool = True, start: str | None = None) -> list[str]:
    """The lines without the TRACK ... ENDTRACK block (track False), or with SEQEDIT (FLATTEN, CYCLE to start,
    FLATTEN) and USE just before TRACK (start given), so only the tracking starts there."""
    i_tr = [i for i, l in enumerate(lines) if re.match(r"\s*TRACK\s*,", l, re.I)]
    i_end = [i for i, l in enumerate(lines) if re.match(r"\s*ENDTRACK\s*;", l, re.I)]
    if not track:
        if len(i_tr) != len(i_end):
            raise ValueError("TRACK and ENDTRACK do not pair up")
        drop = {k for a, b in zip(i_tr, i_end) for k in range(a, b + 1)}
        return [l for k, l in enumerate(lines) if k not in drop]
    if start is None:
        return lines
    if len(i_tr) != 1:
        raise ValueError(f"track_start needs exactly one TRACK command, found {len(i_tr)}")
    i0, i1 = sequence_span(lines)
    if not any((m := SEQ_LINE.match(l)) and m.group(2) == start for l in lines[i0 + 1:i1]):
        raise ValueError(f"track_start {start} not in the sequence")
    seq = re.search(r"^\s*(\w+)\s*:\s*SEQUENCE\b", lines[i0], re.I).group(1)
    block = [f"! tracking from {start} (lattice_mod): TWISS and SURVEY above keep the lattice's start",
             f"SEQEDIT, SEQUENCE={seq};", "FLATTEN;", f"CYCLE, START={start};", "FLATTEN;", "ENDEDIT;",
             f"USE, SEQUENCE={seq};"]
    return lines[:i_tr[0]] + block + lines[i_tr[0]:]


def make_modified(lattice: str | Path, out: str | Path | None = None, elements: str | Path | None = ELEMENTS_FILE,
              class_changes: dict[str, str] | None = None, factors: dict[str, float] | None = None,
              rmatrix: bool = False, misalign: str | Path | None | bool = True,
              misalign_overrides: dict[str, float] | None = None, perm_survey: bool = True, track: bool = True,
              track_start: str | None = None, outputs_tag: str | None = "modified") -> Path:
    """Write the modified lattice (default <lattice stem>_modified.madx next to the lattice); returns its path.
    elements: table file (None = no new elements); class_changes default CLASS_CHANGES; factors default
    KICK_FACTORS of the lattice ({} = none); rmatrix: TWISS with RMATRIX and RE11..RE44 columns; misalign: a
    misalignment device file, True (default) = DEFAULT_MISALIGN of the lattice (none if not listed), None or False
    = none; with misalign_overrides (misalign.applied), which also writes the
    record <out stem>_misalign.json (an old record is removed when misalign is None); perm_survey: the second
    SURVEY with PERM_ALIGN_SURVEY=TRUE; track: keep the TRACK block; track_start: element to start tracking at;
    outputs_tag: the output files get it in their names (edit_outputs, default "modified": MEV400_Booster_modified_
    twiss.outx, ...), so plain and modified runs keep their own tables; None = the lattice's names."""
    lattice = Path(lattice)
    out = Path(out) if out else lattice.with_name(f"{lattice.stem}{SUFFIX}.madx")
    if out.resolve() == lattice.resolve():
        raise ValueError("the modified lattice would overwrite the lattice")
    rows = read_elements(elements) if elements else []
    changes = CLASS_CHANGES if class_changes is None else class_changes
    facs = KICK_FACTORS.get(lattice.stem, {}) if factors is None else factors
    lines = edit_lines(lattice.read_text().splitlines(), rows, changes)
    lines = edit_factors(lines, facs)
    if rmatrix:
        lines = edit_rmatrix(lines)
    record = out.with_name(out.stem + RECORD_SUFFIX)
    if misalign is True:
        misalign = DEFAULT_MISALIGN.get(lattice.stem)
    elif misalign is False:
        misalign = None
    elem = {}
    if misalign:
        devs = ma.load(misalign)
        elem = ma.element_offsets(devs, misalign_overrides)
        lines = edit_misalign(lines, elem)
        ma.write_record(record, devs, misalign_overrides, Path(misalign).name, out)
    elif record.exists():
        record.unlink()
    if outputs_tag:
        lines = edit_outputs(lines, outputs_tag)
    if perm_survey:
        lines = edit_perm_survey(lines)
    if track_start and not track:
        raise ValueError("track_start needs track=True")
    lines = edit_track(lines, track, track_start)
    mis_txt = (f"{Path(misalign).name}, {len(elem)} elements, overrides "
               f"{len(misalign_overrides or {})}; record: {record.name}" if misalign else "none")
    head = (f"! {lattice.name} modified by helper_scripts/lattice_mod.py: do not edit, re-make it.\n"
            f"! New elements: {', '.join(r['name'] for r in rows) or 'none'}"
            f"{f' (from {Path(elements).name})' if rows else ''}\n"
            f"! Class changes: {', '.join(f'{k} -> {v}' for k, v in changes.items()) or 'none'}\n"
            f"! Kick factors: {', '.join(f'{k} x {v:.6g}' for k, v in facs.items()) or 'none'}; "
            f"RMATRIX: {'yes' if rmatrix else 'no'}\n"
            f"! Misalignments: {mis_txt}\n"
            f"! Output files: {'<lattice>_Booster_' + outputs_tag + '_*' if outputs_tag else 'as in the lattice'}; "
            f"PERM_ALIGN_SURVEY survey: {'yes' if perm_survey else 'no'}; TRACK: "
            f"{'removed' if not track else ('from ' + track_start) if track_start else 'as in the lattice'}\n")
    out.write_text(head + "\n".join(lines) + "\n")
    return out


def main() -> None:
    p = argparse.ArgumentParser(description="Write a modified Booster lattice (extra elements, RMATRIX, ...).")
    p.add_argument("lattice")
    p.add_argument("--out")
    p.add_argument("--elements", default=str(ELEMENTS_FILE))
    p.add_argument("--rmatrix", action="store_true", help="TWISS with RMATRIX, RE11..RE44 columns")
    p.add_argument("--no-factors", action="store_true", help="no kick calibration factors")
    mg = p.add_mutually_exclusive_group()
    mg.add_argument("--misalign", help="misalignment device file (default: DEFAULT_MISALIGN of the lattice)")
    mg.add_argument("--no-misalign", action="store_true", help="no misalignments")
    p.add_argument("--mis-override", nargs="*", default=[], metavar="NAME[.COMP][.S|.O]=V",
                   help="misalignment overrides, e.g. BMAL3=0 BMAS12.dy=0 BMAS12.S=0.5 BMAS12.dx.O=0.001")
    p.add_argument("--mis-batch", nargs="*", default=[], metavar="KIND[.COMP][.S|.O]=V",
                   help="family overrides, e.g. CFM.ds=0 BMA=0 BMA.S=0.5 (KIND = CFM, BMA, OTHER or ALL)")
    p.add_argument("--no-perm-survey", action="store_true", help="no second SURVEY with PERM_ALIGN_SURVEY")
    p.add_argument("--same-output-names", action="store_true",
                   help="keep the lattice's output file names (default: <X>_Booster_modified_*)")
    tr = p.add_mutually_exclusive_group()
    tr.add_argument("--no-track", action="store_true", help="remove the TRACK block")
    tr.add_argument("--track-start", help="start the tracking at this element (SEQEDIT CYCLE before TRACK)")
    a = p.parse_args()
    overrides = {}
    mis = None if a.no_misalign else (a.misalign or DEFAULT_MISALIGN.get(Path(a.lattice).stem))
    if a.mis_override or a.mis_batch:
        if not mis:
            p.error("--mis-override / --mis-batch need a misalignment file (--misalign, or a lattice in DEFAULT_MISALIGN)")
        devs = ma.load(mis)
        for item in a.mis_batch:
            key, v = item.split("=")
            parts = key.split(".")
            kind = None if parts[0].upper() == "ALL" else parts[0]
            comp = next((x for x in parts[1:] if x in ma.KEYS), None)
            part = next((x for x in parts[1:] if x in ("S", "O")), None)
            overrides.update(ma.batch(devs, float(v), kind=kind, component=comp, part=part))
        for item in a.mis_override:
            key, v = item.split("=")
            overrides[key] = float(v)
    path = make_modified(a.lattice, a.out, a.elements, factors={} if a.no_factors else None, rmatrix=a.rmatrix,
                         misalign=mis, misalign_overrides=overrides, perm_survey=not a.no_perm_survey,
                         track=not a.no_track, track_start=a.track_start,
                         outputs_tag=None if a.same_output_names else "modified")
    print(f"wrote {path}")


if __name__ == "__main__":
    main()
