"""Survey misalignments: the measured position of each surveyed device, as a device file, and the MAD-X DX..DPSI
that lattice_mod writes into the modified lattice (from studies/survey_misalignments, 2026-11-13).

A device (CFM, BMA, RF cavity, kicker, collimator, ...) is a rigid body. Its measured offsets are surveyed minus
lattice, in the device's own ideal frame about its centre (the midpoint of the two lattice markers that stand for
its surveyed UP and DN points): dx [m] (+ outward, MAD-X x), dy [m] (+ down, MAD-X y; model -y = up), ds [m] (along
the beam), dtheta [rad] (yaw, + turns s toward +x), dphi [rad] (pitch, + turns s toward +y = down), dpsi [rad] (roll,
+ turns x toward y; from the roll column of compareHandZ.csv for the CFMs, all 0 so far; 0 for the others: two
points cannot give it). CFM points are moved by PHYS_CENTER along x first (they undo the surveyors' physical ->
magnetic correction, as when the idealized lattice was built; user 2026-11-13). No global fit by default: the
lattice stays where the user's PIP-II-region fit placed it (a rigid motion of the whole ring does not change the
beam when every device is misaligned).

Every child of a device (the sequence elements it places: CFM flanges, fiducials, RBEND core; BMA spools, magnet
edges, multipole, corrector kick, BPM; UP/CT/DN of the others) moves with it: its MAD-X misalignment is the device's
rigid motion seen from the child's entrance frame (MAD-X pivots at the entrance, geometry.py).

Data: data_Booster/cleaned_beamsheet_Aug20_corrected.csv (everything but the CFMs) and data_Booster/compareHandZ.csv
(CFMs: x, y, z of the 2026 survey; hx, hy, h are nonplanar and not used; roll [mrad] column for later). The two are
from different survey networks: their CFM radial positions differ by a harmonic-2 wave of ~+-2.5 mm (noted, not
corrected; user).

The device file (make_misalign_file, the only step that runs MAD-X: one SURVEY of the ideal lattice) holds per
device: kind (CFM / BMA / OTHER), measured offsets, scale S (default 1), offset O (per component, default 0),
flags (information), note, and the frames of the device and its children, so that element_offsets needs no MAD-X.
Applied offsets = S x measured + O. Devices left out by default have S = 0 and the reason in note (DEFAULT_LEFT_OUT).

Overrides (like the run settings' group / part overrides):
    "BMAL3": 0           the whole device at its lattice position (every component = the value)
    "BMAS12.dy": 0.0     one component replaced [m or rad]
    "BMAS12.S": 0.5      the device's scale
    "BMAS12.dx.O": 1e-3  the offset added to one component
    batch(devs, 0, kind="CFM", component="ds")   -> {"FMAG1_2.ds": 0, ...}   (kind, component, part="S"/"O")

    import misalign as ma
    path = ma.make_misalign_file("madx_files/MEV400_Booster_lattice.madx",
                                 "data_Booster/11_13_26_MISALIGN_MEV400.json")
    devs = ma.load(path)
    ov = {"BMAS12.dy": 0.0, **ma.batch(devs, 0.0, kind="CFM", component="ds")}
    elem = ma.element_offsets(devs, ov)          # {sequence element: {dx..dpsi}} for lattice_mod
    ma.bpm_offsets("madx_files/MEV400_Booster_lattice_modified_misalign.json")   # {BPM: (dx, dy)} [m]

    python helper_scripts/misalign.py madx_files/MEV400_Booster_lattice.madx --out FILE [--cfm-source beamsheet]
        [--global-fit]
"""
from __future__ import annotations

import argparse
import csv
import json
import re
import shutil
import subprocess
import tempfile
from datetime import datetime
from pathlib import Path

import numpy as np

import geometry as g
from tfs_reader import TfsTable

ROOT = Path(__file__).resolve().parents[1]
MADX = ROOT / "madx_files" / "madx"
BEAMSHEET = ROOT / "data_Booster" / "cleaned_beamsheet_Aug20_corrected.csv"
COMPARE_HZ = ROOT / "data_Booster" / "compareHandZ.csv"
KEYS = ("dx", "dy", "ds", "dtheta", "dphi", "dpsi")
KINDS = ("CFM", "BMA", "OTHER")
INCH = 0.0254
PHYS_CENTER = {"F": 0.042 * INCH, "D": -0.053 * INCH}     # [m], magnetic - physical centreline, + outward
BMA_PARTS = ("SPOOL_UP", "MAG_UP", "", "HV_KICK", "BPM", "MAG_DN", "SPOOL_DN")   # "" = the multipole BMA<L|S><n>
# beamsheet device -> lattice prefix (<prefix>_UP/_CT/_DN in the sequence) for the devices that are not CFM or BMA
OTHER_MAP = {
    "KICKER-L12-A": "NOTCH_KICKER_L12_A", "KICKER-L12-B": "NOTCH_KICKER_L12_B",
    "KICKER-L12-C": "NOTCH_KICKER_L12_C", "KICKER-L12-D": "EXTRACTION_KICKER_L12_D",
    "BCOLL1": "COLL_1", "BCOLL2": "COLL_2", "BCOLL3": "COLL_3", "COLL_L8": "COLL_L8",
    "F10053100-V3": "VIPM", "F10053100-H2": "HIPM", "CURR-MON": "L10CURR_MON",
    "DAMPER-KICKER": "DAMPER_KICKER", "DAMPER-PICKUP": "DAMPER_PICKUP", "BSE-102-CAN": "SEPT_CAN",
    "BPM-SPEC-COLL": "BPM_SPEC_COLL", "ORBD-USBPM": "ORBD_USBPM",
    **{f"RFC{n:02d}": f"RF_CAV{n:02d}" for n in range(1, 23)},
}
# devices with S = 0 in a new device file (user 2026-11-12/13; anyone can override them; BMAL3 applied since 2026-11-14)
DEFAULT_LEFT_OUT = {
    "VIPM": "F10053100-V3: fiducials 69 mm outward of the beam axis; what they mark is not known.",
    "HIPM": "F10053100-H2: fiducials 67-71 mm above the beam axis; what they mark is not known.",
    "SEPT_CAN": "BSE-102-CAN: 43-48 mm above the beam axis (vertical extraction septum, can above the beam).",
    "ORBD_USBPM": "ORBD-USBPM: 50 mm outward, in the L1 injection (ORBUMP) chicane; maybe on the chicane chamber axis.",
}
DEVICE_NOTES = {
    "BMAL3": "BMA005-L3 sits inside the L3 vertical dogleg (DOG03_1, BMA, DOG03_2): 12 mm below the lattice, on purpose, "
             "following the beam's shift in the dogleg. Applied (user 2026-11-14): the dogleg runs at a constant current, "
             "so its bump does not scale with Brho; at low Brho the BMA looks like an unmisaligned element (the beam is "
             "lowered with it), at high Brho like a real misalignment. The dogleg itself is not in the lattice yet.",
    "BMAS12": "BMA061-S12 and the old BPM-121 sit ~20 mm outward of the S12 straight-section axis, parallel; the "
              "measured BPM orbit agrees. Probably offset on purpose (user 2026-11-13).",
    "COLL_1": "All collimators applied or none (user 2026-11-13: all). Shifted as a block into one corner; what the "
              "fiducials mark is not known. Markers and a monitor only: no optics effect.",
    "COLL_2": "See COLL_1.", "COLL_3": "See COLL_1.",
}
# information flags (thresholds 2026-11-10)
MAX_ANGLE = 0.01        # [rad] yaw or pitch above this: UP/DN do not give the direction ("direction": angles 0)
MAX_OFFSET = 0.010      # [m] |dx| or |dy| above this ("offset")
MAX_DLENGTH = 0.005     # [m] |surveyed - lattice UP-DN distance| above this ("length")
MAX_RELOCATED = 0.10    # [m] |dx|, |dy| or |ds| above this: the lattice puts the device somewhere else than where it
                        # was surveyed (PIP-II: BMAL11 +0.56 m, L10CURR_MON +0.70 m), so it is a new placement, not
                        # a misalignment: "relocated", S = 0 (like the lattice's new elements; user 2026-11-13)


# ---------------------------------------------------------------------------------------------------- survey data

def survey_to_madx(enz) -> np.ndarray:
    """MAD-X global (X, Y, Z) = (North, elevation, East) [m] of survey (East, North, elevation) point(s)."""
    p = np.asarray(enz, float)
    return p[..., [1, 2, 0]]


def madx_to_survey(xyz) -> np.ndarray:
    """Survey (East, North, elevation) [m] of MAD-X global (X, Y, Z) point(s)."""
    p = np.asarray(xyz, float)
    return p[..., [2, 0, 1]]


def read_beamsheet(path: str | Path = BEAMSHEET) -> dict[str, dict]:
    """{beamsheet device: {type, category, date, points {UP, CT, DN: [E, N, elevation] m}}}, in file order (the
    beamsheet CSV format: two header lines, Device, Type, DISTANCE, EAST, NORTH, ELEVATION, Comments, Date, ...;
    a _DDN point suffix is read as DN)."""
    with open(path, encoding="utf-8-sig") as f:
        rows = list(csv.reader(f))[2:]
    devs: dict[str, dict] = {}
    for r in rows:
        if not r or not r[0]:
            continue
        m = re.fullmatch(r"(.+)_(UP|CT|DN|DDN)", r[0])
        if not m:
            raise ValueError(f"unexpected point name {r[0]!r} in {path}")
        d = devs.setdefault(m.group(1), {"type": r[1], "category": r[6], "date": r[7], "points": {}})
        d["points"][m.group(2).replace("DDN", "DN")] = [float(r[3]), float(r[4]), float(r[5])]
    return devs


def read_compare_hz(path: str | Path = COMPARE_HZ) -> tuple[dict[str, list[float]], dict[str, float]]:
    """({point name: [E, N, elevation] m} from the x, y, z columns, {CFM survey name ("1-1"): roll [mrad]}) of
    compareHandZ.csv. The roll of a magnet is the mean of its rows' roll column (blank or missing = 0). Roll sign:
    MAD-X DPSI of this lattice (+ turns x (outward) toward y (down)); to be checked against the survey's."""
    with open(path, encoding="utf-8-sig") as f:
        rows = list(csv.DictReader(f))[1:]
    pts = {r["Point Name"]: [float(r["x"]), float(r["y"]), float(r["z"])] for r in rows}
    rolls: dict[str, list[float]] = {}
    for r in rows:
        v = (r.get("roll") or "").strip()
        rolls.setdefault(r["Point Name"].rsplit("_", 1)[0], []).append(float(v) if v else 0.0)
    return pts, {k: float(np.mean(v)) for k, v in rolls.items()}


# ---------------------------------------------------------------------------------------------------- MAD-X survey

def ideal_survey(lattice: str | Path) -> TfsTable:
    """SURVEY table of the lattice as it is (no misalignments), from a MAD-X run in a temporary folder (the lattice
    without its TRACK block and the files it CALLs copied from its folder); madx_files/ is not touched. ~1.5 s."""
    lattice = Path(lattice)
    text = lattice.read_text()
    text = re.sub(r"^\s*TRACK\s*,.*?^\s*ENDTRACK\s*;", "", text, flags=re.I | re.M | re.S)
    calls = re.findall(r"^\s*call\s*,\s*file\s*=\s*['\"]([^'\"]+)['\"]", text, re.I | re.M)
    files = re.findall(r"^\s*survey\s*,.*?file\s*=\s*['\"]([^'\"]+)['\"]", text, re.I | re.M)
    if not files:
        raise ValueError(f"{lattice} has no SURVEY command with a FILE")
    with tempfile.TemporaryDirectory() as tmp:
        for c in calls:
            if (lattice.parent / c).exists():
                shutil.copy(lattice.parent / c, Path(tmp) / c)
        (Path(tmp) / lattice.name).write_text(text)
        with open(Path(tmp) / "madx.log", "w") as fh, open(Path(tmp) / lattice.name) as fin:
            subprocess.run([str(MADX)], stdin=fin, cwd=tmp, stdout=fh, stderr=subprocess.STDOUT)
        if "finished normally" not in (Path(tmp) / "madx.log").read_text():
            raise RuntimeError(f"MAD-X failed on the survey of {lattice}")
        return TfsTable(Path(tmp) / files[0])


def frames(survey: TfsTable) -> dict[str, tuple[g.Frame, g.Frame]]:
    """{element: (entrance frame, exit frame)} from a SURVEY table (one row per element at its exit; the entrance is
    the previous row's exit)."""
    names = [str(n) for n in survey["NAME"]]
    o = np.column_stack([survey["X"], survey["Y"], survey["Z"]])
    w = [g.w_matrix(a, b, c) for a, b, c in zip(survey["THETA"], survey["PHI"], survey["PSI"])]
    return {n: (g.Frame(o[max(i - 1, 0)], w[max(i - 1, 0)]), g.Frame(o[i], w[i])) for i, n in enumerate(names)}


# ---------------------------------------------------------------------------------------------------- devices

def survey_devices(names: list[str], cfm_source: str = "compareHandZ") -> dict[str, dict]:
    """{lattice device name: {kind, category, survey_name, date, points {UP, CT, DN: [E, N, elevation] m},
    up, dn (the lattice markers of the surveyed UP/DN), children, magnetic_center_offset_m, roll_mrad}} of the
    surveyed devices that are in the sequence names. cfm_source: "compareHandZ" (default, 2026) or "beamsheet"."""
    sheet = read_beamsheet()
    chz, rolls = read_compare_hz() if cfm_source == "compareHandZ" else ({}, {})
    have = set(names)
    out = {}
    for key, d in sheet.items():
        base = {"category": d["category"], "survey_name": key, "date": d["date"], "points": d["points"],
                "magnetic_center_offset_m": 0.0, "roll_mrad": 0.0}
        if m := re.fullmatch(r"(\d+)-(\d)", key):
            name = f"{d['type'][0]}MAG{m[1]}_{m[2]}"
            if f"{name}_CORE" not in have:
                continue
            if chz:
                base["points"] = {p: chz[f"{key}_{p}"] for p in ("UP", "CT", "DN") if f"{key}_{p}" in chz}
                base["date"] = "2026 (compareHandZ)"
            out[name] = {**base, "kind": "CFM", "up": f"{name}_FID_UP", "dn": f"{name}_FID_DN",
                         "children": [n for n in names if n.startswith(name + "_")],
                         "magnetic_center_offset_m": PHYS_CENTER[d["type"][0]], "roll_mrad": rolls.get(key, 0.0)}
        elif m := re.fullmatch(r"BMA(\d+)-([LS])(\d+)", key):
            name = f"BMA{m[2]}{m[3]}"
            if name in have:
                kids = [f"{name}_{p}" if p else name for p in BMA_PARTS]
                out[name] = {**base, "kind": "BMA", "up": f"{name}_MAG_UP", "dn": f"{name}_MAG_DN",
                             "children": [k for k in kids if k in have]}
        elif (prefix := OTHER_MAP.get(key)) and f"{prefix}_UP" in have:
            out[prefix] = {**base, "kind": "OTHER", "up": f"{prefix}_UP", "dn": f"{prefix}_DN",
                           "children": [n for n in names if n.startswith(prefix + "_")]}
    return out


def _ideal_frame(up: np.ndarray, dn: np.ndarray, w_ref: np.ndarray) -> g.Frame:
    """Body frame from two points [m]: origin at the midpoint, s from up to dn, y = w_ref's y made perpendicular to
    s (MAD-X y, down here), x = y x s (outward)."""
    s = (dn - up) / np.linalg.norm(dn - up)
    y = w_ref[:, 1] - s * (w_ref[:, 1] @ s)
    y /= np.linalg.norm(y)
    return g.Frame(0.5 * (up + dn), np.column_stack([np.cross(y, s), y, s]))


def _measured_frame(up: np.ndarray, dn: np.ndarray, ideal: g.Frame, radial: float, roll: float) -> g.Frame:
    """Body frame from two surveyed points [m, MAD-X global], y from the ideal frame, rolled by roll [rad] about s,
    origin moved by radial [m] along the unrolled x."""
    f = _ideal_frame(up, dn, ideal.w)
    return g.Frame(f.o + radial * f.w[:, 0], f.w @ g.rot_psi(roll))


def fit_global(survey: TfsTable, devices: dict[str, dict]) -> g.Rigid:
    """Rigid motion (no scale) of the ideal ring that best fits the surveyed CFM UP/DN points (after PHYS_CENTER).
    Optional (off by default since 2026-11-13)."""
    pos = {str(n): np.array([x, y, z]) for n, x, y, z in zip(survey["NAME"], survey["X"], survey["Y"], survey["Z"])}
    fr = frames(survey)
    src, dst = [], []
    for e in devices.values():
        if e["kind"] != "CFM":
            continue
        ideal = _ideal_frame(pos[e["up"]], pos[e["dn"]], fr[e["up"]][1].w)
        meas = _measured_frame(survey_to_madx(e["points"]["UP"]), survey_to_madx(e["points"]["DN"]), ideal,
                               e["magnetic_center_offset_m"], 0.0)
        h = 0.5 * np.linalg.norm(pos[e["dn"]] - pos[e["up"]])
        src += [ideal.o - h * ideal.w[:, 2], ideal.o + h * ideal.w[:, 2]]
        dst += [meas.o - h * meas.w[:, 2], meas.o + h * meas.w[:, 2]]
    return g.fit_rigid(np.array(src), np.array(dst))


def _frame_rec(f: g.Frame) -> dict:
    """A frame as JSON: {"o": [3] m, "w": 3 x 3 rows}."""
    return {"o": [float(v) for v in f.o], "w": [[float(v) for v in row] for row in f.w]}


def _frame(rec: dict) -> g.Frame:
    """A frame from its JSON record."""
    return g.Frame(np.array(rec["o"], float), np.array(rec["w"], float))


def measure(lattice: str | Path, cfm_source: str = "compareHandZ", global_fit: bool = False) -> dict:
    """{"meta", "devices"} of a new device file (make_misalign_file): the measured offsets, flags and frames of every
    surveyed device in the lattice, S (0 for DEFAULT_LEFT_OUT, else 1), O = 0, note."""
    survey = ideal_survey(lattice)
    names = [str(n) for n in survey["NAME"]]
    fr = frames(survey)
    pos = {n: fr[n][1].o for n in names}
    devices = survey_devices(names, cfm_source)
    gfit = fit_global(survey, devices) if global_fit else g.Rigid.identity()
    out = {}
    for name, e in devices.items():
        up_i, dn_i = gfit.apply(pos[e["up"]]), gfit.apply(pos[e["dn"]])
        ideal = _ideal_frame(up_i, dn_i, gfit.r @ fr[e["up"]][1].w)
        up_m, dn_m = survey_to_madx(e["points"]["UP"]), survey_to_madx(e["points"]["DN"])
        flags = []
        if (dn_m - up_m) @ ideal.w[:, 2] < 0:
            up_m, dn_m = dn_m, up_m
            flags.append("swapped")
        meas = _measured_frame(up_m, dn_m, ideal, e["magnetic_center_offset_m"], 1e-3 * e["roll_mrad"])
        off = g.offsets_in(ideal, g.Rigid(meas.w @ ideal.w.T, meas.o - meas.w @ ideal.w.T @ ideal.o))
        if max(abs(off["dtheta"]), abs(off["dphi"])) > MAX_ANGLE:
            flags.append("direction")
            meas = g.Frame(meas.o, ideal.w)
            off = g.offsets_in(ideal, g.Rigid(np.eye(3), meas.o - ideal.o))
        if max(abs(off["dx"]), abs(off["dy"])) > MAX_OFFSET:
            flags.append("offset")
        dlength = float(np.linalg.norm(dn_m - up_m) - np.linalg.norm(dn_i - up_i))
        if abs(dlength) > MAX_DLENGTH:
            flags.append("length")
        relocated = max(abs(off["dx"]), abs(off["dy"]), abs(off["ds"])) > MAX_RELOCATED
        if relocated:
            flags.append("relocated")
        children = {}
        for c in e["children"]:
            ent = fr[c][0]
            children[c] = _frame_rec(g.Frame(gfit.apply(ent.o), gfit.r @ ent.w))
        out[name] = {"kind": e["kind"], "category": e["category"], "survey_name": e["survey_name"],
                     "date": e["date"], "S": 0.0 if name in DEFAULT_LEFT_OUT or relocated else 1.0,
                     "O": {k: 0.0 for k in KEYS}, "measured": off, "dlength_m": dlength, "flags": flags,
                     "note": (f"relocated in {Path(lattice).name}: the lattice puts it "
                              f"{1e3 * max(abs(off['dx']), abs(off['dy']), abs(off['ds'])):.0f} mm from where it was "
                              "surveyed; a new placement, not misaligned" if relocated else
                              DEFAULT_LEFT_OUT.get(name, DEVICE_NOTES.get(name, ""))),
                     "up": e["up"], "dn": e["dn"], "magnetic_center_offset_m": e["magnetic_center_offset_m"],
                     "roll_mrad": e["roll_mrad"], "points": e["points"],
                     "frame": _frame_rec(ideal), "measured_frame": _frame_rec(meas), "children": children}
    meta = {"created": datetime.now().isoformat(timespec="seconds"), "made_by": "helper_scripts/misalign.py",
            "lattice": Path(lattice).name, "beamsheet": BEAMSHEET.name,
            "cfm_source": "data_Booster/compareHandZ.csv (x, y, z)" if cfm_source == "compareHandZ" else BEAMSHEET.name,
            "global_fit": ({"r": gfit.r.tolist(), "t": gfit.t.tolist()} if global_fit else None),
            "units": "measured / O: dx, dy, ds [m], dtheta, dphi, dpsi [rad]; dlength_m [m]; points [m] survey East, "
                     "North, elevation; frames [m] MAD-X global X (North), Y (elevation), Z (East), w columns = local "
                     "x, y, s",
            "frame": "offsets in the device's ideal frame about its centre: x outward, y down (MAD-X; model -y = up), "
                     "s along the beam. Applied = S x measured + O (overrides: helper_scripts/misalign.py).",
            "phys_center_inch": {"F": 0.042, "D": -0.053}}
    return {"meta": meta, "devices": out}


def _write(obj: dict, path: Path) -> None:
    """Write {"meta", "devices"[, "elements"]} as JSON, one device (element) per line."""
    with open(path, "w") as f:
        f.write('{"meta": ' + json.dumps(obj["meta"]))
        for part in ("devices", "elements"):
            if part not in obj:
                continue
            f.write(f',\n "{part}": {{\n')
            items = list(obj[part].items())
            for i, (k, v) in enumerate(items):
                f.write(f"  {json.dumps(k)}: {json.dumps(v)}{',' if i < len(items) - 1 else ''}\n")
            f.write(" }")
        f.write("\n}\n")


def make_misalign_file(lattice: str | Path, out: str | Path, cfm_source: str = "compareHandZ",
                       global_fit: bool = False) -> Path:
    """Write the device file (measure) for the lattice (its element names and ideal survey); returns its path.
    Re-make it when the survey data or the lattice geometry change."""
    out = Path(out)
    _write(measure(lattice, cfm_source, global_fit), out)
    return out


# ---------------------------------------------------------------------------------------------------- selection

def load(path: str | Path) -> dict[str, dict]:
    """The devices of a device file: {name: record} with "frame", "measured_frame" and the children's frames as
    geometry.Frame (the meta: load_meta)."""
    with open(path) as f:
        data = json.load(f)
    devs = data["devices"]
    for d in devs.values():
        d["frame"] = _frame(d["frame"])
        d["measured_frame"] = _frame(d["measured_frame"])
        d["children"] = {c: _frame(r) for c, r in d["children"].items()}
    return devs


def load_meta(path: str | Path) -> dict:
    """The meta of a device file or a record file."""
    with open(path) as f:
        return json.load(f)["meta"]


def batch(devs: dict[str, dict], value: float, kind: str | None = None, component: str | None = None,
          part: str | None = None, include_left_out: bool = False) -> dict[str, float]:
    """Overrides for a whole family: kind (CFM, BMA, OTHER; None = all); component (of KEYS; None = the whole
    device); part None (the value replaces), "S" (the scale; no component) or "O" (the offset of a component).
    Values [m or rad, or the scale]. Devices left out in the file (S = 0) are skipped unless include_left_out, so
    e.g. a family scale does not bring BMAL3 back; name them in an override to include them."""
    if component is not None and component not in KEYS:
        raise ValueError(f"component {component!r} not one of {KEYS}")
    if part not in (None, "S", "O"):
        raise ValueError(f"part must be None, 'S' or 'O', not {part!r}")
    if part == "S" and component is not None:
        raise ValueError("the scale S is per device: no component")
    if part == "O" and component is None:
        raise ValueError("the offset O is per component: give one")
    out = {}
    for n, d in devs.items():
        if (kind is not None and d["kind"] != kind) or (d["S"] == 0 and not include_left_out):
            continue
        key = n if component is None else f"{n}.{component}"
        out[key + (f".{part}" if part else "")] = float(value)
    return out


def applied(devs: dict[str, dict], overrides: dict[str, float] | None = None) -> dict[str, dict[str, float]]:
    """{device: {dx..dpsi}} applied offsets [m, rad]: S x measured + O with the overrides (module docstring); a
    whole-device override sets every component first, component overrides then replace single ones. Raises
    KeyError for an override of an unknown device and ValueError for a malformed one."""
    whole: dict[str, float] = {}
    comp: dict[str, dict[str, float]] = {}
    scale: dict[str, float] = {}
    offs: dict[str, dict[str, float]] = {}
    for key, v in (overrides or {}).items():
        p = key.split(".")
        if p[0] not in devs:
            raise KeyError(f"override {key!r}: no device {p[0]!r}")
        if len(p) == 1:
            whole[p[0]] = float(v)
        elif len(p) == 2 and p[1] == "S":
            scale[p[0]] = float(v)
        elif len(p) == 2 and p[1] in KEYS:
            comp.setdefault(p[0], {})[p[1]] = float(v)
        elif len(p) == 3 and p[1] in KEYS and p[2] == "O":
            offs.setdefault(p[0], {})[p[1]] = float(v)
        else:
            raise ValueError(f"override {key!r}: use NAME, NAME.S, NAME.<component> or NAME.<component>.O")
    out = {}
    for n, d in devs.items():
        s = scale.get(n, d["S"])
        o = {**d["O"], **offs.get(n, {})}
        a = {k: whole[n] for k in KEYS} if n in whole else {k: s * d["measured"][k] + o.get(k, 0.0) for k in KEYS}
        a.update(comp.get(n, {}))
        out[n] = a
    return out


def element_offsets(devs: dict[str, dict], overrides: dict[str, float] | None = None) -> dict[str, dict[str, float]]:
    """{sequence element: MAD-X DX..DPSI (keys dx..dpsi) [m, rad]} for the children of every device with a nonzero
    applied offset: the device's rigid motion about its ideal centre seen from each child's entrance frame."""
    out = {}
    for n, a in applied(devs, overrides).items():
        if not any(a.values()):
            continue
        rigid = g.body_rigid(devs[n]["frame"], a)
        for c, ent in devs[n]["children"].items():
            out[c] = g.offsets_in(ent, rigid)
    return out


def write_record(path: str | Path, devs: dict[str, dict], overrides: dict[str, float] | None, source: str | Path,
                 lattice: str | Path) -> dict[str, dict[str, float]]:
    """Write the record of a misaligned lattice (meta: device file, overrides, lattice; "devices": applied offsets;
    "elements": the element offsets written into the lattice) and return the element offsets."""
    elem = element_offsets(devs, overrides)
    app = {n: a for n, a in applied(devs, overrides).items() if any(a.values())}
    meta = {"created": datetime.now().isoformat(timespec="seconds"), "made_by": "helper_scripts/lattice_mod.py",
            "lattice": Path(lattice).name, "misalign_file": str(source), "overrides": overrides or {},
            "units": "dx, dy, ds [m], dtheta, dphi, dpsi [rad]; devices: applied offsets in each device's frame; "
                     "elements: MAD-X DX..DPSI about each element's entrance"}
    _write({"meta": meta, "devices": app, "elements": elem}, Path(path))
    return elem


def bpm_offsets(record: str | Path) -> dict[str, tuple[float, float]]:
    """{element: (DX, DY) [m]} of every misaligned element in a lattice's record file (the BPM reading is the twiss
    X, Y minus these at a MONITOR)."""
    with open(record) as f:
        elem = json.load(f)["elements"]
    return {n: (float(o.get("dx", 0.0)), float(o.get("dy", 0.0))) for n, o in elem.items()}


def main() -> None:
    p = argparse.ArgumentParser(description="Survey data -> misalignment device file")
    p.add_argument("lattice", help="the lattice (its element names and ideal survey)")
    p.add_argument("--out", required=True)
    p.add_argument("--cfm-source", default="compareHandZ", choices=["compareHandZ", "beamsheet"])
    p.add_argument("--global-fit", action="store_true", help="fit the ideal ring to the CFM survey first")
    a = p.parse_args()
    path = make_misalign_file(a.lattice, a.out, a.cfm_source, a.global_fit)
    devs = load(path)
    print(f"wrote {path}: {len(devs)} devices ({', '.join(f'{k} {sum(d['kind'] == k for d in devs.values())}' for k in KINDS)}); "
          f"left out (S = 0): {', '.join(n for n, d in devs.items() if d['S'] == 0)}")


if __name__ == "__main__":
    main()
