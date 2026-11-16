"""Quick start: run the Booster lattice at one time in the cycle, plain and with the survey misalignments.

    python quickstart.py              # t = 5000 us (5 ms of the cycle; injection is at 2000 us)
    python quickstart.py 10000        # another time [us]

It sets up the plain lattice and the modified lattice, runs both at t, prints the tunes, reads the twiss (optics)
and survey tables, and saves a few plots in plots/quickstart/. Change the lines marked "CHOOSE" to switch lattice,
correctors, misalignments or plots. Everything else is explained in README.md (and examples.ipynb).
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT / "helper_scripts"))
import bpm_compare as bc        # noqa: E402  model orbit at the BPMs, as the BPMs read it
import lattice_mod as lm        # noqa: E402  writes the modified lattice
import misalign as ma           # noqa: E402  survey misalignments
import run_madx as rm           # noqa: E402  runs a lattice at a time

parser = argparse.ArgumentParser(description="Run the Booster lattice at one time, plain and with misalignments")
parser.add_argument("t", nargs="?", type=float, default=5000.0, help="time in the cycle [us] (default 5000)")
t = parser.parse_args().t

# ------------------------------------------------------------------------------------------------ which lattice
# CHOOSE the lattice: "MEV400" (default) or "PIP_II".
LATTICE = "MEV400"
# LATTICE = "PIP_II"

DATA, MADX = ROOT / "data_Booster", ROOT / "madx_files"
FILES = {   # device files: BMA ramp tables (correctors, quads, sextupoles) and the GMPS settings that match the lattice
    "MEV400": [DATA / "11_03_26_BMA.json", DATA / "11_03_26_MISC_MEV400.json"],
    "PIP_II": [DATA / "11_03_26_BMA_PIP_II.json", DATA / "11_03_26_MISC_PIP_II.json"],
}
EXTRA = {   # extra settings per lattice: PIP-II painting offsets at the injection point [m] (painting runs at injection)
    "MEV400": {},
    "PIP_II": {"Y_ORBUMP": -0.097, "Y_PM": -0.016, "X_PM": 0.0061},
}
lattice = MADX / f"{LATTICE}_Booster_lattice.madx"
devices = [str(f) for f in FILES[LATTICE]]
pool = rm.load_devices(devices)
correctors_off = {**rm.batch_override(pool, 0.0, kind="H"), **rm.batch_override(pool, 0.0, kind="V")}

# ------------------------------------------------------------------------------------------------ the plain lattice
# Base lattice with the H and V corrector magnets turned off (B:BFLAT stays on), RF off. beam_field=True: the magnet
# strengths are the field the beam sees at t (eddy currents in the beam pipe delay it behind the current; fitted for
# the dipoles, checked on the machine with B:BFLAT, scaled to the quadrupoles and sextupoles); False uses the currents.
BEAM_FIELD = True
base = rm.prepare(rm.Config(lattice=str(lattice), devices=devices, overrides={**correctors_off, **EXTRA[LATTICE]},
                            rf="off", beam_field=BEAM_FIELD))
# CHOOSE instead: all horizontal and vertical correctors on, as in the machine's tables.
# base = rm.prepare(rm.Config(lattice=str(lattice), devices=devices, overrides=EXTRA[LATTICE], rf="off",
#                             beam_field=BEAM_FIELD))
# (RF on: add str(DATA / "rf_phase_curves.json") and, for MEV400, str(DATA / "rfsum_smoothed.json") to devices and
#  leave out rf="off".)

# ------------------------------------------------------------------------------------------------ the modified lattice
# Modified lattice: the lattice plus elements that are in the machine but not surveyed yet (extraction kickers BEX2-4,
# the L2 MKS kickers, old-style BPMs, from an archived lattice), plus the survey misalignments (DX..DPSI of every
# surveyed magnet and the elements it holds, from data_Booster/11_13_26_MISALIGN_<lattice>.json). Misalignments are
# on by default. This rewrites madx_files/<lattice>_modified.madx (and its record ..._modified_misalign.json).
misalign_file = lm.DEFAULT_MISALIGN.get(lattice.stem)
devs = ma.load(misalign_file)                # the surveyed devices: kind, measured offsets, ...
modified_lattice = lm.make_modified(lattice)
# CHOOSE instead: the modified lattice without misalignments.
# modified_lattice = lm.make_modified(lattice, misalign=None)
# CHOOSE instead: only the position misalignments (dx, dy, ds as surveyed; the angles dtheta, dphi, dpsi set to 0).
# modified_lattice = lm.make_modified(lattice, misalign_overrides={
#     **ma.batch(devs, 0.0, component="dtheta"), **ma.batch(devs, 0.0, component="dphi"),
#     **ma.batch(devs, 0.0, component="dpsi")})
# CHOOSE instead: every surveyed device moved by the same constant offsets (not the survey), e.g. 1 mm out and 1 mm
# up (dy is + down in MAD-X), nothing along the beam, no angles. Units: m and rad.
# CONSTANT = {"dx": 1e-3, "dy": -1e-3, "ds": 0.0, "dtheta": 0.0, "dphi": 0.0, "dpsi": 0.0}
# modified_lattice = lm.make_modified(lattice, misalign_overrides={
#     k: v for comp, value in CONSTANT.items()
#     for k, v in ma.batch(devs, value, component=comp, include_left_out=True).items()})
modified = rm.prepare(rm.Config(lattice=str(modified_lattice), devices=devices, overrides=base.cfg.overrides,
                                rf=base.cfg.rf, beam_field=base.cfg.beam_field))

# ------------------------------------------------------------------------------------------------ run both at t
# Each run overwrites the same output files in madx_files/, so the tables are read right after each run.
results = {}
for name, setup in (("plain", base), ("modified", modified)):
    res = rm.run_point(setup, t)
    results[name] = {"res": res,
                     "twiss": res.twiss,                          # optics: madx_files/<lattice>_twiss.outx
                     "survey": rm.output(setup, "survey"),        # geometry: madx_files/<lattice>_survey.outx
                     "bpm": bc.model_orbit(res.twiss)}            # orbit at the 48 BMA BPMs as they read it [mm]
    if "survey_perm" in setup.outputs:                            # MAD-X's own misaligned survey (modified lattice)
        results[name]["survey_perm"] = rm.output(setup, "survey_perm")
    bpm = results[name]["bpm"]
    print(f"{name:9s} {Path(setup.cfg.lattice).name}: t = {t:g} us, KE {res.ke_true:.4f} GeV, Q1 {res.q1:.4f}, "
          f"Q2 {res.q2:.4f}, chromaticity {res.chrom1:+.2f} {res.chrom2:+.2f}, BPM orbit rms x "
          f"{np.sqrt(np.mean(bpm.x ** 2)):.1f} mm, y {np.sqrt(np.mean(bpm.y ** 2)):.1f} mm, "
          f"{len(setup.bpm_offsets)} misaligned elements")

print(f"""
Where the information is:
  twiss (optics, orbit) and survey tables of the last run: plain lattice {MADX}/{LATTICE}_Booster_twiss.outx,
      _survey.outx; modified lattice {LATTICE}_Booster_modified_twiss.outx, _survey.outx, _survey_perm.outx (the
      misaligned survey from MAD-X: its translations are right, its rotations are not, see README)
  MAD-X input values of the last run: {MADX}/Common_Booster_input.dat; MAD-X log: {MADX}/madx.log
  misalignments: {misalign_file} (measured, per device),
      {MADX}/{LATTICE}_Booster_lattice_modified_misalign.json (what the modified lattice got, per device and element)
  in Python: results["plain" or "modified"]["twiss"]["BETX"], ...["twiss"].row("BMAL8_BPM"), ...["bpm"], ...["res"].q1
""")

# ------------------------------------------------------------------------------------------------ plots
out = ROOT / "plots" / "quickstart"
out.mkdir(parents=True, exist_ok=True)
tag = f"{LATTICE}_{t / 1000:g}ms"


MADX_COLOURS = ("black", "red", "green", "blue")   # MAD-X's plot colours, in its order


def optics(name: str) -> None:
    """beta x, beta y, Dx, Dy along the ring for one lattice ("plain" or "modified"), four lines on one plot [m]."""
    tw = results[name]["twiss"]
    fig, ax = plt.subplots(figsize=(12, 5), layout="constrained")
    for col, label, colour in zip(("BETX", "BETY", "DX", "DY"), ("beta x", "beta y", "Dx", "Dy"), MADX_COLOURS):
        ax.plot(tw["S"], tw[col], lw=1, color=colour, label=label)
    ax.set_xlabel("s [m]")
    ax.set_xlim(0.0, float(tw["S"][-1]))            # s = 0 to the ring length
    ax.set_ylabel("[m]")
    ax.grid(alpha=0.3)
    ax.legend(ncol=4, fontsize=8)
    ax.set_title(f"{LATTICE} optics, {name} lattice, t = {t / 1000:g} ms")
    fig.savefig(out / f"optics_{name}_{tag}.png", dpi=150)


def closed_orbit(name: str) -> None:
    """Closed orbit x, x', -y, -y' along the ring for one lattice, four lines on one plot (x + out, -y + up: MAD-X's y
    is + down in this lattice)."""
    tw = results[name]["twiss"]
    fig, ax = plt.subplots(figsize=(12, 5), layout="constrained")
    for col, sign, label, colour in zip(("X", "PX", "Y", "PY"), (1, 1, -1, -1),
                                        ("x [mm] (+ out)", "x' [mrad]", "-y [mm] (+ up)", "-y' [mrad]"), MADX_COLOURS):
        ax.plot(tw["S"], sign * 1e3 * tw[col], lw=1, color=colour, label=label)
    ax.set_xlabel("s [m]")
    ax.set_xlim(0.0, float(tw["S"][-1]))            # s = 0 to the ring length
    ax.set_ylabel("[mm], [mrad]")
    ax.grid(alpha=0.3)
    ax.legend(ncol=4, fontsize=8)
    ax.set_title(f"{LATTICE} closed orbit, {name} lattice, t = {t / 1000:g} ms")
    fig.savefig(out / f"closed_orbit_{name}_{tag}.png", dpi=150)


def survey_errors() -> None:
    """Position error of every surveyed device along the ring, as applied in the modified lattice [mm]: s (along the
    beam), x (+ outward), -y (+ up)."""
    record = modified_lattice.with_name(modified_lattice.stem + lm.RECORD_SUFFIX)   # what the lattice got
    if misalign_file is None or not record.exists():
        print("survey errors plot skipped: the modified lattice has no misalignments")
        return
    devs = ma.load(misalign_file)
    with open(record) as f:
        got = json.load(f)["devices"]                                 # devices with a nonzero applied offset
    applied = {n: got.get(n, dict.fromkeys(ma.KEYS, 0.0)) for n in devs}
    names = list(devs)
    tw = results["modified"]["twiss"]
    s = np.array([0.5 * (tw.row(d["up"])["S"] + tw.row(d["dn"])["S"]) for d in devs.values()])
    fig, axes = plt.subplots(3, 1, sharex=True, figsize=(12, 8), layout="constrained")
    for ax, (k, sign, label) in zip(axes, (("ds", 1, "s [mm]"), ("dx", 1, "x [mm] (+ out)"), ("dy", -1, "-y [mm] (+ up)"))):
        for (kind, marker), colour in zip((("CFM", "o"), ("BMA", "s"), ("OTHER", "d")), MADX_COLOURS):
            idx = [i for i, d in enumerate(devs.values()) if d["kind"] == kind]
            ax.plot(s[idx], [sign * 1e3 * applied[names[i]][k] for i in idx], marker, ms=4, ls="none", color=colour,
                    label=kind)
        ax.axhline(0, color="k", ls="--", lw=0.8)
        ax.set_ylabel(label)
        ax.grid(alpha=0.3)
    axes[0].legend(ncol=3, fontsize=8)
    axes[-1].set_xlabel("s [m]")
    axes[-1].set_xlim(0.0, float(tw["S"][-1]))      # s = 0 to the ring length
    axes[0].set_title(f"{LATTICE}: survey misalignments applied in the modified lattice (0 = left out)")
    fig.savefig(out / f"survey_errors_{LATTICE}.png", dpi=150)


# CHOOSE the plots (comment out the ones you don't want).
optics("plain")
optics("modified")
closed_orbit("plain")
closed_orbit("modified")
survey_errors()
print(f"plots saved in {out}")
# plt.show()        # CHOOSE: also open them in windows (needs a display)
