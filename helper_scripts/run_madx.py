"""Run a Booster MAD-X lattice at given times along the ramp (one point, or a loop / scan).

Steps (the user's outline):
    A/B  prepare(cfg): read all device files into one pool of unique devices, apply the
         overrides (constants that replace devices by name, apply_overrides), ramp settings
         (rt.RAMP_DEVICES: B:VIMIN, B:VIMAX [A], TFLAT, ... from a *_MISC_*.json file) from the
         pool (injection KE checked against the lattice's KE_INJ), RF mode from the devices present.
    C    run_point(setup, t): KE_true, KE, dKE/turn and RF (madx_input.rf_values; LAG = phase / 360).
    D    write Common_Booster_input.dat; for the PIP-II lattice (it calls
         PIP_II_Booster_injection.dat) run twice: pass 1 with all painting angles 0, then
         the angles from pass 1's sectormap (painting.kick_angles); run MAD-X.

The lattice runs in place, in its own folder (madx_files/), like running ./madx there by hand:
every run overwrites the same files there (Common_Booster_input.dat, PIP_II_Booster_injection.dat,
the lattice's .outx outputs, and madx.log). So read what you need from the tables after each run,
before the next one: run_point returns the tunes and the twiss table; output(setup, table) reads
any other output (e.g. "sectormap_table", "survey"); scan(..., collect=f) calls f after each point.

Paths in a Config are relative to the working directory (the project root).

Notebook:
    import run_madx as rm
    cfg = rm.Config(lattice="madx_files/PIP_II_Booster_lattice.madx",
                    devices=["data_Booster/11_03_26_BMA_PIP_II.json", "data_Booster/11_03_26_MISC_PIP_II.json"],
                    overrides={"B:HL3": 0.0, "Y_PM": -0.016})
    pool = rm.load_devices(cfg.devices)
    cfg.overrides.update(rm.batch_override(pool, 0.0, kind="SQ"))     # all skew quads at 0 A
    setup = rm.prepare(cfg)
    res = rm.run_point(setup, 2400.0)          # res.q1, res.chrom1, res.twiss["BETX"]
    df = rm.scan(setup, np.linspace(2000, 30000, 20),
                 collect=lambda res: {"betx_inj": res.twiss.row("TREATYPOINT")["BETX"]})

Command line (from the project root; prints one line per point, --csv to save the table):
    python helper_scripts/run_madx.py --lattice madx_files/MEV400_Booster_lattice.madx \\
        --devices data_Booster/11_03_26_BMA.json data_Booster/11_03_26_MISC_MEV400.json \\
        --batch H=0 V=0 --override B:QL1=0 --t 2400 5000
"""

import argparse
import json
import re
import subprocess
import sys
import warnings
from collections.abc import Callable
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any

import numpy as np

import device_data as dd
import madx_input as mxin
import painting
import ramp_timing as rt
from device_data import Device, Devices
from tfs_reader import TfsTable

COMMON_FILE = "Common_Booster_input.dat"     # the name the lattices call
MADX = "madx_files/madx"
LOG_FILE = "madx.log"                        # MAD-X screen output of the last run, in the lattice folder
RAMP_NAMES = tuple(rt.RAMP_DEVICES.values())   # rt.ramp_settings
_KE_INJ = re.compile(r"^\s*KE_INJ\s*:?=\s*([-+.\deE]+)\s*;", re.M)
KE_INJ_TOL = 0.1   # relative; the injection KE from the currents vs the lattice's KE_INJ (wrong MISC file)


@dataclass
class Config:
    """Everything that defines a run (Config.save / Config.load for a JSON copy).

    lattice     MAD-X lattice script (e.g. madx_files/PIP_II_Booster_lattice.madx); it runs in
                its own folder. PIP-II (it calls painting.INJECTION_FILE) runs two-pass with painting.
    devices     device JSON files, read into one pool; a name in two files raises ValueError.
                Must include the ramp settings (rt.RAMP_DEVICES, the lattice's *_MISC_*.json file);
                their injection KE must be within KE_INJ_TOL of the lattice's KE_INJ.
    overrides   {name: value}: constants that replace devices of the pool (apply_overrides).
                A group ("B:QL1") replaces its whole value S * T(t - dtoff) + O; a part
                ("B:QL1O") replaces only that part. B:HL3 = 0 leaves the B:BFLAT part of I_HL3
                on (override "B:BFLAT" for that). Painting offsets Y_ORBUMP, Y_PM, X_PM [m] are
                overrides too. batch_override(pool, value, kind=...) gives one value for every
                device of a kind.
    ke_source   name of a frequency device in the pool (e.g. "tblFreqCurveFF") for KE_true
                and dKE/turn instead of ramp_flat.
    rf          "auto" (RF mode from the devices present, see madx_input) or "off".
    madx        MAD-X binary.
    beam_field  True: the magnet inputs are the field seen by the beam at t (madx_input.field_at),
                not the current at t.
    """
    lattice: str
    devices: list[str]
    overrides: dict[str, float] = field(default_factory=dict)
    ke_source: str | None = None
    rf: str = "auto"
    madx: str = MADX
    beam_field: bool = False

    def save(self, path: str | Path) -> Path:
        """Write this config as JSON."""
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(asdict(self), indent=2))
        return path

    @classmethod
    def load(cls, path: str | Path) -> "Config":
        """Read a config written by save()."""
        return cls(**json.loads(Path(path).read_text()))


@dataclass
class Setup:
    """Everything prepare() loads once for a run or scan."""
    cfg: Config
    devs: Devices                    # the pool after the overrides
    settings: rt.Settings
    ke_source: Device | None
    rf_mode: str
    folder: Path                     # the lattice's folder: MAD-X runs here
    outputs: dict[str, str]          # MAD-X table name (lower case) -> output file name
    painting: bool
    overridden: list[str]            # override names applied
    ignored: list[str]               # override names that matched nothing (warned)
    _dke_table: tuple[np.ndarray, np.ndarray] | None = None
    bpm_offsets: dict[str, tuple[float, float]] = field(default_factory=dict)   # {element: (DX, DY) [m]} from the
    #   lattice's misalignment record (lattice_mod); bpm_compare subtracts them at the BPMs

    def dke(self, t: float) -> float:
        """Energy gain per turn [GeV/turn] at t [us] (turns computed once)."""
        if t < 0:
            return 0.0
        if self._dke_table is None:
            src = self.ke_source if self.ke_source is not None else self.settings
            t_n, _, dke_n = rt.turn_energy(src, circumference=self.settings["circumference"])
            self._dke_table = (t_n, dke_n)
        return float(rt.dke_at(None, t, table=self._dke_table))


@dataclass
class Result:
    """One point: t [us], energies [GeV], RF mode, tunes, painting, and its twiss table."""
    t: float
    ke_true: float
    ke: float
    dke: float | None                # GeV/turn (RF mode "phase" only)
    rf_mode: str
    q1: float
    q2: float
    dq1: float                       # MAD-X DQ (d Q / d PT)
    dq2: float
    chrom1: float                    # beta * DQ = dQ / (dp/p)
    chrom2: float
    twiss: TfsTable                  # read right after the run (the file is overwritten by the next)
    inputs: mxin.InputValues           # everything written to Common_Booster_input.dat: inputs["I_QL1"], ["V01"]
    offsets: dict[str, float] = field(default_factory=dict)
    angles: dict[str, float] = field(default_factory=dict)

    def row(self) -> dict[str, Any]:
        """Flat dict of the scalars (no twiss table), for a scan table."""
        out = {k: getattr(self, k) for k in ("t", "ke_true", "ke", "dke", "rf_mode", "q1", "q2",
                                            "dq1", "dq2", "chrom1", "chrom2")}
        out.update(self.offsets)
        out.update(self.angles)
        return out


def load_devices(paths: list[str | Path]) -> Devices:
    """One pool of devices from several JSON files; a name in more than one file raises ValueError."""
    pool: Devices = {}
    where: dict[str, str] = {}
    for p in paths:
        devs, _ = dd.load(p)
        dup = sorted(set(devs) & set(pool))
        if dup:
            raise ValueError(f"devices in both {where[dup[0]]} and {p}: {dup[:10]}"
                             + (f" (+{len(dup) - 10} more)" if len(dup) > 10 else ""))
        pool.update(devs)
        where.update(dict.fromkeys(devs, str(p)))
    return pool


def input_names() -> set[str]:
    """Names the MAD-X inputs read that an override may add when the pool lacks them: the 288
    currents, B:BFLAT, the extraction kickers (B:BEX2..4, MKSON), the RF voltages, the painting offsets
    and the ramp settings."""
    return (set(mxin.current_names()) | {"B:BFLAT"} | set(mxin.KICKERS.values()) | set(painting.OFFSETS)
            | set(RAMP_NAMES)
            | {mxin.RF_DEVICE.format(n) for n in range(1, mxin.N_RF + 1)})


def apply_overrides(pool: Devices, overrides: dict[str, float]) -> tuple[Devices, list[str], list[str]]:
    """(devices with the overrides applied, names applied, names ignored); pool is not changed.

    Each override {name: value} is a constant that replaces a device by name:
        group ("B:QL1")        all its parts are replaced by one device {"role": "value",
                               "value": value}: value_at(devs, "B:QL1", t) = value
        part ("B:QL1O", "B:QL1T", "B:BFLATS")
                               only that part: offset/value/table -> "value", scale -> "sf";
                               the rest of the group stays (B:QL1 = S * T(t - dtoff) + value)
        not in the pool        added as a value device if the inputs read it (input_names);
                               anything else is ignored with a warning (e.g. a typo)
    Each replaced device is marked "override": True.
    """
    devs = dict(pool)
    grp = dd.groups(pool)
    applied, ignored = [], []
    for name, value in overrides.items():
        value = float(value)
        if name in grp:
            parts = [pool[p] for p in grp[name].values()]
            for p in grp[name].values():
                devs.pop(p)
            new = {k: parts[0][k] for k in ("kind", "unit") if k in parts[0]}
            devs[name] = {**new, "group": name, "role": "value", "value": value, "override": True}
        elif name in pool:
            old = pool[name]
            new = {k: old[k] for k in ("kind", "unit", "group", "role", "dtoff") if k in old}
            new.update({"sf": value} if old.get("role") == "scale" else {"value": value})
            devs[name] = {**new, "override": True}
        elif name in input_names():
            devs[name] = {"group": name, "role": "value", "value": value, "override": True}
        else:
            ignored.append(name)
            warnings.warn(f"override {name!r} matches no device in the pool and no MAD-X input; "
                          "ignored (typo?)", stacklevel=2)
            continue
        applied.append(name)
    return devs, applied, ignored


def batch_override(devs: Devices, value: float, **match: Any) -> dict[str, float]:
    """Overrides {name: value} for every device matching dd.select(devs, **match), to add to
    Config.overrides. Without role: whole groups; with role: only those parts.

    batch_override(pool, 0.0, kind="SQ")                 # {"B:SQL1": 0.0, ...}: all skew quads at 0 A
    batch_override(pool, 0.5, kind="V")                  # every vertical corrector at 0.5 A
    batch_override(pool, 0.0, kind="Q", role="offset")   # {"B:QL1O": 0.0, ...}: only the quad offsets
    B:BFLAT has its own kind ("BFLAT"), so kind="H" leaves it alone.
    """
    picked = dd.select(devs, **match)
    if "role" in match:
        return dict.fromkeys(picked, float(value))
    return {dev.get("group", name): float(value) for name, dev in picked.items()}


_CALL = re.compile(r"^\s*call\s*,\s*file\s*=\s*['\"]([^'\"]+)['\"]", re.I | re.M)
_WRITE = re.compile(r"^\s*write\s*,\s*file\s*=\s*['\"]([^'\"]+)['\"]\s*,\s*table\s*=\s*(\w+)", re.I | re.M)
_SURVEY = re.compile(r"^\s*survey\s*,.*?file\s*=\s*['\"]([^'\"]+)['\"]", re.I | re.M)
_RECORD = re.compile(r"^!\s*Misalignments:.*record:\s*(\S+)", re.M)


def prepare(cfg: Config) -> Setup:
    """Steps A/B: load the devices once and read which files the lattice calls and writes."""
    devs, applied, ignored = apply_overrides(load_devices(cfg.devices), cfg.overrides)
    try:
        settings = rt.ramp_settings(devs)
    except KeyError as e:
        raise KeyError(f"ramp setting device {e} not in the devices (add a *_MISC_*.json file)") from None
    ke_source = None
    if cfg.ke_source is not None:
        if cfg.ke_source not in devs:
            raise KeyError(f"ke_source {cfg.ke_source!r} not in the devices")
        ke_source = devs[cfg.ke_source]
    if cfg.rf not in ("auto", "off"):
        raise ValueError(f"rf must be 'auto' or 'off', not {cfg.rf!r}")
    mode = mxin.rf_mode(devs) if cfg.rf == "auto" else "off"
    text = Path(cfg.lattice).read_text()
    m = _KE_INJ.search(text)
    if m and abs(settings["viflat"] / float(m.group(1)) - 1) > KE_INJ_TOL:
        raise ValueError(f"injection KE {settings['viflat']:.4f} GeV from the devices but KE_INJ = {m.group(1)} GeV in "
                         f"{cfg.lattice} (MISC file of the other lattice?)")
    calls = _CALL.findall(text)
    if COMMON_FILE not in calls:
        raise ValueError(f"{cfg.lattice} does not call {COMMON_FILE}")
    outputs = {table.lower(): f for f, table in _WRITE.findall(text)}
    surveys = _SURVEY.findall(text)
    if surveys:                                          # lattice_mod adds a PERM_ALIGN_SURVEY one: "survey_perm"
        outputs["survey"] = surveys[0]
        outputs.update({"survey_perm": f for f in surveys[1:] if "_perm" in f})
    if "twiss" not in outputs:
        raise ValueError(f"{cfg.lattice} does not WRITE the twiss table")
    paint = painting.INJECTION_FILE in calls
    if paint and "sectormap_table" not in outputs:
        raise ValueError(f"{cfg.lattice} calls {painting.INJECTION_FILE} but does not WRITE SECTORMAP_TABLE")
    bpm_offsets = {}
    if m := _RECORD.search(text):                        # a misaligned modified lattice (lattice_mod)
        import misalign
        bpm_offsets = misalign.bpm_offsets(Path(cfg.lattice).parent / m.group(1))
    return Setup(cfg, devs, settings, ke_source, mode, Path(cfg.lattice).parent, outputs, paint,
                 applied, ignored, bpm_offsets=bpm_offsets)


def output(setup: Setup, table: str = "twiss") -> TfsTable:
    """The last run's output table: "twiss", "sectormap_table", "survey", "survey_perm" (modified lattices; see
    setup.outputs)."""
    return TfsTable(setup.folder / setup.outputs[table.lower()])


def _run(setup: Setup) -> None:
    """Run the lattice in its folder; raises RuntimeError unless MAD-X finished normally and wrote
    a new twiss file (MAD-X can exit 0 after a failed twiss)."""
    twiss = setup.folder / setup.outputs["twiss"]
    before = twiss.stat().st_mtime_ns if twiss.exists() else None
    log = setup.folder / LOG_FILE
    with open(log, "w") as fh:
        proc = subprocess.run([str(Path(setup.cfg.madx).resolve()), Path(setup.cfg.lattice).name],
                              cwd=setup.folder, stdout=fh, stderr=subprocess.STDOUT)
    text = log.read_text()
    if (proc.returncode != 0 or "finished normally" not in text or "twiss failed" in text.lower()
            or not twiss.exists() or twiss.stat().st_mtime_ns == before):
        tail = "\n".join(text.splitlines()[-15:])
        raise RuntimeError(f"MAD-X failed (exit {proc.returncode}, see {log}):\n{tail}")


def run_point(setup: Setup, t: float) -> Result:
    """Steps C/D at time t [us]: write the inputs, run MAD-X (two-pass for PIP-II), read the tunes.

    Overwrites the input and output files in the lattice's folder (see the module docstring).
    """
    cfg = setup.cfg
    t = float(t)
    dke = setup.dke(t) if setup.rf_mode in ("phase", "sum") else None
    inj = setup.folder / painting.INJECTION_FILE

    offsets, angles = {}, {}
    if setup.painting:                                   # pass 1: no painting
        offsets = painting.offsets_at(setup.devs, t)
        painting.write_injection(inj, None, note=f"run_madx: t = {t} us, pass 1 (all 0)")
    vals = mxin.common_values(t, setup.devs, setup.settings, setup.ke_source, setup.rf_mode, dke,
                              beam_field=cfg.beam_field)
    mxin.write_common(setup.folder / COMMON_FILE, vals,
                    note=f"run_madx: {cfg.devices}, overrides = {sorted(setup.overridden)}")
    _run(setup)
    if setup.painting:                                   # pass 2: angles from pass 1's maps
        angles = painting.kick_angles(output(setup, "sectormap_table"),
                                      offsets["Y_ORBUMP"], offsets["Y_PM"], offsets["X_PM"])
        painting.write_injection(inj, angles, note=f"run_madx: t = {t} us, pass 2, offsets {offsets}")
        _run(setup)

    twiss = output(setup, "twiss")
    twiss.bpm_offsets = setup.bpm_offsets                # for bpm_compare.model_orbit (misaligned lattices)
    h = twiss.header
    beta = float(np.sqrt(1.0 - 1.0 / h["GAMMA"] ** 2))
    return Result(t=t, ke_true=vals["KE_true"], ke=vals["KE"], dke=dke, rf_mode=setup.rf_mode,
                  q1=h["Q1"], q2=h["Q2"], dq1=h["DQ1"], dq2=h["DQ2"],
                  chrom1=beta * h["DQ1"], chrom2=beta * h["DQ2"], twiss=twiss, inputs=vals,
                  offsets=offsets, angles=angles)


def scan(setup: Setup, times: Any, collect: Callable[[Result], dict[str, Any]] | None = None,
         verbose: bool = True) -> "pandas.DataFrame":
    """run_point at each time [us]; returns a DataFrame with one row per point (Result.row(),
    plus collect(result) if given: read anything else from the tables there, e.g.
    res.twiss or output(setup, "sectormap_table"), before the next run overwrites them).
    A point where MAD-X fails gets nan tunes and an "error" column; the scan goes on."""
    import pandas as pd
    rows = []
    for t in np.atleast_1d(np.asarray(times, dtype=float)):
        try:
            res = run_point(setup, t)
        except RuntimeError as e:          # MAD-X failed at this point: nan row, keep going
            rows.append({"t": float(t), "q1": np.nan, "q2": np.nan, "error": str(e).splitlines()[0]})
            if verbose:
                print(f"t = {t:9.1f} us  FAILED: {str(e).splitlines()[0]}")
            continue
        row = res.row()
        if collect is not None:
            row.update(collect(res))
        rows.append(row)
        if verbose:
            print(f"t = {t:9.1f} us  KE_true = {res.ke_true:.4f} GeV  Q1 = {res.q1:.4f}  Q2 = {res.q2:.4f}  "
                  f"xi1 = {res.chrom1:.3f}  xi2 = {res.chrom2:.3f}")
    return pd.DataFrame(rows)


def _parse_overrides(items: list[str], flag: str = "--override") -> dict[str, float]:
    out = {}
    for item in items:
        name, _, val = item.partition("=")
        try:
            out[name] = float(val)
        except ValueError:
            raise SystemExit(f"{flag} takes NAME=VALUE (a number), got {item!r}") from None
    return out


def main(argv: list[str] | None = None) -> None:
    """Command line: see the module docstring (python helper_scripts/run_madx.py -h)."""
    p = argparse.ArgumentParser(description="Run a Booster MAD-X lattice at times along the ramp.")
    p.add_argument("config", nargs="?", help="config JSON (Config.save); the options below override it")
    p.add_argument("--lattice")
    p.add_argument("--devices", nargs="+", help="device JSON files (one pool)")
    p.add_argument("--override", nargs="*", default=[], help="overrides NAME=VALUE (group, part, "
                                                            "or painting offset Y_ORBUMP, Y_PM, X_PM [m])")
    p.add_argument("--batch", nargs="*", default=[], help="KIND=VALUE (e.g. H=0 SQ=0): every device of "
                                                         "the kind overridden to VALUE")
    p.add_argument("--ke-source", help="frequency device name for KE_true")
    p.add_argument("--rf", choices=["auto", "off"])
    p.add_argument("--madx", help="MAD-X binary")
    p.add_argument("--beam-field", action="store_true", default=None,
                   help="magnet inputs = the field seen by the beam at t (madx_input.field_at)")
    p.add_argument("--csv", help="also save the scan table here")
    times = p.add_mutually_exclusive_group(required=True)
    times.add_argument("--t", nargs="+", type=float, help="times [us]")
    times.add_argument("--linspace", nargs=3, type=float, metavar=("START", "STOP", "N"),
                       help="N times from START to STOP [us]")
    a = p.parse_args(argv)

    base = asdict(Config.load(a.config)) if a.config else {}
    for key in ("lattice", "devices", "ke_source", "rf", "madx", "beam_field"):
        val = getattr(a, key)
        if val is not None:
            base[key] = val
    if "lattice" not in base or "devices" not in base:
        p.error("give a config file or --lattice and --devices")
    cfg = Config(**base)
    if a.batch:
        pool = load_devices(cfg.devices)
        for kind, value in _parse_overrides(a.batch, "--batch").items():
            cfg.overrides.update(batch_override(pool, value, kind=kind))
    cfg.overrides.update(_parse_overrides(a.override))
    t = a.t if a.t is not None else np.linspace(a.linspace[0], a.linspace[1], int(a.linspace[2]))
    setup = prepare(cfg)
    print(f"{cfg.lattice}: {len(setup.devs)} devices, {len(setup.overridden)} overrides"
          + (f" ({len(setup.ignored)} ignored: {setup.ignored})" if setup.ignored else "") + ", "
          f"RF mode {setup.rf_mode}, painting {'on (two-pass)' if setup.painting else 'off'}")
    df = scan(setup, t)
    if a.csv:
        df.to_csv(a.csv, index=False)
        print(f"wrote {a.csv}")


if __name__ == "__main__":
    main(sys.argv[1:])
