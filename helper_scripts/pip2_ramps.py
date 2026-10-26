"""PIP-II sample ramps from the MEV400 ramps, matched by beam rigidity (user's method, 2026-10-25).

The PIP-II GMPS ramp (20 Hz, injection 0.79 GeV) is a compressed version of the MEV400 one (15 Hz, 0.39 GeV),
both with the same BDOT and KEMAX. A table breakpoint whose magnet (or beam) sees Brho at cycle time t on the
MEV400 ramp goes to the PIP-II time with the same Brho, and keeps its value: the same current at the same Brho
gives the same kick or strength.
- MEV400 cycle time t = table time + dtoff (the magnet / RF delay is kept on the PIP-II side too).
- Below the PIP-II injection Brho (MEV400 flat injection and its ramp up to the PIP-II injection energy, to
  t_inj, ~7.7 ms): no PIP-II counterpart; those breakpoints are dropped.
- BMA tables (H, V, Q, SQ, SX, SS) through PIP-II flat injection (user, 2026-10-25): the same strength as MEV400
  at its injection, T_REF = 2000 us: the device current I = S * T + O is the MEV400 one at T_REF x
  Brho(PIP-II flat) / Brho(MEV400 at T_REF) (the table takes it all, offset unchanged). One breakpoint at the end
  of the PIP-II flat region holds it; from there the table interpolates to the next Brho-matched breakpoint.
  (RF phases: the value at t_inj is held there instead.)
- Rising ramp: t_pip = the time the PIP-II ramp reaches the same KE (rt.time_gmps). The tops map onto each
  other (same KEMAX).
- Past the MEV400 top (beam Brho held at the top in the model): shifted by the difference of the top times.
- Tables keep at most their original number of slots (64), breakpoints 1:1 where they survive, the padding
  rebuilt (dt 0, value 0).
- B:BFLAT: stretched in time about BDOT from the MEV400 flat half-length to the PIP-II one, and its current
  scaled by Brho(PIP-II flat level) / Brho(MEV400 flat level).
- RF phase curves (RFA, RFB, beam time = t + dtoff): mapped the same way (all points); the transition phase
  jump goes with the beam energy, so it lands at PIP-II transition. The MEV400 injection part (incl. the
  paraphase switch) is dropped with the rest below the PIP-II injection Brho.
- Offsets, scale factors, maps and other devices are copied unchanged.

Notebook:
    import pip2_ramps as p2
    bma_pip, notes = p2.pip2_bma(bma, s400, s_pip)      # s400, s_pip: rt.ramp_settings of the MISC files
    ph_pip = p2.pip2_phases(phases, s400, s_pip)

Command line:
    python helper_scripts/pip2_ramps.py --bma B.json --misc400 M400.json --misc-pip MPIP.json
        --out-bma OUT_BMA.json [--phases P.json --out-phases OUT_P.json]
"""
from __future__ import annotations

import argparse
import copy
import sys
from datetime import date
from pathlib import Path

import numpy as np

import device_data as dd
import ramp_timing as rt
from device_data import Device, Devices
from ramp_timing import Settings

ROOT = Path(__file__).resolve().parents[1]
CORRECTOR_KINDS = ("H", "V", "Q", "SQ", "SX", "SS")
T_REF = 2000.0                  # [us] MEV400 time whose strengths PIP-II flat injection gets (MEV400 injection)
BFLAT = "B:BFLAT"
PHASE_DEVICES = ("RFA", "RFB")


def _vimin(s: Settings) -> float:
    return rt.vimin_flat(s["viflat"], s["vimax"], s["t_flat"], s["freq_pulse"])


def _top(s: Settings) -> float:
    return s["t_offset"] + 0.5e6 / s["freq_pulse"]


def t_inj(s400: Settings, s_pip: Settings) -> float:
    """MEV400 cycle time [us] at which its ramp reaches the PIP-II injection KE."""
    return float(rt.time_gmps(s_pip["viflat"], _vimin(s400), s400["vimax"], s400["freq_pulse"], s400["t_offset"]))


def map_time(t: np.ndarray, s400: Settings, s_pip: Settings) -> np.ndarray:
    """PIP-II cycle time(s) [us] with the same Brho as MEV400 cycle time(s) t (t >= t_inj; nan before)."""
    t = np.asarray(t, dtype=float)
    out = np.full_like(t, np.nan)
    rise = (t >= t_inj(s400, s_pip)) & (t <= _top(s400))
    ke = rt.ke_at(s400, t[rise])
    out[rise] = rt.time_gmps(np.minimum(ke, s_pip["vimax"]), _vimin(s_pip), s_pip["vimax"], s_pip["freq_pulse"],
                             s_pip["t_offset"])
    after = t > _top(s400)
    out[after] = t[after] - (_top(s400) - _top(s_pip))
    return out


def map_table(dev: Device, s400: Settings, s_pip: Settings, keep_length: bool = True,
              v_inj: float | None = None) -> Device:
    """A copy of a ramp table (t, values, dtoff) remapped to the PIP-II ramp (module docstring). v_inj: table
    value held through PIP-II flat injection (one breakpoint at its end); default the value at t_inj, and then
    only if breakpoints were dropped."""
    t, v = dd.ramp(dev)
    dtoff = float(dev.get("dtoff", 0.0))
    t_cyc = t + dtoff
    ti = t_inj(s400, s_pip)
    keep = t_cyc >= ti
    new_t, new_v = map_time(t_cyc[keep], s400, s_pip) - dtoff, v[keep]
    if v_inj is not None or (~keep).any():
        v_inj = float(np.interp(ti, t_cyc, v)) if v_inj is None else v_inj
        new_t = np.concatenate([[s_pip["t_offset"] + s_pip["t_flat"] - dtoff], new_t])
        new_v = np.concatenate([[v_inj], new_v])
    return _with_table(dev, new_t, new_v, keep_length)


def injection_value(bma: Devices, dev: Device, s400: Settings, s_pip: Settings, t_ref: float = None) -> float:
    """Table value that gives the device the MEV400 strength at t_ref (T_REF) at the PIP-II flat level:
    (I(t_ref) x Brho ratio - O) / S, with I = S * T + O of the device's group."""
    t_ref = T_REF if t_ref is None else t_ref
    parts = dd.groups(bma)[dev["group"]]
    sf = float(bma[parts["scale"]]["sf"]) if "scale" in parts else 1.0
    off = float(bma[parts["offset"]]["value"]) if "offset" in parts else 0.0
    ratio = float(rt.ke_to_brho(s_pip["viflat"]) / rt.ke_to_brho(rt.ke_at(s400, t_ref)))
    return (float(dd.value_at(bma, dev["group"], t_ref)) * ratio - off) / sf


def stretch_bflat(dev: Device, s400: Settings, s_pip: Settings, keep_length: bool = True) -> Device:
    """B:BFLAT table stretched about BDOT by t_flat(PIP-II) / t_flat(MEV400), current x Brho ratio of the flat
    levels (same BDOT on both)."""
    t, v = dd.ramp(dev)
    dtoff = float(dev.get("dtoff", 0.0))
    bd = s400["t_offset"]
    k = s_pip["t_flat"] / s400["t_flat"]
    scale = float(rt.ke_to_brho(s_pip["viflat"]) / rt.ke_to_brho(s400["viflat"]))
    new_t = s_pip["t_offset"] + (t + dtoff - bd) * k - dtoff
    out = _with_table(dev, new_t, v * scale, keep_length)
    out["note"] = (f"PIP-II: MEV400 B:BFLATT stretched x {k:.4f} in time about BDOT, current x {scale:.5f} "
                   f"(Brho of the flat levels); pip2_ramps.py")
    return out


def _with_table(dev: Device, t: np.ndarray, v: np.ndarray, keep_length: bool) -> Device:
    n0 = len(dev["t"]) if keep_length else len(t)
    if len(t) > n0:
        raise ValueError(f"{len(t)} breakpoints, table has {n0} slots")
    pad = n0 - len(t)
    out = copy.deepcopy(dev)
    out["t"] = np.concatenate([t, np.full(pad, t[-1])])
    out["values"] = np.concatenate([v, np.zeros(pad)])
    out["dt"] = np.diff(np.concatenate([[0.0], out["t"]]))
    return out


def pip2_bma(bma: Devices, s400: Settings, s_pip: Settings) -> Devices:
    """PIP-II BMA devices: corrector tables (CORRECTOR_KINDS) remapped (map_table) with the MEV400 injection
    strength through PIP-II flat injection (injection_value), B:BFLAT stretched and scaled (stretch_bflat),
    everything else copied."""
    out = {}
    for name, dev in bma.items():
        if dev.get("role") == "table" and dev.get("kind") in CORRECTOR_KINDS:
            out[name] = map_table(dev, s400, s_pip, v_inj=injection_value(bma, dev, s400, s_pip))
        elif dev.get("role") == "table" and dev.get("group") == BFLAT:
            out[name] = stretch_bflat(dev, s400, s_pip)
        else:
            out[name] = copy.deepcopy(dev)
    return out


def pip2_phases(phases: Devices, s400: Settings, s_pip: Settings) -> Devices:
    """PIP-II RF phase devices: RFA, RFB remapped by beam Brho (all points), the rest copied."""
    return {n: map_table(d, s400, s_pip, keep_length=False) if n in PHASE_DEVICES else copy.deepcopy(d)
            for n, d in phases.items()}


# ---------------------------------------------------------------- command line
def main(argv: list[str] | None = None) -> None:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--bma", required=True, help="MEV400 BMA file")
    ap.add_argument("--misc400", required=True, help="MEV400 MISC file (ramp settings)")
    ap.add_argument("--misc-pip", required=True, help="PIP-II MISC file (ramp settings)")
    ap.add_argument("--out-bma", required=True, help="PIP-II BMA file to write")
    ap.add_argument("--phases", help="MEV400 RF phase curve file")
    ap.add_argument("--out-phases", help="PIP-II RF phase curve file to write")
    a = ap.parse_args(argv)
    p = lambda f: ROOT / f
    s400 = rt.ramp_settings(dd.load(p(a.misc400))[0])
    s_pip = rt.ramp_settings(dd.load(p(a.misc_pip))[0])
    ti = t_inj(s400, s_pip)
    head = (f"{date.today().isoformat()}: PIP-II sample ramps from the MEV400 ramps by "
            f"Brho (pip2_ramps.py; settings {Path(a.misc400).name} -> {Path(a.misc_pip).name}): MEV400 breakpoints "
            f"before {ti:.0f} us (PIP-II injection KE {s_pip['viflat']:.4f} GeV) dropped; BMA tables through PIP-II "
            f"flat injection (to {s_pip['t_offset'] + s_pip['t_flat']:g} us) at the MEV400 strength of {T_REF:g} us, "
            f"RF phases at the value at {ti:.0f} us; rising ramp matched by KE; past the top shifted "
            f"by {_top(s400) - _top(s_pip):.0f} us.")
    bma, meta = dd.load(p(a.bma))
    dd.save(pip2_bma(bma, s400, s_pip), p(a.out_bma),
            {"source": a.bma, "notes": head + " B:BFLAT stretched in time and scaled by Brho."})
    msg = [a.out_bma]
    if a.phases and a.out_phases:
        ph, _ = dd.load(p(a.phases))
        dd.save(pip2_phases(ph, s400, s_pip), p(a.out_phases), {"source": a.phases, "notes": head})
        msg.append(a.out_phases)
    print(f"t_inj {ti:.0f} us; written: " + ", ".join(msg))


if __name__ == "__main__":
    sys.path.insert(0, str(Path(__file__).parent))
    main()
