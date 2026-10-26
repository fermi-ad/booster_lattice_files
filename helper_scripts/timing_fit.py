"""Timing of the GMPS ramp and the RF curves (user's method, 2026-10-25).

1. BMA magnets (correctors and B:BFLAT) respond 156 us after their table ("dtoff"); B:BEX2 too (same
   ramp cards); not the GMPS. The response is taken as a pure delay (the real B:BFLAT shape is asymmetric from the
   circuit and beam pipe response; ignored).
2. B:BFLAT's nontrivial part, from the last zero point before it to the first zero point after it, plus
   its dtoff, is the flat injection region: BDOT (GMPS minimum, B:BDOT = 0) = its centre, TFLAT = half its
   length (bflat_window, flat_from_bflat). 10_23_26 BMA: 1986-2556 us -> BDOT 2271, TFLAT 285.
   PIP-II keeps its own TFLAT (and 20 Hz) but the same BDOT.
3. The ramp: B:VIMIN, B:VIMAX and KEMAX (ramp_timing.ramp_settings; B:VIMIN = trough at BDOT, injection KE =
   the sine at BDOT + TFLAT). KEMAX: the top energy the RF curve implies (ke_max_rf below, user 2026-10-25)
   until RR measurements give it (it follows the day-to-day tuning to the Recycler).
4. RF curves: their tables start at the programmed delay (t_start, 1875 us); "dtoff" is the extra
   (hardware) delay on top of it, which stays the same when the programmed delay changes. fit_rf_offset fits
   it to the ramp: pc_curve(t - dtoff) = scale * pc_ramp(t). The pc scale is free so that a KEMAX error
   doesn't bias the time; scale * KEMAX (ke_max_rf) is what the RF curve says the top energy is (written as
   KEMAX by --apply; the fitted time doesn't depend on KEMAX). The RF
   phase curves (RFA, RFB) get the same dtoff (same LLRF).
   Transition vs the phase jump is reported only (transition moves with other optics changes).

Notebook:
    import timing_fit as tf
    bma, misc, rf, ph = (dd.load(p)[0] for p in (...))
    flat = tf.flat_from_bflat(bma)                       # {"BDOT": 2271.0, "TFLAT": 285.0}
    s = tf.settings_with(misc, flat)                     # ramp_settings with that BDOT/TFLAT
    fit = tf.fit_rf_offset(s, rf["tblFreqCurveFF"])      # fit["dtoff"], fit["scale"], fit["rms"], ...
    tf.plot_timing(bma, s, rf["tblFreqCurveFF"], fit, ph, path="plots/timing_fit.png")

Command line (prints the results; --apply writes BDOT/TFLAT, KEMAX and the RF dtoff into the files):
    python helper_scripts/timing_fit.py --bma B.json --misc MEV400.json [--same-bdot PIP_II.json ..]
        --rf FREQ.json [--phases PHASES.json] [--window 2600 35000] [--gamma-t 5.592] [--plot FILE] [--apply]
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

import numpy as np
from scipy.optimize import least_squares

import device_data as dd
import ramp_timing as rt
from device_data import Device, Devices
from ramp_timing import Settings

ROOT = Path(__file__).resolve().parents[1]
BFLAT = "B:BFLAT"
FREQ_DEVICE = "tblFreqCurveFF"
PHASE_DEVICES = ("RFA", "RFB")
WINDOW = (2600.0, 35000.0)      # fit window [us]: after flat injection, before the top


def bflat_window(devs: Devices, group: str = BFLAT) -> tuple[float, float]:
    """(t_on, t_off) [us] of the nontrivial part of the group's table, in cycle time (+ its dtoff):
    from the last zero point before the first non-zero value to the first zero point after the last one."""
    table = next(d for d in devs.values() if d.get("group") == group and d.get("role") == "table")
    t, v = dd.ramp(table)
    nz = np.flatnonzero(np.asarray(v) != 0)
    if not len(nz):
        raise ValueError(f"{group}: table is all zero")
    i0, i1 = max(nz[0] - 1, 0), min(nz[-1] + 1, len(t) - 1)
    dtoff = float(table.get("dtoff", 0.0))
    return float(t[i0]) + dtoff, float(t[i1]) + dtoff


def flat_from_bflat(devs: Devices, group: str = BFLAT) -> dict[str, float]:
    """{"BDOT": centre, "TFLAT": half length} [us] of bflat_window: the GMPS minimum and the half
    length of flat injection."""
    t_on, t_off = bflat_window(devs, group)
    return {"BDOT": 0.5 * (t_on + t_off), "TFLAT": 0.5 * (t_off - t_on)}


def settings_with(misc: Devices, values: dict[str, float]) -> Settings:
    """rt.ramp_settings(misc) with settings devices replaced by values (e.g. {"BDOT": .., "TFLAT": ..})."""
    devs = {n: dict(d) for n, d in misc.items()}
    for name, val in values.items():
        devs[name]["value"] = float(val)
    return rt.ramp_settings(devs)


def _curve_pc(dev: Device, t: np.ndarray, dtoff: float, circumference: float) -> np.ndarray:
    t_dev, f_dev = dd.ramp(dev)                 # t includes the programmed start (t_start)
    return rt.ke_to_pc(rt.frf_to_ke(np.interp(t - dtoff, t_dev, f_dev), circumference))


def fit_rf_offset(settings: Settings, freq_dev: Device, window: tuple[float, float] = WINDOW,
                  scale: bool = True, step: float = 20.0) -> dict:
    """Fit the RF frequency curve's extra delay to the ramp.

    Minimises KE(curve at t - dtoff) - KE(scale * pc_ramp(t)) for t in window [us] (step us apart),
    where the curve's own "t" already has the programmed start. scale (pc) is fitted if scale=True,
    else 1. Returns {"dtoff" [us], "scale", "ke_max_rf" [GeV] = KE of scale * pc(KEMAX), "rms", "max"
    [GeV], "t" [us], "resid" [GeV], "window"}.
    """
    s, c = settings, settings["circumference"]
    t = np.arange(window[0], window[1] + 0.5 * step, step)
    pc_ramp = rt.ke_to_pc(rt.ke_at(s, t))

    def resid(p):
        k = p[1] if scale else 1.0
        return rt.pc_to_ke(_curve_pc(freq_dev, t, p[0], c)) - rt.pc_to_ke(k * pc_ramp)

    p0 = [float(freq_dev.get("dtoff", 0.0))] + ([1.0] if scale else [])
    fit = least_squares(resid, p0, x_scale=[10.0] + ([1e-3] if scale else []))
    k = float(fit.x[1]) if scale else 1.0
    r = resid(fit.x)
    return {"dtoff": float(fit.x[0]), "scale": k, "ke_max_rf": float(rt.pc_to_ke(k * rt.ke_to_pc(s["vimax"]))),
            "rms": float(np.sqrt(np.mean(r**2))), "max": float(np.max(np.abs(r))), "t": t, "resid": r,
            "window": tuple(window)}


def phase_jump_time(dev: Device, dtoff: float | None = None) -> float:
    """Cycle time [us] of the largest step in a phase table (the transition jump), with dtoff
    (default: the device's own)."""
    t, v = dd.ramp(dev)
    i = int(np.argmax(np.abs(np.diff(v))))
    dtoff = float(dev.get("dtoff", 0.0)) if dtoff is None else dtoff
    return float(0.5 * (t[i] + t[i + 1])) + dtoff


def transition_time(settings: Settings, gamma_t: float) -> float:
    """Cycle time [us] at which the ramp's gamma reaches gamma_t (rising half)."""
    s = settings
    ke = rt.gamma_to_ke(gamma_t)
    return float(rt.time_gmps(ke, rt.vimin_flat(s["viflat"], s["vimax"], s["t_flat"], s["freq_pulse"]),
                              s["vimax"], s["freq_pulse"], s["t_offset"]))


def plot_timing(bma: Devices, settings: Settings, freq_dev: Device, fit: dict,
                path: str | Path | None = None) -> np.ndarray:
    """B:BFLAT (with its delay) and the flat region; ramp vs the RF curve with the fitted offset; residual."""
    import matplotlib.pyplot as plt
    s, c = settings, settings["circumference"]
    bd, tfl = s["t_offset"], s["t_flat"]
    fig, ax = plt.subplots(3, 1, figsize=(9, 9.5))
    t = np.arange(bd - 500, bd + 600, 1.0)
    ax[0].plot(t, dd.value_at(bma, BFLAT, t), "k", lw=2, label="B:BFLAT (S * T(t - dtoff))")
    ax[0].axvspan(bd - tfl, bd + tfl, color="C3", alpha=0.12, label=f"flat: BDOT {bd:g} +- TFLAT {tfl:g} us")
    ax[0].axvline(bd, color="C3", ls="--")
    ax[0].set_ylabel("B:BFLAT [A]"); ax[0].set_xlabel("t [us]")
    t = np.arange(0, 40000, 20.0)
    ax[1].plot(t / 1e3, rt.ke_at(s, t), "C3", label=f"ramp (KEMAX {s['vimax']:g} GeV)")
    for dt, ls, lab in ((float(freq_dev.get("dtoff", 0.0)), ":", "file dtoff"), (fit["dtoff"], "-", "fitted")):
        ax[1].plot(t / 1e3, rt.pc_to_ke(_curve_pc(freq_dev, t, dt, c)), "k", ls=ls, lw=1,
                   label=f"RF curve, dtoff {dt:.1f} us ({lab})")
    ax[1].set_ylabel("KE [GeV]"); ax[1].set_xlabel("t [ms]")
    ax[2].plot(fit["t"] / 1e3, 1e3 * fit["resid"], "k")
    ax[2].set_title(f"RF curve - scale x ramp (pc): dtoff {fit['dtoff']:.1f} us, scale {fit['scale']:.5f} "
                    f"(top {fit['ke_max_rf']:.4f} GeV), rms {1e3 * fit['rms']:.1f} MeV", fontsize=9)
    ax[2].set_ylabel("KE residual [MeV]"); ax[2].set_xlabel("t [ms]")
    for a in ax:
        a.grid(alpha=0.3)
    ax[0].legend(fontsize=8); ax[1].legend(fontsize=8)
    fig.tight_layout()
    if path:
        fig.savefig(path, dpi=120); plt.close(fig)
    return ax


# ---------------------------------------------------------------- command line
def _set_and_note(path: Path, values: dict[str, dict[str, float]], note: str) -> None:
    devs, meta = dd.load(path)
    for name, kv in values.items():
        devs[name].update(kv)
    meta["notes"] = (meta.get("notes", "") + " " + note).strip()
    dd.save(devs, path, meta)


def main(argv: list[str] | None = None) -> None:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--bma", required=True, help="BMA file with B:BFLAT")
    ap.add_argument("--misc", required=True, help="MISC file of the BMA data's machine: gets BDOT, TFLAT, KEMAX")
    ap.add_argument("--same-bdot", nargs="*", default=[], help="other MISC files: get the same BDOT and KEMAX (own TFLAT)")
    ap.add_argument("--rf", required=True, help="RF frequency curve file (tblFreqCurveFF)")
    ap.add_argument("--phases", help="RF phase curve file (RFA, RFB): gets the same dtoff")
    ap.add_argument("--window", nargs=2, type=float, default=list(WINDOW), help="fit window [us]")
    ap.add_argument("--gamma-t", type=float, help="report transition (gamma = gamma_t) vs the phase jump")
    ap.add_argument("--plot", help="plot file")
    ap.add_argument("--apply", action="store_true", help="write the results into the files")
    a = ap.parse_args(argv)
    p = lambda f: ROOT / f
    bma, misc = dd.load(p(a.bma))[0], dd.load(p(a.misc))[0]
    freq = dd.load(p(a.rf))[0][FREQ_DEVICE]
    t_on, t_off = bflat_window(bma)
    flat = flat_from_bflat(bma)
    s = settings_with(misc, flat)
    fit = fit_rf_offset(s, freq, tuple(a.window))
    print(f"B:BFLAT {t_on:g}-{t_off:g} us -> BDOT {flat['BDOT']:g} us, TFLAT {flat['TFLAT']:g} us "
          f"(file: BDOT {misc['BDOT']['value']:g}, TFLAT {misc['TFLAT']['value']:g})")
    print(f"ramp: trough {s['vimin']:.5f} GeV, flat {s['viflat']:.5f} GeV, KEMAX {s['vimax']:g} GeV")
    print(f"RF curve: extra dtoff {fit['dtoff']:.1f} us (file {float(freq.get('dtoff', 0.0)):g}), scale {fit['scale']:.5f} "
          f"-> top {fit['ke_max_rf']:.4f} GeV; residual rms {1e3 * fit['rms']:.1f}, max {1e3 * fit['max']:.1f} MeV "
          f"({a.window[0]:g}-{a.window[1]:g} us)")
    phases = dd.load(p(a.phases))[0] if a.phases else None
    if phases is not None:
        t_jump = phase_jump_time(phases["RFA"], fit["dtoff"])
        msg = f"phase jump at {t_jump / 1e3:.3f} ms"
        if a.gamma_t:
            t_tr = transition_time(s, a.gamma_t)
            msg += f", ramp transition (gamma_t {a.gamma_t:g}) at {t_tr / 1e3:.3f} ms: jump {t_jump - t_tr:+.0f} us"
        print(msg)
    if a.plot:
        plot_timing(bma, s, freq, fit, p(a.plot))
    if a.apply:
        day = __import__("datetime").date.today().isoformat()
        note = (f"{day}: BDOT = {flat['BDOT']:g} us (centre of B:BFLAT {t_on:g}-{t_off:g} us, {a.bma}), "
                f"{{}} (timing_fit.py).")
        kemax = round(fit["ke_max_rf"], 3)
        ke_note = f"KEMAX = {kemax:g} GeV (top energy from the RF curve fit)"
        _set_and_note(p(a.misc), {"BDOT": {"value": flat["BDOT"]}, "TFLAT": {"value": flat["TFLAT"]},
                                  "KEMAX": {"value": kemax}},
                      note.format(f"TFLAT = {flat['TFLAT']:g} us (half its length), {ke_note}"))
        for f in a.same_bdot:
            _set_and_note(p(f), {"BDOT": {"value": flat["BDOT"]}, "KEMAX": {"value": kemax}},
                          note.format(f"same BDOT and KEMAX as {a.misc}, own TFLAT; {ke_note}"))
        rf_note = (f"{day}: dtoff = {fit['dtoff']:.1f} us, extra delay on top of the programmed start, fitted to "
                   f"the ramp ({a.misc}, {a.window[0]:g}-{a.window[1]:g} us; timing_fit.py).")
        _set_and_note(p(a.rf), {FREQ_DEVICE: {"dtoff": round(fit["dtoff"], 1)}}, rf_note)
        if a.phases:
            _set_and_note(p(a.phases), {n: {"dtoff": round(fit["dtoff"], 1)} for n in PHASE_DEVICES}, rf_note)
        print("written: " + ", ".join([a.misc, *a.same_bdot, a.rf] + ([a.phases] if a.phases else [])))


if __name__ == "__main__":
    sys.path.insert(0, str(Path(__file__).parent))
    main()
