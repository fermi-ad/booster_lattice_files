"""Compare the model closed orbit at the BMA BPMs with measured (turn-by-turn) BPM data.

Model: the closed orbit X, Y at the 48 BMA BPMs (MAD-X elements BMAL<n>_BPM, BMAS<n>_BPM, n = 1..24)
from a run's twiss table (e.g. run_madx.run_point(setup, t).twiss), in mm.

Measured: BPM devices from bpm_data (one per BPM and plane, "values" per turn, "begin_turn", "turn_offset",
and a turn-timing device that gives the cycle time of each turn; see bpm_data). Signs (user 2026-10-24):
BPM x = model x, BPM y = -model y; the devices keep the BPM sign, measured_orbit applies BPM_SIGN. The slow
orbit at time t is a local polynomial fit (default a straight line) to the points within t +- window / 2:
the fit follows the slow ramp, the betatron oscillation and the noise average out. slow_orbit() returns the
fitted value at t, its standard error, the slope and the rms of the residuals (noise + oscillation).

Names: MAD-X element <-> device name per plane via bpm_data.acnet_name: B:HST<nn><L|S> (x), B:VST<nn><L|S>
(y), as in the text files, except BMAL3_BPM = B:HSTU3L / B:VSTU3L (the L3 BMA-package BPM, user 2026-11-02;
was B:HST03L before). The old-style BPMs (B:H/VST03L, U6L, and U8L read as U7L) are only in the modified
lattice (bpm_data.OLD_BPMS; or pass names= to measured_orbit).

Example:
    import run_madx as rm, bpm_data as bd, bpm_compare as bc
    devs = bd.read_tbt(path); bd.add_timing(devs, setup.settings)   # or devs, _ = bd.load(json_path)
    res = rm.run_point(setup, 8000.0)
    model = bc.model_orbit(res.twiss)                       # name, s, x, y [mm], betx, bety, dx
    meas = bc.measured_orbit(devs, 8000.0, window=200.0)    # name, x, x_err, x_noise, y, ...
    cmp = bc.compare(model, meas)                           # + dx_mm, dy_mm (model - measured)
    bc.plot_compare(cmp, title="MEV400, t = 8 ms")
"""

from __future__ import annotations

import numpy as np
import pandas as pd

import bpm_data as bd
from bpm_data import UNIT_TO_MM, acnet_name, bpm_elements
from device_data import Device, Devices
from tfs_reader import TfsTable

BPM_SIGN = {"x": 1.0, "y": -1.0}                   # model = BPM_SIGN * BPM reading


def model_orbit(twiss: TfsTable, bpm_offsets: dict[str, tuple[float, float]] | None = None) -> pd.DataFrame:
    """Model closed orbit at the BMA BPMs from a TfsTable twiss (X, Y in m in the table), as the BPMs read it.

    bpm_offsets {element: (DX, DY) [m]}: the BPMs' own misalignments; default the twiss's bpm_offsets attribute
    (run_madx sets it from a misaligned lattice's record, lattice_mod), else none. The reading is X - DX, Y - DY
    (MAD-X's X, Y at a misaligned MONITOR are in the ideal frame).
    Columns: name, s [m], x, y [mm] (the reading), x_beam, y_beam [mm] (twiss X, Y), betx, bety [m], dx [m],
    mux, muy [2 pi]; in ring order (by s).
    """
    if bpm_offsets is None:
        bpm_offsets = getattr(twiss, "bpm_offsets", None) or {}
    rows = []
    for el, ls, n in bpm_elements():
        r = twiss.row(el)
        ox, oy = bpm_offsets.get(el, (0.0, 0.0))
        rows.append({"name": el, "s": r["S"], "x": 1e3 * (r["X"] - ox), "y": 1e3 * (r["Y"] - oy),
                     "x_beam": 1e3 * r["X"], "y_beam": 1e3 * r["Y"],
                     "betx": r["BETX"], "bety": r["BETY"], "dx": r["DX"], "mux": r["MUX"], "muy": r["MUY"]})
    return pd.DataFrame(rows).sort_values("s", ignore_index=True)


def slow_orbit(dev: Device, t: float, window: float = 200.0, order: int = 1,
               t_dev: np.ndarray | None = None) -> dict[str, float]:
    """Slow orbit of one BPM plane at time t [us] from turn-by-turn data (module docstring).

    Fits a polynomial of degree `order` to the points with cycle time t_dev (one per reading, e.g.
    bd.times(dev, devs); default the device's own "t" + "dtoff") within t +- window / 2 [us]. Returns {"value" [mm] at t, "err" [mm] its standard error, "slope"
    [mm/ms], "noise" [mm] rms of the residuals, "n" points used}. All nan if fewer than
    order + 3 points.
    """
    if t_dev is None:
        t_dev = np.asarray(dev["t"], dtype=float) + float(dev.get("dtoff", 0.0))
    v = np.asarray(dev["values"], dtype=float) * UNIT_TO_MM[dev.get("unit", "mm")]
    sel = (np.abs(t_dev - t) <= 0.5 * window) & np.isfinite(v) & np.isfinite(t_dev)
    n = int(sel.sum())
    if n < order + 3:
        return {"value": np.nan, "err": np.nan, "slope": np.nan, "noise": np.nan, "n": n}
    x = (t_dev[sel] - t) / 1000.0                      # ms from t
    coef, cov = np.polyfit(x, v[sel], order, cov=True)
    resid = v[sel] - np.polyval(coef, x)
    return {"value": float(coef[-1]), "err": float(np.sqrt(cov[-1, -1])),
            "slope": float(coef[-2]) if order >= 1 else 0.0,
            "noise": float(np.sqrt(np.mean(resid**2))), "n": n}


def measured_orbit(devs: Devices, t: float, window: float = 200.0, order: int = 1,
                   names: dict[tuple[str, str], str] | None = None) -> pd.DataFrame:
    """Slow orbit at time t [us] at every BMA BPM from measured devices (slow_orbit per plane; reading times
    from each device's timing device in devs, bd.times, or its own "t").

    names: optional {(element, plane): device name} overriding acnet_name(). A BPM plane not in
    devs gives nan. Values in the model's sign (BPM_SIGN). Columns: name, x, x_err, x_noise, x_n,
    y, y_err, y_noise, y_n [mm].
    """
    rows = []
    for el, _, _ in bpm_elements():
        row = {"name": el}
        for p in ("x", "y"):
            dev_name = (names or {}).get((el, p), acnet_name(el, p))
            dev = devs.get(dev_name)
            if dev is None:
                fit = {"value": np.nan, "err": np.nan, "noise": np.nan, "n": 0}
            else:
                fit = slow_orbit(dev, t, window, order, bd.times(dev, devs) if dev.get("timing") in devs else None)
            row.update({p: BPM_SIGN[p] * fit["value"], f"{p}_err": fit["err"], f"{p}_noise": fit["noise"], f"{p}_n": fit["n"]})
        rows.append(row)
    return pd.DataFrame(rows)


def compare(model: pd.DataFrame, measured: pd.DataFrame) -> pd.DataFrame:
    """model_orbit and measured_orbit side by side: x_model, x_meas, x_err, dx_mm = model - measured
    (same for y), plus s, betx, bety, dx; ring order. df.attrs["rms"] = rms of dx_mm, dy_mm over the
    BPMs with data."""
    m = model.rename(columns={"x": "x_model", "y": "y_model"})
    d = measured.rename(columns={"x": "x_meas", "y": "y_meas"})
    out = m.merge(d, on="name", how="left")
    out["dx_mm"] = out.x_model - out.x_meas
    out["dy_mm"] = out.y_model - out.y_meas
    out.attrs["rms"] = {p: float(np.sqrt(np.nanmean(out[f"d{p}_mm"] ** 2))) for p in ("x", "y")}
    return out


def plot_compare(cmp: pd.DataFrame, title: str | None = None, axes: np.ndarray | None = None) -> np.ndarray:
    """Model and measured orbit along the ring, two panels (error bars on the measurement): x (+ outward) and -y
    (+ up; the model sign has y + down, BPM_SIGN). Returns the axes."""
    import matplotlib.pyplot as plt
    if axes is None:
        _, axes = plt.subplots(2, 1, figsize=(9, 6), sharex=True)
    for ax, p, sign, label in zip(axes, ("x", "y"), (1, -1), ("x [mm] (+ out)", "-y [mm] (+ up)")):
        ax.plot(cmp.s, sign * cmp[f"{p}_model"], "o-", ms=3, label="model")
        ax.errorbar(cmp.s, sign * cmp[f"{p}_meas"], yerr=cmp[f"{p}_err"], fmt="s", ms=3, capsize=2, label="measured")
        ax.axhline(0, color="gray", lw=0.6)
        ax.set_ylabel(label)
        rms = cmp.attrs.get("rms", {}).get(p)
        ax.legend(title=None if rms is None else f"rms(model - meas) = {rms:.2f} mm", fontsize=8)
    axes[-1].set_xlabel("s [m]")
    if title:
        axes[0].set_title(title)
    return axes
