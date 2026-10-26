"""Tunes, coupling, phase advances and beta functions from pinged turn-by-turn BMA BPM data.

The user's method (2026-10-24), per ping:
1. Pings: kicks at a regular spacing (find_pings: the largest turn-to-turn changes of the pinged plane, median
   over BPMs). Window = from the kick up to the next kick.
2. Each BPM: missing points interpolated linearly, then the second difference v[n] - (v[n-1] + v[n+1]) / 2,
   which removes the slowly varying closed orbit (prepare).
3. Plain FFT, |FFT|; peaks refined by a parabola through the peak bin and its two neighbours. Tunes are
   between 6.5 and 7: Q = 7 - q (TUNE_INT, ABOVE_HALF).
4. Coupling: the two largest peaks of the combined spectrum (average |FFT| of the H BPMs + of the V BPMs)
   are labelled by character: larger H/V amplitude ratio = mode A (horizontal-like), else mode B. With strong
   coupling the largest peak in the pinged plane is not always the pinged mode.
5. Phases: per BPM the Fourier coefficient C at the mode's q; mu = -arg C (because Q = 7 - q). Amplitude
   |C| / (1 - cos 2 pi q) (undoes the second difference's gain).
6. Turn alignment: shifting a BPM's data by m turns shifts its phase by m * 2 pi q. Offsets m (integers) are
   fitted per BPM over all pings of both planes against a reference phase (the model's), fit_turn_offsets;
   with the 10-20 data this finds L24 and S24 one turn off (the data's turn count likely starts between S23 and
   L24, not S24 / L1). The kick itself builds over ~2-3 turns, so the onset of the oscillation can't fix m.
7. Phase advances BPM i -> i+1 (ring order, sector 1..24; the last one wraps with + 2 pi Q); relative beta
   from the amplitude (BPM gains assumed equal) and from the phase (three-BPM method, needs model phases).

Data: bpm_data devices (bd.read(path): TBT text or saved .json; one device per BPM and plane, "values" per turn,
nan = no reading, "turn_offset"). planes(devs) = bd.planes -> the (48, n_turns) arrays in ring order. Model optics (optional) from a
MAD-X twiss table: model_optics(twiss) (MAD-X's phase starts at the sequence start, sector 11 for MEV400:
model_advances takes care of it).

Notebook:
    import ping_analysis as pa
    h, v = pa.planes(bd.read("studies/BPM_CO_opt/TBT_modelhpingtbtBPM.txt")), pa.planes(bd.read(".../vping..."))
    tunes = pa.tune_table({"h": h, "v": v}, timing)                     # one row per ping: Q_A, Q_B, coupling, ...
    model = lambda t_ms: pa.model_optics(rm.run_point(setup, 1000 * t_ms).twiss)   # optional
    optics, offsets = pa.optics_table({"h": h, "v": v}, tunes, model)            # per ping and BPM

Command line (writes CSVs and plots to --out):
    python helper_scripts/ping_analysis.py --h H.txt --v V.txt [--misc MISC.json] [--lattice L --devices F..] [--out DIR]
"""
from __future__ import annotations

import argparse
import sys
import warnings
from pathlib import Path
from typing import Callable

import numpy as np
import pandas as pd

import bpm_compare as bc
import bpm_data as bd
from numpy.typing import ArrayLike
from tfs_reader import TfsTable

TUNE_INT = 7                 # Q = TUNE_INT - q (tunes between 6.5 and 7)
FMIN = 0.02                  # lowest fractional tune searched
MATCH = 0.01                 # half-window peaks within 2 * MATCH of the full-window tune
MAX_MISSING = 0.1            # drop a BPM with more than this fraction of the window missing
SEARCH = 20                  # [turns] search range around the nominal kick
SNR_MIN = 3.0                # |FFT| peak / no-ping (or off-peak) level, amplitude ratio (~10 in power)
NAMES = [el.replace("_BPM", "") for el, _, _ in bc.bpm_elements()]     # BMAL1, BMAS1, ... ring order
MODE_PLANE = {"A": "x", "B": "y"}
PING_MODE = {"h": "A", "v": "B"}


# ---------------------------------------------------------------- data
planes = bd.planes          # (48, n_turns) arrays per plane, turn offsets applied (bpm_data)


def find_pings(X: np.ndarray, period: int | None = None, first: int | None = None) -> list[int]:
    """Column index of the first kicked turn of every ping in the pinged plane's array X (48, n_turns).
    Kicks are at a regular spacing: period and first are estimated from the strongest jumps unless given;
    each kick is then the largest turn-to-turn change (median over BPMs) within +-SEARCH of its nominal index."""
    with warnings.catch_warnings():                   # turns before the beam have no readings
        warnings.simplefilter("ignore", RuntimeWarning)
        jump = np.nanmedian(np.abs(np.diff(X, axis=1)), axis=0)
    if period is None or first is None:
        base = np.nanmedian(jump)
        strong = np.where(jump > 8 * base)[0]
        starts = [strong[0]] + [j for i, j in enumerate(strong[1:]) if j - strong[i] > 50]
        d = np.diff(starts)
        period = period or int(np.median(d[(d > 50) & (d < 2 * np.median(d))]))
        first = first if first is not None else int(np.median(np.array(starts) % period))
    kicks = []
    for k0 in range(first, X.shape[1] - 2, period):
        lo, hi = max(k0 - SEARCH, 0), min(k0 + SEARCH, len(jump))
        if not np.all(np.isnan(jump[lo:hi])):
            kicks.append(lo + int(np.nanargmax(jump[lo:hi])) + 1)
    return sorted(set(kicks))


def prepare(v: np.ndarray) -> np.ndarray | None:
    """Missing points interpolated, then the second difference; None if more than MAX_MISSING is missing."""
    ok = np.isfinite(v)
    if ok.sum() < max((1 - MAX_MISSING) * len(v), 10):
        return None
    n = np.arange(len(v))
    v = np.interp(n, n[ok], v[ok])
    return v[1:-1] - 0.5 * (v[:-2] + v[2:])


# ---------------------------------------------------------------- spectra and tunes
def spectra(X: np.ndarray) -> tuple[np.ndarray | None, np.ndarray | None]:
    """(freq, |FFT|) of every BPM's prepared signal; rows of dropped BPMs are nan; (None, None) if all dropped."""
    sig = [prepare(x) for x in X]
    n = next((len(s) for s in sig if s is not None), None)
    if n is None:
        return None, None
    S = np.array([np.abs(np.fft.rfft(s)) if s is not None else np.full(n // 2 + 1, np.nan) for s in sig])
    return np.fft.rfftfreq(n), S


def refine(f: np.ndarray, A: np.ndarray, i: int) -> float:
    """Peak position from a parabola through bins i-1, i, i+1 of A."""
    y0, y1, y2 = A[i - 1], A[i], A[i + 1]
    den = y0 - 2 * y1 + y2
    return f[i] + (0.5 * (y0 - y2) / den if den != 0 else 0.0) * (f[1] - f[0])


def peaks(f: np.ndarray, A: np.ndarray, n: int = 2, near: float | None = None) -> list[tuple[float, float]]:
    """The n largest local maxima of A in (FMIN, 0.5) (or within 2 MATCH of `near`): [(q, height)], largest first."""
    idx = [i for i in range(1, len(A) - 1) if FMIN < f[i] < 0.5 and A[i] >= A[i - 1] and A[i] >= A[i + 1]
           and (near is None or abs(f[i] - near) < 2 * MATCH)]
    out = []
    for i in sorted(idx, key=lambda i: A[i], reverse=True):
        if all(abs(i - j) > 3 for j, _ in out):
            out.append((i, A[i]))
        if len(out) == n:
            break
    return [(refine(f, A, i), h) for i, h in out]


def eigentunes(h: dict, k: int, e: int) -> dict | None:
    """Modes A, B of one window [k, e) of both planes h["x"], h["y"]: Q, amplitudes in H and V (average |FFT|),
    coupling (A: V/H, B: H/V), half-window spread dQ (second half - first half)."""
    sp = {p: spectra(h[p][:, k:e]) for p in "xy"}
    if sp["x"][0] is None or sp["y"][0] is None:
        return None
    f = sp["x"][0]
    avg = {p: np.nanmean(sp[p][1], axis=0) for p in "xy"}
    pk = peaks(f, avg["x"] + avg["y"], 2)
    if len(pk) < 2:
        return None
    at = lambda A, q: float(A[int(np.argmin(np.abs(f - q)))])
    modes = sorted(((q, at(avg["x"], q), at(avg["y"], q)) for q, _ in pk), key=lambda m: m[1] / m[2], reverse=True)
    out, mid = {}, (k + e) // 2
    floor = {p: np.nanmedian(avg[p]) for p in "xy"}
    for name, (q, ah, av) in zip("AB", modes):
        p = MODE_PLANE[name]
        half = []
        for a, b in ((k, mid), (mid, e)):
            fh, Sh = spectra(h[p][:, a:b])
            pq = peaks(fh, np.nanmean(Sh, axis=0), 1, near=q) if fh is not None else []
            half.append(pq[0][0] if pq else np.nan)
        out.update({f"Q_{name}": TUNE_INT - q, f"H_{name}": ah, f"V_{name}": av,
                    f"coupling_{name}": av / ah if name == "A" else ah / av,
                    f"dQ_{name}_halves": half[0] - half[1],
                    f"peak_to_floor_{name}": (ah if name == "A" else av) / floor[p]})
    return out


def tune_table(data: dict[str, dict], timing: dict | None = None, noping: dict | None = None) -> pd.DataFrame:
    """One row per ping of each ping file: data = {"h": planes(...), "v": planes(...)} (either may be missing).
    timing (bpm_data timing device) gives t_ms; noping (planes of the no-ping file) gives snr_A/B
    against the same windows, else snr is the peak / median of the spectrum."""
    rows = []
    for ping, h in data.items():
        X = h["x" if ping == "h" else "y"]
        kicks = find_pings(X)
        ends = kicks[1:] + [min(kicks[-1] + int(np.median(np.diff(kicks))), X.shape[1])]
        for k, e in zip(kicks, ends):
            m = eigentunes(h, k, e)
            if m is None:
                continue
            row = {"ping": ping, "kick_index": k, "end_index": e, "n_turns": e - k, "turn": k + h["begin_turn"], **m}
            row["t_ms"] = float(bd.turn_time(timing, row["turn"])) / 1000.0 if timing is not None else np.nan
            for name in "AB":
                p = MODE_PLANE[name]
                if noping is not None:
                    fn, Sn = spectra(noping[p][:, k:e])
                    level = peaks(fn, np.nanmean(Sn, axis=0), 1)[0][1] if fn is not None else np.nan
                    row[f"snr_{name}"] = m[f"{'H' if p == 'x' else 'V'}_{name}"] / level
                else:
                    row[f"snr_{name}"] = m[f"peak_to_floor_{name}"]
            row["pinged_mode"] = PING_MODE[ping]
            rows.append(row)
    return pd.DataFrame(rows)


# ---------------------------------------------------------------- phases, alignment, optics
def ping_phases(X: np.ndarray, k: int, e: int, q: float) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Per BPM (nan if dropped): mu [rad] = -arg C at q, its error [rad] (residual rms / (|C| sqrt(N/2))),
    amplitude [mm] = |C| / (1 - cos 2 pi q)."""
    mu, err, amp = (np.full(X.shape[0], np.nan) for _ in range(3))
    for i in range(X.shape[0]):
        s = prepare(X[i, k:e])
        if s is None:
            continue
        n = np.arange(len(s))
        C = 2.0 / len(s) * np.sum(s * np.exp(-2j * np.pi * q * n))
        mu[i] = -np.angle(C)
        err[i] = np.sqrt(np.mean((s - np.real(C * np.exp(2j * np.pi * q * n))) ** 2)) / (abs(C) * np.sqrt(len(s) / 2))
        amp[i] = abs(C) / (1 - np.cos(2 * np.pi * q))
    return mu, err, amp


def advances(mu: np.ndarray, Q: float) -> np.ndarray:
    """Phase advances [rad] BPM i -> i+1 in the given order; the last one wraps to the first (+ 2 pi Q)."""
    nxt = np.roll(mu, -1)
    nxt[-1] = nxt[-1] + 2 * np.pi * Q
    return np.mod(nxt - mu, 2 * np.pi)


def model_optics(twiss: TfsTable) -> dict:
    """Model phase [rad, continuous from BMAL1], beta and dispersion at the BPMs in ring order, both planes,
    and the tunes, from a MAD-X twiss table. MAD-X's phase starts at the sequence start, so the phase is
    rebuilt from the advances taken in MAD-X's own order."""
    out = {"Q1": twiss.header["Q1"], "Q2": twiss.header["Q2"]}
    for p, b, m, q in (("x", "BETX", "MUX", "Q1"), ("y", "BETY", "MUY", "Q2")):
        mu = 2 * np.pi * np.array([twiss.row(f"{n}_BPM")[m] for n in NAMES])
        adv = model_advances(mu, out[q])
        out[f"mu_{p}"] = np.r_[0.0, np.cumsum(adv)[:-1]]
        out[f"adv_{p}"] = adv
        out[f"beta_{p}"] = np.array([twiss.row(f"{n}_BPM")[b] for n in NAMES])
    out["dx"] = np.array([twiss.row(f"{n}_BPM")["DX"] for n in NAMES])
    return out


def model_advances(mu: np.ndarray, Q: float) -> np.ndarray:
    """Advances [rad] in ring order from MAD-X phases mu [rad] that start at the sequence start: taken in
    phase order, with the wrap where the sequence ends."""
    order = np.argsort(mu)
    out = np.empty_like(mu)
    out[order] = advances(mu[order], Q)
    return out


def wrap(a: ArrayLike) -> np.ndarray:
    """Angle(s) to (-pi, pi]."""
    return np.angle(np.exp(1j * np.asarray(a)))


def fit_turn_offsets(dev: np.ndarray, q: np.ndarray, m_range: int = 2, max_frac: float = 0.25) -> tuple[np.ndarray, np.ndarray]:
    """Integer turn offsets per BPM from phase deviations dev (n_ping, n_bpm) [rad] (measured - reference, with
    each ping's common offset removed) and each ping's q: m minimizing sum wrap(dev + 2 pi q m)^2. An offset is
    kept only if the remaining rms deviation is below max_frac * 2 pi q (mean q) and better than m = 0;
    otherwise 0. Returns (m, remaining rms [rad])."""
    nb = dev.shape[1]
    m_best, rms = np.zeros(nb, dtype=int), np.full(nb, np.nan)
    for i in range(nb):
        ok = np.isfinite(dev[:, i])
        if ok.sum() < 3:
            continue
        r = {m: np.sqrt(np.mean(wrap(dev[ok, i] + 2 * np.pi * q[ok] * m) ** 2)) for m in range(-m_range, m_range + 1)}
        m = min(r, key=r.get)
        if m != 0 and r[m] < max_frac * 2 * np.pi * np.mean(q[ok]) and r[m] < r[0]:
            m_best[i] = m
        rms[i] = r[m_best[i]]
    return m_best, rms


def beta_from_phase(phi: np.ndarray, phi_m: np.ndarray, beta_m: np.ndarray) -> np.ndarray:
    """Three-BPM beta at each BPM i from the advances to i+1 and i+2 (measured phi, model phi_m, model beta)."""
    cot = lambda a: 1 / np.tan(a)
    p13, m13 = phi + np.roll(phi, -1), phi_m + np.roll(phi_m, -1)
    return beta_m * (cot(phi) - cot(p13)) / (cot(phi_m) - cot(m13))


def optics_table(data: dict[str, dict], tunes: pd.DataFrame, model: Callable[[float], dict] | None = None,
                 snr_min: float = SNR_MIN, align: bool = True) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Per ping (snr of the pinged mode > snr_min) and BPM: phase, error, amplitude, advance to the next BPM;
    with model(t_ms) -> model_optics(...): model advance, betas from amplitude and phase, and (align) integer
    turn offsets fitted over all pings and planes (fit_turn_offsets), applied before the advances.
    Returns (optics, offsets): offsets has one row per BPM: the integer offset applied (turn_offset), the
    remaining rms deviation [deg], and phase_offset_turns = median phase deviation from the model in units of
    one turn's shift (2 pi q), before the correction, after taking out the tune mismatch and the common
    offset: ~integer for a turn offset, a fraction for e.g. a BPM that blends two turns."""
    recs = []
    for _, r in tunes[tunes.apply(lambda r: r[f"snr_{PING_MODE[r.ping]}"] > snr_min, axis=1)].iterrows():
        mode = PING_MODE[r.ping]
        p = MODE_PLANE[mode]
        q = TUNE_INT - r[f"Q_{mode}"]
        mu, err, amp = ping_phases(data[r.ping][p], int(r.kick_index), int(r.end_index), q)
        rec = {"ping": r.ping, "mode": mode, "plane": p, "t_ms": r.t_ms, "Q": r[f"Q_{mode}"], "q": q,
               "mu": mu, "err": err, "amp": amp}
        if model is not None:
            mo = model(r.t_ms)
            rec.update({"mu_m": mo[f"mu_{p}"], "adv_m": mo[f"adv_{p}"], "beta_m": mo[f"beta_{p}"],
                        "Q_m": mo["Q1" if p == "x" else "Q2"]})
        recs.append(rec)
    m_off = np.zeros(len(NAMES), dtype=int)
    rms_off, frac = np.full(len(NAMES), np.nan), np.full(len(NAMES), np.nan)
    if model is not None and recs:
        # phase deviation from the model; the tune mismatch adds (Q - Q_m) / Q_m * mu_m (known): taken out
        dev = np.array([wrap(rc["mu"] - rc["mu_m"] * rc["Q"] / rc["Q_m"]) for rc in recs])
        dev = wrap(dev - np.angle(np.nanmean(np.exp(1j * dev), axis=1))[:, None])
        q_all = np.array([rc["q"] for rc in recs])
        frac = np.nanmedian(dev / (2 * np.pi * q_all[:, None]), axis=0)       # in turns of shift
        if align:
            m_off, rms_off = fit_turn_offsets(dev, q_all)
    rows = []
    for rc in recs:
        mu = rc["mu"] + 2 * np.pi * rc["q"] * m_off
        adv = advances(mu, rc["Q"])
        base = {k: rc[k] for k in ("ping", "mode", "plane", "t_ms", "Q", "q")}
        cols = {"bpm": NAMES, "next_bpm": NAMES[1:] + NAMES[:1], "turn_offset": m_off, "mu": mu, "mu_err": rc["err"],
                "amp_mm": rc["amp"], "adv": adv}
        if "adv_m" in rc:
            ok = np.isfinite(rc["amp"])
            cols.update({"adv_model": rc["adv_m"], "dadv": wrap(adv - rc["adv_m"]), "beta_model": rc["beta_m"],
                         "beta_amp": rc["amp"] ** 2 * np.nanmean(rc["beta_m"][ok]) / np.nanmean(rc["amp"][ok] ** 2),
                         "beta_phase": beta_from_phase(adv, rc["adv_m"], rc["beta_m"]), "Q_model": rc["Q_m"]})
        rows.append(pd.DataFrame({**base, **cols}))
    optics = pd.concat(rows, ignore_index=True) if rows else pd.DataFrame()
    offsets = pd.DataFrame({"bpm": NAMES, "turn_offset": m_off, "rms_dev_deg": np.degrees(rms_off),
                            "phase_offset_turns": frac})
    return optics, offsets


# ---------------------------------------------------------------- plots
def plot_tunes(tunes: pd.DataFrame, model: pd.DataFrame | None = None, path: str | Path | None = None) -> np.ndarray:
    """Eigentunes and coupling vs time (filled: the pinged mode). model: DataFrame t_ms, Q1, Q2 (optional)."""
    import matplotlib.pyplot as plt
    fig, axes = plt.subplots(2, 1, figsize=(9.5, 7.5), sharex=True)
    for name, color in (("A", "tab:blue"), ("B", "tab:orange")):
        for ping, mk in (("h", "o"), ("v", "s")):
            d = tunes[(tunes.ping == ping) & (tunes[f"snr_{name}"] > SNR_MIN)]
            fill = color if PING_MODE[ping] == name else "none"
            axes[0].errorbar(d.t_ms, d[f"Q_{name}"], yerr=0.5 * np.abs(d[f"dQ_{name}_halves"]), fmt=mk, ms=4,
                             capsize=2, color=color, mfc=fill, label=f"mode {name}, {ping} pings")
            axes[1].plot(d.t_ms, d[f"coupling_{name}"], mk, ms=4, color=color, mfc=fill,
                         label=f"mode {name} ({'V/H' if name == 'A' else 'H/V'}), {ping} pings")
    if model is not None:
        axes[0].plot(model.t_ms, model.Q1, "-", lw=1, color="tab:purple", label="model Q1")
        axes[0].plot(model.t_ms, model.Q2, "-", lw=1, color="tab:brown", label="model Q2")
    axes[0].set_ylabel("Q"); axes[0].legend(fontsize=7, ncol=2)
    axes[0].set_title("Eigentunes from the pings (A: horizontal-like, B: vertical-like; filled: the pinged mode)", fontsize=9)
    axes[1].set_yscale("log"); axes[1].set_ylabel("coupling ratio"); axes[1].set_xlabel("t [ms]"); axes[1].legend(fontsize=7)
    fig.tight_layout()
    if path:
        fig.savefig(path, dpi=120); plt.close(fig)
    return axes


def plot_optics(optics: pd.DataFrame, t_ms: dict[str, float], path: str | Path | None = None) -> np.ndarray:
    """For one h and one v ping (t_ms = {"h": ..., "v": ...}, nearest ping): advances measured vs model, the
    difference (mean +- spread over all pings), and beta from amplitude and phase vs model."""
    import matplotlib.pyplot as plt
    fig, axes = plt.subplots(3, 2, figsize=(12, 9.5))
    xs = np.arange(len(NAMES))
    for j, ping in enumerate(("h", "v")):
        d = optics[optics.ping == ping]
        if d.empty or "adv_model" not in d:
            continue
        t = d.t_ms.unique()[np.argmin(np.abs(d.t_ms.unique() - t_ms.get(ping, 5.0)))]
        s = d[d.t_ms == t]
        ax = axes[0, j]
        ax.plot(xs, np.degrees(s.adv_model), "s-", ms=3, label="model")
        ax.plot(xs, np.degrees(s.adv), "o-", ms=3, label="measured")
        ax.set_ylabel("advance to next BPM [deg]"); ax.legend(fontsize=7)
        ax.set_title(f"{ping} ping {t:.1f} ms (mode {s['mode'].iloc[0]}); turn offsets applied: "
                     + (", ".join(f"{b} {m:+d}" for b, m in zip(s.bpm, s.turn_offset) if m) or "none"), fontsize=8)
        ax = axes[1, j]
        piv = d.pivot_table(index="t_ms", columns="bpm", values="dadv", sort=False)[NAMES]
        ax.errorbar(xs, np.degrees(piv.mean()), yerr=np.degrees(piv.std()), fmt="o", ms=3, capsize=2,
                    label=f"mean +- spread over {len(piv)} pings")
        ax.plot(xs, np.degrees(s.dadv), "x", ms=4, label="this ping")
        ax.axhline(0, color="gray", lw=0.5); ax.set_ylabel("measured - model advance [deg]"); ax.legend(fontsize=7)
        ax = axes[2, j]
        ax.plot(xs, s.beta_model, "s-", ms=3, label="model")
        ax.plot(xs, s.beta_amp, "o-", ms=3, label="from amplitude (scaled to the model mean)")
        ax.plot(xs, s.beta_phase.where((s.beta_phase / s.beta_model - 1).abs() < 1), "^", ms=4, label="from phase (three BPMs)")
        ax.set_ylabel("beta [m]"); ax.legend(fontsize=7)
        for ax in axes[:, j]:
            ax.set_xticks(xs[::4], NAMES[::4], fontsize=6, rotation=45)
    fig.tight_layout()
    if path:
        fig.savefig(path, dpi=120); plt.close(fig)
    return axes


# ---------------------------------------------------------------- command line
def main(argv: list[str] | None = None) -> None:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--h", help="horizontal ping file (TBT text, or bpm_data .json)")
    ap.add_argument("--v", help="vertical ping file")
    ap.add_argument("--n", help="no-ping file (for the SNR)")
    ap.add_argument("--misc", help="*_MISC_*.json: turn -> time (turn 0 at bd.TURN0); default: the .json files' timing device")
    ap.add_argument("--lattice", help="lattice for the model optics (with --devices), run with H = V = 0 and RF off")
    ap.add_argument("--devices", nargs="+", default=[])
    ap.add_argument("--out", default=".", help="output folder")
    ap.add_argument("--show", nargs=2, type=float, default=[5.0, 4.2], metavar=("T_H", "T_V"), help="pings plotted [ms]")
    a = ap.parse_args(argv)
    import device_data as dd, ramp_timing as rt
    raw = {k: bd.read(getattr(a, k)) for k in ("h", "v", "n") if getattr(a, k)}
    timing = bd.timing_device(rt.ramp_settings(dd.load(a.misc)[0]), note=a.misc) if a.misc else \
        next((d[bd.TIMING] for d in raw.values() if bd.TIMING in d), None)
    data = {k: planes(raw[k]) for k in ("h", "v") if k in raw}
    noping = planes(raw["n"]) if "n" in raw else None
    tunes = tune_table(data, timing, noping=noping)
    model, model_df = None, None
    if a.lattice:
        import run_madx as rm
        cfg = rm.Config(lattice=a.lattice, devices=a.devices, rf="off")
        pool = rm.load_devices(cfg.devices)
        cfg.overrides = {**rm.batch_override(pool, 0.0, kind="H"), **rm.batch_override(pool, 0.0, kind="V")}
        setup, cache = rm.prepare(cfg), {}

        def model(t_ms):
            key = round(t_ms, 1)
            if key not in cache:
                cache[key] = model_optics(rm.run_point(setup, 1000.0 * key).twiss)
            return cache[key]
    optics, offsets = optics_table(data, tunes, model)
    if model is not None:
        model_df = pd.DataFrame([{"t_ms": t, "Q1": m["Q1"], "Q2": m["Q2"]} for t, m in sorted(cache.items())])
    out = Path(a.out)
    out.mkdir(parents=True, exist_ok=True)
    tunes.to_csv(out / "ping_tunes.csv", index=False)
    optics.to_csv(out / "ping_optics.csv", index=False)
    offsets.to_csv(out / "ping_turn_offsets.csv", index=False)
    plot_tunes(tunes, model_df, out / "ping_tunes.png")
    if model is not None:
        plot_optics(optics, {"h": a.show[0], "v": a.show[1]}, out / "ping_optics.png")
    print(f"{len(tunes)} pings; turn offsets: " + (", ".join(f"{b} {m:+d}" for b, m in zip(offsets.bpm, offsets.turn_offset) if m) or "none"))
    print(f"written to {out}: ping_tunes.csv/.png, ping_optics.csv" + ("/.png" if model is not None else "") + ", ping_turn_offsets.csv")


if __name__ == "__main__":
    sys.path.insert(0, str(Path(__file__).parent))
    main()
