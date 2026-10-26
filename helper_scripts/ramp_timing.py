"""Beam energy along the Booster ramp. Kinetic energy (KE) is the primary quantity.

Energy-like conversions for a proton, all to and from kinetic energy:
    beta    relativistic beta = v/c
    gamma   relativistic gamma
    pc      momentum x c                 [GeV]
    brho    beam rigidity p/q            [T m]
    frev    revolution frequency         [MHz]  = beta c / circumference
    frf     RF frequency                 [MHz]  = HARMONIC * frev

Ramps: KE [GeV] vs time t [us]. The GMPS (main magnet power supply) is a biased
sine in field, so it is a sine in pc; KE in and out is converted to/from pc
inside each function. freq_pulse in Hz, t_offset [us] is the time of the minimum.
    ramp_gmps(t, vimin, vimax, freq_pulse=15.0, t_offset=0.0)
        plain GMPS sine: vimin at t_offset, vimax half a period later, periodic.
    time_gmps(ke, vimin, vimax, freq_pulse=15.0, t_offset=0.0)
        inverse of ramp_gmps on the rising half: the time at which KE = ke.
    vimin_flat(viflat, vimax, t_flat, freq_pulse=15.0)
        the vimin for which ramp_gmps reaches viflat at t_offset + t_flat.
    ramp_flat(t, viflat, vimax, t_flat, freq_pulse=15.0, t_offset=0.0)
        flat injection: viflat on (t_offset - t_flat, t_offset + t_flat), the GMPS
        sine (vimin from vimin_flat) out to the peaks at t_offset +- half a period,
        and vimax beyond them (one pulse, not periodic).
    ramp_settings(devs)
        the arguments above from a settings file (RAMP_DEVICES: B:VIMIN, B:VIMAX [A],
        TFLAT, GMPSFRQ, BDOT, CIRC, KEMAX); the currents give KE with B:VIMAX at KEMAX
        and the field proportional to the current; B:VIMIN is the trough, the flat level
        is the sine at BDOT + TFLAT. BDOT/TFLAT and the RF offsets: timing_fit.py.

Turns: t_n [us], the time of turn n = 0, 1, 2, ... from injection (t_start) to
the top of the ramp, found by integrating the revolution frequency (turn n is
where the integral reaches n), then n_after (default 100) more turns held at the
top (flattop) frequency.
    turns_gmps(vimin, vimax, freq_pulse, t_offset, circumference, t_start=None, n_after=100)
        injection default t_offset; top = vimax
    turns_flat(viflat, vimax, t_flat, freq_pulse, t_offset, circumference, t_start=None, n_after=100)
        injection default t_offset - t_flat; top = vimax
    turns_device(dev, t_start=None, n_after=100, harmonic=HARMONIC)
        from a frequency device (e.g. the RF curve, values = frf [MHz]), shifted by its
        dtoff [us] (device_curve: t + dtoff, as dd.value_at), linearly interpolated;
        injection default = first point; top = highest frequency.

Either source (a settings dict from ramp_settings, or a frequency device):
    ke_at(source, t)                   KE [GeV] at time t [us]
    ke_gmps_at(settings, t)            effective KE of the GMPS alone (settings only)
    t_n, ke_n, dke_n = turn_energy(source, t_start=None, n_after=100)
        dke_n[n] = ke_n[n + 1] - ke_n[n], the KE [GeV] gained on turn n.
    dke_at(source, t)                  dke_n interpolated at time t [us] (0 outside the turns)

Units: energies (KE, pc, MASS) in GeV, frequencies in MHz (pairs with times in us), rigidity
in T m, circumference in m. All functions take scalars, lists or numpy arrays.
Unphysical inputs (e.g. beta >= 1, or a frequency above the speed-of-light
limit) give nan with a numpy RuntimeWarning.

Example:
    from ramp_timing import ke_to_frf, frf_to_ke, convert
    ke_to_frf(0.4)                            # RF frequency [MHz] at KE = 0.4 GeV
    frf_to_ke([37.94, 52.81])                 # KE [GeV] from RF frequencies
    convert(0.4, "ke", "brho")                # any pair via KE
    convert(1.0, "frev", "gamma", circumference=474.0)
    ramp_gmps([0.0, 16667.0, 33333.0], vimin=0.4, vimax=8.0)    # 0.4, mid, 8.0 GeV
    time_gmps([0.4, 8.0], vimin=0.4, vimax=8.0)                  # 0, 33333 us
    s = ramp_settings(devs)
    ramp_flat(t, s["viflat"], s["vimax"], s["t_flat"], s["freq_pulse"], s["t_offset"])
    t_n = turns_flat(s["viflat"], s["vimax"], s["t_flat"], s["freq_pulse"], s["t_offset"], s["circumference"])
"""

import numpy as np
from numpy.typing import ArrayLike

from device_data import Device, Devices, ramp

Num = float | np.ndarray        # scalar in -> numpy scalar out; array in -> array out
Settings = dict[str, float]     # from ramp_settings()

MASS = 0.93827208816      # proton rest mass [GeV] (CODATA 2018, same as MAD-X)
C_LIGHT = 299792458.0      # speed of light [m/s]
HARMONIC = 84              # Booster RF harmonic number
CIRCUMFERENCE = 474.2      # Booster circumference [m]
# ramp_settings argument -> setting device (data_Booster/*_MISC_*.json); B:VIMIN, B:VIMAX in A
RAMP_DEVICES = {"viflat": "B:VIMIN", "vimax": "B:VIMAX", "t_flat": "TFLAT", "freq_pulse": "GMPSFRQ",
                "t_offset": "BDOT", "circumference": "CIRC", "ke_max": "KEMAX"}


def _a(x):
    return np.asarray(x, dtype=float)


# gamma
def ke_to_gamma(ke: ArrayLike) -> Num:
    """Relativistic gamma from kinetic energy [GeV]."""
    return 1.0 + _a(ke) / MASS


def gamma_to_ke(gamma: ArrayLike) -> Num:
    """Kinetic energy [GeV] from relativistic gamma."""
    return (_a(gamma) - 1.0) * MASS


# pc [GeV]
def ke_to_pc(ke: ArrayLike) -> Num:
    """Momentum x c [GeV] from kinetic energy [GeV]."""
    ke = _a(ke)
    return np.sqrt(ke * (ke + 2.0 * MASS))


def pc_to_ke(pc: ArrayLike) -> Num:
    """Kinetic energy [GeV] from momentum x c [GeV]."""
    pc = _a(pc)
    return np.sqrt(pc**2 + MASS**2) - MASS


# beta
def ke_to_beta(ke: ArrayLike) -> Num:
    """Relativistic beta = v/c from kinetic energy [GeV]."""
    return ke_to_pc(ke) / (_a(ke) + MASS)


def beta_to_ke(beta: ArrayLike) -> Num:
    """Kinetic energy [GeV] from relativistic beta (nan for beta >= 1)."""
    return gamma_to_ke(1.0 / np.sqrt(1.0 - _a(beta) ** 2))


# brho [T m]: p/q = pc[eV] / c for a unit charge
def ke_to_brho(ke: ArrayLike) -> Num:
    """Beam rigidity p/q [T m] from kinetic energy [GeV]."""
    return ke_to_pc(ke) * 1e9 / C_LIGHT


def brho_to_ke(brho: ArrayLike) -> Num:
    """Kinetic energy [GeV] from beam rigidity p/q [T m]."""
    return pc_to_ke(_a(brho) * C_LIGHT / 1e9)


# frev [MHz]
def ke_to_frev(ke: ArrayLike, circumference: float = CIRCUMFERENCE) -> Num:
    """Revolution frequency [MHz] = beta c / circumference [m], from kinetic energy [GeV]."""
    return ke_to_beta(ke) * C_LIGHT / circumference / 1e6


def frev_to_ke(frev: ArrayLike, circumference: float = CIRCUMFERENCE) -> Num:
    """Kinetic energy [GeV] from revolution frequency [MHz] on a circumference [m]."""
    return beta_to_ke(_a(frev) * 1e6 * circumference / C_LIGHT)


# frf [MHz]
def ke_to_frf(ke: ArrayLike, circumference: float = CIRCUMFERENCE) -> Num:
    """RF frequency [MHz] = HARMONIC * revolution frequency, from kinetic energy [GeV]."""
    return HARMONIC * ke_to_frev(ke, circumference)


def frf_to_ke(frf: ArrayLike, circumference: float = CIRCUMFERENCE) -> Num:
    """Kinetic energy [GeV] from RF frequency [MHz] (= HARMONIC * frev) on a circumference [m]."""
    return frev_to_ke(_a(frf) / HARMONIC, circumference)


_TO_KE = {"ke": _a, "beta": beta_to_ke, "gamma": gamma_to_ke, "pc": pc_to_ke,
          "brho": brho_to_ke, "frev": frev_to_ke, "frf": frf_to_ke}
_FROM_KE = {"ke": _a, "beta": ke_to_beta, "gamma": ke_to_gamma, "pc": ke_to_pc,
            "brho": ke_to_brho, "frev": ke_to_frev, "frf": ke_to_frf}
QUANTITIES = tuple(_TO_KE)


def convert(value: ArrayLike, frm: str, to: str, circumference: float = CIRCUMFERENCE) -> Num:
    """Convert between any two of QUANTITIES, going through kinetic energy."""
    for q in (frm, to):
        if q not in _TO_KE:
            raise ValueError(f"unknown quantity {q!r}; use one of {QUANTITIES}")
    kw = {"circumference": circumference}
    ke = _TO_KE[frm](value, **kw) if frm in ("frev", "frf") else _TO_KE[frm](value)
    return _FROM_KE[to](ke, **kw) if to in ("frev", "frf") else _FROM_KE[to](ke)



# Plain biased sine between lo and hi, in whatever units lo/hi are in (pc here).
def _sine(t, lo, hi, freq_pulse, t_offset):
    phase = 2.0 * np.pi * freq_pulse * (_a(t) - t_offset) * 1e-6
    return 0.5 * (hi + lo) - 0.5 * (hi - lo) * np.cos(phase)


def _sine_time(v, lo, hi, freq_pulse, t_offset):
    v = _a(v)
    x = ((hi - v) - (v - lo)) / (hi - lo)       # = cos(phase); exactly +-1 at the ends
    # allow roundoff at the ends (v == lo or hi), nan beyond that
    x = np.where(np.abs(x) <= 1.0 + 1e-12, np.clip(x, -1.0, 1.0), np.nan)
    return t_offset + np.arccos(x) / (2.0 * np.pi * freq_pulse) * 1e6


def _sine_lo_flat(v_flat, hi, t_flat, freq_pulse):
    if not 0.0 <= t_flat < 0.5e6 / freq_pulse:
        raise ValueError(f"t_flat must be in [0, {0.5e6 / freq_pulse:g}) us for {freq_pulse:g} Hz")
    c = np.cos(2.0 * np.pi * freq_pulse * t_flat * 1e-6)
    return (2.0 * v_flat - hi * (1.0 - c)) / (1.0 + c)


def ramp_gmps(t: ArrayLike, vimin: float, vimax: float, freq_pulse: float = 15.0, t_offset: float = 0.0) -> Num:
    """KE [GeV] at time t [us] on the plain GMPS sine.

    The sine is in pc, from pc(vimin) at t = t_offset [us] to pc(vimax) half a
    period (1e6 / (2 freq_pulse) us) later, repeating every 1e6 / freq_pulse us.
    vimin, vimax are KE [GeV]; freq_pulse in Hz.
    """
    pc = _sine(t, ke_to_pc(vimin), ke_to_pc(vimax), freq_pulse, t_offset)
    return pc_to_ke(pc)


def time_gmps(ke: ArrayLike, vimin: float, vimax: float, freq_pulse: float = 15.0, t_offset: float = 0.0) -> Num:
    """Inverse of ramp_gmps on the rising half: the time [us] at which KE = ke [GeV].

    Returns t in [t_offset, t_offset + 1e6 / (2 freq_pulse)]: vimin -> t_offset,
    vimax -> half a period later. ke outside [vimin, vimax] gives nan.
    """
    return _sine_time(ke_to_pc(ke), ke_to_pc(vimin), ke_to_pc(vimax), freq_pulse, t_offset)


def vimin_flat(viflat: float, vimax: float, t_flat: float, freq_pulse: float = 15.0) -> float:
    """vimin [GeV] for which ramp_gmps(t_offset + t_flat, vimin, vimax, ...) == viflat.

    For flat injection: t_flat [us] is half the length of the flat region and
    viflat the KE held there. The sine is solved in pc. 0 <= t_flat < half a period.
    """
    return pc_to_ke(_sine_lo_flat(ke_to_pc(viflat), ke_to_pc(vimax), t_flat, freq_pulse))


def ramp_flat(t: ArrayLike, viflat: float, vimax: float, t_flat: float, freq_pulse: float = 15.0,
              t_offset: float = 0.0) -> Num:
    """KE [GeV] at time t [us] on a GMPS ramp with flat injection. One pulse:

    viflat          on (t_offset - t_flat, t_offset + t_flat)
    GMPS sine       out to the peaks at t_offset +- 1e6 / (2 freq_pulse) us, with
                    vimin = vimin_flat(...) so it meets viflat at both ends
    vimax           beyond the peaks (not periodic)
    """
    dt = np.abs(_a(t) - t_offset)
    ke = ramp_gmps(t, vimin_flat(viflat, vimax, t_flat, freq_pulse), vimax, freq_pulse, t_offset)
    ke = np.where(dt <= t_flat, viflat, ke)
    return np.where(dt >= 0.5e6 / freq_pulse, vimax, ke)[()]


def ramp_settings(devs: Devices) -> Settings:
    """Ramp arguments from setting devices, e.g. data_Booster/10_23_26_MISC_MEV400.json.

    Returns {"viflat", "vimax" [GeV], "t_flat": TFLAT, "freq_pulse": GMPSFRQ, "t_offset": BDOT,
    "circumference": CIRC} (RAMP_DEVICES; without CIRC the circumference is CIRCUMFERENCE).

    B:VIMIN and B:VIMAX are GMPS currents [A] (unit "A"): B:VIMAX is the current at the
    peak, the extraction energy KEMAX [GeV], B:VIMIN the current at the trough (at BDOT). The
    field, so pc, is taken proportional to the current (extraction anchor, user 2026-10-23), so
        vimax = KEMAX,  vimin = pc_to_ke(ke_to_pc(KEMAX) * B:VIMIN / B:VIMAX)  (trough)
        viflat = ramp_gmps(BDOT + TFLAT, vimin, ...)   (flat injection level, user 2026-10-25)
    and the result also has "vimin", "amps_per_pc" = B:VIMAX / ke_to_pc(KEMAX) [A per GeV] and
    "ke_max" = KEMAX. With unit "GeV" (the old *_ramp.json files) both are KE, B:VIMIN is the
    flat level and KEMAX is not needed.
    """
    get = lambda arg: float(devs[RAMP_DEVICES[arg]]["value"])
    out = {arg: get(arg) for arg in ("t_flat", "freq_pulse", "t_offset")}
    vimin, vimax = devs[RAMP_DEVICES["viflat"]], devs[RAMP_DEVICES["vimax"]]
    units = (vimin.get("unit", "GeV"), vimax.get("unit", "GeV"))
    if units == ("GeV", "GeV"):
        out.update(viflat=get("viflat"), vimax=get("vimax"))
    elif units == ("A", "A"):
        ke_max = get("ke_max")
        amps_per_pc = get("vimax") / ke_to_pc(ke_max)
        trough = float(pc_to_ke(get("viflat") / amps_per_pc))
        viflat = float(ramp_gmps(out["t_flat"], trough, ke_max, out["freq_pulse"]))
        out.update(viflat=viflat, vimax=ke_max, vimin=trough, amps_per_pc=float(amps_per_pc), ke_max=ke_max)
    else:
        raise ValueError(f"{RAMP_DEVICES['viflat']}, {RAMP_DEVICES['vimax']}: units {units}, expected both A or both GeV")
    name = RAMP_DEVICES["circumference"]
    out["circumference"] = float(devs[name]["value"]) if name in devs else CIRCUMFERENCE
    return out


def _turn_times(frev, t_start, t_top, n_after, dt):
    """Turn times [us]: integrate frev(t) [MHz = turns/us] on a grid of step dt [us]
    from t_start to t_top, interpolate where the count reaches 0, 1, 2, ..., then
    add n_after turns at the constant frequency frev(t_top)."""
    if t_top > t_start:
        t = np.append(np.arange(t_start, t_top, dt), t_top)
        f = frev(t)
        count = np.concatenate([[0.0], np.cumsum(0.5 * (f[1:] + f[:-1]) * np.diff(t))])
        n_ramp = int(np.floor(count[-1] + 1e-9))
        t_ramp = np.interp(np.arange(n_ramp + 1), count, t)
    else:                                   # starting at (or past) the top
        t_top, count, n_ramp, t_ramp = t_start, [0.0], 0, np.array([float(t_start)])
    f_top = float(frev(np.array([t_top]))[0])
    n_flat = np.arange(n_ramp + 1, n_ramp + 1 + int(n_after))
    return np.concatenate([t_ramp, t_top + (n_flat - count[-1]) / f_top])


def turns_gmps(vimin: float, vimax: float, freq_pulse: float = 15.0, t_offset: float = 0.0,
               circumference: float = CIRCUMFERENCE, t_start: float | None = None, n_after: int = 100,
               dt: float = 0.25) -> np.ndarray:
    """Turn times [us] on ramp_gmps from t_start (default t_offset) to KE = vimax
    (t_offset + half a period), then n_after turns held at vimax."""
    t_start = t_offset if t_start is None else t_start
    t_top = t_offset + 0.5e6 / freq_pulse
    frev = lambda t: ke_to_frev(ramp_gmps(t, vimin, vimax, freq_pulse, t_offset), circumference)
    return _turn_times(frev, t_start, t_top, n_after, dt)


def turns_flat(viflat: float, vimax: float, t_flat: float, freq_pulse: float = 15.0, t_offset: float = 0.0,
               circumference: float = CIRCUMFERENCE, t_start: float | None = None, n_after: int = 100,
               dt: float = 0.25) -> np.ndarray:
    """Turn times [us] on ramp_flat from t_start (default t_offset - t_flat, the start
    of flat injection) to KE = vimax (t_offset + half a period), then n_after turns
    held at vimax."""
    t_start = t_offset - t_flat if t_start is None else t_start
    t_top = t_offset + 0.5e6 / freq_pulse
    frev = lambda t: ke_to_frev(ramp_flat(t, viflat, vimax, t_flat, freq_pulse, t_offset), circumference)
    return _turn_times(frev, t_start, t_top, n_after, dt)


def device_curve(dev: Device) -> tuple[np.ndarray, np.ndarray]:
    """(t [us], values) of a frequency device in cycle time: its trimmed table (dd.ramp) with
    t + dtoff ("dtoff" [us], 0 if absent), the same shift as dd.value_at = T(t - dtoff)."""
    t_dev, f_dev = ramp(dev)
    return t_dev + float(dev.get("dtoff", 0.0)), f_dev


def turns_device(dev: Device, t_start: float | None = None, n_after: int = 100, harmonic: float = HARMONIC,
                 dt: float = 0.25) -> np.ndarray:
    """Turn times [us] from a frequency device ("t" [us], "values" [MHz], "dtoff" [us]), e.g. the
    RF curve; its table is shifted by dtoff (device_curve).

    values / harmonic is the revolution frequency (harmonic=1 for a frev device),
    linearly interpolated in time. Runs from t_start (default: the first point) to
    the time of the highest frequency, then n_after turns held at that frequency.
    """
    t_dev, f_dev = device_curve(dev)
    t_start = t_dev[0] if t_start is None else t_start
    t_top = t_dev[np.argmax(f_dev)]
    frev = lambda t: np.interp(t, t_dev, f_dev) / harmonic
    return _turn_times(frev, t_start, t_top, n_after, dt)


def ke_at(source: Settings | Device, t: ArrayLike, circumference: float | None = None,
          harmonic: float = HARMONIC) -> Num:
    """KE [GeV] at time t [us] from either source:

      - a settings dict from ramp_settings(): ramp_flat() with those settings
        (the circumference is not needed);
      - a frequency device ("t", "values" [MHz], e.g. the RF curve; shifted by its "dtoff",
        device_curve): values / harmonic as frev, linearly interpolated and held at its
        highest value after the peak,
        converted with circumference (default CIRCUMFERENCE).
    """
    if "viflat" in source:
        return ramp_flat(t, source["viflat"], source["vimax"], source["t_flat"],
                         source["freq_pulse"], source["t_offset"])
    c = CIRCUMFERENCE if circumference is None else circumference
    t_dev, f_dev = device_curve(source)
    t_top = t_dev[np.argmax(f_dev)]
    return frev_to_ke(np.interp(np.minimum(_a(t), t_top), t_dev, f_dev) / harmonic, c)


def ke_gmps_at(settings: Settings, t: ArrayLike) -> Num:
    """Effective KE [GeV] of the GMPS alone at time t [us], from a ramp_settings() dict.

    The plain GMPS sine (ramp_gmps) whose minimum is vimin_flat(...), so it equals
    the beam KE (ramp_flat) outside the flat region and dips below it inside; the
    difference is made up by B:BFLAT.
    """
    s = settings
    vimin = vimin_flat(s["viflat"], s["vimax"], s["t_flat"], s["freq_pulse"])
    return ramp_gmps(t, vimin, s["vimax"], s["freq_pulse"], s["t_offset"])


def turn_energy(source: Settings | Device, t_start: float | None = None, n_after: int = 100,
                circumference: float | None = None, harmonic: float = HARMONIC,
                dt: float = 0.25) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Turn times, KE per turn and energy gain per turn: (t_n, ke_n, dke_n).

    source is either a settings dict from ramp_settings() (turns_flat(), with the
    settings' circumference) or a frequency device (turns_device()); KE per turn
    comes from ke_at(). t_n [us] and ke_n [GeV] have one entry per turn;
    dke_n[n] = ke_n[n + 1] - ke_n[n] is the KE gained on turn n (one entry shorter).
    """
    if "viflat" in source:
        c = source["circumference"] if circumference is None else circumference
        t_n = turns_flat(source["viflat"], source["vimax"], source["t_flat"], source["freq_pulse"],
                         source["t_offset"], c, t_start=t_start, n_after=n_after, dt=dt)
    else:
        t_n = turns_device(source, t_start=t_start, n_after=n_after, harmonic=harmonic, dt=dt)
    ke_n = ke_at(source, t_n, circumference, harmonic)
    return t_n, ke_n, np.diff(ke_n)


def dke_at(source: Settings | Device, t: ArrayLike, circumference: float | None = None,
           table: tuple[np.ndarray, np.ndarray] | None = None) -> Num:
    """Energy gain per turn [GeV/turn] at time t [us]: dke_n of turn_energy(source)
    linearly interpolated in t, 0 before injection (t_n[0]) and after the last turn.

    table: optional (t_n, dke_n) from an earlier turn_energy call, to skip recomputing it
    (e.g. in a scan); t_n may be the full turn array (one entry longer than dke_n).
    """
    if table is None:
        t_n, _, dke_n = turn_energy(source, circumference=circumference)
    else:
        t_n, dke_n = table
    t_n = np.asarray(t_n, dtype=float)[:len(dke_n)]
    return np.interp(t, t_n, dke_n, left=0.0, right=0.0)
