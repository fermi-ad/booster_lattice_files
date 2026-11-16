"""Write MAD-X input (.dat) files from the device data.

COMMON_Booster_input.dat (layout of data_Booster/Common_Booster_Input.dat, plus the RF
variables the lattices in madx_files/ use, RF01..RF22: VOLT=V<nn>, LAG=LAG<nn>, FREQ=FREQ_RF):
    KE_true=<beam kinetic energy [GeV] at t>;     ke_at(ke_source or settings, t)
    KE=<effective KE of the GMPS at t>;           ke_gmps_at(settings, t) (sets Brho, magnets)
    I_BEX2=...; I_BEX3=...; I_BEX4=...; MKS_ON=...;   extraction kickers (KICKERS)
    FREQ_RF=<equivalent RF frequency [MHz] of ramp_flat at t>;   ke_to_frf(ke_at(settings, t), B:CIRC)
    V01=...; ... V22=...;                         cavity voltages [MV]
    LAG01=...; ... LAG22=...;                     cavity lags [2 pi]
    I_<family><L|S><n>=<total current at t>;     (288 values)
    RETURN;
(Format changed 2026-10-12, user: was KE = beam, KE_GMPS = GMPS. 2026-10-23, user: kickers moved
before the RF block.)

Total current = device_data.value_at(devs, "B:<family><L|S><n>", t)
= S * T(t - dtoff) + O from the scale factor ...S (1 if absent), ramp table ...T and
offset ...O (0 if absent). A current whose group is not in devs at all is written as 0
and listed in the file header (missing_devices). The horizontal correctors (I_HS<n>,
I_HL<n>) also get value_at(devs, "B:BFLAT", t) = sf (B:BFLATS) * B:BFLATT(t - dtoff); if
B:BFLATT is missing, nothing is added and the file header says so.
Outside a ramp (t - dtoff before its first or after its last point) the table holds its
first/last value (device_data.sample), at any t including t < 0. For t < 0, KE_true = KE = viflat.
Option beam_field=True (studies/bma_kernel, 2026-11-06): every current, B:BFLAT included, is the field the beam
sees (field_at), the exponential kernel over its history, instead of the current at t. The tables do not change.
Extraction kickers (KICKERS, lattices of 2026-10-23; I_BEX3/I_BEX4 2026-11-02): I_BEX<n> =
value_at(devs, "B:BEX<n>", t) [A] (B:BEX<n>S sf * B:BEX<n>T ramp in the 11_02_26 MISC files; the older ones
have only B:BEX2T) and MKS_ON = value_at(devs, "MKSON", t) (a single setting, written as is; the lattices
use THETA_MKS = -5.273e-6 * KV_MKS * MKS_ON * Brho_EXT / Brho). All from the *_MISC_*.json files; 0 if not
in devs, listed in the header like the currents.

RF modes (rf_mode, from which devices are in devs; rf="off" forces the first):
    "off"         no RF phase devices: V<nn> = LAG<nn> = 0 (transverse only). RF voltage
                  devices without phase devices raise ValueError.
    "phase"       phases, no voltages: LAG<nn> from the phase devices, and one voltage for all
                  cavities that are on, so that the energy gain per turn matches the ramp:
                  V = dKE/turn [MV] / sum over the on cavities of sin(2 pi LAG_n), i.e. with RF21/RF22
                  off: dKE/turn = 10 V sin(phase_A) + 10 V sin(phase_B); dKE/turn = dke_at(ke_source
                  or settings, t).
    "phase+volt"  phases and voltages: LAG<nn> from the phase devices, V<nn> = KV_TO_MV *
                  value_at(devs, "B:RF<nn>GA", t) (readbacks in kV); the ramp is not used.
    "sum"         phases and the measured RF sum (RFSUM_DEVICE, data_Booster/rfsum_smoothed.json; 2026-11-13,
                  user): every cavity that is on gets V = RFSUM(t) [MV] / (number on, 20), and the synchronous
                  phase is solved from the ramp: dKE/turn = V sum_on sin(phi_s + d_n), d_n = +-(RFA - RFB) / 2
                  the paraphase of the phase devices (A cavities +, B cavities -), phi_s below transition and
                  180 deg - phi_s above it (above = the phase devices' mean (RFA + RFB) / 2 > 90 deg, i.e. the
                  transition jump of the old phase program). LAG<nn> = (phi_s + d_n) / 360 deg. Replaces the
                  phase program's own mean phase (quantitatively off, e.g. 5-10 ms). Raises ValueError where
                  dKE/turn > what the RF sum can give.
Phase devices: the RF_PHASE_MAP device ("RF_CAVITY_PHASE", in rf_phase_curves.json) gives each
cavity's phase group (RFA or RFB) and the cavities that are off ("off": RF21, RF22, no readback
data; V = 0 in both modes). Without it there are no phases (mode "off"). Phases in degrees,
LAG = phase / 360, so every cavity has the RFA or the RFB lag (user, 2026-10-13). MAD-X adds no
arrival-time phase: the reference particle gains V sin(2 pi LAG) at each cavity wherever it is
(tested), so the RF phase seen by the beam is the LAG itself.

KE_true follows ramp_flat (beam energy, flat injection via B:BFLATT) unless ke_source,
a frequency device such as the RF curve, is given. FREQ_RF is always the equivalent RF
frequency of ramp_flat (h = 84, B:CIRC), whatever KE_true comes from. KE is the plain GMPS sine
and always comes from the settings dict (ramp_settings, B:VIMIN/B:VIMAX currents scaled to KE with
B:VIMAX at KEMAX), as does the circumference.

An existing file is only overwritten if a madx_input writer made it (or overwrite=True).
Note /mnt/c is case-insensitive: COMMON_Booster_input.dat and the template
data_Booster/Common_Booster_Input.dat would be the same file in that folder.

Example:
    import device_data as dd, ramp_timing as rt
    from madx_input import common_values, write_common
    bma, _ = dd.load("data_Booster/11_03_26_BMA.json")
    misc, _ = dd.load("data_Booster/11_03_26_MISC_MEV400.json")     # GMPS settings, B:BEX2, MKSON
    bma = {**bma, **misc}
    settings = rt.ramp_settings(misc)
    vals = common_values(2400.0, bma, settings)               # no RF devices: mode "off"
    vals["KE_true"], vals["I_QL1"]                            # any written value
    write_common("Common_Booster_input.dat", vals)            # write it
    common_values(2400.0, bma, settings, ke_source=rf_dev)    # KE_true from the RF curve
    common_values(2400.0, {**bma, **phases}, settings)        # mode "phase"
"""

from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

import numpy as np

from device_data import Device, Devices, groups, ramp, value_at
from ramp_timing import Settings, dke_at, ke_at, ke_gmps_at, ke_to_frf, ke_to_pc

# (section name, [(family, order of L/S within each n), ...]) in file order
COMMON_LAYOUT = [
    ("_diplBMA", [("H", "SL"), ("V", "SL")]),
    ("_quadBMA", [("Q", "LS"), ("SQ", "LS")]),
    ("_sextBMA", [("SX", "SL"), ("SS", "LS")]),
]
N_SECTORS = 24
N_RF = 22                        # RF cavities RF01..RF22 in the lattices
RF_DEVICE = "B:RF{:02d}GA"       # voltage device (group) of cavity n, as in 10_23_26_VRF_READBACK_clean.json
RF_PHASE_MAP = "RF_CAVITY_PHASE"  # device {"cavities": {"1": "RFB", ...}, "off": [21, 22]}: phase group per cavity
KV_TO_MV = 1e-3                  # readbacks are in kV, MAD-X VOLT is in MV
DEG_TO_LAG = 1.0 / 360.0         # phase devices in degrees, MAD-X LAG in units of 2 pi
RF_MODES = ("off", "phase", "phase+volt", "sum")
RFSUM_DEVICE = "RFSUM_SMOOTH"    # measured total RF gap voltage [MV] (rfsum_smoothed.json), split over the on cavities
# extraction kickers {MAD-X name: device group} (user 2026-10-23). B:BEX2..4: table x scale [A] (BEX2..BEX4,
# own currents since 2026-11-02); MKSON: single setting, the lattices' MKS_ON factor
KICKERS = {"I_BEX2": "B:BEX2", "I_BEX3": "B:BEX3", "I_BEX4": "B:BEX4", "MKS_ON": "MKSON"}
# field_at (studies/bma_kernel): eddy-current time constant of a dipole, fitted to a test-stand square pulse,
# and the kernel length; a family of multipole order m has FIELD_A / m (all of it assumed to scale, user 2026-11-06)
FIELD_A = 300.0                  # us
FIELD_L = 3000.0                 # us
FIELD_ORDER = {"H": 1, "V": 1, "BFLAT": 1, "Q": 2, "SQ": 2, "SX": 3, "SS": 3}


def _num(x):
    """Shortest string that reads back as the same float (0.0 for zero)."""
    x = float(x)
    return repr(x + 0.0)        # + 0.0 turns -0.0 into 0.0


def current_names() -> list[str]:
    """The 288 current groups ("B:HS1", "B:HL1", ...) in file order (I_ names without "B:")."""
    return [f"B:{fam}{ls}{n}" for _, families in COMMON_LAYOUT for fam, order in families
            for n in range(1, N_SECTORS + 1) for ls in order]


def field_at(devs: Devices, name: str, t: float, a: float = FIELD_A, L: float = FIELD_L) -> float:
    """Value of a BMA device as seen by the beam at time t [us]: value_at with a kernel on its history.

    B(t) = integral over x from t - L to t of I(x) exp((x - t) / a_m) / the same of exp((x - t) / a_m),
    with I(x) = value_at(devs, name, x), so the pure delay is the table's dtoff. name: group
    ("B:QL1", "B:BFLAT"). a [us]: dipole time constant, divided here by the order m of the device's
    family (FIELD_ORDER). L [us]: kernel length. Exact for a piecewise-linear table. A group
    without a table (an override) is constant: its value_at.
    """
    t = float(t)
    parts = groups(devs)[name]
    if "table" not in parts:
        return float(value_at(devs, name, t))
    table = devs[parts["table"]]
    a_m = a / FIELD_ORDER[table["kind"]]
    breaks = np.asarray(ramp(table)[0], dtype=float) + table.get("dtoff", 0.0)
    x = np.concatenate([[t - L], breaks[(breaks > t - L) & (breaks < t)], [t]])
    v = value_at(devs, name, x)
    slope = np.diff(v) / np.diff(x)
    weight = np.exp((x - t) / a_m)
    integral = np.sum((v[1:] - slope * a_m) * weight[1:] - (v[:-1] - slope * a_m) * weight[:-1])
    return float(integral / (1 - np.exp(-L / a_m)))


def _present(devs: Devices, grp: dict, name: str) -> bool:
    return name in grp or name in devs


def missing_devices(devs: Devices) -> list[str]:
    """Current groups (current_names, then KICKERS) not in devs; they are written as 0."""
    grp = groups(devs)
    return [n for n in current_names() + list(KICKERS.values()) if not _present(devs, grp, n)]


def phase_names(devs: Devices) -> list[str] | None:
    """Phase group (e.g. "RFA", "RFB") of cavities 1..N_RF from the RF_PHASE_MAP device's
    "cavities", or None if devs has no RF_PHASE_MAP. Raises ValueError if a cavity is missing."""
    if RF_PHASE_MAP not in devs:
        return None
    cav = {int(k): v for k, v in devs[RF_PHASE_MAP]["cavities"].items()}
    missing = [n for n in range(1, N_RF + 1) if n not in cav]
    if missing:
        raise ValueError(f"{RF_PHASE_MAP} has no phase group for cavities {missing}")
    return [cav[n] for n in range(1, N_RF + 1)]


def off_cavities(devs: Devices) -> set[int]:
    """Cavity numbers with V = 0: the RF_PHASE_MAP device's "off" list (e.g. [21, 22]), else none."""
    dev = devs.get(RF_PHASE_MAP)
    return set() if dev is None or dev.get("off") is None else {int(n) for n in dev["off"]}


def rf_mode(devs: Devices) -> str:
    """RF mode from the devices present: "off", "phase", "phase+volt" or "sum" (module docstring).

    Raises ValueError for voltages without phases, a phase group named in the map that is not in
    devs, only some cavities' voltages, or both the RF sum and the cavity voltages.
    """
    grp = groups(devs)
    names = phase_names(devs)
    has_p = [False] * N_RF if names is None else [_present(devs, grp, n) for n in names]
    has_v = [_present(devs, grp, RF_DEVICE.format(n)) for n in range(1, N_RF + 1)]
    for what, has in (("phase", has_p), ("voltage", has_v)):
        if any(has) and not all(has):
            raise ValueError(f"RF {what} devices missing for cavities "
                             f"{[n for n, h in enumerate(has, 1) if not h]}")
    if names is not None and not all(has_p):
        raise ValueError(f"{RF_PHASE_MAP} is in the devices but its phase groups {sorted(set(names))} are not")
    has_sum = _present(devs, grp, RFSUM_DEVICE)
    if not all(has_p):
        if all(has_v) or has_sum:
            raise ValueError("RF voltage devices given but no RF phase devices "
                             "(remove the voltages for transverse only, or add the phases)")
        return "off"
    if has_sum and all(has_v):
        raise ValueError(f"both {RFSUM_DEVICE} and the cavity voltages B:RF<nn>GA are in the devices: give one")
    return "sum" if has_sum else "phase+volt" if all(has_v) else "phase"


def rf_values(t: float, devs: Devices, settings: Settings, mode: str | None = None,
              ke_source: Device | None = None, dke: float | None = None) -> tuple[list[float], list[float]]:
    """(volts [MV], lags [2 pi]) of cavities RF01..RF22 at time t [us], in RF mode `mode`
    (default rf_mode(devs); see the module docstring).

    LAG<nn> = phase_n(t) [deg] / 360, the cavity's set phase (RFA or RFB).
    Cavities in off_cavities(devs) get V = 0.
    "phase": V = dke [MV] / sum over the on cavities of sin(2 pi LAG_n) on each on cavity
    (with RF21/RF22 off: dKE/turn = 10 V sin(phase_A) + 10 V sin(phase_B)). dke [GeV/turn] =
    dke_at(ke_source or settings, t) unless given (pass it to skip recomputing the turns);
    0 for t < 0 or dke = 0. Raises ValueError if dke != 0 and the sines sum to ~0.
    """
    mode = rf_mode(devs) if mode is None else mode
    if mode not in RF_MODES:
        raise ValueError(f"unknown RF mode {mode!r}, expected one of {RF_MODES}")
    if mode == "off":
        return [0.0] * N_RF, [0.0] * N_RF
    names = phase_names(devs)
    sampled = value_at(devs, names, t)
    lags = [DEG_TO_LAG * float(sampled[n]) for n in names]
    on = [n not in off_cavities(devs) for n in range(1, N_RF + 1)]
    if mode == "phase+volt":
        names = [RF_DEVICE.format(n) for n in range(1, N_RF + 1)]
        sampled = value_at(devs, names, t)
        return [KV_TO_MV * float(sampled[n]) if o else 0.0 for n, o in zip(names, on)], lags
    if dke is None:
        dke = 0.0 if t < 0 else float(dke_at(ke_source if ke_source is not None else settings, t,
                                             settings["circumference"]))
    if mode == "sum":
        return _rf_sum(t, devs, names, sampled, on, dke)
    gain = float(np.sum(np.sin(2 * np.pi * np.asarray(lags))[on]))
    if dke == 0.0:
        return [0.0] * N_RF, lags
    if abs(gain) < 1e-9:
        raise ValueError(f"t = {t} us: sum of sin(2 pi LAG) over the on cavities is ~0, cannot give dKE = {dke} GeV/turn")
    v = 1e3 * dke / gain                        # GeV -> MV for a proton
    return [v if o else 0.0 for o in on], lags


def _rf_sum(t: float, devs: Devices, names: list[str], sampled: dict, on: list[bool],
            dke: float) -> tuple[list[float], list[float]]:
    """RF mode "sum" (module docstring): (volts [MV], lags [2 pi]). names: each cavity's phase group, sampled:
    the groups' phases [deg] at t, on: cavity on, dke [GeV/turn]."""
    grps = sorted(set(names))
    if len(grps) != 2:
        raise ValueError(f"RF mode sum needs two phase groups (A and B), found {grps}")
    pa, pb = (float(sampled[g]) for g in grps)                     # RFA, RFB [deg]
    half = 0.5 * np.radians(pa - pb)                               # half the paraphase
    above = 0.5 * (pa + pb) > 90.0                                 # past the old program's transition jump
    d = np.array([half if n == grps[0] else -half for n in names])
    n_on = int(np.sum(on))
    v = float(value_at(devs, RFSUM_DEVICE, t)) / n_on
    c, s = float(np.sum(np.cos(d)[on])), float(np.sum(np.sin(d)[on]))
    amp, psi = np.hypot(c, s), np.arctan2(s, c)                    # sum_on sin(phi + d_n) = amp sin(phi + psi)
    ratio = 1e3 * dke / (v * amp) if v * amp > 0 else (0.0 if dke == 0 else np.inf)
    if abs(ratio) > 1.0:
        raise ValueError(f"t = {t} us: dKE/turn {1e3 * dke:.4f} MeV is more than the RF sum can give "
                         f"({v * amp:.4f} MV)")
    phi = np.pi - np.arcsin(ratio) - psi if above else np.arcsin(ratio) - psi
    lags = [float((phi + dn) / (2 * np.pi)) for dn in d]
    return [v if o else 0.0 for o in on], lags


def synchronous_phase(vals: "InputValues") -> float:
    """Mean synchronous phase [deg] of an InputValues: the mean LAG of the cavities that are on, x 360."""
    on = [n for n in range(1, N_RF + 1) if n not in vals.off]
    return float(np.degrees(2 * np.pi * np.mean([vals.scalars[f"LAG{n:02d}"] for n in on]))) if on else 0.0


@dataclass
class InputValues:
    """Everything written to COMMON_Booster_input.dat for one time t (from common_values).

    scalars: KE_true, KE, FREQ_RF, V01..V22, LAG01..LAG22, I_BEX2..4, MKS_ON; currents: I_<fam><L|S><n> (288, file
    order). vals["I_QL1"] / vals["V01"] looks in both. The other fields are for the file header.
    """
    t: float                       # us
    scalars: dict[str, float]
    currents: dict[str, float]
    rf_mode: str
    ke_from_device: bool           # KE_true from a frequency device (ke_source), not ramp_flat
    bflat: float | None            # value_at(devs, "B:BFLAT", t) added to I_HS, I_HL; None without group B:BFLAT
    bflat_sf: float
    missing: list[str]             # current groups (incl. B:BEX2..4, MKSON) not in the devices, written as 0
    off: list[int]                 # cavities with V = 0
    settings: dict | None = None   # the ramp_settings() used (GMPS calibration in the header)
    beam_field: bool = False       # currents and bflat are field_at values

    def __getitem__(self, name: str) -> float:
        return self.scalars[name] if name in self.scalars else self.currents[name]


def common_values(t: float, devs: Devices, settings: Settings, ke_source: Device | None = None,
                  rf: str = "auto", dke: float | None = None, beam_field: bool = False) -> InputValues:
    """All values of the common MAD-X input for time t [us] (InputValues), without writing.

    devs: one device dict with everything used: ramp tables and offsets (e.g. 11_03_26_BMA.json),
    and any RF phase / voltage devices (they set the RF mode). settings: ramp_settings() dict
    (KE, circumference, and KE_true by default). ke_source: optional frequency device (e.g. the RF
    curve) for KE_true instead of ramp_flat. rf: "auto" (rf_mode(devs)) or one of RF_MODES;
    V/LAG from rf_values (dke: see there). Currents whose group is missing from devs are 0.
    beam_field: write field_at(devs, name, t) for the currents and B:BFLAT instead of value_at.
    Write it with write_common(path, values).
    """
    c = settings["circumference"]
    if t < 0:
        ke_true = ke_gmps = ke_flat = float(settings["viflat"])
    else:
        ke_flat = float(ke_at(settings, t, c))
        ke_true = ke_flat if ke_source is None else float(ke_at(ke_source, t, c))
        ke_gmps = float(ke_gmps_at(settings, t))
    mode = rf_mode(devs) if rf == "auto" else rf
    volts, lags = rf_values(t, devs, settings, mode, ke_source, dke)
    scalars = {
        "KE_true": ke_true,
        "KE": ke_gmps,
        "FREQ_RF": float(ke_to_frf(ke_flat, c)),      # always from ramp_flat, also with ke_source
        **{f"V{n:02d}": v for n, v in enumerate(volts, 1)},
        **{f"LAG{n:02d}": lag for n, lag in enumerate(lags, 1)},
    }
    bflat = bflat_value(devs, t, beam_field)
    missing = missing_devices(devs)
    for var, name in KICKERS.items():
        scalars[var] = 0.0 if name in missing else float(value_at(devs, name, t))
    names = current_names()
    present = [n for n in names if n not in set(missing)]
    values = {n: field_at(devs, n, t) for n in present} if beam_field else value_at(devs, present, t)
    currents = {}
    for name in names:
        total = 0.0 if name in missing else float(values[name])
        if name.startswith("B:H") and bflat is not None:
            total += bflat
        currents[f"I_{name[2:]}"] = total
    return InputValues(t=float(t), scalars=scalars, currents=currents, rf_mode=mode,
                       ke_from_device=ke_source is not None, bflat=bflat, bflat_sf=bflat_sf(devs),
                       missing=missing, off=sorted(off_cavities(devs)) if mode != "off" else [],
                       settings=dict(settings), beam_field=beam_field)


def bflat_sf(devs: Devices) -> float:
    """Scale factor of group B:BFLAT: its "scale" device's "sf" (B:BFLATS; 1.0 if none)."""
    scale = groups(devs).get("B:BFLAT", {}).get("scale")
    return 1.0 if scale is None else float(devs[scale].get("sf", 1.0))


def bflat_value(devs: Devices, t: float, beam_field: bool = False) -> float | None:
    """value_at(devs, "B:BFLAT", t) = sf (B:BFLATS) * B:BFLATT(t - dtoff) at t [us]
    (added to every I_HS, I_HL), or None if devs has no group B:BFLAT (a B:BFLAT override is a
    group without a table: its value). beam_field: field_at instead of value_at."""
    if "B:BFLAT" not in groups(devs):
        return None
    return field_at(devs, "B:BFLAT", t) if beam_field else float(value_at(devs, "B:BFLAT", t))


def _gmps_header(settings: Settings | None) -> list[str]:
    """Header line for the GMPS calibration (ramp_settings from currents), else none."""
    if not settings or "amps_per_pc" not in settings:
        return []
    s = settings
    return [f"!GMPS: B:VIMAX = {_num(s['amps_per_pc'] * ke_to_pc(s['ke_max']))} A at KEMAX = {_num(s['ke_max'])} GeV, "
            f"{_num(s['amps_per_pc'])} A per GeV of pc; B:VIMIN -> trough {_num(s['vimin'])} GeV, flat {_num(s['viflat'])} GeV"]


_STAMP = "by madx_input."      # + writer name; any madx_input writer's file may be replaced


def write_common(path: str | Path, vals: InputValues, note: str | None = None,
                 overwrite: bool = False) -> Path:
    """Write the values from common_values to the common MAD-X input file. Returns the path.

    note: optional extra comment line. An existing file not written by a madx_input writer
    raises FileExistsError unless overwrite=True.
    """
    t, scalars, currents, mode = vals.t, vals.scalars, vals.currents, vals.rf_mode
    path = Path(path)
    if path.exists() and not overwrite and _STAMP not in path.read_text()[:500]:
        raise FileExistsError(f"{path} exists and was not written by madx_input; "
                              "pass overwrite=True to replace it")
    src = "frequency device" if vals.ke_from_device else "ramp_flat settings"
    lines = [
        "!Beam energy and magnet current inputs",
        f"!Generated {datetime.now().isoformat(timespec='seconds')} {_STAMP}write_common",
        f"!t = {_num(t)} us" + (" (t < 0: KE_true = KE = viflat)" if t < 0 else ""),
        f"!KE_true (beam) from {src}; KE (GMPS) from the plain GMPS sine",
        *_gmps_header(vals.settings),
        "!I_ = S * T(t - dtoff) + O (scale factor, ramp table, offset); I_HS, I_HL also + sf (B:BFLATS) * B:BFLATT(t - dtoff)",
    ]
    if vals.beam_field:
        lines.append(f"!Beam field: I_ and B:BFLAT are field_at values, kernel a = {_num(FIELD_A)} / m us, L = {_num(FIELD_L)} us")
    if vals.bflat is None:
        lines.append("!B:BFLATT not in the device data: nothing added to I_HS, I_HL")
    else:
        lines.append(f"!B:BFLAT: sf (B:BFLATS) = {_num(vals.bflat_sf)}, sf * B:BFLATT = {_num(vals.bflat)}")
    if vals.missing:
        lines.append(f"!{len(vals.missing)} current devices not in the device data, written as 0: "
                     + ", ".join(m.removeprefix("B:") for m in vals.missing))
    lines.append({"off": "!RF mode off: V01..V22 = LAG01..LAG22 = 0",
                  "phase": "!RF mode phase: LAG<nn> from phase devices; V = dKE/turn [MV] / sum(sin(2 pi LAG)) over the on cavities",
                  "phase+volt": f"!RF mode phase+volt: LAG<nn> from phase devices; V<nn> = {_num(KV_TO_MV)} * B:RF<nn>GA(t - dtoff)",
                  "sum": f"!RF mode sum: V = {RFSUM_DEVICE}(t - dtoff) / (cavities on); LAG<nn> = (phi_s +- paraphase/2) / 360, phi_s from dKE/turn",
                  }[mode])
    if mode != "off":
        lines.append(f"!LAG<nn> = {_num(DEG_TO_LAG)} * phase(t - dtoff) (RFA or RFB); V = 0 on cavities "
                     + (", ".join(f"RF{n:02d}" for n in vals.off) or "none"))
    if note:
        lines.append(f"!{note}")
    lines += [
        "!Beam kinetic energy [GeV]", f"KE_true={_num(scalars['KE_true'])};",
        "!Effective kinetic energy of the GMPS [GeV]", f"KE={_num(scalars['KE'])};",
        "",
        "!Extraction kickers: " + ", ".join(f"{v} = {d}" for v, d in KICKERS.items()),
        *(f"{var}={_num(scalars[var])};" for var in KICKERS),
        "",
        "!RF frequency [MHz] of all cavities: equivalent RF frequency of ramp_flat (h = 84, B:CIRC)",
        f"FREQ_RF={_num(scalars['FREQ_RF'])};",
        "",
        "!RF cavity voltages [MV], RF01..RF22",
        *(f"V{n:02d}={_num(scalars[f'V{n:02d}'])};" for n in range(1, N_RF + 1)),
        "",
        "!RF cavity lags [2 pi], RF01..RF22",
        *(f"LAG{n:02d}={_num(scalars[f'LAG{n:02d}'])};" for n in range(1, N_RF + 1)),
    ]
    names = iter(currents)
    for section, families in COMMON_LAYOUT:
        lines += ["", f"!current values for {section}"]
        for _ in range(len(families) * N_SECTORS * 2):
            name = next(names)
            lines.append(f"{name}={_num(currents[name])};")
    lines += ["", "RETURN;", ""]
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines))
    return path
