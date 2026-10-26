"""PIP-II injection: ORBUMP and painting magnet kick angles, and the injection .dat file.

Cleaned-up version of gen_input_PIP_II_Booster.py (same physics, 2026-10-12).

The PIP-II lattice calls PIP_II_Booster_injection.dat (INJECTION_FILE), which sets
    ORB_KICK_ANG                 ORBUMP kick [rad] (ORB_KICK_POS / ORB_KICK_NEG legs)
    VPML10_ANG ... VPML12_ANG    vertical painting kicks [rad]
    HPMS09_ANG ... HPMS12_ANG    horizontal painting kicks [rad]

Offsets at the injection point TREATYPOINT [m] (OFFSETS): Y_ORBUMP (ORBUMP), Y_PM and X_PM
(painting). They are devices like any other (role "value" scalar, or a ramp in time):
offsets_at(devs, t) gives value_at(devs, name, t), 0 for any not in devs.

kick_angles(sectormap, y_orbump, y_pm, x_pm) needs the sector maps of the same optics, so a
run is two-pass (run_madx): pass 1 with all angles 0, then the angles from pass 1's
<lattice>_sectormap.outx, then pass 2.
    ORB_KICK_ANG = arcsin(Y_ORBUMP / L), L = distance between the OBMP3 and OBMP4 kicks.
    Up pair (S09, S10 / L10, L11U): bump of X_PM (Y_PM) with zero angle at TREATYPOINT.
    Down pair (S11, S12 / L11D, L12): closes it, zero position and angle after S12 (L12).
The maps are the uncoupled 2x2 blocks (R11..R22, R33..R44) of the per-element sector maps,
so x-y coupling (e.g. skew quads) is ignored.

Example:
    from tfs_reader import TfsTable
    import painting
    angles = painting.kick_angles(TfsTable("runs/x/pass1/PIP_II_Booster_sectormap.outx"),
                                  y_orbump=-0.097, y_pm=-0.016, x_pm=0.0061)
    painting.write_injection("runs/x/PIP_II_Booster_injection.dat", angles)
"""

from pathlib import Path

import numpy as np

from device_data import Devices, groups, value_at
from tfs_reader import TfsTable

INJECTION_FILE = "PIP_II_Booster_injection.dat"
OFFSETS = ("Y_ORBUMP", "Y_PM", "X_PM")     # offset device names [m]
ANGLES = ("ORB_KICK_ANG", "VPML10_ANG", "VPML11U_ANG", "VPML11D_ANG", "VPML12_ANG",
          "HPMS09_ANG", "HPMS10_ANG", "HPMS11_ANG", "HPMS12_ANG")   # file order

# (up 1, up 2, down 1, down 2) painting kicker elements per plane (TREATYPOINT between 2 and 3)
_KICKERS = {
    "x": ("H_PAINT_S09_CT", "H_PAINT_S10_CT", "H_PAINT_S11_CT", "H_PAINT_S12_CT"),
    "y": ("V_PAINT_L10_CT", "V_PAINT_L11U_CT", "V_PAINT_L11D_CT", "V_PAINT_L12_CT"),
}
_ANGLE_NAMES = {"x": ("HPMS09_ANG", "HPMS10_ANG", "HPMS11_ANG", "HPMS12_ANG"),
                "y": ("VPML10_ANG", "VPML11U_ANG", "VPML11D_ANG", "VPML12_ANG")}
_POINT = "TREATYPOINT"
_ORBUMP = ("LBTL_BINJ_OBMP1_8510_KICK", "LBTL_BINJ_OBMP2_8511_KICK",
           "LBTL_BINJ_OBMP3_8510_KICK", "LBTL_BINJ_OBMP4_8511_KICK")


def offsets_at(devs: Devices, t: float, overrides: dict[str, float] | None = None) -> dict[str, float]:
    """{Y_ORBUMP, Y_PM, X_PM} [m] at time t [us]: overrides, else value_at(devs, name, t), else 0."""
    grp = groups(devs)
    out = {}
    for name in OFFSETS:
        if overrides and name in overrides:
            out[name] = float(overrides[name])
        elif name in grp or name in devs:
            out[name] = float(value_at(devs, name, t))
        else:
            out[name] = 0.0
    return out


def transfer(sectormap: TfsTable, start: str, end: str, plane: str) -> np.ndarray:
    """2x2 transfer matrix from element `start` to element `end` (exit to exit), going round
    the ring if end comes before start. plane "x" (R11..R22) or "y" (R33..R44).

    Each sectormap row is the map of that element (from the previous row), so this is the
    product of rows start+1 .. end.
    """
    cols = ("R11", "R12", "R21", "R22") if plane == "x" else ("R33", "R34", "R43", "R44")
    r = np.stack([sectormap[c] for c in cols], axis=1).reshape(-1, 2, 2)
    n = len(r)
    i0, i1 = sectormap.idx(start), sectormap.idx(end)
    m = np.eye(2)
    for i in range(i0 + 1, i1 + 1 + (n if i1 < i0 else 0)):
        m = r[i % n] @ m
    return m


def _paint_pair(sectormap: TfsTable, plane: str, offset: float) -> list[float]:
    """Kicks [rad] of the 4 painting magnets in one plane for a bump `offset` [m] at TREATYPOINT."""
    k1, k2, k3, k4 = _KICKERS[plane]
    m12 = transfer(sectormap, k1, k2, plane)
    m2t = transfer(sectormap, k2, _POINT, plane)
    mt3 = transfer(sectormap, _POINT, k3, plane)
    m34 = transfer(sectormap, k3, k4, plane)
    # up: kick theta1 at k1, theta2 at k2 -> (offset, 0) at TREATYPOINT
    up = np.array([[(m2t @ m12)[0, 1], m2t[0, 1]],
                   [(m2t @ m12)[1, 1], m2t[1, 1]]])
    th1, th2 = np.linalg.solve(up, [offset, 0.0])
    # down: kicks at k3, k4 cancel (offset, 0) carried from TREATYPOINT to k4
    dn = np.array([[m34[0, 1], 0.0],
                   [m34[1, 1], 1.0]])
    th3, th4 = np.linalg.solve(dn, -(m34 @ mt3) @ np.array([offset, 0.0]))
    return [float(th1), float(th2), float(th3), float(th4)]


def kick_angles(sectormap: TfsTable, y_orbump: float = 0.0, y_pm: float = 0.0,
                x_pm: float = 0.0) -> dict[str, float]:
    """{ANGLES name: kick [rad]} for offsets [m] at TREATYPOINT, from a sectormap table
    (<lattice>_sectormap.outx of the same optics). All 0 offsets give all 0 angles.
    """
    pos = {name: sectormap.row(name)["POS"] for name in _ORBUMP}
    leg1 = pos[_ORBUMP[1]] - pos[_ORBUMP[0]]
    leg2 = pos[_ORBUMP[3]] - pos[_ORBUMP[2]]
    if not np.isclose(leg1, leg2, rtol=0, atol=1e-9):
        raise ValueError(f"ORBUMP legs not the same length: {leg1} m, {leg2} m")
    angles = {"ORB_KICK_ANG": float(np.arcsin(y_orbump / leg2))}
    for plane, offset in (("y", y_pm), ("x", x_pm)):
        angles.update(zip(_ANGLE_NAMES[plane], _paint_pair(sectormap, plane, offset)))
    return {name: angles[name] for name in ANGLES}


def write_injection(path: str | Path, angles: dict[str, float] | None = None, note: str | None = None) -> Path:
    """Write the injection .dat file (ORB_KICK_ANG, VPML.._ANG, HPMS.._ANG [rad]; missing = 0)."""
    angles = angles or {}
    num = lambda x: repr(float(x) + 0.0)
    lines = ["!ORBUMP and PAINTING MAGNET CURRENTS"]
    if note:
        lines.append(f"!{note}")
    lines += ["", f"ORB_KICK_ANG={num(angles.get('ORB_KICK_ANG', 0.0))};", ""]
    lines += [f"{name}={num(angles.get(name, 0.0))};" for name in ANGLES[1:]]
    lines += ["", "RETURN;", ""]
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines))
    return path
