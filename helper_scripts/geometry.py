"""3D frames for the MAD-X survey and rigid-body misalignments.

MAD-X global axes: X, Y, Z = North, elevation, East (the Booster survey's EAST/NORTH/ELEVATION map to Z/X/Y).
A local frame is (origin O [m], W): the columns of W are the local x, y, s unit vectors in global coordinates,
W = Theta(theta) Phi(phi) Psi(psi) (MAD-X manual, SURVEY). A point with local coordinates p is at O + W p.

MAD-X misalignments (DX, DY, DS, DTHETA, DPHI, DPSI, as element attributes or EALIGN; both give the same TWISS,
checked 2026-11-10) move an element as a rigid body relative to its ideal ENTRANCE frame (for a bend: the
tangent to the reference orbit at the entrance, not the chord): new entrance origin = O + W d, new entrance
axes = W R(dtheta, dphi, dpsi), with R = Theta Phi Psi like W. Checked in MAD-X 5.08.01 (SURVEY with
PERM_ALIGN_SURVEY=TRUE and TWISS): rotations pivot at the entrance (a quad with DTHETA = a acts like DX = a L / 2),
+DTHETA turns s toward +x, +DPHI turns s toward +y, translations are along the entrance axes.

Units: m and rad. Moved from studies/survey_misalignments (2026-11-13); used by misalign.py.

    import geometry as g
    f = g.Frame(o, g.w_matrix(theta, phi, psi))        # a MAD-X survey frame
    r = g.body_rigid(f, {"dx": 1e-3, "dtheta": 1e-4})  # a body moved about its frame
    g.offsets_in(entrance_frame, r)                   # -> the MAD-X DX..DPSI of an element of that body
"""
from __future__ import annotations

from dataclasses import dataclass

import numpy as np

Vec = np.ndarray
Mat = np.ndarray


def rot_theta(a: float) -> Mat:
    """Theta(a): rotation about the local y axis (+a turns s toward +x)."""
    c, s = np.cos(a), np.sin(a)
    return np.array([[c, 0.0, s], [0.0, 1.0, 0.0], [-s, 0.0, c]])


def rot_phi(a: float) -> Mat:
    """Phi(a): rotation about the local x axis (+a turns s toward +y)."""
    c, s = np.cos(a), np.sin(a)
    return np.array([[1.0, 0.0, 0.0], [0.0, c, s], [0.0, -s, c]])


def rot_psi(a: float) -> Mat:
    """Psi(a): rotation about the local s axis (roll)."""
    c, s = np.cos(a), np.sin(a)
    return np.array([[c, -s, 0.0], [s, c, 0.0], [0.0, 0.0, 1.0]])


def w_matrix(theta: float, phi: float, psi: float) -> Mat:
    """W = Theta(theta) Phi(phi) Psi(psi) [rad]: columns = local x, y, s in global coordinates."""
    return rot_theta(theta) @ rot_phi(phi) @ rot_psi(psi)


def angles(w: Mat) -> tuple[float, float, float]:
    """(theta, phi, psi) [rad] with w_matrix(theta, phi, psi) = w, phi in [-pi/2, pi/2]."""
    theta = np.arctan2(w[0, 2], w[2, 2])
    phi = np.arctan2(w[1, 2], np.hypot(w[0, 2], w[2, 2]))
    psi = np.arctan2(w[1, 0], w[1, 1])
    return float(theta), float(phi), float(psi)


@dataclass
class Frame:
    """A local frame: origin o [m] (global X, Y, Z) and axes w (columns x, y, s)."""
    o: Vec
    w: Mat

    def to_global(self, p: Vec) -> Vec:
        """Global position [m] of local coordinates p [m]."""
        return self.o + self.w @ np.asarray(p, float)

    def to_local(self, q: Vec) -> Vec:
        """Local coordinates [m] of the global position q [m]."""
        return self.w.T @ (np.asarray(q, float) - self.o)

    def moved(self, rigid: "Rigid") -> "Frame":
        """This frame carried by the rigid motion (both are in global coordinates)."""
        return Frame(rigid.apply(self.o), rigid.r @ self.w)


@dataclass
class Rigid:
    """Rigid motion in global coordinates: q -> r q + t (r [3x3], t [m])."""
    r: Mat
    t: Vec

    @staticmethod
    def identity() -> "Rigid":
        """No motion."""
        return Rigid(np.eye(3), np.zeros(3))

    def apply(self, q: Vec) -> Vec:
        """Image of the global position(s) q [m] (shape (3,) or (n, 3))."""
        q = np.asarray(q, float)
        return q @ self.r.T + self.t

    def then(self, other: "Rigid") -> "Rigid":
        """This motion followed by other."""
        return Rigid(other.r @ self.r, other.r @ self.t + other.t)

    def inverse(self) -> "Rigid":
        """The inverse motion."""
        return Rigid(self.r.T, -self.r.T @ self.t)


def body_rigid(frame: Frame, offsets: dict[str, float]) -> Rigid:
    """Global rigid motion of a body whose offsets are given in its own frame (about the frame origin):
    offsets dx, dy, ds [m] along the frame's x, y, s and dtheta, dphi, dpsi [rad] (Theta Phi Psi, like
    MAD-X), missing = 0. The body point at local p goes to frame.o + frame.w (d + R p)."""
    d = np.array([offsets.get(k, 0.0) for k in ("dx", "dy", "ds")])
    r_loc = w_matrix(offsets.get("dtheta", 0.0), offsets.get("dphi", 0.0), offsets.get("dpsi", 0.0))
    r = frame.w @ r_loc @ frame.w.T
    t = frame.o + frame.w @ d - r @ frame.o
    return Rigid(r, t)


def offsets_in(frame: Frame, rigid: Rigid) -> dict[str, float]:
    """The rigid motion as offsets about this frame (the inverse of body_rigid): for an element's entrance frame
    these are its MAD-X DX, DY, DS [m], DTHETA, DPHI, DPSI [rad]."""
    moved = frame.moved(rigid)
    d = frame.w.T @ (moved.o - frame.o)
    th, ph, ps = angles(frame.w.T @ moved.w)
    return {"dx": float(d[0]), "dy": float(d[1]), "ds": float(d[2]),
            "dtheta": th, "dphi": ph, "dpsi": ps}


def fit_rigid(src: np.ndarray, dst: np.ndarray, weights: np.ndarray | None = None) -> Rigid:
    """Least-squares rigid motion (Kabsch, no scale) taking the points src (n, 3) [m] onto dst (n, 3)."""
    src, dst = np.asarray(src, float), np.asarray(dst, float)
    wgt = np.ones(len(src)) if weights is None else np.asarray(weights, float)
    wgt = wgt / wgt.sum()
    cs, cd = wgt @ src, wgt @ dst
    h = (src - cs).T @ ((dst - cd) * wgt[:, None])
    u, _, vt = np.linalg.svd(h)
    sign = np.sign(np.linalg.det(vt.T @ u.T))
    r = vt.T @ np.diag([1.0, 1.0, sign]) @ u.T
    return Rigid(r, cd - r @ cs)


def line_frame(up: Vec, dn: Vec, up_vector: Vec = np.array([0.0, 1.0, 0.0]), y_sign: float = 1.0) -> Frame:
    """Frame of a straight body from two points [m]: origin at the midpoint, s from up to dn, x horizontal
    (perpendicular to s and to up_vector, global elevation by default), no roll; y = s x x ... chosen so the frame is
    right-handed (x, y, s) with y along +up_vector when y_sign = 1, -up_vector when y_sign = -1 (x flips with it)."""
    up, dn = np.asarray(up, float), np.asarray(dn, float)
    s = (dn - up) / np.linalg.norm(dn - up)
    x = np.cross(up_vector, s) * y_sign       # (x, y, s) right-handed: x = y x s
    x /= np.linalg.norm(x)
    y = np.cross(s, x)
    return Frame(0.5 * (up + dn), np.column_stack([x, y, s]))
