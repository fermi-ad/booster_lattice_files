"""Optics and element layout of a region of the ring, from a MAD-X twiss table.

One figure, sharing s: element diagram, beta, dispersion, closed orbit (optional). It works on any twiss table the lattices
write (`TfsTable` file or `res.twiss` of `run_madx.run_point`), so the lattices need no change:

    import lattice_plot as lp
    lp.plot_region(res.twiss, "H_PAINT_S09_UP", "H_PAINT_S12_DN", out="studies/survey/pip2_injection.png")

CLI (run the point first with run_madx, then plot its twiss file; a region change needs no new run):

    python helper_scripts/lattice_plot.py --twiss madx_files/PIP_II_Booster_twiss.outx \
        --baseline madx_files/MEV400_Booster_twiss.outx \
        --start ORB_ENT_BPM_UP --end ORB_EXI_BPM_DN --out studies/survey/pip2_orbump.png

With --survey FILE instead of --start/--end: map of the ring from the lattice's survey file (`plot_survey`).

Elements: bends from the RBEND rows (F above the line, D below, by the sign of K1L), the rest from the names: lengths
are the <name>_UP / <name>_DN marker pairs (the thin kick or monitor sits between them), `FAMILIES` gives the family.
Every element has a black boundary. With a baseline twiss table (e.g. MEV400), elements whose name is in it also get a
black hatch (old BPMs: one line through the diamond).
Between rows the curves are cubic Hermite with the table's slopes: exact in drifts, a small error inside the bends.
"""
import argparse
import re
from collections.abc import Sequence
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.lines import Line2D
from matplotlib.patches import Patch, Polygon, Rectangle

from tfs_reader import TfsTable

# Family of a marker pair, first match of the regex on its stem. None = not drawn (contains the real element, or
# is only a label in the lattice).
FAMILIES = (
    (r"^(BMA[LS]\d+_SPOOL|[HV]_PAINT_.*SPOOL|ORB\d+_SPOOL|MARK_L|VACTEE)|_(FID|FLNG|CASE)$", None),
    (r"^BMA[LS]\d+_MAG$", "BMA corrector package"),
    (r"OBMP", "ORBUMP"),
    (r"^H_PAINT", "H painting"),
    (r"^V_PAINT", "V painting"),
    (r"FOIL", "Foil"),
    (r"KICKER_(L|S)\d|^BEX|^MKS|KICKER", "Kicker"),
    (r"^ABS", "Absorber"),
    (r"COLL", "Collimator"),
    (r"^SEPT", "Septum"),
    (r"^RF_CAV", "RF cavity"),
    (r".", "Instrument"),
)
# Half height in the diagram, drawn around 0 (bends: 0 to +/-1).
HEIGHT = {"BMA corrector package": 0.6, "Foil": 1.2, "Kicker": 0.8, "ORBUMP": 0.8, "H painting": 0.8, "V painting": 0.8,
          "Absorber": 0.7, "Collimator": 0.7, "Septum": 0.8, "RF cavity": 0.6, "Instrument": 0.35}
# Family: (colour, legend entry), in legend order; families with the same colour share one entry, names joined.
STYLE = {"F magnet": ("C0", "Dipole (F above, D below)"), "D magnet": ("C0", "Dipole (F above, D below)"),
         "BMA corrector package": ("C2", "Corrector (BMA)"), "H painting": ("C4", "H. Painting"),
         "V painting": ("C1", "V. Painting"), "ORBUMP": ("C1", "ORBUMP"),
         "Foil": ("C3", "Foil"), "Absorber": ("C3", "ABS (IDUMP 8514)"), "BPM": ("gold", "BPM"),
         **{f: ("C7", "Other") for f in ("Kicker", "Collimator", "Septum", "RF cavity", "Instrument")}}
STARRED = ("Foil",)                                       # also get a star under the box
BPM = re.compile(r"_BPM$|^OLDSTYLE_BPM")
SURVEY_SCALE = 2.5                                        # survey map: metres on the map per unit of diagram height
SURVEY_MIN = 0.7                                          # survey map: least drawn length of a box [m]
MIN_WIDTH = 0.004                                         # least drawn width of a box, fraction of the region
LABEL_GAP = 0.0095                                        # least distance between labels, fraction of the region
TREATY_DZ = 0.113                                         # treaty point above the survey reference line [m]
MARK = "TREATYPOINT"                                      # dashed line through the panels, if in the region


def _family(stem: str) -> str | None:
    """Family of a marker pair's stem (`FAMILIES`)."""
    if stem.endswith("BPM"):
        return "BPM"
    return next(fam for rx, fam in FAMILIES if re.search(rx, stem))


def _label(name: str) -> str:
    """Short element name for the diagram."""
    if name == "ABS":
        return "ABS (IDUMP 8514)"
    name = re.sub(r"_CORE$|_MAG$", "", name)
    name = re.sub(r"^LBTL_BINJ_FOIL_\d+", "FOIL", name)
    name = name.removeprefix("LBTL_BINJ_")
    return name


def elements(twiss: TfsTable) -> list[dict]:
    """Elements to draw, from a twiss table: dicts name, family, s0, s1 [m] (s1 = s0 for a thin BPM), by s0."""
    names, s, length = [str(n) for n in twiss["NAME"]], twiss["S"], twiss["L"]
    up = {n[:-3]: i for i, n in enumerate(names) if n.endswith("_UP")}
    found = []
    for i, n in enumerate(names):
        if n.endswith("_CORE"):
            fam = "F magnet" if twiss["K1L"][i] > 0 else "D magnet"
            found.append({"name": n, "family": fam, "s0": s[i] - length[i], "s1": s[i]})
        elif BPM.search(n):
            found.append({"name": n, "family": "BPM", "s0": s[i], "s1": s[i]})
        elif n.endswith("_DN") and n[:-3] in up and _family(n[:-3]):
            stem = n[:-3]
            found.append({"name": stem, "family": _family(stem), "s0": s[up[stem]], "s1": s[i]})
    return sorted(found, key=lambda e: e["s0"])


def interpolate(twiss: TfsTable, step: float = 0.05) -> dict[str, np.ndarray]:
    """Optics along s, at most `step` [m] apart: s, betx, bety [m], dx, dy [m] (dispersion in dp/p), x, y [mm].

    Cubic Hermite per table interval from the values and slopes at its ends (the table has a row for each drift, so
    an interval ends just before a thin element). Slopes jump at thin elements, as they should."""
    beta = np.sqrt(1 - 1 / float(twiss.header["GAMMA"]) ** 2)
    s = twiss["S"]
    curves = {"betx": (twiss["BETX"], -2 * twiss["ALFX"]), "bety": (twiss["BETY"], -2 * twiss["ALFY"]),
              "dx": (beta * twiss["DX"], beta * twiss["DPX"]), "dy": (beta * twiss["DY"], beta * twiss["DPY"]),
              "x": (1e3 * twiss["X"], 1e3 * twiss["PX"]), "y": (1e3 * twiss["Y"], 1e3 * twiss["PY"])}
    out = {k: [] for k in ("s", *curves)}
    for i in np.flatnonzero(np.diff(s) > 1e-9):
        ds = s[i + 1] - s[i]
        u = np.linspace(0, 1, max(2, int(np.ceil(ds / step)) + 1))
        out["s"].append(s[i] + u * ds)
        for k, (v, m) in curves.items():
            out[k].append((2 * u**3 - 3 * u**2 + 1) * v[i] + (u**3 - 2 * u**2 + u) * ds * m[i]
                          + (-2 * u**3 + 3 * u**2) * v[i + 1] + (u**3 - u**2) * ds * m[i + 1])
    return {k: np.concatenate(v) for k, v in out.items()}


def _position(twiss: TfsTable, where: float | str) -> float:
    """s [m] of a position given as a number or an element name."""
    if isinstance(where, str):
        return float(twiss.row(where)["S"])
    return float(where)


def region(twiss: TfsTable, start: float | str, end: float | str, pad: float = 0.0) -> tuple[float, float]:
    """(s0, s1) [m] of a region, s0 < s1, s0 may be < 0: names or s, across the sequence start if start comes after
    end. pad [m] widens it on both sides."""
    s0, s1 = _position(twiss, start), _position(twiss, end)
    if s0 > s1:
        s0 -= float(twiss["S"][-1])
    return s0 - pad, s1 + pad


def plot_region(twiss: TfsTable, start: float | str, end: float | str, pad: float | None = None, step: float = 0.05,
                labels: bool | None = None, title: str | None = None, out: str | Path | None = None,
                figsize: tuple[float, float] | None = None, baseline: TfsTable | None = None,
                orbit: bool = True) -> plt.Figure:
    """Element diagram, beta, dispersion and closed orbit between start and end.

    start, end: s [m] or element names (`region`; s may be < 0 or past the circumference, the ring repeats).
    pad: margin [m], default 2% of the region. labels: element names on the diagram, default if under 90 m.
    title: default the sequence and tunes. out: file to save; without a suffix, both .png and .pdf.
    baseline: twiss table of the older lattice; elements also in it get a black hatch.
    orbit: include the closed orbit panel. figsize: default (12, 9), (12, 7.5) without the orbit."""
    s0, s1 = region(twiss, start, end)
    pad = 0.02 * (s1 - s0) if pad is None else pad
    s0, s1 = s0 - pad, s1 + pad
    labels = s1 - s0 < 90 if labels is None else labels
    circ = float(twiss["S"][-1])

    turn = interpolate(twiss, step)
    shifts = circ * np.arange(np.floor(s0 / circ), np.floor(s1 / circ) + 1)
    data = {k: np.concatenate([v + d if k == "s" else v for d in shifts]) for k, v in turn.items()}
    keep = (data["s"] >= s0) & (data["s"] <= s1)
    data = {k: v[keep] for k, v in data.items()}

    heights = [1.5 if labels else 0.6, 1.5, 1] + ([1] if orbit else [])
    fig, axes = plt.subplots(len(heights), 1, sharex=True, figsize=figsize or (12, 9 if orbit else 7.5),
                             height_ratios=heights, layout="constrained")
    diagram, ax_beta, ax_disp = axes[:3]
    draw_diagram(diagram, twiss, s0, s1, labels, baseline)

    ax_beta.plot(data["s"], data["betx"], label=r"$\beta_x$")
    ax_beta.plot(data["s"], data["bety"], label=r"$\beta_y$")
    ax_beta.set_ylabel(r"$\beta$ [m]")
    ax_disp.plot(data["s"], data["dx"], label=r"$D_x$")
    ax_disp.plot(data["s"], data["dy"], label=r"$D_y$")
    ax_disp.set_ylabel("D [m]")
    ax_disp.set_ylim(top=ax_disp.get_ylim()[1] * 1.3)     # room for the legend above the peaks
    if orbit:
        axes[3].plot(data["s"], data["x"], label="x (+ out)")
        axes[3].plot(data["s"], -data["y"], label="-y (+ up)")
        axes[3].set_ylabel("orbit [mm]")
    axes[-1].set_xlabel("s [m] from the sequence start")
    axes[-1].set_xlim(s0, s1)

    names = [str(n) for n in twiss["NAME"]]
    if MARK in names:
        for s in twiss.row(MARK)["S"] + shifts:
            if s0 <= s <= s1:
                diagram.plot([s, s], [-2.2, 1.25], color="k", ls="--", lw=0.8)
                for ax in axes[1:]:
                    ax.axvline(s, color="k", ls="--", lw=0.8)
    for ax in axes[1:]:
        ax.legend(loc="best", ncol=2)
        ax.grid(alpha=0.3)
    if title is None:
        title = (f"{twiss.header.get('SEQUENCE', '')}: "
                 f"Q1 = {float(twiss.header['Q1']):.3f}, Q2 = {float(twiss.header['Q2']):.3f}")
    diagram.set_title(title)
    if out is not None:
        out = Path(out)
        out.parent.mkdir(parents=True, exist_ok=True)
        for path in [out] if out.suffix else [out.with_suffix(".png"), out.with_suffix(".pdf")]:
            fig.savefig(path, dpi=200)
    return fig


def draw_diagram(ax: plt.Axes, twiss: TfsTable, s0: float, s1: float, labels: bool = True,
                 baseline: TfsTable | None = None, hide: Sequence[str] = ()) -> None:
    """Element diagram and its key on ax, between s0 and s1 [m] (s0 < s1, s may be < 0).
    labels: element names. baseline: twiss table of the older lattice, elements also in it get a black hatch.
    hide: families not to draw (`STYLE` keys), e.g. ["H painting"]."""
    circ = float(twiss["S"][-1])
    shifts = circ * np.arange(np.floor(s0 / circ), np.floor(s1 / circ) + 1)
    old = None if baseline is None else {e["name"] for e in elements(baseline)}
    seen, ages = _draw_elements(ax, twiss, shifts, s0, s1, labels, old, hide)
    handles = _key_handles(seen, ages)
    ax.legend(handles=handles, loc="upper center", bbox_to_anchor=(0.5, 0.0), ncol=len(handles), fontsize=8,
              frameon=False, handlelength=1.6, columnspacing=1.2, borderaxespad=0.0)


def _key_handles(seen: set[str], ages: set[str]) -> list:
    """Key entries for the families drawn and the "new", "old" marks drawn."""
    entries = {}
    for family, (colour, entry) in STYLE.items():
        if family in seen and entry not in entries.setdefault(colour, []):
            entries[colour].append(entry)
    handles = [Line2D([], [], color=c, marker="D", ls="", label="BPM") if e == ["BPM"]
               else Patch(color=c, label=", ".join(e)) for c, e in entries.items()]
    if "new" in ages:
        handles.append(Patch(facecolor="w", edgecolor="k", label="New element"))
    if "old" in ages:
        handles.append(Patch(facecolor="w", edgecolor="k", hatch="//", label="Old element"))
    return handles


def _extent(family: str) -> tuple[float, float]:
    """Low and high edge of a family's box, in diagram units: dipoles on one side of the line, the rest centred."""
    high = HEIGHT.get(family, 0)
    return {"F magnet": (0, 1), "D magnet": (-1, 0)}.get(family, (-high, high))


def plot_survey(survey: TfsTable, twiss: TfsTable, baseline: TfsTable | None = None, title: str | None = None,
                out: str | Path | None = None, figsize: tuple[float, float] = (10.5, 11)) -> plt.Figure:
    """Map of the ring: the elements at their survey positions, East (MAD-X Z) against North (MAD-X X) [m], which
    is the survey beamsheet's frame (checked against its magnet centres, 2026-11-04).

    survey: the lattice's survey table; twiss: a twiss table of the same lattice, for the elements. Same colours and
    key as `plot_region`, F magnets outside the line, D inside. Box widths across the line are schematic
    (`SURVEY_SCALE`), and short elements are drawn `SURVEY_MIN` long. baseline: elements also in it get a black hatch.
    title: default the sequence name. out: file to save; without a suffix, both .png and .pdf."""
    s, east, north = survey["S"], survey["Z"], survey["X"]
    centre = np.array([0.5 * (east.min() + east.max()), 0.5 * (north.min() + north.max())])
    at = lambda q: np.array([np.interp(q, s, east), np.interp(q, s, north)])
    old = None if baseline is None else {e["name"] for e in elements(baseline)}
    fig, ax = plt.subplots(figsize=figsize, layout="constrained")
    ax.plot(east, north, color="k", lw=0.6, zorder=1)
    seen, ages = set(), set()
    for e in elements(twiss):
        fam, a, b = e["family"], e["s0"], e["s1"]
        ages.add("new" if old is not None and e["name"] not in old else "old" if old is not None else "")
        seen.add(fam)
        out_dir = at(0.5 * (a + b)) - centre
        out_dir = out_dir / np.linalg.norm(out_dir)
        if fam == "BPM":
            ax.plot(*at(0.5 * (a + b)), marker="D", ms=3.5, color=STYLE[fam][0], mec="k", mew=0.4, ls="", zorder=4)
            continue
        if b - a < SURVEY_MIN:
            a, b = 0.5 * (a + b - SURVEY_MIN), 0.5 * (a + b + SURVEY_MIN)
        low, high = (SURVEY_SCALE * v * out_dir for v in _extent(fam))
        hatch = "//" if old is not None and e["name"] in old else None
        ax.add_patch(Polygon([at(a) + low, at(b) + low, at(b) + high, at(a) + high], facecolor=STYLE[fam][0],
                             edgecolor="k", lw=0.4, hatch=hatch, zorder=3))
    radius = np.hypot(*(np.array([east, north]).T - centre).T).max()
    names = [str(n) for n in survey["NAME"]]
    for n in range(1, 25):
        if f"BMAL{n}" in names:
            p = np.array([east[names.index(f"BMAL{n}")], north[names.index(f"BMAL{n}")]])
            q = centre + (p - centre) / np.linalg.norm(p - centre) * (radius + 7.5)
            ax.text(*q, f"L{n}", ha="center", va="center", fontsize=9)
    if MARK in names:
        p = np.array([east[names.index(MARK)], north[names.index(MARK)]])
        ax.plot(*p, marker="*", ms=14, color="C3", mec="k", mew=0.5, ls="", zorder=5)
        i = names.index(MARK)
        z = float(survey["Y"][i])
        beam = at(s[i] + 1) - at(s[i] - 1)
        bearing = np.degrees(np.arctan2(beam[1], beam[0])) % 360
        ax.text(p[0] - 10, p[1], f"Treaty point [m]\nx (East) = {p[0]:.3f}\ny (North) = {p[1]:.3f}\n"
                f"z (elevation) = {z + TREATY_DZ:.3f}\n(survey {z:.3f} + {TREATY_DZ})\n"
                f"beam bearing = {bearing:.3f}\N{DEGREE SIGN} from East", ha="right", va="center", fontsize=9)
    ax.set_aspect("equal")
    ax.grid(alpha=0.3)
    ax.set_xlabel("East [m]")
    ax.set_ylabel("North [m]")
    ax.set_title(title if title is not None else str(twiss.header.get("SEQUENCE", "")))
    handles = _key_handles(seen, ages)
    for h in handles:
        h.set_label(h.get_label().replace("F above, D below", "F outside, D inside"))
    fig.legend(handles=handles, loc="outside lower center", ncol=5, fontsize=9, frameon=False)
    if out is not None:
        out = Path(out)
        out.parent.mkdir(parents=True, exist_ok=True)
        for path in [out] if out.suffix else [out.with_suffix(".png"), out.with_suffix(".pdf")]:
            fig.savefig(path, dpi=200)
    return fig


def _spread(x: np.ndarray, gap: float, low: float, high: float) -> np.ndarray:
    """Sorted positions x moved apart to at least `gap`, staying near where they were and within low..high."""
    x = x.copy()
    for _ in range(500):
        short = np.maximum(gap - np.diff(x), 0) / 2
        if not short.any():
            break
        x[:-1] -= short
        x[1:] += short
        x = np.clip(x, low, high)
    return x


def _draw_elements(ax: plt.Axes, twiss: TfsTable, shifts: np.ndarray, s0: float, s1: float, labels: bool,
                   old: set[str] | None = None, hide: Sequence[str] = ()) -> tuple[set[str], set[str]]:
    """Draw the elements between s0 and s1 on ax, all with a black boundary. With a set of old names: black hatch on
    the elements in it. Return the families drawn and which of "new", "old" were drawn."""
    ax.axhline(0, color="k", lw=0.8)
    seen, names, ages = set(), [], set()
    for e in elements(twiss):
        fam = e["family"]
        if fam in hide:
            continue
        new = old is not None and e["name"] not in old
        is_old = old is not None and not new
        edge = {"edgecolor": "k", "lw": 0.5, "hatch": "//" if is_old else None}
        for d in shifts:
            a, b = e["s0"] + d, e["s1"] + d
            if b < s0 or a > s1:
                continue
            seen.add(fam)
            ages |= {"new"} if new else {"old"} if is_old else set()
            if fam == "BPM":
                ax.plot(0.5 * (a + b), 0, marker="D", ms=5, color=STYLE[fam][0], ls="", zorder=4, mec="k", mew=0.7)
                if is_old:
                    ax.plot(0.5 * (a + b), 0, marker=(2, 2, -45), ms=5, color="k", mew=0.7, ls="", zorder=5)
                continue
            high = HEIGHT.get(fam, 0)
            low, high = {"F magnet": (0, 1), "D magnet": (-1, 0)}.get(fam, (-high, high))
            width = max(b - a, MIN_WIDTH * (s1 - s0))
            ax.add_patch(Rectangle((0.5 * (a + b - width), low), width, high - low, facecolor=STYLE[fam][0], zorder=3,
                                   **edge))
            if fam in STARRED:
                ax.plot(0.5 * (a + b), -1.75, marker="*", ms=10, color="C3", ls="", zorder=4)
            names.append((float(np.clip(0.5 * (a + b), s0, s1)), _label(e["name"])))
    if labels and names:
        names.sort()
        at = _spread(np.array([n[0] for n in names]), LABEL_GAP * (s1 - s0), s0, s1)
        for (where, text), x in zip(names, at):
            ax.plot([where, x], [1.25, 1.75], color="0.5", lw=0.4)
            ax.text(x, 1.8, text, rotation=90, ha="center", va="bottom", fontsize=6.5)
    ax.set_ylim(-2.2, 6.6 if labels else 1.4)
    ax.set_yticks([])
    ax.spines[:].set_visible(False)
    ax.tick_params(bottom=False)
    return seen, ages


def main(argv: Sequence[str] | None = None) -> None:
    """Command line: plot a region of a twiss file."""
    p = argparse.ArgumentParser(description="Optics and element layout of a region, from a MAD-X twiss file.")
    p.add_argument("--twiss", required=True, help="twiss file")
    p.add_argument("--baseline", default=None, help="older lattice's twiss file: elements also in it get a hatch")
    p.add_argument("--start", help="element name or s [m], may be negative")
    p.add_argument("--end", help="element name or s [m]")
    p.add_argument("--survey", help="survey file: draw the map of the ring instead of a region")
    p.add_argument("--pad", type=float, default=None, help="margin [m]")
    p.add_argument("--title", default=None)
    p.add_argument("--no-labels", action="store_true", help="no element names on the diagram")
    p.add_argument("--no-orbit", action="store_true", help="no closed orbit panel")
    p.add_argument("--out", required=True, help="output file; without a suffix, .png and .pdf")
    args = p.parse_args(argv)

    def where(text: str) -> float | str:
        try:
            return float(text)
        except ValueError:
            return text

    baseline = TfsTable(args.baseline) if args.baseline else None
    if args.survey:
        plot_survey(TfsTable(args.survey), TfsTable(args.twiss), baseline, args.title, args.out)
        return
    if args.start is None or args.end is None:
        p.error("--start and --end are needed without --survey")
    plot_region(TfsTable(args.twiss), where(args.start), where(args.end), pad=args.pad, title=args.title,
                labels=False if args.no_labels else None, out=args.out, orbit=not args.no_orbit,
                baseline=baseline)


if __name__ == "__main__":
    main()
