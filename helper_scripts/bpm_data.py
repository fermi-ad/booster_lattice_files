"""BMA BPM turn-by-turn data as devices, a turn-timing device, and a JSON format for both (2026-10-25).

BPM devices (one per BPM and plane, device_data style, names B:HST<nn><L|S> (x) / B:VST<nn><L|S> (y)):
    "values"       numpy array, one reading per turn [mm, "unit"], in the BPM's sign (BPM y = -model y,
                   BPM_SIGN; bpm_compare.measured_orbit applies it); nan = no reading
    "begin_turn"   turn number of values[0] as recorded (the TBT files start at 1)
    "turn_offset"  correction [turns] if the BPM is off by some turns (e.g. -1 for L24/S24 in the 10-20 data,
                   ping_analysis.fit_turn_offsets); 0 by default. Turn of values[m] = begin_turn + turn_offset + m
    "timing"       name of the timing device in the same dict (TIMING)
    "kind" "BPM", "role" "tbt", "plane" "x"/"y", plus "cycle", "timestamp", "source" from the reader.
Timing device TIMING = "TBT_TIME" (kind "TBT_TIMING", role "table"): "values" = turn numbers 0, 1, 2, ...,
"t" [us] = cycle time of each turn, "ke" [GeV] = KE of each turn, from ramp_timing.turn_energy started at
"t0" (TURN0 = 2000 us, the control system's turn 0 = injection; user 2026-10-24), "source" = what it was
made from. As a table, dd.value_at(devs, TIMING, t) is the (fractional) turn number at time t.

Readers return these devices (without the timing device; add_timing adds it); read(path) picks by suffix:
    read_tbt(path)   the user's turn-by-turn text files (blocks "***NEW BPM**"; 1000 = no reading, and 0,
                     what the BPMs give before the beam is there, -> nan)
A new source (pacsys, later also plate signals) gets its own reader returning the same form.

JSON (save / load): the device_data file format {"meta", "devices"}, one device per line, numbers written
short (BPM values 6 significant digits, no reading = NO_READING = 1000, as in the text files; a 0 reading
was already nan, so it comes back as 1000; times to 1 ns), non-BPM devices first. dd.load reads
it too (but keeps 1000; load turns it into nan).

The BPMs run in an injection mode for the first 10-20 turns (different gain and frequency structure),
then switch to the normal mode; all of it is over before turn GAIN_TURNS = 100. Those turns are kept
(read_tbt(first_turn=GAIN_TURNS) drops them).

Example:
    import bpm_data as bd
    devs = bd.read_tbt("studies/BPM_CO_opt/TBT_modelnpingtbtBPM.txt")
    bd.add_timing(devs, rt.ramp_settings(misc), source="10_23_26_MISC_MEV400.json")
    bd.set_turn_offsets(devs, {"B:HST24L": -1})
    t = bd.times(devs["B:HST03L"], devs)        # cycle time [us] of each reading
    i = bd.index_at(devs["B:HST03L"], devs, 8000.0)   # index of the reading at 8 ms
    bd.save(devs, "data_Booster/BPM_10_20_26_noping.json")
    devs, meta = bd.load("data_Booster/BPM_10_20_26_noping.json")
    arr = bd.planes(devs)                       # {"x", "y": (48, n_turns), "begin_turn"}, turn offsets applied

Command line (text file -> JSON with a timing device):
    python helper_scripts/bpm_data.py TBT.txt --misc MISC.json --out OUT.json [--turn-offset B:HST24L=-1 ..] [--note TEXT]
The timing device is computed from the ramp settings at conversion time; after a timing change, add_timing
again (or re-run the conversion). Device names follow ACNET_NAME (may change with the pacsys data).
"""
from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime
from pathlib import Path

import numpy as np
from numpy.typing import ArrayLike

import device_data as dd
import ramp_timing as rt
from device_data import Device, Devices, Meta

N_SECTORS = 24
BPM_ELEMENT = "BMA{ls}{n}_BPM"                     # MAD-X MONITOR names, ls = "L" or "S"
ACNET_NAME = {"x": "B:HST{n:02d}{ls}", "y": "B:VST{n:02d}{ls}"}   # BPM device names
UNIT_TO_MM = {"mm": 1.0, "m": 1e3, "um": 1e-3}
TURN0 = 2000.0                                     # [us] cycle time of turn 0 (control system)
NO_READING = 1000.0                                # file value for "no reading"
GAIN_TURNS = 100                                   # turns < this: BPM injection mode / switch-over (kept by default)
TIMING = "TBT_TIME"                                # name of the turn-timing device


def bpm_elements() -> list[tuple[str, str, int]]:
    """(MAD-X element, "L"/"S", sector n) of the 48 BMA BPMs, sector 1..24, L before S."""
    return [(BPM_ELEMENT.format(ls=ls, n=n), ls, n) for n in range(1, N_SECTORS + 1) for ls in "LS"]


def acnet_name(element: str, plane: str) -> str:
    """ACNET device name of a BMA BPM element ("BMAL3_BPM") for plane "x" or "y" (ACNET_NAME)."""
    for el, ls, n in bpm_elements():
        if el == element:
            return ACNET_NAME[plane].format(ls=ls, n=n)
    raise KeyError(f"{element!r} is not a BMA BPM")


# ---------------------------------------------------------------- devices
def bpm_device(values: ArrayLike, plane: str, begin_turn: int = 0, turn_offset: int = 0, unit: str = "mm",
               timing: str = TIMING, **fields) -> Device:
    """A BPM device: values[m] [unit] is turn begin_turn + turn_offset + m (nan = no reading); plane "x"/"y".
    Extra fields (cycle, timestamp, source, ...) are added."""
    return {"kind": "BPM", "role": "tbt", "plane": plane, "unit": unit, "begin_turn": int(begin_turn),
            "turn_offset": int(turn_offset), "timing": timing, **fields,
            "values": np.asarray(values, dtype=float)}


def timing_device(source: rt.Settings | Device, t0: float = TURN0, n_after: int = 100,
                  note: str | None = None) -> Device:
    """Turn-timing device: turn n (values) at cycle time t[n] [us] with KE ke[n] [GeV], turn 0 at t0, from
    rt.turn_energy(source, t_start=t0) (source: ramp_settings dict or a frequency device). note -> "source"."""
    t_n, ke_n, _ = rt.turn_energy(source, t_start=t0, n_after=n_after)
    return {"kind": "TBT_TIMING", "role": "table", "group": TIMING, "unit": "turn", "t0": float(t0),
            "source": note or "", "values": np.arange(len(t_n), dtype=float), "t": t_n, "ke": ke_n}


def add_timing(devs: Devices, source: rt.Settings | Device, t0: float = TURN0, source_note: str | None = None,
               name: str = TIMING) -> Devices:
    """Add the timing device (timing_device) as devs[name] and point every BPM device at it. In place."""
    devs[name] = timing_device(source, t0, note=source_note)
    for dev in bpm_devices(devs).values():
        dev["timing"] = name
    return devs


def bpm_devices(devs: Devices) -> Devices:
    """The BPM devices (kind "BPM") of devs."""
    return {n: d for n, d in devs.items() if d.get("kind") == "BPM"}


def set_turn_offsets(devs: Devices, offsets: dict[str, int]) -> Devices:
    """Set "turn_offset" of the named BPM devices (e.g. {"B:HST24L": -1}). In place."""
    for name, m in offsets.items():
        devs[name]["turn_offset"] = int(m)
    return devs


def turns(dev: Device) -> np.ndarray:
    """Turn number of each reading: begin_turn + turn_offset + m."""
    return dev.get("begin_turn", 0) + dev.get("turn_offset", 0) + np.arange(len(dev["values"]))


def turn_time(timing: Device, turn: ArrayLike) -> np.ndarray:
    """Cycle time [us] of turn number(s) (integers) from a timing device; nan outside it."""
    idx = np.asarray(turn, dtype=int) - int(timing["values"][0])
    t = np.asarray(timing["t"], dtype=float)
    ok = (idx >= 0) & (idx < len(t))
    return np.where(ok, t[np.clip(idx, 0, len(t) - 1)], np.nan)


def times(dev: Device, devs: Devices) -> np.ndarray:
    """Cycle time [us] of each reading of a BPM device, from its timing device in devs (turn offsets
    applied); nan for turns past the timing device."""
    return turn_time(devs[dev["timing"]], turns(dev))


def turn_at(timing: Device, t: ArrayLike) -> np.ndarray:
    """(Fractional) turn number at cycle time t [us]; nan outside the timing device."""
    return np.interp(t, timing["t"], timing["values"], left=np.nan, right=np.nan)


def index_at(dev: Device, devs: Devices, t: float) -> int:
    """Index into dev["values"] of the turn running at time t [us] (the last turn started at or before t)."""
    n = int(np.floor(turn_at(devs[dev["timing"]], t)))
    return n - int(dev.get("begin_turn", 0) + dev.get("turn_offset", 0))


def planes(devs: Devices) -> dict[str, np.ndarray]:
    """{"x": (48, n), "y": (48, n)} in ring order (BPM sign, mm), columns = turns begin_turn, begin_turn + 1, ...
    with each BPM's turn_offset applied (shifted, nan-padded); a missing BPM is all nan. Also "begin_turn"
    (turn number of column 0, the most common one)."""
    bpms = bpm_devices(devs)
    first = {n: int(d.get("begin_turn", 0) + d.get("turn_offset", 0)) for n, d in bpms.items()}
    begin = int(np.bincount([f - min(first.values()) for f in first.values()]).argmax() + min(first.values()))
    n = max(len(d["values"]) for d in bpms.values())
    out = {}
    for p in "xy":
        arr = np.full((N_SECTORS * 2, n), np.nan)
        for i, (el, _, _) in enumerate(bpm_elements()):
            d = bpms.get(acnet_name(el, p))
            if d is None:
                continue
            v = np.asarray(d["values"], dtype=float) * UNIT_TO_MM[d.get("unit", "mm")]
            s = first[acnet_name(el, p)] - begin             # column of values[0]
            lo, hi = max(s, 0), min(s + len(v), n)
            if hi > lo:
                arr[i, lo:hi] = v[lo - s:hi - s]
        out[p] = arr
    out["begin_turn"] = begin
    return out


# ---------------------------------------------------------------- readers
def read_tbt(path: str | Path, first_turn: int = 0) -> Devices:
    """Read a turn-by-turn BPM text file (blocks "***NEW BPM**", name, "NUMBER OF TURNS n BEGIN TURN: b
    CYCLE: c", MICROTIMESTAMP, GPS TIME, n, then n positions [mm]) into BPM devices (no timing device:
    add_timing). NO_READING, exact 0 and turns before first_turn -> nan. Plane from the name (B:H.. x, B:V.. y)."""
    devs: Devices = {}
    for block in open(path).read().split("***NEW BPM**")[1:]:
        lines = block.strip().splitlines()
        name, head = lines[0].strip(), lines[1].split()
        n = int(lines[4])
        v = np.array(" ".join(lines[5:]).split(), dtype=float)
        if len(v) != n:
            raise ValueError(f"{path}: {name} has {len(v)} values, header says {n}")
        begin = int(head[head.index("TURN:") + 1])
        v[(v == NO_READING) | (v == 0.0) | (np.arange(n) + begin < first_turn)] = np.nan
        devs[name] = bpm_device(v, "x" if name.startswith("B:H") else "y", begin_turn=begin,
                                cycle=int(head[head.index("CYCLE:") + 1]),
                                timestamp=lines[2].split(":", 1)[1].strip(), source=Path(path).name)
    return devs


def read(path: str | Path) -> Devices:
    """BPM devices from a file: a .json written by save (with its timing device, if saved), else a TBT text
    file (read_tbt)."""
    return load(path)[0] if Path(path).suffix.lower() == ".json" else read_tbt(path)


# ---------------------------------------------------------------- JSON
_FMT = {"values": "{:.6g}", "t": "{:.3f}", "ke": "{:.9g}"}     # per array key; others "{:.10g}"


def _dump(x, key: str = "", bpm: bool = False) -> str:
    if isinstance(x, np.ndarray) and x.ndim == 1:
        fmt = "{:.10g}" if key == "values" and not bpm else _FMT.get(key, "{:.10g}")
        v = np.where(np.isnan(x), NO_READING, x) if bpm and key == "values" else x
        return "[" + ",".join(fmt.format(a) for a in v.tolist()) + "]"
    return json.dumps(x, default=dd._to_json)


def save(devs: Devices, path: str | Path, meta: Meta | None = None) -> None:
    """Write devices (BPM and timing, any device_data device) to JSON, one device per line, arrays last.
    BPM nan -> NO_READING. meta gets "created"/"modified" (as dd.save)."""
    meta = {} if meta is None else meta
    now = datetime.now().isoformat(timespec="seconds")
    meta.setdefault("created", now)
    meta["modified"] = now
    lines = []
    for name, dev in sorted(devs.items(), key=lambda kv: kv[1].get("kind") == "BPM"):    # timing etc. first
        bpm = dev.get("kind") == "BPM"
        keys = sorted(dev, key=lambda k: isinstance(dev[k], np.ndarray))       # scalars first
        body = ", ".join(f"{json.dumps(k)}: {_dump(dev[k], k, bpm)}" for k in keys)
        lines.append(f"  {json.dumps(name)}: {{{body}}}")
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w") as f:
        f.write('{"meta": ' + json.dumps(meta, default=dd._to_json) + ',\n "devices": {\n' + ",\n".join(lines) + "\n}}\n")


def load(path: str | Path) -> tuple[Devices, Meta]:
    """Read a file written by save (or any device_data file): dd.load, then BPM NO_READING -> nan."""
    devs, meta = dd.load(path)
    for dev in bpm_devices(devs).values():
        v = np.asarray(dev["values"], dtype=float)
        v[v == NO_READING] = np.nan
        dev["values"] = v
    return devs, meta


# ---------------------------------------------------------------- command line
def main(argv: list[str] | None = None) -> None:
    ap = argparse.ArgumentParser(description="Convert a TBT text file to bpm_data JSON with a timing device.")
    ap.add_argument("tbt", help="turn-by-turn text file")
    ap.add_argument("--misc", required=True, help="*_MISC_*.json: ramp settings for the timing device")
    ap.add_argument("--out", required=True, help="output .json")
    ap.add_argument("--turn-offset", nargs="*", default=[], metavar="NAME=M", help="turn offsets, e.g. B:HST24L=-1")
    ap.add_argument("--note", default="", help="text for meta notes")
    a = ap.parse_args(argv)
    devs = read_tbt(a.tbt)
    add_timing(devs, rt.ramp_settings(dd.load(a.misc)[0]), source_note=Path(a.misc).name)
    offsets = {k: int(v) for k, v in (x.split("=") for x in a.turn_offset)}
    set_turn_offsets(devs, offsets)
    day = datetime.now().date().isoformat()
    note = (f"{day}: from {Path(a.tbt).name} (bpm_data.read_tbt); timing device from {Path(a.misc).name} (turn 0 at "
            f"{TURN0:g} us); turn offsets {offsets or 'none'}. {a.note}").strip()
    save(devs, a.out, {"source": Path(a.tbt).name, "notes": note})
    print(f"{len(bpm_devices(devs))} BPM devices, {len(devs[TIMING]['t'])} timing turns -> {a.out}")


if __name__ == "__main__":
    sys.path.insert(0, str(Path(__file__).parent))
    main()
