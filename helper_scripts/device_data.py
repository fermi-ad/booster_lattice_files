"""Device settings stored as a dict of dicts, keyed by the full device name.

Each device is a plain dict with any keys you like. Conventional keys:
    kind    device family, e.g. "Q", "SQ", "H", "VRF", "BFLAT"
    group   name of the whole device this one is a part of, e.g. "B:QL1" for
            "B:QL1T" and "B:QL1O" (see "Groups" below)
    role    the part it plays in its group: "table", "scale", "offset", "setting" (a single
            value that stands by itself, e.g. B:VIMIN, BDOT; no table goes with it) or
            "value" (a value put in by an override, replacing a setting, offset, table or
            whole group). value_at treats "setting" and "value" the same.
    slot    ACNET table/slot number the data came from (was "table" before 2026-10-23)
    value   a scalar setting
    values  an array setting (e.g. a ramp table)
    t       time of each entry in "values", in microseconds (TIME_UNIT)
    dt      time steps as read from the source, in microseconds (only used to
            build "t"; see below)
    unit    units of the value(s)

Time is handled in microseconds everywhere, and "t" is the only time array that
is used. If a device is given "t", that is used as is. If it only has "dt" (e.g.
a raw ramp table), add_t() builds t = cumsum(dt) once, when the device is loaded
or added; "dt" is then kept only as a record of the source and is not updated
when "t" is changed. To shift a ramp, edit "t" directly.

Groups: the devices that make up one physical device share a "group" key, and
each has a "role": a ramp "table" ("values" + "t"), a "scale" factor ("sf") and an
"offset" ("value"). E.g. group "B:BFLAT" = B:BFLATT (table) + B:BFLATS (scale);
group "B:RF01GA" = B:RF01GA (table) alone. value_at(devs, group, t) gives
S * T(t - dtoff) + O, with S = 1 and O = 0 when those parts are missing; dtoff [us]
is the table's "dtoff" key (0 if absent; positive = later in the cycle, e.g. power
supply delay; negative for a readback that should be earlier). A group with no
table gives its "value" (role "value" or "setting") or 0. sample() is the raw lookup of one
table at its own time, without dtoff, S or O.

group/role are stored in the files. A device without them gets them once, when
loaded or added (add_group), from its contents and name: a ramp ("t"/"dt") is a "table", an
"sf" a "scale", a scalar "value" named "...O" an "offset", any other scalar a
"value"; the group is the name minus a matching T/S/O suffix, else the name itself
(so TFLAT, a scalar, is its own group). Set them by hand for irregular names.

Ramp tables are stored as the full 64 slots with trailing padding, where t stops
increasing (dt == 0). Use ramp(dev) to get the trimmed (t, values) pair.

File format (JSON):
    {
      "meta": {"created": "2026-09-29T21:30:00", "modified": "...", "notes": "..."},
      "devices": {"B:QL1T": {"kind": "Q", "group": "B:QL1", "role": "table",
                             "table": 1, "values": [...], ...}, ...}
    }
save() sets meta["created"] (if absent) and meta["modified"]; any other meta
keys (notes, source, ...) are kept as given.

Example:
    devs, meta = load("data_Booster/10_23_26_BMA.json")
    sxt = select(devs, kind="SX", role="table")
    add_device(devs, "B:HST01L", kind="position", value=1.0, unit="mm")
    meta["notes"] = "added HST01L survey offset"
    save(devs, "out.json", meta)
    t_us, v = ramp(devs["B:QL1T"])
    i_ql1 = value_at(devs, "B:QL1", 5000.0)     # S * T(t - dtoff) + O
    devs["B:QL1T"]["t"] += 100.0                # shift a ramp by 100 us
"""

import json
from datetime import datetime
from pathlib import Path
from typing import Any

import numpy as np
from numpy.typing import ArrayLike

TIME_UNIT = "us"

Device = dict[str, Any]          # one device: {"kind": ..., "values": ..., "t": ...}
Devices = dict[str, Device]      # devices keyed by full name, e.g. "B:QL1T"
Meta = dict[str, Any]


def _is_numeric_list(x):
    return isinstance(x, list) and len(x) > 0 and all(
        isinstance(v, (int, float)) and not isinstance(v, bool) for v in x
    )


def _to_json(x):
    """json.dump fallback for numpy types."""
    if isinstance(x, np.ndarray):
        return x.tolist()
    if isinstance(x, np.generic):
        return x.item()
    raise TypeError(f"Cannot serialize {type(x).__name__}")


def load(path: str | Path) -> tuple[Devices, Meta]:
    """Load (devices, meta) from JSON. Numeric lists become numpy arrays.

    Devices with "dt" but no "t" get "t" (see add_t); devices without "group"/"role"
    get them (see add_group).
    """
    with open(path) as f:
        data = json.load(f)
    devs, meta = data["devices"], data.get("meta", {})
    for dev in devs.values():
        for key, val in dev.items():
            if _is_numeric_list(val):
                dev[key] = np.array(val, dtype=float)
    for name, dev in devs.items():
        add_t(dev)
        add_group(name, dev)
    return devs, meta


def save(devs: Devices, path: str | Path, meta: Meta | None = None, indent: int = 2) -> None:
    """Save devices and meta to JSON. numpy arrays are written as lists.

    meta is updated in place with "created" (if absent) and "modified" timestamps.
    """
    if meta is None:
        meta = {}
    now = datetime.now().isoformat(timespec="seconds")
    meta.setdefault("created", now)
    meta["modified"] = now
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w") as f:
        json.dump({"meta": meta, "devices": devs}, f, indent=indent, default=_to_json)


def add_device(devs: Devices, name: str, **fields: Any) -> Device:
    """Add a device, or merge new/updated keys into an existing one.

    Given "dt" and no "t", the device gets t = cumsum(dt) (see add_t); without
    "group"/"role" it gets them from its contents and name (see add_group).
    """
    dev = devs.setdefault(name, {})
    dev.update(fields)
    return add_group(name, add_t(dev))


def select(devs: Devices, kind: str | list[str] | None = None, **match: Any) -> Devices:
    """Return the devices whose kind (str or list of str) and other keys match.

    select(devs, kind="SX", role="table")
    select(devs, kind=["SQ", "SS"], role="offset")
    select(devs, group="B:BFLAT")
    """
    kinds = {kind} if isinstance(kind, str) else set(kind) if kind is not None else None
    out = {}
    for name, dev in devs.items():
        if kinds is not None and dev.get("kind") not in kinds:
            continue
        if any(dev.get(k) != v for k, v in match.items()):
            continue
        out[name] = dev
    return out


def kinds(devs: Devices) -> list[str]:
    """Sorted list of all kind codes present."""
    return sorted({dev["kind"] for dev in devs.values() if "kind" in dev})


def add_t(dev: Device) -> Device:
    """If the device has "dt" but no "t", add t = cumsum(dt) [us]. In place.

    t[i] is the time of values[i], measured from the cycle start (t[0] = dt[0]).
    A device that already has "t" is left alone.
    """
    if "dt" in dev and "t" not in dev:
        dev["t"] = np.cumsum(np.asarray(dev["dt"], dtype=float))
    return dev


def ramp(dev: Device) -> tuple[np.ndarray, np.ndarray]:
    """Return (t, values) for a ramp table, with the trailing padding removed.

    Padding is the trailing points where t stops increasing (dt == 0 in the
    raw tables). t is in microseconds and t[i] is the time of values[i].
    A device with "values" None (no data, e.g. B:RF21GA) gives values of 0.
    """
    t = np.asarray(dev["t"], dtype=float)
    values = np.zeros_like(t) if dev.get("values") is None else np.asarray(dev["values"], dtype=float)
    rising = np.flatnonzero(np.diff(t) > 0)
    n = rising[-1] + 2 if len(rising) else min(len(t), 1)
    return t[:n], values[:n]


def sample(devs: Device | Devices, t: ArrayLike) -> float | np.ndarray | dict[str, float | np.ndarray]:
    """Linearly interpolate ramp table(s) at time t (us, scalar or array).

    Raw lookup of the table itself: no dtoff, scale factor or offset (use value_at
    for the device value).

    devs is one device dict or a dict of devices (e.g. from select()). Before the
    first ramp point it returns values[0]; after the last it returns values[-1].
    A device with a scalar "value" and no "t" (an offset) returns that value.
    A device with "values" None (no data) returns 0.

    sample(devs["B:QL1T"], 5000.0)                    -> float
    sample(select(devs, kind="QLT"), [0, 1e4, 2e4])   -> {name: array}
    """
    if any(k in devs for k in ("t", "dt", "values", "value")):
        if "t" in devs:
            return np.interp(t, *ramp(devs))
        return np.full(np.shape(t), float(devs["value"]))[()]
    return {name: sample(dev, t) for name, dev in devs.items()}


_SUFFIX = {"table": "T", "scale": "S", "offset": "O"}


def add_group(name: str, dev: Device) -> Device:
    """Fill in missing "group"/"role" keys of device `name` (in place; existing keys are kept).

    role: "table" if it has "t"/"dt" (a ramp in time), "scale" if it has "sf", "offset" if it
    has a scalar "value" and the name ends in "O", else "value".
    group: the name minus the role's suffix (T/S/O) if it ends in it, else the name.
    """
    role = dev.get("role")
    if role is None:
        if "t" in dev or "dt" in dev:
            role = "table"
        elif "sf" in dev:
            role = "scale"
        elif "value" in dev and name.endswith("O"):
            role = "offset"
        else:
            role = "value"
        dev["role"] = role
    if "group" not in dev:
        suffix = _SUFFIX.get(role)
        dev["group"] = name[:-1] if suffix and name.endswith(suffix) and len(name) > 1 else name
    return dev


def groups(devs: Devices) -> dict[str, dict[str, str]]:
    """{group: {role: device name}} for all devices with a "group" key.

    groups(bma)["B:BFLAT"] -> {"table": "B:BFLATT", "scale": "B:BFLATS"}
    Raises ValueError if two devices have the same group and role.
    """
    out: dict[str, dict[str, str]] = {}
    for name, dev in devs.items():
        if "group" not in dev:
            continue
        parts = out.setdefault(dev["group"], {})
        role = dev.get("role", "value")
        if role in parts:
            raise ValueError(f"group {dev['group']!r} has two {role!r} devices: {parts[role]}, {name}")
        parts[role] = name
    return out


def _group_value(devs: Devices, parts: dict[str, str], t: np.ndarray) -> float | np.ndarray:
    if "table" in parts:
        table = devs[parts["table"]]
        val = sample(table, t - float(table.get("dtoff", 0.0)))
    elif "value" in parts or "setting" in parts:
        val = sample(devs[parts.get("value", parts.get("setting"))], t)
    else:
        val = np.zeros(np.shape(t))
    if "scale" in parts:
        s = devs[parts["scale"]]
        val = val * (float(s["sf"]) if "sf" in s else sample(s, t))
    if "offset" in parts:
        val = val + sample(devs[parts["offset"]], t)
    return val[()] if isinstance(val, np.ndarray) else val


def value_at(devs: Devices, name: str | list[str], t: ArrayLike) -> float | np.ndarray | dict[str, float | np.ndarray]:
    """Value of device group(s) at time t [us]: S * T(t - dtoff) + O, linearly interpolated.

    name is a group ("B:QL1") or any device in it ("B:QL1T"); a list of names gives
    {name: value} (faster than separate calls: the groups are built once). Parts are
    found by their "group"/"role" keys (see the module docstring):
        table   sampled with sample() at t - dtoff (its "dtoff" [us], 0 if absent):
                positive puts the ramp later in the cycle (power supply delay); a
                readback given a negative dtoff is moved earlier. Outside the ramp
                it holds its first/last value. No table: the group's "value" (role "value"
                or "setting"), or 0.
        scale   its "sf" (else sampled like an offset); 1 if absent.
        offset  its "value" (sampled if it is a ramp); 0 if absent.

    value_at(bma, "B:QL1", 5000.0)              -> float
    value_at(bma, "B:BFLAT", [1800.0, 1900.0])  -> array (B:BFLATS sf * B:BFLATT)
    value_at(bma, ["B:QL1", "B:QS1"], 5000.0)   -> {name: float}
    """
    grp = groups(devs)
    t = np.asarray(t, dtype=float)

    def one(n: str) -> float | np.ndarray:
        g = n if n in grp else devs[n].get("group") if n in devs else None
        if g not in grp:
            raise KeyError(f"no device or group {n!r}")
        return _group_value(devs, grp[g], t)

    if isinstance(name, str):
        return one(name)
    return {n: one(n) for n in name}
