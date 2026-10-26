"""Reader for ACNET curve CSV exports (e.g. the RF frequency curve).

File layout (times in ms):
    CURVE             file type
    tblFreqCurveFF    curve name
    1                 header fields kept as-is in "header" (meaning unconfirmed;
    1000              the 4th line is the number of points, and the 7th matches
    1.000000          the "-75 us" delay in the filename)
    0.000000
    -75
    0.000000,37.942699    time [ms], value
    ...

read_curve() returns a device dict in the same form as the ramp tables
(values + t in us), so device_data.ramp() and sample() work on it.
The curve's time 0 is placed at t_start (us); time is t = t_start + t_csv.

Example:
    import device_data as dd
    from curve_csv import read_curve, set_start
    name, rf = read_curve("data_Booster/data_input/oper-freq-delayed-minus-75-us-20260914.csv")
    dd.sample(rf, 20000.0)                     # frequency [MHz] at 20 ms
    name, rf = read_curve(path, t_start=1800.0)
    set_start(rf, 1800.0)                      # or move an already loaded curve
    dd.save({name: rf}, "data_Booster/rf_freq_20260914.json", {"source": path})
"""

from pathlib import Path

import numpy as np

from device_data import Device

N_HEADER = 7


def read_curve(path: str | Path, t_start: float = 1875.0, kind: str = "RF", unit: str = "MHz") -> tuple[str, Device]:
    """Read a curve CSV. Returns (name, device). t_start is the time [us] of the first point."""
    path = Path(path)
    with open(path) as f:
        header = [next(f).strip() for _ in range(N_HEADER)]
    data = np.loadtxt(path, delimiter=",", skiprows=N_HEADER, ndmin=2)
    if header[0] != "CURVE":
        raise ValueError(f"{path}: expected 'CURVE' on line 1, got {header[0]!r}")
    if int(header[3]) != len(data):
        raise ValueError(f"{path}: header says {header[3]} points, found {len(data)}")

    t = t_start + np.round(data[:, 0] * 1000.0, 6)    # ms -> us
    return header[1], {
        "kind": kind,
        "unit": unit,
        "t_start": float(t_start),
        "values": data[:, 1],
        "t": t,
        "source": path.name,
        "header": header,
    }


def set_start(dev: Device, t_start: float) -> Device:
    """Move a curve device so its first point is at t_start [us]. Edits dev in place."""
    dev["t"] = np.asarray(dev["t"], dtype=float) + (t_start - dev["t_start"])
    dev["t_start"] = float(t_start)
    return dev
