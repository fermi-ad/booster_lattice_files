"""Reader for MAD-X TFS output tables (.outx / .tfs).

File layout:
    @ KEY  %fmt  value      header scalars
    * NAME S BETX ...       column names
    $ %s   %le %le ...      column types
    "ELEM"  0.0  1.23 ...   data rows

Example:
    t = TfsTable("PIP_II_Booster/PIP_II_Booster_twiss.outx")
    t.header["Q1"]
    i0, i1 = t.idx("MARK_L11_UP"), t.idx("MARK_L11_DN")
    plt.plot(t["S"][i0:i1 + 1], t["BETX"][i0:i1 + 1])
    t.row("MARK_L11_UP")["BETX"]
"""

import re
from pathlib import Path
from typing import Any

import numpy as np

# A quoted string (may contain spaces) or any run of non-whitespace.
_TOKEN = re.compile(r'"[^"]*"|\S+')


def _convert(token, fmt):
    """Convert a raw token to a Python value based on its TFS format code."""
    if fmt.endswith("s"):
        return token.strip('"')
    if fmt.endswith("d"):
        return int(token)
    return float(token)


class TfsTable:
    """A MAD-X TFS table held as a dict of numpy column arrays."""

    def __init__(self, path: str | Path) -> None:
        self.path = Path(path)
        self.header = {}
        self.data = {}
        self._index = {}

        columns, formats = None, None
        rows = []
        with open(self.path) as f:
            for line in f:
                if not line.strip():
                    continue
                marker = line[0]
                if marker == "@":
                    key, fmt, value = line[1:].split(maxsplit=2)
                    self.header[key] = _convert(value.strip(), fmt)
                elif marker == "*":
                    columns = line[1:].split()
                elif marker == "$":
                    formats = line[1:].split()
                else:
                    rows.append(_TOKEN.findall(line))

        if columns is None or formats is None:
            raise ValueError(f"{self.path}: missing '*' or '$' line")

        for j, (col, fmt) in enumerate(zip(columns, formats)):
            raw = [r[j] for r in rows]
            if fmt.endswith("s"):
                self.data[col] = np.array([v.strip('"') for v in raw])
            elif fmt.endswith("d"):
                self.data[col] = np.array(raw, dtype=int)
            else:
                self.data[col] = np.array(raw, dtype=float)

        if "NAME" in self.data:
            for i, name in enumerate(self.data["NAME"]):
                if name in self._index:
                    raise ValueError(f"{self.path}: duplicate element name {name!r}")
                self._index[name] = i

    @property
    def columns(self) -> list[str]:
        """Column names, in file order (e.g. ["NAME", "S", "BETX", ...])."""
        return list(self.data)

    @property
    def names(self) -> np.ndarray:
        """Element names (the NAME column, array of str), in row order."""
        return self.data["NAME"]

    def __len__(self):
        return len(self.names)

    def __getitem__(self, column: str) -> np.ndarray:
        """A column as a numpy array (numbers or str), e.g. t["BETX"]."""
        return self.data[column]

    def __contains__(self, column):
        return column in self.data

    def idx(self, name: str) -> int:
        """Row index of an element by name."""
        try:
            return self._index[name]
        except KeyError:
            raise KeyError(f"{name!r} not found in {self.path.name}") from None

    def row(self, key: str | int) -> dict[str, Any]:
        """All column values at one row, given an element name or integer index."""
        i = self.idx(key) if isinstance(key, str) else key
        return {col: arr[i].item() for col, arr in self.data.items()}

    def to_dataframe(self) -> "pandas.DataFrame":
        """Return the table as a pandas DataFrame indexed by NAME."""
        import pandas as pd

        return pd.DataFrame(self.data).set_index("NAME", drop=False)

    def __repr__(self):
        return f"TfsTable({self.path.name!r}, rows={len(self)}, columns={len(self.data)})"
