# Booster lattice: run MAD-X at an arbitrary time with actual control system settings

This set of scripts are designed to allow for the following:
- PIP-II and 400 MeV lattices
- Model based on as found metrology surveys
- Closed with clean reference orbit for SURVEY
- Read out control system settings in the Booster
- Sequence which contains no nested sequences or lines
- Model Transverse and longitudinal dynamics
- Simple enough for non-experts to use
- Overrides to model changes to device parameters
- PIP-II ORBUMP and Painting

Python tools that write the MAD-X inputs for the FNAL Booster at a time in the cycle, run the lattice, and read the results. Two lattices: `MEV400_Booster_lattice.madx` (the current machine) and `PIP_II_Booster_lattice.madx` (PIP-II, with injection painting).

The flowchart of what the primary function `run_madx` does is the artifact "run_madx Flow Charts"; a copy is `run_madx_flowchart.html` in this folder (open it in a browser; it loads the Mermaid library from the internet).

## The ideas

- **Devices**: what the machine gives you, as JSON files in `data_Booster/`: ramp tables and offsets (BMA files), GMPS ramp settings and the extraction kickers `B:BEX2` / `MKSON` (`*_MISC_MEV400.json`, `*_MISC_PIP_II.json`), RF phases (`rf_phase_curves.json`), the measured RF sum voltage (`rfsum_smoothed.json`), RF voltage readbacks (`10_23_26_VRF_READBACK_clean.json`), and the survey misalignments (`11_13_26_MISALIGN_*.json`, used when making the modified lattice). A device's value at time t is `S * T(t - dtoff) + O`. Times are in µs everywhere.
- **Overrides**: constants that replace a device by name, e.g. `{"B:HL3": 0.0}`.
  - A whole device (`B:QL1`) replaces its full value. A part (`B:QL1O`, the offset) replaces only that part, and the table keeps ramping.
  - `B:HL3 = 0` turns off the corrector but leaves BFLAT in `I_HL3`. Override `B:BFLAT` to change BFLAT.
  - The painting offsets `Y_ORBUMP`, `Y_PM`, `X_PM` [m] are overrides too.
  - A name that matches nothing prints a warning and is skipped.
- **One run per time t**: `prepare` loads and checks everything once; `run_point` writes the inputs, runs MAD-X, and returns the tunes, chromaticity and twiss table.

Which files you load decides what the run does:

| You want | Device files |
|---|---|
| Optics along the ramp, no RF | a BMA file + the lattice's `*_MISC_*.json` |
| RF on (default for MEV400: measured RF sum, synchronous phase from the energy gain per turn) | add `rf_phase_curves.json` and `rfsum_smoothed.json` |
| RF on (voltage from the energy gain per turn) | add `rf_phase_curves.json` |
| RF on with the measured voltages | add `rf_phase_curves.json` and `10_23_26_VRF_READBACK_clean.json` |

The MISC file must match the lattice: `11_03_26_MISC_MEV400.json` for MEV400, `11_03_26_MISC_PIP_II.json` for PIP-II (`prepare` stops if its injection energy is more than 10% from the lattice's `KE_INJ`). `B:VIMIN` and `B:VIMAX` are GMPS currents [A]: `B:VIMAX` is the current at the extraction energy `KEMAX` (7.988 GeV, fitted to the RF frequency curve) and the field is taken proportional to the current, so `B:VIMIN` gives the trough of the GMPS curve and the injection energy is the flat level after it (0.394 GeV MEV400, 0.791 GeV PIP-II with the 11_03_26 files). PIP-II painting turns on by itself for the PIP-II lattice (it runs MAD-X twice per point).

## Coordinates

Three coordinate systems appear in the code and the data. MAD-X's frame is upside down (see Flipped lattice), so the vertical direction is always labelled.

| System | Axes | Where it shows up |
|---|---|---|
| Survey (site) | x (East), y (North), z (up) [m] | the survey files: the beamsheet's EAST (X), NORTH (Y), ELEVATION (Z), compareHandZ's x, y, z (hx, hy, h follow the curvature of the earth; not used) |
| Beam frame (MAD-X) | x̂ (+ outward), ŷ (− up), ŝ | twiss X, PX, Y, PY; the misalignments DX, DY, DS, DTHETA, DPHI, DPSI; corrector and kicker signs in the lattices |
| Shown to users | x (+ outward), −y (up), s | the plots in `quickstart.py` and `examples.ipynb`; BPM readings |

## File Structure

- `helper_scripts/`: the Python modules (import with `sys.path.insert(0, "helper_scripts")`). The ones marked CLI also run from the command line (`python helper_scripts/<name>.py -h`).
  - `run_madx.py` (CLI): the main entry point. Load device files and overrides (`Config`, `prepare`), run the lattice at a time (`run_point`) or over a list of times (`scan`).
  - `madx_input.py`: works out the MAD-X input values at a time (energies, RF voltages and phases, the 288 magnet currents) and writes `Common_Booster_input.dat`.
  - `device_data.py`: reads and writes the device JSON files; `value_at` gives a device's value at a time (`S * T(t - dtoff) + O`).
  - `ramp_timing.py`: beam energy along the ramp from the GMPS settings or an RF frequency curve, energy conversions (KE, pc, Brho, frequencies), turn times.
  - `timing_fit.py` (CLI): the timing bootstrap: BDOT and TFLAT from B:BFLAT, the RF curves' extra offset and `KEMAX` from a fit to the ramp (see Relative timing).
  - `pip2_ramps.py` (CLI): PIP-II sample BMA and RF phase ramps from the 400 MeV ones (see BMA ramps (PIP-II)).
  - `painting.py`: PIP-II ORBUMP and painting kick angles from the painting offsets.
  - `lattice_plot.py` (CLI): optics and element layout of a region of the ring, from a twiss file.
  - `tfs_reader.py`: reads the MAD-X output tables (`.outx`).
  - `curve_csv.py`: reads ACNET curve CSV files (e.g. the RF frequency curve).
  - `bpm_data.py` (CLI): BPM turn-by-turn data as devices, the turn-timing device (time and energy of every turn), and the JSON format the BPM data is saved in; converts the turn-by-turn text files.
  - `bpm_compare.py`: model closed orbit at the BPMs vs the measured slow orbit at a time.
  - `ping_analysis.py` (CLI): tunes, coupling, phase advances and betas from pinged BPM data.
  - `lattice_mod.py` (CLI): writes the modified lattices (`madx_files/<lattice>_modified.madx`): the estimated extraction elements, the survey misalignments, an optional tracking start.
  - `misalign.py` (CLI), `geometry.py`: the survey misalignments: survey data to a device file, selections (overrides), the MAD-X offsets of each element.
  - `ramp_read.py`: reads the device settings from ACNET with pacsys and writes the `data_Booster` files (needs pacsys).
- `madx_files/`: the two lattices, the `madx` binary, and the input and output files of the last run (overwritten on every run).
- `data_Booster/`: device data as JSON:
  - `11_03_26_BMA.json`: BMA ramp tables and offsets, B:BFLAT (read 2026-11-03). `11_03_26_BMA_PIP_II.json`: PIP-II sample ramps made from it.
  - `11_03_26_MISC_MEV400.json`, `11_03_26_MISC_PIP_II.json`: GMPS ramp settings (`B:VIMIN`, `B:VIMAX`, `BDOT`, `TFLAT`, `KEMAX`, ...) and the extraction kickers. One goes with each lattice.
  - `rf_phase_curves.json`, `rf_phase_curves_PIP_II.json`: RF phase curves. `rf_freq_20260914.json`: RF frequency curve. `10_23_26_VRF_READBACK_clean.json`: RF voltage readbacks.
  - `10_20_26_BPM_TBT_noping.json`, `..._hping.json`, `..._vping.json`: BPM turn-by-turn data (no ping, horizontal and vertical pings).
  - `rfsum_smoothed.json`: the measured RF sum voltage (RF mode `sum`, the default for MEV400).
  - `cleaned_beamsheet_Aug20_corrected.csv` / `.json`, `compareHandZ.csv`: survey data; `11_13_26_MISALIGN_MEV400.json`, `11_13_26_MISALIGN_PIP_II.json`: the misalignment device files made from them.
- `quickstart.py`: quick start without reading anything else: `python quickstart.py [t in us]` sets up the plain lattice and the modified lattice with the survey misalignments, runs both at t, reads the optics and survey tables and saves plots of the survey errors, the optics and the closed orbit (`plots/quickstart/`). Lines marked CHOOSE switch the lattice (MEV400 / PIP-II), the correctors, the misalignments and the plots.
- `examples.ipynb`: worked examples for every module, one section each.
- `MODULE_REFERENCE.md`: every module and its public functions and classes, one line each (from the docstrings).
- `ISSUES.md`: open issues and decisions.

## Dependencies

- Python 3.10 or newer (tested with 3.14) with numpy, scipy, pandas, matplotlib; jupyter for the notebook.
- MAD-X 5.08.01, included as `madx_files/madx` (Linux).
- `helper_scripts/ramp_read.py` needs pacsys and ACNET access.

## Running Instructions

```python
import sys
sys.path.insert(0, "helper_scripts")
import numpy as np
import run_madx as rm

cfg = rm.Config(lattice="madx_files/MEV400_Booster_lattice.madx",
                devices=["data_Booster/11_03_26_BMA.json", "data_Booster/11_03_26_MISC_MEV400.json"])
setup = rm.prepare(cfg)                 # load and check once
res = rm.run_point(setup, 10000.0)      # t = 10 ms
print(res.ke_true, res.q1, res.q2, res.chrom1, res.chrom2)
```

What a result holds:
- `res.q1`, `res.q2`: tunes. `res.chrom1`, `res.chrom2`: chromaticity dQ/(dp/p).
- `res.ke_true`: beam kinetic energy [GeV]. `res.ke`: the GMPS energy.
- `res.twiss`: the twiss table, e.g. `res.twiss["BETX"]` or `res.twiss.row("BMAL11_BPM")`.
- `res.inputs`: everything written for MAD-X, e.g. `res.inputs["I_QL1"]` [A] or `res.inputs["V01"]` [MV].
- `res.angles`: the painting kick angles (PIP-II only).

### A scan over time

```python
df = rm.scan(setup, np.linspace(2000, 35000, 12),
             collect=lambda res: {"betx_l11": res.twiss.row("BMAL11_BPM")["BETX"]})
df[["t", "ke_true", "q1", "q2", "chrom1", "chrom2", "betx_l11"]]
```

`scan` returns a pandas DataFrame with one row per time. MAD-X overwrites its output files on every run, so use `collect` to keep anything else from each point. A point where MAD-X fails gets NaN tunes and an `error` column, and the scan goes on.

### Overrides

```python
pool = rm.load_devices(cfg.devices)     # only needed for batch_override
cfg = rm.Config(lattice="madx_files/MEV400_Booster_lattice.madx",
                devices=["data_Booster/11_03_26_BMA.json", "data_Booster/11_03_26_MISC_MEV400.json"],
                overrides={"B:HL3": 0.0,      # corrector off (BFLAT stays)
                           "B:QL1O": 5.0})    # only the QL1 offset
cfg.overrides.update(rm.batch_override(pool, 0.0, kind="SQ"))                  # all skew quads at 0 A
cfg.overrides.update(rm.batch_override(pool, 0.0, kind="SX", role="offset"))   # only the sextupole offsets
setup = rm.prepare(cfg)
print(len(setup.overridden), "overrides applied; ignored:", setup.ignored)
res = rm.run_point(setup, 10000.0)
```

`batch_override(pool, value, kind=...)` gives one value for every device of a kind: `H`, `V` (correctors), `Q`, `SQ`, `SX`, `SS`. BFLAT has its own kind, so `kind="H"` leaves it alone.

### RF on and off

```python
devices = ["data_Booster/11_03_26_BMA.json", "data_Booster/11_03_26_MISC_MEV400.json",
           "data_Booster/rf_phase_curves.json", "data_Booster/rfsum_smoothed.json"]
lattice = "madx_files/MEV400_Booster_lattice.madx"
with_rf = rm.prepare(rm.Config(lattice=lattice, devices=devices))
no_rf = rm.prepare(rm.Config(lattice=lattice, devices=devices, rf="off"))
print(with_rf.rf_mode, no_rf.rf_mode)   # sum off
```

Near transition (about 19.5 ms for MEV400) the RF-on runs can fail or give large tune shifts. Comparing with RF off shows the difference (see Known limitations).

### PIP-II with painting

```python
cfg = rm.Config(lattice="madx_files/PIP_II_Booster_lattice.madx",
                devices=["data_Booster/11_03_26_BMA_PIP_II.json", "data_Booster/11_03_26_MISC_PIP_II.json"],
                overrides={"Y_ORBUMP": -0.097, "Y_PM": -0.016, "X_PM": 0.0061})   # [m]
res = rm.run_point(rm.prepare(cfg), 1900.0)
print(res.q1, res.q2, res.angles)
```

### From the command line

```bash
python helper_scripts/run_madx.py --lattice madx_files/MEV400_Booster_lattice.madx \
    --devices data_Booster/11_03_26_BMA.json data_Booster/11_03_26_MISC_MEV400.json \
    --t 2400 10000
```fus

It prints one line per time:
```
madx_files/MEV400_Booster_lattice.madx: 592 devices, 0 overrides, RF mode off, painting off
t =    2400.0 us  KE_true = 0.3936 GeV  Q1 = 6.6565  Q2 = 6.8242  xi1 = -14.782  xi2 = -5.402
t =   10000.0 us  KE_true = 1.2262 GeV  Q1 = 6.7845  Q2 = 6.8356  xi1 = -9.214  xi2 = -6.037
```

More examples:

```bash
# RF on, skew quads off, BFLAT off, 3 times from 5 to 15 ms, table saved as CSV
python helper_scripts/run_madx.py --lattice madx_files/MEV400_Booster_lattice.madx \
    --devices data_Booster/11_03_26_BMA.json data_Booster/11_03_26_MISC_MEV400.json data_Booster/rf_phase_curves.json data_Booster/rfsum_smoothed.json \
    --batch SQ=0 --override B:BFLAT=0 --linspace 5000 15000 3 --csv scan.csv

# PIP-II with painting offsets [m]
python helper_scripts/run_madx.py --lattice madx_files/PIP_II_Booster_lattice.madx \
    --devices data_Booster/11_03_26_BMA_PIP_II.json data_Booster/11_03_26_MISC_PIP_II.json \
    --override Y_ORBUMP=-0.097 Y_PM=-0.016 X_PM=0.0061 --t 1900
```

| Option | Meaning |
|---|---|
| `--lattice FILE` | MAD-X lattice |
| `--devices FILE ...` | device JSON files |
| `--t T ...` or `--linspace START STOP N` | times [µs] |
| `--override NAME=VALUE ...` | overrides (devices, parts, painting offsets) |
| `--batch KIND=VALUE ...` | every device of a kind, e.g. `H=0 V=0` for all correctors off |
| `--rf off` | run without RF even if RF devices are loaded |
| `--ke-source NAME` | take the beam energy from a frequency device, e.g. `tblFreqCurveFF` (load `rf_freq_20260914.json`) |
| `--beam-field` | magnet inputs are the field seen by the beam at t, eddy-current kernel on each current's history (`Config(beam_field=True)`) |
| `--csv FILE` | save the table |
| `--madx FILE` | a different MAD-X binary |

`python helper_scripts/run_madx.py -h` lists them all.

### The modified lattice (extraction elements and survey misalignments)
`helper_scripts/lattice_mod.py` writes `madx_files/<lattice>_modified.madx`: The modified lattice adds elements that are in the machine but not surveyed (BEX2-4, the L2 MKS kickers, old-style BPMs) and applies alignment errors. Survey errors are treated as MAD-X misalignments (DX, DY, DS, DTHETA, DPHI, DPSI on every surveyed device and the elements it holds). The misalignments are on by default, from `data_Booster/11_13_26_MISALIGN_MEV400.json` / `_PIP_II.json` (made by `helper_scripts/misalign.py` from the survey). Make the modified lattice once with the misalignments you want; after that the runs only change the input files. The plain lattice stays as it is, so you can compare the two: the modified lattice writes its own output files (`<lattice>_Booster_modified_twiss.outx`, `_survey.outx`, `_survey_perm.outx`, ...), so runs of the two don't overwrite each other.

```bash
python helper_scripts/lattice_mod.py madx_files/MEV400_Booster_lattice.madx                    # misalignments on (default)
python helper_scripts/lattice_mod.py madx_files/MEV400_Booster_lattice.madx --no-misalign      # extraction elements only
python helper_scripts/lattice_mod.py madx_files/MEV400_Booster_lattice.madx --mis-override BMAS12.dy=0 --mis-batch CFM.ds=0
```

```python
import lattice_mod as lm, misalign as ma
devs = ma.load("data_Booster/11_13_26_MISALIGN_MEV400.json")
lat = lm.make_modified("madx_files/MEV400_Booster_lattice.madx",          # rewrites madx_files/..._modified.madx
                       misalign_overrides={"BMAS12.dy": 0.0, **ma.batch(devs, 0.0, kind="CFM", component="ds")})
cfg = rm.Config(lattice=str(lat), devices=["data_Booster/11_03_26_BMA.json", "data_Booster/11_03_26_MISC_MEV400.json"])
```

Selecting misalignments: `NAME=0` leaves a device at its idealized lattice position, `NAME.dy=V` replaces one component, `NAME.S=V` scales the device by multaplicative factor (default 1), `NAME.dx.O=V` adds a constant offset to one component; `--mis-batch` (or `ma.batch`) does the same for a whole kind (`CFM`, `BMA`, `OTHER`). A few devices are left out by default (VIPM, HIPM, the septum can, ORBD_USBPM: their survey points are not on the beam axis); name them to put them in.

Units and directions of the misalignments: dx, dy, ds [m] and dtheta, dphi, dpsi [rad], in each device's own frame about its centre, in the beam frame (see Coordinates): dx along x̂ (+ outward), dy along ŷ (+ down, so −dy is up), ds along ŝ, dtheta yaw, dphi pitch, dpsi roll. With misalignments the model orbit at the BPMs (`bpm_compare.model_orbit`) is what the BPMs read: each BPM's own offset is subtracted (`x_beam`, `y_beam` keep the orbit itself).

### Where the files go

MAD-X runs in place in `madx_files/`, like `./madx` there by hand. Each run overwrites:
- the inputs `Common_Booster_input.dat` (and `PIP_II_Booster_injection.dat` for PIP-II),
- the outputs `*_twiss.outx`, `*_sectormap.outx`, `*_survey.outx`, `*_trackone`, `sectormap`; the modified lattices write `*_modified_twiss.outx`, `*_modified_survey.outx` and so on, plus `*_modified_survey_perm.outx` (MAD-X's survey with the misalignments; its translations are right, its rotations are not, so use it with care),
- `madx.log`, the MAD-X screen output. Check it when a run fails.

`lattice_mod.py` (and `quickstart.py`, which calls it) writes `madx_files/<lattice>_modified.madx` and its record `<lattice>_modified_misalign.json` (the misalignment selection and the offsets each element got). `quickstart.py` saves its plots in `plots/quickstart/`.

After a run they hold the last time point. From Python, `rm.output(setup, "sectormap_table")`, `rm.output(setup, "survey")` or `rm.output(setup, "survey_perm")` reads the other tables.

## Known limitations

What the model does not do yet, and how much it matters for your results. Numbers are for MEV400 unless noted (some come from the earlier 10_23_26 data read, which differs only slightly from the shipped 11_03_26 files). The details are in `ISSUES.md`.

### Flipped lattice
The Booster is a CCW machine, generally following S.Y. Lee convention for magnets. However, MAD-X uses a CW coordinate system, the accelerator is effectively flipped upside down, so a '-y' position is 'up'. This has lead to some messiness with corrector conventions, but they should be correct in this case.

The coordinate systems are in the Coordinates section (top).

### BMA field vs current
Eddy currents in the beam pipe delay the field the beam sees behind the magnet current, by about 300 µs / m on slow ramps (m = 1 dipole, 2 quadrupole, 3 sextupole), up to about 2 A from the current. The model has this: `Config(beam_field=True)` or `--beam-field` uses the field the beam sees instead of the current (`quickstart.py` uses it). The time constant was fitted for a dipole (a test-stand square pulse) and scaled to the higher orders as 1/m; with `B:BFLAT` it agrees with what is seen on the machine, so in most cases you want it on. It is off by default (the inputs are then the currents). Only the dipole response has been measured; the quadrupole and sextupole responses are the scaled ones. It moves the model orbit by about 1 mm rms (up to 5 mm at 3 ms).

### Relative timing
Booster devices watch for TCLK events, and trigger after a programmed delay. This means that in principle all devices are synchronized relative to one another. However, hardware delays from rise times, cable delays, processing times, and magnet response are not accounted for in these delays. This means that the actual delay in a signal being sent to the booster and a device changing state is not trivial. Below is a list of measured and inferred timing settings.

- **Injection time** Injection time measured to be 2007 us on oscilloscope
- **Turn 0** is set at exactly t=2 ms in control system
- **B:BDOT=0** GMPS should be in phase with RF, and is believed to be 210 us after injection, on oscilloscope
- **Digital LLRF** Programmed offset 1875 us
- **Analog LLRF** Programmed offset of 1875 us + tunable offset of ~90 us
- **BMA Response** BMA closed orbit bumps can be observed 156 us after signal breakpoint with a change in slope after 2.4 ms breakpoint observed at turn 251 (2.556 ms) on BPMs
- **B:BFLAT Teststand Response** Measurements of B:BFLAT field shows a response of ~200 us, consistent enough with BMA response

These oscilloscope measurements were taken in the control room, some delay is expected signals. This explains why values are not self consistent. Instead, we must 'bootstrap' the timing from what we know well. 

- **Bootstraped model timing** B:BDOT=0 is centered on the nonzero portion of B:BFLAT (+156 us): BDOT 2271 us, TFLAT 285 us (PIP-II same BDOT, TFLAT 400 us). The RF frequency and phase curves get an extra fitted offset of 155 us on top of the programmed 1875 us, and `KEMAX` is the top energy of the fitted RF frequency curve (7.988 GeV). `python helper_scripts/timing_fit.py --bma data_Booster/11_03_26_BMA.json --misc data_Booster/11_03_26_MISC_MEV400.json --same-bdot data_Booster/11_03_26_MISC_PIP_II.json --rf data_Booster/rf_freq_20260914.json --phases data_Booster/rf_phase_curves.json --plot timing_fit.png` recalculates these (`--apply` writes them to the files).

### Closed orbit from the correctors
With the H/V correctors on, the model's closed orbit is large: up to 27-53 mm in x and 39-84 mm in y (rms 10-22 / 20-45 mm), largest at 20 ms in x and 3 ms in y. It comes from local bumps in the corrector tables that almost close, plus a small leftover that the tunes near 7 amplify. The real machine's orbit is much smaller, a few mm on the BPMs. The plain lattices have no misalignments or field errors, so the correctors have nothing to cancel and the tables alone make the large orbit. The surveyed misalignments (in the modified lattices, see Survey misalignments) bring the model a little closer to the measured orbit (5 ms rms difference x 13.3 -> 9.8 mm, y 25.8 -> 17.2 mm) but do not explain it. The model's main magnet K1 carries a constant correction from the pings (`dK1F` -8.06e-3, `dK1D` -6.97e-3, 2026-11-06), which brings the tunes and the coupling onto the measurement at 6.5-18 ms; outside that range the tunes are still off by up to 0.07 (Qx high after transition).

What it does to the optics: the orbit through the sextupoles changes the tunes and chromaticity.

| t | Q1, Q2 (correctors on - off) | chromaticity xi1, xi2 (on - off) |
|---|---|---|
| 3 ms | -0.005, +0.052 | -0.3, -4.8 |
| 14 ms | -0.002, +0.009 | +0.1, -0.3 |
| 25-30 ms | < 0.006 | < 0.7 |

Around 8 ms the differences look larger (up to 0.06), but there they mostly reflect when Q1 and Q2 swap labels due to strong coupling in the model (8.0-8.3 ms), which moves with the correctors.

Like the old Booster model, you currently should run once with the correctors off (`--batch H=0 V=0`, or `batch_override(pool, 0.0, kind="H")` and `kind="V"`).

Status: compared with measured BPM data (`helper_scripts/bpm_data.py`, `bpm_compare.py`, `ping_analysis.py`, notebook sections 7-9; details available from the author). The survey alignment errors are in the modified lattices since 2026-11-13 (`ISSUES.md`).

### Magnet calibration
The main (combined-function) magnets carry a constant K1 correction fitted to the measured tunes (`dK1F` -8.06e-3, `dK1D` -6.97e-3, 2026-11-06). Studies are ongoing to calibrate all the devices: their strengths and their time responses (correctors, quadrupoles, sextupoles, extraction kickers).

### No radial position loop (RPOS)
In the machine, the radial loop adjusts the RF frequency so the beam stays at the design radius, on momentum. The model doesn't: with RF on, `FREQ_RF` follows the design ramp. The closed orbit from the correctors changes the orbit length, so the RF holds the beam slightly off momentum (by -(dC/C)/eta).

Away from transition this hardly matters: RF on vs off changes the tunes by less than 0.006. Near transition eta goes to 0, so the momentum offset blows up (next section).

What you can do: for the transverse optics, run with RF off (`--rf off`, or `rf="off"`); that is what an ideal radial loop would give.

Status: will be modelled once the measured radial offset curve is available.

### Transition
Transition is at about 19.5 ms for MEV400 and 14.5 ms for PIP-II. With RF on and the correctors on, runs near MEV400 transition give large tune shifts (up to 0.17 between 19 and 20.5 ms) or fail: in a check on 2026-10-23, 5 of 13 points between 19.4 and 21 ms failed. This is expected. Over transition the longitudinal dimension is not stable and the results of this feed back to the transverse dimension. However, as the transition region is only unstable for a short time, it is possible to pass this region in a way to prevent catastrophic losses. With the correctors off, RF on and RF off agree to 1e-5 right through transition.

The RF phase jump (from the phase curves) lands at 19.47 ms, 0.07 ms after the model's transition (19.39 ms with the constant K1 correction, correctors off), with the fitted RF offset (Relative timing). The measured RF sum shows transition at 19.33 ms.

PIP-II has no phase program of its own yet: PIP-II RF-on runs use the MEV400 phase curves, whose jump is at 19.47 ms, about 5 ms after PIP-II transition. Treat PIP-II RF-on results near and after transition as meaningless.

To study optics over transition, use RF off mode, or RF on with the correctors off. A failed point in a `scan` shows up as NaN with an `error` column; the scan carries on.

### Beam energy
The beam energy along the ramp comes from the GMPS currents: `B:VIMAX` is taken as `KEMAX` = 7.988 GeV (the top energy of the RF frequency curve) and the field as proportional to the current, and `B:VIMIN` as the trough, so injection is at 0.393 GeV (MEV400) and 0.790 GeV (PIP-II), slightly below the nominal 0.4 / 0.8. Measurement of extraction energy is required to improve these values.

If you do not know the extraction energy you can estimate it with a fit. To do so, take the beam energy from the RF frequency curve instead of the GMPS settings, load `data_Booster/rf_freq_20260914.json` and pass `ke_source="tblFreqCurveFF"` (`--ke-source tblFreqCurveFF`). Comparing the two gives an idea of the uncertainty.

### Painting (PIP-II)
The ORBUMP and painting kicks are worked out from the linear, uncoupled (2x2) sector maps. With the PIP-II sample ramps the bump at the injection point comes out right to about 0.25 mm (6.19 / -113.24 mm for 6.10 / -113.00 asked), and up to about 0.9 mm of it leaks into the rest of the ring (0.8% of the bump). The leak comes from the magnets' nonlinear fields on the displaced orbit, not from x-y coupling. This is considered good enough.

### BMA ramps (PIP-II)
PIP-II has no programmed ramps of its own. Sample ramps are made from the 400 MeV ramps by beam rigidity (`helper_scripts/pip2_ramps.py`): each breakpoint moves to the PIP-II time with the same Brho and keeps its current; through PIP-II flat injection the BMA settings have the same strength as the 400 MeV ones at injection (2000 us), then they interpolate to the Brho-matched settings; B:BFLAT is stretched to the PIP-II flat length and scaled with Brho; the RF phase curves are mapped the same way, so the phase jump lands at PIP-II transition. Load `data_Booster/11_03_26_BMA_PIP_II.json` and `data_Booster/rf_phase_curves_PIP_II.json` with the PIP-II lattice (made from the 11_03_26 files). These are a stand-in, not a machine program.

### RF voltage readbacks
Only used when you load `10_23_26_VRF_READBACK_clean.json` (RF mode `phase+volt`). RF readbacks are not well sampled and seem to sometimes give stale data. `B:RF01GA` reads about 20x the other stations and is divided by 20. That matches the others to about 2% over the ramp, but not at its ends (RF01 stays on for 1-2 ms after the others switch off); the cause is unknown. RF02, RF03 and RF11 read flat (~1.7 kV/turn) and look off. RF21 and RF22 have no data as they are overhead cavities which were deactivated. The time offset of the readbacks (`dtoff` 0) has not been checked. The default RF mode (`sum`, with `rfsum_smoothed.json`, see RF Phase) and the `phase` mode don't use any of this as these numbers are considered instructive rather than quantitative.

### RF Phase
The RF phase curves have an error in them that makes them very 'peaky' between 5 and 10 ms. Investigations are ongoing. Until then the default (RF mode `sum`, load `rfsum_smoothed.json` with the phase curves) puts the measured RF sum voltage, divided evenly over the 20 cavities that are on, and solves the synchronous phase from the energy gain per turn of the ramp; only the paraphase (RFA - RFB) and the transition jump come from the phase curves (examples.ipynb, sections 3 and 5). The RF sum is MEV400 data (one cycle, 2026-06-10).

### DOGLEG, BEX, and MKS Extraction kickers
No As-found Surveys from the metrology department exist for extraction elements in Booster. Their position relative to other magnets are not well known. Installation and non metrology surveys are being investigated to find their true position.

Until then, `helper_scripts/lattice_mod.py` writes a modified version of either lattice (`madx_files/<lattice>_modified.madx`) with estimated positions for BEX2 (L2), BEX3 (L3), BEX4 (L4) and the four L2 MKS kickers (`EXTRACTION_KICKER_L2_A..D`), taken from an archived lattice. The L12 extraction kicker becomes an MKS kicker as well. The modified lattice runs with `run_madx` like the lattice itself. It also carries the survey misalignments by default (see "The modified lattice" under Running Instructions):

```
python helper_scripts/lattice_mod.py madx_files/MEV400_Booster_lattice.madx [--rmatrix] [--no-misalign]
```

The BEX magnets are plain vertical kickers (their edge geometry is not known). Each BEX has its own current, `B:BEX2`, `B:BEX3`, `B:BEX4`: the same table with scale factors 1.1992, 0.9414 and 1.1992 (`data_Booster/11_03_26_MISC_*.json`, with the measured BEX delay of 258 us and the calibrated kicks). The MKS kickers run at `KV_MKS` = 48.75 kV each (an assumption) when `MKSON` = 1. `--rmatrix` also writes the one-turn matrix columns (RE11..RE44) to the twiss table.

The BPMs next to them see optics the model does not have: BMAL3 sits between the two DOGLEG magnets and BMAL1 next to the old ORBUMP magnet (going away, it will not be added). BMAL3 is misaligned on purpose in the modified lattice, 12 mm low, because the BMA package follows the beam's shift in the dogleg. It is applied even though the dogleg is not in the lattice: the dogleg runs at a constant current, so its bump does not scale with Brho, and the package acts like an unmisaligned element at low Brho and more like a real misalignment at high Brho. The measured phase and beta there differ from the model.

### Survey misalignments
The misalignments come from the beamsheet survey (corrected for its data errors) and, for the combined-function magnets, the 2026 survey; the two are different survey networks, which differ by about 2.5 mm around the ring. Roll is not measured yet (0). BMAL3 sits in the L3 vertical dogleg and is misaligned on purpose (see DOGLEG, BEX, and MKS); a few devices whose survey points are not on the beam axis are left out. The survey errors are not a calibration: they are individual imperfections of each element, which in the machine the ramps and offsets account for, not a change of the relative strength of the elements. They do not account for the tune error that the K1 correction fixes (at 5 ms they shift Q1 by about −0.01; the tune error before the correction was about 0.1 in x), and they do not account for the closed-orbit error of the model either: misaligned magnets steer the beam like dipole kicks, so they could have explained the difference between the model's corrector orbit and the measured orbit, but they bring the model only a little closer (at 5 ms the rms difference goes from 13.3 to 9.8 mm in x and from 25.8 to 17.2 mm in y; Closed orbit from the correctors). The rest looks like something else. Details: `ISSUES.md`; the survey study is available from the author.

### BPM turn alignment
The BPMs record turn by turn, but not all on the same turn count: in the 2026-10-20 data the sector 24 BPMs (L24, S24) are one turn off from the others, likely because the turn count starts between S23 and L24. This is set as `turn_offset` -1 on those devices in the data files and found from the ping phases (`ping_analysis.py`). Drift is possible so please turn alignment when using new datasets.

## Planned features

- Model optics vs the machine (closed orbit and tunes):
  1. LOCO optics correction.
  2. A final effective correction to the devices for what is still missing
- DOGLEG, BEX and MKS extraction elements: estimated positions in the modified lattice (`lattice_mod.py`); surveyed positions, DOGLEG, BEX edges and the MKS calibration still to come
- PIP-II sample ramps for the BMA correctors and the RF: done (`helper_scripts/pip2_ramps.py`), from the 400 MeV ramps by Brho
- Radial position loop (RPOS), once the radial offset curve is available

## More

- `quickstart.py`: the quick-start template (above).
- `MODULE_REFERENCE.md`: the modules and their functions, one line each.
- `examples.ipynb`: worked examples for every module, including plots.
- `ISSUES.md`: open questions and decisions about the data.
