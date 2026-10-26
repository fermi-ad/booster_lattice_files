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

- **Devices**: what the machine gives you, as JSON files in `data_Booster/`: ramp tables and offsets (BMA files), GMPS ramp settings and the extraction kickers `B:BEX2` / `MKSON` (`*_MISC_MEV400.json`, `*_MISC_PIP_II.json`), RF phases (`rf_phase_curves.json`), RF voltage readbacks (`10_23_26_VRF_READBACK_clean.json`). A device's value at time t is `S * T(t - dtoff) + O`. Times are in µs everywhere.
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
| RF on (voltage from the energy gain per turn) | add `rf_phase_curves.json` |
| RF on with the measured voltages | add `rf_phase_curves.json` and `10_23_26_VRF_READBACK_clean.json` |

The MISC file must match the lattice: `10_23_26_MISC_MEV400.json` or `10_23_26_MISC_PIP_II.json` (`prepare` stops if its injection energy is more than 10% from the lattice's `KE_INJ`). `B:VIMIN` and `B:VIMAX` are GMPS currents [A]: `B:VIMAX` is the current at the extraction energy `KEMAX` (7.988 GeV, fitted to the RF frequency curve) and the field is taken proportional to the current, so `B:VIMIN` gives the trough of the GMPS curve and the injection energy is the flat level after it (0.393 GeV MEV400, 0.790 GeV PIP-II for the 10_23_26 files). PIP-II painting turns on by itself for the PIP-II lattice (it runs MAD-X twice per point).

## File Structure

- `helper_scripts/`: the Python modules (import with `sys.path.insert(0, "helper_scripts")`). The ones marked CLI also run from the command line (`python helper_scripts/<name>.py -h`).
  - `run_madx.py` (CLI): the main entry point. Load device files and overrides (`Config`, `prepare`), run the lattice at a time (`run_point`) or over a list of times (`scan`).
  - `madx_input.py`: works out the MAD-X input values at a time (energies, RF voltages and phases, the 288 magnet currents) and writes `Common_Booster_input.dat`.
  - `device_data.py`: reads and writes the device JSON files; `value_at` gives a device's value at a time (`S * T(t - dtoff) + O`).
  - `ramp_timing.py`: beam energy along the ramp from the GMPS settings or an RF frequency curve, energy conversions (KE, pc, Brho, frequencies), turn times.
  - `timing_fit.py` (CLI): the timing bootstrap: BDOT and TFLAT from B:BFLAT, the RF curves' extra offset and `KEMAX` from a fit to the ramp (see Relative timing).
  - `pip2_ramps.py` (CLI): PIP-II sample BMA and RF phase ramps from the 400 MeV ones (see BMA ramps (PIP-II)).
  - `painting.py`: PIP-II ORBUMP and painting kick angles from the painting offsets.
  - `tfs_reader.py`: reads the MAD-X output tables (`.outx`).
  - `curve_csv.py`: reads ACNET curve CSV files (e.g. the RF frequency curve).
  - `bpm_data.py` (CLI): BPM turn-by-turn data as devices, the turn-timing device (time and energy of every turn), and the JSON format the BPM data is saved in; converts the turn-by-turn text files.
  - `bpm_compare.py`: model closed orbit at the BPMs vs the measured slow orbit at a time.
  - `ping_analysis.py` (CLI): tunes, coupling, phase advances and betas from pinged BPM data.
  - `ramp_read.py`: reads the device settings from ACNET with pacsys and writes the `data_Booster` files (needs pacsys).
- `madx_files/`: the two lattices, the `madx` binary, and the input and output files of the last run (overwritten on every run).
- `data_Booster/`: device data as JSON:
  - `10_23_26_BMA.json`: BMA ramp tables and offsets, B:BFLAT. `10_23_26_BMA_PIP_II.json`: PIP-II sample ramps.
  - `10_23_26_MISC_MEV400.json`, `10_23_26_MISC_PIP_II.json`: GMPS ramp settings (`B:VIMIN`, `B:VIMAX`, `BDOT`, `TFLAT`, `KEMAX`, ...) and the extraction kickers. One goes with each lattice.
  - `rf_phase_curves.json`, `rf_phase_curves_PIP_II.json`: RF phase curves. `rf_freq_20260914.json`: RF frequency curve. `10_23_26_VRF_READBACK_clean.json`: RF voltage readbacks.
  - `10_20_26_BPM_TBT_noping.json`, `..._hping.json`, `..._vping.json`: BPM turn-by-turn data (no ping, horizontal and vertical pings).
- `examples.ipynb`: worked examples for every module, one section each.
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
                devices=["data_Booster/10_23_26_BMA.json", "data_Booster/10_23_26_MISC_MEV400.json"])
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
                devices=["data_Booster/10_23_26_BMA.json", "data_Booster/10_23_26_MISC_MEV400.json"],
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
devices = ["data_Booster/10_23_26_BMA.json", "data_Booster/10_23_26_MISC_MEV400.json",
           "data_Booster/rf_phase_curves.json"]
lattice = "madx_files/MEV400_Booster_lattice.madx"
with_rf = rm.prepare(rm.Config(lattice=lattice, devices=devices))
no_rf = rm.prepare(rm.Config(lattice=lattice, devices=devices, rf="off"))
print(with_rf.rf_mode, no_rf.rf_mode)   # phase off
```

Near transition (about 19.5 ms for MEV400) the RF-on runs can fail or give large tune shifts. Comparing with RF off shows the difference (see Known limitations).

### PIP-II with painting

```python
cfg = rm.Config(lattice="madx_files/PIP_II_Booster_lattice.madx",
                devices=["data_Booster/10_23_26_BMA_PIP_II.json", "data_Booster/10_23_26_MISC_PIP_II.json"],
                overrides={"Y_ORBUMP": -0.097, "Y_PM": -0.016, "X_PM": 0.0061})   # [m]
res = rm.run_point(rm.prepare(cfg), 1900.0)
print(res.q1, res.q2, res.angles)
```

### Save a run setup

```python
cfg.save("runs/pip2_paint.json")        # lattice, devices, overrides, options
cfg = rm.Config.load("runs/pip2_paint.json")
```

The saved file also works on the command line (below).

### From the command line

```bash
python helper_scripts/run_madx.py --lattice madx_files/MEV400_Booster_lattice.madx \
    --devices data_Booster/10_23_26_BMA.json data_Booster/10_23_26_MISC_MEV400.json \
    --t 2400 10000
```

It prints one line per time:
```
madx_files/MEV400_Booster_lattice.madx: 587 devices, 0 overrides, RF mode off, painting off
t =    2400.0 us  KE_true = 0.3932 GeV  Q1 = 6.7530  Q2 = 6.8785  xi1 = -12.600  xi2 = -6.783
t =   10000.0 us  KE_true = 1.2258 GeV  Q1 = 6.9045  Q2 = 6.8592  xi1 = -9.966  xi2 = -5.971
```

More examples:

```bash
# RF on, skew quads off, BFLAT off, 3 times from 5 to 15 ms, table saved as CSV
python helper_scripts/run_madx.py --lattice madx_files/MEV400_Booster_lattice.madx \
    --devices data_Booster/10_23_26_BMA.json data_Booster/10_23_26_MISC_MEV400.json data_Booster/rf_phase_curves.json \
    --batch SQ=0 --override B:BFLAT=0 --linspace 5000 15000 3 --csv scan.csv

# PIP-II with painting offsets [m]
python helper_scripts/run_madx.py --lattice madx_files/PIP_II_Booster_lattice.madx \
    --devices data_Booster/10_23_26_BMA_PIP_II.json data_Booster/10_23_26_MISC_PIP_II.json \
    --override Y_ORBUMP=-0.097 Y_PM=-0.016 X_PM=0.0061 --t 1900

# From a saved setup (options given here override the file)
python helper_scripts/run_madx.py runs/pip2_paint.json --t 1900 2000
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
| `--csv FILE` | save the table |
| `--madx FILE` | a different MAD-X binary |

`python helper_scripts/run_madx.py -h` lists them all.

### Where the files go

MAD-X runs in place in `madx_files/`, like `./madx` there by hand. Each run overwrites:
- the inputs `Common_Booster_input.dat` (and `PIP_II_Booster_injection.dat` for PIP-II),
- the outputs `*_twiss.outx`, `*_sectormap.outx`, `*_survey.outx`, `*_trackone`, `sectormap`,
- `madx.log`, the MAD-X screen output. Check it when a run fails.

After a run they hold the last time point. From Python, `rm.output(setup, "sectormap_table")` or `rm.output(setup, "survey")` reads the other tables.

## Known limitations

What the model does not do yet, and how much it matters for your results. Numbers are for MEV400 with the 10_23_26 files unless noted. The details are in `ISSUES.md`; the full history is available from the author.

### Flipped lattice
The Booster is a CCW machine, generally following S.Y. Lee convention for magnets. However, MAD-X uses a CW coordinate system, the accelerator is effectively flipped upside down, so a '-y' position is 'up'. This has lead to some messiness with corrector conventions, but they should be correct in this case.

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

- **Bootstraped model timing** B:BDOT=0 is centered on the nonzero portion of B:BFLAT (+156 us): BDOT 2271 us, TFLAT 285 us (PIP-II same BDOT, TFLAT 400 us). The RF frequency and phase curves get an extra fitted offset of 155 us on top of the programmed 1875 us, and `KEMAX` is the top energy of the fitted RF frequency curve (7.988 GeV). `python helper_scripts/timing_fit.py --bma data_Booster/10_23_26_BMA.json --misc data_Booster/10_23_26_MISC_MEV400.json --same-bdot data_Booster/10_23_26_MISC_PIP_II.json --rf data_Booster/rf_freq_20260914.json --phases data_Booster/rf_phase_curves.json --plot plots/timing_fit.png` recalculates these (`--apply` writes them to the files).

### Closed orbit from the correctors
With the H/V correctors on, the model's closed orbit is large: up to 27-53 mm in x and 39-84 mm in y (rms 10-22 / 20-45 mm), largest at 20 ms in x and 3 ms in y. It comes from local bumps in the corrector tables that almost close, plus a small leftover that the tunes near 7 amplify. The real machine's orbit is much smaller, a few mm on the BPMs. The model has no misalignments or field errors, so the correctors have nothing to cancel and the tables alone make the large orbit. The measured tunes are also lower than the model's (by about 0.12 in x and 0.03 in y, from pings).

What it does to the optics: the orbit through the sextupoles changes the tunes and chromaticity.

| t | Q1, Q2 (correctors on - off) | chromaticity xi1, xi2 (on - off) |
|---|---|---|
| 3 ms | -0.005, +0.052 | -0.3, -4.8 |
| 14 ms | -0.002, +0.009 | +0.1, -0.3 |
| 25-30 ms | < 0.006 | < 0.7 |

Around 8 ms the differences look larger (up to 0.06), but there they mostly reflect when Q1 and Q2 swap labels due to strong coupling in the model (8.0-8.3 ms), which moves with the correctors.

Like the old Booster model, you currently should run once with the correctors off (`--batch H=0 V=0`, or `batch_override(pool, 0.0, kind="H")` and `kind="V"`).

Status: compared with measured BPM data (`helper_scripts/bpm_data.py`, `bpm_compare.py`, `ping_analysis.py`, notebook sections 7-9; details available from the author). Next is adding the survey alignment errors to the model (`ISSUES.md`).

### No radial position loop (RPOS)
In the machine, the radial loop adjusts the RF frequency so the beam stays at the design radius, on momentum. The model doesn't: with RF on, `FREQ_RF` follows the design ramp. The closed orbit from the correctors changes the orbit length, so the RF holds the beam slightly off momentum (by -(dC/C)/eta).

Away from transition this hardly matters: RF on vs off changes the tunes by less than 0.006. Near transition eta goes to 0, so the momentum offset blows up (next section).

What you can do: for the transverse optics, run with RF off (`--rf off`, or `rf="off"`); that is what an ideal radial loop would give.

Status: will be modelled once the measured radial offset curve is available.

### Transition
Transition is at about 19.5 ms for MEV400 and 14.5 ms for PIP-II. With RF on and the correctors on, runs near MEV400 transition give large tune shifts (up to 0.17 between 19 and 20.5 ms) or fail: in a check on 2026-10-23, 5 of 13 points between 19.4 and 21 ms failed. This is expected. Over transition the longitudinal dimension is not stable and the results of this feed back to the transverse dimension. However, as the transition region is only unstable for a short time, it is possible to pass this region in a way to prevent catastrophic losses. With the correctors off, RF on and RF off agree to 1e-5 right through transition.

The RF phase jump (from the phase curves) lands at 19.47 ms, 0.15 ms before the model's transition (19.61 ms), with the fitted RF offset (Relative timing).

PIP-II has no phase program of its own yet: PIP-II RF-on runs use the MEV400 phase curves, whose jump is at 19.47 ms, about 5 ms after PIP-II transition. Treat PIP-II RF-on results near and after transition as meaningless.

To study optics over transition, use RF off mode, or RF on with the correctors off. A failed point in a `scan` shows up as NaN with an `error` column; the scan carries on.

### Beam energy
The beam energy along the ramp comes from the GMPS currents: `B:VIMAX` is taken as `KEMAX` = 7.988 GeV (the top energy of the RF frequency curve) and the field as proportional to the current, and `B:VIMIN` as the trough, so injection is at 0.393 GeV (MEV400) and 0.790 GeV (PIP-II), slightly below the nominal 0.4 / 0.8. Measurement of extraction energy is required to improve these values.

If you do not know the extraction energy you can estimate it with a fit. To do so, take the beam energy from the RF frequency curve instead of the GMPS settings, load `data_Booster/rf_freq_20260914.json` and pass `ke_source="tblFreqCurveFF"` (`--ke-source tblFreqCurveFF`). Comparing the two gives an idea of the uncertainty.

### Painting (PIP-II)
The ORBUMP and painting kicks are worked out from the linear, uncoupled (2x2) sector maps. With the PIP-II sample ramps the bump at the injection point comes out right to about 0.25 mm (6.19 / -113.24 mm for 6.10 / -113.00 asked), and up to about 0.9 mm of it leaks into the rest of the ring (0.8% of the bump). The leak comes from the magnets' nonlinear fields on the displaced orbit, not from x-y coupling. This is considered good enough.

### BMA ramps (PIP-II)
PIP-II has no programmed ramps of its own. Sample ramps are made from the 400 MeV ramps by beam rigidity (`helper_scripts/pip2_ramps.py`): each breakpoint moves to the PIP-II time with the same Brho and keeps its current; through PIP-II flat injection the BMA settings have the same strength as the 400 MeV ones at injection (2000 us), then they interpolate to the Brho-matched settings; B:BFLAT is stretched to the PIP-II flat length and scaled with Brho; the RF phase curves are mapped the same way, so the phase jump lands at PIP-II transition. Load `data_Booster/10_23_26_BMA_PIP_II.json` and `data_Booster/rf_phase_curves_PIP_II.json` with the PIP-II lattice. These are a stand-in, not a machine program.

### RF voltage readbacks
Only used when you load `10_23_26_VRF_READBACK_clean.json` (RF mode `phase+volt`). RF readbacks are not well sampled and seem to sometimes give stale data. `B:RF01GA` reads about 20x the other stations and is divided by 20. That matches the others to about 2% over the ramp, but not at its ends (RF01 stays on for 1-2 ms after the others switch off); the cause is unknown. RF02, RF03 and RF11 read flat (~1.7 kV/turn) and look off. RF21 and RF22 have no data as they are overhead cavities which were deactivated. The time offset of the readbacks (`dtoff` 0) has not been checked. The default RF mode (`phase`, without the readback file) doesn't use any of this as these numbers are considered instructive rather than quantitative.

### RF Phase
The RF phase curves have an error in them that makes them very 'peaky' between 5 and 10 ms. Investigations are ongoing.

### DOGLEG, BEX, and MKS Extraction kickers
No As-found Surveys from the metrology department exist for extraction elements in Booster. Their position relative to other magnets are not well known. Installation and non metrology surveys are being investigated to find their true position. Once better information is found, these devices will be included in the lattice.

The BPMs next to them see optics the model does not have: BMAL3 sits between the two DOGLEG magnets and BMAL1 next to the old ORBUMP magnet (going away, it will not be added). The measured phase and beta there differ from the model.

### BPM turn alignment
The BPMs record turn by turn, but not all on the same turn count: in the 2026-10-20 data the sector 24 BPMs (L24, S24) are one turn off from the others, likely because the turn count starts between S23 and L24. This is set as `turn_offset` -1 on those devices in the data files and found from the ping phases (`ping_analysis.py`). Drift is possible so please turn alignment when using new datasets.

## Planned features

- Model optics vs the machine (closed orbit and tunes):
  1. Add magnet alignment errors from the survey and see whether they account for the tune error
  2. LOCO optics correction.
  3. A final effective correction to the devices for what is still missing
- DOGLEG, BEX and MKS extraction elements in the sequences, once their positions are known
- PIP-II sample ramps for the BMA correctors and the RF: done (`helper_scripts/pip2_ramps.py`), from the 400 MeV ramps by Brho
- Radial position loop (RPOS), once the radial offset curve is available

## More

- `examples.ipynb`: worked examples for every module, including plots.
- Module reference and data conventions: available from the author.
- `ISSUES.md`: open questions and decisions about the data.
- Full in-depth issue history: available from the author.
