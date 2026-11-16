# Module reference

The Python modules in `helper_scripts/`: what each is for and its public functions and classes, from their docstrings (the full docstrings, with units and details, are in the code: `help(run_madx.prepare)`). Start with `quickstart.py` and the README; `examples.ipynb` works through every module.

Use them from the project folder:

```python
import sys
sys.path.insert(0, "helper_scripts")
import run_madx as rm
```

Units: time in µs, energy in GeV (kinetic unless noted), lengths in m, angles in rad, magnet currents in A, RF voltage in MV, frequency in MHz.

Modules: [`run_madx`](#run_madxpy), [`madx_input`](#madx_inputpy), [`device_data`](#device_datapy), [`ramp_timing`](#ramp_timingpy), [`tfs_reader`](#tfs_readerpy), [`lattice_mod`](#lattice_modpy), [`misalign`](#misalignpy), [`geometry`](#geometrypy), [`bpm_data`](#bpm_datapy), [`bpm_compare`](#bpm_comparepy), [`ping_analysis`](#ping_analysispy), [`painting`](#paintingpy), [`pip2_ramps`](#pip2_rampspy), [`timing_fit`](#timing_fitpy), [`lattice_plot`](#lattice_plotpy), [`curve_csv`](#curve_csvpy), [`ramp_read`](#ramp_readpy)

## `run_madx.py`

Run a Booster MAD-X lattice at given times along the ramp (one point, or a loop / scan).

Import: `import run_madx as rm` · command line: `python helper_scripts/run_madx.py -h`

- class `Config`: Everything that defines a run (Config.save / Config.load for a JSON copy).
  - `.save(path)`: Write this config as JSON.
  - `.load(path)`: Read a config written by save().
- class `Setup`: Everything prepare() loads once for a run or scan.
  - `.dke(t)`: Energy gain per turn [GeV/turn] at t [us] (turns computed once).
- class `Result`: One point: t [us], energies [GeV], RF mode, tunes, painting, and its twiss table.
  - `.row()`: Flat dict of the scalars (no twiss table), for a scan table.
- `load_devices(paths)`: One pool of devices from several JSON files; a name in more than one file raises ValueError.
- `input_names()`: Names the MAD-X inputs read that an override may add when the pool lacks them: the 288 currents, B:BFLAT, the extraction kickers (B:BEX2..4, MKSON), the RF voltages, the painting offsets and the ramp settings.
- `apply_overrides(pool, overrides)`: (devices with the overrides applied, names applied, names ignored); pool is not changed.
- `batch_override(devs, value, **match)`: Overrides {name: value} for every device matching dd.select(devs, **match), to add to Config.overrides. Without role: whole groups; with role: only those parts.
- `prepare(cfg)`: Steps A/B: load the devices once and read which files the lattice calls and writes.
- `output(setup, table='twiss')`: The last run's output table: "twiss", "sectormap_table", "survey", "survey_perm" (modified lattices; see setup.outputs).
- `run_point(setup, t)`: Steps C/D at time t [us]: write the inputs, run MAD-X (two-pass for PIP-II), read the tunes.
- `scan(setup, times, collect=None, verbose=True)`: run_point at each time [us]; returns a DataFrame with one row per point (Result.row(), plus collect(result) if given: read anything else from the tables there, e.g. res.twiss or output(setup, "sectormap_table"), before the next run overwrites them). A point where MAD-X fails gets nan tunes and an "error" column; the scan goes on.

## `madx_input.py`

Write MAD-X input (.dat) files from the device data.

Import: `import madx_input as mxin`

- `current_names()`: The 288 current groups ("B:HS1", "B:HL1", ...) in file order (I_ names without "B:").
- `field_at(devs, name, t, a=FIELD_A, L=FIELD_L)`: Value of a BMA device as seen by the beam at time t [us]: value_at with a kernel on its history.
- `missing_devices(devs)`: Current groups (current_names, then KICKERS) not in devs; they are written as 0.
- `phase_names(devs)`: Phase group (e.g. "RFA", "RFB") of cavities 1..N_RF from the RF_PHASE_MAP device's "cavities", or None if devs has no RF_PHASE_MAP. Raises ValueError if a cavity is missing.
- `off_cavities(devs)`: Cavity numbers with V = 0: the RF_PHASE_MAP device's "off" list (e.g. [21, 22]), else none.
- `rf_mode(devs)`: RF mode from the devices present: "off", "phase", "phase+volt" or "sum" (module docstring).
- `rf_values(t, devs, settings, mode=None, ke_source=None, dke=None)`: (volts [MV], lags [2 pi]) of cavities RF01..RF22 at time t [us], in RF mode `mode` (default rf_mode(devs); see the module docstring).
- `synchronous_phase(vals)`: Mean synchronous phase [deg] of an InputValues: the mean LAG of the cavities that are on, x 360.
- class `InputValues`: Everything written to COMMON_Booster_input.dat for one time t (from common_values).
- `common_values(t, devs, settings, ke_source=None, rf='auto', dke=None, beam_field=False)`: All values of the common MAD-X input for time t [us] (InputValues), without writing.
- `bflat_sf(devs)`: Scale factor of group B:BFLAT: its "scale" device's "sf" (B:BFLATS; 1.0 if none).
- `bflat_value(devs, t, beam_field=False)`: value_at(devs, "B:BFLAT", t) = sf (B:BFLATS) * B:BFLATT(t - dtoff) at t [us] (added to every I_HS, I_HL), or None if devs has no group B:BFLAT (a B:BFLAT override is a group without a table: its value). beam_field: field_at instead of value_at.
- `write_common(path, vals, note=None, overwrite=False)`: Write the values from common_values to the common MAD-X input file. Returns the path.

## `device_data.py`

Device settings stored as a dict of dicts, keyed by the full device name.

Import: `import device_data as dd`

- `load(path)`: Load (devices, meta) from JSON. Numeric lists become numpy arrays.
- `save(devs, path, meta=None, indent=2)`: Save devices and meta to JSON. numpy arrays are written as lists.
- `add_device(devs, name, **fields)`: Add a device, or merge new/updated keys into an existing one.
- `select(devs, kind=None, **match)`: Return the devices whose kind (str or list of str) and other keys match.
- `kinds(devs)`: Sorted list of all kind codes present.
- `add_t(dev)`: If the device has "dt" but no "t", add t = cumsum(dt) [us]. In place.
- `ramp(dev)`: Return (t, values) for a ramp table, with the trailing padding removed.
- `sample(devs, t)`: Linearly interpolate ramp table(s) at time t (us, scalar or array).
- `add_group(name, dev)`: Fill in missing "group"/"role" keys of device `name` (in place; existing keys are kept).
- `groups(devs)`: {group: {role: device name}} for all devices with a "group" key.
- `value_at(devs, name, t)`: Value of device group(s) at time t [us]: S * T(t - dtoff) + O, linearly interpolated.

## `ramp_timing.py`

Beam energy along the Booster ramp. Kinetic energy (KE) is the primary quantity.

Import: `import ramp_timing as rt`

- `ke_to_gamma(ke)`: Relativistic gamma from kinetic energy [GeV].
- `gamma_to_ke(gamma)`: Kinetic energy [GeV] from relativistic gamma.
- `ke_to_pc(ke)`: Momentum x c [GeV] from kinetic energy [GeV].
- `pc_to_ke(pc)`: Kinetic energy [GeV] from momentum x c [GeV].
- `ke_to_beta(ke)`: Relativistic beta = v/c from kinetic energy [GeV].
- `beta_to_ke(beta)`: Kinetic energy [GeV] from relativistic beta (nan for beta >= 1).
- `ke_to_brho(ke)`: Beam rigidity p/q [T m] from kinetic energy [GeV].
- `brho_to_ke(brho)`: Kinetic energy [GeV] from beam rigidity p/q [T m].
- `ke_to_frev(ke, circumference=CIRCUMFERENCE)`: Revolution frequency [MHz] = beta c / circumference [m], from kinetic energy [GeV].
- `frev_to_ke(frev, circumference=CIRCUMFERENCE)`: Kinetic energy [GeV] from revolution frequency [MHz] on a circumference [m].
- `ke_to_frf(ke, circumference=CIRCUMFERENCE)`: RF frequency [MHz] = HARMONIC * revolution frequency, from kinetic energy [GeV].
- `frf_to_ke(frf, circumference=CIRCUMFERENCE)`: Kinetic energy [GeV] from RF frequency [MHz] (= HARMONIC * frev) on a circumference [m].
- `convert(value, frm, to, circumference=CIRCUMFERENCE)`: Convert between any two of QUANTITIES, going through kinetic energy.
- `ramp_gmps(t, vimin, vimax, freq_pulse=15.0, t_offset=0.0)`: KE [GeV] at time t [us] on the plain GMPS sine.
- `time_gmps(ke, vimin, vimax, freq_pulse=15.0, t_offset=0.0)`: Inverse of ramp_gmps on the rising half: the time [us] at which KE = ke [GeV].
- `vimin_flat(viflat, vimax, t_flat, freq_pulse=15.0)`: vimin [GeV] for which ramp_gmps(t_offset + t_flat, vimin, vimax, ...) == viflat.
- `ramp_flat(t, viflat, vimax, t_flat, freq_pulse=15.0, t_offset=0.0)`: KE [GeV] at time t [us] on a GMPS ramp with flat injection. One pulse:
- `ramp_settings(devs)`: Ramp arguments from setting devices, e.g. data_Booster/11_03_26_MISC_MEV400.json.
- `turns_gmps(vimin, vimax, freq_pulse=15.0, t_offset=0.0, circumference=CIRCUMFERENCE, t_start=None, n_after=100, dt=0.25)`: Turn times [us] on ramp_gmps from t_start (default t_offset) to KE = vimax (t_offset + half a period), then n_after turns held at vimax.
- `turns_flat(viflat, vimax, t_flat, freq_pulse=15.0, t_offset=0.0, circumference=CIRCUMFERENCE, t_start=None, n_after=100, dt=0.25)`: Turn times [us] on ramp_flat from t_start (default t_offset - t_flat, the start of flat injection) to KE = vimax (t_offset + half a period), then n_after turns held at vimax.
- `device_curve(dev)`: (t [us], values) of a frequency device in cycle time: its trimmed table (dd.ramp) with t + dtoff ("dtoff" [us], 0 if absent), the same shift as dd.value_at = T(t - dtoff).
- `turns_device(dev, t_start=None, n_after=100, harmonic=HARMONIC, dt=0.25)`: Turn times [us] from a frequency device ("t" [us], "values" [MHz], "dtoff" [us]), e.g. the RF curve; its table is shifted by dtoff (device_curve).
- `ke_at(source, t, circumference=None, harmonic=HARMONIC)`: KE [GeV] at time t [us] from either source:
- `ke_gmps_at(settings, t)`: Effective KE [GeV] of the GMPS alone at time t [us], from a ramp_settings() dict.
- `turn_energy(source, t_start=None, n_after=100, circumference=None, harmonic=HARMONIC, dt=0.25)`: Turn times, KE per turn and energy gain per turn: (t_n, ke_n, dke_n).
- `dke_at(source, t, circumference=None, table=None)`: Energy gain per turn [GeV/turn] at time t [us]: dke_n of turn_energy(source) linearly interpolated in t, 0 before injection (t_n[0]) and after the last turn.

## `tfs_reader.py`

Reader for MAD-X TFS output tables (.outx / .tfs).

Import: `import tfs_reader`

- class `TfsTable`: A MAD-X TFS table held as a dict of numpy column arrays.
  - `.columns()`: Column names, in file order (e.g. ["NAME", "S", "BETX", ...]).
  - `.names()`: Element names (the NAME column, array of str), in row order.
  - `.idx(name)`: Row index of an element by name.
  - `.row(key)`: All column values at one row, given an element name or integer index.
  - `.to_dataframe()`: Return the table as a pandas DataFrame indexed by NAME.

## `lattice_mod.py`

Write the modified Booster lattice, madx_files/<stem>_modified.madx: the lattice plus the elements it does not have yet (unsurveyed extraction elements), the survey misalignments (on by default) and other edits. It runs with run_madx like the lattice itself (same input and output files).

Import: `import lattice_mod as lm` · command line: `python helper_scripts/lattice_mod.py -h`

- `read_elements(path=ELEMENTS_FILE)`: The element table: dicts with name, class, ref, offset_m [m], length_m [m] (floats), source.
- `sequence_span(lines)`: Line indices of the (only) SEQUENCE statement and its ENDSEQUENCE.
- `edit_lines(lines, elements, class_changes)`: The lattice lines with the elements inserted in AT order and the class changes applied (inside the sequence). Raises ValueError for a ref or a class-change element not in the sequence, or a name that is already there.
- `edit_factors(lines, factors)`: The lines with <NAME>_CAL = factor; before each element definition NAME (outside the sequence) and its KICK multiplied by <NAME>_CAL. Raises ValueError unless each NAME has exactly one definition with a KICK.
- `edit_rmatrix(lines)`: The lines with RMATRIX on every TWISS command and RE11..RE44 added to the twiss SELECT columns.
- `edit_misalign(lines, offsets)`: The lines with DX..DPSI [m, rad] (offsets {element: {dx..dpsi}}, zeros left out) appended to the sequence lines of those elements. Raises ValueError for an element not in the sequence or already misaligned.
- `edit_outputs(lines, tag='modified')`: The lines with the output files of WRITE, SURVEY and TRACK renamed, so the modified lattice does not overwrite the lattice's: <X>_Booster_<what> -> <X>_Booster_<tag>_<what> (e.g. MEV400_Booster_modified_twiss.outx), any other name -> <tag>_<name>. Input files (CALL) are not touched.
- `edit_perm_survey(lines)`: The lines with a second SURVEY after each SURVEY: PERM_ALIGN_SURVEY=TRUE, FILE <name>_perm.outx.
- `edit_track(lines, track=True, start=None)`: The lines without the TRACK ... ENDTRACK block (track False), or with SEQEDIT (FLATTEN, CYCLE to start, FLATTEN) and USE just before TRACK (start given), so only the tracking starts there.
- `make_modified(lattice, out=None, elements=ELEMENTS_FILE, class_changes=None, factors=None, rmatrix=False, misalign=True, misalign_overrides=None, perm_survey=True, track=True, track_start=None, outputs_tag='modified')`: Write the modified lattice (default <lattice stem>_modified.madx next to the lattice); returns its path. elements: table file (None = no new elements); class_changes default CLASS_CHANGES; factors default KICK_FACTORS of the lattice ({} = none); rmatrix: TWISS with RMATRIX and RE11..RE44 columns; misalign: a misalignment device file, True (default) = DEFAULT_MISALIGN of the lattice (none if not listed), None or False = none; with misalign_overrides (misalign.applied), which also writes the record <out stem>_misalign.json (an old record is removed when misalign is None); perm_survey: the second SURVEY with PERM_ALIGN_SURVEY=TRUE; track: keep the TRACK block; track_start: element to start tracking at; outputs_tag: the output files get it in their names (edit_outputs, default "modified": MEV400_Booster_modified_ twiss.outx, ...), so plain and modified runs keep their own tables; None = the lattice's names.

## `misalign.py`

Survey misalignments: the measured position of each surveyed device, as a device file, and the MAD-X DX..DPSI that lattice_mod writes into the modified lattice (from studies/survey_misalignments, 2026-11-13).

Import: `import misalign as ma` · command line: `python helper_scripts/misalign.py -h`

- `survey_to_madx(enz)`: MAD-X global (X, Y, Z) = (North, elevation, East) [m] of survey (East, North, elevation) point(s).
- `madx_to_survey(xyz)`: Survey (East, North, elevation) [m] of MAD-X global (X, Y, Z) point(s).
- `read_beamsheet(path=BEAMSHEET)`: {beamsheet device: {type, category, date, points {UP, CT, DN: [E, N, elevation] m}}}, in file order (the beamsheet CSV format: two header lines, Device, Type, DISTANCE, EAST, NORTH, ELEVATION, Comments, Date, ...; a _DDN point suffix is read as DN).
- `read_compare_hz(path=COMPARE_HZ)`: ({point name: [E, N, elevation] m} from the x, y, z columns, {CFM survey name ("1-1"): roll [mrad]}) of compareHandZ.csv. The roll of a magnet is the mean of its rows' roll column (blank or missing = 0). Roll sign: MAD-X DPSI of this lattice (+ turns x (outward) toward y (down)); to be checked against the survey's.
- `ideal_survey(lattice)`: SURVEY table of the lattice as it is (no misalignments), from a MAD-X run in a temporary folder (the lattice without its TRACK block and the files it CALLs copied from its folder); madx_files/ is not touched. ~1.5 s.
- `frames(survey)`: {element: (entrance frame, exit frame)} from a SURVEY table (one row per element at its exit; the entrance is the previous row's exit).
- `survey_devices(names, cfm_source='compareHandZ')`: {lattice device name: {kind, category, survey_name, date, points {UP, CT, DN: [E, N, elevation] m}, up, dn (the lattice markers of the surveyed UP/DN), children, magnetic_center_offset_m, roll_mrad}} of the surveyed devices that are in the sequence names. cfm_source: "compareHandZ" (default, 2026) or "beamsheet".
- `fit_global(survey, devices)`: Rigid motion (no scale) of the ideal ring that best fits the surveyed CFM UP/DN points (after PHYS_CENTER). Optional (off by default since 2026-11-13).
- `measure(lattice, cfm_source='compareHandZ', global_fit=False)`: {"meta", "devices"} of a new device file (make_misalign_file): the measured offsets, flags and frames of every surveyed device in the lattice, S (0 for DEFAULT_LEFT_OUT, else 1), O = 0, note.
- `make_misalign_file(lattice, out, cfm_source='compareHandZ', global_fit=False)`: Write the device file (measure) for the lattice (its element names and ideal survey); returns its path. Re-make it when the survey data or the lattice geometry change.
- `load(path)`: The devices of a device file: {name: record} with "frame", "measured_frame" and the children's frames as geometry.Frame (the meta: load_meta).
- `load_meta(path)`: The meta of a device file or a record file.
- `batch(devs, value, kind=None, component=None, part=None, include_left_out=False)`: Overrides for a whole family: kind (CFM, BMA, OTHER; None = all); component (of KEYS; None = the whole device); part None (the value replaces), "S" (the scale; no component) or "O" (the offset of a component). Values [m or rad, or the scale]. Devices left out in the file (S = 0) are skipped unless include_left_out, so e.g. a family scale does not bring BMAL3 back; name them in an override to include them.
- `applied(devs, overrides=None)`: {device: {dx..dpsi}} applied offsets [m, rad]: S x measured + O with the overrides (module docstring); a whole-device override sets every component first, component overrides then replace single ones. Raises KeyError for an override of an unknown device and ValueError for a malformed one.
- `element_offsets(devs, overrides=None)`: {sequence element: MAD-X DX..DPSI (keys dx..dpsi) [m, rad]} for the children of every device with a nonzero applied offset: the device's rigid motion about its ideal centre seen from each child's entrance frame.
- `write_record(path, devs, overrides, source, lattice)`: Write the record of a misaligned lattice (meta: device file, overrides, lattice; "devices": applied offsets; "elements": the element offsets written into the lattice) and return the element offsets.
- `bpm_offsets(record)`: {element: (DX, DY) [m]} of every misaligned element in a lattice's record file (the BPM reading is the twiss X, Y minus these at a MONITOR).

## `geometry.py`

3D frames for the MAD-X survey and rigid-body misalignments.

Import: `import geometry as g`

- `rot_theta(a)`: Theta(a): rotation about the local y axis (+a turns s toward +x).
- `rot_phi(a)`: Phi(a): rotation about the local x axis (+a turns s toward +y).
- `rot_psi(a)`: Psi(a): rotation about the local s axis (roll).
- `w_matrix(theta, phi, psi)`: W = Theta(theta) Phi(phi) Psi(psi) [rad]: columns = local x, y, s in global coordinates.
- `angles(w)`: (theta, phi, psi) [rad] with w_matrix(theta, phi, psi) = w, phi in [-pi/2, pi/2].
- class `Frame`: A local frame: origin o [m] (global X, Y, Z) and axes w (columns x, y, s).
  - `.to_global(p)`: Global position [m] of local coordinates p [m].
  - `.to_local(q)`: Local coordinates [m] of the global position q [m].
  - `.moved(rigid)`: This frame carried by the rigid motion (both are in global coordinates).
- class `Rigid`: Rigid motion in global coordinates: q -> r q + t (r [3x3], t [m]).
  - `.identity()`: No motion.
  - `.apply(q)`: Image of the global position(s) q [m] (shape (3,) or (n, 3)).
  - `.then(other)`: This motion followed by other.
  - `.inverse()`: The inverse motion.
- `body_rigid(frame, offsets)`: Global rigid motion of a body whose offsets are given in its own frame (about the frame origin): offsets dx, dy, ds [m] along the frame's x, y, s and dtheta, dphi, dpsi [rad] (Theta Phi Psi, like MAD-X), missing = 0. The body point at local p goes to frame.o + frame.w (d + R p).
- `offsets_in(frame, rigid)`: The rigid motion as offsets about this frame (the inverse of body_rigid): for an element's entrance frame these are its MAD-X DX, DY, DS [m], DTHETA, DPHI, DPSI [rad].
- `fit_rigid(src, dst, weights=None)`: Least-squares rigid motion (Kabsch, no scale) taking the points src (n, 3) [m] onto dst (n, 3).
- `line_frame(up, dn, up_vector=np.array([0.0, 1.0, 0.0]), y_sign=1.0)`: Frame of a straight body from two points [m]: origin at the midpoint, s from up to dn, x horizontal (perpendicular to s and to up_vector, global elevation by default), no roll; y = s x x ... chosen so the frame is right-handed (x, y, s) with y along +up_vector when y_sign = 1, -up_vector when y_sign = -1 (x flips with it).

## `bpm_data.py`

BMA BPM turn-by-turn data as devices, a turn-timing device, and a JSON format for both (2026-10-25).

Import: `import bpm_data as bd` · command line: `python helper_scripts/bpm_data.py -h`

- `bpm_elements()`: (MAD-X element, "L"/"S", sector n) of the 48 BMA BPMs, sector 1..24, L before S.
- `acnet_name(element, plane)`: ACNET device name of a BMA BPM element ("BMAL4_BPM") for plane "x" or "y" (ACNET_NAME, or ACNET_EXCEPTIONS: BMAL3_BPM is B:HSTU3L / B:VSTU3L).
- `bpm_device(values, plane, begin_turn=0, turn_offset=0, unit='mm', timing=TIMING, **fields)`: A BPM device: values[m] [unit] is turn begin_turn + turn_offset + m (nan = no reading); plane "x"/"y". Extra fields (cycle, timestamp, source, ...) are added.
- `timing_device(source, t0=TURN0, n_after=100, note=None)`: Turn-timing device: turn n (values) at cycle time t[n] [us] with KE ke[n] [GeV], turn 0 at t0, from rt.turn_energy(source, t_start=t0) (source: ramp_settings dict or a frequency device). note -> "source".
- `add_timing(devs, source, t0=TURN0, source_note=None, name=TIMING)`: Add the timing device (timing_device) as devs[name] and point every BPM device at it. In place.
- `bpm_devices(devs)`: The BPM devices (kind "BPM") of devs.
- `set_turn_offsets(devs, offsets)`: Set "turn_offset" of the named BPM devices (e.g. {"B:HST24L": -1}). In place.
- `turns(dev)`: Turn number of each reading: begin_turn + turn_offset + m.
- `turn_time(timing, turn)`: Cycle time [us] of turn number(s) (integers) from a timing device; nan outside it.
- `times(dev, devs)`: Cycle time [us] of each reading of a BPM device, from its timing device in devs (turn offsets applied); nan for turns past the timing device.
- `turn_at(timing, t)`: (Fractional) turn number at cycle time t [us]; nan outside the timing device.
- `index_at(dev, devs, t)`: Index into dev["values"] of the turn running at time t [us] (the last turn started at or before t).
- `planes(devs)`: {"x": (48, n), "y": (48, n)} in ring order (BPM sign, mm), columns = turns begin_turn, begin_turn + 1, ... with each BPM's turn_offset applied (shifted, nan-padded); a missing BPM is all nan. Also "begin_turn" (turn number of column 0, the most common one).
- `read_tbt(path, first_turn=0)`: Read a turn-by-turn BPM text file (blocks "***NEW BPM**", name, "NUMBER OF TURNS n BEGIN TURN: b CYCLE: c", MICROTIMESTAMP, GPS TIME, n, then n positions [mm]) into BPM devices (no timing device: add_timing). NO_READING, exact 0 and turns before first_turn -> nan. Plane from the name (B:H.. x, B:V.. y).
- `add_device_notes(devs)`: Set "note" from DEVICE_NOTES on the BPM devices present (in place); returns devs.
- `read(path)`: BPM devices from a file: a .json written by save (with its timing device, if saved), else a TBT text file (read_tbt).
- `save(devs, path, meta=None)`: Write devices (BPM and timing, any device_data device) to JSON, one device per line, arrays last. BPM nan -> NO_READING. meta gets "created"/"modified" (as dd.save).
- `load(path)`: Read a file written by save (or any device_data file): dd.load, then BPM NO_READING -> nan.

## `bpm_compare.py`

Compare the model closed orbit at the BMA BPMs with measured (turn-by-turn) BPM data.

Import: `import bpm_compare as bc`

- `model_orbit(twiss, bpm_offsets=None)`: Model closed orbit at the BMA BPMs from a TfsTable twiss (X, Y in m in the table), as the BPMs read it.
- `slow_orbit(dev, t, window=200.0, order=1, t_dev=None)`: Slow orbit of one BPM plane at time t [us] from turn-by-turn data (module docstring).
- `measured_orbit(devs, t, window=200.0, order=1, names=None)`: Slow orbit at time t [us] at every BMA BPM from measured devices (slow_orbit per plane; reading times from each device's timing device in devs, bd.times, or its own "t").
- `compare(model, measured)`: model_orbit and measured_orbit side by side: x_model, x_meas, x_err, dx_mm = model - measured (same for y), plus s, betx, bety, dx; ring order. df.attrs["rms"] = rms of dx_mm, dy_mm over the BPMs with data.
- `plot_compare(cmp, title=None, axes=None)`: Model and measured orbit along the ring, two panels (error bars on the measurement): x (+ outward) and -y (+ up; the model sign has y + down, BPM_SIGN). Returns the axes.

## `ping_analysis.py`

Tunes, coupling, phase advances and beta functions from pinged turn-by-turn BMA BPM data.

Import: `import ping_analysis as pa` · command line: `python helper_scripts/ping_analysis.py -h`

- `find_pings(X, period=None, first=None)`: Column index of the first kicked turn of every ping in the pinged plane's array X (48, n_turns). Kicks are at a regular spacing: period and first are estimated from the strongest jumps unless given; each kick is then the largest turn-to-turn change (median over BPMs) within +-SEARCH of its nominal index.
- `prepare(v)`: Missing points interpolated, then the second difference; None if more than MAX_MISSING is missing.
- `spectra(X)`: (freq, |FFT|) of every BPM's prepared signal; rows of dropped BPMs are nan; (None, None) if all dropped.
- `refine(f, A, i)`: Peak position from a parabola through bins i-1, i, i+1 of A.
- `peaks(f, A, n=2, near=None)`: The n largest local maxima of A in (FMIN, 0.5) (or within 2 MATCH of `near`): [(q, height)], largest first.
- `eigentunes(h, k, e)`: Modes A, B of one window [k, e) of both planes h["x"], h["y"]: Q, amplitudes in H and V (average |FFT|), coupling (A: V/H, B: H/V), half-window spread dQ (second half - first half).
- `tune_table(data, timing=None, noping=None)`: One row per ping of each ping file: data = {"h": planes(...), "v": planes(...)} (either may be missing). timing (bpm_data timing device) gives t_ms; noping (planes of the no-ping file) gives snr_A/B against the same windows, else snr is the peak / median of the spectrum.
- `ping_phases(X, k, e, q)`: Per BPM (nan if dropped): mu [rad] = -arg C at q, its error [rad] (residual rms / (|C| sqrt(N/2))), amplitude [mm] = |C| / (1 - cos 2 pi q).
- `advances(mu, Q)`: Phase advances [rad] BPM i -> i+1 in the given order; the last one wraps to the first (+ 2 pi Q).
- `model_optics(twiss)`: Model phase [rad, continuous from BMAL1], beta and dispersion at the BPMs in ring order, both planes, and the tunes, from a MAD-X twiss table. MAD-X's phase starts at the sequence start, so the phase is rebuilt from the advances taken in MAD-X's own order. With RE11..RE44 in the table (TWISS, RMATRIX) also the eigenmodes named by character (model_modes): Qx, Qy, hv_x, hv_y, xshare_x, xshare_y.
- `model_modes(twiss)`: The two eigenmodes of the model's one-turn 4x4 matrix (RE11..RE44 of the last row = the map from the sequence start), named by character like the measured modes A/B in tune_table: each eigenvector is carried to the 48 BPMs, and the mode with the larger H/V amplitude ratio (mean |x| / mean |y|) is Qx, the other Qy. Returns Qx, Qy (tunes between 6.5 and 7), hv_x, hv_y (H/V amplitude ratios) and xshare_x, xshare_y (the x share of each mode's action, a_x / (a_x + a_y) with a_p = Im(conj(p) p'), mean over the BPMs: 1 = pure x, 0 = pure y). MAD-X's Q1/Q2 already follow the character (Q1 = Qx at all 120 points checked, 2026-10-27).
- `model_advances(mu, Q)`: Advances [rad] in ring order from MAD-X phases mu [rad] that start at the sequence start: taken in phase order, with the wrap where the sequence ends.
- `wrap(a)`: Angle(s) to (-pi, pi].
- `fit_turn_offsets(dev, q, m_range=2, max_frac=0.25)`: Integer turn offsets per BPM from phase deviations dev (n_ping, n_bpm) [rad] (measured - reference, with each ping's common offset removed) and each ping's q: m minimizing sum wrap(dev + 2 pi q m)^2. An offset is kept only if the remaining rms deviation is below max_frac * 2 pi q (mean q) and better than m = 0; otherwise 0. Returns (m, remaining rms [rad]).
- `beta_from_phase(phi, phi_m, beta_m)`: Three-BPM beta at each BPM i from the advances to i+1 and i+2 (measured phi, model phi_m, model beta).
- `optics_table(data, tunes, model=None, snr_min=SNR_MIN, align=True)`: Per ping (snr of the pinged mode > snr_min) and BPM: phase, error, amplitude, advance to the next BPM; with model(t_ms) -> model_optics(...): model advance, betas from amplitude and phase, and (align) integer turn offsets fitted over all pings and planes (fit_turn_offsets), applied before the advances. Returns (optics, offsets): offsets has one row per BPM: the integer offset applied (turn_offset), the remaining rms deviation [deg], and phase_offset_turns = median phase deviation from the model in units of one turn's shift (2 pi q), before the correction, after taking out the tune mismatch and the common offset: ~integer for a turn offset, a fraction for e.g. a BPM that blends two turns.
- `plot_tunes(tunes, model=None, path=None)`: Eigentunes and coupling vs time (filled: the pinged mode). model: DataFrame t_ms, Q1, Q2 (optional).
- `plot_optics(optics, t_ms, path=None)`: For one h and one v ping (t_ms = {"h": ..., "v": ...}, nearest ping): advances measured vs model, the difference (mean +- spread over all pings), and beta from amplitude and phase vs model.

## `painting.py`

PIP-II injection: ORBUMP and painting magnet kick angles, and the injection .dat file.

Import: `import painting`

- `offsets_at(devs, t, overrides=None)`: {Y_ORBUMP, Y_PM, X_PM} [m] at time t [us]: overrides, else value_at(devs, name, t), else 0.
- `transfer(sectormap, start, end, plane)`: 2x2 transfer matrix from element `start` to element `end` (exit to exit), going round the ring if end comes before start. plane "x" (R11..R22) or "y" (R33..R44).
- `kick_angles(sectormap, y_orbump=0.0, y_pm=0.0, x_pm=0.0)`: {ANGLES name: kick [rad]} for offsets [m] at TREATYPOINT, from a sectormap table (<lattice>_sectormap.outx of the same optics). All 0 offsets give all 0 angles.
- `write_injection(path, angles=None, note=None)`: Write the injection .dat file (ORB_KICK_ANG, VPML.._ANG, HPMS.._ANG [rad]; missing = 0).

## `pip2_ramps.py`

PIP-II sample ramps from the MEV400 ramps, matched by beam rigidity (user's method, 2026-10-25).

Import: `import pip2_ramps as p2` · command line: `python helper_scripts/pip2_ramps.py -h`

- `t_inj(s400, s_pip)`: MEV400 cycle time [us] at which its ramp reaches the PIP-II injection KE.
- `map_time(t, s400, s_pip)`: PIP-II cycle time(s) [us] with the same Brho as MEV400 cycle time(s) t (t >= t_inj; nan before).
- `map_table(dev, s400, s_pip, keep_length=True, v_inj=None)`: A copy of a ramp table (t, values, dtoff) remapped to the PIP-II ramp (module docstring). v_inj: table value held through PIP-II flat injection (one breakpoint at its end); default the value at t_inj, and then only if breakpoints were dropped.
- `injection_value(bma, dev, s400, s_pip, t_ref=None)`: Table value that gives the device the MEV400 strength at t_ref (T_REF) at the PIP-II flat level: (I(t_ref) x Brho ratio - O) / S, with I = S * T + O of the device's group.
- `stretch_bflat(dev, s400, s_pip, keep_length=True)`: B:BFLAT table stretched about BDOT by t_flat(PIP-II) / t_flat(MEV400), current x Brho ratio of the flat levels (same BDOT on both).
- `pip2_bma(bma, s400, s_pip)`: PIP-II BMA devices: corrector tables (CORRECTOR_KINDS) remapped (map_table) with the MEV400 injection strength through PIP-II flat injection (injection_value), B:BFLAT stretched and scaled (stretch_bflat), everything else copied.
- `pip2_phases(phases, s400, s_pip)`: PIP-II RF phase devices: RFA, RFB remapped by beam Brho (all points), the rest copied.

## `timing_fit.py`

Timing of the GMPS ramp and the RF curves (user's method, 2026-10-25).

Import: `import timing_fit as tf` · command line: `python helper_scripts/timing_fit.py -h`

- `bflat_window(devs, group=BFLAT)`: (t_on, t_off) [us] of the nontrivial part of the group's table, in cycle time (+ its dtoff): from the last zero point before the first non-zero value to the first zero point after the last one.
- `flat_from_bflat(devs, group=BFLAT)`: {"BDOT": centre, "TFLAT": half length} [us] of bflat_window: the GMPS minimum and the half length of flat injection.
- `settings_with(misc, values)`: rt.ramp_settings(misc) with settings devices replaced by values (e.g. {"BDOT": .., "TFLAT": ..}).
- `fit_rf_offset(settings, freq_dev, window=WINDOW, scale=True, step=20.0)`: Fit the RF frequency curve's extra delay to the ramp.
- `phase_jump_time(dev, dtoff=None)`: Cycle time [us] of the largest step in a phase table (the transition jump), with dtoff (default: the device's own).
- `transition_time(settings, gamma_t)`: Cycle time [us] at which the ramp's gamma reaches gamma_t (rising half).
- `plot_timing(bma, settings, freq_dev, fit, path=None)`: B:BFLAT (with its delay) and the flat region; ramp vs the RF curve with the fitted offset; residual.

## `lattice_plot.py`

Optics and element layout of a region of the ring, from a MAD-X twiss table.

Import: `import lattice_plot as lp` · command line: `python helper_scripts/lattice_plot.py -h`

- `elements(twiss)`: Elements to draw, from a twiss table: dicts name, family, s0, s1 [m] (s1 = s0 for a thin BPM), by s0.
- `interpolate(twiss, step=0.05)`: Optics along s, at most `step` [m] apart: s, betx, bety [m], dx, dy [m] (dispersion in dp/p), x, y [mm].
- `region(twiss, start, end, pad=0.0)`: (s0, s1) [m] of a region, s0 < s1, s0 may be < 0: names or s, across the sequence start if start comes after end. pad [m] widens it on both sides.
- `plot_region(twiss, start, end, pad=None, step=0.05, labels=None, title=None, out=None, figsize=None, baseline=None, orbit=True)`: Element diagram, beta, dispersion and closed orbit between start and end.
- `draw_diagram(ax, twiss, s0, s1, labels=True, baseline=None, hide=())`: Element diagram and its key on ax, between s0 and s1 [m] (s0 < s1, s may be < 0). labels: element names. baseline: twiss table of the older lattice, elements also in it get a black hatch. hide: families not to draw (`STYLE` keys), e.g. ["H painting"].
- `plot_survey(survey, twiss, baseline=None, title=None, out=None, figsize=(10.5, 11))`: Map of the ring: the elements at their survey positions, East (MAD-X Z) against North (MAD-X X) [m], which is the survey beamsheet's frame (checked against its magnet centres, 2026-11-04).

## `curve_csv.py`

Reader for ACNET curve CSV exports (e.g. the RF frequency curve).

Import: `import curve_csv`

- `read_curve(path, t_start=1875.0, kind='RF', unit='MHz')`: Read a curve CSV. Returns (name, device). t_start is the time [us] of the first point.
- `set_start(dev, t_start)`: Move a curve device so its first point is at t_start [us]. Edits dev in place.

## `ramp_read.py`

Reads the current operational device settings from ACNET with pacsys (read only, it does not write to the control system) and writes the data_Booster files (<date>_BMA.json, <date>_MISC_MEV400.json, <date>_MISC_PIP_II.json, <date>_VRF_READBACK.json). A script, not a module to import; needs pacsys and ACNET access. Usage (its header): `python3 ramp_read.py '11_03_26' -m '[[1,1],[1,1],[1,1]]' -m '[[1,1],[1,1],[1,1]]' -v '[2,7]' -v '[1,1]'`.

