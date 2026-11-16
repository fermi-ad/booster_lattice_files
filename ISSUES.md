# Open questions and data issues

Release note: references to `studies/`, `plots/`, `ISSUES_ARCHIVE.md` and `issues_10_06_26.md` point to the author's
working copy (not in this release); they are available from the author.

This document records the known issues of the Booster lattice model and of its input data. It has four parts:
- **Open**: unresolved issues. Each states the problem as observed, the current understanding of it, and the
  questions that remain. New findings are added here.
- **Resolved**: decisions and fixes, newest first, each as a short record of what was decided.
- **Conventions**: definitions the code and the data files rely on.
- **Study findings**: summaries of the supporting studies behind the open issues.

Provenance: the complete record up to 2026-10-25, with all intermediate numbers, is in
`ISSUES_ARCHIVE.md`; the 2026-10-06 data-file details are in `issues_10_06_26.md`; the closed-orbit
study's details and figures are in `studies/BPM_CO_opt/closed_orbit_notes.pdf` and its scripts. Numbers are for
the MEV400 lattice with the 10_23_26 data files and the timing of 2026-10-25 (BDOT 2271 us, TFLAT 285 us, KEMAX
7.988 GeV) unless noted; all scripts were rerun with it on 2026-10-25.

## Open

### Closed orbit and optics: model vs machine
**Issue:** the model's closed orbit and tunes are not consistent with the measurement (BPM data of 2026-10-20,
cycle 17, `data_Booster/10_20_26_BPM_TBT_*.json`).
- With the H/V corrector tables on, the model's closed orbit is 27-53 mm (x) and 39-84 mm (y) at its largest
  over 2-30 ms; the measured orbit is a few mm (rms 4.5-7.4 mm x, 1.0-3.3 mm y).
- Before the constant K1 of 2026-11-06, model tunes were higher than measured (pings): by ~0.12 in x (mode A) and ~0.03 in y (mode B); see the optics correction entry.
- Strong coupling: in the model Q1 and Q2 swap labels at 8.0-8.3 ms with the correctors on (a label swap, not a
  crossing; plots keep MAD-X's Q1/Q2). With H and V off this is much less of an issue (no swap; the bare tunes
  cross near 6 ms). The measured modes stay ~0.05 apart all cycle, strongly coupled at 6-18 ms.

**Understanding:**
- The lattice is an idealized N-gon derived from the as-found survey with no misalignment or field errors. In
  the actual machine the correctors cancel those errors; in the model there is nothing to cancel, so the corrector
  tables alone make the large orbit.
- That orbit is local bumps that nearly close plus a small n = 7 leftover, made worse by the model tunes being
  near 7 (amplified ~5x). The large orbit excursions sample magnet nonlinearities and shifts the model's own tunes and
  chromaticity.
- The radial loop (on during the measurement) affects the closed orbit, but the beam can generally be
  transported without it, so it is not the primary issue.
- Survey misalignments (2026-11-13, in the modified lattices): they act like dipole kicks but close only part of the
  gap (5 ms, model - measured rms x 13.3 -> 9.8, y 25.8 -> 17.2 mm), so the orbit error is mostly something else; see
  "Survey misalignments" below.
- The thin kick corrections c + d u per location (`studies/optics_correction/`, "Optics correction" below) fit the
  measured orbit, but they are a study result only: not in `helper_scripts` or the lattices.
- Study details (error kicks, corrector response, tune and optics fits): "Study findings" at the end.

**Data questions:**
- BMAL12-BMAS14: phase offset ~0.6 of a one-turn shift, both planes, varying between pings. Not understood.
- BMAL3 (2026-11-02, user): the data's B:H/VST03L is an old-style BPM at the end of L3, not the L3 BMA-package
  BPM, which is B:H/VSTU3L; `bpm_data` now maps BMAL3_BPM to U3L. Every BMAL3 result before 2026-11-02 (below,
  the closed-orbit and ping studies, the talk plots) used 03L: to redo. Measured orbit at 5 ms: U3L x +0.7,
  y +12.0 mm vs 03L -2.2, -2.6 mm.
- BMAL3 and BMAL1: measured optics differ from the model (BMAL3 phase +-45 deg in the h pings, ~2x beta in v).
  Both sit next to elements missing from the lattice: L3 inside the vertical dogleg (the survey puts BMA005-L3
  12 mm low with the beam, between DOG03_1 and DOG03_2; survey entry), L1 in the ORBUMP chicane (the CFMs around L1
  sit 4.5-7.4 mm outward). In the lattice the L3 BMA package is also ~4.5 m upstream of the regular spacing.
  S12: the BMAS12 package sits ~20 mm outward (survey; applied in the misalignments).
- S5 -> L6 and S20 -> L21: -20 / -16 deg phase steps that don't come back; look like real optics.

### Optics correction: tunes, coupling, momentum (2026-11-06)
`studies/optics_correction/` (README there, `oc_summary.png`). **Applied: only the constant K1** (user 2026-11-06; below). K1(t), delta(t), c, d are not applied. The rest of
the time dependence may be remanence or VIMIN/VIMAX calibration; out of scope for now, longitudinal data come next (user). The numbers in this entry are from the study, whose lattice copy sets `dK1F = dK1D = 0`.
- **Applied constant:** `dK1F` -8.06e-3, `dK1D` -6.97e-3 in all four lattices (`qsf`, `qsd` x (1 + dK1)), the 7-18.5 ms means with delta and orbit in. Not the whole-cycle mean (-8.3, -6.1):
  D's drift moves Delta by ~0.02 and the mixing is lost (r_A, r_B 0.395, 0.628 vs measured 0.564, 0.871; with the plateau values 0.594, 0.945). Plain model (H = V = 0, no delta), model - measured: Qx, Qy -0.019, +0.025 at 3.2-6.5 ms;
  +0.009, +0.006 at 6.5-18.4; +0.066, -0.014 at 24.3-28.5; +0.054, -0.022 at 28.5-36. Without delta -8.88, -7.87 would centre 6.5-18 ms in a plain run, but it absorbs that day's delta, so the plateau values with delta in are kept.
  Side effects: gamma_t 5.559 -> 5.506 at 19.4 ms; transition crossing (H = V = 0) 19.528 -> 19.392 ms MEV400 (RF phase jump 19.465), 14.712 -> 14.604 ms PIP-II (jump 14.654). PIP-II painting: foil bump -113.2 mm unchanged. BEX c and closure ratios, the rpos study and the talk plots were made with the old K1 and were not redone.
- **Tune error, not a coupling error.** The two-mode split (`tan^2 theta = r_A r_B`, cancels betas and BPM gains) gives
  the model's |C| close to the measured one with the skew tables as they are. What is wrong is Delta = Qx - Qy: model
  +0.02 to +0.04, machine -0.02 at 9-16 ms. The "swap" and the strong apparent mixing follow from that.
- **The main magnet K1 alone fixes it.** dK1/K1 fitted to two numbers (Q_A + Q_B, Delta); not fitted, 9-16 ms: r_A, r_B
  0.560, 0.891 vs measured 0.564, 0.871 (0.410, 0.658 before), rms ring phase of mode A 15.7 -> 7.3 deg. As uncoupled tunes the
  error at 10 ms is about -0.10 / -0.05: the -0.12 / -0.03 in the entry above is partly the modes repelling.
- **K1 values** (with delta and orbit): F -8.06 +- 0.43, D -6.97 +- 0.51 (1e-3) at 7-18.5 ms; F -9.02, D -5.11 at 24-36 ms; F -6.0, D
  -7.3 at 3-6.5 ms. Cause not known. The D trend and the F step across the gap are not explained.
- **Skew timing.** Near the rise (5-9 ms) the model's |C| is ~0.7 ms early (6.26 ms: model 0.037, measured 0.018; S -0.039 vs
  -0.021). SQ tables + 0.7 ms fits the rise (S -0.025) but overshoots the fall (~0.3 ms there). Unexplained: more than the table
  dtoff + kernel (156 + 150 us), and there is no reason to believe in a supply lag (user 2026-11-06). Not applied.
- **Functional form of dK1/K1** (`oc_k1_form.py`, `oc_k1_form.png`, with delta and orbit): mostly a constant, F -8.3, D -6.1 (1e-3), rms 1.1 about
  it. F is flat at -8.0 for B/B(top) 0.2-0.45; D ~ -7.30 + 2.67 B^2 (rms 0.45); F rises to -5 below B = 0.15 (4 windows, the only 1/B-like part: at
  most ~+3e-3 at injection); F -10.7 -> -8.5 over 24-28 ms after transition looks like a transient, not a function of B. Scatter floor ~0.35.
  Hysteresis or remanence cannot be the main part: a 1/B term would fall 9x from injection to the top, and the constant does not. D does not follow 1/B
  (rms 0.75). A QS calibration error is excluded by sign (its fitted term moves both tunes the same way). Up-ramp data only, so hysteresis itself
  can't be isolated. The F step needs 2.24 x the delta + orbit effect and D the opposite sign, so a wrong chromaticity x delta alone doesn't explain it.
- **Momentum.** delta(t) from the orbit, with the kicks doing no path-length work (a uniform kick pattern and delta are the same
  orbit, so one needs that gauge), +2.0e-3 at 7 ms to -1.6e-3 at 28 ms, within 5% of the plain projection on D.
  x at L8 = 2.24 m * delta - 2.8 mm (corr 0.993, rms 0.32 mm): the radial loop sets delta. `p_RF / p_GMPS - 1` is not delta (+14e-3
  at injection): the 1% gap means the GMPS sine is not the field at low energy. With delta in the model the K1 step after
  transition shrinks (F -12.7 -> -10.7e-3 at 24 ms). delta is per day: up to 1.4e-3 different on 11-03.
- **Orbit.** The measured orbit itself matters little: feed-down moves the tunes <= ~0.01 (0.016 in Qx around 29 ms) and |C| ~5% before 18 ms; after 24 ms it nearly doubles the model's |C| (0.010 -> 0.018), which the pings can't check.
- **Thin kicks c + d u per location** (192 constants) with the H, V tables on, K1(t), delta(t): model - measured rms 0.7-2.3 mm x, 0.2-0.5 mm y by time range (4.8,
  1.1 mm at 2.9 ms), was 14-21, 21-41 mm. Held out, 10-20 c, d with the 11-03 tables: y 0.3-0.6 mm; x 1.3-2.7 mm, 3.7 at 10 ms, 7.7 at 3.7 ms;
  x 0.34-1.4 mm with only delta refitted per time. K1 alone only reduces the orbit error by ~1/3. c is large (rms 0.68 x,
  0.50 y mrad) and includes BPM offsets. No separate x, y offset correction (user 2026-11-06): degenerate with c, not needed. The y table change is predicted (0.65 -> 0.3 mm); the x change mostly is not.
- Mode A has no peak in the h ping at 19.3-24.3 ms, so Q_A is not measured there; |C| and the mixing are measured to 18 ms only.

### `ping_analysis.model_modes` uses the wrong maps (2026-11-06)
**Issue:** `pa.model_modes` carries the one-turn eigenvector to the BPMs with `R_i @ v`, `R_i` = RE11..RE44 at the BPM in the periodic twiss. Those are
the one-turn maps at each element (`T_i M T_i^-1`, checked to 5e-9, BMAS5, 10 ms), not the start-to-element maps `T_i`.
**Effect:** the tunes `Qx`, `Qy` are right (eigenvalues of the last row). `hv_x`, `hv_y`, `xshare_x`, `xshare_y` are not, and neither are the names Qx / Qy, which
come from `hv`. Small at nominal K1 (10 ms: hv 1.916, 0.801 vs right 1.978, 0.789), large after a K1 change (9-16 ms mean, r_A, r_B: 0.82, 0.65 vs right 0.571,
0.908). To recheck: the docstring's "Q1 = Qx at all 120 points", `talk_tunes.py` and its swap times 5.81 / 32.09 ms (`studies/talk_plots/model_modes*.csv`).
**Fix:** needs the `T_i`: a second TWISS from fixed start betas (`TABLE=CUM`, RMATRIX; `studies/optics_correction/oc_common.py`, `make_lattice`), and the
eigenvector from it. `oc_common.modes(cum)` does this, and also uses rms amplitudes and takes the eigenvector of `e^{+i 2 pi Q}`, so `w` compares directly with the data
(`w = conj(C)`). `model_modes` could take the CUM table as an argument; the project lattices don't write it, and `talk_common` adds RMATRIX to a copy only. Helper not changed.

### Timing and beam energy (after the 2026-10-25 fit)
- `KEMAX` = 7.988 GeV: the top energy implied by the RF frequency curve (`timing_fit.py`, written by
  `--apply`). The real value follows the day-to-day tuning to the Recycler; to come from RR measurements. With
  it, injection is at 0.3933 GeV (MEV400) / 0.7905 GeV (PIP-II), below the nominal 0.4 / 0.8 and the RF curve's
  0.4028 GeV at injection.
- The RF curve - ramp residual after the fit (rms 4.5, max 10 MeV) is the shape: a pure field-to-current scale
  doesn't fit both ends (an offset or saturation in the calibration).
- The RF curve CSV's `-75` header field (`oper-freq-delayed-minus-75-us-20260914.csv`) is not applied; if it is
  a programmed delay not in the 1875 us, the fitted 155 us absorbs it.
- `BDOT` 2271 us and `TFLAT` 285 us (PIP-II 400 us) are determined from B:BFLAT (centre and half-length of its
  nontrivial part, + 156 us), not measured on the GMPS itself; a GMPS current readback vs time would check them.
- VRF readback `dtoff` 0: not checked.
- `ramp_read.py` writes BDOT 2271, TFLAT 285 / 400 and KEMAX 7.988 as fixed values (from `timing_fit.py`,
  2026-10-25); run `timing_fit.py --apply` after a new read.

### Transition and the radial loop
- The machine's radial loop (RPOS) is not in the main code yet. Study `studies/rpos/` (2026-11-03; plan for
  `run_madx` in its README.md): a setpoint device (t, mm, nan = off; `bpm` BMAL8_BPM, `plane` x, `dtoff` to be
  found); at a time with a setpoint the RF frequency offset is solved so the model's x at the BPM = the setpoint
  (RF on). The beam is taken to sit on the setpoint (no memory along the ramp; RPOS switching on/off not modelled,
  user). MAD-X finding: the cavities' FREQ does not move the RF-on (6D) closed orbit, so the study lattice has a
  zero-length MATRIX time kick (KICK5 = -(df/f) C / beta) that acts as the frequency offset. Synthetic setpoint
  -2 -> +3 mm, off until 3 ms (paraphasing ends ~2.5 ms) and at 18.8-20.5 ms (transition). Works with H = V = 0;
  with the correctors on the large model orbit at L8 (5-18 mm) makes the 6D closed orbit fail at many times.
  Waiting on: the real RPOS curve, its device name and delay.
- The RFA phase jump lands at 19.465 ms; the model's transition crossing is 19.392 ms (H = V = 0, gamma_t 5.506 since
  the constant K1 of 2026-11-06; was 19.528 / 19.613 ms) and the RF sum's transition event 19.334 ms. Transition
  moves with the optics, so it is reported, not fitted.

### PIP-II ramps
- **ORBUMP rename (2026-11-04):** the lattice's ORBUMP magnets are now `OBMP1_8510`, `OBMP2_8511`, `OBMP3_8512`,
  `OBMP4_8513` (user, twice that day; optics unchanged). `painting.py` finds `OBMP<n>_<number>_KICK` by pattern, so a new
  number needs no change.
- **Painting magnet calibration unknown (2026-11-04):** no T m/A for the H_PAINT / V_PAINT magnets, which are separate
  magnets from the correctors (user). Kicks and Bdl for X_PM 6.1, Y_PM -16 mm: `studies/survey/painting_kicks.csv`.
- **Painting leak (2026-11-04):** painting alone (X_PM 6.1, Y_PM -16 mm, 1900 us) leaves 0.22 / 0.30 mm x / y outside the
  bump; x starts at -0.19 mm upstream of S09. `kick_angles` is linear, the leak is not: y bump alone gives -0.29 mm in x,
  x bump alone 0.03. Fit at the start: -0.07 f - 0.12 f^2 mm for offset fraction f. All SQ, SX, SS and the main magnet K2
  off: 0.004 mm. Nothing but the sextupoles and skew quads is needed to explain it. Not corrected.
- **Archived corrector kicks (2026-11-04):** in `lattice_archive/pip2_lattice/booster_PIP2.ele`
  `DHZ->CALIB = 0.000366 / 0.024 / Brho` is per metre, but `HKICK := CALIB * I` is a total kick, never times L = 0.024 m.
  So that file's kick per amp is 41.7 x the true 0.000366 / Brho, and its `i_hs` painting values (0.07 "A") are 41.7 x
  too small; its kicks in mrad and G-m are right. Today's lattices are correct (`HKICK=I_HS11*0.000366/Brho`), as is the older
  `pip_lattice` (`CALIB = 0.000366 / Brho`). No effect in the archive's runs beyond the painting trims: timeslot 1, tables 0.
- PIP-II has no programmed ramps. Sample ramps (`pip2_ramps.py`, user's method; remade 2026-11-14 from the 11_03_26 files as
  `11_03_26_BMA_PIP_II.json`, the 10_23_26 set below is the older one):
  `data_Booster/10_23_26_BMA_PIP_II.json`, `rf_phase_curves_PIP_II.json`, made from the MEV400 ones by Brho
  (README "BMA ramps (PIP-II)"). The PIP-II phase jump lands at 14.654 ms, the ramp's transition (gamma_t
  5.534, MAD-X 2026-10-23) at 14.653 ms.
- Through PIP-II flat injection the BMA tables (H, V, Q, SQ, SX, SS) give the MEV400 strength at 2000 us
  (current x 1.537, the Brho ratio; user 2026-10-25), then interpolate to the first Brho-matched breakpoint.
- What the mapping leaves out: the MEV400 breakpoints below the PIP-II injection Brho (before ~7.7 ms,
  including the RF paraphase switch at injection; the RF phases hold their 7.7 ms value) are dropped; only
  breakpoints are mapped, so values between them differ slightly from MEV400's at the same Brho
  (e.g. B:HL1 0.1-0.2 A); `B:BEX2T` in the PIP-II MISC file is still the MEV400 table, not mapped.

### Extraction elements
- DOG_*, BEX2-4 and MKS_KICKER are defined in both lattices and `I_BEX2` / `MKS_ON` are written, but they are
  not in the sequences: not measured in as-found surveys; installation surveys are being investigated.
- Modified lattice (`helper_scripts/lattice_mod.py` (was `lattice_copy.py`) -> `madx_files/<lattice>_modified.madx`, 2026-11-02): BEX2-4
  and the four L2 MKS (`EXTRACTION_KICKER_L2_A..D`) are placed from the archived lattice (estimated, not
  surveyed), and L12_D is an MKS_KICKER.
- BEX currents (2026-11-02): `B:BEX2..4` = one table x scale 1.1992 / 0.9414 / 1.1992 (`11_02_26_MISC_*.json`).
  In the bare model these do not close the bump: closing needs BEX3/BEX2 = 0.479 and BEX4/BEX2 = 0.948 in kick
  (MEV400; PIP-II 0.523 / 0.927; BEX3 moved to the end of L3, next to the old-style BPM, 2026-11-02) vs 0.785 / 1 from the scales; at the top ~6-7 mm leaks outside a ~22 mm bump.
  BEX geometry/edges not known. Scale kept at 0.9414 (user). Tests at the top (2026-11-02, MKS off, outside
  max |dy| bare / full MEV400): BEX3 x 1.0 6.7 / 8.0 mm; x 2 24 / 29 mm; reversed 28 / 35 mm; x 0.5 2.4 / 3.2 mm
  (bump ~27 mm at the septum); the bare model closes at ~x 0.57 of the scale (x 0.61 in kick; PIP-II ~x 0.67).
  To test again once the DOGLEG is in the lattice: BEX3 x 1.0, x 0.5 and x 0.57.
- Older BEX studies (2026-11-02/03: turn-4028 change, bex_calibration, TBT plan): superseded by the entry below; dtoff 0 and c ~ -1 are no longer right. Text in `ISSUES_ARCHIVE.md`, the study files in `../Booster_lattice_old_BEX_studies_2026_11_05.zip` (removed from `studies/` 2026-11-05, user).
- BEX timing and strength (2026-11-05, `studies/BEX_calibration/bex_timing_position.ipynb`, cycle 1D, BMA BPMs; user: BEX2 and BEX4 share a c, BEX3 its own, maybe a different supply; table x scale = magnet amps; +0.5 A at the plateau's flat top). **Applied:** BEX `dtoff` 258 us in both 11_03_26 MISC files (the 11_02_26 files keep 0) and in both lattices `KICK = -0.0000295*I_BEX2/4*...`, `-0.0000137*I_BEX3*...` (were 0.0001). Sign confirmed as expected (user 2026-11-05): +current kicks -y (MAD-X) / +z (up in the world); `ramp_read.py` now writes BEX dtoff 258 (user).
  Timing: the BMAL3/BMAS3 linear rise meets the baseline 258 us after breakpoint 11 (rms 9 us; 253-274 with the fit window). As one number 258 fits the rise and flat top (rms 0.028 A; 156: 0.098, 0: 0.203); it is really a 168 +- 2 us delay + a 127 +- 2 us lag (0.009 A). The lag / build-up is not needed for BEX (user); to be done for the BMAs later. Falling edges are slower than the table: total current -1.09 / -1.17 A/ms vs about -1.75 / -1.45 asked (rising +0.6), possibly a rate limit (not modelled).
  Strength: c24 = -0.295 +- 0.005, c3 = -0.137 +- 0.004 at lattice positions, rms 0.031 mm; other windows / without BMAS2 -0.290..-0.304, -0.133..-0.143. One c for all three does not fit (rms 0.366 mm; 0.249 free in the long straights; 0.067 only with BEX2/BEX3 moved -5.75 / -8.75 m out of their straights). Independent check, turn 4028 of 2026-11-02: with dtoff 258 these constants give a model scale 0.96, rms 0.055 mm (dtoff 156: 1.26, 0.113; dtoff 0: 2.58, 0.303), which explains the old c ~ -1.
  Positions not determined: free in the straights with c24, c3 the rms is 0.028 vs 0.031 mm (BEX2 -0.46, BEX3 +0.38 at the straight's end, BEX4 +1.18 m; c3 -0.159), so not moved.
  At the top (35 ms, H = V = 0, MKS off, MEV400): bump +8.7 mm at SEPT_CAN_CT (model y, down), was -20; max outside 1.3 mm, was 7.7. Against the MKS, which kicks -y (user 2026-11-02). OLDSTYLE_BPM_L3 reads 2.55 mm vs 1.81 predicted (BEX2-4 fit c per magnet); the other two old-style BPMs agree within 0.04 mm.
- BEX 3-bump closure (2026-11-05, `bex_timing_position.ipynb` section 6, suggestion, not applied): MEV400 model, calibrated constants, BEX dtoff 258, H = V = 0, RF off, MKS off, BMA BPMs outside the bump. Closure at the top needs I3/I2 1.085, I4/I2 0.939 (10.3 ms: 1.151, 0.936). Keep B:BEX2S 1.199: B:BEX3S 1.301, B:BEX4S 1.126, leak at 35 ms max 0.008 mm (now 1.34), at 10.3-20 ms 0.09-0.18 mm, septum 7.45 mm (now 8.73), BEX3 -13.9 A at 35 ms (now -10.1). Or keep B:BEX3S 0.941: B:BEX2S 0.868, B:BEX4S 0.814, septum 5.39 mm. Uncertainty: c3 and positions move S3 about 5% (case F positions and c: S3 lower), a 4% S3 error leaks ~0.17 mm max. PIP-II closure not adjusted for now: any change there will be minor because of BGDW (user 2026-11-05). `plateau_vs_model.py`: the calibrated lattice vs the plateau peak, y rms difference 0.032 mm on 0.368 mm.
- MKS strength (user's lattices 2026-11-02): `KV_MKS = 48.75` kV per kicker (5 kickers, assumed; operating voltage
  not known) x 5.273 urad/kV, -0.257 mrad each at the top, ~-20 mm at the septum from the four L2 MKS on the
  first pass. Calibration 5.273 urad/kV confirmed by the user 2026-11-02 (an earlier message said 5.2273).
- MKS sign flipped 2026-11-02 (both lattices): the kick was +y (down), the opposite way to the BEX bump; now -y
  (up, model -y = up), the same way as the bump.

### BMA field response (eddy currents, 2026-11-06)

Square-pulse scope data from a test stand (`studies/bma_kernel/`, dipole only) fit an exponential kernel with a pure
delay: dtoff 146 us, a 300 us; L is only bounded below (>= ~2 ms). dtoff should be close to the table `dtoff` of 156 us
(the stand's cables differ; user). The tables and currents are not changed (user): the kernel gives the field the beam
sees at a time t (`mxin.field_at`, on top of `value_at`; off by default, `Config(beam_field=True)`, `--beam-field`). Up to ~1.9 A (H, V, 5 ms)
and ~2 A (SX, 20 ms) from the current. a_m = a/m from the thin-wall formula (Eldred, slide 7), not measured; it fails
at high frequency, where the skin depth reaches the wall (~8 kHz for 3/16"). The wall is taken as 3/16" (user, from
memory; slide 9 says 1/8"). The two models differ: measured a 300 us, formula 266 us (177 us for 1/8"). All of the measured a is assumed to scale as 1/m,
like the pipe (user 2026-11-06). The orbit with the kernel differs from the orbit without by 0.1-1.3 mm rms in x, 0.1-2.2 mm
in y (2.8 / 5.0 mm at 3 ms, in the steep first H, V segment, 2.56-3.16 ms); against the measured orbit (rms 13-21 mm in x, 21-45 mm in y)
that is an improvement of < 2 mm in x, < 5 mm in y (`orbit_compare.py`), so it does not explain the n = 7 orbit. The -0.1 dip in B at the rising edge is not in the kernel.

### RF phase and the RF sum mode (2026-11-13)
- The RF phase curves are 'peaky' between 5 and 10 ms (their mean phase 60 deg at 3.5 ms, 4 deg at 9-10 ms); investigations
  are ongoing. Until then the default RF mode is `sum` (MEV400, user 2026-11-13): the measured RF sum `rfsum_smoothed.json`
  (scalar sum of the cavity amplitudes, user) / 20 on each cavity, phi_s from dKE/turn; only the paraphase and the transition
  jump come from the phase curves. It agrees with the phase curves' mean to 1-3 deg after ~14 ms (`plots/rf_sum_phase.png`).
- The RF sum is one cycle (2026-06-10, its own calibration in the file); `dtoff` 2000 us puts its injection at turn 0. Its
  transition event (19333.6 us) is 131 us before the phase curves' jump (19465 us), where phi_s is switched. PIP-II has no RF
  sum data (it keeps the `phase` mode).

### Survey misalignments (2026-11-13; `helper_scripts/misalign.py`, study in `studies/survey_misalignments`)
- **In the main code:** `misalign.py` + `geometry.py`; device files `data_Booster/11_13_26_MISALIGN_MEV400.json` /
  `_PIP_II.json`; `lattice_mod` writes them by default into both `_modified` lattices (record file, PERM_ALIGN_SURVEY
  survey, TRACK option / tracking start); `run_madx` + `bpm_compare` subtract the BPM offsets. Checked against the study
  (element offsets 3e-10 m; orbit vs measured identical). Decisions and details: the study README.
- **Data:** the corrected beamsheet `data_Booster/cleaned_beamsheet_Aug20_corrected.csv` (BMAL16 NORTH swap, VIPM and
  BPM-SPEC-COLL UP/DN, BPM-SPEC-COLL CT, DAMPER-PICKUP name) for everything but the CFMs; CFMs from `compareHandZ.csv`
  (2026). The two are different survey networks: CFM radial difference a harmonic-2 wave of ~+-2.5 mm; the BMAs and
  others stay in the beamsheet network (noted, not corrected; user).
- **Survey vs lattice** (2026 CFMs, no global fit): CFM dx +2.9 mm mean (surveyed ring ~18 mm longer), rms 5.5; ds a
  +-7 mm sawtooth stepping at the straights (noted; maybe the straight drift lengths); yaw 0.56, pitch 0.26 mrad.
- **Real odd positions:** BMAS12 ~20 mm outward (applied); BMAL3 in the L3 vertical dogleg (applied since 2026-11-14, user: the dogleg
  runs at constant current, so the package is misaligned on purpose); the four CFMs around L1 4.5-7.4 mm outward (ORBUMP chicane, kept); DMAG5_1 ~4 mm (kept); all
  collimators applied. Left out by default: VIPM, HIPM, SEPT_CAN, ORBD_USBPM (fiducials 43-73 mm off); PIP-II
  also the relocated BMAL11 / L10CURR_MON. BGDS/BGDW and the renamed PIP-II dampers are new elements, not misaligned.
- **Optics effect** (MEV400, 5 ms, H = V = 0, RF off): 16 / 12.5 mm rms BPM orbit, dQ1 -0.010, dQ2 -0.001, 2.9% beta
  beat; after transition dQ1 up to +0.09 (BMA sextupole feed-down from the uncorrected orbit). Correctors on vs the
  measured 10-20 orbit (5 ms): rms difference x 13.3 -> 9.8, y 25.8 -> 17.2 mm; better, but the corrector-orbit
  mismatch is not explained. PIP-II (1.9 ms): BPM rms 2.1 / 2.6 -> 13.8 / 7.0 mm.
- **MAD-X `PERM_ALIGN_SURVEY=TRUE` rotates about the global axes** (W' = R W), TWISS about the local ones: the yaw
  has the wrong sign in these lattices (a CFM core exit up to 15.6 mm off); translations are right. Use the Python
  geometry for rotated positions; the perm survey is kept to quantify this (user).
- **Open:** roll data (the `roll` column of compareHandZ.csv); the L3 dogleg itself (DOG03_3/4 survey); PHYS_CENTER (with it
  the F/D mean dx differ by 1.9 mm; the user's lattice-building script); the PIP-II misalignment orbit at the foil
  (painting does not correct it; check later, user).

### RF voltage readbacks (only used in RF mode `phase+volt`)
- `B:RF01GA` reads ~20x the others: divided by 20 (matches to ~2% over the ramp, not at the ends; RF01 stays on
  1-2 ms after the others). Cause unknown.
- RF02, RF03, RF11 read flat (~1.5-1.8 kV/turn) all cycle; RF21, RF22 have no data (off in the model).
- The 10_23_26 read is identical to the 10_07_26 one: pacsys may return a stored waveform. Check new reads.

## Resolved (newest first)
- **Constant main magnet K1 (2026-11-06, user; finding from the 2026-10-20 pings):** `dK1F` = -8.06e-3, `dK1D` = -6.97e-3, `qsf`/`qsd` x (1 + dK1) in
  all four lattices. The model's coupling was right and its tune split Delta had the wrong sign; this constant brings the tunes and the mode
  mixing onto the measurement at 6.5-18 ms (Qx, Qy +0.009, +0.006; r_A, r_B 0.594, 0.945 vs 0.564, 0.871). gamma_t 5.559 -> 5.506. The only optics
  correction applied; K1(t), delta(t) and the x'/y' kicks c + d u stay in `studies/optics_correction/`. Details: "Optics correction" under Open and
  `studies/optics_correction/optics_correction_notes.pdf`.
- **Skew quad sign (2026-10-25, user):** both lattices now use +0.003924 (was -0.003924) in the skew quad
  KSL, from an earlier study; equals the 2026-10-24 SQneg test. It changes only the corrector-on results (the
  orbit through the sextupoles); with the orbit at 0 a sign flip of all skew quads gives the same eigentunes
  and coupling, so the H = V = 0 fits and ping comparisons are unchanged. All numbers here are after it.
- **Data files to JSON (2026-10-25):** the BPM text files are in `data_Booster/10_20_26_BPM_TBT_*.json`
  (`bpm_data`), the format the pacsys BPM data will be saved in.
- **Timing (2026-10-25, user's method, `timing_fit.py`):** BMA magnets (correctors, B:BFLAT) and `B:BEX2T`
  (same ramp cards) have the 156 us delay, as a pure delay. BDOT = centre, TFLAT = half the length of
  B:BFLAT's nontrivial part (1830-2400 + 156 us): BDOT 2271, TFLAT 285 (PIP-II: same BDOT, TFLAT 400, 20 Hz).
  B:VIMIN = GMPS trough, injection KE = the sine at BDOT + TFLAT. RF curves: `t` starts at the programmed
  1875 us, `dtoff` = the extra delay, fitted with a free pc scale: 155.2 us (frequency and phase curves).
  KEMAX = the fitted top energy, 7.988 GeV (was 8.05); injection 0.3933 / 0.7905 GeV. Replaces BDOT 2215 /
  TFLAT 208 of 2026-10-14 and the VIMIN-as-flat-level calibration.
- **GMPS calibration (2026-10-23, user):** B:VIMIN/B:VIMAX are currents [A]; B:VIMAX is the current at KEMAX,
  field proportional to current (extraction anchor; the injection anchor gave an 8.09 GeV top, off the RF curve).
- **Turn 0 (2026-10-24, user):** injection = the control system's turn 0 = 2000 us (`bd.TURN0`); the BMA
  breakpoint at 2.4 ms + 156 us is seen at turn 251.
- **BPM conventions (2026-10-24, user):** BPM x = model x, BPM y = -model y (`bc.BPM_SIGN`); 1000 = no reading;
  the first 10-20 turns are in an injection mode (kept, `bd.GAIN_TURNS`).
- **Sextupole dt /20 and skew quad /13 (2026-09-29, closed 2026-10-24):** known pacsys errors in the old
  exports; the 2026-10-23 reads come out corrected. Check again on any new reader.
- **HT/VT tables (2026-10-23, user):** 8 points to 60 ms, as intended; only the part past the ramp top differs.
- **Painting (2026-10-23, rechecked 2026-10-25):** 2x2 sector maps are good enough. With the PIP-II sample ramps
  (1.9 ms, Y_ORBUMP -0.097, Y_PM -0.016, X_PM 0.0061 m): bump at TREATYPOINT 6.19 / -113.24 mm (asked 6.10 /
  -113.00), leak > 30 m away 0.95 / 0.84 mm, from nonlinear fields (SQ off: 0.92 / 0.83). The first PIP-II
  ramps (MEV400 7.7 ms settings held through injection) gave 6.98 / -114.14 and 3.1 / 2.1 mm, half of it
  coupling: fixed by matching the injection strengths (PIP-II ramps entry).
- **Lattices of 2026-10-22:** `Brho_EXT` typo fixed; PIP-II ORBUMP `DIPEDGE` E1 = 0 (relative to the flat
  reference; with E1 = ORB_KICK_ANG the bump didn't close).
- **Data files of 2026-10-23:** reader fixes (BFLAT kind/group, VRF times and RF01 /20, `slot` key, roles
  `setting`/`value`); names with `B:` = ACNET devices or fakes of them, without = our settings or file data.
- **Overrides (2026-10-14, user):** replace `zero`/`painting`; a group replaces S * T + O, a part only itself;
  `B:HL3 = 0` keeps BFLAT; unknown names are warned about.
- **RF model (2026-10-13):** LAG = phase / 360 (RFA or RFB per cavity); no arrival-time term (MAD-X adds none,
  tested); RF mode `phase`: one V from dKE/turn; RF21/RF22 off.
- **KE / KE_true (2026-10-12/13):** `KE_true` = beam, `KE` = GMPS; lattices use `PDELTA = pc_true/pc - 1`.
- **Earlier fixes:** RF variable names in the input file (`V<nn>`, `LAG<nn>`, `FREQ_RF`; were silently 0),
  `optics_scan` removed (scans go through `run_madx`), old painting scripts replaced by `painting.py`,
  `device_data.save` kept `meta["notes"]`, PIP-II/MEV400 sequence names swapped (user).

## Conventions (answered)
- `dtoff`: delay from command to the device's response (or a readback's travel time); value at t =
  S * T(t - dtoff) + O. BMA tables, `B:BFLATT`, `B:BEX2T`: 156 us. RF frequency and phase curves: `t` from
  the programmed start `t_start` 1875 us, `dtoff` 155.2 us (fitted). VRF readbacks: 0 (not checked).
- Chromaticity: dQ/(dp/p) = beta * MAD-X DQ (DQ is w.r.t. PT); checked against finite differences.
- `I_` currents = ramp table + offset; B:BFLAT = `B:BFLATT` * `B:BFLATS` (sf -3.0), added to all I_HS / I_HL.
  Outside a table it holds its first / last value (also t < 0); t < 0 gives KE_true = KE = the flat level.
  Past the peak KE_true holds at the top, KE follows the sine down.
- The GMPS sine is in pc; B:BDOT = time of its minimum.
- `CIRC` = the twiss `LENGTH` of each lattice: MEV400 474.2000000010 m, PIP-II 474.2002212361 m.
- Ramp table `dt` is in us; tables keep all 64 slots (trailing `dt == 0` padding, trimmed by `dd.ramp`).

## Study findings (closed orbit and optics, 2026-10-24)
Short summary; the numbers and figures are in `studies/BPM_CO_opt/closed_orbit_notes.pdf`, the scripts there,
and `ISSUES_ARCHIVE.md`.
- Measured orbit vs model: the model with H = V = 0 is much closer to the machine than with the correctors on;
  the model corrector orbit at full scale makes the fit worse than no model (fitted scale 0.1-0.2).
- Corrector response: around a small orbit, as in the machine, the model's response to corrector changes over
  short steps matches the measured one (correlation ~0.9, `co_steps.py`, `co_small_orbit.py`); calibration
  (0.000366 T m/A, from the previous model), signs and placement look roughly right. The x scale (~0.3) is open.
- Error kicks fitted to the measured orbit (`co_error_kicks.py`, `co_joint_fit.py`): per location
  c + d Brho(t0)/Brho(t) explains 96% (x) / 98% (y); rms c ~0.6 (x) / 0.4 (y) mrad (scales with the field:
  misalignments, strength errors, BPM offsets), d ~0.2-0.3 mrad at injection. Lowering the model tunes
  (x by 0.08-0.25) improves the fit, as the pings say.
- Tunes (`ping_analysis.py`): mode A 6.75-6.77 at 3-17 ms, ~6.69 at 30-35 ms; mode B 6.77-6.83; coupling
  0.5-0.9 at 6-18 ms, 0.1-0.3 before and after.
- Phase advances: most BPM-to-BPM gaps agree with the model within +-5 deg; beta beat ~20% (x), ~17% (y) rms.
- Fitted momentum offset in x: +0.2 to +1.7e-3 before transition, -0.2 to -1.5e-3 after; compare with the
  RPOS curve when it comes.
