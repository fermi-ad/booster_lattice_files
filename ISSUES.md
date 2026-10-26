# Open questions and data issues

This document records the known issues of the Booster lattice model and of its input data. It has four parts:
- **Open**: unresolved issues. Each states the problem as observed, the current understanding of it, and the
  questions that remain. New findings are added here.
- **Resolved**: decisions and fixes, newest first, each as a short record of what was decided.
- **Conventions**: definitions the code and the data files rely on.
- **Study findings**: summaries of the supporting studies behind the open issues.

Provenance: the complete record up to 2026-10-25 with all intermediate numbers, the 2026-10-06 data-file
details, and the closed-orbit study's details, figures and scripts are available from the author. Numbers are for
the MEV400 lattice with the 10_23_26 data files and the timing of 2026-10-25 (BDOT 2271 us, TFLAT 285 us, KEMAX
7.988 GeV) unless noted; all scripts were rerun with it on 2026-10-25.

## Open

### Closed orbit and optics: model vs machine
**Issue:** the model's closed orbit and tunes are not consistent with the measurement (BPM data of 2026-10-20,
cycle 17, `data_Booster/10_20_26_BPM_TBT_*.json`).
- With the H/V corrector tables on, the model's closed orbit is 27-53 mm (x) and 39-84 mm (y) at its largest
  over 2-30 ms; the measured orbit is a few mm (rms 4.5-7.4 mm x, 1.0-3.3 mm y).
- Model tunes are higher than measured (pings): by ~0.12 in x (mode A) and ~0.03 in y (mode B).
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
- Study details (error kicks, corrector response, tune and optics fits): "Study findings" at the end.

**Data questions:**
- BMAL12-BMAS14: phase offset ~0.6 of a one-turn shift, both planes, varying between pings. Not understood.
- BMAL3 and BMAL1: measured optics differ from the model (BMAL3 phase +-45 deg in the h pings, ~2x beta in v).
  Both sit next to elements missing from the lattice (L3 between the two DOGLEG magnets, L1 next to the old
  ORBUMP); see README "DOGLEG, BEX, and MKS". In the lattice the L3 BMA package is also ~4.5 m upstream of the
  regular spacing. S12: not known.
- S5 -> L6 and S20 -> L21: -20 / -16 deg phase steps that don't come back; look like real optics.

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
- The machine's radial loop (RPOS) is not included in the lattice; deferred until the user has the radial offset curve.
- The RFA phase jump lands at 19.465 ms, 0.15 ms before the ramp's transition (gamma_t 5.592 at 19.613 ms with
  KEMAX 7.988). Transition moves with the optics, so it is reported, not fitted.

### PIP-II ramps
- PIP-II has no programmed ramps. Sample ramps (`pip2_ramps.py`, 2026-10-25, user's method):
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

### RF voltage readbacks (only used in RF mode `phase+volt`)
- `B:RF01GA` reads ~20x the others: divided by 20 (matches to ~2% over the ramp, not at the ends; RF01 stays on
  1-2 ms after the others). Cause unknown.
- RF02, RF03, RF11 read flat (~1.5-1.8 kV/turn) all cycle; RF21, RF22 have no data (off in the model).
- The 10_23_26 read is identical to the 10_07_26 one: pacsys may return a stored waveform. Check new reads.

### RF Phase
The RF phase curves have an error in them that makes them very 'peaky' between 5 and 10 ms. Investigations are ongoing.

## Resolved (newest first)
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
Short summary; the full numbers, figures and study scripts are available from the author.
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
