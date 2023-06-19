! This file is NO longer maintained!
! use "git log" to see all changes.

!     Sign of bending in the horizontal magnets is taken reverse to get a positive dispersion in a Booster 
!     lattice and in the 0.4 GeV beam line from Linac to Booster. For geometry calculations we need to change 
!     sign of bending in both lattices. 
! 
! Booster full lattice  (modified 03/07/03)
! Version 1.0  10/23/01  A. Drozhdin
! Version 1.1  10/25/01  A. Drozhdin  BPM names changed
! Version 1.2  10/26/01  W. Chou      Body sextupoles added
! Version 1.3  10/26/01  A. Drozhdin  Magnet rotations (March 1989, before move)

! Version 1.4  10/26/01  A. Drozhdin. No magnet rotations. Position of extraction at long 03 
!                        and long 13, injection at long 01, pinger, notcher and damper at 
!                        long 09 and 11 and RF modules are added. Extraction dogleg magnets 
!                        "DOG03_1, 2, 3, 4" and "DOG13_1, 2, 3, 4" are on only at injection. 
!                        Chromaticity and harmonics sextupoles and octupoles are added. 

! Version 1.5  10/26/01  A. Drozhdin. Lattice with magnet rotations. Position of extraction at 
!                        long 03 and long 13, injection at long 01, pinger, notcher and damper 
!                        at long 09 and 11 and RF modules are added. Bend angle of extraction 
!                        dogleg magnets "DOG03_1, 2, 3, 4" and "DOG13_1, 2, 3, 4" and quadrupole 
!                        and sextupole harmonics are done at injection. These magnets are CD. 
!                        Chromaticity and harmonics sextupoles and octupoles are added. 

! Version 1.6  06/11/02  A. Drozhdin. Lattice with magnet rotations. Extraction bump-magnets at 
!                        long 03 and long 13, and injection at long 01 with "OBBUMPS" in their real 
!                        positions and real strengths and functions (see. "bcel01" line below) 
!                        (real "OBBUMPS" have strong sextupole and decapole harmonics of the magnetic field. 
!      01/09/03          Measured harmonics are inserted as multipoles) (real orbit has a displacement 
!                        in the Booster at injection by 8.8 mm with respect to the optical center 
!                        line, in these set of the Booster parameters there is no orbit displacement by 8.8 mm), 
!                        pinger, notcher and damper at long 09 and 11 and RF modules are added. Bend angle of 
!                        extraction dogleg magnets "DOG03_1, 2, 3, 4" and "DOG13_1, 2, 3, 4" quadrupole 
!                        and sextupole harmonics are done at injection (see. "bcel03" and "bcel13" lines below). 
!                        These magnets are CD. Chromaticity and harmonics sextupoles and octupoles are added. 

! Version 1.7a 02/25/03  A. Drozhdin. Dogleg magnets effective length and magnetic field are changed 
!                        according to the measurements on 04/30/97:
!                                             BL(03)=1.9896 kG.m (480A)   BL(13)=1.9059 kG.m (460A). 
!                        Layout of Dogleg-03 and Dogleg-13 is corrected according to the mechanical drawing. 
!
! ***                    Sextupole and quadrupole harmonics are inserted to the second and third DogLeg magnets with 
! ***                    displacement in a vertical plane corresponding to the beam orbit (as a function of Pc). 
! ***                    This is done by a "multipole" with effective K0L,K1L and K2L in the center of magnet.
!                        Main magnet body+end-pack sextupoles are corrected according to the dc chromaticity 
!                        measurement at 400 MeV on 01/30/03:    ssf=-0.0030,  ssd=-0.0454 
!                        Chromaticity sextupole setting at 400 MeV for dynamic aperture tracking:  
!                                             SEXTS=0.233 (15A), SEXTL=0.0 (0 A)
!                        Gamma-t jump quadrupoles setting (scaled from transition energy):
!                                              k1(qgf ) = 0.1128 (at 2000A)
!                                              k1(qgd) = -0.1128 (at 2000 A) 
! -----------------------------------------------------------------------------------------------------------------                      
! Version 1.7e 05/20/03  A. Drozhdin.                                                                             -
!                        Layout of Dogleg-03 is changed according to new proposal to increase                     -
!                        distance between magnet centers to 40", and to decrease current to 180 A.                -
!                                BL(03)=1.9896 kG.m (480A)   BL(13)=1.9059 kG.m (460A) - current version          -
!                                BL(03)=0.7461 kG.m (180A)   BL(13)=0.0 kG.m (0 A) - new version                  -
!                                                                                                                 -
!              12/08/03   New injection scheme is used with OrBump strength=0.2926T*0.4823m, angle=44.23mrad      -
!                         Nonlinearities are increased by k=48/39.37 with respect to the old version.             -
!                         Quadrupole correction No.2 is NOT used.                                                 -
!                                                                                                                 -
!     ATANTION  !!!!!    ATANTION  !!!!!    ATANTION  !!!!!    ATANTION  !!!!!    ATANTION  !!!!!                 -
! See coefficient:    kpc=pc0/pc used for "body sextupole" and "dogleg" bump strength (pc0=0.954263 at injection) -
!                     korbump=1.0 or 0.0   for injection or after injection                                       -
!                     ekin=0.4 GeV  at injection, 8.0 GeV  at extraction, 4.174176 GeV  at transition             -
! ISEXTS= 11.400                ! sext.correctors SEXTS current (A) (at 0.4 GeV)                                  -
! ISEXTL=  0.000                ! sext.correctors SEXTL current (A) (at 0.4 GeV)                                  -
! ISEXTS= 73.200                ! sext.correctors SEXTS current (A) (at 8 GeV)                                    -
! ISEXTL=-80.000                ! sext.correctors SEXTL current (A) (at 8 GeV)                                    -
! ----------------------------------------------------------------------------------------------------------------- 
! 04/09/2010   A. Drozhdin                                                                                        -
!                                                                                                                 -
!            corrector packages were changed from:                                                                 -
!                                                                                                                 -
!   CPL01 :    line=(hlN,vlN,qlN,qslN),                                                                           -
!   CPS24 :    line=(hsN,vsN,qsN,qssN), with each corrector length of 0.075m,    to                               -
!                                                                                                                 -
!   CPL01 :    line=(drift9,hlN,vlN,qlN,bpmsN,qslN,sxlN,sslN,drift9)                                              -
!   CPS24 :    line=(drift9,hsN,vsN,qsN,bpmsN,qssN,sxsN,sssN,drift9),  with each corrector length of 0.024m,      -
!                                                                      and drift9 length of 0.066m                -
!   SEXTL, SEXTS   were changed to drift spaces with the same names                                               -
!                                                                                                                 -
!        Chromaticity is left the same, using strength of new sextupoles sxlN and sxsN                            -
!                                                                                                                 -
!      gamma-t quadrupoles are changed to drift spaces with the same names      qgf   : drift, l=Lqgf             -
!                                                                               qgd   : drift, l=Lqgd             -
!                                                                                                                 -
! -----------------------------------------------------------------------------------------------------------------
! 5/03/10   Jim Lackey
!			Made several Corrections
!
!@L01	Put in Mark24D and Mark1 in bcel01
!	Changed L01d to L01da & L01db 
!	Put L01da,CPL01,L01db into bcel01
!
!@L03	Changed L03b to L03ba and L03bb
!	Put L03b,CPL03,L03bb into bcel03
!
!@L05	Changed drift lengths of L05e1 and L05e2
!	Added NOTCHER: vkicker, l=1.08, kick=0.0 & PINGER: hkicker, l=1.08, kick=0.0 to bcel05
!
!@L09	Replaced PINGER & NOTCHER with L09g and L09h
!
!@L13	Uncommented the CPL13 that defined the new L13 corrector package, commented out the other CPL13.  
!
! -----------------------------------------------------------------------------------------------------------------
! 12/06/10   Jim Lackey
!			Made one Correction 
!
!@L03	Uncommented the CPL03 that defined the new L03 corrector package, commented out the other CPL03.
!          I thought I had done this the last time but apparently didn't!!
!    
! ----------------------------------------------------------------------------------------------------------------- 
!02/16/11 Kiyomi Seiya
!
!Made each cell length = 19.7605 [m]
!
!Removed non existing componets and replaced to drif.
!
!Changed scale factor for the quad and skew quad on the new collectors from kquad= 0.02178 to 0.0025 
!and from ksquad = 0.00889 to 0.004
!
!-----------------------------------------------------------------------------------------------------------------
!
!            Integrated Trim Magnets strength: 
!
!  outer dipole (V)     5.360E-04  T-m/A   (Max. current = ???? )
!  inner dipole (H)     9.260E-04  T-m/A   (Max. current = ???? )
!  skew quadrupole      8.889E-03  T/A     (Max. current = ???? )
!  normal quadrupole    2.178E-02  T/A     (Max. current = ???? )
!
!         Chromaticity correction sextupoles: 
!
!         SEXTS (H)     1.487E-02  T/m/A   (Max. strength = 0.915 T/m   ?????)   (Max. current = ???? )
!         SEXTL (H)     3.750E-02  T/m/A   (Max. strength = 2.318 T/m   ?????)   (Max. current = ???? )
!
!               Harmonic sextupoles:
!
!
!  Sexl4, Sexl5, Sexl6, Sexl7   3.717E-03  T/m/A     (Max. current = ???? )
!
!  Octupole:              Oct   0.525E-03  T/m^2/A   (Max. current = ???? )

///*********************************************************************************************************///
///*********************************************************************************************************///
///*********************************************************************************************************///
!	03/25/2013		
!	Booster lattice converted from MAD8 to MADX format by M. McAteer and C.Y. Tan 
!	changes by M. McAteer:
!	MADX file is now in three parts: booster.madx, booster.ele, booster.seq, plus all revision history notes in booster_readme.txt
!	Layout of Long 3 dogleg has been altered to match drawings by J. Lackey, but U3L bpm position is not exactly known (needs to be checked with survey data)
!	Operational values of corrector package elements, dogleg current, and harmonic sx current are read in via tables; user can select a slot for the time 
!		in the acceleration cycle to use (corresponding to a row in the QL.dat table) for reading magnet currents. The normal and skew pseudo quad errors from
!		LOCO can also be included; the variable "errorSwitch" turns this off and on.
!
!-----------------------------------------------------------------------------------------------------------------
!01/14/2020 Jeff Eldred
!	Added RF20 to BCEL18, RF21 & RF22 to BCEL20 in .seq file. BRF represents Booster RF, but left as drift element.
!	Set I_SEXL4, I_SEXL5, I_SEXL6, I_SEXL7 in the DC.dat file to zero - components obsolete.
!	Updated corrector .dat files to reflect 06/19/19 values.
!	Added some bcell description to .seq file - injection, extraction, bex, RF, collimators, notcher.
!
!03/09/2020 Jeff Eldred
!	Fixed BCEL20 length discrepancy introduced in 01/14 update, removed OLDPOS/RPOS_MON_OLD. 
!
!03/25/2020 Jeff Eldred
!	Removed some commented out lines.
!       Verified that all cell lengths = 19.758448 at timeslot=3, with orbump & dogleg off
!
!04/20/2020 Jeff Eldred
!	Increased angle_orbump from 1.1 mrad to 11 mrad, fixing an apparent typo.
!	Fixed length-distortion effect in Booster cell 01 so that it is removed when orbump is turned off.
!
!03/31/2023 Jeff Eldred & Cheng-yang Tan (12/6/21)
!	Commented out SEXTL, OCT, SEX_DISC statements that threw spurious warnings.
!   Added chromaticity outputs to booster.madx to accurately beta factor in chromaticity.
!       Updated definition of ssd,ssf to more accuratle recreate chromaticity.
!
!06/19/2023 Jeff Eldred
!	Switched from Fermilab Redmine repository to Fermi Controls github repository.


