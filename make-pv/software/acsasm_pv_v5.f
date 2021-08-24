      SUBROUTINE PV_UKA (OUT_L, NL, PV, U, V, P_TH,                     00010004
     + NX, NY, GRID, NXW, NYW, GRIDW, ITIME,                            00020004
     + LUN, LUN_PRT, R_MDI, WORK, NWORK, IOUTL, IVERT,                  00030004
     + IERROR, MESAGE)                                                  00040004
CL    Calculate Ertel potential vorticity and wind and pressure fields  00050004
CL    on isentropic surfaces for a staggered Arakawa B grid, from       00060004
CL    input fields on standard UARS levels from 1000 mb to 0.32 mb.     00070004
CL    Alternatively, PV may be calculated on P surfaces; note that,     00080004
CL    in that case, vorticity is calculated on P (not theta) levels.    00090004
CL I    OUT_L(NL)             Output level(s) required                  00100004
CL                            If IOUTL=1, these are potential temps (K);00110004
CL                            if IOUTL=2, these are pressures (mb)      00120004
CL I    NL                    No. Theta values required                 00130004
CL O    PV(NX,NY,NL)          Output potential vorticity                00140004
CL                            (for UKMO analyses we currently have      00150004
CL                            NX*NY = 7008, so allow NL*7008 words      00160004
CL                            for PV; similarly for U, V and P)         00170004
CL O    U (NXW,NYW,NL)        Output zonal wind component               00180004
CL O    V (NXW,NYW,NL)        Output meridional wind component          00190004
CL O    P_TH (NX,NY,NL)       Output Pressure on theta surface          00200004
CL                            (or, if IOUTL=2, theta on P surface)      00210004
CL O    NX, NY                Dimensions of PV, P fields (columns, rows)00220004
CL O    GRID(4)               1st lat, Dlat, 1st lon, Dlon mass grid (as00230004
CL                            used for PV and P fields)                 00240004
CL O    NXW, NYW              Dimensions of U, V fields (columns, rows) 00250004
CL O    GRIDW(4)              1st lat, Dlat, 1st lon, Dlon wind grid    00260004
CL O    ITIME(4)              Year, month, day, hour of field           00270004
CL I    LUN                   Unit number from which fields are read    00280004
CL I    LUN_PRT               Unit number for printed messages (if > 0) 00290004
CL O    R_MDI                 Missing data indicator                    00300004
CL O    WORK(NWORK)           Work space                                00310004
CL I    NWORK                 Number of words of workspace allowed      00320004
CL                            (must be at least (NP+3*NL)*NX*NY; for    00330004
CL                            UKMO analyses we currently use NP=22,     00340004
CL                            NX*NY=7008 so NWORK >= 154176 + NL*21024) 00350004
CL I    IOUTL                 Output level type:                        00360004
CL                            1 - output on theta levels;               00370004
CL                            2 - output on pressure levels             00380004
CL I    IVERT                 Vertical differencing mode for dTh/dP:    00390004
CL                            1 linear,                                 00400004
CL                            2 cubic,                                  00410004
CL                            3 deep linear;                            00420004
CL                            +10 if calculated as                      00430004
CL                                1/P (Delta_Theta/Delta_logP)          00440004
CL O    IERROR                Error indicated by non-zero value         00450004
CL O    MESAGE                Error message                             00460004
C                                                                       00470004
      IMPLICIT NONE                                                     00480004
      INTEGER NL, NWORK                                                 00490004
      INTEGER NX, NY                                                    00500004
      INTEGER NXW, NYW                                                  00510004
      REAL OUT_L(NL)                                                    00520004
      REAL PV(*), U(*), V(*), P_TH(*)                                   00530004
      REAL WORK(*)                                                      00540004
      REAL GRID(4)                                                      00550004
      REAL GRIDW(4)                                                     00560004
      REAL R_MDI                                                        00570004
      INTEGER ITIME(4)                                                  00580004
      INTEGER LUN, LUN_PRT                                              00590004
      INTEGER IERROR                                                    00600004
      INTEGER IOUTL                                                     00610004
      INTEGER IVERT                                                     00620004
      CHARACTER*256 MESAGE                                              00630004
C                                                                       00640004
      INTEGER NLEVP                                                     00650004
      PARAMETER (NLEVP=22)                                              00660004
      REAL P_UARS(NLEVP)                                                00670004
     + /1000.0, 681.3, 464.2, 316.2, 215.4, 146.8,                      00680004
     +  100.0, 68.13, 46.42, 31.62, 21.54, 14.68,                       00690004
     +  10.00, 6.813, 4.642, 3.162, 2.154, 1.468,                       00700004
     +  1.000, .6813, .4642, .3162/                                     00710004
C                                                                       00720004
      EXTERNAL PV_X                                                     00730004
C                                                                       00740004
CL*** 1       Call PV_X specifying UARS levels                          00750004
      IERROR=0                                                          00760004
      MESAGE='    '                                                     00770004
C                                                                       00780004
      CALL PV_X (OUT_L, NL, P_UARS, NLEVP, PV, U, V, P_TH,              00790004
     + NX, NY, GRID, NXW, NYW, GRIDW, ITIME,                            00800004
     + LUN, LUN_PRT, R_MDI, WORK, NWORK, IOUTL, IVERT,                  00810004
     + IERROR, MESAGE)                                                  00820004
C                                                                       00830004
CL*** 2       Print Error message if LUN_PRT is set                     00840004
      IF(LUN_PRT.GT.0) THEN                                             00850004
        CALL UKAPRT(MESAGE, LUN_PRT, 80)                                00860004
      END IF                                                            00870004
      RETURN                                                            00880004
      END                                                               00890004
      SUBROUTINE PV_X (OUT_L, NL, F_P, NP, PV, U, V, P_TH,              00010007
     + NX, NY, GRID, NXW, NYW, GRIDW, ITIME,                            00020000
     + LUN, LUN_PRT, R_MDI, WORK, NWORK,                                00030003
     + IOUTL, IVERT, IERROR, MESAGE)                                    00040003
CL    Calculate either                                                  00050003
CL      Ertel potential vorticity, wind and pressure fields on          00060009
CL      Theta surfaces                                                  00070003
CL    or                                                                00080003
CL      Ertel potential vorticity, wind and theta fields                00090009
CL      PRESSURE surfaces (NB the voricity is calculated on P-surfaces  00100003
CL      not theta-surfaces)                                             00110003
CL    for a staggered Arakawa B grid.                                   00120003
CL I    OUT_L (NL)            Output theta/pressure levels required     00130003
CL                            (depends on IOUTL)                        00140003
CL I    NL                    No. output level values                   00150003
CL I    F_P(NP)               Pressure levels of input fields           00160000
CL I    NP                    Number of input levels                    00170000
CL O    PV(NX,NY,NL)          Output potential vorticity                00180003
CL O    U (NXW,NYW,NL)        Output U-wind                             00190003
CL O    V (NXW,NYW,NL)        Output V-wind                             00200003
CL O    P_TH (NX,NY,NL)       Output P on theta levels or theta on P    00210003
CL O    NX, NY                Dimensions of PV, P fields (columns, rows)00220000
CL O    GRID(4)               1st lat, Dlat, 1st lon, Dlon mass grid    00230000
CL O    NXW, NYW              Dimensions of U, V fields (columns, rows) 00240000
CL O    GRIDW(4)              1st lat, Dlat, 1st lon, Dlon wind grid    00250000
CL O    ITIME(4)              Yr, mon, day, hr of field                 00260000
CL I    LUN                   Unit number from which fields are read    00270000
CL I    LUN_PRT               Unit number for printed messages (if > 0) 00280000
CL O    R_MDI                 Missing data indicator                    00290000
CL O    WORK(NWORK)           Work space                                00300000
CL I    NWORK                 Number of words of workspace allowed      00310000
CL                            (must be at least (NP+3*NL)*NX*NY)        00320005
CL I    IOUTL                 Output level type                         00330003
CL                            1 - output on theta levels                00340003
CL                            2 - output on pressure levels             00350003
CL I    IVERT                 Vertical differencing mode                00360003
CL                            1 - linear; 2 - cubic; 3 - "deep linear"  00370012
CL O    IERROR                Error indicated by non-zero value         00380000
CL O    MESAGE                Error message                             00390000
C                                                                       00400000
C  The work area is split into the following horizontal fields          00410000
C     No.      Start   Used for                                         00420000
C     1 to NP  1       3-D data read from input file                    00430000
C     NP+1     IA1     Theta in terms of input level number (mass grid) 00440000
C                      vorticity                                        00450000
C     NP+2     IA2     dTheta/dp                                        00460000
C     NP+3     IA3     Theta in terms of input level number (wind grid) 00470000
C                      Workspace for PV_DVB                             00480000
C     The last three fields are repeated NL times                       00490004
C     The first field of PV is used for workspace for PV_STAG           00500000
C     and (if IOUTL=2) to pass Pressure from PV_PTH to PV_DTHDP         00510004
C                                                                       00520000
      IMPLICIT NONE                                                     00530000
      INTEGER NLMAX                                                     00540003
      PARAMETER (NLMAX=30)                                              00550003
      INTEGER NL, NP, NWORK                                             00560003
      INTEGER NX, NY                                                    00570000
      INTEGER NXW, NYW                                                  00580000
      REAL OUT_L(NL)                                                    00590003
      REAL F_P(NP)                                                      00600000
      REAL PV(*), U(*), V(*), P_TH(*)                                   00610006
      REAL WORK(*)                                                      00620000
      REAL GRID(4)                                                      00630000
      REAL GRIDW(4)                                                     00640000
      REAL R_MDI                                                        00650000
      INTEGER ITIME(4)                                                  00660000
      INTEGER LUN, LUN_PRT                                              00670000
      INTEGER IOUTL                                                     00680006
      INTEGER IVERT                                                     00681006
      INTEGER IERROR                                                    00690000
      CHARACTER*256 MESAGE                                              00700000
C                                                                       00710000
      REAL RADIUS, DEG_TO_R, OMEGA, GRAVITY, R_KAPPA                    00720000
      INTEGER NDMAX, ICHECK, JL                                         00730009
      INTEGER IESAVE                                                    00740000
      CHARACTER*256 MESAVE                                              00750000
      INTEGER IA1(NLMAX), IA2(NLMAX), IA3(NLMAX)                        00760003
      INTEGER IOUT(NLMAX), IOUTW(NLMAX)                                 00770003
      CHARACTER*12 TYPE                                                 00780000
C                                                                       00790000
      DATA RADIUS /6.371229E+06/, OMEGA /7.292116E-05/,                 00800000
     + GRAVITY /9.80665/, R_KAPPA/0.2857/                               00810000
C                                                                       00820000
      EXTERNAL PV_R3D, PV_LTH, PV_DTHDP, PV_PTH,                        00830009
     + PV_DVB, PV_STAG, PV_FVINT, PV_VG2PV                              00840000
C                                                                       00850000
CL*** 1       Preliminaries                                             00860000
      IERROR=0                                                          00870000
      IESAVE=0                                                          00880000
      MESAGE='    '                                                     00890000
      MESAVE='    '                                                     00900000
      DEG_TO_R=ASIN(1.)/90.0                                            00910000
C---- Check NL is within limits                                         00920003
      IF(NL.GT.NLMAX) THEN                                              00930003
        IERROR=402                                                      00940000
        WRITE(MESAGE,101) NL, NLMAX, IERROR                             00950003
101     FORMAT(' PV_X - Too many output levels requested;'/             00960003
     + ' Requested, max NL: ',2I10,';',                                 00970003
     + ' Error code =',I10)                                             00980000
        RETURN                                                          00990000
      END IF                                                            01000000
      NDMAX=NWORK/(NP+3*NL)                                             01010003
C---- Check IOUTL is valid                                              01020003
      IF (IOUTL.LT.1 .OR. IOUTL.GT.2) THEN                              01030003
        IERROR=403                                                      01040010
        WRITE(MESAGE,102) IOUTL, IERROR                                 01050003
102     FORMAT(' PV_X - Invalid output level type: ',I10,';',           01060003
     + ' Error code =',I10)                                             01070000
        RETURN                                                          01080000
      END IF                                                            01090000
C---- (IVERT is checked in PV_DTHDP)                                    01100010
C                                                                       01180000
CL*** 2       Process temperatures                                      01190000
CL    2.1     Get 3-D temperature field                                 01200000
      TYPE='TEMP'                                                       01210000
      ICHECK=0                                                          01220000
      CALL PV_R3D(WORK(1), F_P, NP, NDMAX, NX, NY,                      01230000
     + TYPE, LUN, GRID, ITIME, R_MDI, ICHECK, IERROR, MESAGE)           01240000
      IF(IERROR.GT.0) THEN                                              01250000
        GO TO 999                                                       01260000
      ELSE IF(IERROR.LT.0) THEN                                         01270000
        IESAVE=IERROR                                                   01280000
        MESAVE=MESAGE                                                   01290000
      END IF                                                            01300000
      IF(LUN_PRT.GT.0) WRITE(LUN_PRT,211)                               01310000
211   FORMAT(' Temperature fields successfully read')                   01320000
C                                                                       01330000
CL    2.2     From grid size, set up workspace addresses                01340000
C---- reset max field size to no. mass grid points                      01350000
      NDMAX=NX*NY                                                       01360000
      DO 220 JL=1,NL                                                    01370009
C---- Set up addresses (in WORK) of 2-D work fields                     01380000
      IA1(JL)=(NP+3*(JL-1))*NDMAX+1                                     01390009
      IA2(JL)=IA1(JL)+NDMAX                                             01400009
      IA3(JL)=IA2(JL)+NDMAX                                             01410009
C---- Set up addresses in output arrays for output fields               01420000
      IOUT(JL)=(JL-1)*NDMAX+1                                           01430009
C---- For wind fields, there is one less row                            01440000
      IOUTW(JL)=(JL-1)*NX*(NY-1)+1                                      01450009
220   CONTINUE                                                          01460000
C                                                                       01470000
CL    2.3     Process T (theta-level output)                            01480004
C                                                                       01490000
      IF (IOUTL.EQ.1) THEN                                              01500003
        DO JL=1,NL                                                      01510009
C                                                                       01520004
C     2.3.1   Locate output theta surface                               01530004
          CALL PV_LTH(OUT_L(JL), WORK(1), F_P,                          01540009
     +     WORK(IA1(JL)), P_TH(IOUT(JL)),                               01550009
     +     NX, NY, NP, R_MDI, R_KAPPA)                                  01560003
C                                                                       01570004
C     2.3.2   Calculate dTheta/dp                                       01580004
          CALL PV_DTHDP (WORK(1), F_P,                                  01590004
     +     WORK(IA1(JL)), P_TH(IOUT(JL)), WORK(IA2(JL)),                01600009
     +     NX, NY, NP, R_MDI, R_KAPPA, IVERT, IERROR, MESAGE)           01610004
          IF(IERROR.GT.0) GO TO 999                                     01620004
        END DO                                                          01630003
C                                                                       01640004
CL    2.4     Process T (P-level output)                                01650004
C                                                                       01660004
      ELSE                                                              01670003
        DO JL=1,NL                                                      01680009
C                                                                       01690004
C     2.4.1   Get output P in terms of input P, and theta at output P   01700004
          CALL PV_PTH(OUT_L(JL), WORK(1), F_P,                          01710009
     +     WORK(IA1(JL)), PV(1), P_TH(IOUT(JL)),                        01720009
     +     NX, NY, NP, R_MDI, R_KAPPA, IERROR, MESAGE)                  01730011
          IF(IERROR.GT.0) GO TO 999                                     01731011
C                                                                       01740004
C     2.4.2   Calculate dTheta/dp                                       01750004
          CALL PV_DTHDP (WORK(1), F_P,                                  01760004
     +     WORK(IA1(JL)), PV(1), WORK(IA2(JL)),                         01770009
     +     NX, NY, NP, R_MDI, R_KAPPA, IVERT, IERROR, MESAGE)           01780004
          IF(IERROR.GT.0) GO TO 999                                     01790004
        END DO                                                          01800003
      END IF                                                            01810006
C                                                                       01820000
      IF(LUN_PRT.GT.0) WRITE(LUN_PRT,241)                               01830000
241   FORMAT(' Temperature fields successfully processed')              01840000
C                                                                       01850000
CL*** 3       Process winds                                             01860000
CL    3.1     determine theta level on wind grid points                 01870000
C                                                                       01880000
      DO JL=1,NL                                                        01890009
        CALL PV_STAG (WORK(IA1(JL)), WORK(IA3(JL)), PV(1),              01900009
     +   1, NX, NY, GRID,                                               01910005
     +   NXW, NYW, GRIDW)                                               01920005
      END DO                                                            01930005
C                                                                       01940000
CL    3.2     Get 3-D u-wind field                                      01950000
      TYPE='ZONWIN_P'                                                   01960008
      ICHECK=2                                                          01970000
      CALL PV_R3D(WORK(1), F_P, NP, NDMAX, NXW, NYW,                    01980000
     + TYPE, LUN, GRIDW, ITIME, R_MDI, ICHECK, IERROR, MESAGE)          01990000
      IF(IERROR.GT.0) THEN                                              02000000
        GO TO 999                                                       02010000
      ELSE IF(IERROR.LT.0) THEN                                         02020000
        IESAVE=IERROR                                                   02030000
        MESAVE=MESAGE                                                   02040000
      END IF                                                            02050000
      IF(LUN_PRT.GT.0) WRITE(LUN_PRT,321)                               02060000
321   FORMAT(' U wind fields successfully read')                        02070000
C                                                                       02080000
CL    3.3     get u-wind at required Theta                              02090000
C                                                                       02100000
      DO 330 JL=1,NL                                                    02110009
      CALL PV_FVINT (WORK(1), WORK(IA3(JL)), U(IOUTW(JL)),              02120009
     + NXW, NYW, NP)                                                    02130000
330   CONTINUE                                                          02140000
C                                                                       02150000
CL    3.4     Get 3-D v-wind field                                      02160000
      TYPE='MERWIN_P'                                                   02170008
      ICHECK=2                                                          02180000
      CALL PV_R3D(WORK(1), F_P, NP, NDMAX, NXW, NYW,                    02190000
     + TYPE, LUN, GRIDW, ITIME, R_MDI, ICHECK, IERROR, MESAGE)          02200000
      IF(IERROR.GT.0) THEN                                              02210000
        GO TO 999                                                       02220000
      ELSE IF(IERROR.LT.0) THEN                                         02230000
        IESAVE=IERROR                                                   02240000
        MESAVE=MESAGE                                                   02250000
      END IF                                                            02260000
      IF(LUN_PRT.GT.0) WRITE(LUN_PRT,341)                               02270000
341   FORMAT(' V wind fields successfully read')                        02280000
C                                                                       02290000
CL    3.5     get v-wind at required Theta                              02300000
C                                                                       02310000
      DO 350 JL=1,NL                                                    02320009
      CALL PV_FVINT (WORK(1), WORK(IA3(JL)), V(IOUTW(JL)),              02330009
     + NXW, NYW, NP)                                                    02340000
350   CONTINUE                                                          02350000
C                                                                       02360000
CL*** 4       Calculate potential vorticity                             02370000
CL    4.1     calculate relative vorticity (on mass grid)               02380000
C                                                                       02390000
      DO 410 JL=1,NL                                                    02400009
      CALL PV_DVB (U(IOUTW(JL)), V(IOUTW(JL)), WORK(IA1(JL)),           02410009
     + WORK(IA3(JL)), 2, NXW, NYW, GRIDW,                               02420009
     + RADIUS, DEG_TO_R, R_MDI, IERROR, MESAGE)                         02430000
      IF(IERROR.GT.0) GO TO 999                                         02440000
410   CONTINUE                                                          02450000
C                                                                       02460000
CL    4.2     potential vorticity from relative vort. and dTheta/dp     02470000
C                                                                       02480000
      DO 420 JL=1,NL                                                    02490009
      CALL PV_VG2PV (WORK(IA1(JL)), WORK(IA2(JL)), PV(IOUT(JL)),        02500009
     + NX, NY, GRID,                                                    02510000
     + OMEGA, GRAVITY, DEG_TO_R, R_MDI)                                 02520000
420   CONTINUE                                                          02530000
C                                                                       02540000
C     Return any warning error saved if IERROR is still zero            02550000
      IF(IERROR.EQ.0) THEN                                              02560000
        IERROR=IESAVE                                                   02570000
        MESAGE=MESAVE                                                   02580000
      END IF                                                            02590000
C                                                                       02600000
C     Set message for successful completion if no error detected        02610000
      IF(IERROR.EQ.0)                                                   02620000
     + MESAGE='Potential vorticity successfully calculated'             02630000
C                                                                       02640000
999   CONTINUE                                                          02650000
C                                                                       02660000
      RETURN                                                            02670000
      END                                                               02680000
      SUBROUTINE PV_R3D(A3D, F_P, NL, NDMAX, NX, NY,                    03040000
     + TYPE, LUN, GRID, ITIME, R_MDI, ICHECK, IERROR, MESAGE)           03050000
CL*** Subroutine to read in fields (using UKAFLD access routine),       03060000
CL*** to set up a three-dimensional array                               03070000
CL O    A3D        Three-dimensional array of grid-point values         03080000
CL I    F_P        List of NL field pressure levels (in decreasing      03090000
CL I               order) - in mb                                       03100000
CL I    NL         Number of field levels                               03110000
CL I    NDMAX      Maximum length of field for horiz. array dimensions  03120000
CL I/O  NX, NY     Actual dimensions of field (columns, rows)           03130000
CL             NB. Returned array of field values is stored as          03140000
CL                 A3D(NX,NY,NL), and NOT A3D(NDMAX,NL).                03150000
CL I    TYPE       Data (sub)type to be read in                         03160000
CL I    LUN        Unit number from which fields are read               03170000
CL I/O  GRID       1st lat, Dlat, 1st lon, Dlon of field grid           03180000
CL I/O  ITIME      Yr, mon, day, hr of field                            03190000
CL O    R_MDI      Missing data indicator                               03200000
CL I    ICHECK   0 indicates no grid/time checking                      03210000
CL               1 indicates that the time parameters (ITIME)           03220000
CL                 are to be checked against input arguments.           03230000
CL               2 indicates that both the grid and time parameters     03240000
CL                 (i.e. all of NX, NY, GRID and ITIME) are to be       03250000
CL                 checked against input arguments.                     03260000
CL                 (Note that, irrespective of ICHECK, the first field  03270000
CL                 read is used to check subsequent fields read by      03280000
CL                 the same call to PV_R3D).                            03290000
CL O    IERROR     Non-zero indicates error                             03300000
CL                 (1-400 from access routines, <0 are non-fatal;       03310000
CL                 401-500 from processing routines)                    03320000
CL O    MESAGE     Error message for non-zero IERROR                    03330000
CL    This routine searches for fields of type TYPE on the pressure     03340000
CL    levels in array F_P; they are built up into a three-dimensional   03350000
CL    array A3D.                                                        03360000
      IMPLICIT NONE                                                     03370000
      INTEGER NX, NY, NDMAX, NL, LUN                                    03380000
      REAL R_MDI                                                        03390000
      REAL A3D(NDMAX*NL)                                                03400000
      REAL F_P(NL)                                                      03410000
      CHARACTER*12 TYPE                                                 03420000
      REAL GRID(4)                                                      03430000
      INTEGER ITIME(4)                                                  03440000
      INTEGER ICHECK                                                    03450000
      INTEGER IERROR                                                    03460000
      CHARACTER*256 MESAGE                                              03470000
C                                                                       03480000
      LOGICAL ILFRST                                                    03490000
      INTEGER J, JL, IADD, INX, INY, IICHCK                             03500000
      INTEGER IITIME(4)                                                 03510000
      REAL ZGRID(4)                                                     03520000
      DATA ILFRST/.TRUE./                                               03530000
      SAVE ILFRST                                                       03540000
C                                                                       03550000
CL*** 1       Preliminaries                                             03560000
      IERROR=0                                                          03570000
      MESAGE='    '                                                     03580000
      IICHCK=MIN(ICHECK,2)                                              03590000
      IICHCK=MAX(IICHCK,0)                                              03600000
      IF(ILFRST) IICHCK=0                                               03610000
      ILFRST=.FALSE.                                                    03620000
      IADD=1                                                            03630000
C                                                                       03640000
CL*** 2       Read in fields level by level                             03650000
      DO 290 JL=1,NL                                                    03660000
C                                                                       03670000
C---- 2.1     Read in field using UKAFIELD                              03680000
      CALL UKAFLD(LUN,TYPE,F_P(JL),                                     03690000
     + A3D(IADD), INY, INX, ZGRID, IITIME, R_MDI, IERROR, MESAGE)       03700000
      IF(IERROR.GT.0) RETURN                                            03710000
C                                                                       03720000
C---- 2.2     Do grid checks if required                                03730000
C---- Check array size always                                           03740000
      IF(INX*INY.GT.NDMAX) THEN                                         03750000
        IERROR=410                                                      03760000
        WRITE(MESAGE,221)                                               03770000
     +  TYPE, F_P(1), INX, INY, INX*INY, NDMAX, IERROR                  03780000
221     FORMAT(' PV_R3D - Size too big for type/level ',A12,F10.4,';',  03790000
     + ' Columns (NX), rows (NY), NX*NY, max size : ',4I10,';',         03800000
     + ' Error code =',I10)                                             03810000
        RETURN                                                          03820000
      END IF                                                            03830000
C---- Check consistency of grid if required                             03840000
      IF(IICHCK.EQ.2) THEN                                              03850000
        IF(INX.NE.NX.OR.INY.NE.NY) THEN                                 03860000
          IERROR=411                                                    03870000
          WRITE(MESAGE,223)                                             03880000
     +    TYPE, F_P(JL), INX, NX, INY, NY, IERROR                       03890000
223       FORMAT(' PV_R3D - Size inconsistent for type/level ',         03900000
     +    A12,F10.4,';',                                                03910000
     +   ' Current, previous (columns, rows): ',4I10,';',               03920000
     +   ' Error code =',I10)                                           03930000
          RETURN                                                        03940000
        END IF                                                          03950000
        DO 226 J=1,4                                                    03960000
        IF(ABS(ZGRID(J)-GRID(J)).GT.0.01) THEN                          03970000
          IERROR=412                                                    03980000
          WRITE(MESAGE,225)                                             03990000
     +    TYPE, F_P(JL), ZGRID, GRID, IERROR                            04000000
225      FORMAT(' PV_R3D - Grid inconsistent for type/level ',          04010000
     +    A12 ,F10.4,';',                                               04020000
     +   ' Current, previous grid data: ', 8F10.4,';',                  04030000
     +   ' Error code =',I10)                                           04040000
          RETURN                                                        04050000
        END IF                                                          04060000
226     CONTINUE                                                        04070000
      ELSE                                                              04080000
C----   Copy grid parameters, if no checking                            04090000
        NX=INX                                                          04100000
        NY=INY                                                          04110000
        DO 228 J=1,4                                                    04120000
        GRID(J)=ZGRID(J)                                                04130000
228     CONTINUE                                                        04140000
      END IF                                                            04150000
C                                                                       04160000
C---- 2.3     Do time checks if required                                04170000
      IF(IICHCK.GE.1) THEN                                              04180000
        DO 234 J=1,4                                                    04190000
        IF(IITIME(J).NE.ITIME(J)) THEN                                  04200000
          IERROR=413                                                    04210000
          WRITE(MESAGE,233)                                             04220000
     +    TYPE, F_P(JL), IITIME, ITIME, IERROR                          04230000
233       FORMAT(' PV_R3D - Time inconsistent for type/level ',         04240000
     +    A12,F10.4,';',                                                04250000
     +   ' Current, previous times: ', 8F10.4,';',                      04260000
     +   ' Error code =',I10)                                           04270000
          RETURN                                                        04280000
        END IF                                                          04290000
234     CONTINUE                                                        04300000
      ELSE                                                              04310000
C----   Else copy times                                                 04320000
        DO 238 J=1,4                                                    04330000
        ITIME(J)=IITIME(J)                                              04340000
238     CONTINUE                                                        04350000
      END IF                                                            04360000
C                                                                       04370000
C---- 2.4     Update address in A3D for next field, reset IICHCK        04380000
      IADD=IADD+NX*NY                                                   04390000
      IICHCK=2                                                          04400000
C                                                                       04410000
290   CONTINUE                                                          04420000
C                                                                       04430000
      RETURN                                                            04440000
      END                                                               04450000
      SUBROUTINE PV_LTH (THETA, T_IN, F_P, TH_LEVEL, TH_P,                      
     + NX, NY, NP, R_MDI, R_KAPPA)                                              
CL*** Subroutine to find the position of a theta surface in terms of            
CL*** the input field levels                                                    
CL I    THETA      Potential temperature (theta) to be located                  
CL I    T_IN       3-D input field of temperature                               
CL I    F_P        List of field pressure levels (in decreasing order)          
CL O    TH_LEVEL   Theta surface in terms of field levels (a value of           
CL                 n.fff indicates it is between level n and n+1;               
CL                 0.fff is the vertical interpolation weight)                  
CL O    TH_P       Theta surface in terms of pressure, assuming Theta           
CL                 varies linearly with log(pressure); set to missing           
CL                 data (R_MDI) if outside range of input levels.               
CL I    NX, NY     Horizontal dimensions of field (columns, rows)               
CL I    NP         Number of field levels                                       
CL I    R_MDI      Missing data indicator                                       
CL I    R_KAPPA    Ratio of specific heats for air                              
CL    This takes the 3-D temperature array T_IN (on pressure levels             
CL    given in array F_P) and determines the level of the required              
CL    isentropic surface at each grid point.                                    
CL    When the isentropic surface is outside the range of F_P, TH_LEVEL         
CL    is set to 1 or NP, as appropriate and TH_P to missing data (R_MDI)        
      IMPLICIT NONE                                                             
      INTEGER IMAXL                                                             
      PARAMETER (IMAXL=50)                                                      
C                                                                               
      INTEGER NX, NY, NP                                                        
      REAL THETA, R_MDI                                                         
      REAL T_IN(NX*NY,NP), TH_LEVEL(NX*NY), TH_P(NX*NY)                         
      REAL F_P(NP)                                                              
      REAL R_KAPPA                                                              
C                                                                               
      INTEGER J, JP                                                             
      REAL ZCONV(IMAXL), ZLOGP(IMAXL)                                           
      REAL Z, ZNL, ZJL                                                          
      REAL ZLPU, ZLPL, ZTHU, ZTHL                                               
C                                                                               
CL*** 0       Set up T to theta conversion factors                              
      DO JP=1,NP                                                                
        ZCONV(JP)=(1000.0/F_P(JP))**R_KAPPA                                     
        ZLOGP(JP)=LOG(F_P(JP))                                                  
      END DO                                                                    
C                                                                               
CL*** 1       Process field at lowest input level                               
      DO 390 J=1,NX*NY                                                          
C---- 1.1     Convert temperature to potential temperature                      
        ZTHU=ZCONV(1)*T_IN(J,1)                                                 
C---- 1.2     Set up TH_LEVEL and TH_P                                          
C---- Where required Theta < theta at lowest level, set TH_LEVEL to 1.0         
C---- and TH_P to missing data                                                  
        IF(THETA.LT.ZTHU) THEN                                                  
          TH_LEVEL(J)=1.0                                                       
          TH_P(J)=R_MDI                                                         
        END IF                                                                  
C                                                                               
CL*** 2       Process further levels                                            
C---- 2.1     Start loop over levels                                            
C---- Within loop, we are processing P between F_P(JP-1) and F_P(JP)            
        DO 290 JP=2,NP                                                          
          ZJL=REAL(JP-1)                                                        
C---- 2.2     Theta and log(p) at lower and upper levels                        
          ZLPL=ZLOGP(JP-1)                                                      
          ZLPU=ZLOGP(JP)                                                        
          ZTHL=ZTHU                                                             
          ZTHU=ZCONV(JP)*T_IN(J,JP)                                             
C---- 2.3     Set up TH_LEVEL and TH_P                                          
C---- Set when reqd Theta > theta below and < theta above                       
C---- (at this stage TH_P is set to log(p))                                     
          IF(THETA.GE.ZTHL.AND.THETA.LE.ZTHU) THEN                              
            Z=(THETA-ZTHL)/(ZTHU-ZTHL)                                          
            TH_LEVEL(J)=ZJL+Z                                                   
            TH_P(J)=ZLPL+Z*(ZLPU-ZLPL)                                          
          END IF                                                                
290     CONTINUE                                                                
C                                                                               
CL*** 3       Finish off                                                        
C---- 3.1     Set up TH_LEVEL for Theta>highest level                           
C---- Where required Theta > theta at highest level, TH_LEVEL = NP              
        ZNL=REAL(NP)                                                            
        IF(THETA.GT.ZTHU) THEN                                                  
          TH_LEVEL(J)=ZNL                                                       
          TH_P(J)=R_MDI                                                         
        END IF                                                                  
C---- 3.2     Convert TH_P from log(pressure) to pressure                       
        IF(TH_P(J).NE.R_MDI) THEN                                               
          TH_P(J)=EXP(TH_P(J))                                                  
        ENDIF                                                                   
390   CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PV_PTH (PROUT, T_IN, F_P, P_LEVEL, P_P, P_TH,                  
     + NX, NY, NP, R_MDI, R_KAPPA, IERROR, MESAGE)                              
CL*** Subroutine to find the position of an output pressure level in            
CL*** terms of the input field levels, and to return theta at that              
CL*** level                                                                     
CL I    PROUT      Output pressure level                                        
CL I    T_IN       3-D input field of temperature                               
CL I    F_P        List of field pressure levels (in decreasing order)          
CL O    P_LEVEL    P surface in terms of field levels (a value of               
CL                 n.fff indicates it is between level n and n+1;               
CL                 0.fff is the vertical interpolation weight)                  
CL                 (for compatibility with PV_LTH, a whole field of             
CL                 identical values is returned).                               
CL O    P_P        Field of output pressure (for compatibility with             
CL                 PV_LTH; a whole field of identical values is                 
CL                 returned, set to R_MDI if outside input range).              
CL O    P_TH       Theta at the output pressure level (T is interpolated        
CL                 linearly with log(pressure)); set to missing                 
CL                 data (R_MDI) if outside range of input levels.               
CL I    NX, NY     Horizontal dimensions of field (columns, rows)               
CL I    NP         Number of field levels                                       
CL I    R_MDI      Missing data indicator                                       
CL I    R_KAPPA    Ratio of specific heats for air                              
CL O    IERROR     Error indicator                                              
CL O    MESAGE     Error message                                                
CL    When the output level is outside the range of F_P, P_LEVEL is set         
CL    to 1 or NP, as appropriate and P_TH to missing data (R_MDI)               
      IMPLICIT NONE                                                             
      INTEGER IMAXL                                                             
      PARAMETER (IMAXL=50)                                                      
C                                                                               
      INTEGER NX, NY, NP                                                        
      REAL PROUT, R_MDI                                                         
      REAL T_IN(NX*NY,NP), P_LEVEL(NX*NY), P_P(NX*NY), P_TH(NX*NY)              
      REAL F_P(NP)                                                              
      REAL R_KAPPA                                                              
      INTEGER IERROR                                                            
      CHARACTER*256 MESAGE                                                      
C                                                                               
      INTEGER J, JP                                                             
      INTEGER IILEV                                                             
      REAL ZCONV, ZLOGP(IMAXL)                                                  
      REAL ZLPU, ZLPL, ZLP, ZTU, ZTL, ZT                                        
      REAL ZFRACT, ZPLEV                                                        
      LOGICAL ILMISS                                                            
C                                                                               
CL*** 1       Calculate weighting and T-to-theta conversion                     
      IERROR=0                                                                  
C                                                                               
      DO JP=1,NP                                                                
        ZLOGP(JP)=LOG(F_P(JP))                                                  
      END DO                                                                    
C                                                                               
      ZCONV=(1000./PROUT)**R_KAPPA                                              
C                                                                               
      ILMISS = .TRUE.                                                           
      ZLP=LOG(PROUT)                                                            
      IF (PROUT.GT.F_P(1)) THEN                                                 
        ZPLEV=1.0                                                               
      ELSE IF (PROUT.LT.F_P(NP)) THEN                                           
        ZPLEV=REAL(NP)                                                          
      ELSE                                                                      
        DO JP=1,NP-1                                                            
          ZLPL=LOG(F_P(JP))                                                     
          ZLPU=LOG(F_P(JP+1))                                                   
          IF (ZLP.LE.ZLPL .AND. ZLP.GE.ZLPU) THEN                               
            IILEV=JP                                                            
            ZFRACT=(ZLP-ZLPL)/(ZLPU-ZLPL)                                       
            ZPLEV=REAL(IILEV)+ZFRACT                                            
            ILMISS=.FALSE.                                                      
          END IF                                                                
        END DO                                                                  
        IF (ILMISS) THEN                                                        
          IERROR=440                                                            
          WRITE(MESAGE,121) PROUT, F_P(NP), F_P(1)                              
121       FORMAT(' PV_PTH - P consistency failure',                             
     +      ' ; P reqd, top, bottom ', 3F10.4,                                  
     +      ' ; Error code = ',I10)                                             
          RETURN                                                                
        END IF                                                                  
      END IF                                                                    
C                                                                               
CL*** 2       Set up output fields P_LEVEL P_P and P_TH                         
C---- 2.1     Deal with out-of-range levels                                     
      IF (ILMISS) THEN                                                          
        DO J=1,NX*NY                                                            
          P_LEVEL(J)=ZPLEV                                                      
          P_P(J) =R_MDI                                                         
          P_TH(J)=R_MDI                                                         
        END DO                                                                  
      ELSE                                                                      
C                                                                               
C---- 2.2     Levels within range                                               
        DO J=1,NX*NY                                                            
          P_LEVEL(J)=ZPLEV                                                      
          P_P(J)=PROUT                                                          
          ZTL=T_IN (J,IILEV)                                                    
          ZTU=T_IN (J,IILEV+1)                                                  
          ZT=ZTL+ZFRACT*(ZTU-ZTL)                                               
          P_TH(J)=ZCONV*ZT                                                      
        END DO                                                                  
      END IF                                                                    
C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PV_DTHDP (T_IN, F_P, SF_LEVEL, SF_P, TH_GRAD,          00010000
     + NX, NY, NP, R_MDI, R_KAPPA, IVERT, IERROR, MESAGE)               00020009
CL*** Subroutine to find the gradient of Theta with respect to Pressure 00030000
CL*** on a specified surface.                                           00040000
CL I    T_IN       3-D input field of temperature                       00050000
CL I    F_P        List of field pressure levels (in decreasing order)  00060000
CL I    SF_LEVEL   Surface in terms of field levels (a value of         00070000
CL                 n.fff indicates it is between level n and n+1;       00080000
CL                 0.fff is the vertical interpolation weight)          00090000
CL I    SF_P       Surface in terms of pressure; set to missing data    00100000
CL                 (R_MDI) if outside range of input levels.            00110000
CL O    TH_GRAD    Calculated field of Dtheta/Dp.                       00120000
CL I    NX, NY     Horizontal dimensions of field (columns, rows)       00130000
CL I    NP         Number of field levels                               00140000
CL I    R_MDI      Missing data indicator                               00150000
CL I    R_KAPPA    Ratio of specific heats for air                      00160000
CL I    IVERT      Calculation mode:                                    00170010
CL                 1 linear,                                            00171012
CL                 2 cubic,                                             00171112
CL                 3 deep linear;                                       00171212
CL                 +10 if calculated as 1/P (Delta_Theta/Delta_logP)    00172010
CL O    IERROR     Error indicator                                      00180001
CL O    MESAGE     Error message                                        00181007
CL    This takes the 3-D temperature array T_IN (on pressure levels     00190000
CL    given in array F_P) and determines Dtheta/Dp for a specified      00200000
CL    surface.  This surface is defined both in terms of input levels   00210000
CL    (field SF_LEVEL) and pressure (SF_P); they MUST be consistent.    00220000
      IMPLICIT NONE                                                     00230000
      INTEGER IMAXL                                                     00240000
      PARAMETER(IMAXL=50)                                               00250000
C                                                                       00260000
      INTEGER NX, NY, NP                                                00270000
      REAL T_IN(NX*NY,NP), SF_LEVEL(NX*NY), SF_P(NX*NY)                 00280000
      REAL TH_GRAD(NX*NY)                                               00290000
      REAL F_P(NP)                                                      00300000
      REAL R_MDI                                                        00310000
      REAL R_KAPPA                                                      00320000
      INTEGER IVERT, IERROR                                             00330009
      CHARACTER*256 MESAGE                                              00331008
C                                                                       00340000
      INTEGER J, JL, II, I, IVERTA                                      00350011
      REAL ZTH1, ZTH2, ZTH3, ZTH4, ZTHU, ZTHL                           00360003
      REAL ZP1, ZP2, ZP3, ZP4, ZPU, ZPL                                 00370003
      REAL ZLP1, ZLP2, ZLP3, ZLP4, ZLPU, ZLPL                           00371011
      REAL ZGU, ZGL, ZGC, ZWU, ZWL, ZWC                                 00380000
      REAL ZF, Z1F                                                      00390000
      REAL ZCONV(IMAXL), ZLOGP(IMAXL)                                   00400011
C                                                                       00410000
CL*** 1       Preliminary section                                       00420000
      IERROR=0                                                          00430000
C---- Set T to Theta conversion factor for each level                   00440000
      IF(NP.GT.IMAXL) THEN                                              00450000
        IERROR=420                                                      00460000
        WRITE(MESAGE,101) NP, IMAXL, IERROR                             00470007
101     FORMAT(' PV_DTHDP - Too many input levels ;',                   00480007
     +  ' No. levels (NP), maximum allowed ',2I10,' ;',                 00490007
     +  ' Error code = ',I10)                                           00491007
        RETURN                                                          00500000
      END IF                                                            00510000
      DO JL=1,NP                                                        00520011
        ZCONV(JL)=(1000.0/F_P(JL))**R_KAPPA                             00530011
        ZLOGP(JL)=LOG(F_P(JL))                                          00531011
      END DO                                                            00540011
      IVERTA=MOD(IVERT,10)                                              00541011
C                                                                       00550000
CL*** 2       Calculate DTh/DP at each point                            00560010
C             ("cubic" or "deep linear" options)                        00561010
C                                                                       00570002
      IF(IVERT.EQ.2 .OR. IVERT.EQ.3 .OR.                                00580010
     +   IVERT.EQ.12.OR. IVERT.EQ.13) THEN                              00581011
C                                                                       00590000
        DO 290 J=1,NX*NY                                                00600001
C                                                                       00610000
C---- 2.1     Get Theta and P values for gradient calculation           00620000
C----   Get Set of four Theta and P values to be used for gradient      00630001
C----   calculation (ZTH# and ZP# respectively)                         00640001
C----   Required pressure level is between ZP2 and ZP3; ZP1 is          00650001
C----   level below, ZP4 is level above.                                00660001
        II=INT(SF_LEVEL(J))                                             00670001
C----   If SF_LEVEL is set to NP, reduce by one                         00680001
        II=MIN(NP-1,II)                                                 00690001
        ZP2=F_P(II)                                                     00700001
        ZLP2=ZLOGP(II)                                                  00701011
        ZTH2=T_IN(J,II)*ZCONV(II)                                       00710001
        I=II+1                                                          00720001
        ZP3=F_P(I)                                                      00730001
        ZLP3=ZLOGP(I)                                                   00731011
        ZTH3=T_IN(J,I)*ZCONV(I)                                         00740001
C----   Ensure ZP1 and ZP4 stay within range                            00750001
        I=MAX(II-1,1)                                                   00760001
        ZP1=F_P(I)                                                      00770001
        ZLP1=ZLOGP(I)                                                   00771011
        ZTH1=T_IN(J,I)*ZCONV(I)                                         00780001
        I=MIN(II+2,NP)                                                  00790011
        ZP4=F_P(I)                                                      00800001
        ZLP4=ZLOGP(I)                                                   00801011
        ZTH4=T_IN(J,I)*ZCONV(I)                                         00810001
                                                                        00820000
C---- 2.2     Calculate gradient                                        00830000
        IF(SF_P(J).EQ.R_MDI) THEN                                       00840001
          TH_GRAD(J)=R_MDI                                              00850001
        ELSE                                                            00860001
*IF TEST                                                                00870004
*END TEST                                                    *****  10  00960004
C----     calculate gradients from (1)&(3), (2)&(4), (2)&(3)            00970001
          IF (IVERT.LT.10) THEN                                         00971011
            ZGL=(ZTH3-ZTH1)/(ZP3-ZP1)                                   00980011
            ZGU=(ZTH4-ZTH2)/(ZP4-ZP2)                                   00990011
            ZGC=(ZTH3-ZTH2)/(ZP3-ZP2)                                   01000011
          ELSE    ! IVERT > 10 - log method                             01001011
            ZGL=(ZTH3-ZTH1)/(ZLP3-ZLP1)                                 01002011
            ZGU=(ZTH4-ZTH2)/(ZLP4-ZLP2)                                 01003011
            ZGC=(ZTH3-ZTH2)/(ZLP3-ZLP2)                                 01004011
          END IF                                                        01005011
C----     Fraction of interval (2)-(3) to required pressure level       01010001
          ZF=(SF_P(J)-ZP2)/(ZP3-ZP2)                                    01020001
C----     Calculate weighted average gradient                           01040001
          IF (IVERTA.EQ.2) THEN                                         01041011
C----       Cubic                                                       01042011
C----       (This is equivalent to evaluating the gradient of a cubic   01050011
C----       though (ZTH2,ZP2) and (ZTH3,ZP3) with gradients ZGL and ZGU 01060011
C----       respectively; ZGC is the straight line gradient between     01070011
C----       these points i.e. we assume that locally Theta varies as a  01080011
C----       cubic function of p (not log(p)) ).                         01090011
            Z1F=1.0-ZF                                                  01091011
            ZWL=Z1F-3.0*ZF*Z1F                                          01100011
            ZWU=ZF-3.0*ZF*Z1F                                           01110011
            ZWC=1.0-(ZWU+ZWL)                                           01120011
C----       (extra factor of 0.01 to convert to SI units)               01130011
            TH_GRAD(J)=0.01*(ZWL*ZGL+ZWU*ZGU+ZWC*ZGC)                   01140011
          ELSE    ! IVERTA.EQ.3                                         01141011
C----       Deep linear                                                 01142011
C----       This is calculated as the weighted average of centred       01143011
C----       differences                                                 01144011
            TH_GRAD(J)=0.01*(ZGL + ZF*(ZGU-ZGL))                        01148011
          END IF                                                        01149411
          IF (IVERT.GE.10) THEN                                         01149511
            TH_GRAD(J)=TH_GRAD(J)/SF_P(J)                               01149611
          END IF                                                        01149711
        END IF                                                          01150001
C                                                                       01160000
290     CONTINUE                                                        01170001
C                                                                       01180001
CL*** 3       Calculate DTh/DP assuming linear variation of Th with P   01190001
C                                                                       01200001
      ELSE IF (IVERT.EQ.1 .OR. IVERT.EQ.11) THEN                        01210011
C                                                                       01220002
        DO 390 J=1,NX*NY                                                01230002
C                                                                       01240001
C---- 3.1     Get Theta and P values for gradient calculation           01250001
C----   Get two Theta and P values (ZTH# and ZP# respectively)          01260001
C----   Required pressure level is between ZPL and ZPU                  01270001
        II=INT(SF_LEVEL(J))                                             01280001
C----   If SF_LEVEL is set to NP, reduce by one                         01290001
        II=MIN(NP-1,II)                                                 01300001
        ZPL=F_P(II)                                                     01310001
        ZLPL=ZLOGP(II)                                                  01311011
        ZTHL=T_IN(J,II)*ZCONV(II)                                       01320001
        I=II+1                                                          01330001
        ZPU=F_P(I)                                                      01340001
        ZLPU=ZLOGP(I)                                                   01341011
        ZTHU=T_IN(J,I)*ZCONV(I)                                         01350001
                                                                        01360001
C---- 2.2     Calculate gradient                                        01370001
        IF(SF_P(J).EQ.R_MDI) THEN                                       01380002
          TH_GRAD(J)=R_MDI                                              01390002
        ELSE                                                            01400002
*IF TEST                                                                01410004
*END TEST                                                    *****   6  01500004
C----   (extra factor of 0.01 to convert to SI units)                   01510001
          IF (IVERT.LT.10) THEN                                         01511011
            TH_GRAD(J)=0.01*(ZTHU-ZTHL)/(ZPU-ZPL)                       01520011
          ELSE                                                          01521011
            TH_GRAD(J)=0.01*(ZTHU-ZTHL)/((ZLPU-ZLPL)*SF_P(J))           01522011
          END IF                                                        01523011
        END IF                                                          01530002
C                                                                       01540001
390     CONTINUE                                                        01550002
C                                                                       01560002
      ELSE                                                              01570002
        IERROR=422                                                      01591007
        WRITE(MESAGE,401) IVERT,IERROR                                  01592009
401     FORMAT(' PV_DTHDP - Invalid mode ',I10,' ;',                    01593008
     +  ' Error code = ',I10)                                           01594008
      END IF                                                            01600002
C                                                                       01610000
      RETURN                                                            01620000
      END                                                               01630000
      SUBROUTINE PV_STAG (AIN, AOUT, WORK, MODE, NX, NY, GRID,          00010000
     + NX_OUT, NY_OUT, GRID_OUT)                                        00020000
CL*** Interpolate values from one grid to another, staggered, grid      00030000
CL                                                                      00040000
CL    For grid points arranged as below:-                               00050000
CL      x -->                                                           00060000
CL    y a a a a a                                                       00070000
CL    |  b b b b b                                                      00080000
CL    v a a a a a                                                       00090000
CL       b b b b b                                                      00100000
CL      a a a a a                                                       00110000
CL                                                                      00120000
CL    (the grid is assumed to wrap around in E-W direction, and         00130000
CL    there is one more row in the 'a' grid than the 'b' grid;          00140000
CL    for Unified Model fields 'a' are mass points, 'b' wind)           00150000
CL                                                                      00160000
CL I    MODE=1 interpolates from 'a' points to 'b' points               00170000
CL           2 interpolates from 'b' points to 'a' points               00180000
CL I    NX, NY       - dimensions of INPUT array (no. columns, rows)    00190000
CL I    AIN(NX,NY)   - input array                                      00200000
CL O    AOUT(NX,*)   - output array (one row less or more then input)   00210000
CL O    WORK(NX,NY)  - work array (same dims as input)                  00220000
CL I    GRID(4)      - Grid info for input array; 1st lat, Dlat,        00230000
CL                     1st lon, Dlon (used only to set GRID_OUT)        00240000
CL O    NX_OUT, NY_OUT, GRID_OUT(4)                                     00250000
CL                   - As NX, NY, GRID for output field                 00260000
C                                                                       00270000
      IMPLICIT NONE                                                     00280000
      INTEGER MODE, NX, NY                                              00290000
      REAL AIN(NX,NY), AOUT(NX,*), WORK (NX,NY)                         00300000
      REAL GRID(4)                                                      00310000
      INTEGER NX_OUT, NY_OUT                                            00320000
      REAL GRID_OUT(4)                                                  00330000
C                                                                       00340000
      LOGICAL WRAP                                                      00341002
      INTEGER JX, JY                                                    00350003
C                                                                       00360000
CL*** 1       Interpolate in x-direction                                00370000
      WRAP=(360.0-ABS(NX*GRID(4))).LT.0.1                               00372002
      IF(MODE.EQ.1) THEN                                                00380000
        DO 118 JY=1,NY                                                  00390001
        DO 112 JX=1,NX-1                                                00400001
        WORK(JX,JY)=AIN(JX+1,JY)+AIN(JX,JY)                             00420001
112     CONTINUE                                                        00440001
        IF(WRAP) THEN                                                   00440102
          WORK(NX,JY)=AIN(1   ,JY)+AIN(NX,JY)                           00441002
        ELSE                                                            00442002
          WORK(NX,JY)=2*AIN(NX,JY)                                      00443002
        END IF                                                          00444002
118     CONTINUE                                                        00450001
        NX_OUT=NX                                                       00460000
        GRID_OUT(3)=GRID(3)+0.5*GRID(4)                                 00470000
        GRID_OUT(4)=GRID(4)                                             00480000
      ELSE IF(MODE.EQ.2) THEN                                           00490000
        DO 128 JY=1,NY                                                  00500001
        DO 122 JX=2,NX                                                  00520001
        WORK(JX,JY)=AIN(JX-1,JY)+AIN(JX,JY)                             00530001
122     CONTINUE                                                        00550001
        IF(WRAP) THEN                                                   00550102
          WORK( 1,JY)=AIN(NX  ,JY)+AIN( 1,JY)                           00551002
        ELSE                                                            00554002
          WORK( 1,JY)=2*AIN( 1,JY)                                      00555002
        END IF                                                          00556002
128     CONTINUE                                                        00560001
        NX_OUT=NX                                                       00570000
        GRID_OUT(3)=GRID(3)-0.5*GRID(4)                                 00580000
        GRID_OUT(4)=GRID(4)                                             00590000
      END IF                                                            00600000
C                                                                       00610000
CL*** 2       Interpolate in y-direction                                00620000
      IF(MODE.EQ.1) THEN                                                00630000
        DO 212 JY=1,NY-1                                                00640000
        DO 211 JX=1,NX                                                  00650000
        AOUT(JX,JY)=0.25*(WORK(JX,JY)+WORK(JX,JY+1))                    00660000
211     CONTINUE                                                        00670000
212     CONTINUE                                                        00680000
        NY_OUT=NY-1                                                     00690000
        GRID_OUT(1)=GRID(1)+0.5*GRID(2)                                 00700000
        GRID_OUT(2)=GRID(2)                                             00710000
      ELSE IF(MODE.EQ.2) THEN                                           00720000
        DO 221 JX=1,NX                                                  00730000
        AOUT(JX,1)=0.5*WORK(JX,1)                                       00740000
221     CONTINUE                                                        00750000
        DO 224 JY=2,NY                                                  00760000
        DO 223 JX=1,NX                                                  00770000
        AOUT(JX,JY)=0.25*(WORK(JX,JY)+WORK(JX,JY-1))                    00780000
223     CONTINUE                                                        00790000
224     CONTINUE                                                        00800000
        DO 226 JX=1,NX                                                  00810000
        AOUT(JX,NY+1)=0.5*WORK(JX,NY)                                   00820000
226     CONTINUE                                                        00830000
        NY_OUT=NY+1                                                     00840000
        GRID_OUT(1)=GRID(1)-0.5*GRID(2)                                 00850000
        GRID_OUT(2)=GRID(2)                                             00860000
      END IF                                                            00870000
C                                                                       00880000
      RETURN                                                            00890000
      END                                                               00900000
      SUBROUTINE PV_FVINT(AIN, TH_LEVEL, AOUT, NX, NY, NL)              00010002
CL*** Subroutine to vertically interpolate fields to a surface which    00020000
CL*** is defined in terms of input level number                         00030000
CL I    AIN        3-D input field                                      00040000
CL I    TH_LEVEL   Output surface in terms of field levels in range 1   00042000
CL                 to NL, inclusive.  (A value of n.fff indicates it is 00043000
CL                 between level n and n+1; 0.fff is the vertical       00044000
CL                 interpolation weight).                               00045000
CL O    AOUT       Field interpolated to required surface               00046000
CL I    NX, NY     Horizontal dimensions of field (columns, rows)       00047000
CL I    NL         Number of field levels                               00048000
CL    This routine takes fields from the 3-D array AIN and derives an   00049000
CL    output field AOUT by interpolating the fields using the           00050000
CL    information in TH_LEVEL.                                          00060000
      IMPLICIT NONE                                                     00070000
      INTEGER NX, NY, NL                                                00080000
      REAL AIN(NX*NY,NL), TH_LEVEL(NX*NY),  AOUT(NX*NY)                 00090000
C                                                                       00110000
      INTEGER J, JL                                                     00120000
      REAL Z, ZJL, ZJM                                                  00130000
C                                                                       00140000
C----         Do vertical interpolation                                 00150000
      DO 120 JL=2,NL                                                    00160000
      ZJL=REAL(JL)                                                      00170000
      ZJM=REAL(JL-1)                                                    00180000
      DO 110 J=1,NX*NY                                                  00190000
      IF(TH_LEVEL(J).GE.ZJM.AND.TH_LEVEL(J).LE.ZJL) THEN                00200000
        Z=ZJL-TH_LEVEL(J)                                               00210000
        AOUT(J)=AIN(J,JL)+Z*(AIN(J,JL-1)-AIN(J,JL))                     00220000
      END IF                                                            00230000
110   CONTINUE                                                          00240000
120   CONTINUE                                                          00250000
C                                                                       00260000
      RETURN                                                            00270000
      END                                                               00280000
      SUBROUTINE PV_DVB (U, V, AOUT, WORK, MODE, NX, NY, GRID,                  
     + RADIUS, DEG_TO_R, R_MDI, IERROR, MESAGE)                                 
CL    Calculate divergence or vorticity (AOUT) of a vector field (U,V)          
CL    on a staggered Arakawa B grid                                             
CL    Vorticity/divergence is calculated by dividing circulation around         
CL    (or outflow from) each grid box by the box area.                          
CL    The vector field is assumed to be on the wind grid, and the               
CL    output field on the mass grid points, as shown below                      
CL                                                                              
CL    o o o o o          o = output (mass) grid points                          
CL     i i i i i         i = input (wind) grid points                           
CL    o o o o o                                                                 
CL     i i i i i                                                                
CL    o o o o o                                                                 
CL                                                                              
CL I    U(NX,NY), V(NX,NY)    Input vector field                                
CL O    AOUT (NX,NY+1)        Output field                                      
CL O    WORK (NX,NY)          Work space                                        
CL I    MODE                  1 for divergence, 2 for vorticity                 
CL I    NX, NY                No of rows, points per row of input               
CL I    GRID(4)               1st lat, Dlat, 1st lon, Dlon input grid           
CL I    RADIUS                Radius of earth (m)                               
CL I    DEG_TO_R              Degrees to radians conversion factor              
CL I    R_MDI                 Missing data indicator                            
CL O    IERROR                Error indicated by non-zero value                 
CL O    MESAGE                Error message                                     
C                                                                               
C     For fields with E-W wrap-around, the first point of each output           
C     row is calculated from the end points of the input fields, other-         
C     wise it is set to R_MDI.                                                  
C     An extra row is produced for the output field; when the input             
C     fields start or end a half grid length from the pole, the extreme         
C     rows are set to polar values, otherwise they are set to R_MDI.            
C     (For non-wrap-around, the polar caps are incomplete, so are not           
C     set).                                                                     
C                                                                               
      IMPLICIT NONE                                                             
      INTEGER MODE, NX, NY                                                      
      REAL U(NX,NY), V(NX,NY), AOUT(NX, NY+1), WORK(NX, NY)                     
      REAL GRID(4)                                                              
      REAL RADIUS, DEG_TO_R, R_MDI                                              
      INTEGER IERROR                                                            
      CHARACTER*256 MESAGE                                                      
C                                                                               
      LOGICAL WRAP, N_POLE, S_POLE                                              
      REAL Z, ZSS, ZSN, ZRA                                                     
      REAL SLAT, DLAT, SLON, DLON                                               
      INTEGER JX, JY                                                            
C                                                                               
      EXTERNAL PV_DX, PV_DY                                                     
C                                                                               
CL    1    Preliminaries                                                        
      IERROR=0                                                                  
      SLAT=GRID(1)                                                              
      DLAT=GRID(2)                                                              
      SLON=GRID(3)                                                              
      DLON=GRID(4)                                                              
C     Test for wrap-around                                                      
      Z=NX*DLON                                                                 
      WRAP=ABS(360.-ABS(Z)).LT.0.1                                              
C     Test for Pole at start of grid                                            
      Z=SLAT-0.5*DLAT                                                           
      N_POLE=(ABS(90.-ABS(Z)).LT.0.1).AND.WRAP                                  
C     Test for Pole at end of grid                                              
      Z=SLAT+(NY-0.5)*DLAT                                                      
      S_POLE=(ABS(90.-ABS(Z)).LT.0.1).AND.WRAP                                  
C                                                                               
CL    2    Do differentation for divergence                                     
      IF(MODE.EQ.1) THEN                                                        
        CALL PV_DX(U, WORK, NX, NY, GRID, DEG_TO_R, R_MDI)                      
        CALL PV_DY(V, AOUT, NX, NY, GRID, DEG_TO_R, R_MDI)                      
        DO JY=2,NY                                                              
          DO JX=1,NX                                                            
            IF(AOUT(JX,JY).EQ.R_MDI.OR.WORK(JX,JY-1).EQ.R_MDI) THEN             
              AOUT(JX,JY)=R_MDI                                                 
            ELSE                                                                
              AOUT(JX,JY)=AOUT(JX,JY)+WORK(JX,JY-1)                             
            END IF                                                              
          END DO                                                                
        END DO                                                                  
C                                                                               
CL    3    Do differentation for vorticity  dv/dx - du/dy                       
      ELSE IF(MODE.EQ.2) THEN                                                   
        CALL PV_DX(V, WORK, NX, NY, GRID, DEG_TO_R, R_MDI)                      
        CALL PV_DY(U, AOUT, NX, NY, GRID, DEG_TO_R, R_MDI)                      
*IF TEST                                                                        
*END TEST                                                    *****   6          
        DO JY=1,NY+1                                                            
          IF(JY.GT.1.AND.JY.LT.NY+1) THEN                                       
            DO JX=1,NX                                                          
              IF(AOUT(JX,JY).EQ.R_MDI.OR.WORK(JX,JY-1).EQ.R_MDI) THEN           
                AOUT(JX,JY)=R_MDI                                               
              ELSE                                                              
                AOUT(JX,JY)=WORK(JX,JY-1)-AOUT(JX,JY)                           
              END IF                                                            
            END DO                                                              
          ELSE                                                                  
            DO JX=1,NX                                                          
              IF(AOUT(JX,JY).NE.R_MDI) THEN                                     
                AOUT(JX,JY)=-AOUT(JX,JY)                                        
              END IF                                                            
            END DO                                                              
          END IF                                                                
        END DO                                                                  
C                                                                               
      ELSE                                                                      
        IERROR=430                                                              
        WRITE(MESAGE,391) MODE, IERROR                                          
391     FORMAT(' PV_DVB - Invalid MODE ',I10,' ;',                              
     +  ' Error code = ',I10)                                                   
        RETURN                                                                  
      END IF                                                                    
C                                                                               
CL    4     Divide by area of grid boxes                                        
C (only 1 factor of RADIUS, because PV_DX, PV_DY assume a unit sphere)          
      Z=SLAT-DLAT                                                               
      IF(ABS(Z).GT.90.0) Z=SIGN(90.0,Z)                                         
      ZSN=SIN(DEG_TO_R*Z)                                                       
      DO JY=1,NY+1                                                              
        Z=SLAT+(JY-1)*DLAT                                                      
        IF(ABS(Z).GT.90.0) Z=SIGN(90.0,Z)                                       
        ZSS=SIN(DEG_TO_R*Z)                                                     
        ZRA=1.0/ABS(RADIUS*DLON*DEG_TO_R*(ZSS-ZSN))                             
        DO JX=1,NX                                                              
          IF(AOUT(JX,JY).NE.R_MDI) THEN                                         
            AOUT(JX,JY)=ZRA*AOUT(JX,JY)                                         
          END IF                                                                
        END DO                                                                  
        ZSN=ZSS                                                                 
      END DO                                                                    
C                                                                               
CL    5     Reset to missing data if non-wrap-around / non-polar                
      IF(.NOT.WRAP) THEN                                                        
        DO JY=1,NY+1                                                            
          AOUT(1,JY)=R_MDI                                                      
        END DO                                                                  
      END IF                                                                    
      IF(.NOT.N_POLE) THEN                                                      
        DO JX=1,NX                                                              
          AOUT(JX,1)=R_MDI                                                      
        END DO                                                                  
      END IF                                                                    
      IF(.NOT.S_POLE) THEN                                                      
        DO JX=1,NX                                                              
          AOUT(JX,NY+1)=R_MDI                                                   
        END DO                                                                  
      END IF                                                                    
*IF TEST                                                                        
*END TEST                                                    *****   4          
C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PV_DX(AIN, AOUT, NX, NY, GRID, DEG_TO_R, R_MDI)                
CL    Routine to do x-differentiation for vorticity/divergence.                 
C     It calculates the difference between the line integral of AIN             
C     along the western edge of each grid box, and the line integral            
C     along the eastern edge (on a unit sphere).                                
C     The input field is assumed to be on the wind points of an                 
C     Arakawa B grid, and the output field on the mass grid points.             
CL I    AIN                   Input array                                       
CL O    AOUT                  Output array (last row only used for work)        
CL I    NX, NY                No of rows, points per row of input               
CL I    GRID(4)               1st lat, Dlat, 1st lon, Dlon input grid           
CL I    DEG_TO_R              Degrees to radians conversion factor              
CL I    R_MDI                 Missing data indicator                            
C NB. - The n'th row of AOUT is derived from the n'th and n+1'th rows           
C     of AIN, so only NY-1 rows of AOUT are set                                 
C     - 1st point in each row is set assuming wrap-around; ignore if            
C     non-wrap-around grid!                                                     
C                                                                               
      IMPLICIT NONE                                                             
      INTEGER NX, NY                                                            
      REAL AIN(NX,NY), AOUT(NX, NY)                                             
      REAL GRID(4)                                                              
      REAL DEG_TO_R, R_MDI                                                      
      INTEGER JX, JY                                                            
      REAL ZG                                                                   
C                                                                               
CL    Do differentiation                                                        
      ZG=SIGN(1.0,GRID(4))*ABS(0.5*DEG_TO_R*GRID(2))                            
      DO 190 JY=1,NY-1                                                          
C                                                                               
C     Average input in N-S; multiply by grid length                             
C     (use last row of AOUT as workspace)                                       
      DO 110 JX=1,NX                                                            
      IF(AIN(JX,JY).EQ.R_MDI.OR.AIN(JX,JY+1).EQ.R_MDI) THEN                     
        AOUT(JX,NY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(JX,NY)=ZG*(AIN(JX,JY)+AIN(JX,JY+1))                                
      END IF                                                                    
110   CONTINUE                                                                  
C                                                                               
C     Difference in E-W                                                         
      IF(AOUT(1 ,NY).EQ.R_MDI.OR.AOUT(NX  ,NY).EQ.R_MDI) THEN                   
        AOUT(1 ,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(1 ,JY)=AOUT(1 ,NY)-AOUT(NX,NY)                                     
      END IF                                                                    
      DO 120 JX=2,NX                                                            
      IF(AOUT(JX,NY).EQ.R_MDI.OR.AOUT(JX-1,NY).EQ.R_MDI) THEN                   
        AOUT(JX,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(JX,JY)=AOUT(JX,NY)-AOUT(JX-1,NY)                                   
      END IF                                                                    
120   CONTINUE                                                                  
C                                                                               
190   CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
C                                                                               
      SUBROUTINE PV_DY(AIN, AOUT, NX, NY, GRID, DEG_TO_R, R_MDI)                
CL    Routine to do y-differentiation for vorticity/divergence.                 
C     It calculates the difference between the line integral of AIN             
C     along the northern edge of each grid box, and the line integral           
C     along the southern edge (on a unit sphere).                               
C     The input field is assumed to be on the wind points of an                 
C     Arakawa B grid, and the output field on the mass grid points.             
CL I    AIN                   Input array                                       
CL O    AOUT                  Output array                                      
CL I    NX, NY                No of rows, points per row of input               
CL I    GRID(4)               1st lat, Dlat, 1st lon, Dlon input grid           
CL I    DEG_TO_R              Degrees to radians conversion factor              
CL I    R_MDI                 Missing data indicator                            
C NB. - The n'th row of AOUT is derived from the n'th and n-1'th rows           
C     of AIN; the extreme rows 1 and NY+1 are set to polar values if            
C     output grid latitudes are at pole, but otherwise extreme rows are         
C     set to R_MDI                                                              
C     - 1st point in each row is set assuming wrap-around; ignore if            
C     non-wrap-around grid!                                                     
C                                                                               
      IMPLICIT NONE                                                             
      INTEGER NX, NY                                                            
      REAL AIN(NX, NY), AOUT(NX, NY+1)                                          
      REAL GRID(4)                                                              
      REAL R_MDI, DEG_TO_R                                                      
C                                                                               
      INTEGER JX, JY                                                            
      REAL ZG,Z,ZI                                                              
      LOGICAL ILMISS                                                            
C                                                                               
CL    1      Do differentiation for body of grid                                
      DO 120 JY=1,NY                                                            
      Z=DEG_TO_R*(GRID(1)+(JY-1)*GRID(2))                                       
      ZG=SIGN(1.0,GRID(2))*ABS(0.5*DEG_TO_R*GRID(4)*COS(Z))                     
C                                                                               
C     Average input in E-W, multiply by grid-length                             
C     Store in AOUT                                                             
      IF(AIN( 1,JY).EQ.R_MDI.OR.AIN(NX,JY).EQ.R_MDI) THEN                       
        AOUT( 1,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT( 1,JY)=ZG*(AIN( 1,JY)+AIN(NX,JY))                                  
      END IF                                                                    
      DO 110 JX=2,NX                                                            
      IF(AIN(JX,JY).EQ.R_MDI.OR.AIN(JX-1,JY).EQ.R_MDI) THEN                     
        AOUT(JX,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(JX,JY)=ZG*(AIN(JX,JY)+AIN(JX-1,JY))                                
      END IF                                                                    
110   CONTINUE                                                                  
120   CONTINUE                                                                  
C                                                                               
C     Difference in N-S                                                         
C     (note reverse order in JY, to avoid over-writing problem)                 
      DO 140 JY=NY,2,-1                                                         
      DO 130 JX=1,NX                                                            
      IF(AOUT(JX,JY).EQ.R_MDI.OR.AOUT(JX,JY-1).EQ.R_MDI) THEN                   
        AOUT(JX,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(JX,JY)=(AOUT(JX,JY)-AOUT(JX,JY-1))                                 
      END IF                                                                    
130   CONTINUE                                                                  
140   CONTINUE                                                                  
C                                                                               
CL    2      Deal with polar caps                                               
C     Integrate around polar cap, but divide by NX to allow for grid            
C     boxes (wedges) 1 grid length wide (rather than complete cap)              
      ZG=ABS(DEG_TO_R*GRID(4)*SIN(0.5*GRID(2)*DEG_TO_R))                        
C                                                                               
C     Pole at start of grid                                                     
      Z=GRID(1)-0.5*GRID(2)                                                     
      IF(ABS(90.0-ABS(Z)).LT.0.1) THEN                                          
        ZI=0.0                                                                  
        ILMISS=.FALSE.                                                          
        DO 212 JX=1,NX                                                          
        ZI=ZI-AIN(JX,1)                                                         
        IF(AIN(JX,1).EQ.R_MDI) ILMISS=.TRUE.                                    
212     CONTINUE                                                                
        ZI=ZI*ZG/REAL(NX)*SIGN(1.0,GRID(2))                                     
        IF(ILMISS) ZI=R_MDI                                                     
        DO 214 JX=1,NX                                                          
        AOUT(JX,1)=ZI                                                           
214     CONTINUE                                                                
      ELSE                                                                      
        DO 216 JX=1,NX                                                          
        AOUT(JX,1)=R_MDI                                                        
216     CONTINUE                                                                
      END IF                                                                    
C                                                                               
C     Pole at end of grid                                                       
      Z=GRID(1)+(NY-0.5)*GRID(2)                                                
      IF(ABS(90.0-ABS(Z)).LT.0.1) THEN                                          
        ZI=0.0                                                                  
        ILMISS=.FALSE.                                                          
        DO 222 JX=1,NX                                                          
        ZI=ZI+AIN(JX,NY)                                                        
        IF(AIN(JX,NY).EQ.R_MDI) ILMISS=.TRUE.                                   
222     CONTINUE                                                                
        ZI=ZI*ZG/REAL(NX)*SIGN(1.0,GRID(2))                                     
        IF(ILMISS) ZI=R_MDI                                                     
        DO 224 JX=1,NX                                                          
        AOUT(JX,NY+1)=ZI                                                        
224     CONTINUE                                                                
      ELSE                                                                      
        DO 226 JX=1,NX                                                          
        AOUT(JX,NY+1)=R_MDI                                                     
226     CONTINUE                                                                
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PV_VG2PV (VORT, TH_GRAD, PV, NX, NY, GRID,             00010000
     + OMEGA, GRAVITY, DEG_TO_R, R_MDI)                                 00020000
CL    Calculate Ertel potential vorticity from vorticity and dTheta/dp  00030000
CL    fields.                                                           00040000
CL I    VORT(NX,NY)           Relative vorticity                        00050000
CL I    TH_GRAD (NX,NY)       Dtheta/dp                                 00060000
CL O    PV(NX,NY)             Output potential vorticity                00070000
CL I    NX, NY                Dimensions of fields (columns, rows)      00080000
CL I    GRID(4)               1st lat, Dlat, 1st lon, Dlon grid         00090000
CL I    OMEGA                 Earth's rotation rate                     00100000
CL I    GRAVITY               Acceleration due to gravity (g)           00110000
CL I    DEG_TO_R              Degrees to radians conversion factor      00120000
CL I    R_MDI                 Missing data indicator                    00130000
C                                                                       00140000
C     The Coriolis parameter f is added to the relative vorticity,      00150000
C     to give absolute vorticity; the result is multiplied by dTheta/dp 00160000
C     to produce potential vorticity.                                   00170000
C                                                                       00180000
      IMPLICIT NONE                                                     00190000
      INTEGER NX, NY                                                    00200000
      REAL VORT(NX,NY), TH_GRAD(NX,NY), PV(NX,NY)                       00210000
      REAL GRID(4)                                                      00220000
      REAL R_MDI, OMEGA, DEG_TO_R, GRAVITY                              00230000
C                                                                       00240000
      REAL Z, F                                                         00250000
      INTEGER JX, JY                                                    00260000
C                                                                       00270000
CL*** 1       Calculation                                               00280000
      DO 190 JY=1,NY                                                    00290000
      Z=DEG_TO_R*(GRID(1)+(JY-1)*GRID(2))                               00300000
      F=2.0*OMEGA*SIN(Z)                                                00310000
      DO 180 JX=1,NX                                                    00320000
      IF(TH_GRAD(JX,JY).EQ.R_MDI.OR.VORT(JX,JY).EQ.R_MDI) THEN          00330000
        PV(JX,JY)=R_MDI                                                 00340000
      ELSE                                                              00350000
        PV(JX,JY)=-GRAVITY*(VORT(JX,JY)+F)*TH_GRAD(JX,JY)               00360000
      END IF                                                            00370000
180   CONTINUE                                                          00380000
190   CONTINUE                                                          00390000
C                                                                       00400000
      RETURN                                                            00410000
      END                                                               00420000
