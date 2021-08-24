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
