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
