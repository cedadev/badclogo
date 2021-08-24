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
