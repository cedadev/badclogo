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
