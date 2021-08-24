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
