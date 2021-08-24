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
