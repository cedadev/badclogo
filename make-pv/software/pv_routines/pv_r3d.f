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
