C            ACCESSING UKMO ASSIMILATION CORRELATIVE FILES
C            =============================================
C
C
C  This is an example programme which demonstrates the use of the 
C  routines UKAOPN, UKAFLD, UKAPRF, PV_UKA and UKACLS to access UKMO 
C  assimilation correlative files.
C
C  Some comments (starting CUCSS) in the program indicate how the access 
C  routines can by used in conjunction with UCSS routines (UARS RAC 
C  simulated services), which allow the user access to the catalogued 
C  UKMO assimilation correlative files.
C
C  The program obtains wind components, temperatures, heights and 
C  vertical velocities at various pressure levels. It obtains profiles 
C  these fields at a number of points across the globe. It determines 
C  Ertel potential vorticity and wind and pressure fields on isentropic 
C  (and also pressure) surfaces, using both linear and cubic 
C  interpolation in the vertical.
C
C  N.B. Vertical velocity (the fifth correlative data parameter) is
C  available in UKMO correlative files for 26th August 1992 onwards 
C  (and in files for analyses that have been re-run since then).
C
C  Correct dimensions for PV_UKA output  RS 11/06/04
C
      IMPLICIT NONE
C                
      INTEGER NP,NXNY
      INTEGER KNTYPE,KNLD,KNL,KNR,NSPR,NSPV,NOPV,NWORK,NTTYPE
C
      PARAMETER (NP = 22, NXNY = 7008)
      PARAMETER (KNTYPE = 12, KNLD = 11)
      PARAMETER (KNL = 11, KNR = 10)
      PARAMETER (NSPR=3, NSPV = 2, NOPV =4)
      PARAMETER (NWORK = (NP+3*NSPV)*NXNY)
      PARAMETER (NTTYPE = 5)
C         
C      NP = No of pressure levels in the UKMO correlative data
C    NXNY = Maximum size (in 4-byte words) of a 2-dimensional assimilation
C           field  
C  KNTYPE = Maximum no of correlative data parameters present in the 
C           UKMO correlative data
C    KNLD = Maximum no of pressure levels at which a profile can be produced
C     KNL = No of pressure levels at which a profile is to be produced
C     KNR = No of (lat,long) points at which profiles are to be produced
C     NPR = No of pressure surfaces for test prints
C    NSPV = No of isentropic/pressure surfaces at which Ertel potential 
C           vorticity is required
C    NOPV = No of options for PV (combinations of IVERT & IOUTL)
C   NWORK = Size (in 4-byte words) of work area required when calculating
C           fields on isentropic surfaces  
C
      INTEGER LUN,IERROR,KTIME,I,K,L,IQUAL
      INTEGER LUN_PRT,IWIDTH,NX,NY,NXW,NYW
      INTEGER NROW, J, J1, J2
      INTEGER IVERT,IOUTL, IOPVV,IOPVL
      INTEGER START_TIME(2),STOP_TIME(2),COND_CODE
      INTEGER UARS_DAY,STATUS
      INTEGER NFORM, LUN_NL
      INTEGER IX1, IX2, IY1, IY2
      INTEGER ITTYPE 
      INTEGER INR, IJ
C
      LOGICAL NLFAST
C
      REAL PLEVEL,PFIELD
      REAL PLEVLS,PLATS,PLONS,PROFS
      REAL THETA,PV,U,V,GRID,GRIDW,WORK
      REAL ALAT
      REAL PMAX,PMIN,PMDI
      REAL PRESS,THETAP
C   
      CHARACTER*256 MESAGE
      CHARACTER*80 DESCRP
      CHARACTER*80 DUMMY_ATTR,COMMENTS
      CHARACTER*72 LABEL
      CHARACTER*23 DATE
      CHARACTER*20 PARAMS_TABLE(2,20)
      CHARACTER*80 LID
      CHARACTER*12 GTYPE,KTYPE(KNTYPE)
      CHARACTER*12 SOURCE,SUBTYPE
      CHARACTER*12 TTYPE(NTTYPE)
      CHARACTER*8  PVLEVT(2),PVLEVV
      CHARACTER*4  PASS_FAIL_FLAG,DISP
C
      DIMENSION KTIME(4)
      DIMENSION PFIELD(NXNY)
      DIMENSION PLEVLS(KNLD),PLATS(KNR),PLONS(KNR),PROFS(KNLD,KNR)
      DIMENSION PV(NXNY*NSPV),U(NXNY*NSPV),V(NXNY*NSPV)
      DIMENSION GRID(4),GRIDW(4),WORK(NWORK)
      DIMENSION THETA(NSPV),PRESS(NSPR),THETAP(NXNY*NSPV)
      DIMENSION ITTYPE(NTTYPE)
      DIMENSION IOPVV(NOPV), IOPVL(NOPV)
C
      DATA PLEVLS/1000.0,500.0,250.0,100.0,50.0,20.0,10.0,5.0,
     *    2.0,1.0,0.5/
      DATA PLATS/75.0,65.0,55.7,40.0,25.0,-5.1,-15.0,-30.3,-55.0,-80.0/
      DATA PLONS/0.0,10.0,35.0,50.0,75.0,90.0,150.0,230.5,348.6,359.9/
      DATA THETA/420.0,850.0/
      DATA LUN_PRT/6/,IWIDTH/80/
      DATA KTYPE/KNTYPE*' '/
      DATA PRESS/100.0,10.0,1.0/
      DATA DUMMY_ATTR/' '/,COMMENTS/' '/,DISP/'FREE'/
      DATA TTYPE/'ZONWIN_P    ','MERWIN_P    ','TEMP        ',
     +           'HEIGHT      ','OMEGA       '/
      DATA NLFAST/.TRUE./   ! Fast open?
      DATA NFORM /1/        ! File format; 1 CDHF, 2 PP, 3 FP, 4 DAAC
      DATA LUN_NL /5/       ! Unit no. for namelist /INPUT/
      DATA IX1/1/, IX2/5/, IY1/10/, IY2/15/  ! standard sub-grid for printing
      DATA IOPVV /2,1,3,2/, IOPVL/1,1,1,2/
      DATA PVLEVT/'Theta   ','Pressure'/
C
CUCSS - the following statements are for use when running outside the UCSS 
C     environment
C
C     LUN & LID are normally given by UCSS routines
      DATA LUN/10/  
C
      NAMELIST /INPUT/ NFORM, NLFAST, LID
C
C     Set LID to VMS default for unit 10
      LID='FOR010'
      IF (LUN_NL.GT.0) THEN
        READ (LUN_NL,INPUT)
        WRITE (LUN_PRT,INPUT)
      END IF
C
CUCSS - The following section is for use in conjunction with UCSS
C 
C     EXTERNAL SS$_NORMAL
C
C
C  Set up PARAMS_TABLE and also return other parameters from 
C  the PROGRAM_PARAMS namelist in the runstream
C         
C     CALL PGINIT(PARAMS_TABLE,START_TIME,STOP_TIME,UARS_DAY)
C
C
C  Extract the required information from PARAMS_TABLE
C
C     SOURCE = PARAMS_TABLE(2,1)
C     SUBTYPE = PARAMS_TABLE(2,2)
C     LID = PARAMS_TABLE(2,3)
C
C        
C  Assign the required UKMO assimilation correlative file
C
C     CALL ASGCOR(SOURCE,SUBTYPE,UARS_DAY,LID,LUN,STATUS)
C
C  Check STATUS returned by ASGCOR
C
CUCSS - end of UCSS code
C       
C  Open the UKMO assimilation correlative file
C
      CALL UKAOPN(LUN,LID,GTYPE,DATE,PMAX,PMIN,DESCRP,KTYPE,KNTYPE,
     *  IQUAL,NFORM,NLFAST,IERROR,MESAGE)
C
C  Print message returned by UKAOPN
C
      CALL UKAPRT(MESAGE,LUN_PRT,IWIDTH)
C
      WRITE(LUN_PRT,*) ' IERROR AFTER UKAOPN ',IERROR
      WRITE(LUN_PRT,*) ' '
C
      WRITE(LUN_PRT,*) ' GENERAL TYPE OF DATA ', GTYPE
      WRITE(LUN_PRT,*) ' '
C
      WRITE(LUN_PRT,*) ' DATE OF DATA ', DATE
      WRITE(LUN_PRT,*) ' ' 
C
      WRITE(LUN_PRT,*) ' MAXIMUM PRESSURE LEVEL ', PMAX
      WRITE(LUN_PRT,*) ' '
C
      WRITE(LUN_PRT,*) ' MINIMUM PRESSURE LEVEL ', PMIN
      WRITE(LUN_PRT,*) ' '
C
      WRITE(LUN_PRT,*) ' GENERAL DESCRIPTION OF DATA '
      WRITE(LUN_PRT,*) DESCRP
      WRITE(LUN_PRT,*) ' '
C
      WRITE(LUN_PRT,*) ' CORRELATIVE DATA PARAMETERS'
      WRITE(LUN_PRT,*) KTYPE
      WRITE(LUN_PRT,*) ' '
C
      WRITE(LUN_PRT,*) ' QUALITY OF DATA ', IQUAL
      WRITE(LUN_PRT,*) ' '
C
C---- Check list of types found (KTYPE) against standard list (TTYPE)
C
      DO J=1,NTTYPE
        I=0
        DO J1=1,KNTYPE
          IF (TTYPE(J).EQ.KTYPE(J1)) I=J1
        END DO
        WRITE(LUN_PRT,*) 'Type ',TTYPE(J),' matches file type no. ',I  
        ITTYPE(J)=I
      END DO
C
C  Obtain each test correlative data parameter in turn, on test
C  pressure surfaces
      IJ=0
      DO J=1,NTTYPE
        IF(ITTYPE(J).GT.0) THEN
          IJ=IJ+1
          I=1+MOD(IJ-1,NSPR)
          PLEVEL=PRESS(I)
C  Get pressure level field
          CALL UKAFLD(LUN,TTYPE(J),PLEVEL,PFIELD,NY,NX,GRID,
     *    KTIME,PMDI,IERROR,MESAGE)
C  Print message & status code returned by UKAFLD
          CALL UKAPRT(MESAGE,LUN_PRT,IWIDTH)
          WRITE(LUN_PRT,*) ' IERROR AFTER UKAFLD ',IERROR
          WRITE(LUN_PRT,*) ' '
C  Construct label for printing
          WRITE (LABEL,201) TTYPE(J), PLEVEL
201       FORMAT(A,' at pressure (mb):',F10.3)
C  Print standard area   
          CALL PRINTA2 (PFIELD, NX, IX1, IX2, IY1, IY2,
     *    LABEL, GRID, LUN_PRT)   
C  First time, print out all rows, col IX1 & all points in row IY1
          IF (IJ.EQ.1) THEN
            CALL PRINTA2 (PFIELD, NX, IX1, IX1, 1, NY,
     *      LABEL, GRID, LUN_PRT)   
            CALL PRINTA2 (PFIELD, NX, 1, NX, IY1, IY1,
     *      LABEL, GRID, LUN_PRT)
          END IF
        END IF
      END DO
C
C
C  Obtain vertical profiles of each test correlative data parameter 
C  in turn at the specified points
      IJ=0
      DO J=1,NTTYPE
        IF(ITTYPE(J).GT.0) THEN
          IJ=IJ+1
C First time, get all profiles; subsequent time just get two
          IF(IJ.EQ.1) THEN
            INR=KNR
          ELSE
            INR=2
          END IF
C  Get profiles
          CALL UKAPRF(LUN,TTYPE(J),PLEVLS,KNL,KNLD,PLATS,PLONS,INR,
     *    PROFS,KTIME,PMDI,IERROR,MESAGE)
C  Print message & status code returned by UKAPRF
          CALL UKAPRT(MESAGE,LUN_PRT,IWIDTH)
          WRITE(LUN_PRT,*) ' IERROR AFTER UKAPRF ',IERROR
          WRITE(LUN_PRT,*) ' '
C  Print out the profiles 
          DO J1 = 1,INR
            WRITE (LUN_PRT,301) TTYPE(J), PLATS(J1), PLONS(J1)
301         FORMAT ('0Profile of ',A,' at (lat,long)',2F8.2)
            WRITE (LUN_PRT,302) TTYPE(J)
302         FORMAT (3X,A,T21,'Level (mb)')
            DO J2 = 1, KNL
              WRITE (LUN_PRT,303) PROFS(J2, J1), PLEVLS(J2)
303           FORMAT(1X,G14.3,T21,F10.3)
            END DO
          END DO
        END IF
      END DO
C
C  Get Ertel potential vorticity, wind and pressure fields on 
C  required isentropic surfaces
      IJ=0
      DO J=1,NOPV
        IOUTL=IOPVL(J)
        IVERT=IOPVV(J)
        WRITE (LUN_PRT,*) ' '
        WRITE (LUN_PRT,*) 'IVERT (1=linear, 2=cubic, 3=deep) ',IVERT
        WRITE (LUN_PRT,*) 'IOUTL (1=theta, 2=pressure) ',IOUTL
        IF (IOUTL.EQ.1) THEN
          CALL PV_UKA(THETA,NSPV,PV,U,V,THETAP,NX,NY,GRID,NXW,NYW,
     *    GRIDW, KTIME, LUN,LUN_PRT,PMDI,WORK,NWORK,
     *    IOUTL,IVERT,IERROR,MESAGE)
        ELSE 
          CALL PV_UKA(PRESS,NSPV,PV,U,V,THETAP,NX,NY,GRID,NXW,NYW,
     *    GRIDW, KTIME, LUN,LUN_PRT,PMDI,WORK,NWORK,
     *    IOUTL,IVERT,IERROR,MESAGE)
        END IF
C
C  Print message/error code returned by PV_UKA
        CALL UKAPRT(MESAGE,LUN_PRT,IWIDTH)
        WRITE(LUN_PRT,*) ' IERROR AFTER PV_UKA ',IERROR
        WRITE(LUN_PRT,*) ' '
C
C  Print out values for each level
        DO J1=1,NSPV
          IF (IOUTL.EQ.1) THEN
            WRITE(PVLEVV,'(F8.2)') THETA(J1)
          ELSE
            WRITE(PVLEVV,'(F8.2)') PRESS(J1)
          END IF
C  Print PV for standard area 
          WRITE(LABEL,411) PVLEVT(IOUTL), PVLEVV 
411       FORMAT('PV at ',A,' = ',A) 
          CALL PRINTA2 (PV(1+(J1-1)*NX*NY), NX, IX1, IX2, IY1, IY2,
     *    LABEL, GRID, LUN_PRT)   
C  First option, and 1st level also print u,v, P/theta 
          IF (J.EQ.1 .AND. J1.EQ.1) THEN
            WRITE(LABEL,412) PVLEVT(IOUTL), PVLEVV 
412         FORMAT('U wind at ',A,' = ',A) 
            CALL PRINTA2 (U(1+(J1-1)*NXW*NYW), NX, IX1, IX2, IY1, IY2,
     *      LABEL, GRIDW, LUN_PRT)   
            WRITE(LABEL,413) PVLEVT(IOUTL), PVLEVV 
413         FORMAT('V wind at ',A,' = ',A) 
            CALL PRINTA2 (V(1+(J1-1)*NXW*NYW), NX, IX1, IX2, IY1, IY2,
     *      LABEL, GRIDW, LUN_PRT)   
            WRITE(LABEL,414) PVLEVT(3-IOUTL),PVLEVT(IOUTL), PVLEVV 
414         FORMAT(A,' at ',A,' = ',A) 
            CALL PRINTA2 (THETAP(1+(J1-1)*NX*NY), NX, IX1, IX2, 
     *      IY1, IY2, LABEL, GRID, LUN_PRT) 
          END IF
        END DO 
      END DO 
      WRITE(LUN_PRT,*) ' '
C
C  Close the UKMO assimilation correlative file
C
      CALL UKACLS(LUN,IERROR,MESAGE)
C
C  Print message returned by UKACLS
C
      CALL UKAPRT(MESAGE,LUN_PRT,IWIDTH)
C
      WRITE(LUN_PRT,*) ' '
      WRITE(LUN_PRT,*) ' IERROR AFTER UKACLS ',IERROR
C
CUCSS - the following section is for use in conjunction with UCSS
C
C  De-assign the UKMO assimilation correlative file
C
C     CALL DASLID(LID,DISP,,DUMMY_ATTR,STATUS)
C
C  Check STATUS returned by DASLID
C
C        
C  Set PASS_FAIL_FLAG, COND_CODE and COMMENTS 
C 
C     PASS_FAIL_FLAG = 'PASS'
C     COND_CODE = %LOC(SS$_NORMAL)
C
C     COMMENTS = ' EXAMPLE PROGRAM HAS FINISHED SUCCESSFULLY'
C
C
C  Call PGTERM to terminate the UCSS simulated services routines
C
C     CALL PGTERM(PASS_FAIL_FLAG,COND_CODE,COMMENTS)
C                             
      STOP
      END
      SUBROUTINE PRINTA2(ARRAY,KNX,KX1,KX2,KY1,KY2,LABEL,GRID,LUN_PRT)
C     Print sub-set of data from 2-D array
      IMPLICIT NONE
      CHARACTER*(*) LABEL
      INTEGER KNX,KX1,KX2,KY1,KY2,LUN_PRT
      REAL ARRAY(KNX,*),GRID(4)
      INTEGER JX,JY
      REAL ZLAT1, ZLAT2, ZLON1, ZLON2
C
      ZLAT1=GRID(1)+(KY1-1)*GRID(2)
      ZLAT2=GRID(1)+(KY2-1)*GRID(2)
      ZLON1=GRID(3)+(KX1-1)*GRID(4)
      ZLON2=GRID(3)+(KX2-1)*GRID(4)
      WRITE (LUN_PRT,101) LABEL, KY1, KY2, KX1, KX2,
     + ZLAT1, ZLAT2, ZLON1, ZLON2
101   FORMAT('0',A/' Rows ',I4,' to ',I4,'  columns ',I4,' TO ',I4/
     + ' Latitudes (N) ',F7.2,' to ',F7.2, 
     + ' longitudes (E) ',F7.2,' to ',F7.2)  
C
      DO JY=KY1,KY2
        WRITE(LUN_PRT,*) (ARRAY(JX,JY),JX=KX1,KX2)
      END DO
C
      RETURN
      END     
C                 
C                 
C   More information on the use of PGINIT, ASGCOR, DASLID and PGTERM or
C on how to link the assimilation access routines with the RAC simulated
C services can be found in the UCSS Programmer's Guide or through 
C UARS::UCSSTECH
C                                  

