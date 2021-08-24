      SUBROUTINE UKAGET(LUN,KFC,PLEVEL,KFLD,KTIME,PMDI,IERROR,MESAGE)           
CL======================================================================        
CL    SUBROUTINE UKAGET(LUN,KFC,PLEVEL,KFLD,KTIME,PMDI,IERROR,MESAGE)           
C                                                                               
CL    Get the requested UKMO correlative data field                             
CL    Called by either UKAFLD or UKAPRF                                         
C                                                                               
CL    The correlative data file is searched for the requested field             
CL    type and pressure level; vertical interpolation is carried out            
CL    if necessary.                                                             
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
C                                                                               
CL Arguments:                                                                   
CL                                                                              
CL    LUN       IN      Unit Number                                             
CL    KFC       IN      Requested field type (PP field code)                    
CL    PLEVEL    IN      Requested pressure level (mb)                           
CL    KFLD      OUT     Field number containing requested data                  
CL    KTIME(4)  OUT     Array showing time at which field is valid              
CL                      (1) Year                                                
CL                      (2) Month                                               
CL                      (3) Day                                                 
CL                      (4) Hour (GMT)                                          
CL    PMDI      OUT     The value used to indicate missing or bad data          
CL    IERROR    OUT     Non-zero indicates error                                
CL    MESAGE    OUT     Error message                                           
C                                                                               
      INTEGER LUN, KFC, KFLD, KTIME, IERROR                                     
      DIMENSION KTIME(4)                                                        
      REAL PLEVEL, PMDI                                                         
      CHARACTER*256 MESAGE                                                      
C                                                                               
C Common blocks:                                                                
*CALL COMBUF                                                 ***** 156          
C-----------------------------------------------------------------------        
C                                                                               
C     Common /COMBUF/ is used as a buffer to hold UKMO correlative              
C     data fields.  The format is based on that used in the UKMO                
C     Post-Processing package.                                                  
C                                                                               
C     Common /COMPNT/ records section of COMBUF used by each field,             
C     and actual record and field lengths.                                      
C                                                                               
      INTEGER LHEAD, LDSIZE, LRSIZE, LBSIZE, NFLDX, LIHEAD                      
      PARAMETER (LHEAD=64)              ! Words in PP header                    
      PARAMETER (LDSIZE=7008)           ! Max words of PP data                  
      PARAMETER (LRSIZE=LHEAD+LDSIZE)   ! Max record length                     
      PARAMETER (NFLDX=3)               ! Max no. fields in buffer              
      PARAMETER (LBSIZE=NFLDX*(LRSIZE)) ! Size of buffer COMBUF                 
      PARAMETER (LIHEAD=45)             ! Integers in header                    
C                                                                               
      INTEGER LBYR, LBMON, LBDAT, LBHR, LBMIN, LBDAY,                           
     *  LBYRD, LBMOND, LBDATD, LBHRD, LBMIND, LBDAYD,                           
     *  LBTIM, LBFT, LBLREC, LBCODE, LBHEM, LBROW,                              
     *  LBNPT, LBEXT, LBPACK, LBREL, LBFC, LBCFC,                               
     *  LBPROC, LBVC, LBRVC, LBEXP, LBEGIN, LBNREC,                             
     *  LBPROJ, LBTYP, LBLEV, LBRSVD, LBSRCE, LBUSER, LB                        
      REAL  BRSVD, BDATUM, BACC, BLEV, BRLEV, BHLEV,                            
     *  BHRLEV, BPLAT, BPLON, BGOR, BZY, BDY, BZX,                              
     *  BDX, BMDI, BMKS, BDATA, B                                               
      COMMON /COMBUF/                                                           
     *  LBYR(1), LBMON(1), LBDAT(1), LBHR(1), LBMIN(1), LBDAY(1),               
     *  LBYRD(1), LBMOND(1), LBDATD(1), LBHRD(1), LBMIND(1), LBDAYD(1),         
     *  LBTIM(1), LBFT(1), LBLREC(1), LBCODE(1), LBHEM(1), LBROW(1),            
     *  LBNPT(1), LBEXT(1), LBPACK(1), LBREL(1), LBFC(1), LBCFC(1),             
     *  LBPROC(1), LBVC(1), LBRVC(1), LBEXP(1), LBEGIN(1), LBNREC(1),           
     *  LBPROJ(1), LBTYP(1), LBLEV(1), LBRSVD(4), LBSRCE(1), LBUSER(7),         
     *  BRSVD(4), BDATUM(1), BACC(1), BLEV(1), BRLEV(1), BHLEV(1),              
     *  BHRLEV(1), BPLAT(1), BPLON(1), BGOR(1), BZY(1), BDY(1), BZX(1),         
     *  BDX(1), BMDI(1), BMKS(1), BDATA(LDSIZE)                                 
      DIMENSION B(LBSIZE),LB(LBSIZE)                                            
      EQUIVALENCE (B(1),LB(1),LBYR(1))                                          
C                                                                               
C     DESCRIPTION OF HEADER PARAMETERS:                                         
C                                                                               
*-->  ALL HEADER PARAMETERS SHOULD BE SET ZERO IF NOT RELEVANT.                 
* 1-6 LBYR, LBMON, LBDAT, LBHR, LBMIN, LBDAY  -  TIME VALID (TIME1)             
* 7+  LBYRD,LBMOND,LBDATD,LBHRD,LBMIND,LBDAYD -  DATA TIME ETC (TIME2)          
* 13  LBTIM   TIME INDICATOR = 100*IA + 10*IB + IC                              
*         IA = AVERAGING INTERVAL (HRS); 0 IF NOT SPECIFIED                     
*         IB = 0 SINGLE TIME (TIME1) ONLY                                       
*              1 FORECAST FROM TIME2 VALID AT TIME1                             
*              2 AVERAGE FROM TIME1 TO TIME2                                    
*              3 AVERAGE FROM TIME1 TO TIME2 FOR EACH YR LBYR TO LBYRD          
*              4 DIFFERENCE TIME2-TIME1 BETWEEN FIELDS                          
*         IC -  CALENDAR TYPE;   0 MODEL TIME ONLY                              
*          1 REAL CALENDAR       2 360-DAY CALENDAR                             
*          3 MODEL TIME ONLY; IGNORE YR, MON, DAT                               
* 14  LBFT    (FOR FORECAST FIELDS) FORECAST PERIOD IN HOURS                    
* 15  LBLREC  LENGTH OF FIELD RECORD IN WORDS (INCLUDING EXTRA DATA)            
* 16  LBCODE  GRID CODE                                                         
*             1=REGULAR LAT-LONG, 2=LAT-LONG 'BOX'                              
*             3=POLAR STEREOGRAPHIC                                             
*             4=SPECTRAL COEFFICIENTS,                                          
*             7=MERCATOR                                                        
*             FOR CROSS SECTIONS ETC LBCODE=10000+100*IX+IY WHERE IX            
*             & IY ARE CODES FOR X & Y AXES:                                    
*             0=SIGMA, 1=PRESSURE (MB), 2=HEIGHT ASL (KM), 10=LAT (N),          
*             11=LONG (DEG E), 20=TIME (DAYS), 31=LOG10 P                       
* 17  LBHEM   HEMISPHERE / LIMITED AREA INDICATOR                               
*             0=GLOBE,                                                          
*             1=NORTH, 2=SOUTH,    (FOR P.S. GRIDS SPECIFY 1 OR 2)              
*             3=LAT/LONG LTD AREA  (I.E.  NO 'WRAP AROUND')                     
*             4=LAT/LONG CHANNEL   (I.E. WITH 'WRAP AROUND')                    
* 18  LBROW   NUNBER OF ROWS IN GRID (E-W FOR LAT-LONG GRIDS)                   
* 19  LBNPT   NUMBER OF POINTS PER ROW OF GRID                                  
* 20  LBEXT   LENGTH OF EXTRA DATA IN WORDS. EXTRA DATA FOLLOWS FIELD           
*             DATA AND IS ARRANGED IN VECTORS EACH BEING MADE UP OF AN          
*             INTEGER CODE (=1000*IA+IB) FOLLOWED BY IA DATA VALUES             
*             IB=1 VECTOR GIVES LBNPT X-COORDINATE VALUES  *                    
*                2 VECTOR GIVES LBROW Y COORDINATE VALUES  *                    
*               10 CHARACTER TITLE OF FIELD                                     
*            * - FOR CROSS SECTIONS ONLY; EACH HAS 1 X- & 1 Y-VECTOR            
* 21  LBPACK  PACKING INDICATOR - 0=NOT PACKED, 1=WGDOS ARCHIVE PACKING         
* 22  LBREL   HEADER RELEASE FORMAT - SET TO "2" FOR THIS HEADER FORMAT         
* 23  LBFC    FIELD CODE FROM FOLLOWING LIST:                                   
*             1=Z, 3=FI, 8=PRESSURE, 10=SIGMA 16=T, 19=POT.T, 40=OMEGA,         
*             45=SIGDT, 48/56=U, 49/57=V ,63=KE, 73=VORT, 74=DIV,               
*             80=STRMFNCTN, 81=VELPOT, 88=RH, 90=PPTN,                          
*             93=SNOWDEP, 94=SHOWER, 95=Q, 102=LRAIN, 103=SNOW,                 
*             104=RAIN, 105=EVAP ,106=SMC, 178=SENSHT                           
*             128=SEA LEVEL, 129=SURFACE,130=TROP, 176=LAT, 177=LON             
*             (SEE "M11.PPREF.DATA(@FCODES)" FOR FULLER LIST.)                  
* 24  LBCFC   2ND FIELD CODE FOR COMBINATION OF 2 BASIC FIELDS                  
* 25  LBPROC  PROCESSING CODE : BUILT UP BY ADDING -                            
*             0 BASIC FIELD                                                     
*             1 DIFFERENCE FROM ANOTHER EXPERIMENT                              
*             2 DIFFERENCE FROM ZONAL-(OR SOME OTHER SPACE)-MEAN                
*             4 DIFFERENCE FROM TIME-MEAN (OR HIGH OR BAND PASS FILTER)         
*             8 D/DX                                                            
*            16 D/DY                                                            
*            32 D/DT                                                            
*            64 ZONAL MEAN (OR SPATIALLY SMOOTHED)                              
*           128 TIME MEAN (OR TIME SMOOTHED - LOW PASS FILTERED)                
*           256 PRODUCT OF 2 FIELDS                                             
*           512 SQUARE-ROOT                                                     
*          1024 DIFFERENCE BETWEEN BLEV AND BRLEV FIELDS                        
*          2048 MEAN BETWEEN BLEV & BRLEV                                       
* 26  LBVC    'VERTICAL' COORDINATE TYPE. FOR CROSS SECTIONS ETC THIS           
*             IS THE COORDINATE PERP TO PLANE OF SECTION. CODED AS LBFC         
* 27  LBRVC   REF. LEVEL (BRLEV) VERTICAL COORDINATE                            
* 28  LBEXP   EXPERIMENT NUMBER (FOR USER'S REFERENCE)                          
* 29  LBEGIN  (ON DIRECT ACCESS DATASETS ONLY) START RECORD NUMBER              
* 30  LBNREC  (ON DIRECT ACCESS DATASETS ONLY) NUMBER OF RECORDS                
* 31  LBPROJ  (FOR MET.O.8 FIELDS FILE USE) MET.O.8 PROJECTION NUMBER           
* 32  LBTYP   (FOR MET.O.8 FIELDS FILE USE) MET.O.8 FIELD TYPE CODE             
* 33  LBLEV   (FOR MET.O.8 FIELDS FILE USE) MET.O.8 LEVEL CODE                  
* 34+ LBRSVD(4)  RESERVED FOR FUTURE PP-PACKAGE USE                             
* 38  LBSRCE  CODE GIVING SOURCE OF FIELD (FOR USER'S INFORMATION ONLY)         
* 39+ LBUSER(7)  SPARES FOR GENERAL USE   (EQUIVALENCED TO 'BUSER')             
* 46+ BRSVD(4)   RESERVED FOR FUTURE PP-PACKAGE USE                             
* 50  BDATUM  OFFSET FROM 0 (=273.15 FOR TEMPERATURE FIELDS IN CELSIUS)         
* 51  BACC    (PACKED DATA) PACKING ACCURACY OF FIELD                           
* 52  BLEV    VERTICAL LEVEL (VALUE OF CO-ORDINATE DEFINED BY "LBVC")           
* 53  BRLEV   SECOND VERTICAL LEVEL (NEAREST GROUND)                            
* 54  BHLEV   (HYBRID LEVELS) A-VALUE OF LEVEL                                  
* 55  BHRLEV  (HYBRID LEVELS) A-VALUE OF REFERENCE LEVEL                        
* 56  BPLAT   REAL LATITUDE OF 'PSEUDO' NORTH POLE OF GRID                      
* 57  BPLON   REAL LONGITUDE OF 'PSEUDO' NORTH POLE OF GRID                     
*            LAT-LONG GRIDS       /  POLAR STEREO GRIDS                         
* 58  BGOR   0.0                  /  GRID ORIENTATION (VERTICAL LONG)           
* 59  BZY    ZEROTH LATITUDE      /  90.0                                       
* 60  BDY    LATITUDE INTERVAL    /  GRID LENGTH (M) AT POLE                    
* 61  BZX    ZEROTH LONGITUDE     /  X CO-ORD OF POLE (PT. NO. IN ROW)          
* 62  BDX    LONGITUDE INTERVAL   /  Y CO-ORD OF POLE (ROW NUMBER)              
*            (CROSS SECTIONS AS LAT-LONG WITH X=LON, Y=LAT)                     
* 63  BMDI    MISSING DATA INDICATOR                                            
* 64  BMKS    MKS SCALING FACTOR (= FIELD UNITS / M.K.S UNITS)                  
C                                                                               
C     BDATA   CONTAINS THE ACTUAL FIELD VALUES                                  
C                                                                               
C-----------------------------------------------------------------------        
C                                                                               
      INTEGER NPND, NPBGN, NPFD, NPXT, NFLD, MRSIZE, MDSIZE                     
C                                                                               
      COMMON /COMPNT/ NPND(0:NFLDX), NPBGN(NFLDX), NPFD(NFLDX),                 
     *                NPXT(NFLDX), NFLD, MRSIZE, MDSIZE                         
C                                                                               
C     LHEAD   (IN PARAMETER STMT) LENGTH OF HEADER                              
C     NFLDX   (IN PARAMETER STMT) MAX ALLOWED NO OF FIELDS                      
C     NPBGN   NTH ELEMENT POINTS TO BEGINNING OF NTH FIELD'S HEADER             
C     NPFD    POINTS TO BEGINNING OF EACH FIELD'S DATA                          
C     NPND    ONE AFTER END OF EACH FIELD                                       
C     NPXT    BEGINNING OF EXTRA DATA (or padding, if any;=NPND if none)        
C     NFLD    NO OF FIELDS POINTED TO                                           
C     MRSIZE  Actual record length in file being processed                      
C     MDSIZE  Maximum field length consistent with MRSIZE                       
C             NB. MRSIZE & MDSIZE are only correct for CDHF (MFORM=1);          
C             for PP & FP, the lengths are taken from the headers               
C                                                                               
*CALL COMPAR                                                 *****  29          
C-----------------------------------------------------------------------        
C                                                                               
C     Common /COMPAR/ contains information about parameters stored              
C     in the current UKMO correlative data file                                 
C                                                                               
      INTEGER NTYPEX                                                            
      PARAMETER (NTYPEX=9)                                                      
      INTEGER NTYPED                                                            
      PARAMETER (NTYPED=24)                                                     
C                                                                               
      REAL PLEVMAX, PLEVMIN, RMDI                                               
      INTEGER NTYPE, MFC, MFORM, MCREC, MNREC                                   
      CHARACTER*12 MTYPE                                                        
C                                                                               
      COMMON /COMPAR/ PLEVMAX, PLEVMIN, RMDI,                                   
     + MFORM, MCREC, MNREC, NTYPE, MFC(NTYPEX), MTYPE(NTYPEX)                   
C                                                                               
C     PLEVMAX  MAXIMUM PRESSURE LEVEL                                           
C     PLEVMIN  MINIMUM PRESSURE LEVEL                                           
C     RMDI     MISSING DATA INDICATOR                                           
C     MFORM    Dataset format (1=CDHF, 2=PP, 3=FP, 4=DAAC)                      
C     MCREC    Current record number (for direct access (DAAC) files)           
C     MNREC    Number of records in file (CDHF/DAAC formats)                    
C     NTYPE    NUMBER OF PARAMETERS                                             
C     NTYPEX   Max. number of parameters allowed in one file                    
C     NTYPED   Max. number of parameters defined                                
C     MTYPE    LIST OF PARAMETERS                                               
C     MFC      EQUIVALENT PP FIELD CODES                                        
C                                                                               
C                                                                               
C Local variables:                                                              
      INTEGER IFLD, IH, J, ID1, ID2, IOERR                                      
C     IFLD          Field number currently being processed                      
C     IH            Address of header in buffer                                 
C     J             Loop counter                                                
C     ID1/2         Field offsets for vertical interpolation                    
C     IOERR         I/O status                                                  
      LOGICAL ILEND, ILFOUND, ILVINT, ILPERR                                    
C     ILEND         True if end of file has been reached on this call           
C     ILFOUND       True if required field(s) have been found                   
C     ILVINT        True if vertical interpolation is required                  
C     ILPERR        True if pressure is out of range                            
      REAL WT1, WT2                                                             
C     WT1/2         Vertical interpolation weights                              
C                                                                               
      SAVE IFLD                                                                 
C                                                                               
C     Define ILREQ, used to test that real numbers are 'equal';                 
C     similarly ILRGT, to test for P1>P2, but not 'equal' in this sense         
      LOGICAL ILREQ, ILRGT                                                      
      REAL P1, P2                                                               
      ILREQ(P1,P2)=ABS(P1-P2).LE.0.001*(P1+P2)                                  
      ILRGT(P1,P2)=P1.GT.P2 .AND. .NOT. ILREQ(P1,P2)                            
C-----------------------------------------------------------------------        
C                                                                               
CL*** 1       PRELIMINARIES; CHECK WHETHER FILE IS TO BE READ                   
C                                                                               
C---- Initialise Indicators                                                     
      ILEND=.FALSE.                                                             
      ILFOUND=.FALSE.                                                           
      ILVINT =.FALSE.                                                           
      ILPERR =.FALSE.                                                           
C                                                                               
100   CONTINUE                                                                  
C---- Check whether required field(s) have (already) been found                 
C---- (if so then jump to section 3)                                            
      IF(NFLD.NE.0) THEN                                                        
C---- First check for exact match                                               
        DO 110 J=1,NFLD                                                         
        IF (KFC.EQ.LBFC(NPBGN(J)) .AND.                                         
     +  ILREQ(PLEVEL,BLEV(NPBGN(J)))) THEN                                      
          IFLD=J                                                                
          ILFOUND=.TRUE.                                                        
          GO TO 300                                                             
        END IF                                                                  
110     CONTINUE                                                                
C---- Note that all other circumstances require at least 2 fields               
        IF(NFLD.GE.2 .AND.                                                      
     +  (KFC.EQ.LBFC(NPBGN(1)).AND.KFC.EQ.LBFC(NPBGN(2))) )THEN                 
C---- Check for requested level between levels in buffer                        
          IF ( (PLEVEL-BLEV(NPBGN(1)))*(PLEVEL-BLEV(NPBGN(2)))                  
     +    .LE.0.0)THEN                                                          
            ILFOUND=.TRUE.                                                      
            ILVINT=.TRUE.                                                       
C----       Set COMPNT pointers for interpolating data (section 3)              
C----       (we always interpolate to field 3 from fields 1 and 2)              
            NPBGN(3)=NPND(2)                                                    
            NPFD(3)=NPBGN(3)+LHEAD                                              
            NPXT(3)=NPFD(3)+(NPXT(1)-NPFD(1))                                   
            NPND(3)=NPFD(3)+MDSIZE                                              
            GO TO 300                                                           
C---- Check for requested level out of range                                    
          ELSE IF (ILRGT(PLEVMIN,PLEVEL) .OR.                                   
     +             ILRGT(PLEVEL,PLEVMAX)) THEN                                  
            ILFOUND=.TRUE.                                                      
            ILPERR=.TRUE.                                                       
C----       Set COMPNT pointers for field 3                                     
            NPBGN(3)=NPND(2)                                                    
            NPFD(3)=NPBGN(3)+LHEAD                                              
            NPXT(3)=NPFD(3)+(NPXT(1)-NPFD(1))                                   
            NPND(3)=NPFD(3)+MDSIZE                                              
            GO TO 300                                                           
          END IF                                                                
        END IF                                                                  
      END IF                                                                    
C                                                                               
CL*** 2       READ FILE                                                         
C                                                                               
      IF (.NOT.ILFOUND) THEN                                                    
C                                                                               
C----   Set IFLD value (i.e. next section of buffer to be used)                 
C----   IFLD=1 for empty buffer                                                 
        IF(NFLD.EQ.0) THEN                                                      
          IFLD=1                                                                
C                                                                               
C----   If previous field had correct field code, overwrite other one           
C----   (this should ensure we keep adjacent levels in the buffer for           
C----   any vertical interpolation)                                             
        ELSE IF(LBFC(NPBGN(IFLD)).EQ.KFC) THEN                                  
          IFLD=3-IFLD                                                           
C                                                                               
C----   Otherwise start again with field 1                                      
C----   (Reset NFLD, the total number of fields, too)                           
        ELSE                                                                    
          IFLD=1                                                                
          NFLD=1                                                                
        END IF                                                                  
C                                                                               
C----   Set COMPNT pointers                                                     
        NPBGN(IFLD)=NPND(IFLD-1)                                                
        NPFD(IFLD)=NPBGN(IFLD)+LHEAD                                            
        NPND(IFLD)=NPFD(IFLD)+MDSIZE                                            
        NFLD=MAX(IFLD,NFLD)                                                     
C                                                                               
        CALL UKARD (LUN, NPBGN(IFLD), NPFD(IFLD), MFORM, IOERR,                 
     +   IERROR, MESAGE)                                                        
        IF (IERROR.GT.0) THEN                                                   
          RETURN                                                                
        END IF                                                                  
C----   Rewind (once!) and try again if end of data is reached                  
        IF(IOERR.LT.0) THEN                                                     
          IF(ILEND) THEN                                                        
            IERROR=201                                                          
            WRITE(MESAGE,210) IOERR,IERROR                                      
210         FORMAT(' UKAGET - Requested field not found in',                    
     +      ' UKMO correlative data file;',                                     
     +      ' I/O status code =',I10,                                           
     +      ' ; Error code =',I10)                                              
            RETURN                                                              
          ELSE                                                                  
            CALL UKARWD(LUN,MFORM,IERROR,MESAGE)                                
            IF(IERROR.NE.0) RETURN                                              
            ILEND=.TRUE.                                                        
            NFLD=0                                                              
            GO TO 100                                                           
          END IF                                                                
        ELSE IF(IOERR.GT.0) THEN                                                
          IERROR=111                                                            
          WRITE(MESAGE,221) IOERR,IERROR                                        
221       FORMAT(' UKAGET - Error reading record from',                         
     +    ' UKMO correlative data file;',                                       
     +    ' I/O status code =',I10,                                             
     +    ' ; Error code =',I10)                                                
          RETURN                                                                
        END IF                                                                  
C----   Set NPXT to show section of data actually used                          
        NPXT(IFLD)=NPFD(IFLD)+LBLREC(NPBGN(IFLD))                               
C----   Go back to section 1 to check header                                    
        GO TO 100                                                               
      END IF                                                                    
C                                                                               
CL*** 3       VERTICAL INTERPOLATION                                            
C                                                                               
300   CONTINUE                                                                  
C                                                                               
C---- If requested pressure is between levels found, interpolate                
C---- (assumes fields vary linearly with log(pressure),                         
C---- and they are on the same grid)                                            
      IF (ILVINT) THEN                                                          
        WT1=LOG(PLEVEL/BLEV(NPBGN(2)))/                                         
     +      LOG(BLEV(NPBGN(1))/BLEV(NPBGN(2)))                                  
        WT2=1.0-WT1                                                             
        ID1=NPFD(1)-NPFD(3)                                                     
        ID2=NPFD(2)-NPFD(3)                                                     
C       Copy header (reset level)                                               
        IH=NPBGN(3)                                                             
        DO 310 J=IH,NPFD(3)-1                                                   
        B(J)=B(J+ID1)                                                           
310     CONTINUE                                                                
        BLEV(IH)=PLEVEL                                                         
C       Interpolate                                                             
        DO 312 J=NPFD(3),NPXT(3)-1                                              
        B(J)=WT1*B(J+ID1)+WT2*B(J+ID2)                                          
312     CONTINUE                                                                
C----   Set KFLD to indicate field 3 contains interpolated data                 
        KFLD=3                                                                  
        NFLD=3                                                                  
      ELSE IF(ILPERR) THEN                                                      
C---- If requested pressure is out of range, set field of MDI's                 
C       Copy header (reset level)                                               
        IH=NPBGN(3)                                                             
        ID1=NPFD(1)-NPFD(3)                                                     
        DO 320 J=IH,NPFD(3)-1                                                   
        B(J)=B(J+ID1)                                                           
320     CONTINUE                                                                
        BLEV(IH)=PLEVEL                                                         
C       Reset field                                                             
        IF(RMDI.EQ.0.0) RMDI=BMDI(IH)                                           
        DO 322 J=NPFD(3),NPXT(3)-1                                              
        B(J)=RMDI                                                               
322     CONTINUE                                                                
C----   Set KFLD to indicate field 3 contains data                              
        KFLD=3                                                                  
        NFLD=3                                                                  
      ELSE                                                                      
C----   Set KFLD to indicate field containing required data                     
        KFLD=IFLD                                                               
      END IF                                                                    
C                                                                               
CL*** 4       MISCELLANEOUS CHECKS, ETC.                                        
C                                                                               
      IH=NPBGN(KFLD)                                                            
C                                                                               
C---- Check grid type is one that can be processed                              
      IF((LBCODE(IH).NE.1.AND.LBCODE(IH).NE.2).OR.                              
     + LBHEM(IH).EQ.3) THEN                                                     
        IERROR=301                                                              
        WRITE(MESAGE,411) LBCODE(IH),LBHEM(IH),IERROR                           
411     FORMAT(' UKAGET - Cannot process this grid type from',                  
     + ' UKMO correlative data file;',                                          
     + ' Grid + area codes = ',2I10,                                            
     + ' ; Error code = ',I10)                                                  
        RETURN                                                                  
      END IF                                                                    
C                                                                               
C---- Check level type                                                          
      IF(LBVC(IH).NE.8) THEN                                                    
        IERROR=302                                                              
        WRITE(MESAGE,412) LBVC(IH),IERROR                                       
412     FORMAT(' UKAGET - Non-pressure-level field found in',                   
     + ' UKMO correlative data file;',                                          
     + ' Level type = ',I10,                                                    
     + ' ; Error code = ',I10)                                                  
        RETURN                                                                  
      END IF                                                                    
C                                                                               
C---- Check field length                                                        
      IF(LBROW(IH)*LBNPT(IH).NE.LBLREC(IH)) THEN                                
        IERROR=303                                                              
        WRITE(MESAGE,413) LBROW(IH),LBNPT(IH),LBLREC(IH),IERROR                 
413     FORMAT(' UKAGET - Field length inconsistent in',                        
     + ' UKMO correlative data file;',                                          
     + ' Rows, points/row, points/field = ',3I10,                               
     + ' Error code = ',I10)                                                    
        RETURN                                                                  
      END IF                                                                    
C                                                                               
C---- Set time information                                                      
      KTIME(1)=LBYR(IH)                                                         
      KTIME(2)=LBMON(IH)                                                        
      KTIME(3)=LBDAT(IH)                                                        
      KTIME(4)=LBHR(IH)                                                         
C                                                                               
C---- Ensure RMDI (in COMPAR) is set                                            
      IF(RMDI.EQ.0.0) RMDI=BMDI(IH)                                             
C---- Set PMDI equal to RMDI to pass out in the argument list                   
      PMDI = RMDI                                                               
C                                                                               
      RETURN                                                                    
      END                                                                       
