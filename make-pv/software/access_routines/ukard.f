      SUBROUTINE UKARD (LUN, KH, KF, KFORM, IOERR, IERROR, MESAGE)              
CL======================================================================        
CL    SUBROUTINE UKARD (LUN, KH, KF, KFORM, IOERR, IERROR, MESAGE)              
C                                                                               
CL    Read header and field from file attached to unit number LUN.              
CL    Header and field are read into specified areas of the buffer.             
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
CL  Arguments                                                                   
CL                                                                              
CL I  LUN           IN    Unit number                                           
CL I  KH            IN    Address of header in buffer B                         
CL I  KF            IN    Address of field data in buffer B                     
CL I  KFORM         IN    Format (1=CDHF, 2=PP, 3=FP, 4=DAAC)                   
CL I  IOERR         OUT   I/O status from READ (-ve for end-of-file)            
CL I  IERROR        OUT   Error code for other error                            
CL C  MESAGE        OUT   Error message for non-zero IERROR                     
C                                                                               
      INTEGER LUN, KH, KF, KFORM, IOERR, IERROR                                 
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
      INTEGER J, JH, JF, I1, I2                                           
      INTEGER IDSIZE, ICHUNK, IEXTRA                                            
      REAL RHEAD(LHEAD), Z                                                         
      INTEGER IHEAD(LHEAD)                                                      
      EQUIVALENCE (IHEAD(1),RHEAD(1))                                           
C-----------------------------------------------------------------------        
C     NB  PP/FP formats assume rectangular grid (eg. lat-long)                  
C                                                                               
CL    1       CDHF format                                                       
      IOERR=0                                                                   
      IF (KFORM.EQ.1) THEN                                                      
        READ (LUN, IOSTAT=IOERR) (B(KH+JH),JH=0,LHEAD-1),                       
     +  (B(KF+JF),JF=0,MDSIZE-1)                                                
        IF (IOERR.NE.0) RETURN                                                  
C                                                                               
CL    2       PP format                                                         
      ELSE IF (KFORM.EQ.2) THEN                                                 
C       read header (use correct integer/real types, in case                    
C       numbers are converted automatically)                                    
        READ (LUN, IOSTAT=IOERR) (IHEAD(JH),JH=1,LIHEAD),                       
     +   (RHEAD(J),J=LIHEAD+1,LHEAD)                                            
        IF (IOERR.NE.0) RETURN                                                  
        DO JH=1,LHEAD                                                           
          LB(KH+JH-1)=IHEAD(JH)                                                 
        END DO                                                                  
C       field size, without any extra data                                      
        IDSIZE=LBROW(KH)*LBNPT(KH)                                              
C       ensure field size is set correctly in header                            
        LBLREC(KH)=IDSIZE                                                       
        LBEXT(KH)=0                                                             
C       Check for header release (only process 64-word version, r=2)            
        IF (LBREL(KH).NE.2) THEN                                                
          IERROR=306                                                            
          WRITE(MESAGE,213) LBREL(KH),IERROR                                    
213       FORMAT(' UKAOPN - Invalid PP header release number;',                 
     +   ' Header version =',I10,                                               
     +   '; Error code =',I10)                                                  
          RETURN                                                                
        END IF                                                                  
C       read field (ignore any extra data)                                      
        READ (LUN, IOSTAT=IOERR) (B(KF+JF),JF=0,IDSIZE-1)                       
        IF (IOERR.NE.0) RETURN                                                  
C                                                                               
CL    3       FP format                                                         
      ELSE IF (KFORM.EQ.3) THEN                                                 
C       read header                                                             
        READ (LUN, IOSTAT=IOERR) (RHEAD(JH),JH=1,LHEAD)                         
        IF (IOERR.NE.0) RETURN                                                  
C       convert integer part from reals (and copy back to IHEAD)                
        DO JH=1,LIHEAD                                                          
          LB(KH+JH-1)=INT(RHEAD(JH))                                            
        END DO                                                                  
C       copy reals as they are                                                  
        DO JH=LIHEAD+1,LHEAD                                                    
          B(KH+JH-1)=RHEAD(JH)                                                  
        END DO                                                                  
C       size of field, without any extra data                                   
        IDSIZE=LBROW(KH)*LBNPT(KH)                                              
C       chunk size in word #29 (LBEGIN), FP extra data in #30 (LBNREC)          
        ICHUNK=LBEGIN(KH)                                                       
        IEXTRA=LBNREC(KH)                                                       
C       ensure field size is set correctly in header                            
        LBLREC(KH)=IDSIZE                                                       
        LBEXT(KH)=0                                                             
C       Check for header release (only process 64-word version, r=2)            
        IF (LBREL(KH).NE.2) THEN                                                
          IERROR=306                                                            
          WRITE(MESAGE,213) LBREL(KH),IERROR                                    
          RETURN                                                                
        END IF                                                                  
C       read field data (split into chunks, max size ICHUNK)                    
        DO J=1,IDSIZE,ICHUNK                                                    
          I1=J+KF-1                                                             
          I2=MIN(IDSIZE,J+ICHUNK-1)+KF-1                                        
          READ (LUN, IOSTAT=IOERR) (B(JF),JF=I1,I2)                             
          IF (IOERR.NE.0) RETURN                                                
        END DO                                                                  
C       if there is an "extra data" record, skip it                             
        IF (IEXTRA.GT.0) THEN                                                   
          READ (LUN, IOSTAT=IOERR)                                              
          IF (IOERR.NE.0) RETURN                                                
        END IF                                                                  
C                                                                               
CL    4       DAAC format                                                       
      ELSE ! IF (KFORM.EQ.4) THEN                                               
C       if current record > total records, end of file has been                 
C       reached; set negative status & return.                                  
        IF (MCREC.GT.MNREC) THEN                                                
          IOERR=-1                                                              
          RETURN                                                                
        END IF                                                                  
        READ (LUN, REC=MCREC, IOSTAT=IOERR) (B(KH+JH),JH=0,LHEAD-1),            
     +  (B(KF+JF),JF=0,MDSIZE-1)                                                
        IF (IOERR.NE.0) RETURN                                                  
      END IF                                                                    
C                                                                               
CL    5       Finish                                                            
      MCREC=MCREC+1                                                             
C                                                                               
C     If PP header codes 49/49 (u/v) are found, convert to 56/57 for            
C     consistency with expected values                                          
      IF(LBFC(KH).EQ.48) LBFC(KH)=56                                            
      IF(LBFC(KH).EQ.49) LBFC(KH)=57                                            
C                                                                               
C     If height field is found (LBFC=1), ensure units are m                     
      IF (LBFC(KH).EQ.1) THEN                                                   
        IF (BMKS(KH).LT.0.999.OR.BMKS(KH).GT.1.001) THEN                        
          Z=BMKS(KH)                                                            
          BMKS(KH)=1.0                                                          
          IDSIZE=LBROW(KH)*LBNPT(KH)                                            
          DO JF=0,IDSIZE-1                                                      
            B(KF+JF)=Z*B(KF+JF)                                                 
          END DO                                                                
        END IF                                                                  
      END IF                                                                    
C                                                                               
      RETURN                                                                    
C                                                                               
      END                                                                       
