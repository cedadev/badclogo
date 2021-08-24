      SUBROUTINE UKAFLD(LUN,KTYPE,PLEVEL,                                       
     + PFIELD,KROW,KNPT,PGRID,KTIME,PMDI,IERROR,MESAGE)                         
CL======================================================================        
CL    SUBROUTINE UKAFLD(LUN,KTYPE,PLEVEL,                                       
CL   + PFIELD,KROW,KNPT,PGRID,KTIME,PMDI,IERROR,MESAGE)                         
C                                                                               
CL    Obtains a 2-dimensional field of UKMO assimilation data                   
C                                                                               
CL    A field of assimilated data for a specified parameter at a                
CL    given pressure level is obtained from the correlative data file.          
CL    Details of the grid used, and the time of the data are also               
CL    returned.                                                                 
C                                                                               
CL    NB. The grid used may depend on the parameter                             
CL        The user must supply at least 7008 words to store a field             
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
C                                                                               
CL Arguments:                                                                   
CL                                                                              
CL    LUN          IN      Unit Number                                          
CL    KTYPE        IN      Requested field type                                 
CL    PLEVEL       IN      Requested pressure level (mb)                        
CL    PFIELD(*)    OUT     Grid point data field                                
CL    KROW         OUT     Number of rows in grid                               
CL    KNPT         OUT     Number of points per row in grid                     
CL    PGRID(4)     OUT     Array showing grid structure on each level           
CL                         (1) Latitude of first row (North +ve)                
CL                         (2) Latitude gridlength                              
CL                             (-ve means latitudes are stored N to S)          
CL                             (+ve  ,,      ,,      ,,   ,,   S ,, N)          
CL                         (3) Longitude of first point in row (E +ve)          
CL                         (4) Longitude gridlength                             
CL                             (-ve means longitudes are stored E to W)         
CL                             (+ve  ,,       ,,      ,,   ,,   W ,, E)         
CL                         All elements of PGRID are in degrees                 
CL    KTIME(4)     OUT     Array showing time at which field is valid           
CL                         (1) Year                                             
CL                         (2) Month                                            
CL                         (3) Day                                              
CL                         (4) Hour (GMT)                                       
CL    PMDI         OUT     The value used to indicate missing or bad            
CL                         data                                                 
CL    IERROR       OUT     Set non-zero to indicate error                       
CL    MESAGE       OUT     Error message                                        
C                                                                               
      INTEGER LUN, KROW, KNPT, KTIME, IERROR                                    
      REAL PLEVEL, PFIELD, PGRID, PMDI                                          
      CHARACTER*(*) KTYPE                                                       
      CHARACTER*256 MESAGE                                                      
      DIMENSION PFIELD(*), PGRID(4), KTIME(4)                                   
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
C                                                                               
C Local variables:                                                              
      INTEGER IFC, IFLD, IH, ID, J                                              
C     IFC          PP field code                                                
C     IFLD         Field number in buffer                                       
C     IH           Address of header in buffer                                  
C     ID           Offest between fields                                        
C     J            Loop counter                                                 
C                                                                               
C-----------------------------------------------------------------------        
C                                                                               
C     Set zero error code initially                                             
      IERROR = 0                                                                
C                                                                               
C     Initialise MESAGE                                                         
      MESAGE = ' '                                                              
C                                                                               
CL*** 1       USE UKACHK TO VERIFY REQUESTED PARAMETERS                         
C                                                                               
      CALL UKACHK(KTYPE,IFC,PLEVEL,PLEVEL,IERROR,MESAGE)                        
      IF(IERROR.GT.0) RETURN                                                    
C                                                                               
CL*** 2       USE UKAGET TO OBTAIN REQUIRED FIELD                               
C                                                                               
      CALL UKAGET(LUN,IFC,PLEVEL,IFLD,KTIME,PMDI,IERROR,MESAGE)                 
      IF(IERROR.GT.0) RETURN                                                    
C                                                                               
CL*** 3       SET UP DATA TO BE RETURNED TO CALLING PROGRAM                     
C                                                                               
      IH=NPBGN(IFLD)                                                            
C                                                                               
C---- Copy field                                                                
      ID=NPFD(IFLD)-1                                                           
      DO 310 J=1,LBLREC(IH)                                                     
      PFIELD(J)=B(J+ID)                                                         
310   CONTINUE                                                                  
C                                                                               
C---- Set grid information                                                      
      KROW=LBROW(IH)                                                            
      KNPT=LBNPT(IH)                                                            
      PGRID(1)=BZY(IH)+BDY(IH)                                                  
      PGRID(2)=BDY(IH)                                                          
      PGRID(3)=BZX(IH)+BDX(IH)                                                  
      PGRID(4)=BDX(IH)                                                          
C                                                                               
      RETURN                                                                    
C                                                                               
      END                                                                       
