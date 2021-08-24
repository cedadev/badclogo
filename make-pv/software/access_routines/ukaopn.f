      SUBROUTINE UKAOPN (LUN, LID, GTYPE, DATE, PMAX, PMIN, DESCRP,            

     *  KTYPE, KNTYPE, IQUAL, KFORM, KLFAST, IERROR, MESAGE)                    
CL======================================================================        
CL    SUBROUTINE UKAOPN (LUN, LID, GTYPE, DATE, PMAX, PMIN, DESCRP,             
CL   *  KTYPE, KNTYPE, IQUAL, KFORM, KLFAST, IERROR, MESAGE)                    
C                                                                               
CL    Opens the specified UKMO correlative data file                            
C                                                                               
CL    It also reads the UARS header record and returns relevant                 
CL    information from it.                                                      
CL    It also returns an integer indicating the quality of the                  
CL    assimilation data.                                                        
CL    The header information is used to set up common block COMPAR,             
CL    to provide a record of the fields available.                              
CL    The file is positioned so that subsequent calls to UKAGET will            
CL    access the data fields.                                                   
CL    A new argument KFORM allows the option to read Met Office                 
CL    "PP" or PP floating-point or DAAC direct-access formats                   
CL    directly.                                                                 
C                                                                               
CL    NB. This code may need slight adjustment (in particular to "OPEN"         
CL    statements) when run on different operating systems!                      
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
C                                                                               
CL Arguments:                                                                   
CL                                                                              
CL    LUN          IN      Unit Number                                          
CL    LID          IN      File Identifier                                      
CL    GTYPE        OUT     General type of data in file                         
CL    DATE         OUT     Date of the data in VAX VMS format                   
CL    PMAX         OUT     Maximum pressure level in assimilation data          
CL                         (mb)                                                 
CL    PMIN         OUT     Minimum pressure level in assimilation data          
CL    DESCRP       OUT     Brief description of the data                        
CL    KTYPE        OUT     Array containing a list of the correlative           
CL                         data parameters present in the data  i.e. a          
CL                         list of the valid KTYPE's to be used when            
CL                         calling the subroutines UKAFLD or UKAPRF             
CL    KNTYPE       IN      Array length of KTYPE                                
CL                         (use KNTYPE >= 5 at present)                         
CL    IQUAL        OUT     Code indicating quality of the assimilation          
CL                         data (last digit of P.I. quality code)               
CL    KFORM        IN      Dataset format                                       
CL                         1 = CDHF                                             
CL                         2 = UKMO "PP format"                                 
CL                         3 = Floating-point PP ("semi-PP" / "FP")             
CL                         4 = DAAC                                             
CL    KLFAST       IN      "Fast open" switch.  For PP or FP formats            
CL                         (KFORM = 2 or 3) - otherwise ignored.                
CL                         If KLFAST=false, the program reads through           
CL                         the whole file to initialise parameters.             
CL                         If KLFAST=true, default settings are used,           
CL                         and the file is not scanned.                         
CL    IERROR       OUT     Non-zero indicates error                             
CL    MESAGE       OUT     Message indicating success or failure                
C                                                                               
      INTEGER LUN, IERROR, IQUAL, KFORM                                         
      INTEGER KNTYPE                                                            
      CHARACTER*(*) LID                                                         
      CHARACTER*12  GTYPE,KTYPE(KNTYPE)                                         
      CHARACTER*23  DATE                                                        
      CHARACTER*80  DESCRP                                                      
      CHARACTER*256 MESAGE                                                      
      REAL PMAX,PMIN                                                            
      LOGICAL KLFAST                                                            
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
      INTEGER LSFDU,LUARSH                                                      
      PARAMETER (LSFDU=86,LUARSH=400)                                           
      CHARACTER*86 CSFDU                                                        
C     SFDU                                                                      
C     The length of the SFDU should always be 86 bytes for this data            
      CHARACTER*400 CUARSH                                                      
C     UARS header                                                               
C     CUARSH must be long enough to hold all data in UARS header                
C                                                                               
      INTEGER ID, IM, IY                                                        
C     Day month year (from 'DATE')                                              
      INTEGER I, I1, I2, IP, IIH, IIF, J, JP, JM, IOERR                         
C     Misc temporary storage                                                    
C                                                                               
      CHARACTER*12 ITYPE(NTYPED)                                                
C     ITYPE        Complete set of possible parameters                          
      INTEGER IFC(NTYPED)                                                       
C     IFC          PP field codes corresponding to ITYPE                        
      CHARACTER*3 MONTH3(12)                                                    
C     MONTH3       3-digit abbreviations for months                             
      DATA ITYPE /                                                              
     + 'HEIGHT      ','TEMP        ','ZONWIN_P    ','MERWIN_P    ',             
     + 'OMEGA       ','O3          ','H2O         ','CO          ',             
     + 'CH4         ','N2O         ','NO          ','NO2         ',             
     + 'HNO3        ','N2O5        ','ClONO2      ','ClO         ',             
     + 'HCl         ','CF2Cl2      ','CFCl3       ','HF          ',             
     + '(heating?)  ','            ','            ','            '/             
      DATA IFC/                                                                 
     + 1             ,16            ,56            ,57            ,             
     + 40            ,501           ,502           ,503           ,             
     + 504           ,505           ,506           ,507           ,             
     + 508           ,509           ,510           ,511           ,             
     + 512           ,513           ,514           ,515           ,             
     + 0             ,0             ,0             ,0             /             
      DATA MONTH3/                                                              
     + 'JAN','FEB','MAR','APR','MAY','JUN',                                     
     + 'JUL','AUG','SEP','OCT','NOV','DEC'/                                     
      DATA CUARSH/' '/                                                          
C-----------------------------------------------------------------------        
C                                                                               
CL*** 1       OPEN FILE                                                         
C                                                                               
C     Set zero error codes initially                                            
      IERROR=0                                                                  
      IOERR =0                                                                  
C                                                                               
C     Initialise MESAGE                                                         
      MESAGE = ' '                                                              
C                                                                               
C     Verify file format                                                        
      IF (KFORM.LT.1.OR.KFORM.GT.4) THEN                                        
        IERROR=211                                                              
        WRITE(MESAGE,102) KFORM,IERROR                                          
102     FORMAT(' UKAOPN - Invalid correlative data file format;',               
     +  ' Requested format =',I10,                                              
     +  '; Error code =',I10)                                                   
        RETURN                                                                  
      END IF                                                                    
C                                                                               
C     Open file (record type depends on KFORM)                                  
C     KFORM=1 - CDHF                                                            
      IF (KFORM.EQ.1) THEN                                                      
        OPEN (UNIT=LUN,FILE=LID,STATUS='OLD',                                   
*IF VAX                                                                         
*     +   RECORDTYPE='FIXED', READONLY,                                          
*END VAX                                                                        
     +   ACCESS='SEQUENTIAL',FORM='UNFORMATTED',IOSTAT=IOERR)                   
C     KFORM=2 - Standard Met Office PP                                          
      ELSE IF (KFORM.EQ.2) THEN                                                 
        OPEN (UNIT=LUN,FILE=LID,STATUS='OLD',                                   
*IF VAX                                                                         
*     +   RECORDTYPE='SEGMENTED', READONLY,                                      
*END VAX                                                                        
     +   ACCESS='SEQUENTIAL',FORM='UNFORMATTED',IOSTAT=IOERR)                   
C     KFORM=3 - PP floating-point version                                       
      ELSE IF (KFORM.EQ.3) THEN                                                 
        OPEN (UNIT=LUN,FILE=LID,STATUS='OLD',                                   
*IF VAX                                                                         
*     +   RECORDTYPE='VARIABLE', READONLY,                                       
*END VAX                                                                        
     +   ACCESS='SEQUENTIAL',FORM='UNFORMATTED',IOSTAT=IOERR)                   
C     KFORM=4 - DAAC                                                            
C     NB. record length specification may be 7072 (in words - Sun?),            
C     or                                     28288 (in bytes - HP?)             
      ELSE IF (KFORM.EQ.4) THEN                                                 
        OPEN (UNIT=LUN,FILE=LID,STATUS='OLD',                                   
     +   ACCESS='DIRECT',FORM='UNFORMATTED', RECL=28288,                        
     +   IOSTAT=IOERR)                                                          
      END IF                                                                    
      IF(IOERR.NE.0) THEN                                                       
        IERROR=101                                                              
        WRITE(MESAGE,101) IOERR,IERROR                                          
101     FORMAT(' UKAOPN - Error opening UKMO correlative data file;',           
     +  ' I/O status code =',I10,                                               
     +  '; Error code =',I10)                                                   
        RETURN                                                                  
      END IF                                                                    
C                                                                               
C     Initialise COMPNT parameters                                              
      NPND(0)=1                                                                 
      NFLD=0                                                                    
C                                                                               
C     Set format in MFORM                                                       
      MFORM=KFORM                                                               
C     Record number (for direct access files only)                              
      MCREC=1                                                                   
C     Total number of records; reset from UARS header for CDHF/DAAC             
      MNREC=0                                                                   
C     Set RMDI to zero (reset from 1st field processed)                         
      RMDI=0.0                                                                  
C     Set no. types to 0                                                        
      NTYPE=0                                                                   
C     Set KTYPE to blanks                                                       
      DO JP=1,KNTYPE                                                            
        KTYPE(JP)='    '                                                        
      END DO                                                                    
C     For PP and FP, skip to section 4                                          
      IF (KFORM.EQ.2.OR.KFORM.EQ.3) GO TO 400                                   
C                                                                               
CL*** 2       READ DATA FROM UARS HEADER                                        
C                                                                               
      IF (KFORM.NE.4) THEN                                                      
        READ(LUN,IOSTAT=IOERR) CSFDU,CUARSH                                     
      ELSE                                                                      
        READ(LUN,REC=MCREC,IOSTAT=IOERR) CSFDU,CUARSH                           
      END IF                                                                    
      MCREC=MCREC+1                                                             
C     (Any remaining data in this record is ignored)                            
      IF(IOERR.NE.0) THEN                                                       
        IERROR=102                                                              
        WRITE(MESAGE,201) IOERR,IERROR                                          
201     FORMAT(' UKAOPN - Error reading header record of',                      
     +  ' UKMO correlative data file;',                                         
     +  ' I/O status code =',I10,                                               
     +  '; Error code =',I10)                                                   
        RETURN                                                                  
      END IF                                                                    
C                                                                               
      I1=77                                                                     
      I2=I1+11                                                                  
      GTYPE = CUARSH(I1:I2)                                                     
      I1=89                                                                     
      I2=I1+22                                                                  
      DATE = CUARSH(I1:I2)                                                      
C                                                                               
C     Extract day/month/year                                                    
      READ (CUARSH(I1:I1+1),'(I2)') ID                                          
      IM=0                                                                      
      DO JM=1,12                                                                
        IF (CUARSH(I1+3:I1+5).EQ.MONTH3(JM)) IM=JM                              
      END DO                                                                    
      READ (CUARSH(I1+7:I1+10),'(I4)') IY                                       
C                                                                               
      I1 = 179                                                                  
      I2 = I1 + 7                                                               
      READ (CUARSH(I1:I2),'(F8.3)') PMIN                                        
      I1 = 187                                                                  
      I2 = I1 + 7                                                               
      READ(CUARSH(I1:I2),'(F8.3)') PMAX                                         
      I1 = 201                                                                  
      I2 = I1 + 5                                                               
      READ (CUARSH(I1:I2),'(I6)') MNREC                                         
      I1 = 212                                                                  
      I2 = I1                                                                   
      READ (CUARSH(I1:I2),'(I1)') IQUAL                                         
      I1=213                                                                    
      I2=I1+79                                                                  
      DESCRP = CUARSH(I1:I2)                                                    
C                                                                               
C     Find actual record length and check against assumed maximum               
      I1=195                                                                    
      I2=I1+5                                                                   
      READ (CUARSH(I1:I2),'(I6)') I                                             
C     Remember actual record size and implied (max) field length                
      MRSIZE=I/4                                                                
      MDSIZE=MRSIZE-LHEAD                                                       
C     Check value                                                               
      IF(MRSIZE.GT.LRSIZE) THEN                                                 
        IERROR=304                                                              
        WRITE(MESAGE,221) MRSIZE,I,LRSIZE,IERROR                                
221     FORMAT('UKAOPN - Record length too large in',                           
     +  ' UKMO correlative data file;',                                         
     +  ' Record length (words, bytes) =',2I10,                                 
     +  '; Maximum allowed (words)      =',I10,                                 
     +  '; Error code =',I10)                                                   
        RETURN                                                                  
      END IF                                                                    
C                                                                               
CL*** 3       SET PARAMETERS IN COMPAR                                          
C                                                                               
      I1=293                                                                    
      DO 329 JP=1,NTYPEX                                                        
                                                                                
C     Set parameter type in COMPAR                                              
      I2=I1+11                                                                  
      IF(I2.GT.LUARSH) GO TO 330                                                
      MTYPE(JP)=CUARSH(I1:I2)                                                   
      IF(MTYPE(JP).EQ.'            ') THEN                                      
C       OMEGA is in file (though not header) between 26/8/92 & 11/11/92         
C       Check for only 4 parameters so far, as well as date range               
        IF ((JP.EQ.5) .AND. (IY.EQ.1992)) THEN                                  
          IF ((IM.EQ.8 .AND. ID.GE.26) .OR.                                     
     +        (IM.EQ.9 .OR. IM.EQ.10) .OR.                                      
     +        (IM.EQ.11 .AND. ID.LE.11) ) THEN                                  
            MTYPE(JP)='OMEGA'                                                   
          ELSE                                                                  
            GO TO 330                                                           
          END IF                                                                
        ELSE                                                                    
          GO TO 330                                                             
        END IF                                                                  
      END IF                                                                    
      IF (JP.LE.KNTYPE) THEN                                                    
        KTYPE(JP) = MTYPE(JP)                                                   
      ELSE                                                                      
        IERROR=-310                                                             
        WRITE(MESAGE,312) JP,IERROR                                             
312     FORMAT(' UKAOPN - WARNING insufficient field-type array size;',         
     +  ' no. field types = ',I10,';',                                          
     +  ' Non-fatal error code = ',I10)                                         
      END IF                                                                    
                                                                                
C     Set up field code in COMPAR                                               
      DO 320 J=1,NTYPED                                                         
      IF(MTYPE(JP).EQ.ITYPE(J)) THEN                                            
        MFC(JP)=IFC(J)                                                          
        GO TO 328                                                               
      END IF                                                                    
320   CONTINUE                                                                  
      IERROR=-311                                                               
      WRITE(MESAGE,323) MTYPE(JP),IERROR                                        
323   FORMAT(' UKAOPN - WARNING invalid field type found = ',A12,               
     +' no field code set;',                                                    
     +' Non-fatal error code = ',I10)                                           
      MFC(JP)=0                                                                 
                                                                                
328   CONTINUE                                                                  
      NTYPE = NTYPE + 1                                                         
      I1=I1+12                                                                  
C                                                                               
329   CONTINUE                                                                  
C                                                                               
330   CONTINUE                                                                  
C                                                                               
C     Set max, min pressures in COMPAR                                          
      PLEVMAX=PMAX                                                              
      PLEVMIN=PMIN                                                              
C                                                                               
C     If no errors found, display success message                               
      IF (IERROR.EQ.0) THEN                                                     
        WRITE(MESAGE,331)                                                       
331     FORMAT(' UKAOPN -',                                                     
     +   ' UKMO correlative data file successfully opened')                     
      END IF                                                                    
C                                                                               
      RETURN                                                                    
C                                                                               
CL*** 4        PP and FP format                                                 
400   CONTINUE                                                                  
C---- Set size parameters (in COMPAR) to values in parameter stmts              
      MRSIZE=LRSIZE                                                             
      MDSIZE=LDSIZE                                                             
C---- set parameters not obtained from headers                                  
      GTYPE='    '                                                              
      DESCRP='    '                                                             
      IQUAL=0                                                                   
C                                                                               
CL--- 4.1      If fast-open mode, set parameters to defaults                    
      IF (KLFAST) THEN                                                          
        PLEVMIN=0.316                                                           
        PLEVMAX=1000.0                                                          
        DATE='    '                                                             
        PMIN=PLEVMIN                                                            
        PMAX=PLEVMAX                                                            
        NTYPE=5                                                                 
        DO JP=1,NTYPE                                                           
          MTYPE(JP)=ITYPE(JP)                                                   
          MFC(JP)=IFC(JP)                                                       
          KTYPE(JP)=ITYPE(JP)                                                   
        END DO                                                                  
        IF (IERROR.EQ.0) THEN                                                   
          WRITE(MESAGE,419)                                                     
419       FORMAT(' UKAOPN -',                                                   
     +     ' PP/FP file opened, default parameters returned      ')             
        END IF                                                                  
        RETURN                                                                  
      END IF                                                                    
                                                                                
CL--- 4.2      Scan file: read a header record                                  
      IIH=1                                                                     
      IIF=IIH+LHEAD                                                             
420   CONTINUE                                                                  
      CALL UKARD(LUN, IIH, IIF, KFORM, IOERR, IERROR, MESAGE)                   
C---- check for error, I/O error or end-of-file                                 
      IF(IERROR.GT.0) THEN                                                      
        RETURN                                                                  
      ELSE IF(IOERR.LT.0) THEN                                                  
        GO TO 450                                                               
      ELSE IF(IOERR.GT.0) THEN                                                  
        IERROR=103                                                              
        WRITE(MESAGE,421) IOERR,IERROR                                          
421     FORMAT(' UKAOPN - Error scanning',                                      
     +  ' UKMO correlative data file;',                                         
     +  ' I/O status code =',I10,                                               
     +  '; Error code =',I10)                                                   
        RETURN                                                                  
      END IF                                                                    
                                                                                
CL--- 4.3      Set Pmax/min, date, etc.                                         
C---- first time - set date string, Pmax/Pmin & RMDI                            
      IF(NTYPE.EQ.0) THEN                                                       
        PLEVMIN=BLEV(IIH)                                                       
        PLEVMAX=BLEV(IIH)                                                       
        RMDI=BMDI(IIH)                                                          
        IM=LBMON(IIH)                                                           
        WRITE (DATE,433) LBDAT(IIH),MONTH3(IM),LBYR(IIH),                       
     +  LBHR(IIH)                                                               
433     FORMAT(I2.2,'-',A3,'-',I4.4,':',I2.2,':00:00.00')                       
      ELSE                                                                      
C---- otherwise just reset Pmax/Pmin                                            
        PLEVMIN=MIN(BLEV(IIH),PLEVMIN)                                          
        PLEVMAX=MAX(BLEV(IIH),PLEVMAX)                                          
      END IF                                                                    
                                                                                
CL--- 4.4      Set up type array (if necessary)                                 
C---- first, check whether this type is already in array                        
      IF(NTYPE.GT.0) THEN                                                       
        DO JP=1,NTYPE                                                           
          IF(LBFC(IIH).EQ.MFC(JP)) GO TO 420                                    
        END DO                                                                  
      END IF                                                                    
      IP=0                                                                      
      DO JP=1,NTYPED                                                            
        IF(LBFC(IIH).EQ.IFC(JP)) IP=JP                                          
      END DO                                                                    
      IF (IP.EQ.0) THEN                                                         
        IERROR=-312                                                             
        WRITE(MESAGE,441) LBFC(IIH),IERROR                                      
441     FORMAT(' UKAOPN - WARNING unknown field code found = ',I10,             
     +  ' no type set;',                                                        
     +  ' Non-fatal error code = ',I10)                                         
        GO TO 420                                                               
      END IF                                                                    
      NTYPE=NTYPE+1                                                             
      IF (NTYPE.GT.NTYPEX) THEN                                                 
        IERROR=305                                                              
        WRITE(MESAGE,443) NTYPE,IERROR                                          
443     FORMAT('UKAOPN - Too many field types in correlative data file;'        
     + ,' no. field types = ',I10,';',                                          
     +  ' Error code = ',I10)                                                   
        RETURN                                                                  
      END IF                                                                    
      MTYPE(NTYPE)=ITYPE(IP)                                                    
      MFC(NTYPE)=IFC(IP)                                                        
      IF (NTYPE.LE.KNTYPE) THEN                                                 
        KTYPE(NTYPE) = MTYPE(NTYPE)                                             
      ELSE                                                                      
        IERROR=-310                                                             
        WRITE(MESAGE,445) NTYPE,IERROR                                          
445     FORMAT(' UKAOPN - WARNING insufficient field-type array size;',         
     +  ' no. field types = ',I10,';',                                          
     +  ' Non-fatal error code = ',I10)                                         
      END IF                                                                    
      GO TO 420                                                                 
C                                                                               
CL--- 4.5      At end of file, rewind & finish setting parameters               
450   CONTINUE                                                                  
      CALL UKARWD (LUN, MFORM, IERROR, MESAGE)                                  
      PMAX=PLEVMAX                                                              
      PMIN=PLEVMIN                                                              
C                                                                               
      IF (IERROR.EQ.0) THEN                                                     
        WRITE(MESAGE,459)                                                       
459     FORMAT(' UKAOPN -',                                                     
     +    ' PP/FP format file successfully opened    ')                         
      END IF                                                                    
C                                                                               
      RETURN                                                                    
      END                                                                       
