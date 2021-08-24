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
      SUBROUTINE UKAPRF(LUN,KTYPE,PLEVELS,KNL,KNLD,PLATS,PLONS,KNR,             
     + PROFS,KTIME,PMDI,IERROR,MESAGE)                                          
CL======================================================================        
CL    SUBROUTINE UKAPRF(LUN,KTYPE,PLEVELS,KNL,KNLD,PLATS,PLONS,KNR,             
CL   + PROFS,KTIME,PMDI,IERROR,MESAGE)                                          
C                                                                               
CL    Obtains a set of profiles from UKMO assimilation data                     
C                                                                               
CL    The user specifies the points and pressure levels at which                
CL    profiles are required.  Information on the time of the data is            
CL    also returned.                                                            
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
C                                                                               
CL Arguments:                                                                   
CL                                                                              
CL    LUN          IN      Unit Number                                          
CL    KTYPE        IN      Requested field type                                 
CL    PLEVELS(KNLD)IN      Requested pressure levels (mb) (in order)            
CL    KNL          IN      Number of pressure levels required                   
CL    KNLD         IN      Number of levels used in array dimensions            
CL    PLATS(KNR)   IN      Latitudes (deg.N) of required points                 
CL    PLONS(KNR)   IN      Longitudes (deg.E) of required points                
CL    KNR          IN      Number of points                                     
CL    PROFS(KNLD,KNR) OUT  Profiles derived from data                           
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
      INTEGER LUN, KNL, KNLD, KNR, KTIME, IERROR                                
      REAL PLEVELS, PLATS, PLONS, PROFS, PMDI                                   
      CHARACTER*(*) KTYPE                                                       
      CHARACTER*256 MESAGE                                                      
      DIMENSION PLEVELS(KNLD), PLATS(KNR), PLONS(KNR),                          
     + PROFS(KNLD,KNR), KTIME(4)                                                
C                                                                               
C Common blocks: (none)                                                         
C                                                                               
C Local variables:                                                              
      INTEGER IFC, IFLD, I1, I2, II, JL, J                                      
      REAL UKAVAL                                                               
C     IFC          PP field code                                                
C     IFLD         Field number in buffer                                       
C                                                                               
C-----------------------------------------------------------------------        
C                                                                               
C     Set zero error code initially                                             
C                                                                               
      IERROR = 0                                                                
C                                                                               
C     Initialise MESAGE                                                         
C                                                                               
      MESAGE = ' '                                                              
C                                                                               
CL*** 1       USE UKACHK TO VERIFY REQUESTED PARAMETERS                         
C                                                                               
      CALL UKACHK(KTYPE,IFC,PLEVELS(1),PLEVELS(KNL),IERROR,MESAGE)              
      IF(IERROR.GT.0) RETURN                                                    
C                                                                               
CL*** 2       DERIVE PROFILES                                                   
C                                                                               
C---- Start loop over levels (highest pressure first)                           
      IF(PLEVELS(1).GT.PLEVELS(KNL)) THEN                                       
        I1=1                                                                    
        I2=KNL                                                                  
        II=1                                                                    
      ELSE                                                                      
        I1=KNL                                                                  
        I2=1                                                                    
        II=-1                                                                   
      END IF                                                                    
C                                                                               
      DO 290 JL=I1,I2,II                                                        
C                                                                               
C---- Check pressures are in order                                              
      IF(JL.NE.I1) THEN                                                         
        IF(PLEVELS(JL).GT.PLEVELS(JL-II)) THEN                                  
          IERROR=221                                                            
          WRITE(MESAGE,221) IERROR, PLEVELS                                     
221       FORMAT(' UKAPRF - Requested levels out of order;',                    
     +    ' Error code =',I10,                                                  
     +    ' ; Levels:',(T16,1P,5E12.3,';'))                                     
          RETURN                                                                
        END IF                                                                  
      END IF                                                                    
C                                                                               
C---- Use UKAGET to obtain field                                                
      CALL UKAGET(LUN,IFC,PLEVELS(JL),IFLD,KTIME,PMDI,IERROR,MESAGE)            
      IF(IERROR.GT.0) RETURN                                                    
C                                                                               
C---- Get values at requested points                                            
      DO 230 J=1,KNR                                                            
      PROFS(JL,J)=UKAVAL(IFLD,PLATS(J),PLONS(J))                                
230   CONTINUE                                                                  
C                                                                               
290   CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
C                                                                               
      END                                                                       
      SUBROUTINE UKACLS(LUN,IERROR,MESAGE)                                      
CL======================================================================        
CL    SUBROUTINE UKACLS(LUN,IERROR,MESAGE)                                      
C                                                                               
CL    Closes the specified UKMO correlative data file                           
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
C                                                                               
CL Arguments:                                                                   
CL                                                                              
CL     LUN          IN      Unit Number                                         
CL     IERROR       OUT     Non-zero indicates error                            
CL     MESAGE       OUT     Message indicating success or failure               
C                                                                               
      INTEGER LUN, IERROR                                                       
      CHARACTER*256 MESAGE                                                      
C                                                                               
C Local variables:                                                              
      INTEGER IOERR                                                             
C     IOERR        I/O status                                                   
C-----------------------------------------------------------------------        
C                                                                               
CL*** 1       CLOSE FILE                                                        
C                                                                               
C     Set zero error code initially                                             
      IERROR = 0                                                                
C                                                                               
C     Initialise MESAGE                                                         
      MESAGE = ' '                                                              
C                                                                               
      CLOSE (UNIT=LUN,STATUS='KEEP',                                            
     + IOSTAT=IOERR)                                                            
      IF(IOERR.NE.0) THEN                                                       
        IERROR=121                                                              
        WRITE(MESAGE,101) IOERR,IERROR                                          
101     FORMAT(' UKACLS - Error closing UKMO correlative data file;',           
     +  ' I/O status code =',I10,                                               
     +  ' ; Error code =',I10)                                                  
        RETURN                                                                  
      END IF                                                                    
C                                                                               
      WRITE(MESAGE,110)                                                         
110   FORMAT(' UKACLS -',                                                       
     + ' UKMO correlative data file successfully closed')                       
C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE UKARWD(LUN, KFORM, IERROR, MESAGE)                             
CL======================================================================        
CL    SUBROUTINE UKARWD(LUN, KFORM, IERROR, MESAGE)                             
C                                                                               
CL    Rewinds the specified UKMO correlative data file and                      
CL    skips the SFDU and UARS header record so that the file                    
CL    is then positioned at the beginning of the first                          
CL    assimilation record.                                                      
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
C                                                                               
CL Arguments:                                                                   
CL                                                                              
CL     LUN          IN      Unit Number                                         
CL     KFORM        IN      Format (1=CDHF, 2=pp, 3=FP)                         
CL     IERROR       OUT     Non-zero indicates error                            
CL     MESAGE       OUT     Error message                                       
C                                                                               
      INTEGER LUN, KFORM, IERROR                                                
      CHARACTER*256 MESAGE                                                      
C                                                                               
C Common blocks:                                                                
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
      INTEGER IOERR                                                             
C     IOERR        I/O status                                                   
C-----------------------------------------------------------------------        
C                                                                               
CL*** 1       REWIND FILE AND SKIP SFDU AND UARS HEADER RECORD                  
C                                                                               
C     If direct-access, just set pointer so that first data record              
C     (not SFDU/UARS header) is read next.  Then return.                        
      IF (KFORM.EQ.4) THEN                                                      
        MCREC=2                                                                 
        RETURN                                                                  
      END IF                                                                    
C                                                                               
C  Rewind file                                                                  
C                                                                               
      REWIND (UNIT=LUN,IOSTAT=IOERR)                                            
      IF(IOERR.NE.0) THEN                                                       
        IERROR=112                                                              
        WRITE(MESAGE,211) IOERR,IERROR                                          
211     FORMAT(' UKARWD - Error while rewinding',                               
     +        ' UKMO correlative data file;',                                   
     +        ' I/O status code =',I10,                                         
     +        ' ; Error code =',I10)                                            
        RETURN                                                                  
      END IF                                                                    
C                                                                               
C  If format=1 skip SFDU and UARS header record                                 
C                                                                               
      IF (KFORM.EQ.1) THEN                                                      
        READ (UNIT=LUN,IOSTAT=IOERR)                                            
C                                                                               
        IF(IOERR.NE.0) THEN                                                     
          IERROR=113                                                            
          WRITE(MESAGE,212) IOERR,IERROR                                        
212       FORMAT(' UKARWD - Error skipping header from',                        
     +        ' UKMO correlative data file;',                                   
     +        ' I/O status code =',I10,                                         
     +        ' ; Error code =',I10)                                            
                                                                                
          RETURN                                                                
        END IF                                                                  
      END IF                                                                    
C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE UKACHK(KTYPE,KFC,PLEV1,PLEV2,IERROR,MESAGE)                    
CL======================================================================        
CL    SUBROUTINE UKACHK(KTYPE,KFC,PLEV1,PLEV2,IERROR,MESAGE)                    
C                                                                               
CL    Checks requested field type and level for UKMO correlative data           
CL    N.B. Only the first 4 characters of the requested field type              
CL    are checked against the field types available in the current              
CL    data file.                                                                
C                                                                               
CL    An Error results if the requested field is not available in the           
CL    current data file.                                                        
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
C                                                                               
CL Arguments:                                                                   
CL                                                                              
CL     KTYPE   IN      Requested parameter                                      
CL     KFC     OUT     PP field code of requested parameter                     
CL     PLEV1   IN      ) Requested range of pressure levels (mb)                
CL     PLEV2   IN      )                                                        
CL     IERROR  OUT     Non-zero indicates error                                 
CL     MESAGE  OUT     Error message                                            
C                                                                               
      CHARACTER*(*) KTYPE                                                       
      CHARACTER*256 MESAGE                                                      
      INTEGER KFC, IERROR                                                       
      REAL PLEV1, PLEV2                                                         
C                                                                               
C Common blocks:                                                                
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
      CHARACTER*4 ITYPE                                                         
C     ITYPE         First 4 characters of KTYPE                                 
      INTEGER J                                                                 
C     J             Loop counter                                                
      REAL ZX, ZN                                                               
C     ZX, ZN        Max, min pressures                                          
C-----------------------------------------------------------------------        
C                                                                               
CL*** 1       CHECK PARAMETERS                                                  
C                                                                               
C---- Check levels                                                              
      ZX=MAX(PLEV1,PLEV2)                                                       
      ZN=MIN(PLEV1,PLEV2)                                                       
      IF(ZX.GT.PLEVMAX) THEN                                                    
        IERROR=-202                                                             
        WRITE(MESAGE,111) ZX,IERROR                                             
111     FORMAT (' UKACHK - Requested pressure too large;',                      
     +  ' Pressure = ',1P,E12.3,                                                
     +  ' ; Non-fatal error code = ',I10)                                       
      ELSE IF(ZN.LT.PLEVMIN) THEN                                               
        IERROR=-203                                                             
        WRITE(MESAGE,112) ZN,IERROR                                             
112     FORMAT (' UKACHK - Requested pressure too small;',                      
     +  ' Pressure = ',1P,E12.3,                                                
     +  ' ; Non-fatal error code = ',I10)                                       
      END IF                                                                    
C                                                                               
C---- Check field type is in file (only check 1st 4 characters)                 
      ITYPE=KTYPE                                                               
      DO 120 J=1,NTYPE                                                          
        IF (ITYPE.EQ.MTYPE(J)(1:4)) THEN                                        
          KFC=MFC(J)                                                            
          GO TO 130                                                             
        END IF                                                                  
120   CONTINUE                                                                  
      IERROR=204                                                                
      WRITE(MESAGE,121) KTYPE,IERROR                                            
121   FORMAT (' UKACHK - Requested field type not in file;',                    
     +' Parameter = ',A12,                                                      
     +' ; Error code = ',I10)                                                   
C                                                                               
130   CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      FUNCTION UKAVAL(KFLD,PLAT,PLON)                                           
CL======================================================================        
CL    FUNCTION UKAVAL(KFLD,PLAT,PLON)                                           
CL    Interpolate one value in field at given lat, long                         
C                                                                               
CL    NB. This version assumes a lat-long grid with E-W wrap-around             
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
C                                                                               
CL Arguments:                                                                   
CL                                                                              
CL    KFLD    IN      I    Field number                                         
CL    PLAT    IN      R    Latitude (degrees N)                                 
CL    PLON    IN      R    Longitude (degrees E)                                
C                                                                               
      INTEGER KFLD                                                              
      REAL PLAT, PLON                                                           
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
C Local variables                                                               
      INTEGER IH, IROW, INPT, IR, IP, IP1, I                                    
C     IH              Address of header in buffer                               
C     IROW/INPT       No. points / points per row                               
C     IR, IP, IP1     Row, point numbers                                        
      REAL ZLAT, ZR, ZP, UKAVAL                                                 
C     ZLAT            Latitude                                                  
C     ZR, ZP          (fractional) row, point numbers                           
C     UKAVAL          Interpolation function                                    
C-----------------------------------------------------------------------        
C                                                                               
CL*** 1       LINEAR INTERPOLATION IN LAT-LONG GRID                             
      IH=NPBGN(KFLD)                                                            
      IROW=LBROW(IH)                                                            
      INPT=LBNPT(IH)                                                            
C                                                                               
CL--- 1.1     FIND ROW                                                          
      ZLAT=PLAT                                                                 
      ZR=(ZLAT-BZY(IH))/BDY(IH)                                                 
      IF(ZR.LT.0.) GO TO 8                                                      
      IR=IFIX(ZR)                                                               
      ZR=ZR-IR                                                                  
C     IF MORE THAN 1 ROW OUTSIDE LIMITS RETURN MDI                              
C     IF WITHIN 1 ROW OF LIMITS USE EXTREME ROW                                 
      IF(IR)8,111,112                                                           
 111  IR=1                                                                      
      ZR=0.                                                                     
 112  IF(IR-IROW)114,113,8                                                      
 113  ZR=0.                                                                     
 114  CONTINUE                                                                  
C     NOW USE ROWS IR & IR+1 WITH WEIGHTS 1-ZR & ZR                             
C                                                                               
CL--- 1.2     N-S AND E-W INTERPOLATION FROM LAT-LONG                           
      ZP=AMOD(PLON-BZX(IH)+SIGN(360.,BDX(IH)),360.)/BDX(IH)                     
      IP=ZP                                                                     
      ZP=ZP-IP                                                                  
      IP1=IP+1                                                                  
      IF(IP.EQ.INPT) THEN                                                       
        IP1=1                                                                   
      ELSE IF(IP.EQ.0) THEN                                                     
        IP=INPT                                                                 
      END IF                                                                    
      IF(ZR.LT. .01) GO TO 141                                                  
      IF(ZR.GT. .99) GO TO 140                                                  
      I=NPFD(KFLD)+INPT*(IR-1) - 1                                              
      IF(   B(I+IP).EQ.RMDI .OR.      B(I+IP1).EQ.RMDI .OR.                     
     1 B(I+INPT+IP).EQ.RMDI .OR. B(I+INPT+IP1).EQ.RMDI) GO TO 8                 
      UKAVAL=(1.-ZR)*((1.-ZP)*B(I+IP)+ZP*B(I+IP1)) +                            
     1 ZR*((1.-ZP)*B(I+INPT+IP)+ZP*B(I+INPT+IP1))                               
      RETURN                                                                    
C                                                                               
CL--- 1.4     E-W INTERPOLATION ONLY WHEN POINT LIES ON ROW                     
 140  IR=IR+1                                                                   
 141  CONTINUE                                                                  
      I=NPFD(KFLD)+INPT*(IR-1) - 1                                              
      IF(   B(I+IP).EQ.RMDI .OR.      B(I+IP1).EQ.RMDI) GO TO 8                 
      UKAVAL=(1.-ZP)*B(I+IP)+ZP*B(I+IP1)                                        
      RETURN                                                                    
C                                                                               
CL*** 8       OUTSIDE AREA - returns missing data indicator RMDI                
 8    UKAVAL=RMDI                                                               
      RETURN                                                                    
      END                                                                       
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
      SUBROUTINE UKAPRT (MESAGE, LUN_PRT, IWIDTH)                               
CL======================================================================        
CL    SUBROUTINE UKAPRT (MESAGE, LUN_PRT, IWIDTH)                               
C                                                                               
CL    Print the message produced by UKA access routines                         
CL    The message is chopped up (at semi-colons) into lines of                  
CL    up to IWIDTH characters                                                   
CL    If Message, Unit or Width is invalid, Message is not printed.             
C                                                                               
C-----------------------------------------------------------------------        
      IMPLICIT NONE                                                             
C                                                                               
CL    Arguments:                                                                
CL                                                                              
CL I    MESAGE                Error message                                     
CL I    LUN_PRT               Unit number for printed messages                  
CL                            (UKAPRT does not print anything if                
CL                            LUN_PRT is less than or equal to zero)            
CL I    IWIDTH                Width of printed message (includes                
CL                            control character in column 1); max 133           
C                                                                               
      INTEGER LUN_PRT, IWIDTH                                                   
      CHARACTER*(*) MESAGE                                                      
C                                                                               
C     Local variables                                                           
      INTEGER ISTART, IEND, INEXT, IM, IW, II, IWMAX, JM                        
      PARAMETER (IWMAX=132)                                                     
C                                                                               
C     Return if unit number is invalid (<0)                                     
      IF(LUN_PRT.LE.0) RETURN                                                   
                                                                                
C---- message is chopped up at semi-colons, into lines up to IW chars           
      IW=IWIDTH-1                                                               
      IW=MIN(IW,IWMAX)                                                          
      ISTART=1                                                                  
C---- Length of message = length of variable, less any trailing blanks          
      II=LEN(MESAGE)                                                            
      DO JM=II,1,-1                                                             
        IF (MESAGE(JM:JM).NE.' ') THEN                                          
          IM=JM                                                                 
          GO TO 110                                                             
        END IF                                                                  
      END DO                                                                    
      IM=1                                                                      
110   CONTINUE                                                                  
C                                                                               
C     Return if message length <1  (or >256!)                                   
      IF (IM.LT.1 .OR. IM.GT.256) RETURN                                        
C                                                                               
C     Return if width <=1                                                       
      IF (IW.LT.1) RETURN                                                       
C                                                                               
210   CONTINUE                                                                  
C     ISTART is first character to be printed                                   
      II=INDEX(MESAGE(ISTART:),';')                                             
C     IEND is end of character substring                                        
C         (end of message or one before ';')                                    
C     INEXT is start of next substring                                          
      IF (II.LE.0) THEN                                                         
        IEND=IM                                                                 
        INEXT=IEND+1                                                            
      ELSE                                                                      
        IEND=ISTART+II-2                                                        
        INEXT=IEND+2                                                            
      END IF                                                                    
C     adjust IEND if more than IW characters are to be printed                  
      IF (IEND-ISTART .GT. IW-1) THEN                                           
        IEND=ISTART+IW-1                                                        
        INEXT=IEND+1                                                            
      END IF                                                                    
C                                                                               
      WRITE(LUN_PRT,'(1X,A)') MESAGE(ISTART:IEND)                               
C     If more message remains, go round again                                   
      IF(INEXT.LE.IM) THEN                                                      
        ISTART=INEXT                                                            
        GO TO 210                                                               
      END IF                                                                    
C                                                                               
      RETURN                                                                    
      END                                                                       
