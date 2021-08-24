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
