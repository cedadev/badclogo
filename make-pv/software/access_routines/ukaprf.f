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
