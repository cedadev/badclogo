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
