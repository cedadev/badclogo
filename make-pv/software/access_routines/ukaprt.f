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
