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
