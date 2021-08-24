      SUBROUTINE PV_PTH (PROUT, T_IN, F_P, P_LEVEL, P_P, P_TH,                  
     + NX, NY, NP, R_MDI, R_KAPPA, IERROR, MESAGE)                              
CL*** Subroutine to find the position of an output pressure level in            
CL*** terms of the input field levels, and to return theta at that              
CL*** level                                                                     
CL I    PROUT      Output pressure level                                        
CL I    T_IN       3-D input field of temperature                               
CL I    F_P        List of field pressure levels (in decreasing order)          
CL O    P_LEVEL    P surface in terms of field levels (a value of               
CL                 n.fff indicates it is between level n and n+1;               
CL                 0.fff is the vertical interpolation weight)                  
CL                 (for compatibility with PV_LTH, a whole field of             
CL                 identical values is returned).                               
CL O    P_P        Field of output pressure (for compatibility with             
CL                 PV_LTH; a whole field of identical values is                 
CL                 returned, set to R_MDI if outside input range).              
CL O    P_TH       Theta at the output pressure level (T is interpolated        
CL                 linearly with log(pressure)); set to missing                 
CL                 data (R_MDI) if outside range of input levels.               
CL I    NX, NY     Horizontal dimensions of field (columns, rows)               
CL I    NP         Number of field levels                                       
CL I    R_MDI      Missing data indicator                                       
CL I    R_KAPPA    Ratio of specific heats for air                              
CL O    IERROR     Error indicator                                              
CL O    MESAGE     Error message                                                
CL    When the output level is outside the range of F_P, P_LEVEL is set         
CL    to 1 or NP, as appropriate and P_TH to missing data (R_MDI)               
      IMPLICIT NONE                                                             
      INTEGER IMAXL                                                             
      PARAMETER (IMAXL=50)                                                      
C                                                                               
      INTEGER NX, NY, NP                                                        
      REAL PROUT, R_MDI                                                         
      REAL T_IN(NX*NY,NP), P_LEVEL(NX*NY), P_P(NX*NY), P_TH(NX*NY)              
      REAL F_P(NP)                                                              
      REAL R_KAPPA                                                              
      INTEGER IERROR                                                            
      CHARACTER*256 MESAGE                                                      
C                                                                               
      INTEGER J, JP                                                             
      INTEGER IILEV                                                             
      REAL ZCONV, ZLOGP(IMAXL)                                                  
      REAL ZLPU, ZLPL, ZLP, ZTU, ZTL, ZT                                        
      REAL ZFRACT, ZPLEV                                                        
      LOGICAL ILMISS                                                            
C                                                                               
CL*** 1       Calculate weighting and T-to-theta conversion                     
      IERROR=0                                                                  
C                                                                               
      DO JP=1,NP                                                                
        ZLOGP(JP)=LOG(F_P(JP))                                                  
      END DO                                                                    
C                                                                               
      ZCONV=(1000./PROUT)**R_KAPPA                                              
C                                                                               
      ILMISS = .TRUE.                                                           
      ZLP=LOG(PROUT)                                                            
      IF (PROUT.GT.F_P(1)) THEN                                                 
        ZPLEV=1.0                                                               
      ELSE IF (PROUT.LT.F_P(NP)) THEN                                           
        ZPLEV=REAL(NP)                                                          
      ELSE                                                                      
        DO JP=1,NP-1                                                            
          ZLPL=LOG(F_P(JP))                                                     
          ZLPU=LOG(F_P(JP+1))                                                   
          IF (ZLP.LE.ZLPL .AND. ZLP.GE.ZLPU) THEN                               
            IILEV=JP                                                            
            ZFRACT=(ZLP-ZLPL)/(ZLPU-ZLPL)                                       
            ZPLEV=REAL(IILEV)+ZFRACT                                            
            ILMISS=.FALSE.                                                      
          END IF                                                                
        END DO                                                                  
        IF (ILMISS) THEN                                                        
          IERROR=440                                                            
          WRITE(MESAGE,121) PROUT, F_P(NP), F_P(1)                              
121       FORMAT(' PV_PTH - P consistency failure',                             
     +      ' ; P reqd, top, bottom ', 3F10.4,                                  
     +      ' ; Error code = ',I10)                                             
          RETURN                                                                
        END IF                                                                  
      END IF                                                                    
C                                                                               
CL*** 2       Set up output fields P_LEVEL P_P and P_TH                         
C---- 2.1     Deal with out-of-range levels                                     
      IF (ILMISS) THEN                                                          
        DO J=1,NX*NY                                                            
          P_LEVEL(J)=ZPLEV                                                      
          P_P(J) =R_MDI                                                         
          P_TH(J)=R_MDI                                                         
        END DO                                                                  
      ELSE                                                                      
C                                                                               
C---- 2.2     Levels within range                                               
        DO J=1,NX*NY                                                            
          P_LEVEL(J)=ZPLEV                                                      
          P_P(J)=PROUT                                                          
          ZTL=T_IN (J,IILEV)                                                    
          ZTU=T_IN (J,IILEV+1)                                                  
          ZT=ZTL+ZFRACT*(ZTU-ZTL)                                               
          P_TH(J)=ZCONV*ZT                                                      
        END DO                                                                  
      END IF                                                                    
C                                                                               
      RETURN                                                                    
      END                                                                       
