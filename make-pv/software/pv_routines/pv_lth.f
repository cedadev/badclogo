      SUBROUTINE PV_LTH (THETA, T_IN, F_P, TH_LEVEL, TH_P,                      
     + NX, NY, NP, R_MDI, R_KAPPA)                                              
CL*** Subroutine to find the position of a theta surface in terms of            
CL*** the input field levels                                                    
CL I    THETA      Potential temperature (theta) to be located                  
CL I    T_IN       3-D input field of temperature                               
CL I    F_P        List of field pressure levels (in decreasing order)          
CL O    TH_LEVEL   Theta surface in terms of field levels (a value of           
CL                 n.fff indicates it is between level n and n+1;               
CL                 0.fff is the vertical interpolation weight)                  
CL O    TH_P       Theta surface in terms of pressure, assuming Theta           
CL                 varies linearly with log(pressure); set to missing           
CL                 data (R_MDI) if outside range of input levels.               
CL I    NX, NY     Horizontal dimensions of field (columns, rows)               
CL I    NP         Number of field levels                                       
CL I    R_MDI      Missing data indicator                                       
CL I    R_KAPPA    Ratio of specific heats for air                              
CL    This takes the 3-D temperature array T_IN (on pressure levels             
CL    given in array F_P) and determines the level of the required              
CL    isentropic surface at each grid point.                                    
CL    When the isentropic surface is outside the range of F_P, TH_LEVEL         
CL    is set to 1 or NP, as appropriate and TH_P to missing data (R_MDI)        
      IMPLICIT NONE                                                             
      INTEGER IMAXL                                                             
      PARAMETER (IMAXL=50)                                                      
C                                                                               
      INTEGER NX, NY, NP                                                        
      REAL THETA, R_MDI                                                         
      REAL T_IN(NX*NY,NP), TH_LEVEL(NX*NY), TH_P(NX*NY)                         
      REAL F_P(NP)                                                              
      REAL R_KAPPA                                                              
C                                                                               
      INTEGER J, JP                                                             
      REAL ZCONV(IMAXL), ZLOGP(IMAXL)                                           
      REAL Z, ZNL, ZJL                                                          
      REAL ZLPU, ZLPL, ZTHU, ZTHL                                               
C                                                                               
CL*** 0       Set up T to theta conversion factors                              
      DO JP=1,NP                                                                
        ZCONV(JP)=(1000.0/F_P(JP))**R_KAPPA                                     
        ZLOGP(JP)=LOG(F_P(JP))                                                  
      END DO                                                                    
C                                                                               
CL*** 1       Process field at lowest input level                               
      DO 390 J=1,NX*NY                                                          
C---- 1.1     Convert temperature to potential temperature                      
        ZTHU=ZCONV(1)*T_IN(J,1)                                                 
C---- 1.2     Set up TH_LEVEL and TH_P                                          
C---- Where required Theta < theta at lowest level, set TH_LEVEL to 1.0         
C---- and TH_P to missing data                                                  
        IF(THETA.LT.ZTHU) THEN                                                  
          TH_LEVEL(J)=1.0                                                       
          TH_P(J)=R_MDI                                                         
        END IF                                                                  
C                                                                               
CL*** 2       Process further levels                                            
C---- 2.1     Start loop over levels                                            
C---- Within loop, we are processing P between F_P(JP-1) and F_P(JP)            
        DO 290 JP=2,NP                                                          
          ZJL=REAL(JP-1)                                                        
C---- 2.2     Theta and log(p) at lower and upper levels                        
          ZLPL=ZLOGP(JP-1)                                                      
          ZLPU=ZLOGP(JP)                                                        
          ZTHL=ZTHU                                                             
          ZTHU=ZCONV(JP)*T_IN(J,JP)                                             
C---- 2.3     Set up TH_LEVEL and TH_P                                          
C---- Set when reqd Theta > theta below and < theta above                       
C---- (at this stage TH_P is set to log(p))                                     
          IF(THETA.GE.ZTHL.AND.THETA.LE.ZTHU) THEN                              
            Z=(THETA-ZTHL)/(ZTHU-ZTHL)                                          
            TH_LEVEL(J)=ZJL+Z                                                   
            TH_P(J)=ZLPL+Z*(ZLPU-ZLPL)                                          
          END IF                                                                
290     CONTINUE                                                                
C                                                                               
CL*** 3       Finish off                                                        
C---- 3.1     Set up TH_LEVEL for Theta>highest level                           
C---- Where required Theta > theta at highest level, TH_LEVEL = NP              
        ZNL=REAL(NP)                                                            
        IF(THETA.GT.ZTHU) THEN                                                  
          TH_LEVEL(J)=ZNL                                                       
          TH_P(J)=R_MDI                                                         
        END IF                                                                  
C---- 3.2     Convert TH_P from log(pressure) to pressure                       
        IF(TH_P(J).NE.R_MDI) THEN                                               
          TH_P(J)=EXP(TH_P(J))                                                  
        ENDIF                                                                   
390   CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
