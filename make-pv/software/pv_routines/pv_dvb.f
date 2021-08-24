      SUBROUTINE PV_DVB (U, V, AOUT, WORK, MODE, NX, NY, GRID,                  
     + RADIUS, DEG_TO_R, R_MDI, IERROR, MESAGE)                                 
CL    Calculate divergence or vorticity (AOUT) of a vector field (U,V)          
CL    on a staggered Arakawa B grid                                             
CL    Vorticity/divergence is calculated by dividing circulation around         
CL    (or outflow from) each grid box by the box area.                          
CL    The vector field is assumed to be on the wind grid, and the               
CL    output field on the mass grid points, as shown below                      
CL                                                                              
CL    o o o o o          o = output (mass) grid points                          
CL     i i i i i         i = input (wind) grid points                           
CL    o o o o o                                                                 
CL     i i i i i                                                                
CL    o o o o o                                                                 
CL                                                                              
CL I    U(NX,NY), V(NX,NY)    Input vector field                                
CL O    AOUT (NX,NY+1)        Output field                                      
CL O    WORK (NX,NY)          Work space                                        
CL I    MODE                  1 for divergence, 2 for vorticity                 
CL I    NX, NY                No of rows, points per row of input               
CL I    GRID(4)               1st lat, Dlat, 1st lon, Dlon input grid           
CL I    RADIUS                Radius of earth (m)                               
CL I    DEG_TO_R              Degrees to radians conversion factor              
CL I    R_MDI                 Missing data indicator                            
CL O    IERROR                Error indicated by non-zero value                 
CL O    MESAGE                Error message                                     
C                                                                               
C     For fields with E-W wrap-around, the first point of each output           
C     row is calculated from the end points of the input fields, other-         
C     wise it is set to R_MDI.                                                  
C     An extra row is produced for the output field; when the input             
C     fields start or end a half grid length from the pole, the extreme         
C     rows are set to polar values, otherwise they are set to R_MDI.            
C     (For non-wrap-around, the polar caps are incomplete, so are not           
C     set).                                                                     
C                                                                               
      IMPLICIT NONE                                                             
      INTEGER MODE, NX, NY                                                      
      REAL U(NX,NY), V(NX,NY), AOUT(NX, NY+1), WORK(NX, NY)                     
      REAL GRID(4)                                                              
      REAL RADIUS, DEG_TO_R, R_MDI                                              
      INTEGER IERROR                                                            
      CHARACTER*256 MESAGE                                                      
C                                                                               
      LOGICAL WRAP, N_POLE, S_POLE                                              
      REAL Z, ZSS, ZSN, ZRA                                                     
      REAL SLAT, DLAT, SLON, DLON                                               
      INTEGER JX, JY                                                            
C                                                                               
      EXTERNAL PV_DX, PV_DY                                                     
C                                                                               
CL    1    Preliminaries                                                        
      IERROR=0                                                                  
      SLAT=GRID(1)                                                              
      DLAT=GRID(2)                                                              
      SLON=GRID(3)                                                              
      DLON=GRID(4)                                                              
C     Test for wrap-around                                                      
      Z=NX*DLON                                                                 
      WRAP=ABS(360.-ABS(Z)).LT.0.1                                              
C     Test for Pole at start of grid                                            
      Z=SLAT-0.5*DLAT                                                           
      N_POLE=(ABS(90.-ABS(Z)).LT.0.1).AND.WRAP                                  
C     Test for Pole at end of grid                                              
      Z=SLAT+(NY-0.5)*DLAT                                                      
      S_POLE=(ABS(90.-ABS(Z)).LT.0.1).AND.WRAP                                  
C                                                                               
CL    2    Do differentation for divergence                                     
      IF(MODE.EQ.1) THEN                                                        
        CALL PV_DX(U, WORK, NX, NY, GRID, DEG_TO_R, R_MDI)                      
        CALL PV_DY(V, AOUT, NX, NY, GRID, DEG_TO_R, R_MDI)                      
        DO JY=2,NY                                                              
          DO JX=1,NX                                                            
            IF(AOUT(JX,JY).EQ.R_MDI.OR.WORK(JX,JY-1).EQ.R_MDI) THEN             
              AOUT(JX,JY)=R_MDI                                                 
            ELSE                                                                
              AOUT(JX,JY)=AOUT(JX,JY)+WORK(JX,JY-1)                             
            END IF                                                              
          END DO                                                                
        END DO                                                                  
C                                                                               
CL    3    Do differentation for vorticity  dv/dx - du/dy                       
      ELSE IF(MODE.EQ.2) THEN                                                   
        CALL PV_DX(V, WORK, NX, NY, GRID, DEG_TO_R, R_MDI)                      
        CALL PV_DY(U, AOUT, NX, NY, GRID, DEG_TO_R, R_MDI)                      
*IF TEST                                                                        
*END TEST                                                    *****   6          
        DO JY=1,NY+1                                                            
          IF(JY.GT.1.AND.JY.LT.NY+1) THEN                                       
            DO JX=1,NX                                                          
              IF(AOUT(JX,JY).EQ.R_MDI.OR.WORK(JX,JY-1).EQ.R_MDI) THEN           
                AOUT(JX,JY)=R_MDI                                               
              ELSE                                                              
                AOUT(JX,JY)=WORK(JX,JY-1)-AOUT(JX,JY)                           
              END IF                                                            
            END DO                                                              
          ELSE                                                                  
            DO JX=1,NX                                                          
              IF(AOUT(JX,JY).NE.R_MDI) THEN                                     
                AOUT(JX,JY)=-AOUT(JX,JY)                                        
              END IF                                                            
            END DO                                                              
          END IF                                                                
        END DO                                                                  
C                                                                               
      ELSE                                                                      
        IERROR=430                                                              
        WRITE(MESAGE,391) MODE, IERROR                                          
391     FORMAT(' PV_DVB - Invalid MODE ',I10,' ;',                              
     +  ' Error code = ',I10)                                                   
        RETURN                                                                  
      END IF                                                                    
C                                                                               
CL    4     Divide by area of grid boxes                                        
C (only 1 factor of RADIUS, because PV_DX, PV_DY assume a unit sphere)          
      Z=SLAT-DLAT                                                               
      IF(ABS(Z).GT.90.0) Z=SIGN(90.0,Z)                                         
      ZSN=SIN(DEG_TO_R*Z)                                                       
      DO JY=1,NY+1                                                              
        Z=SLAT+(JY-1)*DLAT                                                      
        IF(ABS(Z).GT.90.0) Z=SIGN(90.0,Z)                                       
        ZSS=SIN(DEG_TO_R*Z)                                                     
        ZRA=1.0/ABS(RADIUS*DLON*DEG_TO_R*(ZSS-ZSN))                             
        DO JX=1,NX                                                              
          IF(AOUT(JX,JY).NE.R_MDI) THEN                                         
            AOUT(JX,JY)=ZRA*AOUT(JX,JY)                                         
          END IF                                                                
        END DO                                                                  
        ZSN=ZSS                                                                 
      END DO                                                                    
C                                                                               
CL    5     Reset to missing data if non-wrap-around / non-polar                
      IF(.NOT.WRAP) THEN                                                        
        DO JY=1,NY+1                                                            
          AOUT(1,JY)=R_MDI                                                      
        END DO                                                                  
      END IF                                                                    
      IF(.NOT.N_POLE) THEN                                                      
        DO JX=1,NX                                                              
          AOUT(JX,1)=R_MDI                                                      
        END DO                                                                  
      END IF                                                                    
      IF(.NOT.S_POLE) THEN                                                      
        DO JX=1,NX                                                              
          AOUT(JX,NY+1)=R_MDI                                                   
        END DO                                                                  
      END IF                                                                    
*IF TEST                                                                        
*END TEST                                                    *****   4          
C                                                                               
      RETURN                                                                    
      END                                                                       
