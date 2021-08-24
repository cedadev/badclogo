C                                                                               
      SUBROUTINE PV_DY(AIN, AOUT, NX, NY, GRID, DEG_TO_R, R_MDI)                
CL    Routine to do y-differentiation for vorticity/divergence.                 
C     It calculates the difference between the line integral of AIN             
C     along the northern edge of each grid box, and the line integral           
C     along the southern edge (on a unit sphere).                               
C     The input field is assumed to be on the wind points of an                 
C     Arakawa B grid, and the output field on the mass grid points.             
CL I    AIN                   Input array                                       
CL O    AOUT                  Output array                                      
CL I    NX, NY                No of rows, points per row of input               
CL I    GRID(4)               1st lat, Dlat, 1st lon, Dlon input grid           
CL I    DEG_TO_R              Degrees to radians conversion factor              
CL I    R_MDI                 Missing data indicator                            
C NB. - The n'th row of AOUT is derived from the n'th and n-1'th rows           
C     of AIN; the extreme rows 1 and NY+1 are set to polar values if            
C     output grid latitudes are at pole, but otherwise extreme rows are         
C     set to R_MDI                                                              
C     - 1st point in each row is set assuming wrap-around; ignore if            
C     non-wrap-around grid!                                                     
C                                                                               
      IMPLICIT NONE                                                             
      INTEGER NX, NY                                                            
      REAL AIN(NX, NY), AOUT(NX, NY+1)                                          
      REAL GRID(4)                                                              
      REAL R_MDI, DEG_TO_R                                                      
C                                                                               
      INTEGER JX, JY                                                            
      REAL ZG,Z,ZI                                                              
      LOGICAL ILMISS                                                            
C                                                                               
CL    1      Do differentiation for body of grid                                
      DO 120 JY=1,NY                                                            
      Z=DEG_TO_R*(GRID(1)+(JY-1)*GRID(2))                                       
      ZG=SIGN(1.0,GRID(2))*ABS(0.5*DEG_TO_R*GRID(4)*COS(Z))                     
C                                                                               
C     Average input in E-W, multiply by grid-length                             
C     Store in AOUT                                                             
      IF(AIN( 1,JY).EQ.R_MDI.OR.AIN(NX,JY).EQ.R_MDI) THEN                       
        AOUT( 1,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT( 1,JY)=ZG*(AIN( 1,JY)+AIN(NX,JY))                                  
      END IF                                                                    
      DO 110 JX=2,NX                                                            
      IF(AIN(JX,JY).EQ.R_MDI.OR.AIN(JX-1,JY).EQ.R_MDI) THEN                     
        AOUT(JX,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(JX,JY)=ZG*(AIN(JX,JY)+AIN(JX-1,JY))                                
      END IF                                                                    
110   CONTINUE                                                                  
120   CONTINUE                                                                  
C                                                                               
C     Difference in N-S                                                         
C     (note reverse order in JY, to avoid over-writing problem)                 
      DO 140 JY=NY,2,-1                                                         
      DO 130 JX=1,NX                                                            
      IF(AOUT(JX,JY).EQ.R_MDI.OR.AOUT(JX,JY-1).EQ.R_MDI) THEN                   
        AOUT(JX,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(JX,JY)=(AOUT(JX,JY)-AOUT(JX,JY-1))                                 
      END IF                                                                    
130   CONTINUE                                                                  
140   CONTINUE                                                                  
C                                                                               
CL    2      Deal with polar caps                                               
C     Integrate around polar cap, but divide by NX to allow for grid            
C     boxes (wedges) 1 grid length wide (rather than complete cap)              
      ZG=ABS(DEG_TO_R*GRID(4)*SIN(0.5*GRID(2)*DEG_TO_R))                        
C                                                                               
C     Pole at start of grid                                                     
      Z=GRID(1)-0.5*GRID(2)                                                     
      IF(ABS(90.0-ABS(Z)).LT.0.1) THEN                                          
        ZI=0.0                                                                  
        ILMISS=.FALSE.                                                          
        DO 212 JX=1,NX                                                          
        ZI=ZI-AIN(JX,1)                                                         
        IF(AIN(JX,1).EQ.R_MDI) ILMISS=.TRUE.                                    
212     CONTINUE                                                                
        ZI=ZI*ZG/REAL(NX)*SIGN(1.0,GRID(2))                                     
        IF(ILMISS) ZI=R_MDI                                                     
        DO 214 JX=1,NX                                                          
        AOUT(JX,1)=ZI                                                           
214     CONTINUE                                                                
      ELSE                                                                      
        DO 216 JX=1,NX                                                          
        AOUT(JX,1)=R_MDI                                                        
216     CONTINUE                                                                
      END IF                                                                    
C                                                                               
C     Pole at end of grid                                                       
      Z=GRID(1)+(NY-0.5)*GRID(2)                                                
      IF(ABS(90.0-ABS(Z)).LT.0.1) THEN                                          
        ZI=0.0                                                                  
        ILMISS=.FALSE.                                                          
        DO 222 JX=1,NX                                                          
        ZI=ZI+AIN(JX,NY)                                                        
        IF(AIN(JX,NY).EQ.R_MDI) ILMISS=.TRUE.                                   
222     CONTINUE                                                                
        ZI=ZI*ZG/REAL(NX)*SIGN(1.0,GRID(2))                                     
        IF(ILMISS) ZI=R_MDI                                                     
        DO 224 JX=1,NX                                                          
        AOUT(JX,NY+1)=ZI                                                        
224     CONTINUE                                                                
      ELSE                                                                      
        DO 226 JX=1,NX                                                          
        AOUT(JX,NY+1)=R_MDI                                                     
226     CONTINUE                                                                
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
