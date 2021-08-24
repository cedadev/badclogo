      SUBROUTINE PV_DX(AIN, AOUT, NX, NY, GRID, DEG_TO_R, R_MDI)                
CL    Routine to do x-differentiation for vorticity/divergence.                 
C     It calculates the difference between the line integral of AIN             
C     along the western edge of each grid box, and the line integral            
C     along the eastern edge (on a unit sphere).                                
C     The input field is assumed to be on the wind points of an                 
C     Arakawa B grid, and the output field on the mass grid points.             
CL I    AIN                   Input array                                       
CL O    AOUT                  Output array (last row only used for work)        
CL I    NX, NY                No of rows, points per row of input               
CL I    GRID(4)               1st lat, Dlat, 1st lon, Dlon input grid           
CL I    DEG_TO_R              Degrees to radians conversion factor              
CL I    R_MDI                 Missing data indicator                            
C NB. - The n'th row of AOUT is derived from the n'th and n+1'th rows           
C     of AIN, so only NY-1 rows of AOUT are set                                 
C     - 1st point in each row is set assuming wrap-around; ignore if            
C     non-wrap-around grid!                                                     
C                                                                               
      IMPLICIT NONE                                                             
      INTEGER NX, NY                                                            
      REAL AIN(NX,NY), AOUT(NX, NY)                                             
      REAL GRID(4)                                                              
      REAL DEG_TO_R, R_MDI                                                      
      INTEGER JX, JY                                                            
      REAL ZG                                                                   
C                                                                               
CL    Do differentiation                                                        
      ZG=SIGN(1.0,GRID(4))*ABS(0.5*DEG_TO_R*GRID(2))                            
      DO 190 JY=1,NY-1                                                          
C                                                                               
C     Average input in N-S; multiply by grid length                             
C     (use last row of AOUT as workspace)                                       
      DO 110 JX=1,NX                                                            
      IF(AIN(JX,JY).EQ.R_MDI.OR.AIN(JX,JY+1).EQ.R_MDI) THEN                     
        AOUT(JX,NY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(JX,NY)=ZG*(AIN(JX,JY)+AIN(JX,JY+1))                                
      END IF                                                                    
110   CONTINUE                                                                  
C                                                                               
C     Difference in E-W                                                         
      IF(AOUT(1 ,NY).EQ.R_MDI.OR.AOUT(NX  ,NY).EQ.R_MDI) THEN                   
        AOUT(1 ,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(1 ,JY)=AOUT(1 ,NY)-AOUT(NX,NY)                                     
      END IF                                                                    
      DO 120 JX=2,NX                                                            
      IF(AOUT(JX,NY).EQ.R_MDI.OR.AOUT(JX-1,NY).EQ.R_MDI) THEN                   
        AOUT(JX,JY)=R_MDI                                                       
      ELSE                                                                      
        AOUT(JX,JY)=AOUT(JX,NY)-AOUT(JX-1,NY)                                   
      END IF                                                                    
120   CONTINUE                                                                  
C                                                                               
190   CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
