!***************************************************************************************************
!- PURPORSE:
!     CALCULATE THE SHAPE FUNCTIONS AND THEIR DERIVATIVES FOR 2D ELEMENTS.
!  
!- INPUT ARGUMENTS:
!  INTE_POINT: THE INFORMATION OF INTEGRATION POINTS
!  NUM_POINT:  THE NUMBER OF INTEGRATION POINTS
!  NUM_NODE :  THE NUMBER OF ELEMENT NODES
!
!- OUTPUT ARGUMENTS:
!  SHAPES : THE SHAPE FUNCTION
!  D_SHAPE: THE DERIVATIVE OF SHAPE FUNCTION OVER NATURAL COORDINATES
!
!- CALL PROCEDURES:
!  NONE
!
!- CALLED BY
!  INIT_SHAPE
!
!- PROGAMMED BY:
!  ZHIHAI XIANG, DEPARTMENT OF ENGINEERING MECHANICS, TSINGHUA UNIVERSITY, JANUARY 16, 2016
!***************************************************************************************************

SUBROUTINE SHAPE_2D(INTE_POINT, NUM_POINT, NUM_NODE, SHAPES, D_SHAPE)

USE BASIC_DATA, ONLY: INT_KIND, REAL_KIND, NUM_INTE
                          
IMPLICIT NONE

INTEGER(INT_KIND), INTENT(IN):: NUM_POINT                        ! THE NUMBER OF INTEGRATION POINTS
INTEGER(INT_KIND), INTENT(IN):: NUM_NODE                         ! THE NUMBER OF ELEMENT NODES
TYPE(NUM_INTE), INTENT(IN)::    INTE_POINT(NUM_POINT)            ! THE INFORMATION OF NUMERICAL INTEGRATION POINTS
REAL(REAL_KIND), INTENT(OUT)::  SHAPES(NUM_NODE, NUM_POINT)      ! THE SHAPE FUNCTION
REAL(REAL_KIND), INTENT(OUT)::  D_SHAPE(2, NUM_NODE, NUM_POINT)  ! THE DERIVATIVE OF SHAPE FUNCTION OVER NATURAL COORDINATES
REAL(REAL_KIND), PARAMETER::    NODE_SIGN(2,8) = &               ! THE SIGN OF KESI, ETA FOR EACH ELEMENT NODE
                                (/-1.0, -1.0,    1.0, -1.0,   1.0,  1.0,   -1.0,  1.0, &
                                   0.0, -1.0,    1.0,  0.0,   0.0,  1.0,   -1.0,  0.0/)
INTEGER(INT_KIND), PARAMETER::  CORNER_EDGE(2,4) = &             ! THE ADJACENT EDGE NODE NUMBER CLOSE TO A CORNER NODE
                                (/5, 8,  5, 6,  6, 7,  7, 8/)
INTEGER(INT_KIND)               I                                ! LOOP INDEX FOR INTEGRATION POINTS
INTEGER(INT_KIND)               J                                ! LOOP INDEX FOR ELEMENT NODES

!-- SET SHAPE FUNCTIONS --
DO I = 1, NUM_POINT
   ! TRIANGULAR ELEMENTS (AREA COORDINATES)
   IF(NUM_NODE .EQ. 3 .OR. NUM_NODE .EQ. 6) THEN   ! 王勖成《有限单元法》2003年版 P108
      DO J = 1, 3
         SHAPES(J,I) = INTE_POINT(I)%COORD(J)
      ENDDO
   
      IF(NUM_NODE .EQ. 3) CYCLE
      
      SHAPES(4,I) = 4.0 * INTE_POINT(I)%COORD(1) * INTE_POINT(I)%COORD(2)
      SHAPES(5,I) = 4.0 * INTE_POINT(I)%COORD(2) * INTE_POINT(I)%COORD(3)
      SHAPES(6,I) = 4.0 * INTE_POINT(I)%COORD(3) * INTE_POINT(I)%COORD(1)
      
      SHAPES(1,I) = SHAPES(1,I) - (SHAPES(4,I) + SHAPES(6,I)) / 2.0
      SHAPES(2,I) = SHAPES(2,I) - (SHAPES(4,I) + SHAPES(5,I)) / 2.0
      SHAPES(3,I) = SHAPES(3,I) - (SHAPES(5,I) + SHAPES(6,I)) / 2.0
   ELSE ! RECTANGULAR ELEMENTS (CARTESIAN COORDINATES) 王勖成《有限单元法》2003年版 P114
      ! NODES: (1,2,3,4) 
      DO J = 1, 4 
         SHAPES(J,I) = (1.0 + NODE_SIGN(1,J) * INTE_POINT(I)%COORD(1)) * &
                       (1.0 + NODE_SIGN(2,J) * INTE_POINT(I)%COORD(2)) / 4.0
      ENDDO   

      IF(NUM_NODE .EQ. 4) CYCLE
   
      DO J = 5, 7, 2
         SHAPES(J,I) = (1.0 + NODE_SIGN(2,J) * INTE_POINT(I)%COORD(2)) / 2.0 * &  
                       (1.0 - INTE_POINT(I)%COORD(1) ** 2)
      ENDDO
      
      ! NODES: (6,8) ETA = 0
      DO J = 6, 8, 2
         SHAPES(J,I) = (1.0 + NODE_SIGN(1,J) * INTE_POINT(I)%COORD(1)) / 2.0 * &  
                       (1.0 - INTE_POINT(I)%COORD(2) ** 2)
      ENDDO

      ! ADJUST THE CORNER NODES
      DO J = 1, 4
         SHAPES(J,I) = SHAPES(J,I) - (SHAPES(CORNER_EDGE(1,J),I) + SHAPES(CORNER_EDGE(2,J),I)) / 2.0
      ENDDO
      
      IF(NUM_NODE .EQ. 4 .OR. NUM_NODE .EQ. 8) CYCLE
      DO J = 9,9
         SHAPES(J,I) = (1.0 - INTE_POINT(I)%COORD(1) ** 2) * (1.0 - INTE_POINT(I)%COORD(2) ** 2)
      ENDDO
      DO J = 1, 4
          SHAPES(J,I) = SHAPES(J,I) - SHAPES(9,I) / 4.0
      ENDDO
      DO J = 5, 8
          SHAPES(J,I) = SHAPES(J,I) - SHAPES(9,I) / 2.0
      ENDDO
    ENDIF
ENDDO

!-- SET THE DERIVATIVE OF SHAPE FUNCTIONS --
DO I = 1, NUM_POINT
   ! TRIANGULAR ELEMENTS (AREA COORDINATES)
   IF(NUM_NODE .EQ. 3 .OR. NUM_NODE .EQ. 6) THEN
      D_SHAPE(1,1,I) =  1.0
      D_SHAPE(2,1,I) =  0.0
      D_SHAPE(1,2,I) =  0.0
      D_SHAPE(2,2,I) =  1.0
      D_SHAPE(1,3,I) = -1.0
      D_SHAPE(2,3,I) = -1.0      
         
      IF(NUM_NODE .EQ. 3) CYCLE
      
      D_SHAPE(1,4,I) =  4.0 *  INTE_POINT(I)%COORD(2)
      D_SHAPE(2,4,I) =  4.0 *  INTE_POINT(I)%COORD(1)
      D_SHAPE(1,5,I) = -4.0 *  INTE_POINT(I)%COORD(2)
      D_SHAPE(2,5,I) =  4.0 * (INTE_POINT(I)%COORD(3) - INTE_POINT(I)%COORD(2))
      D_SHAPE(1,6,I) =  4.0 * (INTE_POINT(I)%COORD(3) - INTE_POINT(I)%COORD(1))
      D_SHAPE(2,6,I) = -4.0 *  INTE_POINT(I)%COORD(1)
            
      D_SHAPE(:,1,I) = D_SHAPE(:,1,I) - (D_SHAPE(:,4,I) + D_SHAPE(:,6,I)) / 2.0
      D_SHAPE(:,2,I) = D_SHAPE(:,2,I) - (D_SHAPE(:,4,I) + D_SHAPE(:,5,I)) / 2.0
      D_SHAPE(:,3,I) = D_SHAPE(:,3,I) - (D_SHAPE(:,5,I) + D_SHAPE(:,6,I)) / 2.0
   ELSE ! RECTANGULAR ELEMENTS (CARTESIAN COORDINATES)
      ! NODES: (1,2,3,4)
      DO J = 1, 4
         D_SHAPE(1,J,I) = NODE_SIGN(1,J) * (1.0 + NODE_SIGN(2,J) * INTE_POINT(I)%COORD(2)) / 4.0
         D_SHAPE(2,J,I) = NODE_SIGN(2,J) * (1.0 + NODE_SIGN(1,J) * INTE_POINT(I)%COORD(1)) / 4.0
      ENDDO   

      IF(NUM_NODE .EQ. 4) CYCLE
   
      ! NODES: (5,7) KESI = 0
      DO J = 5, 7, 2
         D_SHAPE(1,J,I) = -(1.0 + NODE_SIGN(2,J) * INTE_POINT(I)%COORD(2)) * INTE_POINT(I)%COORD(1)
         D_SHAPE(2,J,I) = NODE_SIGN(2,J) * (1.0 - INTE_POINT(I)%COORD(1) ** 2)/ 2.0 
      ENDDO
      
      ! NODES: (6,8) ETA = 0
      DO J = 6, 8, 2
         D_SHAPE(1,J,I) = NODE_SIGN(1,J) * (1.0 - INTE_POINT(I)%COORD(2) ** 2)/ 2.0
         D_SHAPE(2,J,I) = -(1.0 + NODE_SIGN(1,J) * INTE_POINT(I)%COORD(1)) * INTE_POINT(I)%COORD(2)
      ENDDO

      ! ADJUST THE CORNER NODES
      DO J = 1, 4
         D_SHAPE(:,J,I) = D_SHAPE(:,J,I) - (D_SHAPE(:,CORNER_EDGE(1,J),I) + D_SHAPE(:,CORNER_EDGE(2,J),I)) / 2.0
      ENDDO
   
   
      IF(NUM_NODE .EQ. 4 .OR. NUM_NODE .EQ. 8) CYCLE
        D_SHAPE(1,9,I) = -2.0 * (1.0 - INTE_POINT(I)%COORD(2)) * INTE_POINT(I)%COORD(1)
        D_SHAPE(2,9,I) = -2.0 * (1.0 - INTE_POINT(I)%COORD(1)) * INTE_POINT(I)%COORD(2)
        
        DO J = 1, 4
           D_SHAPE(:,J,I) = D_SHAPE(:,J,I) - D_SHAPE(:,9,I) / 4.0
        ENDDO
        DO J = 5, 8
           D_SHAPE(:,J,I) = D_SHAPE(:,J,I) - D_SHAPE(:,9,I) / 2.0
        ENDDO 
    ENDIF
ENDDO

END SUBROUTINE SHAPE_2D