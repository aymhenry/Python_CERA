$debug
      SUBROUTINE START
C     ******************************************************************
C     *     ROUTINE TO START THE REFRIGERATOR LOADS PROGRAM            *
C     *     CLEAR THE COMMON BLOCKS, READ THE INPUT DATA, AND CALL     *
C     *     ON THE SUBROUTINES THAT CALCULATE THE LOADS                *
C     ******************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
C
C           ZERO OUT ALL INPUT VARIABLES
C
      CALL CLEAR
C
C           PERFORM THE ANALYSIS
C
      CALL QREAD(VFF,VFZ)
C
C            PRINT REFRIGERATOR INPUT DESCRIPTION DATA
c
      CALL QWRITI(VFF,VFZ)
C
C          BEGIN THE LOADS ANALYSIS
C
      CALL QINIT
      SELECT CASE (NMOD)
         CASE (1, 3)
            CALL QL13
 
         CASE (2)
            CALL QL2
 
         CASE (4, 6, 7)
            CALL QL467
 
         CASE (5)
            CALL QL5
 
         CASE (8)
            CALL QL8
 
      END SELECT
C
C          OUTPUT THE RESULTS
C
      CALL QWRITO
      RETURN
      END
