      SUBROUTINE CLEAR
C     ******************************************************************
C     *     THIS ROUTINE ZEROS AND INITIALIZES SOME OF                 *
C     *     THE VARIABLES IN COMMON                                    *
C     ******************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ZER0(5), ZER1(11), ZER2(20), ZER3(22), ZER4(13)
      DIMENSION ZER5(8), ZER6(8),  ZER7(13)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / AIRCAB / QRS, QZS, EVAPCA, RIR, RIZ
      COMMON / EVAPHX / FEAREA, FEAF, FEUA, FECA, REAREA,
     .                  REAF, REUA, RECA, ITPFEV, ITPREV, IHWP
      COMMON / TRIAL /  NTRIAL(8), GRL, HNC, HA, HIN,
     .                  ETA, H1SAVE, H3SAVE, H10SAV
      COMMON / QUALTY / DHEVAP, CPT9, QUAL, T7SHT
      COMMON / LIMIT /  FINISH, DAYKWH, T3, T6, T10, RUN, RUNR, RUNZ,
     .                  TWP, W, PCOMP, QCOMP, TMA
      COMMON / INPT /   ISIENG, IC, ICC
      COMMON / INPTAB / EDH, TCYDEF, EHTR, EFAN, EMISC, TCOMON
 
      EQUIVALENCE (ZER0(1),QRS),    (ZER1(1),TRSH),
     .            (ZER2(1),TR),     (ZER3(1),RIRHTR),  (ZER4(1),ENERGY),
     .            (ZER5(1),FEAREA), (ZER6(1), EDH),    (ZER7(1), FINISH)
C
C           ZERO OUT SOME INPUT VARIABLES
C           INITIALIZE T2 AND T7
C
      DO  I = 1, 5
         ZER0(I) = 0.
      END DO
 
      DO  I = 1, 11
         ZER1(I) = 0.
      END DO
 
      DO  I = 1, 20
         ZER2(I) = 0.
      END DO
 
      DO I = 1, 22
         ZER3(I) = 0.
      END DO
 
      DO I = 1, 13
         ZER4(I) = 0.
      END DO
 
      DO I = 1, 8
         ZER5(I) = 0
      END DO
 
      DO I = 1, 6
         ZER6(I) = 0
      END DO
 
      DO I = 1, 8
         NTRIAL(I) = 0
      END DO
 
      DO I = 1, 13
         ZER7(I) = 0.0
      END DO
 
      T7 = -20.
      T2 = -1.E6
 
      RETURN
      END
