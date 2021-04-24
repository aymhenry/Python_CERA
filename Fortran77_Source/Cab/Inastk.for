      SUBROUTINE INASTK(A)
C     ******************************************************************
C     *   THIS ROUTINE READS IN ONE LINE OF INPUT DATA AND SCREENS OUT *
C     *   ALL LINES THAT BEGIN WITH AN ASTERICK                        *
C     ******************************************************************
C
      CHARACTER*80 A, AA
      CHARACTER*1 A1(80)
 
      COMMON /INIO/ IN, IO, IPRINT, ICYCL
 
      EQUIVALENCE (AA,A1(1))
C
C           Sort through input data and reject comment cards
C
      DO WHILE (.TRUE.)
         READ(IN, '(A80)', IOSTAT=IOCHECK) A
         IF(IOCHECK .NE. 0) THEN
            CALL GOTOXY(28,14)
            CALL PRINT('Error: Incomplete data set$',26,-2)
            CLOSE (IO)
            CALL EXIT
         END IF
 
         AA = A
         IF(A1(1) .NE. '*') RETURN
      END DO
      END
