      SUBROUTINE WARBLE
C     ******************************************************************
C     *         MAKE A WARBLE SOUND                                    *
C     ******************************************************************
      INTEGER*2       iptch1, iptch2, idur1, idur2
      DATA            iptch1 /55/,
     .                iptch2 /40/,
     .                idur1  /16/,
     .                idur2  /24/
C
C          CALL ASSEMBLY ROUTINE sound THREE TIMES
C
      CALL sound(iptch1,idur1)
      CALL sound(iptch2,idur1)
      CALL sound(iptch1,idur2)
 
      RETURN
      END
 
      SUBROUTINE CHECK(KCHAR,LEN,MENUE,VALID,LOC)
C     ************************************************************
C     *       DETERMINE IF ONE BYTE CHARACTER IS A valid ENTRY   *
C     ************************************************************
      LOGICAL         valid
      CHARACTER       kchar, MENUE
      INTEGER         ASCII
      DIMENSION       MENUE(1)
C
C          MAKE SURE THE CHARACTER IS UPPER CASE
C
      ascii = ICHAR(kchar)
      IF(ascii .GE. 97 .AND. ascii .LE. 122) ascii = ascii - 32
      kchar = CHAR(ascii)
C
C          INITIALIZE 'VALID' TO FALSE AND CHECK CHARACTER
C
      loc = 0
      valid = .FALSE.
      DO l = 1, LEN
        IF(kchar .NE. MENUE(l)) CYCLE
        valid=.TRUE.
        loc = l
      END DO
 
      RETURN
      END
 
      FUNCTION NFIX(X)
C     ******************************************************************
C     *     FIX A REAL NUMBER WITH A ROUND UP                          *
C     ******************************************************************
C
      nfix = IFIX(x + 0.5)
 
      RETURN
      END
C
      SUBROUTINE ATRBUT(NUMBER, IATR)
C     ******************************************************************
C     *     SCREEN OUT COLOR ATRIBUTE CALLS FOR MONOCHROME MONITOR     *
C     ******************************************************************
C
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
 
      IF(icolr .EQ. 0) RETURN
 
      CALL atrbut1(number, iatr)
 
      RETURN
      END
C
      SUBROUTINE SETATR(IATR)
C     ******************************************************************
C     *     SCREEN OUT COLOR ATRIBUTE CALLS FOR MONOCHROME MONITOR     *
C     ******************************************************************
C
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
 
      IF(icolr .EQ. 0) RETURN
 
      CALL setatr1(iatr)
 
      RETURN
      END
