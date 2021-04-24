$DEBUG
      SUBROUTINE SHWFIG(ICYCL)
C     *****************************************************************
C     *     OPEN FILE SCHEME.FIG                                      *
C     *****************************************************************
C
      CHARACTER*80 TITLE(24)
      COMMON / FIGURE / IEVAP
      DATA ISCHEM/1/
C
C          OPEN THE FILE AND PUT IT ON THE SCREEN
C
      I = 1
      SELECT CASE (ICYCL)
           CASE (1,3)
                OPEN(ISCHEM,FILE='SINGLE.FIG')
                DO WHILE (I .LE. 18)
                     READ(ISCHEM,'(A80)') TITLE(I)
                     I = I + 1
                END DO
                CLOSE (ISCHEM)
           CASE (2)
                OPEN(ISCHEM,FILE='LORENZ.FIG')
                DO WHILE (I .LE. 24)
                     READ(ISCHEM,'(A80)') TITLE(I)
                     I = I + 1
                END DO
                CLOSE (ISCHEM)
      END SELECT
C
C          SHOWN FIGURE ON SCREEN
C
      WRITE(*,'(1X\)')
      CALL GOTOXY(0,0)
      CALL SCREEN(0)
      CALL GOTOXY(0,0)
      I = 1
      IF(ICYCL .EQ. 2) THEN
           IEND = 24
      ELSE
           IEND = 18
      END IF
      DO WHILE (I .LE. IEND)
           CALL GOTOXY(0,I-1)
           WRITE(*,'(A80\)') TITLE(I)
           I = I + 1
      END DO
C
C          OUTPUT TITLE OF THE REFRIGERATION CIRCUIT
C
      IF(ICYCL .EQ. 1) THEN
           SELECT CASE (IEVAP)
                CASE (0)
                     CALL GOTOXY(17,4)
                     CALL PRINT('- STANDARD SINGLE EVAPORATOR CYCLE -',
     .                          36,-2)
                CASE (1)
                     CALL GOTOXY(24,4)
                     CALL PRINT('- FRESH FOOD SECTION -',22,-2)
                CASE (2)
                     CALL GOTOXY(26,4)
                     CALL PRINT('- FREEZER SECTION -',19,-2)
           END SELECT
      END IF
      IF(ICYCL .EQ. 2) THEN
           SELECT CASE (IEVAP)
                CASE (1)
                     CALL GOTOXY(27,4)
                     CALL PRINT('- LORENZ CYCLE -',16,-2)
                CASE (2)
                     CALL GOTOXY(25,4)
                     CALL PRINT('- DUAL EVAP CYCLE  -',20,-2)
           END SELECT
      END IF
      IF(ICYCL .EQ. 3) THEN
           IF(IEVAP .EQ. 1) THEN
                CALL GOTOXY(17,4)
                CALL PRINT('DUAL LOOP CYCLE: FRESH FOOD LOOP',32,-2)
           ELSE
                CALL GOTOXY(18,4)
                CALL PRINT('DUAL LOOP CYCLE: FREEZER LOOP',29,-2)
           END IF
      END IF
      RETURN
      END
C
      SUBROUTINE OUTPUT(ICYCL,T,W,QC,QE,QZ)
C     *****************************************************************
C     *     OPEN FILE SCHEME.FIG                                      *
C     *****************************************************************
C
      CHARACTER*80 TITLE
      DIMENSION T(1)
 
      COMMON/FEVAP/UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
      COMMON/CONDEN/UDSC,UTPC,USCC,ATOTC,UACOND
 
      DATA ISCHEM /1/, IO /8/
 
C
C          OUTPUT A FIGURE OF THE RESULTS
C
      WRITE(IO,'(/)')
      IF(ICYCL .EQ. 2) THEN
           OPEN(ISCHEM,FILE='OUTPTL.FIG')
           K = 1
           DO WHILE (K .LE. 31)
                READ (ISCHEM,'(A80)') TITLE
                SELECT CASE (K)
                     CASE(1)
                          CALL SHWOUT(1,32,6,TITLE,QC)
                     CASE(3)
                          CALL SHWOUT(3,15,5,TITLE,T(4))
                          CALL SHWOUT(3,55,5,TITLE,T(14))
                     CASE(4)
                          CALL SHWOUT(4,64,5,TITLE,T(2))
                     CASE(6)
                          CALL SHWOUT(6,35,4,TITLE,UACOND/3.6)
                     CASE(7)
                          CALL SHWOUT(7,68,6,TITLE,W)
                     CASE(8)
                          CALL SHWOUT(8,5,5,TITLE,T(16))
                          CALL SHWOUT(8,16,5,TITLE,T(13))
                          CALL SHWOUT(8,50,5,TITLE,T(1))
                     CASE(13)
                          CALL SHWOUT(13,32,6,TITLE,QE)
                     CASE(15)
                          CALL SHWOUT(15,5,5,TITLE,T(6))
                          CALL SHWOUT(15,16,5,TITLE,T(7))
                          CALL SHWOUT(15,54,5,TITLE,T(5))
                     CASE(19)
                          CALL SHWOUT(19,35,4,TITLE,UAFF/3.6)
                     CASE(26)
                          CALL SHWOUT(26,32,6,TITLE,QZ)
                     CASE(27)
                          CALL SHWOUT(27,5,5,TITLE,T(10))
                     CASE(28)
                          CALL SHWOUT(28,16,5,TITLE,T(8))
                          CALL SHWOUT(28,55,5,TITLE,T(9))
                     CASE(31)
                          CALL SHWOUT(31,35,4,TITLE,UAFZ/3.6)
                END SELECT
                WRITE(8,'(A80)') TITLE
                K = K + 1
           END DO
      ELSE
           OPEN(ISCHEM,FILE='OUTPTS.FIG')
           K = 1
           DO WHILE (K .LE. 23)
                READ (ISCHEM,'(A80)') TITLE
                SELECT CASE (K)
                     CASE(1)
                          CALL SHWOUT(1,32,6,TITLE,QC)
                     CASE(3)
                          CALL SHWOUT(3,15,5,TITLE,T(4))
                          CALL SHWOUT(3,55,5,TITLE,T(14))
                     CASE(4)
                          CALL SHWOUT(4,64,5,TITLE,T(2))
                     CASE(6)
                          CALL SHWOUT(6,36,4,TITLE,UACOND/3.6)
                     CASE(7)
                          CALL SHWOUT(7,68,6,TITLE,W)
                     CASE(9)
                          CALL SHWOUT(9,5,5,TITLE,T(16))
                          CALL SHWOUT(9,16,5,TITLE,T(13))
                          CALL SHWOUT(9,50,5,TITLE,T(1))
                     CASE(15)
                          CALL SHWOUT(15,5,5,TITLE,T(6))
                          CALL SHWOUT(15,16,5,TITLE,T(7))
                     CASE(18)
                          CALL SHWOUT(18,32,6,TITLE,QE)
                     CASE(20)
                          CALL SHWOUT(20,16,5,TITLE,T(5))
                     CASE(23)
                          CALL SHWOUT(23,36,4,TITLE,UAFF/3.6)
                END SELECT
                WRITE(8,'(A80)') TITLE
                K = K + 1
           END DO
      END IF
C
C          RETURN TO CALLER
C
      CLOSE(ISCHEM)
      CLOSE(IO)
      RETURN
      END
