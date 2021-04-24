      SUBROUTINE SETINT(I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14)
C     ******************************************************************
C     *    SET UP COMMON BLOCK INTCOM TO IDENTIFY INTEGER VALUES       *
C     ******************************************************************
C
      COMMON /INTCOM/ INTCOD(14)
C
C          LOOP THROUGH THE CODE VALUES
C
      INTCOD(1)  = i1
      INTCOD(2)  = i2
      INTCOD(3)  = i3
      INTCOD(4)  = i4
      INTCOD(5)  = i5
      INTCOD(6)  = i6
      INTCOD(7)  = i7
      INTCOD(8)  = i8
      INTCOD(9)  = i9
      INTCOD(10) = i10
      INTCOD(11) = i11
      INTCOD(12) = i12
      INTCOD(13) = i13
      INTCOD(14) = i14
 
      RETURN
      END
 
      SUBROUTINE SETIOK(I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,
     .                  I14,IOK)
C     ******************************************************************
C     *    RESET THE IOK VECTOR FOR ERROR LIMITS                       *
C     ******************************************************************
C
      INTEGER*2       IOK
      DIMENSION       IOK(1)
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
 
C          LOOP THROUGH THE CODE VALUES
 
      IF(ignore .EQ. 1) RETURN
 
      IOK(1)  = i1
      IOK(2)  = i2
      IOK(3)  = i3
      IOK(4)  = i4
      IOK(5)  = i5
      IOK(6)  = i6
      IOK(7)  = i7
      IOK(8)  = i8
      IOK(9)  = i9
      IOK(10) = i10
      IOK(11) = i11
      IOK(12) = i12
      IOK(13) = i13
      IOK(14) = i14
 
      RETURN
      END
 
      SUBROUTINE BLOCK_HEAD(HEAD_LINE)
C     ******************************************************************
C     *      SET UP THE BLOCK DATA EDIT MENU AND THE HEADER TITLE      *
C     ******************************************************************
C
      LOGICAL         found_end
      CHARACTER       head_chr, form_feed
      CHARACTER*72    head_line, work_line
      DIMENSION       head_chr(72)
 
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
      COMMON /DSKINP/ iwrite, iprint, iout
      COMMON /EDTCOM/ icodet, inote
      COMMON /LINCNT/ num3, num_lines, form_feed
 
      EQUIVALENCE     (work_line, head_chr(1))
 
      work_line = head_line
 
      found_end = .FALSE.
      DO i = 72, 1, -1
         IF(found_end) CYCLE
 
         IF(head_chr(i) .EQ. ' ') THEN
            l_len = i
         ELSE
            found_end = .TRUE.
         END IF
 
      END DO
      l_len = l_len - 1
      l_start = (80 - l_len)/2
 
      IF(iprint .EQ. 1) THEN
 
         num_lines = num_lines + num3 + 4
         IF (num_lines .GE. 65) THEN
           IF(iwrite .EQ. 0) WRITE(iout,'(A)') form_feed
           IF(iwrite .EQ. 1) WRITE(iout,'(A)') CHAR(12)
           num_lines = num3 + 4
         END IF
 
         WRITE(iout, '(///13X, A72)') work_line
         IF(iwrite .EQ. 0) THEN
            CALL gotoxy(4, 15)
            CALL light(75, 2)
            CALL gotoxy(l_start,15)
            DO l = 1, l_len
               CALL print(head_chr(l),1,-2)
            END DO
         END IF
         RETURN
      END IF
 
      CALL display(5+inote, 4, 22, 3, 76)
 
      CALL gotoxy(l_start-1, 3)
      CALL print(' ', 1, -2)
      DO l = 1, l_len
         CALL print(head_chr(l),1,-2)
      END DO
      CALL print(' ', 1, -2)
 
      CALL gotoxy(l_start-1, 3)
      CALL atrbut(l_len+2, 79)
 
      IF(icodet .EQ. 0) RETURN
 
      SELECT CASE (icodet)
         CASE (-1)
  !        CALL gotoxy(22,22)
  !        CALL print(' <F5>: Specify Fractional Coverage ',35,-2)
  !        CALL gotoxy(22,22)
  !        CALL atrbut(35,79)
 
           CALL gotoxy(06,22)
           CALL print(' <F4>: Panel Thickness ',23,-2)
           CALL gotoxy(06,22)
           CALL atrbut(23,79)
           CALL gotoxy(50,22)
           CALL print(' <F5>: Fraction Covered ',24,-2)
           CALL gotoxy(50,22)
           CALL atrbut(24,79)
 
         CASE (15, 16)
           CALL gotoxy(06,22)
           CALL print(' <F4>: View Properties ',23,-2)
           CALL gotoxy(06,22)
           CALL atrbut(23,79)
           CALL gotoxy(52,22)
           CALL print(' * <F5>: Default Data ',22,-2)
           CALL gotoxy(52,22)
           CALL atrbut(22,79)
 
         CASE (19)
           CALL gotoxy(22,22)
           CALL print(' Press <F4> Key to View Properties ',35,-2)
           CALL gotoxy(22,22)
           CALL atrbut(35,79)
 
         CASE (20, 21)
          !Nothing
 
         CASE (23)
           CALL gotoxy(19,22)
           CALL print(' Press <F5> to Review DOE Product Classes ',
     .                                                           42,-2)
           CALL gotoxy(19,22)
           CALL atrbut(42,79)
 
         CASE DEFAULT
           CALL gotoxy(22,22)
           CALL print(' * Press <F5> Key to Calculate Data ',36,-2)
           CALL gotoxy(22,22)
           CALL atrbut(36,79)
 
      END SELECT
 
      RETURN
      END
 
      SUBROUTINE LINE(I_START, I_END, I_ROW)
C     ******************************************************************
C     *                 DRAW A WHITE LINE ACROSS THE SCREEN            *
C     ******************************************************************
C
      INTEGER*2       SET(5)
      DATA            SET /0, 0, 196, 07, 0/
 
      SET(1) = i_start + 1
      SET(2) = i_row +1
      SET(5) = i_end - i_start + 1
 
      CALL pictre(1, SET)
 
      RETURN
      END
 
      SUBROUTINE SHWSTR(ICODET, NUMMAX)
C     **************************************************************
C     *     SHOW ASTERISK FOR PARAMETERS CHAGED USING <F5> KEY    *
C     **************************************************************
C
      INTEGER*2       ICOLS, IROWS, IROW
      INTEGER         STRROW(53), STRBEG(26), STRNUM(26)
      COMMON /CONFIG/ icolr,iform,ifeed,iport,ignore
 
      DATA            STRROW /2,3,4,5,  2,3,4,5,6,  2,3,  2,3,4,5,
     .                        2,3,4,5,6,  1,2,  1,  4,9,  3,  1,3,4,5,
     .                        12,  0,  1,3,4,5,6,  8,  4,  7,8,9, 2,
     .                        1,2, 0, 0, 0, 8, 0, 0, 0, 0/
 
      DATA            STRBEG /1, 5, 10, 12, 16, 21, 23, 24, 26, 27,
     .                        31, 32, 33, 38, 39, 40, 43, 44, 46, 47,
     .                        48, 49, 50, 51, 52, 53/
 
      DATA            STRNUM /4, 5, 2, 4, 5, 2, 1, 2, 1, 4, 1, 1, 5,
     .                        1, 1, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1/
 
      DATA            i_offset /1/
C
C          SAVE CURSOR LOCATION AND THEN BRANCH TO DO PLACE THE
C          ASTERISKS
C
  !   IF(icodet .EQ. 12) RETURN
 
      CALL getxy(icols,irows)
 
      ibeg = STRBEG(icodet)
      iend = ibeg + STRNUM(icodet) - 1
 
      DO i = ibeg, iend
        irow = STRROW(I) + i_offset
        IF(irow .GT. nummax+i_offset) CYCLE
        IF(STRROW(i) .EQ. 0) CYCLE
        CALL gotoxy(10,irow + 4)
        CALL print('*',1,-2)
        CALL gotoxy(10,irow+4)
        IF(icolr .EQ. 1) CALL atrbut(1,30)
      END DO
 
      CALL gotoxy(icols,irows)
 
      RETURN
      END
 
      SUBROUTINE SHWFAN(VAL)
C     ******************************************************************
C     *    DISPLAY THE CURRENT FAN PARAMETERS                          *
C     ******************************************************************
C
      DIMENSION       VAL(1)
 
      ival = 1
      fan_pwr = VAL(ival+2)*(VAL(ival+6)/VAL(ival+1))**3
      fan_air = VAL(ival+3)*(VAL(ival+6)/VAL(ival+1))
 
      CALL gotoxy(24,16)
      CALL print('Calculated Fan Power  (W): $',27,-2)
      CALL print(fan_pwr,5,1)
 
      CALL gotoxy(24,17)
      CALL print('Calculated Air Flow (L/s): $',27,-2)
      CALL print(fan_air,5,1)
 
      RETURN
      END
 
      SUBROUTINE SHWEVP(VAL)
C     ******************************************************************
C     *    DISPLAY APPROXIMATE VALUE FOR EVAPORATOR UA                 *
C     ******************************************************************
C
      COMMON /EVPCOM/ degsup, evpopt
      DIMENSION       VAL(1)
 
      ievap = nfix(evpopt)
      IF(ievap .EQ. 1 .AND. degsup .GT. 0.0) THEN
         UA = VAL(1)*(0.9*VAL(3) + 0.1*VAL(4))
      ELSE
         UA = VAL(1)*VAL(3)
      END IF
 
      CALL gotoxy(19,14)
      CALL print('Estimated Overall Conductance (W/C): $', 37 ,-2)
      CALL print(UA,5,1)
 
 
      CALL gotoxy(23, 17)
      CALL atrbut(33, 28)
      CALL window(16, 18, 22, 56, 32, 2)
      CALL gotoxy(24, 17)
      CALL print('Press <F4> to Define Overall UA',31,-2)
 
      RETURN
      END
C
      SUBROUTINE SHWCND(VAL)
C     ******************************************************************
C     *    DISPLAY APPROXIMATE VALUE FOR CONDENSER UA                  *
C     ******************************************************************
C
      COMMON /EVPCOM/ degsub, cndopt
      DIMENSION       VAL(1)
 
      IF(degsub .GT. 0.0) THEN
         UA = VAL(1)*(0.05*VAL(3) + 0.80*VAL(4) + 0.15*VAL(5))
      ELSE
         UA = VAL(1)*(0.85*VAL(4) + 0.15*VAL(5))
      END IF
 
      CALL gotoxy(19,14)
      CALL print('Estimated Overall Conductance (W/C): $', 37 ,-2)
      CALL print(UA,5,1)
 
      CALL gotoxy(23, 17)
      CALL atrbut(33, 28)
      CALL window(16, 18, 22, 56, 32, 2)
      CALL gotoxy(24, 17)
      CALL print('Press <F4> to Define Overall UA',31,-2)
 
      RETURN
      END
C
      SUBROUTINE SHWPRP(ICODET, VAL)
C     ******************************************************************
C     *              DISPLAY REFFRIGERANT PROPERTIES                   *
C     ******************************************************************
C
      CHARACTER*5     AREF
      DIMENSION       VAL(1), AREF(34), BPREF(34)
      DATA            AREF / 'R11  ', 'R12  ', 'R13  ', 'n-C5',
     .                       'R14  ', 'R22  ', 'R23  ', 'R113 ',
     .                       'R114 ', 'R142b', 'R152a', 'R216a',
     .                       'R125 ', 'R143a', 'R134a', 'R123 ',
     .                       'RC318', 'R134 ', 'RC270', 'R141b',
     .                       'i-C5',  'R290 ', 'R600 ', 'R600a',
     .                       'R32  ', 'R1270', 'R124 ', 'R115 ',
     .                       'CE216', 'E125 ', 'R123a', 'R143 ',
     .                       'R218 ', 'E134 ' /
 
      DATA            BPREF / 23.8,  -29.8,  -81.5,   36.2, -128.0,
     .                       -40.9,  -82.0,   47.7,    3.7,   -9.1,
     .                       -24.7,   35.7,  -48.6,  -47.4,  -26.2,
     .                        27.9,   -7.0,  -19.8,  -32.9,   32.2,
     .                        27.8,  -42.1,   -0.5,  -11.7,  -51.8,
     .                       -47.7,  -13.2,  -39.2,  -29.2,  -35.7,
     .                        30.0,    4.0,  -36.7,    6.2 /
 
      SELECT CASE (icodet)
        CASE (19)
          CALL window(16, 18, 16, 63, 32, 2)
 
        CASE (15)
          CALL window(16, 19, 16, 63, 32, 2)
 
        CASE (16)
          CALL window(16, 20, 16, 63, 32, 2)
 
      END SELECT
 
      iref1 = nfix(VAL(1))
      CALL gotoxy(20,17)
      CALL print('Refrigerant ', 12, -2)
      CALL print(iref1, 2, -1)
      CALL print(':  ', 3, -2)
      CALL print(AREF(iref1), 5, -2)
      CALL print('     bp(C): ',12, -2)
      CALL print(BPREF(iref1), 6, 1)
 
      IF(icodet .EQ. 19) RETURN
 
      iref2 = nfix(VAL(3))
      CALL gotoxy(20,18)
      CALL print('Refrigerant ', 12, -2)
      CALL print(iref2, 2, -1)
      CALL print(':  ', 3, -2)
      CALL print(AREF(iref2), 5, -2)
      CALL print('     bp(C): ',12, -2)
      CALL print(BPREF(iref2), 6, 1)
 
      IF(icodet .EQ. 15) RETURN
 
      iref3 = nfix(VAL(5))
      CALL gotoxy(20,19)
      CALL print('Refrigerant ', 12, -2)
      CALL print(iref3, 2, -1)
      CALL print(':  ', 3, -2)
      CALL print(AREF(iref3), 5, -2)
      CALL print('     bp(C): ',12, -2)
      CALL print(BPREF(iref3), 6, 1)
 
      RETURN
      END
