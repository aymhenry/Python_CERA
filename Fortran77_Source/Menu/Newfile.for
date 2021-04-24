      SUBROUTINE NEWFILE(FILBUF, ICOD)
C     ******************************************************************
C     *    CREATE NEW FILE FROM STANDARD INPUT AND PASS BACK NAME      *
C     ******************************************************************
C
      LOGICAL         inputing_name
      CHARACTER       key, FILCOL, blank
      CHARACTER*13    filnam, blnkfl, filbuf
      INTEGER*2       icol, irow
 
      DIMENSION       FILCOL(13)
 
      EQUIVALENCE     (filnam, FILCOL(1))
 
      DATA            blank/' '/, blnkfl/'             '/
C
C          INITIALIZE
C
      filnam = blnkfl
 
      CALL video(0)
      CALL video(1)
      CALL gotoxy(31,5)
      CALL print('FILE CREATION MENU$',18,-2)
      CALL gotoxy(30,5)
      CALL light(49,3)
 
      CALL gotoxy(24,14)
      CALL print('Enter Name of New File: $',24,-2)
      CALL light(55,3)
      CALL gotoxy(48,14)
      CALL cursor(0)
 
      icod = 0
      number = 0
C
C          READ THE TITLE CHARACTER BY CHARACTER
C
      inputing_name = .TRUE.
      DO WHILE (inputing_name)
         IF(number .EQ. 8) THEN
            inputing_name = .FALSE.
            CYCLE
         END IF
 
         CALL getxy(icol,irow)
         CALL gotoxy(48,14)
         CALL light(55,3)
 
         CALL gotoxy(icol,irow)
         CALL inchr(1,j,key)
 
         IF(j .EQ. 1) THEN
            icod = 1
            RETURN
         END IF
 
         ichr = ICHAR(key)
         IF((ichr .GE. 48 .AND. ichr .LE. 57) .OR.
     .      (ichr .GE. 65 .AND. ichr .LE. 122)) THEN
              WRITE(*,'(A1\)') key
              number = number + 1
              FILCOL(number) = key
              CYCLE
         END IF
 
         IF(number .GT. 0) THEN
            IF(j .EQ. 14) THEN
               WRITE(*,'(A1\)') key
               WRITE(*,'(A1\)') blank
               WRITE(*,'(A1\)') key
               FILCOL(number) = blank
               number = number - 1
               CYCLE
            END IF
            IF(j .EQ. 28 .OR. j .EQ. 73 .OR. j.EQ. 81) THEN
               inputing_name = .FALSE.
               CYCLE
            END IF
         END IF
C
C          INVALID KEYPRESS
C
         CALL warble
 
      END DO
C
C          ADD THE EXTENSION AND THEN READ
C
      FILCOL(number+1) = '.'
      FILCOL(number+2) = 'E'
      FILCOL(number+3) = 'R'
      FILCOL(number+4) = 'A'
      FILCOL(number+5) = CHAR(0)
 
      filbuf = filnam
 
      icod = 0
      CALL cursor(1)
      RETURN
 
      END
