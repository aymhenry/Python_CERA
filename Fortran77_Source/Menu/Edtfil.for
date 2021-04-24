      SUBROUTINE EDTFIL (FILNAM, NUM1_EDT, NUM2_EDT, ITITL_EDT,
     .                   INUM1_EDT, IFLAG, IO, VAL, VALMN, VALMX, IOK,
     .                   INUM2_EDT)
C     ******************************************************************
C     *       EDIT BLOCK??.DAT DATA FILES AND READS HELP MENU          *
C     ******************************************************************
 
      LOGICAL TEST, EXISTS, FILE_NOT_FOUND, TITLES_BLOCK,
     .        GET_KEYBOARD, SHOW_DATA, NOT_DONE_YET, LEAVE, IN_ERROR,
     .        EDITING_TITLE, valid_entry
 
      CHARACTER TITL1, TITL2, KEY, TITLE, form_feed
      CHARACTER*11 FILNAM, FILLST, FILRED, FILBLK
 
      INTEGER*2 IOK, ICOLS, IROWS, NFLAG
      REAL NEWVAL
 
 
      DIMENSION TITL1(27,20), TITL2(27,20)
 
      DIMENSION LINE(30), TEST(30), NINE(30)
      DIMENSION VAL(1), VALMN(1), VALMX(1), IOK(1)
      DIMENSION ROUND(6)
 
      COMMON /CHANGE/ ISAV, ILPT1, NUMDUM, IBYPAS, NUMPAT, IBUF, IFILE
      COMMON /DSKINP/ IWRITE
      COMMON /LSTBLK /FILLST, FILRED
      COMMON /TITL /NTITL, TITLE(68,5)
      COMMON /EDTCOM/ ICODET, INOTE
      COMMON /INTCOM/ INTCOD(14)
      COMMON /LINCNT/ num3, num_lines, form_feed
 
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore, iold_data
 
      DATA IDISK /6/,  FILBLK/'           '/, i_offset /1/
 
      DATA ROUND /0.0, 0.05, 0.005, 0.0005, 0.00005, 0.000005/
 
C
C          CHECK FOR COMMAND TO HALT WRITE OUT OF FILE
C
      IF(io .NE. 0) THEN
         CALL inchr(0, j, key)
         IF (j .EQ. 1) THEN
            iflag = 1
            RETURN
         END IF
      END IF
C
C          INITIALIZE  AND SAVE INPUT PARAMETERS
C
      NUM1 = NUM1_EDT
      NUM2 = NUM2_EDT
      ITITL = ITITL_EDT
      INUM1 = INUM1_EDT
      INUM2 = IABS(INUM2_EDT)                              !<0 means
                                                           !re-open file
 
      VALSV1 = VAL(1)
      VALSV2 = VAL(9)
      VALSV3 = VAL(5)
      VALSV4 = VAL(8)
      VALSV5 = VAL(1)
C
C          SET FLAGS RELATING TO FUNCTIONS REQUESTED
C
      IESC = 0
      IHELP = 0
C
C          RE-SET FILE NAMES TO BLANK IF THE FILE IS TO BE RE-READ
C
      SELECT CASE (ITITL)
         CASE(10, 11, 13, 14, 15)
            FILRED = FILBLK
            FILLST = FILBLK
      END SELECT
C
C          ESTABLISH IF THE FILE HAS ALREADY BEEN OPENED
C
      IOPEN = 0
      IF(FILNAM .EQ. FILRED .AND. IO .EQ. 0
     .                      .AND. INUM2_EDT .GT. 0) IOPEN = 1
 
      IF(ITITL .EQ. 16) IOPEN = 0
 
 
C
C          BRANCH PAST READING THE FILE IF THE TITLES MENU (NUM1 = 0)
C
      IF(NUM1 .NE. 0) THEN
         CALL CURSOR (1)
         IF(IO .EQ. 0) THEN
  !         CALL GOTOXY(10,10)
  !         WRITE(*,'('' ''\)')
         END IF
         FILE_NOT_FOUND = .TRUE.
         TITLES_BLOCK = .FALSE.
 
      ELSE
         FILE_NOT_FOUND = .FALSE.                          !Edit title
         TITLES_BLOCK = .TRUE.
      END IF
C
C          LOOK FOR A FILE TO OPEN
C
      DO WHILE (FILE_NOT_FOUND)
         KMAX1 = 27*NUM1
         KMAX2 = 27*NUM2
         IFLAG = 0
         ILAST = 0
         IFIRST = 0
         ICUR = 0
C
C          IS DRIVE DOOR OPEN ?
C
         IRET = 0
         INQUIRE(FILE=FILNAM, EXIST=EXISTS, IOSTAT=IRET)
         IF(EXISTS) THEN
            FILE_NOT_FOUND = .FALSE.
            CYCLE
         END IF
C
C          ERROR : DISK READ OR FILE MISSING
C
  !      CALL WARBLE
         CALL GOTOXY(19,21)
         CALL LIGHT(70,2)
 
  !      CALL GOTOXY(1,22)
  !      CALL LIGHT(78,2)
 
  !      CALL GOTOXY(12,23)
  !      CALL LIGHT(75,2)
C
C          IS FILE MISSING ?
C
         CALL GOTOXY(26,11)
         WRITE(*,'(''File '',A11,'' is Missing.''\)') FILNAM
 
         CALL GOTOXY(27,13)
         CALL PRINT('The File Cannot be Edited$',26,-2)
         CALL GOTOXY(27,21)
         CALL PRINT('Press Any Key to Continue$',26,-2)
         CALL INCHR(3,J,KEY)
         CALL INCHR(1,J,KEY)
         RETURN
 
      END DO
 
C
C          OPEN FILE 'FILNAM'
C
      IF(IOPEN .EQ. 0) THEN
         OPEN(IDISK,FILE=FILNAM,FORM='BINARY')
 
         IF(.NOT. TITLES_BLOCK) THEN
C
C          DETERMINE TYPE OF READ BASED ON ITITL :
C          ITITL = 0 : READ TWO SETS OF TITLES AND
C                         USE THE VALUE OF VAL(INUM1) TO
C                         SELECT WHICH TITLES TO USE
C                  1 : READ 1ST SET ONLY
C                  2 : READ TWO SETS AND USE 2ND
C                  3 : READ FIRST SET AND DISPLAY NUM2
C                         IF VAL(INUM1) = 1
C                  4 : READ TWO SETS AND USE SET INUM1
C                  5 : READ SET AND DISPLAY IN INCREMENTS OF 2
C                         DEPENDING ON VAL(INUM1)
C                  6 : SAME AS 0 BUT USE VALUE (VAL(INUM1 - 1)
C                  7 : READ TWO SETS AND USE THE SECOND; AFTER
C                         THE READ SET NUM2 = INUM1 AND INUM1 = 2, AND
C                         ITITL = 4
C                  8 : READ ONE SET OF DATA AND USE VAL(INUM1)
C                         TO SELECT THE PRINTOUT; 1 = 1 LINE,
C                         2 = 5 LINES, 3 = 7 LINES ,4 = 10 LINES
C                  9 : READ TWO SETS OF DAT AND USE THE SECOND
C                         IF VAL(1) = INUM1
C                 10:  SAME AS 4 BUT RETURN IF VAL(1) IS CHANGED
C                 11:  SAME AS 4 BUT RETURN IF VAL(9) CHANGED
C                 12:  SAME AS 3 BUT PRINT OUT ADDITIONAL LINE
C                 13:  SAME AS 4 BUT RETURN IF VAL(5) CHANGED
C                 14:  SAME AS 4 BUT RETURN IF VAL(8) CHANGED
C                 15:  SAME AS 4 BUT RETURN IF VAL(1) CHANGED
C                 16:  SAME AS 1 BUT RETURN IF VAL(1) CHANGED
C                         FROM 4 TO SOMETHING ELSE OR FROM
C                         SOMETHING ELSE TO 4
C
            SELECT CASE (ITITL)
               CASE (1, 3, 5, 8, 12, 16)
                  READ(IDISK) (TITL1(K,1),K=1,KMAX1)
 
               CASE DEFAULT
                  READ(IDISK) (TITL1(K,1),K=1,KMAX1),
     .                        (TITL2(K,1),K=1,KMAX2)
 
            END SELECT
         END IF
 
         IF(IO .NE. 0) CLOSE(IDISK)
      END IF
C
C          RESET INDICES BASED ON ITITL (MAJOR OPTION)
C
      SELECT CASE (ITITL)
         CASE (4, 10, 11, 13, 14, 15)
            ITITL = INUM1
            NUM3 = NUM2
 
         CASE (7)
            NUM3 = NUM2
            NUM2 = INUM1
            INUM1 = 2
            ITITL = 4
            KMAX2 = 27*NUM2
 
         CASE DEFAULT
            NUM3 = NUM2
 
      END SELECT
 
      IF(NUM1 .GT. NUM3) NUM3 = NUM1
C
C          SET UP LINE COUNTERS BASED ON VALUE CODE
C
      L = 0
      IF(.NOT. TITLES_BLOCK) THEN
         DO N = 1, NUM3
            TEST(N) = .FALSE.
            IF(VAL(N) .EQ. -99.0) CYCLE
            IF(VALMX(N) .EQ. -99.0) CYCLE
            L = L + 1
            LINE(N) = L
            NINE(L) = N
            IF(N .LE. NUM1) LMAX1 = L
            IF(N .LE. NUM2) LMAX2 = L
            TEST(N) = .TRUE.
         END DO
 
      ELSE                                                 !Set flag for
         ITITL = -1                                        !Edit title
 
      END IF
 
C
C          DETERMINE WHICH SET OF TITLES TO USE
C
      DO WHILE (.TRUE.)
         SELECT CASE (ITITL)
            CASE (-1)                                      !Edit title
 
            CASE (1, 3, 5, 8, 12, 16)
               IREAD = 1
 
            CASE (2)
               IREAD = 2
 
            CASE (6)
               IF(NFIX(VAL(INUM1)) .EQ. 1) THEN
                  IREAD = 2
               ELSE
                  IREAD = 1
               END IF
 
            CASE (9)
               IF(NFIX(VAL(1)) .EQ. INUM1) THEN
                  IREAD = 2
               ELSE
                  IREAD = 1
               END IF
 
            CASE (15)
               iread = 1
 
            CASE DEFAULT                                   !ITITL = 4
               IF(NFIX(VAL(INUM1)+0.5) .EQ. INUM2 .OR.
     .                NFIX(VAL(INUM1)) .EQ. 2) THEN
                  IREAD = 2
               ELSE
                  IREAD = 1
               END IF
 
         END SELECT
 
         IF(IREAD .EQ. 1) THEN
            NUMMAX = NUM1
            LMAX = LMAX1
 
         ELSE
            NUMMAX = NUM2
            LMAX = LMAX2
         END IF
C
C          RESET THE INDICES RELATING TO THE NUMBER OF LINES DEPENDING
C          UPON THE CALL PARAMETER ITITL.  THIS CONTROLS THE PRINT OUT
C
         SELECT CASE (ITITL)
             CASE (1, 16)
               LMAX2 = L
 
             CASE (3)
               ILINES = 2.0*VAL(INUM1) - 0.9
               LMAX = LMAX2 + ILINES - 1
               NUMMAX = NUM2 + ILINES
 
            CASE (5)
               ILINES = 2.0*VAL(INUM1) + 0.1
               LMAX = LMAX2 + ILINES
               NUMMAX = NUM2 + ILINES
 
            CASE (12)
               ILINES = 2.0*VAL(INUM1) - 0.9
               LMAX = LMAX2 + ILINES
               NUMMAX = NUM2 + ILINES
 
            CASE (8)
               NVAL = NFIX(VAL(INUM1))
               SELECT CASE (NVAL)
                  CASE (1)
                     LMAX = 1
 
                  CASE (2)
                     LMAX = 3
 
                  CASE (3)
                     LMAX = 5
 
                  CASE (4)
                     LMAX = 8
 
               END SELECT
 
               IF(LMAX .EQ. 1) THEN
                  NUMMAX = 1
               ELSE
                  NUMMAX = LMAX + 2
               END IF
 
         END SELECT
C
C          SET UP FLAGS TO HANDLE EDIT TITLE BLOCK
C
         GET_KEYBOARD = .TRUE.
         IF(TITLES_BLOCK) THEN                            !Edit Title
            GET_KEYBOARD = .FALSE.
            SHOW_DATA = .FALSE.
            IRET = 99
            NFLAG = 0
         END IF
 
C
C          BRANCH ON ILAST AS A FLAG FOR SHOWING THE TITLE LINES
C
         IF(IO .EQ. 0 .AND. .NOT. TITLES_BLOCK) THEN
 
            IF(ILAST .NE. IREAD) THEN
               SHOW_DATA = .TRUE.
 
               CALL GOTOXY(0,3)
               IF(IFIRST .NE. 0) THEN
                  CALL GOTOXY(12,NUMSEL+4+i_offset)
                  CALL LIGHT(51,6)
  !               CALL WINDOW(NUMSEL+3, NUMSEL+5, 60, 72, 32, 2)
                  CALL WINDOW(5, 19, 4, 75, 32, 0)
                  ICUR = 0
               END IF
 
               IFIRST = 1
               ILAST = IREAD
 
            ELSE
               SHOW_DATA = .FALSE.
 
   !           CALL GOTOXY(5,2)
   !           WRITE(IO,'('' ''\)')
 
               M = N + 4 + i_offset
               CALL GOTOXY(40,M)
 
               DIFF = ROUND(INTCOD(N)+1)
 
               VALUE = VAL(N) + DIFF
               IF(VAL(N) .LT. 0.0) VALUE = VAL(N) - DIFF
               IF(INTCOD(N) .NE. 0) THEN
                  IF(VAL(N) .NE. -99.0) CALL PRINT(VALUE,11,INTCOD(N))
               ELSE
                  IF(VAL(N) .NE. -99.0) CALL PRINT(NFIX(VALUE),11,-1)
               END IF
 
            END IF
 
         END IF
C
C          SHOW DATA
C
         IF(IO .NE. 0) SHOW_DATA = .TRUE.
 
         IF(SHOW_DATA) THEN
            N = 1
            DO WHILE (N .LE. NUMMAX)
               LL = N + 4 + i_offset
               CALL GOTOXY(13,LL)
 
               K = N
 
               DIFF = ROUND(INTCOD(N)+1)
               VALUE = VAL(N) + DIFF                       !Correct for
               IF(VAL(N) .LT. 0.0) VALUE = VAL(N) - DIFF   !roundoff
 
               IF(N .EQ. NUMMAX) THEN
                  IF(ITITL .EQ. 3 .OR. ITITL .EQ. 12) K = NUM1
                  IF(ITITL .EQ. 3 .AND. VAL(INUM1) .EQ. 1.0) THEN
                     N = N + 1
                     CYCLE
                  END IF
 
               END IF
 
C
C          SHOW LINE WITHOUT A DATA VALUE (VAL = -99)
C
               IF(.NOT. TEST(K)) THEN
                  IF(IO .NE. 0) THEN
                     IF(IREAD .EQ. 1) THEN
                           IF(VAL(K) .EQ. -99.0) THEN
                              WRITE(IO,801) (TITL1(J,K),J=1,27)
                           ELSE
                              WRITE(IO,803) (TITL1(J,K),J=1,27),
     .                                      NFIX(VAL(K))
                           END IF
 
                     ELSE
                           IF(VAL(K) .EQ. -99.0) THEN
                              WRITE(IO,801) (TITL2(J,K),J=1,27)
                           ELSE
                              WRITE(IO,803) (TITL2(J,K),J=1,27),
     .                                      NFIX(VAL(K))
                           END IF
 
                     END IF
 
                  ELSE
                     IF(IREAD.EQ.1) CALL PRINT(TITL1(1,K),27,-2)
                     IF(IREAD.EQ.2) CALL PRINT(TITL2(1,K),27,-2)
                        IF(VAL(K) .NE. -99.0) THEN
                              CALL PRINT(NFIX(VALUE),19,-1)
                        END IF
 
                  END IF
 
 
               ELSE
C
C          SHOW LINE WITH A DATA VALUE
C
 
                  CALL GOTOXY(13,LL)
                  MM = LINE(N)
 
                  IF(IO .NE. 0) THEN
                     IF(IREAD .EQ. 1) THEN
                        SELECT CASE (INTCOD(N))
                           CASE (0)                        !Integer
                              WRITE(IO,900) (TITL1(J,N),J=1,27),
     .                                      NFIX(VAL(N))
 
                           CASE (1)                        !1 decimal place
                              WRITE(IO,901) (TITL1(J,N),J=1,27), VAL(N)
 
                           CASE (2)                        !2 decimal places
                              WRITE(IO,902) (TITL1(J,N),J=1,27), VAL(N)
 
                           CASE (3)                        !3 decimal places
                              WRITE(IO,903) (TITL1(J,N),J=1,27), VAL(N)
 
                           CASE (4)                        !4 decimal places
                              WRITE(IO,904) (TITL1(J,N),J=1,27), VAL(N)
 
                           CASE (5)                        !5 decimal places
                              WRITE(IO,905) (TITL1(J,N),J=1,27), VAL(N)
 
                        END SELECT
 
                     ELSE
 
                        SELECT CASE (INTCOD(N))
                           CASE (0)                        !Integer
                              WRITE(IO,900) (TITL2(J,N),J=1,27),
     .                                                      NFIX(VAL(N))
 
                           CASE (1)                        !1 decimal place
                              WRITE(IO,901) (TITL2(J,N),J=1,27), VAL(N)
 
                           CASE (2)                        !2 decimal places
                              WRITE(IO,902) (TITL2(J,N),J=1,27), VAL(N)
 
                           CASE (3)                        !3 decimal places
                              WRITE(IO,903) (TITL2(J,N),J=1,27), VAL(N)
 
                           CASE (4)                        !4 decimal places
                              WRITE(IO,904) (TITL2(J,N),J=1,27), VAL(N)
 
                           CASE (5)                        !5 decimal places
                              WRITE(IO,905) (TITL2(J,N),J=1,27), VAL(N)
 
                        END SELECT
 
                     END IF
 
                  ELSE
 
                     IF(IREAD .EQ. 1) THEN
                        CALL PRINT(TITL1(1,N),27,-2)
 
                     ELSE
                        CALL PRINT(TITL2(1,N),27,-2)
                     END IF
 
                     IF(INTCOD(N) .NE. 0) THEN
                        CALL PRINT(VALUE,11,INTCOD(N))
                     ELSE
                        CALL PRINT(NFIX(VALUE),11,-1)
                     END IF
 
                  END IF
 
               END IF
 
               N = N + 1
 
            END DO
C
C          CHECK ON SPECIAL VALUES OF ICODET
C
            IF(IO .EQ. 0) THEN
               IF(ICODET .GT. 0) CALL SHWSTR(ICODET, NUMMAX)
 
               SELECT CASE (ICODET)
                  CASE (10)                                !Evaporator
                     CALL SHWEVP(VAL)
 
                  CASE (12)                                !Fan
                     CALL SHWFAN(VAL)
 
                  CASE (13)                                !Condenser
                     CALL SHWCND(VAL)
 
                  CASE (15, 16, 19)
                     CALL SHWPRP(ICODET, VAL)
 
                  CASE (20, 21)                            !Evap UA
                     CALL gotoxy(18, 17)
                     CALL atrbut(44, 28)
                     CALL gotoxy(18, 18)
                     CALL atrbut(44, 28)
                     CALL window(16, 19, 17, 63, 32, 2)
                     CALL gotoxy(19, 17)
                     CALL print('Press <F4> to Specify Heat ', 27,-2)
                     CALL print('Exchanger Area', 14,-2)
                     CALL gotoxy(19, 18)
                     CALL print('and U-Values for Each Heat ', 27,-2)
                     CALL print('Transfer Regime', 15,-2)
 
                  CASE (24)                                !Insul thickness
                     CALL gotoxy(24, 15)
                     CALL print('Note: Enter Insulation ',23,-2)
                     CALL print('Thickness',9,-2)
                     CALL gotoxy(24, 16)
                     CALL print('Do Not Include Cabinet Liners', 29, -2)
 
                  CASE (25)                                !Mullion
                     CALL gotoxy(14, 15)
                     CALL print('Note: Enter Total Thickness, ',29,-2)
                     CALL print('Including Inner Liners', 22, -2)
                     CALL gotoxy(14, 16)
                     CALL print('      Resistivity Should ',25,-2)
                     CALL print('Account for the Liners',22,-2)
 
                  CASE (26)                                !Refrigerant change
                     CALL gotoxy(14, 13)
                     CALL print('NOTE: Compressor Design ',24,-2)
                     CALL print('Being Changed for New Fluid.',28,-2)
                     CALL gotoxy(14, 15)
                     CALL print('      Suggested Displacement ',29,-2)
                     CALL print('Multiplier is Shown.', 20, -2)
                     CALL gotoxy(14, 16)
                     CALL print('      Capacity and Displa',25,-2)
                     CALL print('cement are Both Adjusted.',25,-2)
                     CALL gotoxy(14, 17)
                     CALL print('      See the Help Menu <F1> ',29,-2)
                     CALL print('for More Details.',17,-2)
 
               END SELECT
            END IF
 
C
C          CHECK FOR PRINT ONLY
C
            IF(IO .EQ. 2 .OR. IO .EQ. 3) RETURN
C
C          HIGHLIGHT FIRST LINE.  SET FLAGS TO GO TO FIRST LINE
C
            NUMSEL = NINE(1)
            NEWSEL = NUMSEL
            IRET = 3
            GET_KEYBOARD = .FALSE.
 
  !         CALL GOTOXY(12, NUMSEL+4)
            CALL GOTOXY(42, NUMSEL+4+i_offset)
  !         CALL LIGHT(51, 3)
            CALL ATRBUT(10, 79)
 
 
         END IF
C
C          GET KEYBOARD INPUT
C
         NOT_DONE_YET = .TRUE.
 
         DO WHILE(NOT_DONE_YET)
         IN_ERROR = .FALSE.
 
            IF(GET_KEYBOARD) THEN
               CALL CURSOR(0)
  !            CALL GOTOXY(58,23)
               CALL GOTOXY(58,NUMSEL+4+i_offset)
               CALL LIGHT(66,3)
  !            CALL WINDOW (NewSEL+3, NewSEL+5, 60, 72, 32, 2)
               CALL GOTOXY(58,NUMSEL+4+i_offset)
 
               IF(ICUR .EQ. 0) THEN
                  call gotoxy(53, numsel+4+i_offset)
                  call print('<--- $',5, -2)
                  CALL GOTOXY(58,NUMSEL+4+i_offset)
                  call setatr(62)
                  call atrbut(9,62)
                  call gotoxy(58, numsel+4+i_offset)
  !               CALL GETEDT(KEY,RVAL,IRET,INTCOD(N))
                  CALL GETEDT(KEY,RVAL,IRET,INTCOD(numsel))
                  call setatr(30)
                  CALL CURSOR(1)
 
                  IF(IRET .NE. 7 .AND. IRET .NE. 8) THEN
                     CALL GOTOXY(4,21)   !!!
                     CALL LIGHT(75,2)
                  END IF
 
               END IF
 
               IF(IRET .NE. 11 .AND. IESC .NE. 0) THEN
                  IESC = 0
                  CALL GOTOXY(4,21)   !!!
                  CALL LIGHT(75,2)
               END IF
 
            END IF
C
C          BRANCH ON THE IRET CODE
C
            GET_KEYBOARD = .TRUE.
            LEAVE = .FALSE.
            SELECT CASE (IRET)
               CASE (0)                                    !Invalid
                  CYCLE
 
               CASE (1)                                    !<Esc> & data
  !               CALL GOTOXY(58,23)
                  CALL GOTOXY(58,NUMSEL+4+i_offset)
                  CALL LIGHT(68,2)
                  CYCLE
 
               CASE (2)                                    !Error
  !               CALL WARBLE
  !               CALL GOTOXY(58,23)
                  CALL GOTOXY(58,NUMSEL+4+i_offset)
                  CALL LIGHT(68,2)
                  CYCLE
 
               CASE (3, 4, 5, 16, 17)
                  IF(IRET .EQ. 17) THEN                    !<End>
                     ICUR = 0
                     L = LMAX
                     NEWSEL = NINE(L)
 
                  ELSE IF(IRET .EQ. 3 .OR. IRET .EQ. 16) THEN
                     ICUR = 0
                     L = 1
                     NEWSEL = NINE(L)
 
                  ELSE                                     !Cursor
                     NEWSEL = LINE(NUMSEL)
                     IF(IRET .EQ. 4) NEWSEL = NEWSEL - 1
                     IF(IRET .EQ. 5 )NEWSEL = NEWSEL + 1
                     IF(NEWSEL .GT. LMAX) NEWSEL = 1
                     IF(NEWSEL .EQ. 0) NEWSEL = LMAX
                     NEWSEL = NINE(NEWSEL)
                     ICUR = 0
 
                  END IF
 
                  IF(NEWSEL .EQ. NUMSEL) CYCLE
 
  !               CALL GOTOXY(12,NUMSEL+4)                 !Move
                  CALL GOTOXY(42,NUMSEL+4+i_offset)        !Move
                  CALL LIGHT(51,6)                         !highlight
  !               CALL GOTOXY(58,NUMSEL+4)
                  CALL GOTOXY(53,NUMSEL+4+i_offset)
                  CALL LIGHT(67,2)
                  CALL GOTOXY(53,NUMSEL+4+i_offset)
                  CALL LIGHT(67,6)
 
  !               CALL GOTOXY(12,NEWSEL+4)
                  CALL GOTOXY(42,NEWSEL+4+i_offset)
  !               CALL LIGHT(51,3)
                  call atrbut(10, 79)
 
                  NOT_DONE_YET = .FALSE.
 
                  NUMSEL = NEWSEL
                  N = NUMSEL
                  CYCLE
 
               CASE (7)                                    !<F1>
                  !Handle Help Outside Select Case Loop
 
               CASE (8)                                    !<F9>
                  call help(6)
                  cycle
 
               CASE (9)                                    !<PgUp>
                  IFLAG = 2
                  LEAVE = .TRUE.
                  !Finish Outside Select Case Loop
 
               CASE (10)                                   !<PgDn>
                  LEAVE = .TRUE.
                  !Finish Outside Select Case Loop
 
               CASE (12)                                   !<F5>
                  SELECT CASE (ICODET)
                     CASE (-1)
                        CALL setatr(79)
                        CALL window(12, 18, 16, 63, 32, 0)
                        CALL window(13, 17, 20, 59, 32, 1)
 
                        CALL setatr(01)
                        CALL window(19, 19, 18, 65, 219, 3)
                        CALL window(13, 18, 64, 65, 219, 3)
                        CALL setatr(0)
                        CALL setatr(79)
 
                        CALL gotoxy(29,15)
                        CALL print('Fractional Coverage: $', 21, -2)
 
                        CALL atrbut(5, 79)
                        CALL gotoxy(50,15)
                        CALL cursor(0)
 
                        valid_entry = .FALSE.
                        DO WHILE (.NOT. valid_entry)
                           CALL gotoxy(50,15)
                           CALL print('     ',5,-2)
                           CALL gotoxy(50,15)
                           rval = 1.0
                           CALL GETEDT(KEY, RVAL, IRET, 2)
                           IF(rval .ge. 0.0 .and .rval .le. 1.0) THEN
                              valid_entry = .true.
                              CYCLE
                           END IF
                           CALL warble
 
                        END DO
 
                        f_cov = rval
 
                        CALL cursor(1)
                        CALL setatr(0)
                        CALL setatr(30)
 
                        IF(iret .eq. 6) then
                           b = (val(1) +val(2))/2.0
                           c = (1.0 - f_cov)*val(1)*val(2)/4.0
                           val(3) = b/2.0 - sqrt(b*b - 4*c)/2.0
                           val(4) = val(3)
                           val(5) = val(3)
                           val(6) = val(3)
                        END IF
 
                     CASE (0)                              !Nothing
                        CYCLE
 
                     CASE (1)                              !Freezer
                        IF(NUMSEL .EQ. 1 .OR. NUMSEL .EQ. 6) CYCLE
 
                     CASE (2)                              !Freezer
                        IF(NUMSEL .EQ. 1 .OR. NUMSEL .EQ. 7) CYCLE
 
                     CASE (3)                              !Freezer
                        IF(NUMSEL .EQ. 1 .OR. NUMSEL .EQ. 4) CYCLE
 
                     CASE (4)                              !Fresh food
                        IF(NUMSEL .EQ. 1 .OR. NUMSEL .EQ. 6) CYCLE
 
                     CASE (5)                              !Fresh food
                        IF(NUMSEL .EQ. 1 .OR. NUMSEL .EQ. 7) CYCLE
 
                     CASE (8)                              !Membrane
                        IF(NUMSEL .NE. 4 .AND. NUMSEL .NE. 9) CYCLE
 
                     CASE (9)                              !Membrane
                        IF(NUMSEL .NE. 3) CYCLE
 
                     CASE (10)                             !Evap U's
                        !Nothing
 
                     CASE (11, 14)                         !Fan Analysis
                        !Nothing
 
                     CASE (12)                             !Fan Results
                        !Nothing
 
                     CASE (13)                             !Cond U's
                        !Nothing
 
                     CASE (15)                             !Binary fluid
                        IREF1 = NFIX(VAL(1))
                        IREF2 = NFIX(VAL(3))
                        IREF3 = 0
                        CALL FIJ(IREF1, IREF2, IREF3, F12, F13, F23,
     .                           IFF1, IFF2, IFF3)
 
                        VAL(4) = F12
                        CALL GETXY(ICOLS, IROWS)
                        CALL GOTOXY(44,9)
                        IF(F12 .GE. 0.0) THEN
                           CALL PRINT(F12+0.0005,07,3)
                        ELSE
                           CALL PRINT(F12-0.0005,07,3)
                        END IF
                        IF (IFF1 .EQ. 0) CALL PRINT ('  (EST)', 7, -2)
                        CALL GOTOXY(ICOLS,IROWS)
                        CYCLE
 
                     CASE (16)                             !Ternary fluid
                        IREF1 = NFIX(VAL(1))
                        IREF2 = NFIX(VAL(3))
                        IREF3 = NFIX(VAL(5))
                        CALL FIJ(IREF1, IREF2, IREF3, F12, F13, F23,
     .                           IFF1, IFF2, IFF3)
 
                        VAL(7) = F12
                        VAL(8) = F13
                        VAL(9) = F23
                        CALL GETXY(ICOLS, IROWS)
                        CALL GOTOXY(44,12)
                        IF(F12 .GE. 0.0) THEN
                           CALL PRINT(F12+0.0005,07,3)
                        ELSE
                           CALL PRINT(F12-0.0005,07,3)
                        END IF
                        IF (IFF1 .EQ. 0) CALL PRINT ('  (EST)', 7, -2)
 
                        CALL GOTOXY(44,13)
                        IF(F13 .GE. 0.0) THEN
                           CALL PRINT(F13+0.0005,07,3)
                        ELSE
                           CALL PRINT(F13-0.0005,07,3)
                        END IF
                        IF (IFF2 .EQ. 0) CALL PRINT ('  (EST)', 7, -2)
 
                        CALL GOTOXY(44,14)
                        IF(F23 .GE. 0.0) THEN
                          CALL PRINT(F23+0.0005,07,3)
                        ELSE
                          CALL PRINT(F23-0.0005,07,3)
                        END IF
                        IF (IFF3 .EQ. 0) CALL PRINT ('  (EST)', 7, -2)
 
                        CALL GOTOXY(ICOLS,IROWS)
                        CYCLE
 
                     CASE (17, 18)
                        !Nothing
 
                     CASE (19, 20, 21)
                        CYCLE
 
                     CASE (23)                             !DOE Product Class
                        CALL help(10)
                        CYCLE
 
                  END SELECT
 
                  IFLAG = 3 + NUMSEL
 
                  LEAVE = .TRUE.
                  !Finish Outside Select Case Loop
 
               CASE (62)                                   !<F4>
                   if(icodet .eq. -1) then
                        CALL setatr(79)
                        CALL window(12, 18, 16, 63, 32, 0)
                        CALL window(13, 17, 20, 59, 32, 1)
 
                        CALL setatr(01)
                        CALL window(19, 19, 18, 65, 219, 3)
                        CALL window(13, 18, 64, 65, 219, 3)
                        CALL setatr(0)
                        CALL setatr(79)
 
                        CALL gotoxy(29,15)
                        CALL print('Panel Thickness: $', 17, -2)
 
                        CALL atrbut(5, 79)
                        CALL gotoxy(46,15)
                        CALL cursor(0)
 
                        valid_entry = .FALSE.
                        DO WHILE (.NOT. valid_entry)
                           CALL gotoxy(46,15)
                           CALL print('     ',5,-2)
                           CALL gotoxy(46,15)
                           rval = 2.54
                           CALL GETEDT(KEY, RVAL, IRET, 2)
                           IF(rval .gt. 0.0 .and. rval .le. val(7)) THEN
                              valid_entry = .true.
                              CYCLE
                           END IF
                           CALL warble
 
                        END DO
 
                        panel_thickness = rval
 
                        CALL cursor(1)
                        CALL setatr(0)
                        CALL setatr(30)
 
                        IF(iret .eq. 6) then
                           val(8) = 0
                           val(9) = (val(7) - rval)/val(7)
                        END IF
 
                      iflag = 4
                      LEAVE = .TRUE.
                      !Finish Outside Select Case Loop
                   END IF
 
                   IF(icodet .EQ. 15 .OR. icodet .EQ. 16
     .                               .OR. icodet .EQ. 19) THEN
                      CALL help(9)
                   END IF
 
                   IF(icodet .EQ. 10 .OR. icodet .EQ. 13
     .                               .OR. icodet .EQ. 20
     .                               .OR. icodet .EQ. 21) THEN
                      IFLAG = -4
                      LEAVE = .TRUE.
                      !Finish Outside Select Case Loop
                   ELSE
                      if(icodet .ne. -1) CYCLE
                   END IF
 
               CASE (99)                                   !Edit titles
                  !Handle Outside Select Case Loop
 
               CASE DEFAULT
                  IF(IRET .EQ. 11) THEN
                     IF(IESC .EQ. 0) THEN
                        IFLAG = 3
                        LEAVE = .TRUE.
                        !Finish Outside Select Case Loop
                     ELSE
                        IN_ERROR = .FALSE.
                     END IF
 
                  ELSE                                     !Read data and
                                                           !watch for -99
                     N = NUMSEL
                     NEWVAL = RVAL
                     IF(NEWVAL .EQ. -99.0) NEWVAL = -99.0001
 
                     IF(NEWVAL .LT. VALMN(N) .OR.
     .                  NEWVAL .GT. VALMX(N)) IN_ERROR = .TRUE.
 
                  END IF
 
                  IF (ignore .EQ. 1) IN_ERROR = .FALSE.
 
                  IF(.NOT. IN_ERROR .AND. .NOT. LEAVE) THEN
                     ISAV = 0                              !Valid data
   !                 CALL GOTOXY(58,23)
   !                 CALL WINDOW(5, 19, 53, 68, 32, 0)
                     CALL WINDOW(5, nummax+4+i_offset, 53, 68, 32, 0)
                     CALL GOTOXY(58,NUMSEL+4+i_offset)
                     CALL LIGHT(68,2)
 
                     VAL(N) = NEWVAL
                     IESC = 0
 
                     IF(IRET .EQ. 14 .OR. IRET .EQ. 15) THEN
                        ICUR = 1
                        IRET = IRET - 10
                     END IF
 
                     IF(IRET .EQ. 18 .OR. IRET .EQ. 19) THEN
                        ICUR = 2
                        IRET = IRET - 2
                     END IF
 
                     SELECT CASE (ITITL_EDT)
                        CASE (3, 5, 8, 12)
                          IF(N .EQ. INUM1 .AND. VAL(N) .NE. NEWVAL)
     .                       ILAST = 0
 
                        CASE (10)
                           IF(VAL(1) .NE. VALSV1) LEAVE = .TRUE.
 
                        CASE (11)
                           IF(VAL(9) .NE. VALSV2) LEAVE = .TRUE.
 
                        CASE (13)
                           IF(VAL(5) .NE. VALSV3) LEAVE = .TRUE.
 
                        CASE (14)
                           IF(VAL(8) .NE. VALSV4) LEAVE = .TRUE.
 
                        CASE (15)
                           IF(nfix(VAL(1)) .EQ. 0) LEAVE = .TRUE.
 
                        CASE (16)
                           IF(nfix(VAL(1)) .NE. VALSV1 .AND.
     .                       (VALSV1. EQ. 4 .OR. VAL(1) .EQ. 4))
     .                       LEAVE = .TRUE.
 
                     END SELECT
 
                     SELECT CASE (ICODET)
                        CASE (10)                          !Evaporator
                           CALL SHWEVP(VAL)
 
                        CASE (12)                          !Fan
                           CALL SHWFAN(VAL)
 
                        CASE (13)                          !Condenser
                           CALL SHWCND(VAL)
 
                        CASE (15, 16, 19)
                           CALL SHWPRP(ICODET, VAL)
 
                     END SELECT
 
                     IF(INUM2_EDT .LT. 0 .AND. VAL(1) .NE. VALSV1)
     .                  LEAVE = .TRUE.
 
                     NOT_DONE_YET = .FALSE.
 
                     IF (LEAVE) CYCLE
 
                  ELSE IF(.NOT. LEAVE) THEN                !Error in data
 
  !                  CALL WARBLE
 
C
C          DETERMINE LEN PARAMETERS FOR THE ERROR MESSAGE WRITEOUT
C
                     DIFF = ROUND(INTCOD(N)+1)
                     IDLEN = INTCOD(N) + 2
                     IF(NEWVAL .NE. 0.0) THEN
                        LEN0 = ALOG10(ABS(NEWVAL)) + IDLEN
                     END IF
                     IF(ABS(NEWVAL) .LT. 1.0) LEN0 = IDLEN
                     IF(NEWVAL .EQ. 0) LEN0 = 3
                     IF(NEWVAL .LT. 0.0) LEN0 = LEN0 + 1
 
                     IF(VALMN(N) .NE. 0.0) THEN
                        LEN1 = ALOG10(ABS(VALMN(N))) + IDLEN
                     END IF
                     IF(ABS(VALMN(N)) .LT. 1.0) LEN1 = IDLEN
                     IF(VALMN(N) .EQ. 0) LEN1 = 3
                     IF(VALMN(N) .LT. 0.0) LEN1 = LEN1 + 1
 
                     IF(VALMX(N) .NE. 0.0) THEN
                        LEN2 = ALOG10(ABS(VALMX(N))) + IDLEN
                     END IF
                     IF(ABS(VALMX(N)) .LT. 1.0) LEN2 = IDLEN
                     IF(VALMX(N) .EQ. 0) LEN2 = 3
                     IF(VALMX(N) .LT. 0.0) LEN2 = LEN2 + 1
 
C
C          ADJUST LEN? VALUES FOR AN INTEGER
C
                     IF(INTCOD(N) .EQ. 0) THEN
                        LEN0 = LEN0 - 1
                        LEN1 = LEN1 - 1
                        LEN2 = LEN2 - 1
                        IF(LEN1 .LE. 0) LEN1 = 1
                        IF(LEN0 .LE. 0) LEN0 = 1
                        IF(LEN2 .LE. 0) LEN2 = 1
                     END IF
C
C          BRANCH ON THE LIMITS INDEX IOK
C
                     IF(IOK(N) .EQ. 1 .OR.
     .                  (IOK(N) .EQ. 2 .AND. NEWVAL .LT. VALMN(N))) THEN
 
                        IF(IOK(N) .EQ. 2) LEN2 = 0
                        ICOL = 53 - LEN1 - LEN2
                        ICOL = ICOL/2
                        CALL GOTOXY(ICOL,21)  !!!
 
                        IF(IOK(N).EQ.1) THEN
                           CALL PRINT('Value Must be Between $',23,-2)
                        ELSE
                           CALL PRINT('Value Must be at Least $',24,-2)
                        END IF
 
                        VALUE = VALMN(N) + DIFF
                        IF(VALMN(N) .LE. 0.0) VALUE = VALMN(N) - DIFF
 
                        IF(INTCOD(N) .NE. 0) THEN
                           IF(LEN1 .EQ. 3 .AND. INTCOD(N) .NE. 1) THEN
                              CALL PRINT(0.0,3,1)
                           ELSE
                              CALL PRINT(VALUE,LEN1,INTCOD(N))
                           END IF
                        ELSE
                           CALL PRINT(NFIX(VALUE),LEN1,-1)
                        END IF
 
                        IF(IOK(N) .NE. 2) THEN
                           CALL PRINT(' and ',5,-2)
                           VALUE = VALMX(N) + DIFF
                           IF(INTCOD(N) .NE. 0) THEN
                             IF(LEN2 .EQ. 3 .AND. INTCOD(N) .NE. 1) THEN
                                CALL PRINT(0.0,3,1)
                             ELSE
                                CALL PRINT(VALUE,LEN2,INTCOD(N))
                             END IF
                           ELSE
                              CALL PRINT(NFIX(VALUE),LEN2,-1)
                           END IF
                        END IF
 
                        CALL GETXY(ICOLE, IROWE)
                        ILEN = ICOLE - ICOL + 2
                        CALL GOTOXY(ICOL-1, 21)  !!!
                        CALL ATRBUT(ILEN, 28)
  !                     CALL GOTOXY(4,20)
  !                     CALL ATRBUT(72,62)
 
  !                     CALL GOTOXY(58,23)
                        CALL GOTOXY(58,NUMSEL+4+i_offset)
                        CALL LIGHT(68,2)
 
                        CYCLE
 
                     ELSE
C
C          ISSUE WARNING AND ACCEPT VALUE IF <ESC> PRESSED
C
                        ICOL = 35 - LEN0 - LEN1 - LEN2
                        ICOL = ICOL/2
 
                        CALL GOTOXY(ICOL,21)   !!!
                        CALL PRINT('Value ($',8,-2)
 
                        VALUE = NEWVAL + DIFF
                        IF(NEWVAL .LT. 0.0) VALUE = VALUE - DIFF
 
                        IF(INTCOD(N) .NE. 0) THEN
                           IF(LEN0 .EQ. 3 .AND. INTCOD(N) .NE. 1) THEN
                              CALL PRINT(0.0,3,1)
                           ELSE
                              CALL PRINT(VALUE,LEN0,INTCOD(N))
                           END IF
                        ELSE
                           CALL PRINT(NFIX(VALUE),LEN0,-1)
                        END IF
                        CALL PRINT(') Outside $',11,-2)
 
                        VALUE = VALMN(N) + DIFF
                        IF(VALMN(N) .LE. 0.0) VALUE = VALMN(N) - DIFF
 
                        IF(INTCOD(N) .NE. 0) THEN
                           IF(LEN1 .EQ. 3 .AND. INTCOD(N) .NE. 1) THEN
                              CALL PRINT(0.0,3,1)
                           ELSE
                              CALL PRINT(VALUE,LEN1,INTCOD(N))
                           END IF
                        ELSE
                           CALL PRINT(NFIX(VALUE),LEN1,-1)
                        END IF
                        CALL PRINT(' to ',4,-2)
 
                        VALUE = VALMX(N) + DIFF
                        IF(INTCOD(N) .NE. 0) THEN
                           IF(LEN2 .EQ. 3 .AND. INTCOD(N) .NE. 1) THEN
                              CALL PRINT(0.0,3,1)
                           ELSE
                              CALL PRINT(VALUE,LEN2,INTCOD(N))
                           END IF
                        ELSE
                           CALL PRINT(NFIX(VALUE),LEN2,-1)
                        END IF
 
                        CALL PRINT('   Press <Esc> to Accept$',25,-2)
 
                        CALL GETXY(ICOLE, IROWE)
                        ILEN = ICOLE - ICOL + 2
                        CALL GOTOXY(ICOL-1, 21)  !!!
                        CALL ATRBUT(ILEN, 28)
 
  !                     CALL GOTOXY(4,20)
  !                     CALL ATRBUT(72,62)
 
  !                     CALL GOTOXY(58,23)
                        CALL GOTOXY(58,NUMSEL+4+i_offset)
                        CALL LIGHT(68,2)
 
                        IESC = 1
 
                        CYCLE
 
                     END IF
 
                  END IF                                   !Case Default
 
            END SELECT
C
C          EDIT THE RUN TITLE
C
            IF(IRET .EQ. 99) THEN
               EDITING_TITLE = .TRUE.
               DO WHILE (EDITING_TITLE)
                  CALL GETTIT(NFLAG,TITLE)
 
                  SELECT CASE (NFLAG)
                     CASE (0)                              !Finished Edit
                        IFLAG = 0
                        LEAVE = .TRUE.
                        EDITING_TITLE = .FALSE.
                        CYCLE
 
                     CASE (1, 3)                           !Call Help
                        EDITING_TITLE = .FALSE.
 
                     CASE (2)                              !<PgUp>
                        IFLAG = 2
                        LEAVE = .TRUE.
                        EDITING_TITLE = .FALSE.
                        CYCLE
 
                     CASE (4)                              !Show new time
 
                  END SELECT
 
               END DO
 
            END IF
C
C          FINISHED SELECT CASE LOOP.  LOOK FOR LEAVE = .TRUE.
C
            IF(LEAVE) THEN
               NOT_DONE_YET = .FALSE.
               CYCLE
            END IF
C
C          HANDLE HELP MENU
C
            IF(IRET .EQ. 7 .OR. IRET .EQ. 99) THEN
               IF(IHELP .NE. 1 .AND. FILNAM .NE. FILLST) THEN
                  IHELP = 1
 
                  CALL CURSOR(1)
                  FILLST = FILNAM
 
                  CALL GETXY(ICOLS,IROWS)
 
                  IF(IOPEN .NE. 0) THEN
                     OPEN(IDISK,FILE=FILNAM,FORM='BINARY')
                     SELECT CASE (ITITL_EDT)
                        CASE (1, 3, 5, 8, 12, 16)
                           READ(IDISK) (TITL1(K,1),K=1,KMAX1)
 
                        CASE DEFAULT
                           READ(IDISK) (TITL1(K,1),K=1,KMAX1),
     .                                 (TITL2(K,1),K=1,KMAX2)
 
                     END SELECT
 
                  END IF
 
                  CALL HELP(1)
                  IF(IOPEN .EQ. 1) CLOSE(IDISK)
 
   !              CALL GOTOXY(74,1)
   !              CALL LIGHT(78,2)
                  CALL GOTOXY(ICOLS,IROWS)
               END IF
 
               CALL HELP(2)
               IF(IRET .EQ. 7) THEN
   !              CALL GOTOXY(58,23)
                  CALL GOTOXY(58,NUMSEL+4+i_offset)
                  IF(NUM1 .NE. 0) CALL LIGHT(68,2)
               ELSE
                  GET_KEYBOARD = .FALSE.
                  NFLAG = 1
               END IF
 
            END IF
 
         END DO                                            !NOT_DONE_YET
C
C          RETURN TO CALLER.  CLOSE IDISK FIRST
C
         IF (.NOT. LEAVE) CYCLE
 
         FILRED = FILNAM
         IF(IOPEN .EQ. 0) CLOSE(IDISK)
         RETURN
      END DO
C
C          FORMAT STATEMENTS
C
  900 FORMAT(13X,27A1,10X,I9)
  901 FORMAT(13X,27A1,10X,F9.1)
  902 FORMAT(13X,27A1,10X,F9.2)
  903 FORMAT(13X,27A1,10X,F9.3)
  904 FORMAT(13X,27A1,10X,F9.4)
  905 FORMAT(13X,27A1,10X,F9.5)
  801 FORMAT(13X,27A1)
  802 FORMAT(13X,27A1,16X,'N/A')
  803 FORMAT(13X,27A1,10X,I9)
      END
C
      SUBROUTINE GETEDT(KEY,RVAL,IRET,INTCOD)
C     ******************************************************************
C     *    READ THE KEYBOARD FOR A LETTER DESIGNATION OF THE LINE OR   *
C     *    FOR THE VALUE OF THE PARAMETER ON THE LINE                  *
C     *                                                                *
C     *    IRET = 0 IF NO ENTRY (CARRIAGE RETURN)                      *
C     *         = 1 IF <ESC> KEY PRESSED AND A DATA VALUE IS ENTERED   *
C     *         = 2 IF AN ERROR ENCOUNTERED IN THE VALUE               *
C     *         = 3 IF A LETTER SELECTION OF THE LINE                  *
C     *         = 4 IF UP-CURSOR AND 5 IF DOWN-CURSOR                  *
C     *         = 6 IF VALID REAL NUMBER                               *
C     *         = 7 IF FUNCTION KEY F1                                 *
C     *         = 8 IF FUNCTION KEY F9                                 *
C     *         = 9 IF <PgUp> KEY                                      *
C     *         = 10 IF <PgDn> KEY                                     *
C     *         = 11 IF <Esc> KEY PRESSED AND NO DATA VALUE ENTERED    *
C     *         = 12 IF FUNCTION KEY F5                                *
C     *         = 14 IF UP-CURSOR AND 15 IF DOWN-CURSOR AND VALID NUM  *
C     *         = 17 IF <End> KEY                                      *
C     *         = 18 IF <Home> KEY AND 19 IF <End> KEY  AND VALID NUM  *
C     *         = 62 IF FUNCTION KEY F4                                *
C     ******************************************************************
      LOGICAL ERROR, A_BAD_READ
 
      CHARACTER*10 VAR1,SPACE
      CHARACTER SVAR,KEY,BS,BLANK,zero
 
      DIMENSION SVAR(10)
 
      EQUIVALENCE (VAR1,SVAR(1))
 
      DATA SPACE/'          '/,BLANK/' '/,zero/'0'/
C
C          SET IRET TO 0 TO INDICATE NO ENTRY
C
      IRET = 0
      IDEC = 0
      VAR1 = SPACE
      BS = CHAR(8)
      L = 0
 
      A_BAD_READ = .FALSE.
 
      DO WHILE (.NOT. A_BAD_READ)
         L = L + 1
         IF(L .EQ. 9) THEN                                !Full 8 characters
            J = 28
         ELSE
            CALL INCHR(1,J,KEY)
         END IF
C
C          HANDLE NUM_LOCK CASE
C
         IASCII = ICHAR(KEY)
         IF(IASCII .EQ. 46 .OR.
     .     (IASCII .GE. 48 .AND. IASCII .LE. 57)) J = 0
         IF(L .EQ. 9) J = 28
 
         ERROR = .FALSE.
 
         SELECT CASE (J)
            CASE (1)                                       !<Esc>
               IRET = 1
               IF(L .EQ. 1) IRET = 11
               RETURN
 
            CASE (28)                                      !<Enter>
               IF(IRET .EQ. 0) RETURN
 
               READ(VAR1,801,IOSTAT=IOCHECK) RVAL
               IF(IOCHECK .NE. 0) THEN
                  A_BAD_READ = .TRUE.
                  CYCLE
               END IF
 
               RETURN
 
            CASE (71, 79)
               IF(J .EQ. 71) THEN                          !<Home>
                  KEY = CHAR(65)
                  IF(IRET .EQ. 0) THEN
                     IRET = 3
                     RETURN
                  END IF
 
               ELSE                                        !<End>
                  IF(IRET .EQ. 0) THEN
                     IRET = 17
                     RETURN
                  END IF
               END IF
 
               IRET = 18
               IF(J .EQ. 79) IRET = 19
 
               READ(VAR1,801,IOSTAT=IOCHECK) RVAL
               IF(IOCHECK .NE. 0) THEN
                  A_BAD_READ = .TRUE.
                  CYCLE
               END IF
 
               RETURN
 
            CASE (72, 80)                                  !<Up> or <Dn>
 
               IF(IRET .NE. 0) THEN                        !Valid number
                  IRET = 14
                  IF(J .EQ. 80) IRET = 15
 
                  READ(VAR1,801,IOSTAT=IOCHECK) RVAL
                  IF(IOCHECK .NE. 0) THEN
                     A_BAD_READ = .TRUE.
                     CYCLE
                  END IF
 
               ELSE
                  IRET = 4
                  IF(J .EQ. 80) IRET = 5
               END IF
               RETURN
 
            CASE (81)                                      !<PgDn>
               IF(IRET .NE. 0) THEN                        !Valid number
                  IRET = 15
 
                  READ(VAR1,801,IOSTAT=IOCHECK) RVAL
                  IF(IOCHECK .NE. 0) THEN
                     A_BAD_READ = .TRUE.
                     CYCLE
                  END IF
 
               ELSE                                        !No number
                  IRET = 10
               END IF
               RETURN
 
            CASE (73)                                      !<PgUp>
               IF(IRET .NE. 0) THEN                        !Valid number
                  IRET = 14
 
                  READ(VAR1,801,IOSTAT=IOCHECK) RVAL
                  IF(IOCHECK .NE. 0) THEN
                     A_BAD_READ = .TRUE.
                     CYCLE
                  END IF
 
               ELSE                                        !No number
                  IRET = 9
               END IF
               RETURN
 
            CASE (14, 75)                                  !Back space
               IF(IRET .EQ. 0) THEN                        !End of line
                  L = 0
                  CYCLE
 
               ELSE                                        !Something to do
 
                  WRITE(*,800) BS
                  WRITE(*,800) BLANK
                  WRITE(*,800) BS
                  L = L - 1
                  KEY = SVAR(L)
                  IF(ICHAR(KEY) .EQ. 46) IDEC = 0
                  SVAR(L) = BLANK
                  L = L - 1
                  IF(L .EQ. 0) IRET = 0
                  CYCLE
               END IF
 
            CASE (59)                                      !<F1>
               IRET=7
               RETURN
 
            CASE (67)                                      !<F9>
               IRET=8
               RETURN
 
            CASE (62)
               IRET = 62
               RETURN
 
            CASE (63)                                      !<F5>
               IF(IRET .NE. 0) THEN
                  ERROR = .TRUE.
               ELSE
                  IRET = 12
                  RETURN
               END IF
 
            CASE (57)                                      !Space bar
               KEY = ZERO
 
            CASE DEFAULT
               IASCII = ICHAR(KEY)
               IF(IASCII .GE. 97 .AND. IASCII .LE. 122) THEN
                  IASCII = IASCII - 32
               END IF
 
               SELECT CASE (IASCII)
                  CASE (65 : 90)
  !                  IF(IRET .EQ. 0) IRET = 3
  !                  IF(IRET .EQ. 3) RETURN
                     ERROR = .TRUE.
 
                  CASE (43, 45)
                     IF(IRET .NE. 0) ERROR = .TRUE.
 
                  CASE (46)
                     IF(INTCOD .NE. 0) THEN
                        IDEC = IDEC + 1
                        IF(IDEC .NE. 1) THEN
                           IDEC = IDEC - 1
                           ERROR = .TRUE.
                        END IF
                     ELSE
                        ERROR = .TRUE.
                     END IF
 
                  CASE (48 : 57)
 
                  CASE DEFAULT
                     ERROR = .TRUE.
 
            END SELECT
 
         END SELECT
C
C          CHECK VALIDITY OF ENTRY
C
         IF(ERROR) THEN
  !         CALL WARBLE
            L = L - 1
         ELSE
            IRET = 6
            SVAR(L) = KEY
            WRITE(*,800) KEY
         END IF
         CYCLE
 
      END DO
C
C          BAD READ MADE
C
      IRET = 2
      RETURN
C
C          FORMATS
C
  800 FORMAT(A1\)
  801 FORMAT(BN,F9.0)
      END
