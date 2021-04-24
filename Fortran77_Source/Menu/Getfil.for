      SUBROUTINE GETFIL(IFUN, FILERA, FILBUF, ICOD, iF5)
C     ******************************************************************
C     *         DATA FILE SELECTION MENU                               *
C     ******************************************************************
C
      LOGICAL         finding_filera, selecting_option
      CHARACTER       key, FILCOL, pth_col(32)
      CHARACTER*13    path, patc, filera, STRING, filnam, blnkfl, filbuf
      CHARACTER*32    file_path
      CHARACTER*64    doschr
      INTEGER*2       icod, locx, locy
 
      DIMENSION  STRING(200), FILCOL(13)
 
      COMMON /CHANGE/ isav, ilpt1, numdum, ibypas, numsel, ibuf, ifile
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
      COMMON /FILPTH/ i_path, file_path, length_path
 
      EQUIVALENCE (filnam,FILCOL(1)),      (file_path, pth_col(1))
 
      DATA path/'*.ERA        '/,  blnkfl/'             '/
      DATA patc/'*.TAB        '/
 
C          INITIALIZE
 
      point = 0
   10 CONTINUE
      iF5 = 0
      CALL display(1+ifun, 4, 22, 3, 76)
 
      IF (ifun .EQ. 1) THEN
         IF(i_path .EQ. 1) THEN
            CALL setatr (28)
            CALL gotoxy(6, 5)
            CALL print ('Current Path: $',14, -2)
            CALL print (pth_col, length_path - 1, -2)
            CALL gotoxy(6, 5)
            CALL atrbut(14+length_path, 28)
         END IF
 
         CALL gotoxy(56, 5)
         CALL print ('<F10>: Change Path$',18,-2)
         CALL gotoxy(56, 5)
         CALL atrbut(18, 28)
         CALL setatr (30)
      END IF
 
      IF(i_path .EQ. 1) THEN
         i = length_path + 1
         pth_col(i) = '*'
         i = i + 1
         pth_col(i) = '.'
         i = i + 1
         pth_col(i) = 'E'
         i = i + 1
         pth_col(i) = 'R'
         i = i + 1
         pth_col(i) = 'A'
         i = i + 1
         pth_col(i) = ' '
      END IF
 
      SELECT CASE (ifun)
      CASE (1)
  !   IF(ifun .EQ. 1) THEN
        CALL gotoxy(29,3)
        CALL print (' File Selection Menu $',21,-2)
        CALL gotoxy(29, 3)
        CALL atrbut(21, 79)
 
      CASE (2)
  !   ELSE IF (ifun .EQ. 2) THEN
        CALL line (4, 75, 20)
        CALL gotoxy(32,3)
        CALL print (' Save File Menu $',16,-2)
        CALL gotoxy(32, 3)
        CALL atrbut(16, 79)
 
  !   ELSE
      CASE (7)
        CALL gotoxy(22,3)
        CALL print (' Compressor Map File Selection Menu $',36,-2)
        CALL gotoxy(22, 3)
        CALL atrbut(36, 79)
 
      CASE (8)
        CALL gotoxy(16,3)
        CALL print (' Fresh Food Compressor Map File Selection Menu $',
     .                                                           47,-2)
        CALL gotoxy(16, 3)
        CALL atrbut(47, 79)
 
      CASE (9)
        CALL gotoxy(18,3)
        CALL print (' Freezer Compressor Map File Selection Menu $',
     .                                                           44,-2)
        CALL gotoxy(18, 3)
        CALL atrbut(44, 79)
 
      CASE (10)
        CALL gotoxy(26,3)
        CALL print (' Delete File Selection Menu $', 27, -2)
        CALL gotoxy(26, 3)
        CALL atrbut(27, 79)
 
      END SELECT
  !   END IF
 
      CALL cursor(1)
 
C          OUTPUT NAME OF FILE IN BUFFER
 
      IF(filbuf .NE. blnkfl) THEN
        CALL gotoxy(3,2)
        CALL print(' Buffer File: $',14,-2)
        CALL gotoxy(3,2)
        CALL atrbut(14,48)
 
        CALL gotoxy(3,3)
        CALL atrbut(14,48)
        CALL gotoxy(3,3)
        CALL print('              $',14,-2)
        CALL gotoxy(6,3)
        filnam = filbuf
 
        ipnt = 0
        DO WHILE (FILCOL(ipnt) .NE. '.')
          ipnt = ipnt + 1
          CALL gotoxy(3+ipnt, 3)
          IF( FILCOL(ipnt) .NE. '.') CALL print(FILCOL(ipnt),1,-2)
        END DO
      END IF
 
C          SEARCH FOR FILES ON DISK
 
      numera = 0
      numcol = 0
      locx = 8
      iret = 0
 
      finding_filera = .TRUE.
      DO WHILE (finding_filera)
        CALL gotoxy(27,22)
        IF(ifun .LT. 7 .OR. ifun .EQ. 10) THEN
           IF(i_path .EQ. 1) THEN
              iret = 32
              CALL MATCH(iret,file_path,filera,numera)        !Match .ERA
           ELSE
              CALL MATCH(iret,path,filera,numera)             !Match .ERA
           END IF
 
        ELSE
           CALL MATCH(iret,patc,filera,numera)                !Match .TAB
        END IF
 
        IF(iret .GT. 18) THEN                              !Disk error
          CALL warble
          CALL warble
 
          CALL gotoxy(8,21)
          CALL print('Disk read error.  Press any key $',32,-2)
          CALL print('to redefine the data file path$',31,-2)
          CALL inchr(1,j,key)
 
          call new_path (iret_path, file_path, length_path)
          IF(iret_path .EQ. 0) i_path = 1
          go to 10
 
  !       CALL inchr(1,j,key)
  !       IF(j .EQ. 1) THEN                                !<Esc> key
  !         icod=1
  !         CALL setatr(30)
  !         RETURN
  !       ELSE
  !         CALL gotoxy(4,21)
  !         CALL light(78,2)
  !         CYCLE
  !       END IF
        END IF
 
        IF(iret .NE. 0) THEN
          finding_filera = .FALSE.
          CYCLE
        END IF
 
        numera = numera + 1
        if(numera .LE. 200) STRING(numera) = filera
        IF(numera .GT. 200) then
           finding_filera = .FALSE.
           icod = 1
           CALL error (2)
           CALL gotoxy(20, 11)
           CALL PRINT('More than 200 Files (*.ERA) are Present.',40,-2)
           CALL gotoxy(20, 12)
           CALL PRINT('Delete Some Files to Free Up Space.',35,-2)
           CALL inchr(1,j,key)
           RETURN
        END IF
      END DO
 
C          END OF .ERA FILES
 
      IF(ifun .EQ. 2) THEN                                 !Save option
         CALL SAVFIL(STRING, NUMERA, FILERA, FILBUF, ICOD)
         RETURN
      END IF
                                                           !Get option
      IF(numera .EQ. 0) THEN
  !     CALL video(1)
              CALL window(8,18,5,74,32,0)
        CALL gotoxy(12,12)
        CALL warble
 
        CALL print('No data files are present.$',26,-2)
        CALL print('  Press <Esc> to return to DOS$',30,-2)
        CALL gotoxy(12,14)
        CALL print('or any other key to return to $',30,-2)
        CALL print('redefine the data path.$',24,-2)
 
        CALL INCHR(1, j, key)
        IF (j .NE. 1) THEN
           call new_path (iret_path, file_path, length_path)
           IF(iret_path .EQ. 0) i_path = 1
           go to 10
        END IF
 
        IF(icolr .EQ. 1) CALL setatr(1)
        CALL cursor(0)
        CALL exit
      END IF
 
C          FINISH OUTPUT OF THE MENU
 
      IF(numera .GT. 1) CALL SORT(13,numera,STRING)
 
      ipage = 1
      ipgmax = (numera - 1)/40 + 1
      IF (point .EQ. 0) numoff = 0
 
      DO WHILE (.TRUE.)
        maxera = 40
        IF(ipage .EQ. ipgmax) maxera = numera - 40*(ipgmax - 1)
        numcol = 0
        locx = 8
 
        DO numsel = 1, maxera
          filera =  STRING(numsel + numoff)
          locy = numsel + 7 - 10*numcol
          IF(locy .EQ. 18) THEN
            locx = locx + 18
            locy = 8
            numcol = numcol + 1
          END IF
 
          CALL gotoxy(locx,locy)
          CALL print(filera,13,-3)
        END DO
 
        filera =  STRING(1 + numoff)
 
        IF(ifun .LT. 7) THEN
           CALL gotoxy(23,21)
           CALL print(' Press <F5> to Create a New File ',33,-2)
           CALL gotoxy(23,21)
           IF(icolr .EQ. 1) CALL atrbut(33,79)
        ELSE IF(ifun .lt. 10) THEN
           CALL gotoxy(27,21)
           CALL print(' Press <F5> to View File ',25,-2)
           CALL gotoxy(27,21)
           IF(icolr .EQ. 1) CALL atrbut(25,79)
        END IF
 
        numsel=1
        IF (point .NE. 0) numsel = point
 
 !      CALL gotoxy(7,8)
 !      CALL light(16,3)
 
        numcol = (numsel - 1)/10
        locx = 7 + 18*numcol
        locy = 7 + numsel - 10*numcol
        locl = locx + 9
        CALL gotoxy(locx,locy)
        CALL light(locl,3)
 
C          LOOK FOR KEYPRESS
 
        point = 0
        selecting_option = .TRUE.
        DO WHILE (selecting_option)
          CALL inchr(3,j,key)
          CALL inchr(1,j,key)
 
          SELECT CASE (j)
            CASE (1)                                       !<Esc>
              icod = 1
              if(filbuf .ne. blnkfl) filera = filbuf    !!
              CALL setatr(30)
              RETURN
 
            CASE (28)                                      !<Enter>
              filera = STRING(numsel + numoff)
 
              IF (ifun .GE. 7 .AND. ifun .LT. 10) THEN
                 CALL DOSFUN(1,filera,'COMPMAP.DAT ',DOSCHR)
                 CALL DOSCAL(.FALSE., .TRUE., .FALSE.,.FALSE.,DOSCHR)
              END IF
 
              icod = 0
              RETURN
 
            CASE (59)                                      !<F1>
              CALL help(4)
              CALL cursor(1)
              CYCLE
 
            CASE (63)                                      !<F5>
              IF(ifun .EQ. 10) go to 10
              IF(ifun .GE. 7) THEN
                 filera = STRING(numsel + numoff)
                 CALL DOSFUN(1,filera,'COMPMAP.DAT ',DOSCHR)
                 CALL DOSCAL(.FALSE., .TRUE., .FALSE.,.FALSE.,DOSCHR)
                 CALL doscal(.FALSE., .TRUE., .FALSE., .FALSE.,
     .              'SHOWR COMPMAP.DAT$')
                 point = numsel
                 go to 10
              END IF
 
              CALL newfile(filbuf, IFLAG)
              IF(IFLAG .EQ. 0) THEN
                filera = STRING(numsel + numoff)
                icod = 0
                IF5 = 1
                RETURN
              ELSE
                GO TO 10
              END IF
 
            CASE (67)                                      !<F9>
              CALL help(6)
              CALL cursor(1)
              CYCLE
 
            CASE (43, 68)                                  !<F10>
              if(ifun .NE. 1) CYCLE
              call new_path (iret_path, file_path, length_path)
              IF(iret_path .EQ. 0) i_path = 1
              go to 10
 
            CASE (71)                                      !<Home>
              newsel = 1
 
            CASE (72)                                      !Up cursor
              newsel = numsel - 1
              IF(newsel .EQ. 0) newsel = 1
 
            CASE (73)                                      !<PgUp>
              IF(ipage .EQ. 1) CYCLE
              ipage = ipage - 1
              numoff = numoff - 40
              CALL window(8,18,5,74,32,0)
              selecting_option = .FALSE.
              CYCLE
 
            CASE (75)                                      !Left cursor
              newsel = numsel - 10
              IF(newsel .lt. 1) newsel = numsel
 
            CASE (77)                                      !Right cursor
              newsel = numsel + 10
              IF(newsel .GT. maxera) newsel = numsel
 
            CASE (79)                                      !<End>
              newsel = maxera
 
            CASE (80)                                      !Dn cursor
              newsel = numsel + 1
              IF(newsel .GT. maxera) newsel = maxera
 
            CASE (81)                                      !<PgDn>
              IF(ipage .EQ. ipgmax) CYCLE
              ipage = ipage + 1
              numoff = numoff + 40
              CALL window(8,18,5,74,32,0)
              selecting_option = .FALSE.
              CYCLE
 
            CASE DEFAULT
              CALL warble
              CYCLE
 
          END SELECT
 
C          MOVE cursor TO OLD POSITION AND REMOVE HIGHlightING
 
          IF(newsel .EQ. numsel) CYCLE
          numcol = (numsel - 1)/10
          locx = 7 + 18*numcol
          locy = 7 + numsel - 10*numcol
          locl = locx + 9
          CALL gotoxy(locx,locy)
          CALL light(locl,6)
 
C          MOVE cursor TO NEW POSITION AND ADD HIGHlightING
 
          numcol = (newsel - 1)/10
          locx = 7 + 18*numcol
          locy = 7 + newsel - 10*numcol
          locl = locx + 9
          CALL gotoxy(locx,locy)
          CALL light(locl,3)
          numsel = newsel
          CYCLE
 
        END DO
      END DO
 
      END
 
      SUBROUTINE FULLNAME(filera, filpat)
C     ******************************************************************
C     *         FILL OUT THE PATH NAME FOR THE FILE                    *
C     ******************************************************************
C
      CHARACTER       filcol, pth_col, work_col
      CHARACTER*13    filera, filnam
      CHARACTER*32    file_path, filpat, work_pat
 
      DIMENSION       filcol(13), pth_col(32), work_col(32)
 
      COMMON /FILPTH/ i_path, file_path, length_path
 
      EQUIVALENCE (filnam, filcol(1)),      (file_path, pth_col(1)),
     .            (work_pat, work_col(1))
 
      filnam = filera
 
      i = 0
      DO i = 1, 32
         work_col(i) = ' '
      END DO
 
      i = 0
      if(i_path .NE. 0) THEN
         DO i = 1, length_path
            work_col(i) = pth_col(i)
         END DO
         i = length_path
      END IF
 
      DO k = 1, 13
        work_col(i+1) = filcol(k)
        i = i + 1
      END DO
 
      filpat = work_pat
 
      RETURN
      END
 
      SUBROUTINE SAVFIL(STRING, NUMERA, FILERA, FILBUF, ICOD)
C     ******************************************************************
C     *         DATA FILE SELECTION MENU                               *
C     ******************************************************************
C
      LOGICAL         selecting_option
      CHARACTER       key, FILCOL, blank
      CHARACTER*13    filera, STRING, filnam, blnkfl, filbuf
      INTEGER*2       icod, icol, irow, locx, locy
 
      DIMENSION  STRING(200), FILCOL(13)
 
      COMMON /CHANGE/ isav, ilpt1, numdum, ibypas, numsel, ibuf, ifile
 
      EQUIVALENCE (filnam,FILCOL(1))
 
      DATA blank/' '/, blnkfl/'             '/
 
      IF(numera .GT. 1) CALL sort(13,numera,STRING)
 
      ipage = 1
      ipgmax = (numera - 1)/40 + 1
      numoff = 0
 
      DO WHILE (.TRUE.)
        CALL setatr(30)
        maxera = 40
        IF(ipage .EQ. ipgmax) maxera = numera - 40*(ipgmax - 1)
        numcol = 0
        locx = 8
 
        DO numsel = 1, maxera
          filera =  STRING(numsel + numoff)
          locy = numsel + 7 - 10*numcol
          IF(locy .EQ. 18) THEN
            locx = locx + 18
            locy = 8
            numcol = numcol + 1
          END IF
          CALL gotoxy(locx,locy)
          CALL print(filera,13,-3)
        END DO
 
        filnam = blnkfl
        filera = filbuf
 
        CALL gotoxy(20,21)
        CALL print('Enter Name of File to be Saved: $',32,-2)
        CALL gotoxy(22,19)
        CALL print(' Press <F5> to Save to Buffer File $',35,-2)
        CALL gotoxy(22,19)
        CALL atrbut(35,49)
        CALL gotoxy(53,21)
        CALL setatr(62)
        CALL atrbut(8,62)
        CALL gotoxy(53,21)
 
        CALL cursor(0)
        number = 0
 
        selecting_option = .TRUE.
        DO WHILE (selecting_option)
          IF(number .EQ. 8) THEN
             selecting_option = .FALSE.
          END IF
 
          IF(selecting_option) THEN
             CALL getxy(icol,irow)
             CALL gotoxy(icol,irow)
             CALL inchr(1,j,key)
          ELSE
             j = 28
          END IF
 
          SELECT CASE (j)
            CASE (1)                                       !<Esc>
              icod=1
              CALL setatr(30)
              RETURN
 
            CASE (14)                                      !Backspace
              IF(number .GT. 0) THEN
                WRITE(*,'(A1\)') key
                WRITE(*,'(A1\)') blank
                WRITE(*,'(A1\)') key
                FILCOL(number) = blank
                number = number - 1
              END IF
 
            CASE (28)                                      !<Enter>
              IF(number .GT. 0) THEN
                FILCOL(number+1) = '.'
                FILCOL(number+2) = 'E'
                FILCOL(number+3) = 'R'
                FILCOL(number+4) = 'A'
                FILCOL(number+5) = CHAR(0)
 
                filera = filnam
                icod = 0
                CALL cursor(1)
                CALL setatr(30)
                RETURN
              END IF
 
            CASE (59)                                      !<F1>
              CALL help(7)
 
            CASE (63)                                      !<F5>
              filera = filbuf
              icod = 0
              CALL cursor(1)
              CALL setatr(30)
              RETURN
 
            CASE (67)                                      !<F9>
              CALL help(6)
 
            CASE (73)                                      !<PgUp>
              IF(ipage .EQ. 1) CYCLE
              ipage = ipage - 1
              numoff = numoff - 40
              CALL setatr(30)
              CALL window(8,18,5,74,32,0)
              selecting_option = .FALSE.
 
            CASE (81)
              IF(ipage .EQ. ipgmax) CYCLE
              ipage = ipage + 1
              numoff = numoff + 40
              CALL setatr(30)
              CALL window(8,18,5,74,32,0)
              selecting_option = .FALSE.
 
            CASE DEFAULT
             ichr = ICHAR(key)
             IF((ichr .GE. 48 .AND. ichr .LE. 57) .OR. ichr .EQ. 45 .OR.
     .         (ichr .GE. 65 .AND. ichr .LE. 122)) THEN
                 WRITE(*,'(A1\)') key
                 number = number + 1
                 FILCOL(number) = key
             ELSE
                 CALL warble
             END IF
          END SELECT
 
        END DO
 
      END DO
 
      END
c
      SUBROUTINE DOSFUN(LOC,FILIN,FILOUT,DOSCHR)
C     *****************************************************************
C     *                CREATE STRING TO MANIPULATE FILE               *
C     *****************************************************************
C
      LOGICAL DONE, DOT
      CHARACTER CHRBIT(64), CHRTST(13)
      CHARACTER*13 FILIN, FILOUT, FILTST
      CHARACTER*64 DOSCHR, WRKCHR
 
      EQUIVALENCE (WRKCHR, CHRBIT(1)),        (FILTST, CHRTST(1))
C
C          SELECT FUNCTION AND BUILD UP THE COMMAND LINE
C
      N = 1
      DO WHILE (N .LE. 60)
         CHRBIT(N) = CHAR(32)
         N = N + 1
      END DO
 
      SELECT CASE (LOC)
         CASE (1, 3)                                       !Copy or erase
            CHRBIT(1) = 'C'
            CHRBIT(2) = 'O'
            CHRBIT(3) = 'P'
            CHRBIT(4) = 'Y'
            CHRBIT(5) = ' '
            m = 0
 
            if(loc .eq. 3) then
               chrbit(1) = 'E'
               chrbit(2) = 'R'
               chrbit(3) = 'A'
               chrbit(4) = 'S'
               chrbit(5) = 'E'
               m = 1
            end if
 
            DONE = .FALSE.
            DOT = .FALSE.
 
            N = 1
            FILTST = FILIN
            DO WHILE (.NOT. DONE)
               IF(ICHAR(CHRTST(N)) .LE. 32 .AND. DOT) THEN
                  DONE = .TRUE.
                  CYCLE
               END IF
 
               CHRBIT(5+N+m) = CHRTST(N)
               IF(CHRTST(N) .EQ. '.') DOT = .TRUE.
               N = N + 1
            END DO
 
            if(loc .eq. 1) then
 
               N = N + 1
               CHRBIT(5+N+m) = CHAR(32)
 
               FILTST = FILOUT
               L = 1
               DONE = .FALSE.
               DOT = .FALSE.
               DO WHILE (.NOT. DONE)
                  IF(ICHAR(CHRTST(L)) .LE. 32 .AND. DOT) THEN
                     DONE = .TRUE.
                     CYCLE
                  END IF
 
                  CHRBIT(5+N+m) = CHRTST(L)
                  IF(CHRTST(L) .EQ. '.') DOT = .TRUE.
                  N = N + 1
                  L = L + 1
               END DO
            end if
 
            CHRBIT(5+N+m) = '$'
 
         CASE (2)                                          !Change .ext
            DONE = .FALSE.
            FILTST = FILIN
            N = 1
            DO WHILE (.NOT. DONE)
               CHRBIT(N) = CHRTST(N)
               IF(CHRTST(N) .EQ. '.') THEN
                  DONE = .TRUE.
                  CYCLE
               ELSE
                  N = N + 1
               END IF
            END DO
 
            FILTST = FILOUT
            CHRBIT(N+1) = CHRTST(10)
            CHRBIT(N+2) = CHRTST(11)
            CHRBIT(N+3) = CHRTST(12)
 
            N = 1
            DO WHILE (N .LE. 12)
               CHRTST(N) = CHRBIT(N)
               N = N + 1
            END DO
 
            FILIN = FILTST
 
      END SELECT
 
      DOSCHR = WRKCHR
      RETURN
      END
C
      SUBROUTINE new_path (iret_path, file_path, length_path)
C     ******************************************************************
C     *         DATA FILE SELECTION MENU                               *
C     ******************************************************************
C
      LOGICAL         finding_path
      CHARACTER       key, str_col
      CHARACTER*32    file_path, string
      DIMENSION       str_col(32)
      EQUIVALENCE     (string, str_col(1))
C
C          GET NAME OF NEW PATH
C
      CALL setatr(79)
      CALL window(12, 18, 16, 63, 32, 0)
      CALL window(13, 17, 20, 59, 32, 1)
 
      CALL setatr(01)
      CALL window(19, 19, 18, 65, 219, 3)
      CALL window(13, 18, 64, 65, 219, 3)
      CALL setatr(0)
      CALL setatr(79)
 
      CALL gotoxy(24,15)
      CALL print('Path: $', 06, -2)
 
      CALL atrbut(32, 79)
      CALL gotoxy(30,15)
      CALL cursor(0)
 
C
C          GET THE NAME OF THE PATH
C
      iret_path = 1
      DO i = 1, 32
         str_col(i) = ' '
      END DO
 
      finding_path = .TRUE.
      number = 0
      DO WHILE (finding_path)
 
        CALL getxy(icol,irow)
        CALL gotoxy(icol,irow)
        CALL inchr(1,j,key)
 
        SELECT CASE (j)
          CASE (1)                                       !<Esc>
            finding_path = .false.
            CYCLE
 
          CASE (14)                                      !Backspace
            IF(number .GT. 0) THEN
              WRITE(*,'(A1\)') key
              WRITE(*,'(A1\)') ' '
              WRITE(*,'(A1\)') key
              str_col(number) = ' '
              number = number - 1
            END IF
 
          CASE (28)                                      !<Enter>
            IF(number .GT. 0) iret_path = 0
              finding_path = .false.
              CYCLE
 
          CASE DEFAULT
             WRITE(*,'(A1\)') key
             number = number + 1
             str_col(number) = key
 
        END SELECT
 
      END DO
C
C          TAKE CARE OF THE LAST BLACKSLASH
C
      IF(iret_path .EQ. 0) THEN
 
         DO i = 1, number                                  !Handle '/'
            IF(str_col(i) .EQ. '/') str_col(i) = '\'
         END DO
 
         IF(str_col(number) .NE. '\') THEN
            number = number + 1
            str_col(number) = '\'
         END IF
 
         ncolon = 0                                        !Make sure a leading
         DO n = 1, number                                  !  \ is in place
            IF(str_col(n) .EQ. ':') ncolon = n
         END DO
 
         IF (str_col(ncolon+1) .NE. '\') THEN
            DO n = number, ncolon+1, -1
               str_col(n+1) = str_col(n)
            END DO
            str_col(ncolon+1) = '\'
            number = number + 1
         END IF
 
         file_path = string
         length_path = number
 
         OPEN (6, FILE='new_path.pth', STATUS = 'UNKNOWN')
         WRITE(6, '(A32)') file_path
         CLOSE (6)
 
      END IF
 
C
C          RESET SCREEN AND RETURN
C
      CALL cursor(1)
      CALL setatr(0)
      CALL setatr(30)
 
      RETURN
      END
