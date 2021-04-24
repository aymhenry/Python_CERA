      SUBROUTINE INPUT (chain)
C     *****************************************************************
C     *                Input Routine for ERA                          *
C     *****************************************************************
      LOGICAL         valid, initialize, save_file, any_save, chain,
     .                change_window
      LOGICAL         exists,  changed_fluid, getting_choice,
     .                changed_compressor
 
      CHARACTER       chr, TITLE, yesno, form_feed, key
 
      CHARACTER*13    filsel, filera, filbuf, blnkf
      CHARACTER*32    filpat
      CHARACTER*64    dirpath
      CHARACTER*72    head_line
 
      INTEGER*2       icod, jdrve, IOK
 
      REAL            MLEFT
 
      DIMENSION       YESNO(2)
      DIMENSION       DUMLAST (20)
      DIMENSION       IR(5), F(5, 5), XM(5)
 
      COMMON /CHANGE/ isav, ilpt1, numsel, ibypas, numpat, ibuf, ifile
      COMMON /DSKINP/ iwrite, iprint, iout
      COMMON /TITL/   ititl, TITLE(68,5)
      COMMON /LIMITS/ IOK(800)
      COMMON /VALUES/ VAL(800), VALMN(800), VALMX(800)
      COMMON /FILINF/ filera
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
      COMMON /DISPLY/ change_window
      COMMON /LINCNT/ num3, num_lines, form_feed
      COMMON /TSTREF/ icheck_refrig
      COMMON /EDTCOM/ icodet, inote
 
      DATA            io/0/, ipass/3/, YESNO/'Y','N'/
      DATA            filbuf /'             '/
      DATA            blnkf  /'             '/
 
C          INITIALIZE PROGRAM
 
      initialize = .TRUE.
      DO WHILE (.TRUE.)
        IF(INITIALIZE) THEN
          ibypas = 0
 
          isav = 1
          in_buf = 0
          form_feed = CHAR(12)
          if(iform .EQ. 0) form_feed = CHAR(49)
 
          initialize = .FALSE.
        END IF
 
C          MAIN MENUE
 
        numsel = 13
 
        ilpt1 = 0
        iprint = 0
        iwrite = 0
        iout = io
        iopt = 0
 
        CALL getmen (in_buf, loc)
 
C          BRANCH ON LOC (SELECTION)
 
        save_file = .FALSE.
        SELECT CASE (loc)
          CASE (1)                                        !Read data file
            IF(isav .EQ. 0) THEN
              CALL savyes(save_file)
            END IF
 
            IF(.NOT. save_file) THEN
              CALL getsav(1, filera, filbuf, icod, isav, in_buf)
            ELSE
              CALL getsav(2, filera, filbuf, icod, isav, in_buf)
            END IF
 
  !         IF(icod .eq. 2) THEN
  !            CALL video(0)
  !            CALL screen(11)
  !            chain = .FALSE.
  !            RETURN
  !         END IF
 
            CYCLE
 
          CASE (2, 3)                                       !Edit data
            IF(in_buf .EQ. 0) THEN
              CALL getsav(1, filera, filbuf, icod, isav, in_buf)
              IF(icod .EQ. 1) CYCLE
            END IF
 
          CASE (4)                                         !Save file
            CALL getsav(2, filera, filbuf, icod, isav, in_buf)
            CYCLE
 
          CASE (5, 6)                                      !Run program
            any_save = .FALSE.
            IF(in_buf .EQ. 0) THEN
              CALL getsav(1, filera, filbuf, icod, isav, in_buf)
              IF(icod .EQ. 1) CYCLE
            END IF
 
C          Check Refrigerant Change?
 
            IF (icheck_refrig .EQ. 0) GO TO 200
 
            exists = .FALSE.
            INQUIRE (FILE='LAST.BIN', EXIST=exists)
            IF (loc .EQ. 6 .AND. exists) THEN    !! cycle analysis called
               OPEN (10, FILE='LAST.BIN', FORM='BINARY')
               READ (10) DUMLAST(1)
               READ (10) DUMLAST(2)
               READ (10) DUMLAST(3)
 
               IDUM1 = NFIX(DUMLAST(1))
               IDUM2 = NFIX(DUMLAST(2))
               IDUM3 = NFIX(DUMLAST(3))
 
               IF (IDUM1 .EQ. 3 .OR. IDUM3 .NE. 1) THEN
                  CLOSE (10)
                  GO TO 200
               END IF
 
               DO i = 4, 20
                  IF (i .NE. 7) THEN
                     READ (10) DUMLAST(i)
                  ELSE
                    READ (10) NDUMREF
                  END IF
               END DO
               CLOSE (10)
 
               !! CHECK CURRENT DATA AGAINST LAST DATA
               NUMREF = NFIX(VAL(175))
               changed_fluid = .FALSE.
               changed_compressor = .FALSE.
               IPNT = 180
               IF (NUMREF .EQ. 2) IPNT = 320
               SELECT CASE (NUMREF)
                  CASE (1)
                     IF (VAL(IPNT) .NE. DUMLAST(4))
     .                                            changed_fluid = .TRUE.
                     IF (NUMREF .NE. NDUMREF)     changed_fluid = .TRUE.
                     IF (VAL(IPNT+1) .NE. DUMLAST(11))
     .                                            changed_fluid = .TRUE.
 
                   CASE (2)
                     IF (VAL(IPNT) .NE. DUMLAST(4))
     .                                            changed_fluid = .TRUE.
                     IF (VAL(IPNT+ 2) .NE. DUMLAST(5))
     .                                            changed_fluid = .TRUE.
                     IF (NUMREF .NE. NDUMREF)     changed_fluid = .TRUE.
                     IF (VAL(IPNT+3) .NE. DUMLAST(8))
     .                                            changed_fluid = .TRUE.
                     IF (VAL(IPNT+1) .NE. DUMLAST(11))
     .                                            changed_fluid = .TRUE.
                     IF (1.0 - VAL(IPNT+1) .NE. DUMLAST(12))
     .                                            changed_fluid = .TRUE.
 
                   CASE (3)
                     IF (VAL(IPNT) .NE. DUMLAST(4))
     .                                            changed_fluid = .TRUE.
                     IF (VAL(IPNT+ 2) .NE. DUMLAST(5))
     .                                            changed_fluid = .TRUE.
                     IF (VAL(IPNT+ 4) .NE. DUMLAST(6))
     .                                            changed_fluid = .TRUE.
                     IF (NUMREF .NE. NDUMREF)     changed_fluid = .TRUE.
                     IF (VAL(IPNT+6) .NE. DUMLAST(8))
     .                                            changed_fluid = .TRUE.
                     IF (VAL(IPNT+7) .NE. DUMLAST(9))
     .                                            changed_fluid = .TRUE.
                     IF (VAL(IPNT+8) .NE. DUMLAST(10))
     .                                            changed_fluid = .TRUE.
                     IF (VAL(IPNT+1) .NE. DUMLAST(11))
     .                                            changed_fluid = .TRUE.
                     IF (VAL(IPNT+3) .NE. DUMLAST(12))
     .                                            changed_fluid = .TRUE.
                     MLEFT = 1.0 - VAL(IPNT+1) - VAL(IPNT+3)
                     IF (MLEFT .NE. DUMLAST(13))  changed_fluid = .TRUE.
 
               END SELECT
 
               getting_choice = .TRUE.
 
               IF (changed_fluid) THEN
                  IF (DUMLAST(14) .NE. VAL(494) .OR.
     .                DUMLAST(15) .NE. VAL(495) .OR.
     .                DUMLAST(16) .NE. VAL(496) .OR.
     .                DUMLAST(18) .NE. VAL(500))
     .                changed_compressor = .TRUE.
 
                  IF (changed_compressor) THEN
                     GO TO 200
                  ELSE
                     CALL error (4)
                     CALL gotoxy(20, 11)
                     CALL print('The refrigerant has changed $',28,-2)
                     CALL print('but the $',8,-2)
                     CALL gotoxy(20, 12)
                     CALL print('compressor is the same.  Do  $',29,-2)
                     CALL print('you wish $', 9,-2)
                     CALL gotoxy(20, 13)
                     CALL print('to adjust the compressor $', 25, -2)
                     CALL print('displacement?$', 13, -2)
                     CALL gotoxy(28, 15)
                     CALL print('Press Y or N to continue.$', 26,-2)
                  END IF
 
                  DO WHILE (getting_choice)
                     CALL inchr(1,j,key)
                     IF (key .EQ. 'Y' .OR. key . EQ. 'y') THEN
                        getting_choice = .FALSE.
 
                        DISPLC = VAL(494) / 16.3871
                        CAPCOMP = VAL(495) / 0.252
                        SPEEDN = VAL(496)
                        EER = VAL(497)
                        ICOOL = VAL(498)
                        FRACTIONAL_SPEED = VAL(500)
                        ICOMP = VAL(455)
 
                        !! find the clearance volume
                        CALL COMPR (0, 1, IR, XM, F, DISPLC, CAPCOMP,
     .                              SPEEDN, EER, ICOMP, ICOOL,
     .                              FRACTIONAL_SPEED, CAPRATIO,
     .                              DENRATIO)
 
                        DO i = 1, 5
                           IR(i) = 0
                           XM(i) = 0
                           DO j = 1, 5
                              F(i,j) = 0
                           END DO
                        END DO
 
                        IR(1) = DUMLAST(4)
                        IR(2) = DUMLAST(5)
                        IR(3) = DUMLAST(6)
 
                        F(1, 2) = DUMLAST(8)
                        F(1, 3) = DUMLAST(9)
                        F(2, 3) = DUMLAST(10)
                        F(2, 1) = F(1, 2)
                        F(3, 1) = F(1, 3)
                        F(3, 2) = F(2, 3)
 
                        XM(1) = DUMLAST(11)
                        XM(2) = DUMLAST(12)
                        XM(3) = DUMLAST(13)
 
                        NC = NDUMREF
 
                        !!  get capacity for previous fluid
                        CALL COMPR (1, NC, IR, XM, F, DISPLC, CAPCOMP,
     .                              SPEEDN, EER, ICOMP, ICOOL,
     .                              FRACTIONAL_SPEED, CAPRATIO,
     .                              DENRATIO)
 
                        DO i = 1, 5
                           IR(i) = 0
                           XM(i) = 0
                           DO j = 1, 5
                              F(i,j) = 0
                           END DO
                        END DO
 
                        IPNT = 180
                        IF (NUMREF .EQ. 2) IPNT = 320
 
                        IR(1) = VAL(IPNT)
                        XM(1) = VAL(IPNT+1)
 
                        IF (NUMREF .EQ. 2) THEN
                           IR(2) = VAL(IPNT+2)
                           XM(2) = 1.0 - XM(1)
                           F(1, 2) = VAL(IPNT+3)
                        END IF
 
                        IF (NUMREF .EQ. 3) THEN
                           IR(2) = VAL(IPNT+2)
                           XM(2) = VAL(IPNT+3)
                           IR(3) = VAL(IPNT+4)
                           XM(3) = 1.0 - XM(1) - XM(2)
                           F(1,2) = VAL(IPNT+6)
                           F(1,3) = VAL(IPNT+7)
                           F(2,3) = VAL(IPNT+8)
                        END IF
 
 
                        F(2, 1) = F(1, 2)
                        F(3, 1) = F(1, 3)
                        F(3, 2) = F(2, 3)
 
                        NC = NUMREF
 
                        !!  get capacity for previous fluid
                        CALL COMPR (2, NC, IR, XM, F, DISPLC, CAPCOMP,
     .                              SPEEDN, EER, ICOMP, ICOOL,
     .                              FRACTIONAL_SPEED, CAPRATIO,
     .                              DENRATIO)
 
 
                        WRITE(HEAD_LINE, '(''CAPACITY ADJUSTMENT'')')
                        CALL BLOCK_HEAD(HEAD_LINE)
 
                        CALL SETINT(3,0,0,0,0,0,0,0,0,0,0,0,0,0)
                        CALL SETIOK(1,1,1,1,1,1,1,1,1,1,1,1,
     .                                                    1,1,IOK(767))
 
                        VAL(767) = CAPRATIO
                        VALMN(767) = 0.25
                        VALMX(767) = 4.00
                        ICODET = 26
                        isavsav = isav
                        CALL EDTFIL('BLOCK85.DAT', 01, 01, 1, 0, IFLAG,
     .                     IOUT, VAL(767), VALMN(767), VALMX(767),
     .                     IOK(767), 1)
                        isav = isavsav
                        ICODET = 0
 
                        IF (VAL(767) .NE. 1.00) THEN
                           VAL(494) = VAL(767) * VAL(494)
                           VAL(495) = VAL(767) * VAL(495)
 
                           IF (DENRATIO .LT. 1.0) THEN
                              VAL(501) = DENRATIO * DUMLAST(19)
                              VAL(502) = DENRATIO * DUMLAST(20)
                           END IF
 
                           CALL INPUTC(3,iret)
                           isav = 0
                           GO TO 300
                        END IF
 
                     END IF
 
 
                     IF (key .EQ. 'N' .OR. key .EQ. 'n') THEN
                        CALL error (2)
                        CALL gotoxy(21, 11)
                        CALL print('WARNING:  Check compressor $',27,-2)
                        CALL print('duty cycle.$',11,-2)
                        CALL gotoxy(26, 13)
                        CALL print('Press any key to continue.$', 26,-2)
                        CALL inchr(1,j,key)
                        getting_choice = .FALSE.
                     END IF
                  END DO
 
               END IF
 
            END IF  !! loc = 6 and file exists
 
  200       CONTINUE   !!  jump here if necessary
 
            CALL chkcab(2, VAL, iflag)
            IF(iflag .EQ. 1) CYCLE
 
            CALL chkcab(3, VAL, iflag)
            IF(iflag .EQ. 1) CYCLE
 
            IF(loc .EQ. 6) THEN
              CALL chkcab(4, VAL, iflag)
              IF(iflag .EQ. 1) CYCLE
            END IF
 
            save_file = .FALSE.
            IF(isav .EQ. 0) THEN
              CALL savyes(save_file)
            END IF
 
            IF(save_file) THEN
              any_save = .TRUE.
              CALL getsav(2, filera, filbuf, icod, isav, in_buf)
            END IF
 
            IF(.NOT. any_save) THEN
              CALL cursor(1)
              IF(loc .EQ. 6) THEN
  !             CALL chkcab(2, VAL, iflag)
  !             IF(iflag .EQ. 1) CYCLE
              END IF
 
  !           CALL chkcab(3, VAL, iflag)
  !           IF(iflag .EQ. 1) CYCLE
 
  !           val(363) = val(362)   !Adjust for change made in BLOCK31.DAT
  !           val(373) = val(372)   ! to add behind-the-wall evaporator
 
              OPEN(ipass, FILE='CABINET.DAT', STATUS='UNKNOWN')
              CALL chkcab(1, VAL, iflag)
              CALL cabdat(ipass, TITLE, VAL)
              CLOSE(ipass)
 
              IF(loc .EQ. 6) THEN
                OPEN(ipass, FILE='CYCLE.DAT', STATUS='UNKNOWN')
                CALL cycdat(ipass)
                CLOSE(ipass)
              END IF
 
              chain = .TRUE.
              RETURN
 
            END IF
  300       CYCLE
 
          CASE (7, 8)                                      !Write input
            IF(in_buf .EQ. 0) THEN
              CALL getsav(1, filera, filbuf, icod, isav, in_buf)
              IF(icod .EQ. 1) CYCLE
            END IF
 
          CASE (9)                                         !Quit
            CALL video(2)
            CALL gotoxy(26,12)
            CALL print('  Do You Wish to Quit (Y/N) : $',30,-2)
 
            valid = .FALSE.
              CALL inchr(3,j,chr)
            DO WHILE (.NOT. valid)
              CALL cursor(0)
              CALL inchr(1,j,chr)
              CALL check(chr, 2, YESNO, valid, l_ans)
              IF(.NOT. valid) CALL warble
            END DO
 
            CALL print(chr,1,-2)
            CALL WAIT(9)
            IF(l_ans .EQ. 2) CYCLE
 
            save_file = .FALSE.
            IF(isav .eq. 0) THEN
              CALL savyes(save_file)
            END IF
 
            IF(save_file) THEN
              CALL getsav(2, filera, filbuf, icod, isav, in_buf)
              CYCLE
            END IF
 
            CALL video(0)
            CALL screen(11)
            chain = .FALSE.
            RETURN
 
          CASE (10)                                        !Call DOS
            CALL drive(0, jdrve, ndrive)
            idrive = jdrve
            CALL getdir (idrive+1, dirpath)
 
            CALL doscal(.TRUE.,.TRUE.,.FALSE.,.FALSE.,'$')
            CALL drive(1, jdrve, ndrive)
            CALL doscal(.FALSE., .TRUE., .FALSE., .FALSE., dirpath)
 
            change_window = .TRUE.
            CYCLE
 
          CASE (11)                                        !Delete file
            CALL errhan(0)
            CALL GETFIL(10, filera, blnkf, icod, IF5)
            CALL errhan(1)
            IF (icod .EQ. 0) THEN
               CALL FULLNAME(filera, filpat)
 !             CALL BACKUP(filera)
               CALL BACKUP(filpat)
            END IF
 
            CYCLE
 
        END SELECT
 
C          BRANCH ACCORDING TO LOC: EDIT, WRITE TO FILE, PRINT
 
        SELECT CASE (loc)
           CASE (2)                                        !Cabnet data
             CALL inpute(1, iret)
             IF(iret .EQ. 2) initialize = .TRUE.
             CYCLE
 
           CASE (3)
             CALL inputc(1, iret)
             IF(iret .EQ. 2) initialize = .TRUE.
             CYCLE
 
           CASE (7, 8)                                     !Output
             iprint = 1
             iret = 0
             iwrite = 0
             IF(loc .EQ. 8) iwrite = 1
 
             IF(iwrite .EQ. 0) THEN
               ilpt1 = 1
               iout = 2
 
               IF(iport .EQ. 1) THEN
                 OPEN(iout, FILE='LPT2', STATUS='OLD')
               ELSE
                 OPEN(iout, FILE='LPT1')
               END IF
             END IF
 
             IF(iwrite .EQ. 1) THEN
               iout  = 3
               CALL filinp(filera,filsel)
               OPEN(iout, FILE=filsel, STATUS='UNKNOWN')
             END IF
 
             CALL video(3)
             IF(iwrite .EQ. 0) THEN
               CALL gotoxy(28,12)
               CALL print('Printing the Input Data.$',24,-2)
               IF(ifeed .EQ. 1) WRITE(iout,2001) form_feed
             ELSE
               CALL gotoxy(21,12)
               CALL print('Writing the Input Data to a Disk File.$',
     .                                                        38,-2)
               IF(ifeed .EQ. 1) WRITE(iout,2001) CHAR(12)
             END IF
 
             CALL gotoxy(0,0)
 
C          PRINT TITLE OF RUN
 
           DO n = 1,5
             WRITE(iout,2002) (TITLE(j,n), j=1,68)
           END DO
 
           num3 = 0
           num_lines = 6
 
           CALL INPUTE(2,iret)
           CALL INPUTC(2,iret)
 
           IF(iwrite .EQ. 0) WRITE(iout,2001) form_feed
           IF(iwrite .EQ. 1) WRITE(iout,2001) CHAR(12)
 
           IF(iwrite .EQ. 1) THEN
              CLOSE (iout, STATUS='KEEP')
           ELSE
              CLOSE (iout)
           END IF
 
           CYCLE
 
        END SELECT
      END DO
 
C          FORMAT STATEMENTS
 
 2001 FORMAT(A1)
 2002 FORMAT(3X,68A1)
      END
 
      SUBROUTINE FILINP(filera,filsel)
C     ****************************************************************
C     *    CREATE FILENAME TO OUTPUT INPUT DATA                       *
C     *****************************************************************
C
      CHARACTER       FILCOL(13), FILWRK(13)
      CHARACTER*13    filnam, filera, flblnk, work, filsel
 
      EQUIVALENCE     (filnam, FILCOL(1)),   (work, FILWRK(1))
 
      DATA            flblnk /'             '/
 
C          INITIALIZE THE WORK SPACES
 
      filnam = flblnk
      work   = filera
 
C          BUILD UP THE FILE NAME BY COLUMN
 
      n = 1
      DO WHILE (FILWRK(n) .NE. '.')
         FILCOL(n) = FILWRK(n)
         n = n + 1
      END DO
C
C          PERIOD FOUND.  NOW ADD THE EXTENSION
C
      FILCOL(N)   = '.'
      FILCOL(N+1) = CHAR(73)
      FILCOL(N+2) = CHAR(78)
      FILCOL(N+3) = CHAR(80)
 
      filsel = filnam
 
      RETURN
      END
 
      SUBROUTINE SAVYES(SAVE_FILE)
C     ******************************************************************
C     *    ASK IF USER WANTS TO SAVE THE FILE                         *
C     ******************************************************************
C
      LOGICAL         valid, save_file
      CHARACTER       chr, YESNO
 
      DIMENSION       YESNO(2)
 
      DATA            YESNO/'Y','N'/
C
C          OUTPUT SAVE MESSAGE
C
      CALL video(2)
      CALL gotoxy(22,11)
      CALL print(' You Have Not Saved the Input Data.$',35,-2)
      CALL gotoxy(22,12)
      CALL print(' Do You Wish to Save It (Y/N) : $',32,-2)
      CALL inchr(3, j, chr)
 
      valid = .FALSE.
      CALL cursor(0)
 
      DO WHILE (.NOT. valid)
         CALL warble
         CALL inchr(1, j, chr)
         CALL check(chr, 2, YESNO, valid, l_ans)
      END DO
 
      IF(l_ans .EQ. 1) save_file = .TRUE.
 
      CALL print(chr,1,-2)
      CALL wait(18)
      CALL video(0)
      CALL cursor(1)
 
      RETURN
      END
 
      SUBROUTINE GETSAV(IFUN, FILERA, FILBUF, ICOD, ISAV, IN_BUF)
C     ******************************************************************
C     *    FILL THE MEMORY BUFFER WITH A FILE OR SAVE FILE             *
C     ******************************************************************
C
      CHARACTER       TITLE, key
      CHARACTER*13    filera, filbuf
      CHARACTER*32    filpat
      INTEGER*2       icod
 
      COMMON /TITL/   ititl, TITLE(68,5)
      COMMON /VALUES/ VAL(800), VALMN(800), VALMX(800)
 
      DATA            idisk /6/
C
C          BRANCH ON IFUN
C
      SELECT CASE (ifun)
        CASE (1)                                          !Get file
C
C          CALL GETFIL TO LOCATE FILE NAME
C
          CALL errhan(0)
          IF5 = 0
 
          CALL GETFIL(1, filera, filbuf, icod, IF5)
 
          CALL errhan(1)
 
 
 
          IF(icod .EQ. 1) RETURN
C
C          OPEN FILE FILERA AND READ VECTOR VAL
C
          CALL FULLNAME(filera, filpat)
  !       OPEN(idisk, FILE=filera, FORM='BINARY')
          OPEN(idisk, FILE=filpat, FORM='BINARY')
          READ(idisk) TITLE, VAL, VALMN, VALMX
          CLOSE(idisk)
 
          !! handle new files for ERA 2.0 (with hot-wall condesner help)
          IF (VALMN(503) .EQ. 0.0) THEN
             VALMN(503) = 20.0
             VALMN(504) = 0.0
             VALMN(505) = 0.0
             VALMN(506) = 20.0
             VALMN(507) = 0.0
             VALMN(508) = 0.0
             VALMN(509) = 20.0
             VALMN(510) = 0.0
             VALMN(511) = 0.0
 
             VALMX(503) = 100.0
             VALMX(504) = 8.0
             VALMX(505) = 800.0
             VALMX(506) = 120.0
             VALMX(507) = 8.0
             VALMX(508) = 800.0
             VALMX(509) = 120.0
             VALMX(510) = 8.0
             VALMX(511) = 400.0
 
             VAL(503) = 50.0
             VAL(504) = 4.0
             VAL(505) = 0.0
             VAL(506) = 50.0
             VAL(507) = 4.0
             VAL(508) = 0.0
             VAL(509) = 50.0
             VAL(510) = 4.0
             VAL(511) = 0.0
          END IF
 
          !! remaining data for hot wall condenser
          IF (VAL(345) .EQ. 0.0) THEN
             VALMN(345) = 2.0
             VALMN(346) = 0.5
             VALMN(347) = 20.0
             VALMN(348) = 0.5
 
             VALMX(345) = 40.0
             VALMX(346) = 1.0
             VALMX(347) = 50.0
             VALMX(348) = 50.0
 
             VAL(345) = 8.33
             VAL(346) = 0.71
             VAL(347) = 32.0
             VAL(348) = 6.0
          END IF
 
          !! compressor speed data for compressor map option
          IF (VAL(791) .EQ. 0.0) THEN
             VALMN(791) = 0.55
             VAL(791) = 1.0
             VALMX(791) = 1.15
          END IF
 
          !! handle liner data
          VALMN(786) = 0.2
          VALMN(789) = 0.2
          IF (VAL(786) .LT. VALMN(786)) VAL(786) = 0.5
          IF (VAL(789) .LT. VALMN(789)) VAL(789) = 2.0
 
          !! override minimum vacuum panel barrier thickness
          VALMN(151) = 0.05
 
          !! reenatble specification of a static condenser
          VALMX(246) = 3.0
          VALMX(266) = 3.0
          VALMX(660) = 2.0
          VALMX(676) = 2.0
 
          !! exte4nd limit on quality
          VALMX(482) = 0.8
          VALMX(640) = 0.8
          VALMX(656) = 0.8
 
          !! aluminum inner liner
          VALMX(790) = 220.0
 
          !!  create file LAST.BIN
          CALL cycdat(0)
 
          IF(IF5 .EQ. 1) filera = filbuf
          filbuf = filera
          in_buf = 1
          isav = 1
 
  !       IF (VALMX(785) .eq. 0.0) THEN
          IF (VALMX(792) .eq. 0.0) THEN
             CALL error (2)
             CALL gotoxy(21, 11)
             CALL print('Data File Not Compatible with ERA 1.2',37,-2)
             CALL gotoxy(21, 12)
             CALL print('Convert Data File with Update Program',37,-2)
             CALL inchr(1,j,key)
             icod = 1
             in_buf = 0
             filbuf = '             '
            RETURN
         END IF
 
        CASE (2)                                          !Save file
          IF(in_buf .EQ. 0) RETURN
 
          CALL errhan(0)
          CALL GETFIL(2, filera, filbuf, icod, IF5)
          CALL errhan(1)
 
          IF(ICOD .EQ. 1) RETURN
 
          CALL FULLNAME(filera, filpat)
  !       OPEN (idisk, FILE=filera, FORM='BINARY')
          OPEN (idisk, FILE=filpat, FORM='BINARY')
          WRITE(idisk) TITLE, VAL, VALMN, VALMX
          ENDFILE idisk
          CLOSE(idisk)
 
          isav = 1
          in_buf = 1
 
      END SELECT
 
      RETURN
      END
