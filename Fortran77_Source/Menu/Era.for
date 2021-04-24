      BLOCK DATA
C     ******************************************************************
C     *                                                                *
C     *       EPA Refrigerator-Freezer Design Analysis Tool            *
C     *                                                                *
C     *                      Developed by                              *
C     *                                                                *
C     *                  Arthur D. Little, Inc.                        *
C     *                     Richard Merriam                            *
C     *                     Anthony Varone                             *
C     *                                                                *
C     ******************************************************************
C
      LOGICAL         static, shadow
      INTEGER*2       SET, SET1, SET2, SET3, SET4, SET5, SET6,
     .                SET7, SET8
 
      DIMENSION       SET1(120), SET2(150), SET3(060), SET4(150),
     .                SET5(090), SET6(150), SET7(150), SET8(090)
 
      COMMON /DRWSET/ SET(960), static, shadow
 
      EQUIVALENCE     (SET(001),SET1(1)),    (SET(121),SET2(1)),
     .                (SET(271),SET3(1)),    (SET(331),SET4(1)),
     .                (SET(481),SET5(1)),    (SET(571),SET6(1)),
     .                (SET(721),SET7(1)),    (SET(871),SET8(1))
 
      DATA SET1 /
     .                10, 4, 49, 7, 1, 11, 4,223, 7,10, 21, 4, 50, 7, 1,
     .                10, 5, 49, 7, 1, 11, 5,223, 7,10, 21, 5, 50, 7, 1,
     .                10, 6, 49, 7, 1, 11, 6,223, 7, 4, 15, 6, 50, 7, 1,
     .                10, 7, 49, 7, 1, 11, 7,223, 7, 8, 19, 7, 50, 7, 1,
     .                10, 8, 49, 7, 1, 11, 8,223, 7, 8, 19, 8, 50, 7, 1,
     .                10, 9, 49, 7, 1, 11, 9,223, 7, 4, 15, 9, 50, 7, 1,
     .                10,10, 49, 7, 1, 11,10,223, 7,10, 21,10, 50, 7, 1,
     .                10,11, 49, 7, 1, 11,11,223, 7,10, 21,11, 50, 7, 1/
 
      DATA SET2 /
     .                31, 4, 49, 7, 1, 32, 4,223, 7,10, 42, 4, 50, 7, 1,
     .                31, 5, 49, 7, 1, 32, 5,223, 7, 2, 34, 5, 50, 7, 1,
     .                40, 5, 49, 7, 1, 41, 5,223, 7, 2, 43, 5, 50, 7, 1,
     .                31, 6, 49, 7, 1, 32, 6,223, 7, 2, 34, 6, 50, 7, 1,
     .                40, 6, 49, 7, 1, 41, 6,223, 7, 2, 43, 6, 50, 7, 1,
     .                31, 7, 49, 7, 1, 32, 7,223, 7,10, 42, 7, 50, 7, 1,
     .                31, 8, 49, 7, 1, 32, 8,223, 7, 2, 34, 8, 50, 7, 1,
     .                40, 8, 49, 7, 1, 41, 8,223, 7, 2, 43, 8, 50, 7, 1,
     .                31, 9, 49, 7, 1, 32, 9,223, 7, 2, 34, 9, 50, 7, 1,
     .                41, 9, 49, 7, 1, 42, 9,223, 7, 2, 44, 9, 50, 7, 1/
      DATA SET3 /
     .                31,10, 49, 7, 1, 32,10,223, 7, 2, 34,10, 50, 7, 1,
     .                42,10, 49, 7, 1, 43,10,223, 7, 2, 45,10, 50, 7, 1,
     .                31,11, 49, 7, 1, 32,11,223, 7, 2, 34,11, 50, 7, 1,
     .                43,11, 49, 7, 1, 44,11,223, 7, 2, 46,11, 50, 7, 1/
 
      DATA SET4 /
     .                61, 4, 49, 7, 1, 62, 4,223, 7, 3, 65, 4, 50, 7, 1,
     .                60, 5, 49, 7, 1, 61, 5,223, 7, 1, 62, 5, 50, 7, 1,
     .                64, 5, 49, 7, 1, 65, 5,223, 7, 1, 66, 5, 50, 7, 1,
     .                59, 6, 49, 7, 1, 60, 6,223, 7, 2, 62, 6, 50, 7, 1,
     .                64, 6, 49, 7, 1, 65, 6,223, 7, 2, 67, 6, 50, 7, 1,
     .                58, 7, 49, 7, 1, 59, 7,223, 7, 2, 61, 7, 50, 7, 1,
     .                65, 7, 49, 7, 1, 66, 7,223, 7, 2, 68, 7, 50, 7, 1,
     .                57, 8, 49, 7, 1, 58, 8,223, 7, 2, 60, 8, 50, 7, 1,
     .                61, 8, 49, 7, 1, 62, 8,223, 7, 3, 65, 8, 50, 7, 1,
     .                66, 8, 49, 7, 1, 67, 8,223, 7, 2, 69, 8, 50, 7, 1/
      DATA SET5 /
     .                56, 9, 49, 7, 1, 57, 9,223, 7, 2, 59, 9, 50, 7, 1,
     .                67, 9, 49, 7, 1, 68, 9,223, 7, 2, 70, 9, 50, 7, 1,
     .                55,10, 49, 7, 1, 56,10,223, 7, 2, 58,10, 50, 7, 1,
     .                68,10, 49, 7, 1, 69,10,223, 7, 2, 71,10, 50, 7, 1,
     .                54,11, 49, 7, 1, 55,11,223, 7, 2, 57,11, 50, 7, 1,
     .                69,11, 49, 7, 1, 70,11,223, 7, 2, 72,11, 50, 7, 1/
 
      DATA SET6 /
     .                29,16, 69, 7, 1, 30,16, 80, 7, 1, 31,16, 65, 7, 1,
     .                33,16, 82, 7, 1, 34,16,101, 7, 1, 35,16,102, 7, 1,
     .                36,16,114, 7, 1, 37,16,105, 7, 1, 38,16,103, 7, 1,
     .                39,16,101, 7, 1, 40,16,114, 7, 1, 41,16, 97, 7, 1,
     .                42,16,116, 7, 1, 43,16,111, 7, 1, 44,16,114, 7, 1,
     .                46,16, 65, 7, 1, 47,16,110, 7, 1, 48,16, 97, 7, 1,
     .                49,16,108, 7, 1, 50,16,121, 7, 1, 51,16,115, 7, 1,
     .                52,16,105, 7, 1, 53,16,115, 7, 1, 40,18, 98, 7, 1,
     .                41,18,121, 7, 1, 19,20, 82, 7, 1, 20,20,105, 7, 1,
     .                21,20, 99, 7, 1, 22,20,104, 7, 1, 23,20, 97, 7, 1/
      DATA SET7 /
     .                24,20,114, 7, 1, 25,20,100, 7, 1, 27,20, 77, 7, 1,
     .                28,20,101, 7, 1, 29,20,114, 7, 2, 31,20,105, 7, 1,
     .                32,20, 97, 7, 1, 33,20,109, 7, 1, 34,20, 44, 7, 1,
     .                36,20, 65, 7, 1, 37,20,110, 7, 1, 38,20,116, 7, 1,
     .                39,20,104, 7, 1, 40,20,111, 7, 1, 41,20,110, 7, 1,
     .                42,20,121, 7, 1, 44,20, 86, 7, 1, 45,20, 97, 7, 1,
     .                46,20,114, 7, 1, 47,20,111, 7, 1, 48,20,110, 7, 1,
     .                49,20,101, 7, 1, 50,20, 44, 7, 1, 52,20, 97, 7, 1,
     .                53,20,110, 7, 1, 54,20,100, 7, 1, 56,20, 72, 7, 1,
     .                57,20,101, 7, 1, 59,20, 70, 7, 1, 60,20,101, 7, 1/
      DATA SET8 /
     .                61,20,110, 7, 1, 62,20,103, 7, 1, 31,21, 65, 7, 1,
     .                32,21,114, 7, 1, 33,21,116, 7, 1, 34,21,104, 7, 1,
     .                35,21,117, 7, 1, 36,21,114, 7, 1, 38,21, 68, 7, 1,
     .                40,21, 76, 7, 1, 41,21,105, 7, 1, 42,21,116, 7, 2,
     .                44,21,108, 7, 1, 45,21,101, 7, 1, 46,21, 44, 7, 1,
     .                48,21, 73, 7, 1, 49,21,110, 7, 1, 50,21, 99, 7, 1/
      END
C     PROGRAM ERA
C     ******************************************************************
C     *           MAIN PROGRAM FOR REFRIGERATOR-FREEZER CABINET        *
C     *           LOADS ANALYSIS AND CYCLE ANALYSIS.                   *
C     ******************************************************************
 
      LOGICAL         change_window, chain, static, shadow, exists
 
      CHARACTER       TITLE, key
      CHARACTER       CHRMNM, CHRMNG, CHRMNE, CHRKEY, CHRSUB, CHRSAV,
     .                CHRCYC, CHRREF, CHRDOE
      CHARACTER*11    fillst, filblk, filred
      CHARACTER*13    filmap1, filmap2
 
      INTEGER*2       SET
 
      COMMON /CHANGE/ isav, ilpt1, numsel, ibypas, numpat, ibuf, ifile
      COMMON /CHRHLP/ CHRMNM(3600), CHRMNG(1100), CHRMNE(1100),
     .                CHRKEY(1000), CHRSUB(9000), CHRSAV(1100),
     .                CHRCYC(1100), CHRREF(8200), CHRDOE(4000)
      COMMON /DRWSET/ SET(960), static, shadow
      COMMON /DSKINP/ iwrite, iprint, iout
      COMMON /EDTCOM/ icodet, inote
      COMMON /LSTBLK/ fillst, filred
      COMMON /TITL/   ititl, TITLE(68,5)
      COMMON /VALUES/ VAL(800), VALMN(800), VALMX(800)
      COMMON /HLPCOL/ iatr
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore, iold_data,
     .                icompr
      COMMON /DISPLY/ change_window
      COMMON /MAPNAM/ filmap1, filmap2
      COMMON /TSTREF/ check_refrig
 
      DATA            idisk /6/
      DATA            filblk/'           '/
 
C          INITIALIZE AND SET UP DRIVE CODE
 
      filmap1 = '             '
      filmap2 = '             '
 
      change_window = .TRUE.
      CALL options()
 
      IF(icolr .EQ. 1) THEN
           CALL trap                                       !Call exit
           CALL setatr(0)
           CALL setatr(30)
      END IF
 
C          Look for \COMMAND.COM
 
      INQUIRE (FILE='\COMMAND.COM', EXIST=exists)
      IF(.NOT. exists) THEN
         CALL error (2)
         CALL gotoxy(21, 11)
         CALL print('A Copy of COMMAND.COM Must be Present',37,-2)
         CALL gotoxy(21, 12)
         CALL print('in the Root Directory of Current Drive',38,-2)
 
         CALL inchr(1,j,key)
         CALL setatr(1)
         CALL gotoxy(0,0)
         CALL screen(0)
         STOP ' '
      END IF
 
C          INITIALIZE SCREEN DATA
 
      CALL screen(10)                                      !Page number
 
C          READ SAVE MENU HELP MENU DATA
 
      OPEN (idisk, FILE='BLOCK36.DAT', FORM='BINARY')
      READ (idisk, IOSTAT=iocheck) CHRSAV
      CLOSE(idisk)
 
C          READ EDIT MENU HELP MENU DATA
 
      OPEN (idisk, FILE='BLOCK37.DAT', FORM='BINARY')
      READ (idisk, IOSTAT=iocheck) CHRMNE
      CLOSE(idisk)
 
C          READ MAIN MENU HELP MENU DATA
 
      OPEN (idisk, FILE='BLOCK38.DAT', FORM='BINARY')
      READ (idisk, IOSTAT=iocheck) CHRMNM
      CLOSE(idisk)
 
C          READ GET MENU HELP MENU DATA
 
      OPEN (idisk, FILE='BLOCK39.DAT', FORM='BINARY')
      READ (idisk, IOSTAT=iocheck) CHRMNG
      CLOSE(idisk)
 
C          READ KEYS HELP MENU DATA
 
      OPEN (idisk, FILE='BLOCK40.DAT', FORM='BINARY')
      READ (idisk, IOSTAT=iocheck) CHRKEY
      CLOSE(idisk)
 
C          READ CYCLE HELP MENU DATA
 
      OPEN (idisk, FILE='BLOCK34.DAT', FORM='BINARY')
      READ (idisk, IOSTAT=iocheck) CHRCYC
      CLOSE(idisk)
 
C          READ REFRIGERANT PROPERTY DATA MENU
 
      OPEN (idisk, FILE='BLOCK63.DAT', FORM='BINARY')
      READ (idisk, IOSTAT=iocheck) CHRREF
      CLOSE(idisk)
 
C          READ DOE PRODUCT CLASSIFICATION INFORMATION
 
      OPEN (idisk, FILE='BLOCK78.DAT', FORM='BINARY')
      READ (idisk, IOSTAT=iocheck) CHRDOE
      CLOSE(idisk)
 
C          INITIALIZE FOR ANALYSIS
 
      fillst = filblk
      filred = filblk
      icodet = 0
 
C          WRITE TITLE PAGE
 
      CALL cursor(1)
      WRITE (*, '('' ''\)')
      CALL video(0)
      CALL draw
      CALL inchr(1,j,key)
  !   CALL peer
 
C          CALL INPUT ROUTINE
 
      chain = .FALSE.
      CALL INPUT (chain)
 
C          TURN OFF COLOR IF ON AND EXIT
 
      IF(.NOT. chain) THEN
        IF(icolr .EQ. 1) CALL setatr(1)
        CALL video(0)
        CALL cursor(0)
      ELSE
        CALL gotoxy(0,0)
        CALL screen(0)
        CALL video(1)
        CALL gotoxy(28,12)
        CALL print('CHAINING TO NEXT MODULE$',23,-2)
        IF(icolr.EQ. 1) CALL setatr(1)
      END IF
 
      END
C
      SUBROUTINE OPTIONS ()
C     *****************************************************************
C     *    LOOK FOR SETUP.DAT FILE AND ANALYZE OPTIONS                *
C     *****************************************************************
C
      LOGICAL         static,  shadow, finding_nul, exists
      INTEGER*2       IOK, SET, jdrve
      CHARACTER       str_col
      CHARACTER*32    file_path, string
      DIMENSION       str_col(32)
 
      COMMON /HLPCOL/ IATR
      COMMON /DRWSET/ SET(960), static, shadow
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore, iold_data,
     .                icompr
      COMMON /LIMITS/ IOK(800)
      COMMON /FILPTH/ i_path, file_path, length_path
      COMMON /TSTREF/ icheck_refrig
 
      EQUIVALENCE     (string, str_col(1))
 
C          COLOR MONITOR
 
      CALL getcol('MONO_SCREEN=YES$', 'SETUP.DAT ', iret)
      icolr = 1 - iret
      iatr = 07
      IF(iret .EQ. 0) iatr = 113
 
C          LOOK FOR PRINT OUT TO LPT2:
C
      CALL getcol('PRINT_PORT=LPT2$', 'SETUP.DAT ', iret)
      iport = iret
 
C          LOOK FOR A FORM_FEED REQUEST
 
      CALL getcol('FORM_FEED=YES$', 'SETUP.DAT ', iret)
      iform = 1 - iret
      iform = 0
 
C          LOOK FOR A PAGE_FEED REQUEST
 
      CALL getcol('PAGE_FEED=YES$', 'SETUP.DAT ', iret)
      ifeed = iret
 
C          LOOK FOR THE COMMAND TO OVERRIDE THE LIMITS CHECK
 
      CALL getcol('IGNORE_LIMITS=YES$', 'SETUP.DAT ', iret)
      ignore = iret
      DO i = 1, 800
        IOK(I) = 0
      END DO
 
C          LOOK FOR THE COMMAND TO OVERRIDE REFRIGERANT CHANGE CHECK
 
      CALL getcol('REFRIGERANT_CHECK=NO$', 'SETUP.DAT ', iret)
      icheck_refrig = 1 - iret
 
C          LOOK FOR A STATIC COVER PAGE
 
      static = .TRUE.
      CALL getcol('EXPAND$', 'SETUP.DAT ', iret)
      if(iret .EQ. 1) static = .FALSE.
      static = .FALSE.
 
C          LOOK FOR A SHADOW COMMAND
 
      shadow = .FALSE.
      CALL getcol('SHADOW$', 'SETUP.DAT ', iret)
      if(iret .EQ. 1) shadow = .TRUE.
      shadow = .TRUE.
 
C          LOOK FOR SKIP COMPRESSOR COMMAND
      icompr = 0
      CALL getcol('SKIP_COMPRESSOR=YES$', 'SETUP.DAT ', iret)
      if (iret .EQ. 1) icompr = 1
 
C          LOOK FOR A REQUEST TO SHOW OLD DATA
 
      CALL getcol('SHOW_OLD_DATA$', 'SETUP.DAT ', iret)
      iold_data = IRET
 
C          LOOK FOR A FILE PATH COMMAND
 
      CALL getcol('PATH=$', 'SETUP.DAT ', iret)
      i_path = IRET
 
      IF(i_path .EQ. 1) THEN
         CALL find_path(file_path, iret, length_path)
         IF(iret .eq. 1) i_path = 0
      END IF
 
      IF(i_path .EQ. 0) THEN
         CALL drive(0, jdrve, ndrive)
         idrive = jdrve
         CALL getdir(idrive+1, file_path)
 
         n = 1
         string = file_path
         finding_nul = .TRUE.
         DO WHILE (finding_nul)
            IF(str_col(n+2) .EQ. '$') THEN
               finding_nul = .FALSE.
               CYCLE
            END IF
            str_col(n) = str_col(n+2)
            n = n + 1
         END DO
 
         str_col(n) = '\'
         length_path = n
         i_path = 1
 
         DO i = n+1, 32
           str_col(i) = ' '
         END DO
 
         file_path = string
      END IF
 
      INQUIRE (FILE='new_path.pth', EXIST=exists)
      IF(exists) THEN
         OPEN (6, FILE='new_path.pth', STATUS='unknown')
         READ (6, '(A)') string
         CLOSE(6)
 
         i = 0
         finding_nul = .TRUE.
         DO WHILE (finding_nul)
            i = i + 1
            IF(str_col(i) .EQ. ' ') THEN
               i = i - 1
               finding_nul = .false.
            END IF
         END DO
 
         file_path = string
         length_path = i
         i_path = 1
      END IF
 
      RETURN
      END
C
      SUBROUTINE VIDEO(L)
C     *************************************************************
C     *           NORMAL AND REVERSE VIDEO CALLS                  *
C     *************************************************************
      LOGICAL         change_window
 
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore, iold_data,
     .                icompr
      COMMON /DISPLY/ change_window
C
C          BRANCH ACCORDING TO THE VALUE OF I
C
      i = l
      SELECT CASE (i)
        CASE (0)                                           !Clear only
          CALL gotoxy(0,0)
          CALL screen(0)
          CALL gotoxy(0,0)
 
        CASE (1)                                           !Draw VSET
          CALL window(0, 24, 0, 79, 32, 1)
          CALL gotoxy(36, 0)
          CALL print ('µ ERA Æ$',07,-2)
          CALL gotoxy(37,0)
          CALL atrbut1(5,79)
 
        CASE (2, 3)                                        !Quiting
          CALL gotoxy(0,0)
          CALL screen(0)
 
          CALL setatr(48)
          CALL window(0, 0, 0, 79, 32, 0)
          CALL gotoxy(37,0)
          CALL print(' ERA ',05,-2)
          CALL gotoxy(37,0)
          CALL atrbut1(05,79)
 
          CALL gotoxy(0,24)
          CALL atrbut1(80,112)
 
          IF(i .EQ. 2) THEN                                !Exploding window
            CALL setatr(63)
            CALL window(11, 12, 37, 42, 32, 0)
            CALL wait(1)
            CALL window(10, 13, 29, 50, 32, 0)
            CALL wait(1)
            CALL window(9, 14, 21, 58, 32, 0)
            CALL wait(1)
            CALL window(8, 15, 13, 66, 32, 0)
            CALL window(9, 14, 17, 62, 32, 1)
 
            CALL setatr(48)                                !Shadow
            IF(icolr .EQ. 1) THEN
              CALL window(16, 16, 15, 68, 219, 0)
              CALL window(09, 15, 67, 68, 219, 0)
            END IF
 
            CALL gotoxy(34,24)
            CALL print('Press Y or N$',12,-2)
          ELSE
            CALL gotoxy(23,24)
            CALL print('Press <Esc> Key to Abort Writing.$',34,-2)
          END IF
 
          CALL setatr(30)
 
        CASE (4)                                           !Run title
          CALL setatr(30)
          CALL gotoxy(0,0)
          CALL screen(0)
 
          CALL setatr(48)
          CALL window(0, 0, 0, 79, 32, 0)
 
          CALL gotoxy(04,0)
          CALL print('F1=Help',07,-2)
          CALL gotoxy(37,0)
          CALL print(' ERA ',05,-2)
          CALL gotoxy(37,0)
          CALL atrbut1(05,79)
          CALL gotoxy(69,0)
          CALL print('F9=Keys',07,-2)
 
          CALL setatr(112)
          CALL window(21, 24, 0, 79, 32, 0)
          CALL gotoxy(30,2)
          CALL print('TITLE OF ANALYSIS: $',19,-2)
          CALL gotoxy(0,0)
          CALL setatr(30)
 
      END SELECT
 
      change_window = .TRUE.
 
      RETURN
      END
C
      SUBROUTINE DRAW
C     ******************************************************************
C     *         SET UP THE TITLE PAGE                                  *
C     ******************************************************************
      LOGICAL         static, shadow
      CHARACTER       key
      INTEGER*2       SET
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore, iold_data,
     .                icompr
      COMMON /DRWSET/ SET(960), static, shadow
C
C          FLUSH THE KEYBOARD BUFFER
C
      CALL inchr(3,j,key)
      iatr = 15
      ichr = 176
C
C               SET UP THE PICTURE
C
      IF(icolr .eq. 1) THEN
         CALL window(0, 24, 0, 79, 32, 0)
      END IF
 
      CALL setatr(63)
      CALL window(13, 22, 12, 67, 32, 0)
      CALL window(14, 21, 14, 65, 32, 1)
      IF (shadow) THEN
        CALL setatr(48)
        CALL window(23, 23, 14, 69, 219, 0)
        CALL window(14, 22, 68, 69, 219, 0)
      ELSE
        CALL setatr(16)
        CALL window(23, 23, 14, 68, 223, 0)
        CALL window(14, 22, 68, 68, 219, 0)
      END IF
      CALL setatr(63)
      CALL pictre(78, SET(571))
 
      call gotoxy(55, 13)
      call print('Version 2.0', 11, -2)
 
      DO i = 1, 114
        k = 5*(i-1) + 3
        IF(SET(k) .EQ. 49) THEN
          IF (shadow) THEN
            SET(k) = 223
            SET(k+1) = 31
          ELSE
            SET(k) = 223
            SET(k+1) = 07
          END IF
        END IF
 
        IF(SET(k) .eq. 50) THEN
          IF (shadow) THEN
            SET(k) = 220
            SET(k+1) = 16
          ELSE
            SET(k) = 223
            SET(k+1) = 07
          END IF
        END IF
      END DO
 
      IF (.NOT. static) THEN
        CALL wait(3)
 
        CALL setatr(iatr)
        CALL window(06, 07, 12, 16, ichr, 0)
        CALL window(06, 07, 34, 40, ichr, 0)
        CALL window(06, 07, 15, 15, ichr, 0)
        CALL wait(1)
 
        CALL window(05, 08, 11, 16, ichr, 0)
        CALL window(05, 08, 34, 40, ichr, 0)
        CALL window(05, 08, 13, 16, ichr, 0)
        CALL wait(1)
 
        CALL window(04, 09, 10, 18, ichr, 0)
        CALL window(04, 09, 32, 42, ichr, 0)
        CALL window(04, 09, 11, 17, ichr, 0)
        CALL wait(1)
 
        CALL setatr(30)
        CALL window(03, 10, 09, 19, 032, 0)
        CALL window(03, 10, 30, 44, 032, 0)
        CALL window(03, 10, 53, 70, 032, 0)
      END IF
 
      CALL setatr(iatr)
      CALL pictre(24,SET)
      CALL pictre(42,SET(121))
      CALL pictre(48,SET(331))
      CALL setatr(30)
 
      RETURN
      END
c
      SUBROUTINE PEER
C     ******************************************************************
C     *         WARN USER THIS IS A PEER REVIEW VERSION OF ERA        *
C     ******************************************************************
      LOGICAL         static, shadow
      CHARACTER       key
      INTEGER*2       SET
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore, iold_data,
     .                icompr
      COMMON /DRWSET/ SET(960), static, shadow
C
C          FLUSH THE KEYBOARD BUFFER
C
      CALL getcol('WARNING$', 'SETUP.DAT ', iret)
      IF (iret .eq. 0) RETURN
 
      CALL setatr(79)
      CALL inchr(3,j,key)
      IF(icolr .eq. 1) THEN
         CALL window(0, 24, 0, 79, 32, 0)
      END IF
 
      CALL setatr(63)
      CALL window(09, 16, 12, 67, 32, 0)
      CALL window(10, 15, 14, 65, 32, 1)
 
      CALL setatr(48)
      CALL window(17, 17, 14, 69, 219, 0)
      CALL window(10, 16, 68, 69, 219, 0)
      CALL setatr(63)
 
      CALL gotoxy(17,12)
      CALL print('WARNING: This is a peer review version of ERA.',46,-2)
      CALL gotoxy(17,13)
      CALL print('Do not distribute outside your organization.',44,-2)
      CALL warble
      CALL warble
      CALL warble
 
      CALL inchr(1,J,KEY)
      CALL setatr(30)
      RETURN
      END
C
      SUBROUTINE FIND_PATH (file_path, iret, length_path)
C     ******************************************************************
C     *         READ SETUP.DAT FOR THE PATH DATA                     *
C     ******************************************************************
C
      CHARACTER       com_col, str_col
      CHARACTER*5     command
      CHARACTER*32    file_path, string
      DIMENSION       com_col(5), str_col(32)
      EQUIVALENCE    (command, com_col(1)),    (string, str_col(1))
 
      OPEN(1, FILE='SETUP.DAT', STATUS='UNKNOWN')
 
      iret = 1
      iocheck = 0
      DO WHILE (iocheck .NE. -1)
         READ (1, '(A, A)', IOSTAT=iocheck) command, string
         IF(iocheck .NE. 0) CYCLE
 
         DO I = 1, 5
            IF(com_col(i) .GE. 'a') THEN
               com_col(i) = char(ichar(com_col(i)) - 32)
            END IF
         END DO
 
         IF(command .EQ. 'path=' .OR. command .EQ. 'PATH=') THEN
            iret = 0
 
            i = 1
            DO WHILE (str_col(i) .GT. ' ')
               i = i + 1
            END DO
 
            IF(i .EQ. 1) THEN
               CLOSE(1)
               RETURN
            END IF
 
            DO k = 1, i                                    !Correct '/'
               IF(str_col(k) .EQ. '/') str_col(k) = '\'
            END DO
 
            iret = 0
            IF(str_col(i-1) .EQ. '\') THEN
               i = i - 1
            ELSE
               str_col(i) = '\'
            END IF
            iocheck = -1
 
            ncolon = 0                                     !Make sure a leading
            DO n = 1, i                                    !  \ is in place
               IF(str_col(n) .EQ. ':') ncolon = n
            END DO
 
            IF (str_col(ncolon+1) .NE. '\') THEN
               DO n = i, ncolon+1, -1
                  str_col(n+1) = str_col(n)
               END DO
               str_col(ncolon+1) = '\'
               i = i + 1
            END IF
 
            file_path = string
            length_path = i
 
         END IF
      END DO
      CLOSE (1)
 
      RETURN
      END
