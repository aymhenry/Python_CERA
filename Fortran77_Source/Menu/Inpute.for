      SUBROUTINE INPUTE(INSTR,IRET)
C     *****************************************************************
C     *         SUBSET OF THE INPUT ROUTINE FOR ERA                   *
C     *****************************************************************
      CHARACTER       form_feed
      CHARACTER*72    head_line
 
      INTEGER*2       IOK
 
      COMMON /CHANGE/ isav, ilpt1, numsel, ibypas, numpat, ibuf, ifile
      COMMON /DSKINP/ iwrite, iprint, iout
      COMMON /LIMITS/ IOK(800)
      COMMON /TITL/   ititl,TITLE(68,5)
      COMMON /EDTCOM/ icodet, inote
      COMMON /VALUES/ VAL(800), VALMN(800), VALMX(800)
      COMMON /LINCNT/ num3, num_lines, form_feed
 
      DATA iOL / 786 /, iIL / 789 /
C
C          SET UP THE FLAGS RELATING TO REFRIGERATOR TYPE
C
      IRFTYP = nfix(VAL(1))
      DOOR_GSKT = VAL(6) + VAL(7)
C
C          BRANCH ON INSTR (1 = EDIT DATA, 2 = PRINT THE DATA)
C
      iopt = 0
      go to (100, 120), instr
C
C          OUTPUT DATA EDIT MENU AND GET USER SELECTION
C
  100 CONTINUE
      IFLAG = 0
      IOPT = 4
      CALL EDTMEN(LOC)
      GO TO (110, 120, 130, 140, 170, 200, 210, 220, 230, 250, 260,
     .       270, 280, 310), LOC
C
C          EDIT RUN TITLE
C
  110 CONTINUE
      CALL VIDEO(4)
      CALL EDTFIL('BLOCK35.DAT', 0, 0, 0, 0, IFLAG, IOUT,
     .             VAL(1), VALMN(1), VALMX(1), IOK(1),1)
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      GO TO 100
C
C          REFRIGERATOR TYPE AND DIMENSIONS
C
  120 CONTINUE
 
      valmx(1) = 6 !!!!!!!lock-out type 7
      if (val(1) .GE. 7) val(1) = 6   !!!lock-out type 7
 
      icodet = 23
      num3 = 9
      WRITE(HEAD_LINE, '(''CABINET TYPE AND OVERALL DIMENSIONS'')')
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETIOK(1,2,2,2,1,2,2,1,1,0,0,0,0,0,IOK(1))
      CALL SETINT(0,2,2,2,0,2,2,0,0,0,0,0,0,0)
      IF(NFIX(VAL(1)) .NE. 4) THEN
         VALMX(3) = 120.0
         VALMX(4) = 100.0
         CALL EDTFIL('BLOCK01.DAT', 09, 09, 16, 0, IFLAG, IOUT, VAL(1),
     .                VALMN(1), VALMX(1), IOK(1),1)
 
         IF(NFIX(VAL(1)) .EQ. 4) GO TO 120
 
      ELSE
         VALMX(3) = 200.0
         VALMX(4) = 200.0
         CALL EDTFIL('BLOCK68.DAT', 09, 09, 16, 0, IFLAG, IOUT, VAL(1),
     .                VALMN(1), VALMX(1), IOK(1),1)
 
         IF(NFIX(VAL(1)) .NE. 4) GO TO 120
 
      END IF
 
      if (val(1) .GE. 7) val(1) = 6   !!!lock-out type 7
 
      icodet = 0
 
      DOOR_GSKT = VAL(6) + VAL(7)
      IRFTYP = nfix(VAL(1))
C
C          BRANCH ON ERROR FLAGS
C
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
C
C          CABINET LINERS
C
  125 CONTINUE
      WRITE(HEAD_LINE, '(''CABINET LINERS'')')
      CALL SETINT(0,2,2,0,2,2,0,0,0,0,0,0,0,0)
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETIOK(1,2,2,1,2,2,0,0,0,0,0,0,0,0,IOK(785))
      CALL EDTFIL('BLOCK79.DAT', 06, 06, 1, 0, IFLAG, IOUT,
     .            VAL(785), VALMN(785), VALMX(785), IOK(785),1)
C
C          BRANCH ON ERROR FLAGS
C
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 120
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IRFTYP .EQ. 4 .AND. IOPT  .EQ. 4) GO TO 100
      IF(IRFTYP .EQ. 4) go to 130
C
C          CABINET WEDGE
C
      WRITE(HEAD_LINE, '(''CABINET WEDGE DIMENSIONS'')')
      CALL SETINT(2,2,2,2,2,2,0,0,0,0,0,0,0,0)
      SELECT CASE (IRFTYP)
        CASE (1, 2, 3)                                     !Two cabinets
          num3 = 6
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETIOK(2,2,2,2,2,2,0,0,0,0,0,0,0,0,IOK(621))
          CALL EDTFIL('BLOCK60.DAT', 02, 06, 4, 2, IFLAG, IOUT,
     .                 VAL(621), VALMN(621), VALMX(621), IOK(621),1)
 
        CASE (5, 6, 7)
          num3 = 2
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETIOK(2,2,2,2,2,2,0,0,0,0,0,0,0,0,IOK(17))
          CALL EDTFIL('BLOCK60.DAT', 02, 06, 4, 1, IFLAG, IOUT,
     .                 VAL(17), VALMN(17), VALMX(17), IOK(17),1)
          if (irftyp .eq. 7) then
             val(622) = val(17)
             val(623) = val(18)
             val(625) = val(17)
             val(626) = val(18)
          end if
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 125
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IOPT  .EQ. 4) GO TO 100
C
C          REFRIGERATED VOLUMES
C
  130 CONTINUE
 
      WRITE(HEAD_LINE, '(''REFRIGERATED VOLUME'')')
 
      SELECT CASE (IRFTYP)
         CASE (1, 2, 3, 7)                                 !R/F
            num3 = 10
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(1,2,2,2,1,2,2,2,2,2,0,0,0,0,IOK(130))
            CALL SETINT(0,2,2,2,2,0,2,2,2,2,0,0,0,0)
            CALL EDTFIL('BLOCK14.DAT', 10, 10, 1, 0, IFLAG, IOUT,
     .                   VAL(130), VALMN(130), VALMX(130), IOK(130), 1)
 
         CASE (4)                                          !Chest freezer
            num3 = 5
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,2,0,0,0,0,0,0,0,0,0,IOK(035))
            CALL SETINT(2,2,2,2,2,0,0,0,0,0,0,0,0,0)
            CALL EDTFIL('BLOCK03.DAT', 05, 05, 1, 0, IFLAG, IOUT,
     .                   VAL(035), VALMN(035), VALMX(035), IOK(035), 1)
 
         CASE (5)                                          !Upright freezer
            num3 = 5
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(1,2,2,2,1,2,2,2,2,2,0,0,0,0,IOK(130))
            CALL SETINT(0,2,2,2,2,0,2,2,2,2,0,0,0,0)
            CALL EDTFIL('BLOCK14.DAT', 05, 05, 4, 1, IFLAG, IOUT,
     .                   VAL(130), VALMN(130), VALMX(130), IOK(130), 1)
 
         CASE (6)                                          !Refrigerator
            num3 = 6
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,2,2,0,0,0,0,0,0,0,0,IOK(155))
            CALL SETINT(2,2,2,2,2,2,0,0,0,0,0,0,0,0)
            CALL EDTFIL('BLOCK73.DAT', 06, 06, 1, 0, IFLAG, IOUT,
     .                   VAL(155), VALMN(155), VALMX(155), IOK(155), 1)
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
C
C          COMPRESSOR COMPARTMENT DIMENSIONS FOR TYPE 1, 2, 3, 5, 6, and 7
C
  135 CONTINUE
      SELECT CASE (IRFTYP)                                                !!!
         CASE (1, 2, 3, 5, 6, 7)                                          !!!
 
            WRITE(HEAD_LINE, '(''COMPRESSOR COMPARTMENT DIMENSIONS '')')
            num3 = 3
            CALL BLOCK_HEAD(HEAD_LINE)
 
  !         IF (IRFTYP .NE. 3) THEN
               CALL SETINT(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
               CALL SETIOK(2,2,2,0,0,0,0,0,0,0,0,0,0,0,IOK(057))
 
               CALL EDTFIL('BLOCK33.DAT', 03, 03, 1, 0, IFLAG, IOUT,
     .                 VAL(057), VALMN(057), VALMX(057), IOK(057) ,1)
  !         ELSE
  !            CALL SETINT(0,2,2,2,0,2,2,2,0,0,0,0,0,0)
  !            CALL SETIOK(1,2,2,2,1,2,2,2,0,0,0,0,0,0,IOK(710))
  !
  !            CALL EDTFIL('BLOCK66.DAT', 08, 08, 1, 0, IFLAG, IOUT,
  !  .                 VAL(710), VALMN(710), VALMX(710), IOK(710) ,1)
  !         END IF
                                                                          !!!
            IF(VAL(058) .LT. VAL(057)) THEN
               CALL error (2)
               CALL gotoxy(18, 11)
               CALL print('The Top Depth of the Compressor Compartment',
     .                                                           43, -2)
               CALL gotoxy(18, 12)
               CALL print('is Greater Than the Bottom Depth.',33,-2)
 
               CALL inchr(1,j,key)
               GO TO 135
             END IF
 
            IF(IFLAG .EQ. 1) GO TO 320                                    !!!
            IF(IFLAG .EQ. 2) GO TO 130                                    !!!
            IF(IFLAG .EQ. 3) GO TO 100                                    !!!
      END SELECT                                                          !!!
 
      IF(IOPT  .EQ. 4) GO TO 100
C
C          FREEZER (OR REFRIGERATOR) INSULATION THICKNESSES
C
  140 CONTINUE
 
      IF (irftyp .NE. 6) THEN
         WRITE(HEAD_LINE, '(''FREEZER INSULATION THICKNESS'')')
      ELSE
         WRITE(HEAD_LINE, '(''REFRIGERATOR INSULATION THICKNESS'')')
      END IF
 
      CALL SETINT(2,2,2,2,2,0,0,0,0,0,0,0,0,0)
 
      icodet = 24
      SELECT CASE (IRFTYP)
         CASE(1)                                           !Top mount
            num3 = 4
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,0,0,0,0,0,0,0,0,0,0,IOK(41))
            CALL EDTFIL('BLOCK08.DAT', 04, 04, 1, 0, IFLAG, IOUT,
     .                   VAL(41), VALMN(41), VALMX(41), IOK(41), 1)
 
         CASE(2)                                           !Bottom mount
            num3 = 4
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,0,0,0,0,0,0,0,0,0,0,IOK(42))
            CALL EDTFIL('BLOCK05.DAT', 01, 04, 4, 2, IFLAG, IOUT,
     .                   VAL(42), VALMN(42), VALMX(42), IOK(42), 1)
 
         CASE (3, 5)                                       !Full freezer
            num3 = 5
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,2,0,0,0,0,0,0,0,0,0,IOK(41))
            CALL EDTFIL('BLOCK05.DAT', 05, 05, 1, 0, IFLAG, IOUT,
     .                   VAL(41), VALMN(41), VALMX(41), IOK(41), 1)
 
         CASE (4)                                          !Chest freezer
            num3 = 5
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,2,0,0,0,0,0,0,0,0,0,IOK(11))
            CALL EDTFIL('BLOCK15.DAT', 05, 05, 1, 0, IFLAG, IOUT,
     .                   VAL(11), VALMN(11), VALMX(11), IOK(11), 1)
 
         CASE (6)                                       !Refrigerator
            num3 = 5
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,2,0,0,0,0,0,0,0,0,0,IOK(41))
            CALL EDTFIL('BLOCK74.DAT', 05, 05, 1, 0, IFLAG, IOUT,
     .                   VAL(41), VALMN(41), VALMX(41), IOK(41), 1)
 
         CASE(7)                                           !One-Door R/F
            num3 = 3
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,0,0,0,0,0,0,0,0,0,0,IOK(41))
            CALL EDTFIL('BLOCK79.DAT', 03, 03, 1, 0, IFLAG, IOUT,
     .                   VAL(41), VALMN(41), VALMX(41), IOK(41), 1)
 
      END SELECT
      icodet = 0
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
C
C          FREEZER INSULATION PROPERTIES
C
  150 CONTINUE
 
      ICODET = 99
      IF (irftyp .NE. 6) THEN
         WRITE(HEAD_LINE, '(''FREEZER INSULATION RESISTIVITIES'')')
      ELSE
         WRITE(HEAD_LINE, '(''REFRIGERATOR INSULATION RESISTIVITIES'')')
      END IF
 
      CALL SETINT(3,3,3,3,3,3,3,0,0,0,0,0,0,0)
 
      SELECT CASE (IRFTYP)
         CASE (1)                                          !Top mount
            num3 = 6
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 1
            CALL SETIOK(1,2,2,2,2,2,0,0,0,0,0,0,0,0,IOK(95))
            CALL EDTFIL('BLOCK20.DAT', 06, 06, 1, 0, IFLAG, IOUT,
     .                   VAL(95), VALMN(95), VALMX(95), IOK(95), 1)
 
         CASE (2)                                          !Bottom mount
            num3 = 6
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 1
            CALL SETIOK(1,2,2,2,2,2,0,0,0,0,0,0,0,0,IOK(95))
            CALL EDTFIL('BLOCK21.DAT', 06, 06, 1, 0, IFLAG, IOUT,
     .                   VAL(95), VALMN(95), VALMX(95), IOK(95), 1)
 
         CASE (3)                                          !Side by side
            num3 = 7
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 2
            CALL SETIOK(1,2,2,2,2,2,2,0,0,0,0,0,0,0,IOK(95))
            CALL EDTFIL('BLOCK16.DAT', 07, 07, 1, 0, IFLAG, IOUT,
     .                   VAL(95), VALMN(95), VALMX(95), IOK(95), 1)
 
         CASE (4)                                          !Chest freezer
            num3 = 3
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 3
            CALL SETIOK(1,2,2,2,0,0,0,0,0,0,0,0,0,0,IOK(91))
            CALL EDTFIL('BLOCK07.DAT', 03, 01, 4, 1, IFLAG, IOUT,
     .                   VAL(91), VALMN(91), VALMX(91), IOK(91), 1)
 
 !!      CASE (5, 6)                                       !Upright freezer
 !!         num3 = 4                                       ! and Refrigerator
 
         CASE (5)                                          !Upright freezer
            num3 = 4
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 3
            CALL SETIOK(1,2,2,2,0,0,0,0,0,0,0,0,0,0,IOK(91))
            CALL EDTFIL('BLOCK07.DAT', 04, 04, 1, 0, IFLAG, IOUT,
     .                   VAL(91), VALMN(91), VALMX(91), IOK(91), 1)
 
         CASE (6)                                          !Refrigerator
            num7 = 4
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 2
            CALL SETIOK(1,2,2,2,2,2,2,0,0,0,0,0,0,0,IOK(95))
            CALL EDTFIL('BLOCK02.DAT', 07, 07, 1, 0, IFLAG, IOUT,
     .                   VAL(95), VALMN(95), VALMX(95), IOK(95), 1)
 
         CASE (7)                                          !One-Door R/F
            num3 = 4
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 1
            CALL SETIOK(1,2,2,2,2,2,0,0,0,0,0,0,0,0,IOK(95))
            CALL EDTFIL('BLOCK80.DAT', 04, 04, 1, 0, IFLAG, IOUT,
     .                   VAL(95), VALMN(95), VALMX(95), IOK(95), 1)
 
      END SELECT
 
      IFLAGS = IFLAG
      ICODET = 0
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 140
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IFLAG .GE. 4) GO TO 160
      IF(IOPT  .EQ. 4) GO TO 100
      GO TO 170
C
C          CALCULATE RESISTIVITY DATA FOR FREEZER PANELS
C
  160 CONTINUE
 
      VAL59 = VAL(59)
      IF(VAL(58) .EQ. 0.0) VAL59 = 0.0
      CALL SETINT(2,2,2,2,2,2,2,2,2,2,2,2,2,0)
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top mount
            SELECT CASE (IFLAGS)
               CASE (5)                                    !Top panel
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER TOP WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL (iOL)
                  VAL(146) = VAL(41)
                  I_VAL   = 96
                  I_PANEL = 3
 
               CASE (6)                                    !Side panels
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER SIDE WALL RESISTIVITY'')')
                  VAL(140) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(141) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(146) = VAL(42)
                  I_VAL   = 97
                  I_PANEL = 1
 
               CASE (7)                                    !Back panel
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER BACK WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(146) = VAL(43)
                  I_VAL   = 98
                  I_PANEL = 2
 
               CASE (8)                                    !Door
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER DOOR RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(146) = VAL(44)
                  I_VAL   = 99
                  I_PANEL = 5
 
            END SELECT
 
         CASE (2)                                          !Bottom mount
            SELECT CASE (IFLAGS)
               CASE (5)                                    !Side panels
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER SIDE WALL RESISTIVITY'')')
                  VAL(140) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(141) = VAL(2) - VAL(31) - VAL(32) - 0.1 * VAL(iOL)
                  VAL(146) = VAL(42)
                  I_VAL   = 96
                  I_PANEL = 1
 
               CASE (6)                                    !Back panel
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER BACK WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(2) - VAL(31) - VAL(32)
     .                                        - VAL59 - 0.1 * VAL(iOL)
                  VAL(146) = VAL(43)
                  I_VAL   = 97
                  I_PANEL = 2
 
               CASE (7)                                    !Bottom panel
                  WRITE(HEAD_LINE,
     .               '(''CALCULATE FREEZER BOTTOM WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(44)
                  I_VAL   = 98
                  I_PANEL = 4
 
               CASE (8)                                    !Door
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER DOOR RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(2) - VAL(31) - VAL(32) - 0.1 * VAL(iOL)
                  VAL(146) = VAL(45)
                  I_VAL   = 99
                  I_PANEL = 5
 
            END SELECT
 
         CASE (3)                                          !Side by side
            SELECT CASE (IFLAGS)
               CASE (5)                                    !Top panel
                  WRITE(HEAD_LINE,
     .                  '(''CALCULATE FREEZER TOP WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - VAL(31) - VAL(32) - 0.1*VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(41)
                  I_VAL   = 96
                  I_PANEL = 3
 
               CASE (6)                                    !Side panels
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER SIDE WALL RESISTIVITY'')')
                  VAL(140) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(141) = VAL(02) - 0.2 * VAL(iOL)
                  VAL(146) = VAL(42)
                  I_VAL   = 97
                  I_PANEL = 1
 
               CASE (7)                                    !Back panel
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER BACK WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - VAL(31) - VAL(32) - 0.1*VAL(iOL)
                  VAL(141) = VAL(02) - VAL59 - 0.2 * VAL(iOL)
                  VAL(146) = VAL(43)
                  I_VAL   = 98
                  I_PANEL = 2
 
               CASE (8)                                   !Bottom panel
                  WRITE(HEAD_LINE,
     .               '(''CALCULATE FREEZER BOTTOM WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - VAL(31) - VAL(32) - 0.1*VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(44)
                  I_VAL   = 99
                  I_PANEL = 4
 
               CASE (9)                                    !Door
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE FREEZER DOOR RESISTIVITY'')')
                  VAL(140) = VAL(03) - VAL(31) - VAL(32) - 0.1*VAL(iOL)
                  VAL(141) = VAL(02) - 0.2 * VAL(iOL)
                  VAL(146) = VAL(45)
                  I_VAL   = 100
                  I_PANEL = 5
 
            END SELECT
 
         CASE (4)                                          !Freezers
            SELECT CASE (IFLAGS)
               CASE (5)                                    !Panels
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE CABINET WALL RESISTIVITY'')')
                  VAL(140) = SQRT(VAL(03)*VAL(04)) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(02) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(12)
                  I_VAL   = 92
                  I_PANEL = 2
 
               CASE (6)                                    !Door
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE CABINET DOOR RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(04) - 0.2 * VAL(iOL)
                  VAL(146) = VAL(11)
                  I_VAL   = 93
                  I_PANEL = 5
 
            END SELECT
 
 !!      CASE (5, 6)                                       !Upright Freezer
 !!         SELECT CASE (IFLAGS)                           !  or Refrigerator
 
         CASE (5)                                          !Upright Freezer
            SELECT CASE (IFLAGS)
               CASE (5)                                    !Panels
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE CABINET WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(02) - 0.2 * VAL(iOL)
                  VAL(146) = VAL(43)
                  I_VAL   = 92
                  I_PANEL = 2
 
               CASE (6)                                    !Door
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE CABINET DOOR RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(02) - 0.2 * VAL(iOL)
                  VAL(146) = VAL(45)
                  I_VAL   = 93
                  I_PANEL = 5
 
            END SELECT
 
         CASE (6)                                          !Refrigerator
            SELECT CASE (IFLAGS)
               CASE (5)                                    !Top panel
                  WRITE(HEAD_LINE,
     .              '(''CALCULATE REFRIGERATOR TOP WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2*VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(41)
                  I_VAL   = 96
                  I_PANEL = 3
 
               CASE (6)                                    !Side panels
                  WRITE(HEAD_LINE,
     .             '(''CALCULATE REFRIGERATOR SIDE WALL RESISTIVITY'')')
                  VAL(140) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(141) = VAL(02) - 0.2 * VAL(iOL)
                  VAL(146) = VAL(42)
                  I_VAL   = 97
                  I_PANEL = 1
 
               CASE (7)                                    !Back panel
                  WRITE(HEAD_LINE,
     .             '(''CALCULATE REFRIGERATOR BACK WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2*VAL(iOL)
                  VAL(141) = VAL(02) - VAL59 - 0.2 * VAL(iOL)
                  VAL(146) = VAL(43)
                  I_VAL   = 98
                  I_PANEL = 2
 
               CASE (8)                                   !Bottom panel
                  WRITE(HEAD_LINE,
     .           '(''CALCULATE REFRIGERATOR BOTTOM WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2*VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(44)
                  I_VAL   = 99
                  I_PANEL = 4
 
               CASE (9)                                    !Door
                  WRITE(HEAD_LINE,
     .                '(''CALCULATE REFRIGERATOR DOOR RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2*VAL(iOL)
                  VAL(141) = VAL(02) - 0.2 * VAL(iOL)
                  VAL(146) = VAL(45)
                  I_VAL   = 100
                  I_PANEL = 5
 
            END SELECT
 
      END SELECT
 
  165 continue
      icodet = -1
      inote = 2
      CALL BLOCK_HEAD(HEAD_LINE)
      inote = 0
 
      CALL SETIOK(2,2,2,2,2,2,2,1,1,2,2,2,2,0,IOK(140))
      CALL EDTFIL('BLOCK17.DAT', 13, 13, 1, 0, IFLAG, IOUT,
     .             VAL(140), VALMN(140), VALMX(140), IOK(140), 1)
      icodet = 0
      if(iflag .gt. 3) go to 165
 
      IF(IFLAG .EQ. 1) GO TO 150
      IF(IFLAG .EQ. 3) GO TO 150
 
      CALL CALINS(VAL, I_PANEL, R_NET)
      VAL(I_VAL) = R_NET
 
      GO TO 150
C
C          FRESH FOOD INSULATION THICKNESSES
C
  170 CONTINUE
      IF(IRFTYP .GE. 4 .and. irftyp .le. 6) GO TO 210
 
      WRITE(HEAD_LINE, '(''FRESH FOOD INSULATION THICKNESS'')')
      CALL SETINT(2,2,2,2,2,0,0,0,0,0,0,0,0,0)
 
      icodet = 24
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top mount
            num3 = 4
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,0,0,0,0,0,0,0,0,0,0,IOK(52))
            CALL EDTFIL('BLOCK06.DAT', 01, 04, 4, 2, IFLAG, IOUT,
     .                   VAL(52), VALMN(52), VALMX(52), IOK(52), 1)
 
         CASE (2)                                          !Bottom mount
            num3 = 4
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,0,0,0,0,0,0,0,0,0,0,IOK(51))
            CALL EDTFIL('BLOCK19.DAT', 04, 04, 1, 0, IFLAG, IOUT,
     .                   VAL(51), VALMN(51), VALMX(51), IOK(51), 1)
 
         CASE (3)
            num3 = 5
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,2,2,0,0,0,0,0,0,0,0,0,IOK(51))
            CALL EDTFIL('BLOCK06.DAT', 05, 05, 1, 0, IFLAG, IOUT,
     .                   VAL(51), VALMN(51), VALMX(51), IOK(51), 1)
 
      END SELECT
      icodet = 0
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
C
C          FRESH FOOD INSULATION PROPERTIES
C
  180 CONTINUE
      ICODET = 99
 
      WRITE(HEAD_LINE, '(''FRESH FOOD INSULATION RESISTIVITIES'')')
      CALL SETINT(3,3,3,3,3,3,3,0,0,0,0,0,0,0)
 
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top mount
            num3 = 6
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 4
            CALL SETIOK(1,2,2,2,2,2,0,0,0,0,0,0,0,0,IOK(21))
            CALL EDTFIL('BLOCK10.DAT', 07, 06, 4, 2, IFLAG, IOUT,
     .                   VAL(21), VALMN(21), VALMX(21), IOK(21), 1)
 
         CASE (2)                                          !Bottom mount
            num3 = 6
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 4
            CALL SETIOK(1,2,2,2,2,2,0,0,0,0,0,0,0,0,IOK(21))
            CALL EDTFIL('BLOCK22.DAT', 06, 06, 1, 0, IFLAG, IOUT,
     .                   VAL(21), VALMN(21), VALMX(21), IOK(21), 1)
 
         CASE (3)                                          !Side by side
            num3 = 7
            CALL BLOCK_HEAD(HEAD_LINE)
 
            ICODET = 5
            CALL SETIOK(1,2,2,2,2,2,2,0,0,0,0,0,0,0,IOK(21))
            CALL EDTFIL('BLOCK10.DAT', 07, 06, 4, 1, IFLAG, IOUT,
     .                   VAL(21), VALMN(21), VALMX(21), IOK(21), 1)
 
      END SELECT
 
      IFLAGS = IFLAG
      ICODET = 0
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 170
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IFLAG .GE. 4) GO TO 190
      IF(IOPT  .EQ. 4) GO TO 100
      GO TO 200
C
C          CALCULATE RESISTIVITY DATA FOR FRESH FOOD PANELS
C
  190 CONTINUE
      VAL59 = VAL(59)
      IF(VAL(58) .EQ. 0.0) VAL59 = 0.0
 
      CALL SETINT(2,2,2,2,2,2,2,2,2,2,2,2,2,0)
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top mount
            SELECT CASE (IFLAGS)
               CASE (5)                                    !Side panels
                  WRITE(HEAD_LINE,
     .              '(''CALCULATE FRESH FOOD SIDE WALL RESISTIVITY'')')
                  VAL(140) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(141) = VAL(2) - VAL(31) - VAL(32) - 0.1*VAL(iOL)
                  VAL(146) = VAL(52)
                  I_VAL   = 22
                  I_PANEL = 1
 
               CASE (6)                                    !Back panel
                  WRITE(HEAD_LINE,
     .              '(''CALCULATE FRESH FOOD BACK WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(2) - VAL(31) - VAL(32)
     .                                        - VAL59 - 0.1 * VAL(iOL)
                  VAL(146) = VAL(53)
                  I_VAL   = 23
                  I_PANEL = 2
 
               CASE (7)                                    !Bottom panel
                  WRITE(HEAD_LINE,
     .            '(''CALCULATE FRESH FOOD BOTTOM WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(54)
                  I_VAL   = 24
                  I_PANEL = 4
 
               CASE (8)                                    !Door
                  WRITE(HEAD_LINE,
     .            '(''CALCULATE FRESH FOOD DOOR RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(2) - VAL(31) - VAL(32) - 0.1*VAL(iOL)
                  if (irftyp .eq. 7) val(141) = val(02)
                  VAL(146) = VAL(55)
                  I_VAL   = 25
                  I_PANEL = 5
 
            END SELECT
 
         CASE (2)                                          !Bottom mount
            SELECT CASE (IFLAGS)
               CASE (5)                                    !Top panel
                  WRITE(HEAD_LINE,
     .            '(''CALCULATE FRESH FOOD TOP WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(51)
                  I_VAL   = 22
                  I_PANEL = 3
 
               CASE (6)                                    !Side panels
                  WRITE(HEAD_LINE,
     .              '(''CALCULATE FRESH FOOD SIDE WALL RESISTIVITY'')')
                  VAL(140) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(141) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(146) = VAL(52)
                  I_VAL   = 23
                  I_PANEL = 1
 
               CASE (7)                                    !Back panel
                  WRITE(HEAD_LINE,
     .              '(''CALCULATE FRESH FOOD BACK WALL RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(146) = VAL(53)
                  I_VAL   = 24
                  I_PANEL = 2
 
               CASE (8)                                    !Door
                  WRITE(HEAD_LINE,
     .            '(''CALCULATE FRESH FOOD DOOR RESISTIVITY'')')
                  VAL(140) = VAL(03) - 0.2 * VAL(iOL)
                  VAL(141) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(146) = VAL(54)
                  I_VAL   = 25
                  I_PANEL = 5
 
            END SELECT
 
         CASE (3)                                          !Side by side
            SELECT CASE (IFLAGS)
               CASE (5)                                    !Top panel
                  WRITE(HEAD_LINE,
     .            '(''CALCULATE FRESH FOOD TOP WALL RESISTIVITY'')')
                  VAL(140) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(51)
                  I_VAL   = 22
                  I_PANEL = 3
 
               CASE (6)                                    !Side panels
                  WRITE(HEAD_LINE,
     .              '(''CALCULATE FRESH FOOD SIDE WALL RESISTIVITY'')')
                  VAL(140) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(141) = VAL(02) - 0.2 * VAL(iOL)
                  VAL(146) = VAL(52)
                  I_VAL   = 23
                  I_PANEL = 1
 
               CASE (7)                                    !Back panel
                  WRITE(HEAD_LINE,
     .              '(''CALCULATE FRESH FOOD BACK WALL RESISTIVITY'')')
                  VAL(140) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(141) = VAL(02) - VAL59 - 0.2 * VAL(iOL)
                  VAL(146) = VAL(53)
                  I_VAL   = 24
                  I_PANEL = 2
 
               CASE (8)                                    !Bottom panel
                  WRITE(HEAD_LINE,
     .            '(''CALCULATE FRESH FOOD BOTTOM WALL RESISTIVITY'')')
                  VAL(140) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(141) = VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL)
                  VAL(146) = VAL(54)
                  I_VAL   = 25
                  I_PANEL = 4
 
               CASE (9)                                    !Door
                  WRITE(HEAD_LINE,
     .            '(''CALCULATE FRESH FOOD DOOR RESISTIVITY'')')
                  VAL(140) = VAL(31) - 0.1 * VAL(iOL)
                  VAL(141) = VAL(02) - 0.2 * VAL(iOL)
                  VAL(146) = VAL(55)
                  I_VAL   = 26
                  I_PANEL = 5
 
            END SELECT
 
      END SELECT
 
  195 continue
      icodet = -1
      inote = 2
      CALL BLOCK_HEAD(HEAD_LINE)
      inote = 0
 
      CALL SETIOK(2,2,2,2,2,2,2,1,1,2,2,2,2,0,IOK(140))
      CALL EDTFIL('BLOCK17.DAT', 13, 13, 1, 0, IFLAG, IOUT,
     .             VAL(140), VALMN(140), VALMX(140), IOK(140), 1)
      icodet = 0
      if(iflag .gt. 3) go to 195
 
      IF(IFLAG .EQ. 1) GO TO 180
      IF(IFLAG .EQ. 3) GO TO 180
 
      CALL CALINS(VAL, I_PANEL, R_NET)
      VAL(I_VAL) = R_NET
 
      GO TO 180
C
C          MULLION
C
  200 CONTINUE
      IF(IRFTYP .GE. 4 .and. irftyp .le. 6) GO TO 210
 
      num3 = 3
      WRITE(HEAD_LINE, '(''MULLION'')')
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
 
      icodet = 25
      SELECT CASE (IRFTYP)
         CASE (1, 2, 7)
            CALL SETIOK(2,2,2,0,0,0,0,0,0,0,0,0,0,0,IOK(31))
            CALL EDTFIL('BLOCK04.DAT', 03, 03, 4, 1, IFLAG, IOUT,
     .                   VAL(31), VALMN(31), VALMX(31), IOK(31), 1)
 
         CASE (3)
            CALL SETIOK(2,2,2,0,0,0,0,0,0,0,0,0,0,0,IOK(31))
            CALL EDTFIL('BLOCK04.DAT', 03, 03, 4, 2, IFLAG, IOUT,
     .                   VAL(31), VALMN(31), VALMX(31), IOK(31), 1)
 
      END SELECT
      icodet = 0
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IOPT  .EQ. 4) GO TO 100
C
C          AIR AND CABINET SETPOINT TEMPERATURES
C
  210 CONTINUE
 
      WRITE(HEAD_LINE, '(''AIR AND CABINET TEMPERATURES'')')
      CALL SETINT(2,2,2,2,2,0,0,0,0,0,0,0,0,0)
 
      SELECT CASE (IRFTYP)
         CASE (1, 2, 3, 6, 7)
            num3 = 5
            CALL BLOCK_HEAD(HEAD_LINE)
 
            VALMX(82) = -2
            CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(81))
            CALL EDTFIL('BLOCK09.DAT', 05, 04, 4, 1, IFLAG, IOUT,
     .                   VAL(81), VALMN(81), VALMX(81), IOK(81), 1)
 
         CASE (4, 5)
            num3 = 4
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(86))
            CALL EDTFIL('BLOCK09.DAT', 05, 04, 4, 2, IFLAG, IOUT,
     .                   VAL(86), VALMN(86), VALMX(86), IOK(86), 1)
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IOPT  .EQ. 4) GO TO 100
C
C          DOOR OPENING SCHEDULE
C
  220 CONTINUE
 
      WRITE(HEAD_LINE, '(''DOOR OPENING SCHEDULE'')')
 
      SELECT CASE (IRFTYP)
         CASE (1:3)
            num3 = 6
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETINT(2,2,2,2,2,0,0,0,0,0,0,0,0,0)
            CALL SETIOK(2,2,2,2,2,1,0,0,0,0,0,0,0,0,IOK(108))
            CALL EDTFIL('BLOCK12.DAT', 06, 04, 4, 1, IFLAG, IOUT,
     .                   VAL(108), VALMN(108), VALMX(108), IOK(108), 1)
 
         CASE (4, 5, 7)
            num3 = 4
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETINT(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
            CALL SETIOK(2,2,2,1,0,0,0,0,0,0,0,0,0,0,IOK(114))
            CALL EDTFIL('BLOCK12.DAT', 06, 04, 4, 2, IFLAG, IOUT,
     .                   VAL(114), VALMN(114), VALMX(114), IOK(114), 1)
 
         CASE (6)
            num3 = 3
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETINT(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
            CALL SETIOK(2,2,2,0,0,0,0,0,0,0,0,0,0,0,IOK(114))
            CALL EDTFIL('BLOCK75.DAT', 03, 03, 1, 0, IFLAG, IOUT,
     .                   VAL(114), VALMN(114), VALMX(114), IOK(114), 1)
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IOPT  .EQ. 4) GO TO 100
C
C          GASKET HEAT LEAKS
C
  230 CONTINUE
 
      WRITE(HEAD_LINE, '(''GASKET HEAT LEAKS (W/M-100C)'')')
      CALL SETINT(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
 
      SELECT CASE (IRFTYP)
         CASE (1:3)
            num3 = 2
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,2,2,0,0,0,0,0,0,0,0,0,0,0,IOK(104))
            CALL EDTFIL('BLOCK11.DAT', 02, 01, 4, 1, IFLAG, IOUT,
     .                   VAL(104), VALMN(104), VALMX(104), IOK(104), 1)
 
         CASE (4, 5, 6, 7)
            num3 = 1
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(106))
            CALL EDTFIL('BLOCK11.DAT', 02, 01, 4, 2, IFLAG, IOUT,
     .                   VAL(106), VALMN(106), VALMX(106), IOK(106), 1)
 
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IOPT  .EQ. 4) GO TO 100
C
C          DEFROST AND CONTROLS ENERGY USE
C
  250 CONTINUE
 
      IF (irftyp .NE. 6) THEN
         WRITE(HEAD_LINE, '(''DEFROST AND CONTROLS ENERGY USE'')')
      ELSE
         WRITE(HEAD_LINE, '(''CONTROLS ENERGY USE'')')
      END IF
 
      val765 = val(765)
      CALL SETINT(2,2,2,2,2,2,2,2,2,2,2,2,0,0)
      SELECT CASE (IRFTYP)
         CASE (1, 2, 3, 7)
            num3 = 14
            CALL BLOCK_HEAD(HEAD_LINE)
 
            val(765) = -99.0
            CALL SETIOK(1,2,2,2,1,2,2,2,1,2,2,2,1,1,IOK(710))
            CALL EDTFIL('BLOCK69.DAT', 14, 12, 4, 1, IFLAG, IOUT,
     .                   VAL(710), VALMN(710), VALMX(710), IOK(710), 1)
            val(765) = val765
 
         CASE (4, 5)
            num3 = 12
            CALL BLOCK_HEAD(HEAD_LINE)
 
            val(765) = -99.0
            CALL SETIOK(1,2,2,2,1,2,2,1,2,2,1,1,0,0,IOK(755))
            CALL EDTFIL('BLOCK69.DAT', 14, 12, 4, 2, IFLAG, IOUT,
     .                   VAL(755), VALMN(755), VALMX(755), IOK(755), 1)
            val(765) = val765
 
         CASE (6)
            num3 = 8
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(1,2,2,1,2,2,2,1,0,0,0,0,0,0,IOK(759))
            CALL EDTFIL('BLOCK76.DAT', 8, 8, 1, 0, IFLAG, IOUT,
     .                   VAL(759), VALMN(759), VALMX(759), IOK(759), 1)
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IOPT  .EQ. 4) GO TO 100
C
C          ELECTRIC ANTI-SWEAT HEAT
C
  260 CONTINUE
 
      WRITE(HEAD_LINE, '(''ELECTRIC ANTI-SWEAT HEAT'')')
      CALL SETINT(2,2,2,2,2,2,2,2,2,2,2,2,0,0)
 
      SELECT CASE (IRFTYP)
         CASE (1:3)
            num3 = 12
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(1,2,1,1,1,2,1,1,1,2,1,1,0,0,IOK(725))
            CALL EDTFIL('BLOCK70.DAT', 12, 02, 4, 1, IFLAG, IOUT,
     .                   VAL(725), VALMN(725), VALMX(725), IOK(725), 1)
 
 
C                Check consistency of data
 
             IF((VAL(735) + VAL(736)) .GT. 1.0) THEN
               CALL error (2)
               CALL gotoxy(24, 11)
               CALL print('The Sum of the Mullion Heat Leak',32,-2)
               CALL gotoxy(24, 12)
               CALL print('Fractions is Greater Than One.',30,-2)
 
               CALL inchr(1,j,key)
               GO TO 260
             END IF
 
         CASE (4, 5, 6, 7)
            num3 = 2
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(2,1,0,0,0,0,0,0,0,0,0,0,0,0,IOK(737))
            CALL EDTFIL('BLOCK70.DAT', 12, 02, 4, 2, IFLAG, IOUT,
     .                   VAL(737), VALMN(737), VALMX(737), IOK(737), 1)
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IOPT  .EQ. 4) GO TO 100
C
C          REFRIGERANT LINE ANTI-SWEAT HEAT
C
  270 CONTINUE
 
      WRITE(HEAD_LINE,'(''REFRIGERANT LINE ANTI-SWEAT HEAT'')')
      CALL SETINT(0,2,2,2,2,2,2,2,2,2,2,2,2,2)
 
      SELECT CASE (IRFTYP)
         CASE (1:3)                                        !R/F
            num3 = 14
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(1,1,1,2,1,1,1,2,1,1,1,2,1,1,IOK(740))
            CALL EDTFIL('BLOCK71.DAT', 14, 03, 4, 1, IFLAG, IOUT,
     .                   VAL(740), VALMN(740), VALMX(740), IOK(740), 1)
 
C                Check consistency of data
 
             IF((VAL(752) + VAL(753)) .GT. 1.0) THEN
               CALL error (2)
               CALL gotoxy(24, 11)
               CALL print('The Sum of the Mullion Heat Leak',32,-2)
               CALL gotoxy(24, 12)
               CALL print('Fractions is Greater Than One.',30,-2)
 
               CALL inchr(1,j,key)
               GO TO 270
             END IF
 
 
         CASE (4, 5, 6, 7)
            num3 = 3
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL SETIOK(1,2,1,0,0,0,0,0,0,0,0,0,0,0,IOK(770))
            CALL EDTFIL('BLOCK71.DAT', 14, 03, 4, 2, IFLAG, IOUT,
     .                   VAL(770), VALMN(770), VALMX(770), IOK(770), 1)
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
      IF(IOPT  .EQ. 4) GO TO 100
C
C          PENETRATIONS HEAT INPUTS (THROUGH THE DOOR)
C
  280 CONTINUE
 
      WRITE(HEAD_LINE, '(''PENETRATION HEAT INPUT'')')
 
      CALL SETINT(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
      CALL SETIOK(2,2,2,0,0,0,0,0,0,0,0,0,0,0,IOK(555))
      SELECT CASE (IRFTYP)
         CASE (1:3)
            num3 = 2
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL EDTFIL('BLOCK72.DAT', 02, 01, 4, 1, IFLAG, IOUT,
     .                   VAL(555), VALMN(555), VALMX(555), IOK(555), 1)
 
         CASE (4, 5, 6, 7)
            num3 = 1
            CALL BLOCK_HEAD(HEAD_LINE)
 
            CALL EDTFIL('BLOCK72.DAT', 02, 01, 4, 2, IFLAG, IOUT,
     .                   VAL(557), VALMN(557), VALMX(557), IOK(557), 1)
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 320
      IF(IFLAG .EQ. 2) GO TO 300
      IF(IFLAG .EQ. 3) GO TO 100
 
      IF(IPRINT .EQ. 1) GO TO 400
      GO TO 100
C
C          HANDLE <PgUp> COMMAND
C
  300 CONTINUE
      IBYPAS = -1
      GO TO 100
C
C          A BRANCH BACK TO 30 IS GIVEN IRET = 3
C
  310 CONTINUE
      IRET = 3
C     CALL CHKCAB(VAL,IFLAG)
      IF(IFLAG .NE. 0) GO TO 100
      GO TO 400
C
C          A BRANCH BACK TO 20 IS GIVEN IRET = 2
C
  320 CONTINUE
      IRET = 2
C     CALL CHKCAB(VAL,IFLAG)
      IF(IFLAG .NE. 0) GO TO 100
      GO TO 400
 
C
C        Handle Case (6) -- Refrigerator Cabinet Loads
C
  400 CONTINUE
      IF (IRFTYP .EQ. 6) THEN           !! Refrigerator
         A_TOP = (VAL(03) - 0.2*VAL(iOL))
     .         * (VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL))
 
         A_SIDE = (VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL))
     .          * (VAL(02) - 0.2 * VAL(iOL))
 
         A_BACK = (VAL(03) - 0.2*VAL(iOL))
     .          * (VAL(02) - VAL59 - 0.2 * VAL(iOL))
 
         A_BOTM = (VAL(03) - 0.2*VAL(iOL))
     .          * (VAL(04) - DOOR_GSKT - 0.2 * VAL(iOL))
 
         VAL(91) =  A_TOP * VAL(96) + A_SIDE * VAL(97)
     .           + A_BACK * VAL(98) + A_BOTM * VAL(99)
         VAL(91) = VAL(91) / (A_TOP + A_SIDE + A_BACK + A_BOTM)
 
         VAL(92) = VAL(100)
         VAL(93) = VAL(101)
      END IF
 
      RETURN
      END
