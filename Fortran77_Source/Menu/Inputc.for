      SUBROUTINE INPUTC(INSTR,IRET)
C     ******************************************************************
C     *         SUBSET OF THE INPUT ROUTINE FOR ERA                    *
C     *         INPUT MENUS FOR CYCLE DATA                             *
C     ******************************************************************
      CHARACTER       key, form_feed
      CHARACTER*13    filmap, filmap1, filmap2
      CHARACTER*72    head_line
C
      INTEGER*2       EDRVE,WDRVE,IOK
C
      COMMON/CHANGE/ISAV,ILPT1,NUMSEL,IBYPAS,NUMPAT,EDRVE,WDRVE,IBUF,
     .     IFILE
      COMMON/DSKINP/IWRITE,IPRINT,IOUT
      COMMON/LIMITS/IOK(800)
      COMMON/TITL/ITITL,TITLE(68,5)
      COMMON/EDTCOM/ICODET, INOTE
      COMMON /EVPCOM/ DEGSUP, EVPOPT
      COMMON /LINCNT/ num3, num_lines, form_feed
      COMMON /MAPNAM/ filmap1, filmap2
 
      COMMON/VALUES/VAL(800),VALMN(800),VALMX(800)
C
      DATA IO/ 0 /
C
C          SET UP THE FLAGS RELATING TO REFRIGERATOR TYPE
C
      IRFTYP = NFIX(VAL(1))
      ICYCL  = NFIX(VAL(170))
      NUMREF = NFIX(VAL(175))
C
C          BRANCH ON INSTR
C
      IOPT = 0
      GO TO (90,100,175), INSTR
C
C          OUTPUT DATA EDIT MENU AND GET USER SELECTION
C
   90 CONTINUE
      IFLAG = 0
      IOPT = 4
      CALL EDTCYC(LOC)
      GO TO (100,110,130,150,170,190,30), LOC
C
C          CYCLE DEFINITION
C
  100 CONTINUE
 
      num3 = 9
      WRITE(HEAD_LINE, '(''CYCLE DEFINITION'')')
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
      CALL SETIOK(1,1,1,1,1,1,1,1,1,1,1,1,1,1,IOK(170))
      CALL EDTFIL('BLOCK43.DAT', 09, 09, 1, 0, IFLAG, IOUT,
     .             VAL(170), VALMN(170), VALMX(170), IOK(170), 1)
 
      ICYCL = NFIX(VAL(170))
      NUMREF = NFIX(VAL(175))
 
      IF (IRFTYP .GE. 4 .and. irftyp .le. 6 .AND. ICYCL .NE. 1) THEN
         CALL error (2)
         CALL gotoxy(20, 11)
         CALL print('A Single Evaporator Cycle Must be Used',38,-2)
         IF (irftyp .NE. 6) THEN
            CALL gotoxy(20, 12)
            CALL print('with Cabinet Types 4 or 5 (Freezer).',36,-2)
         ELSE
            CALL gotoxy(24, 12)
            CALL print('with a Single-Door Refrigerator.',32,-2)
         END IF
         CALL inchr(1,j,key)
         GO TO 100
 
      END IF
C
C          BRANCH ON ERROR FLAGS
C
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 700
      IF(IFLAG .EQ. 3) GO TO 90
 
      IF(ICYCL .EQ. 1 .OR. ICYCL .EQ. 3) THEN
         IF(IOPT .EQ. 4) GO TO 90
         GO TO 110
      END IF
C
C          LORENZ CYCLE OR DUAL EVAPORATOR CONTROL
C
 
      num3 = 7
      WRITE(HEAD_LINE, '(''LORENZ OR DUAL EVAPORATOR CYCLE CONTROL'')')
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
      CALL SETIOK(1,1,1,1,1,1,1,1,1,1,1,1,1,1,IOK(405))
      valmx(405) = 5
      CALL EDTFIL('BLOCK28.DAT', 07, 07, 1, 0, IFLAG, IOUT,
     .             VAL(405), VALMN(405), VALMX(405), IOK(405), 1)
C
C          BRANCH ON ERROR FLAGS
C
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 100
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT  .EQ. 4) GO TO 90
C
C          REFRIGERANT CODES: FRESH FOOD LOOP
C
  110 CONTINUE
      NUMREF = NFIX(VAL(175))
      IF(ICYCL .NE. 3) THEN
         WRITE(HEAD_LINE, '(''REFRIGERANT CODES'')')
      ELSE
         WRITE(HEAD_LINE, '(''REFRIGERANT CODES: FRESH FOOD LOOP'')')
      END IF
 
      IF(NUMREF .EQ. 1) THEN
        ICODET = 19
           num3 = 1
           CALL BLOCK_HEAD(HEAD_LINE)
 
           CALL SETINT(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
           CALL SETIOK(1,1,1,1,1,1,1,1,1,1,1,1,1,1,IOK(180))
           CALL EDTFIL('BLOCK44.DAT', 01, 09, 4, 1, IFLAG, IOUT,
     .                  VAL(180), VALMN(180), VALMX(180), IOK(180), 1)
           icodet = 0
      END IF
 
      IF(NUMREF.EQ. 2) THEN
           ICODET = 15
           num3 = 4
           CALL BLOCK_HEAD(HEAD_LINE)
 
           CALL SETINT(0,2,0,3,0,0,0,0,0,0,0,0,0,0)
           CALL SETIOK(1,1,1,1,1,1,1,1,1,1,1,1,1,1,IOK(320))
           CALL EDTFIL('BLOCK27.DAT', 04, 04, 1, 0, IFLAG, IOUT,
     .                  VAL(320), VALMN(320), VALMX(320), IOK(320), 1)
           ICODET = 0
      END IF
 
      IF(NUMREF .EQ. 3) THEN
           ICODET = 16
           num3 = 9
           CALL BLOCK_HEAD(HEAD_LINE)
 
           CALL SETINT(0,2,0,2,0,0,3,3,3,0,0,0,0,0)
           CALL SETIOK(1,1,1,1,1,1,1,1,1,1,1,1,1,1,IOK(180))
           CALL EDTFIL('BLOCK44.DAT', 01, 09, 4, 2, IFLAG, IOUT,
     .                  VAL(180), VALMN(180), VALMX(180), IOK(180), 1)
           ICODET = 0
      END IF
 
      IF(NUMREF .LE. 2) GO TO 115
      TOTMAS = VAL(181) + VAL(183)
      IF(TOTMAS .LE. 1.0) GO TO 115
C
C          ERROR: DEFINED MASS FRACTION > 1
C
      CALL error (2)
      CALL gotoxy(20, 11)
      CALL print('The Specified Mass Fractions of the Three',41,-2)
      CALL gotoxy(20, 12)
      CALL print('Components is Greater Than 1.0',30,-2)
 
      CALL inchr(1,j,key)
      GO TO 110
C
C          CHECK ERROR FLAGS
C
  115 CONTINUE
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 700
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT  .EQ. 4) GO TO 120
      IF(ICYCL .NE. 3) GO TO 130
C
C          REFRIGERANT CODES: FREEZER LOOP
C
  120 CONTINUE
      IF(ICYCL .NE. 3) GO TO 90
 
      WRITE(HEAD_LINE, '(''REFRIGERANT CODES: FREEZER LOOP'')')
 
      IF(NUMREF .EQ. 1) THEN
           icodet = 19
           num3 = 1
           CALL BLOCK_HEAD(HEAD_LINE)
 
           CALL SETINT(0,2,0,2,0,0,3,3,3,0,0,0,0,0)
           CALL SETIOK(1,1,1,1,1,1,1,1,1,1,1,1,1,1,IOK(190))
           CALL EDTFIL('BLOCK44.DAT', 01, 09, 4, 1, IFLAG, IOUT,
     .                  VAL(190), VALMN(190), VALMX(190), IOK(190), 1)
           icodet = 0
      END IF
 
      IF(NUMREF.EQ. 2) THEN
           ICODET = 15
           num3 = 4
           CALL BLOCK_HEAD(HEAD_LINE)
 
           CALL SETINT(0,2,0,3,0,0,0,0,0,0,0,0,0,0)
           CALL SETIOK(1,1,1,1,1,1,1,1,1,1,1,1,1,1,IOK(325))
           CALL EDTFIL('BLOCK27.DAT', 04, 04, 1, 0, IFLAG, IOUT,
     .                  VAL(325), VALMN(325), VALMX(325), IOK(325), 1)
           ICODET = 0
      END IF
 
      IF(NUMREF .EQ. 3) THEN
           ICODET = 16
           num3 = 9
           CALL BLOCK_HEAD(HEAD_LINE)
 
           CALL SETINT(0,2,0,2,0,0,3,3,3,0,0,0,0,0)
           CALL SETINT(0,2,0,2,0,0,3,3,3,0,0,0,0,0)
           CALL SETIOK(1,1,1,1,1,1,1,1,1,1,1,1,1,1,IOK(190))
           CALL EDTFIL('BLOCK44.DAT', 01, 09, 4, 2, IFLAG, IOUT,
     .                  VAL(190), VALMN(190), VALMX(190), IOK(190), 1)
           ICODET = 0
      END IF
 
      IF(NUMREF .LE. 2) GO TO 125
      TOTMAS = VAL(191) + VAL(193)
      IF(TOTMAS .LE. 1.0) GO TO 125
C
C          ERROR: DEFINED MASS FRACTION > 1
C
      CALL error (2)
      CALL gotoxy(20, 11)
      CALL print('The Specified Mass Fractions of the Three',41,-2)
      CALL gotoxy(20, 12)
      CALL print('Components is Greater Than 1.0',30,-2)
 
      CALL inchr(1,j,key)
      GO TO 120
C
C          CHECK ERROR FLAGS
C
  125 CONTINUE
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 110
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT  .EQ. 4) GO TO 90
C
C          FRESH FOOD EVAPORATOR
C
  130 CONTINUE
      IF(ICYCL .EQ. 1) THEN
        WRITE(HEAD_LINE, '(''EVAPORATOR DATA'')')
      ELSE
        WRITE(HEAD_LINE, '(''FRESH FOOD EVAPORATOR DATA'')')
      END IF
 
 
      IF (IRFTYP .EQ. 6) VAL(209) = 2.0
      IFAN = NFIX(VAL(209))
      IF(IFAN .EQ. 1) THEN
         ICODET = 11
      ELSE
         ICODET = 0
      END IF
      ISPEC = NFIX(VAL(201))
 
      num3 = 12
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(0,0,0,0,2,0,0,0,0,0,0,2,0,0)
      SELECT CASE (ISPEC)
        CASE (1)                                          !Evap superheat
          IF(IFAN .EQ. 2) THEN
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(201))
            IF (IRFTYP .NE. 6) THEN
               CALL EDTFIL('BLOCK45.DAT', 11, 12, 11, 1, IFLAG, IOUT,
     .                      VAL(201), VALMN(201), VALMX(201), IOK(201),
     .                                                              -1)
            ELSE
               num3 = 5
               CALL EDTFIL('BLOCK45.DAT', 5, 18, 11, 1, IFLAG, IOUT,
     .                      VAL(201), VALMN(201), VALMX(201), IOK(201),
     .                                                              -1)
            END IF
            IFAN = NFIX(VAL(209))
            IF(IFAN .NE. 2) GO TO 130
 
          ELSE
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(201))
            CALL EDTFIL('BLOCK45.DAT', 11, 12, 11, 2, IFLAG, IOUT,
     .                   VAL(201), VALMN(201), VALMX(201), IOK(201), -1)
 
            IFAN = NFIX(VAL(209))
            IF(IFAN .NE. 1) GO TO 130
          END IF
 
            ISPEC = NFIX(VAL(201))
            IF(ISPEC .NE. 1) GO TO 130
 
        CASE (2)                                          !IHX superheat
          IF(IFAN .EQ. 2) THEN
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(201))
            IF (IRFTYP .NE. 6) THEN
               CALL EDTFIL('BLOCK29.DAT', 11, 12, 11, 1, IFLAG, IOUT,
     .                      VAL(201), VALMN(201), VALMX(201), IOK(201),
     .                                                              -1)
            ELSE
               num3 = 5
               CALL EDTFIL('BLOCK29.DAT', 5, 18, 11, 1, IFLAG, IOUT,
     .                      VAL(201), VALMN(201), VALMX(201), IOK(201),
     .                                                              -1)
            END IF
 
            IFAN = NFIX(VAL(209))
            IF(IFAN .NE. 2) GO TO 130
          ELSE
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(201))
            CALL EDTFIL('BLOCK29.DAT', 11, 12, 11, 2, IFLAG, IOUT,
     .                   VAL(201), VALMN(201), VALMX(201), IOK(201), -1)
 
            IFAN = NFIX(VAL(209))
            IF(IFAN .NE. 1) GO TO 130
          END IF
 
            ISPEC = NFIX(VAL(201))
            IF(ISPEC .NE. 2) GO TO 130
 
        CASE (3)                                          !Exit quality
          VALMXS = VALMX(205)
          VALMX(205) = 1.0
          IF(VAL(205) .GT. 1.0) VAL(205) = 1.0
 
          IF(IFAN .EQ. 2) THEN
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(201))
            IF (IRFTYP .NE. 6) THEN
               CALL EDTFIL('BLOCK30.DAT', 11, 12, 11, 1, IFLAG, IOUT,
     .                      VAL(201), VALMN(201), VALMX(201), IOK(201),
     .                                                              -1)
            ELSE
               num3 = 5
               CALL EDTFIL('BLOCK30.DAT', 5, 18, 11, 1, IFLAG, IOUT,
     .                      VAL(201), VALMN(201), VALMX(201), IOK(201),
     .                                                              -1)
            END IF
            VALMX(205) = VALMXS
 
            IFAN = NFIX(VAL(209))
            IF(IFAN .NE. 2) GO TO 130
          ELSE
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(201))
            CALL EDTFIL('BLOCK30.DAT', 11, 12, 11, 2, IFLAG, IOUT,
     .                   VAL(201), VALMN(201), VALMX(201), IOK(201), -1)
            VALMX(205) = VALMXS
 
            IFAN = NFIX(VAL(209))
            IF(IFAN .NE. 1) GO TO 130
          END IF
 
          ISPEC = NFIX(VAL(201))
          IF(ISPEC .NE. 3) GO TO 130
 
      END SELECT
 
      ICODET = 0
      IF(IFLAG .GT. 3) THEN
         IF(VAL(401) .EQ. VAL(396)) THEN
            VAL(397) = VAL(212)
            VAL(398) = VAL(216)
         END IF
 
         CALL FAN(1, IFAN_RET, FAN_PWR, FAN_AIR)
            IF(IFAN_RET .EQ. 0) THEN
               VAL(212) = FAN_PWR
               VAL(216) = FAN_AIR
            END IF
         GO TO 130
      END IF
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 700
      IF(IFLAG .EQ. 3) GO TO 90
C
C          REST OF EVAPORATOR DATA
C
  135 CONTINUE
      IF(ICYCL .EQ. 1) THEN
        WRITE(HEAD_LINE, '(''EVAPORATOR DATA, CONTINUED'')')
      ELSE
        WRITE(HEAD_LINE, '(''FRESH FOOD EVAPORATOR DATA, CONTINUED'')')
      END IF
 
      INAT = NFIX(VAL(209))
 
      SELECT CASE (INAT)
        CASE (1)                                          !Forced convection
          EVPOPT = VAL(201)
          DEGSUP = VAL(205)
 
          IF (nfix(VAL(690)) .eq. 0) THEN
             ICODET = 10
             num3 = 5
          ELSE
             ICODET = 20
             num3 = 3
          END IF
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETINT(3,3,3,3,3,0,0,0,0,0,0,0,0,0)
          CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(215))
          CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(691))
 
          IF (nfix(VAL(690)) .eq. 0) THEN
             CALL EDTFIL('BLOCK46.DAT', 05, 05, 1, 0, IFLAG, IOUT,
     .                    VAL(215), VALMN(215), VALMX(215), IOK(215), 1)
             VAL(691) = VAL(216)
             VAL(693) = VAL(219)
          ELSE
             CALL EDTFIL('BLOCK64.DAT', 03, 03, 1, 0, IFLAG, IOUT,
     .                    VAL(691), VALMN(691), VALMX(691), IOK(691), 1)
             VAL(216) = VAL(691)
             VAL(219) = VAL(693)
          END IF
 
          IFLAGS = IFLAG
          ICODET = 0
 
          IF (IFLAG .eq. -4) THEN
             IF (nfix(VAL(690)) .eq. 0) THEN
                VAL(690) = 1
             ELSE
                VAL(690) = 0
             END IF
             go to 135
          END IF
 
          IF(IFLAGS .GT. 3 .AND. nfix(VAL(690)) .EQ. 0) THEN
               CALL FINDUS (1, IFND, AEX, USC, U2P, U1P, PDROP)
               IF(IFND .EQ. 0) THEN
                  VAL(215) = AEX
                  VAL(217) = U2P
                  VAL(218) = U1P
                  VAL(219) = PDROP
               END IF
               GO TO 135
          END IF
 
        CASE (2)                                          !Free convection
          if(irftyp .ne. 6) then
             icodet = 17
             num3 = 2
             CALL BLOCK_HEAD(HEAD_LINE)
 
             CALL SETINT(3,3,0,0,0,0,0,0,0,0,0,0,0,0)
             CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(412))
             CALL EDTFIL('BLOCK47.DAT', 02, 02, 1, 0, IFLAG, IOUT,
     .                    VAL(412), VALMN(412), VALMX(412), IOK(412), 1)
          else
             icodet = 22
             num3 = 8
             CALL BLOCK_HEAD(HEAD_LINE)
 
             valmx(778) = (val(4) - val(43) - val(45))
     .                  * (val(3) - 2.0 * val(42)) / 1000.0
 
             valmx(779) = 0.25 * (val(4) - val(43) - val(45)) * val(2)
     .                  / 1000.0
             valmx(780) = 0.25 * (val(3) - 2.0 * val(42)) * val(2)
     .                  / 1000.0
 
             CALL SETINT(0,3,3,0,3,3,3,3,0,0,0,0,0,0)
             CALL SETIOK(1,2,2,1,2,2,2,2,0,0,0,0,0,0,IOK(774))
             CALL EDTFIL('BLOCK77.DAT', 08, 08, 1, 0, IFLAG, IOUT,
     .                    VAL(774), VALMN(774), VALMX(774), IOK(774), 1)
 
             areawall = val(778) + val(779) + val(780)
             val(162) = val(778) / (val(775) + 0.001)      !fraction top
             val(163) = val(779) / (val(775) + 0.001)      !fraction side
             val(164) = val(780) / (val(775) + 0.001)      !fraction back
             val(165) = 0.0                                !fraction bottom
             val(167) = 0.0                                !depth in wall
 
             if (areawall .GT. val(775)) then
                CALL error (2)
                CALL gotoxy(20, 11)
                CALL print('Areas Covering Cabinet Walls are Greater',
     .                                                           40, -2)
                CALL gotoxy(25, 12)
                CALL print('Than the Total Evaporator Area', 30, -2)
                CALL inchr(1,j,key)
                GO TO 135
 
             end if
 
          end if
 
          IFLAGS = IFLAG
          ICODET = 0
 
          IF(IFLAGS .GT. 3) THEN
               CALL FINDUS (5, IFND, AEX, USC, U2P, U1P, PDROP)
               IF(IFND .EQ. 0) THEN
                  if (irftyp .ne. 6) then
                     VAL(413) = PDROP
                  else
                     val(781) = pdrop
                  end if
               END IF
               GO TO 135
          END IF
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 130
      IF(IFLAG .EQ. 3) GO TO 90
C
C          LOCATION OF NATURAL CONVECTION EVAPORATOR
C
  137 CONTINUE
      IF(NFIX(VAL(209)) .EQ. 2 .and. irftyp .ne. 6) THEN
 
        WRITE(HEAD_LINE, '(''NATURAL CONVECTION EVAPORATOR LOCATION'')')
 
        CALL SETINT(0,0,0,2,2,2,2,2,2,0,0,0,0,0)
        n_e = 372
        if(irftyp .eq. 5) n_e = 362
 
        IF(NFIX(VAL(n_e)) .EQ. 0) THEN
          num3 = 4
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETIOK(1,1,1,1,2,1,1,1,1,0,0,0,0,0,IOK(n_e))
 
          IF (irftyp .LE. 3 .or. irftyp .eq. 7) THEN
            CALL EDTFIL('BLOCK31.DAT', 04, 05, 04, 1, IFLAG, IOUT,
     .                   VAL(372), VALMN(372), VALMX(372), IOK(372), -1)
          END IF
 
          IF(IRFTYP .EQ. 4) THEN
            CALL EDTFIL('BLOCK67.DAT', 04, 04, 04, 1, IFLAG, IOUT,
     .                   VAL(372), VALMN(372), VALMX(372), IOK(372), -1)
          END IF
 
          IF(IRFTYP .EQ. 5) THEN
            CALL EDTFIL('BLOCK66.DAT', 04, 06, 04, 1, IFLAG, IOUT,
     .                   VAL(362), VALMN(362), VALMX(362), IOK(362), -1)
          END IF
 
          IF(NFIX(VAL(n_e)) .NE. 0) GO TO 137
 
        ELSE
          num3 = 9
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETIOK(1,1,1,1,2,1,1,1,1,0,0,0,0,0,IOK(n_e))
 
          IF(IRFTYP .LE. 3 .or. irftyp .eq. 7) THEN
            CALL EDTFIL('BLOCK31.DAT', 07, 02, 15, 1, IFLAG, IOUT,
     .                   VAL(372), VALMN(372), VALMX(372), IOK(372), 1)
          END IF
 
          IF(IRFTYP .EQ. 4) THEN
            CALL EDTFIL('BLOCK67.DAT', 07, 01, 15, 1, IFLAG, IOUT,
     .                   VAL(372), VALMN(372), VALMX(372), IOK(372), 1)
 
          END IF
 
          IF(IRFTYP .EQ. 5) THEN
            CALL EDTFIL('BLOCK66.DAT', 09, 01, 15, 1, IFLAG, IOUT,
     .                   VAL(362), VALMN(362), VALMX(362), IOK(362), -1)
          END IF
 
          IF(NFIX(VAL(n_e)) .EQ. 0) GO TO 137
 
        END IF
C
C          ERROR CHECK ON AREAS SPECIFIED
C
         IF(NFIX(VAL(n_e)) .NE. 0) THEN
            AREA = VAL(n_e+5) + VAL(n_e+6)
            IF(IRFTYP .EQ. 5) AREA = AREA + VAL(n_e+7) + VAL(n_e+8)
            area_error = abs(AREA - 1.0)
            IF(area_error .GT. 0.005) THEN
              CALL error (2)
              CALL gotoxy(20, 11)
              CALL print('Sum of Fractional Areas of Evaporator',37,-2)
              CALL gotoxy(20, 12)
              CALL print('Must Equal Unity.',17,-2)
 
             CALL inchr(1,j,key)
               GO TO 137
            END IF
         END IF
 
         IF(IFLAG .EQ. 1) GO TO 20
         IF(IFLAG .EQ. 2) GO TO 135
 
      END IF
 
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT .EQ. 4) GO TO 140
      IF(ICYCL .EQ. 1) GO TO 150
C
C          FREEZER EVAPORATOR
C
  140 CONTINUE
      IF(ICYCL .EQ. 1) GO TO 90
 
      IFAN = NFIX(VAL(229))
      IF(IFAN .EQ. 1) THEN
         ICODET = 11
      ELSE
         ICODET = 0
      END IF
      ISPEC = NFIX(VAL(221))
 
      num3 = 12
      WRITE(HEAD_LINE, '(''FREEZER EVAPORATOR DATA'')')
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(0,0,0,0,2,0,0,0,0,0,0,2,0,0)
      SELECT CASE (ISPEC)
        CASE (1)                                          !Evap superheat
          IF(IFAN .EQ. 2) THEN
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(221))
            CALL EDTFIL('BLOCK45.DAT', 11, 12, 11, 1, IFLAG, IOUT,
     .                   VAL(221), VALMN(221), VALMX(221), IOK(221), -1)
 
            IFAN = NFIX(VAL(229))
            IF(IFAN .NE. 2) GO TO 140
          ELSE
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(221))
            CALL EDTFIL('BLOCK45.DAT', 11, 12, 11, 2, IFLAG, IOUT,
     .                   VAL(221), VALMN(221), VALMX(221), IOK(221), -1)
 
            IFAN = NFIX(VAL(229))
            IF(IFAN .NE. 1) GO TO 140
          END IF
 
          ISPEC = NFIX(VAL(221))
          IF(ISPEC .NE. 1) GO TO 140
 
        CASE (2)                                          !IHX superheat
          IF(IFAN .EQ. 2) THEN
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(221))
            CALL EDTFIL('BLOCK29.DAT', 11, 12, 11, 1, IFLAG, IOUT,
     .                   VAL(221), VALMN(221), VALMX(221), IOK(221), -1)
 
            IFAN = NFIX(VAL(229))
            IF(IFAN .NE. 2) GO TO 140
          ELSE
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(221))
            CALL EDTFIL('BLOCK29.DAT', 11, 12, 11, 2, IFLAG, IOUT,
     .                   VAL(221), VALMN(221), VALMX(221), IOK(221), -1)
 
            IFAN = NFIX(VAL(229))
            IF(IFAN .NE. 1) GO TO 140
          END IF
 
          ISPEC = NFIX(VAL(221))
          IF(ISPEC .NE. 2) GO TO 140
 
        CASE (3)                                          !Exit quality
          VALMXS = VALMX(225)
          VALMX(225) = 1.0
          IF(VAL(225) .GT. 1.0) VAL(225) = 1.0
 
          IF(IFAN .EQ. 2) THEN
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(221))
            CALL EDTFIL('BLOCK30.DAT', 11, 12, 11, 1, IFLAG, IOUT,
     .                   VAL(221), VALMN(221), VALMX(221), IOK(221), -1)
            VALMX(225) = VALMXS
 
            IFAN = NFIX(VAL(229))
            IF(IFAN .NE. 2) GO TO 140
          ELSE
            CALL SETIOK(1,1,1,1,0,1,1,1,1,1,1,0,0,0,IOK(221))
            CALL EDTFIL('BLOCK30.DAT', 11, 12, 11, 2, IFLAG, IOUT,
     .                   VAL(221), VALMN(221), VALMX(221), IOK(221), -1)
            VALMX(225) = VALMXS
 
            IFAN = NFIX(VAL(229))
            IF(IFAN .NE. 1) GO TO 140
          END IF
 
          ISPEC = NFIX(VAL(221))
          IF(ISPEC .NE. 3) GO TO 140
 
      END SELECT
 
      ICODET = 0
      IF(IFLAG .GT. 3) THEN
         IF(VAL(401) .EQ. VAL(396)) THEN
            VAL(397) = VAL(232)
            VAL(398) = VAL(236)
         END IF
 
         CALL FAN(1, IFAN_RET, FAN_PWR, FAN_AIR)
            IF(IFAN_RET .EQ. 0) THEN
               VAL(232) = FAN_PWR
               VAL(236) = FAN_AIR
            END IF
         GO TO 140
      END IF
 
      IF(IFLAG .EQ. 1) GO TO 20
 
      IF(IFLAG .EQ. 2) THEN
         IF(NFIX(VAL(209)) .EQ. 2) GO TO 137
         GO TO 135
      END IF
 
      IF(IFLAG .EQ. 3) GO TO 90
C
C          REST OF EVAPORATOR DATA
C
  142 CONTINUE
 
      INAT = NFIX(VAL(229))
 
      WRITE(HEAD_LINE, '(''FREEZER EVAPORATOR DATA, CONTINUED'')')
 
      CALL SETINT(2,2,2,2,2,0,0,0,0,0,0,0,0,0)
      SELECT CASE (INAT)
        CASE (1)                                          !Forced convection
          EVPOPT = VAL(221)
          DEGSUP = VAL(225)
 
 
          IF (nfix(VAL(695)) .eq. 0) THEN
             ICODET = 10
             num3 = 5
          ELSE
             ICODET = 20
             num3 = 3
          END IF
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETINT(3,3,3,3,3,0,0,0,0,0,0,0,0,0)
          CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(235))
          CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(696))
 
          IF (nfix(VAL(695)) .eq. 0) THEN
             CALL EDTFIL('BLOCK46.DAT', 05, 05, 1, 0, IFLAG, IOUT,
     .                    VAL(235), VALMN(235), VALMX(235), IOK(235), 1)
             VAL(696) = VAL(236)
             VAL(698) = VAL(239)
          ELSE
             CALL EDTFIL('BLOCK64.DAT', 03, 03, 1, 0, IFLAG, IOUT,
     .                    VAL(696), VALMN(696), VALMX(696), IOK(696), 1)
             VAL(236) = VAL(696)
             VAL(239) = VAL(698)
          END IF
 
          IFLAGS = IFLAG
          ICODET = 0
 
          IF (IFLAG .eq. -4) THEN
             IF (nfix(VAL(695)) .eq. 0) THEN
                VAL(695) = 1
             ELSE
                VAL(695) = 0
             END IF
             go to 142
          END IF
 
          IF(IFLAGS .GT. 3 .AND. nfix(VAL(695)) .EQ. 0) THEN
            EVPOPT = VAL(221)
            DEGSUP = VAL(225)
            CALL FINDUS (2, IFND, AEX, USC, U2P, U1P, PDROP)
            IF(IFND .EQ. 0) THEN
              VAL(235) = AEX
              VAL(237) = U2P
              VAL(238) = U1P
              VAL(239) = PDROP
            END IF
            GO TO 142
          END IF
 
        CASE (2)                                          !Free convection
          icodet = 17
          num3 = 2
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETINT(3,3,0,0,0,0,0,0,0,0,0,0,0,0)
          CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(416))
          CALL EDTFIL('BLOCK47.DAT', 02, 02, 1, 0, IFLAG, IOUT,
     .                 VAL(416), VALMN(416), VALMX(416), IOK(416), 1)
 
          IFLAGS = IFLAG
          ICODET = 0
 
          IF(IFLAGS .GT. 3) THEN
            CALL FINDUS (6, IFND, AEX, USC, U2P, U1P, PDROP)
            IF(IFND .EQ. 0) THEN
              VAL(417) = PDROP
            END IF
            GO TO 142
          END IF
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 140
      IF(IFLAG .EQ. 3) GO TO 90
C
C          LOCATION OF NATURAL CONVECTION EVAPORATOR
C
  145 CONTINUE
      IF(NFIX(VAL(229)) .EQ. 2) THEN
 
      WRITE(HEAD_LINE, '(''NATURAL CONVECTION EVAPORATOR LOCATION'')')
 
        CALL SETINT(0,0,0,2,2,2,2,2,2,0,0,0,0,0)
        IF(NFIX(VAL(362)) .EQ. 0) THEN
          num3 = 4
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETIOK(1,1,1,1,0,1,1,1,1,0,0,0,0,0,IOK(362))
          CALL EDTFIL('BLOCK31.DAT', 04, 05, 04, 1, IFLAG, IOUT,
     .                 VAL(362), VALMN(362), VALMX(362), IOK(362), -1)
          IF(NFIX(VAL(362)) .NE. 0) GO TO 145
 
        ELSE
          num3 = 9
          CALL BLOCK_HEAD(HEAD_LINE)
 
          SELECT CASE (IRFTYP)
            CASE (1, 7)
              IMUL = 1
              CALL SETIOK(1,1,1,1,0,1,1,1,1,0,0,0,0,0,IOK(362))
              CALL EDTFIL('BLOCK31.DAT', 09, 00, 15, 1, IFLAG, IOUT,
     .                     VAL(362), VALMN(362), VALMX(362),
     .                                           IOK(362), 1)
 
            CASE (2)
              VAL(369) = VAL(370)
              IMUL = 1
              CALL SETIOK(1,1,1,1,0,1,1,1,1,0,0,0,0,0,IOK(362))
              CALL EDTFIL('BLOCK57.DAT', 08, 01, 15, 1, IFLAG, IOUT,
     .                     VAL(362), VALMN(362), VALMX(362),
     .                                           IOK(362), 1)
              VAL(370) = VAL(369)
              VAL(369) = 0
 
            CASE (3)
              IMUL = 0
              CALL SETIOK(1,1,1,1,0,1,1,1,1,0,0,0,0,0,IOK(362))
              CALL EDTFIL('BLOCK31.DAT', 08, 01, 15, 1, IFLAG, IOUT,
     .                     VAL(362), VALMN(362), VALMX(362),
     .                                           IOK(362), 1)
 
            CASE DEFAULT
              IMUL = 0
              CALL SETIOK(1,1,1,1,0,1,1,1,1,0,0,0,0,0,IOK(362))
              CALL EDTFIL('BLOCK31.DAT', 08, 01, 15, 1, IFLAG, IOUT,
     .                     VAL(362), VALMN(362), VALMX(362),
     .                                           IOK(362), 1)
              VAL(370) = 0
        END SELECT
 
          IF(NFIX(VAL(362)) .EQ. 0) GO TO 145
 
      END IF
C
C          ERROR CHECK ON AREAS SPECIFIED
C
         IF(NFIX(VAL(362)) .NE. 0) THEN
            AREA = VAL(367) + VAL(368) + VAL(369)
 
            IF(IMUL .EQ. 1) THEN
               AREA = AREA + VAL(370)
            ELSE
              VAL(370) = 0
            END IF
 
            area_error = abs(area - 1.0)
            IF(area_error .GT. 0.005) THEN
              CALL error (2)
              CALL gotoxy(20, 11)
              CALL print('Sum of Fractional Areas of Evaporator',37,-2)
              CALL gotoxy(20, 12)
              CALL print('Must Equal Unity.',17,-2)
 
             CALL inchr(1,j,key)
               GO TO 145
            END IF
         END IF
 
         IF(IFLAG .EQ. 1) GO TO 20
         IF(IFLAG .EQ. 2) GO TO 142
 
      END IF
 
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT  .EQ. 4) GO TO 90
C
C          FRESH FOOD LOOP CONDENSER
C
  150 CONTINUE
      iconderror = 0
      IF(ICYCL .NE. 3) THEN
         WRITE(HEAD_LINE, '(''CONDENSER DATA'')')
      ELSE
         WRITE(HEAD_LINE, '(''FRESH FOOD CONDENSER DATA'')')
      END IF
      IFAN = NFIX(VAL(246))
      IF(IFAN .EQ. 1) THEN
         ICODET = 14
      ELSE
         ICODET = 0
      END IF
 
      num3 = 8
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(2,0,0,0,0,0,0,2,0,0,0,0,0,0)
      IF(IFAN .EQ. 2 .OR. IFAN .EQ. 3) THEN
        CALL SETIOK(0,1,1,1,1,1,1,0,0,0,0,0,0,0,IOK(242))
        CALL EDTFIL('BLOCK23.DAT', 07, 08, 13, 1, IFLAG, IOUT,
     .               VAL(242), VALMN(242), VALMX(242), IOK(242), 1)
        IFAN = NFIX(VAL(246))
        IF(IFAN .EQ. 1) GO TO 150
      ELSE
        CALL SETIOK(0,1,1,1,1,1,1,0,0,0,0,0,0,0,IOK(242))
        CALL EDTFIL('BLOCK23.DAT', 07, 08, 13, 2, IFLAG, IOUT,
     .               VAL(242), VALMN(242), VALMX(242), IOK(242), 1)
        IFAN = NFIX(VAL(246))
        IF(IFAN .NE. 1) GO TO 150
      END IF
 
      ICODET = 0
      IF(IFLAG .GT. 3) THEN
        IF(VAL(451) .EQ. VAL(446)) THEN
          VAL(447) = VAL(249)
          VAL(448) = VAL(255)
        END IF
 
        CALL FAN(2, IFAN_RET, FAN_PWR, FAN_AIR)
          IF(IFAN_RET .EQ. 0) THEN
            VAL(249) = FAN_PWR
            VAL(255) = FAN_AIR
          END IF
        GO TO 150
      END IF
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 700
      IF(IFLAG .EQ. 3) GO TO 90
C
C          REST OF CONDENSER DATA
C
  155 CONTINUE
      INAT = VAL(246)
 
   !! check on dual loop -- do not allow a natural convection condenser
      IF (ICYCL .EQ. 3 .AND. INAT .ne. 1) THEN
         CALL error (2)
         CALL gotoxy(20, 11)
         CALL print('Natural Convection Condenser Not Allowed ',41,-2)
         CALL gotoxy(20, 12)
         CALL print('in Dual Loop Systems          ',30,-2)
 
         CALL inchr(1,j,key)
         GO TO 150
      END IF
 
   !! check on side by side -- do not allow a natural convection condenser
      IF (IRFTYP .EQ. 3 .AND. INAT .ne. 1) THEN
         CALL error (2)
         CALL gotoxy(20, 11)
         CALL print('Natural Convection Condenser Not Allowed ',41,-2)
         CALL gotoxy(20, 12)
         CALL print('in Side-by-Side Refrigerator  ',30,-2)
 
         CALL inchr(1,j,key)
         GO TO 150
      END IF
 
      IF(ICYCL .NE. 3) THEN
         WRITE(HEAD_LINE, '(''CONDENSER DATA, CONTINUED'')')
      ELSE
         WRITE(HEAD_LINE, '(''FRESH FOOD CONDENSER DATA, CONTINUED'')')
      END IF
 
      INAT =  VAL(246)
 
 
      SELECT CASE (INAT)
        CASE (1)                                          !Forced convection
          DEGSUP = VAL(242)
 
          IF (nfix(VAL(700)) .eq. 0) THEN
             ICODET = 13
             num3 = 6
          ELSE
             ICODET = 21
             num3 = 3
          END IF
 
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETINT(2,2,2,2,2,2,0,0,0,0,0,0,0,0)
          CALL SETIOK(0,0,0,0,0,2,0,0,0,0,0,0,0,0,IOK(254))
          CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(701))
 
          IF (nfix(VAL(700)) .eq. 0) THEN
             CALL EDTFIL('BLOCK24.DAT', 06, 06, 1, 0, IFLAG, IOUT,
     .                    VAL(254), VALMN(254), VALMX(254), IOK(254), 1)
             VAL(701) = VAL(255)
             VAL(703) = VAL(259)
          ELSE
             CALL EDTFIL('BLOCK65.DAT', 03, 03, 1, 0, IFLAG, IOUT,
     .                    VAL(701), VALMN(701), VALMX(701), IOK(701), 1)
             VAL(255) = VAL(701)
             VAL(259) = VAL(703)
          END IF
 
          IFLAGS = IFLAG
          ICODET = 0
 
          IF (IFLAG .eq. -4) THEN
             IF (nfix(VAL(700)) .eq. 0) THEN
                VAL(700) = 1
             ELSE
                VAL(700) = 0
             END IF
             go to 155
          END IF
 
          IF(IFLAGS .GT. 3 .AND. nfix(VAL(700)) .EQ. 0) THEN
            CALL FINDUS (3, IFND, AEX, USC, U2P, U1P, PDROP)
            IF(IFND .EQ. 0) THEN
              VAL(254) = AEX
              VAL(256) = USC
              VAL(257) = U2P
              VAL(258) = U1P
              VAL(259) = PDROP
            END IF
            GO TO 155
          END IF
 
        CASE (2, 3)                                          !Free convection
   !      val(700) = 0   !UA option on forced convection
          icodet = 18
          num3 = 2
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETINT(2,2,0,0,0,0,0,0,0,0,0,0,0,0)
          CALL SETIOK(0,2,0,0,0,0,0,0,0,0,0,0,0,0,IOK(512))
          CALL EDTFIL('BLOCK50.DAT', 02, 02, 1, 0, IFLAG, IOUT,
     .                 VAL(512), VALMN(512), VALMX(512), IOK(512), 1)
 
          IFLAGS = IFLAG
          ICODET = 0
 
          iCondError = 0
          IF(IFLAGS .GT. 3) THEN
            CALL FINDUS (7, IFND, AEX, USC, U2P, U1P, PDROP)
            IF(IFND .EQ. 0) THEN
              VAL(512) = AEX
              VAL(513) = PDROP
            END IF
 
            !! set error flag if calculated condenser area = 0
            IF (AEX .LE. 0.0) THEN
               CALL error (2)
               CALL gotoxy(27, 11)
               CALL print('ERROR: Zero Condenser Area',26,-2)
               CALL gotoxy(27, 12)
               CALL print('Re-calculate Area.',18,-2)
 
               CALL inchr(1,j,key)
            END IF
 
 
            GO TO 155
          END IF
 
          iCondError = 0
          IF (VAL(512) .LE. 0.0 .AND. INAT .EQ. 2) THEN
             iCondError = 1
          END IF
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 150
      IF(IFLAG .EQ. 3) GO TO 90
 
      !! error check for zero condenser area
      IF (iCondError .EQ. 1) THEN
         iCondError = 0
 
         CALL error (1)
         CALL gotoxy(27, 11)
         CALL print('ERROR: Zero Condenser Area',26,-2)
 
         CALL inchr(1,j,key)
         go to 155
      END IF
 
C
C          LOCATION OF NATURAL CONVECTION CONDENSER
C
  158 CONTINUE
      IF(NFIX(VAL(246)) .EQ. 2) THEN
 
        WRITE(HEAD_LINE, '(''HOT-WALL CONDENSER LOCATION'')')
        CALL BLOCK_HEAD(HEAD_LINE)
 
        CALL SETINT(2,2,2,2,0,0,0,0,0,0,0,0,0,0)
 
        VAL(585) = 1
        IF(NFIX(VAL(585)) .EQ. 0) THEN
          num3 = 3
 !        CALL SETIOK(2,1,1,1,0,0,0,0,0,0,0,0,0,0,IOK(588))
 !        CALL EDTFIL('BLOCK56.DAT', 03, 04, 04, 1, IFLAG, IOUT,
 !   .                 VAL(585), VALMN(585), VALMX(585), IOK(585), -1)
 !        IF(NFIX(VAL(585)) .EQ. 1) GO TO 158
 
        ELSE
          num3 = 4
          CALL SETIOK(2,1,1,1,0,0,0,0,0,0,0,0,0,0,IOK(588))
          IF(IRFTYP .EQ. 4) THEN
            CALL EDTFIL('BLOCK56.DAT', 01, 03, 04, 1, IFLAG, IOUT,
     .                   VAL(588), VALMN(588), VALMX(588), IOK(588), -1)
            VAL(589) = 1.0
          ELSE
            num3 = 7
            CALL EDTFIL('BLOCK56.DAT', 04, 04, 01, 1, IFLAG, IOUT,
     .                   VAL(588), VALMN(588), VALMX(588), IOK(588), -1)
          END IF
  !       IF(NFIX(VAL(585)) .EQ. 0) GO TO 158
 
        END IF
C
C          ERROR CHECK ON AREAS SPECIFIED
C
        IF(NFIX(VAL(585)) .NE. 0) THEN
          AREA = VAL(589) + VAL(590) + VAL(591)
 
          IF(IRFTYP .EQ. 4) THEN
              AREA = 1.0
              VAL(589) = 1.0
          END IF
 
          area_error = abs(area - 1.0)
          IF(AREA_error .GT. 0.005) THEN
              CALL error (2)
              CALL gotoxy(20, 11)
              CALL print('Sum of Fractional Areas of Condenser',36,-2)
              CALL gotoxy(20, 12)
              CALL print('Must Equal Unity.',17,-2)
 
             CALL inchr(1,j,key)
            GO TO 158
          END IF
        END IF
 
        IF(IFLAG .EQ. 1) GO TO 20
        IF(IFLAG .EQ. 2) GO TO 155
 
      END IF
 
      IF(IFLAG .EQ. 3) GO TO 90
 
      IF(IOPT  .EQ. 4) GO TO 160
      IF(ICYCL .NE. 3) GO TO 170
C
C          FREEZER LOOP CONDENSER
C
  160 CONTINUE
      IF(ICYCL .NE. 3) GO TO 90
 
      IFAN = NFIX(VAL(266))
      IF(IFAN .EQ. 1) THEN
         ICODET = 14
      ELSE
         ICODET = 0
      END IF
 
 
      WRITE(HEAD_LINE, '(''FREEZER CONDENSER DATA'')')
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(2,0,0,0,0,0,0,2,0,0,0,0,0,0)
 
      num3 = 8
      IF(IFAN .EQ. 2 .OR. IFAN .EQ. 3) THEN
        CALL SETIOK(0,1,1,1,1,1,1,0,0,0,0,0,0,0,IOK(262))
        CALL EDTFIL('BLOCK23.DAT', 07, 08, 13, 1, IFLAG, IOUT,
     .               VAL(262), VALMN(262), VALMX(262), IOK(262), 1)
        IFAN = NFIX(VAL(266))
        IF(IFAN .EQ. 1) GO TO 160
      ELSE
        CALL SETIOK(0,1,1,1,1,1,1,0,0,0,0,0,0,0,IOK(262))
        CALL EDTFIL('BLOCK23.DAT', 07, 08, 13, 2, IFLAG, IOUT,
     .               VAL(262), VALMN(262), VALMX(262), IOK(262), 1)
        IFAN = NFIX(VAL(266))
        IF(IFAN .NE. 1) GO TO 160
      END IF
 
      ICODET = 0
      IF(IFLAG .GT. 3) THEN
        IF(VAL(451) .EQ. VAL(446)) THEN
          VAL(447) = VAL(269)
          VAL(448) = VAL(273)
        END IF
 
        CALL FAN(2, IFAN_RET, FAN_PWR, FAN_AIR)
          IF(IFAN_RET .EQ. 0) THEN
            VAL(269) = FAN_PWR
            VAL(273) = FAN_AIR
          END IF
        GO TO 160
      END IF
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) THEN
        IF(NFIX(VAL(246)) .EQ. 2) GO TO 158
        GO TO 155
      END IF
      IF(IFLAG .EQ. 3) GO TO 90
C
C          REST OF CONDENSER DATA
C
  165 CONTINUE
 
      INAT =  VAL(266)
 
   !! check on dual loop -- do not allow a natural convection condenser
      IF (ICYCL .EQ. 3 .AND. INAT .ne. 1) THEN
         CALL error (2)
         CALL gotoxy(20, 11)
         CALL print('Natural Convection Condenser Not Allowed ',41,-2)
         CALL gotoxy(20, 12)
         CALL print('in Dual Loop Systems          ',30,-2)
 
         CALL inchr(1,j,key)
         GO TO 160
      END IF
 
      WRITE(HEAD_LINE, '(''FREEZER CONDENSER DATA, CONTINUED'')')
 
      CALL SETINT(2,2,2,2,2,2,0,0,0,0,0,0,0,0)
      SELECT CASE (INAT)
        CASE (1)                                          !Forced convection
          DEGSUP = VAL(262)
 
          IF (nfix(VAL(705)) .eq. 0) THEN
             ICODET = 13
             num3 = 6
          ELSE
             ICODET = 21
             num3 = 3
          END IF
 
 
          CALL BLOCK_HEAD(HEAD_LINE)
 
          CALL SETIOK(0,0,0,0,0,2,0,0,0,0,0,0,0,0,IOK(272))
          CALL SETIOK(0,0,0,0,0,0,0,0,0,0,0,0,0,0,IOK(706))
 
          IF (nfix(VAL(705)) .eq. 0) THEN
             CALL EDTFIL('BLOCK24.DAT', 06, 06, 1, 0, IFLAG, IOUT,
     .                    VAL(272), VALMN(272), VALMX(272), IOK(272), 1)
             VAL(706) = VAL(273)
             VAL(708) = VAL(277)
          ELSE
             CALL EDTFIL('BLOCK65.DAT', 03, 03, 1, 0, IFLAG, IOUT,
     .                    VAL(706), VALMN(706), VALMX(706), IOK(706), 1)
             VAL(273) = VAL(706)
             VAL(277) = VAL(708)
          END IF
 
          IFLAGS = IFLAG
          ICODET = 0
 
          IF (IFLAG .eq. -4) THEN
             IF (nfix(VAL(705)) .eq. 0) THEN
                VAL(705) = 1
             ELSE
                VAL(705) = 0
             END IF
             go to 165
          END IF
 
          IF(IFLAGS .GT. 3 .AND. nfix(VAL(705)) .EQ. 0) THEN
            CALL FINDUS (4, IFND, AEX, USC, U2P, U1P, PDROP)
            IF(IFND .EQ. 0) THEN
              VAL(272) = AEX
              VAL(274) = USC
              VAL(275) = U2P
              VAL(276) = U1P
              VAL(277) = PDROP
            END IF
            GO TO 165
          END IF
 
        CASE (2, 3)                                          !Free convection
  !       val(705) = 0  !forced convection UA option
          num3 = 2
          CALL BLOCK_HEAD(HEAD_LINE)
 
          icodet = 18
          CALL SETIOK(0,2,0,0,0,0,0,0,0,0,0,0,0,0,IOK(516))
          CALL EDTFIL('BLOCK50.DAT', 02, 02, 1, 0, IFLAG, IOUT,
     .                 VAL(516), VALMN(516), VALMX(516), IOK(516), 1)
 
          IFLAGS = IFLAG
          ICODET = 0
 
          IF(IFLAGS .GT. 3) THEN
            CALL FINDUS (8, IFND, AEX, USC, U2P, U1P, PDROP)
            IF(IFND .EQ. 0) THEN
              VAL(517) = PDROP
            END IF
            GO TO 165
          END IF
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 160
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT  .EQ. 4) GO TO 90
C
C          COMPRESSOR MODEL OPTIONS
C
  170 CONTINUE
 
      WRITE(HEAD_LINE, '(''COMPRESSOR MODEL OPTIONS'')')
 
      CALL SETINT(0,0,0,0,0,0,2,0,0,0,0,0,0,0)
 
      !! set the model type and compressor type options
      VAL(455) = 1
      VALMX(458) = 1
      VAL(466) = 0
 
      IF(NFIX(VAL(462)) .EQ. 1) THEN
        num3 = 4
        CALL BLOCK_HEAD(HEAD_LINE)
 
        CALL SETIOK(1,1,1,1,1,1,2,1,1,1,1,1,1,1,IOK(462))
        CALL EDTFIL('BLOCK48.DAT', 08, 06, 13, 1, IFLAG, IOUT,
     .               VAL(458), VALMN(458), VALMX(458), IOK(458), 1)
        IF(NFIX(VAL(462)) .EQ. 0) GO TO 170
      ELSE
        num3 = 2
        CALL BLOCK_HEAD(HEAD_LINE)
 
        CALL SETIOK(1,1,1,1,1,1,2,1,1,1,1,1,1,1,IOK(462))
        CALL EDTFIL('BLOCK48.DAT', 08, 06, 13, 2, IFLAG, IOUT,
     .               VAL(458), VALMN(458), VALMX(458), IOK(458), 1)
        IF(NFIX(VAL(462)) .EQ. 1) GO TO 170
      END IF
 
      IF(nfix(VAL(455)) .EQ. 2 .AND. nfix(VAL(458)) .EQ. 2) THEN
        CALL error (2)
        CALL gotoxy(20, 11)
        CALL print('The Efficiency Model is Available Only ',39,-2)
        CALL gotoxy(20, 12)
        CALL print('for a Reciprocating Type Compressor.',36,-2)
 
        CALL inchr(1,j,key)
        GO TO 170
 
      END IF
 
      IF(nfix(VAL(455)) .EQ. 1 .AND. nfix(VAL(462)) .EQ. 1
     .                         .AND. nfix(VAL(466)) .EQ. 1
     .                         .AND. iopt .EQ. 4) THEN
        CALL error (2)
        CALL gotoxy(21, 11)
        CALL print('WARNING: Shut-Off Valve Specified for',37,-2)
        CALL gotoxy(21, 12)
        CALL print('a Reciprocating Type Compressor.',32,-2)
 
        CALL inchr(1,j,key)
       !GO TO 170
 
      END IF
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 700
      IF(IFLAG .EQ. 3 .and. nfix(VAL(458)) .NE. 0) GO TO 90
 
C
C          COMPRESSOR: FRESH FOOD LOOP
C
  175 CONTINUE
      IF(nfix(VAL(458)) .EQ. 0) THEN
         if(iopt .eq. 0) go to 195
         CALL errhan(0)
         IF5 = 0
         filmap = ' '
         IF(ICYCL .EQ. 3) THEN
            CALL GETFIL(8, filmap, filmap1, icod, IF5)
         ELSE
            CALL GETFIL(7, filmap, filmap1, icod, IF5)
         END IF
         IF(icod .EQ. 0) filmap1 = filmap
         CALL errhan(1)
         GO TO 180
      END IF
 
      IF(ICYCL .NE. 3) THEN
        WRITE(HEAD_LINE, '(''COMPRESSOR DATA'')')
      ELSE
        WRITE(HEAD_LINE, '(''FRESH FOOD COMPRESSOR DATA'')')
      END IF
 
      CALL BLOCK_HEAD(HEAD_LINE)
 
      !! Set up a branch code
      ibranch = 0
      IF ((ICYCL .EQ. 2 .OR. ICYCL .EQ. 4) .AND. nfix(VAL(405)) .EQ. 4)
     .     ibranch = 1
      IF ((ICYCL .EQ. 2 .OR. ICYCL .EQ. 4) .AND. nfix(VAL(405)) .EQ. 5)
     .     ibranch = 2
      IF (nfix(VAL(458)) .EQ. 2) ibranch = ibranch + 3
 
      SELECT CASE (ibranch)
         CASE (0)
            NUM3 = 8
            CALL SETINT(2,2,2,2,0,0,2,2,0,0,0,0,0,0)
            CALL SETIOK(2,2,2,2,1,1,1,2,0,0,0,0,0,0,IOK(494))
            CALL EDTFIL('BLOCK49.DAT', 08, 08, 1, 0, IFLAG, IOUT,
     .                   VAL(494), VALMN(494), VALMX(494), IOK(494), 1)
 
         CASE (1)
            NUM3 = 9
            CALL SETINT(2,2,2,2,0,0,2,2,2,0,0,0,0,0)
            CALL SETIOK(2,2,2,2,1,1,1,2,2,0,0,0,0,0,IOK(494))
            CALL EDTFIL('BLOCK83.DAT', 09, 09, 4, 2, IFLAG, IOUT,
     .                   VAL(494), VALMN(494), VALMX(494), IOK(494), 1)
 
         CASE (2)
            NUM3 = 9
            CALL SETINT(2,2,2,2,0,0,2,2,2,0,0,0,0,0)
            CALL SETIOK(2,2,2,2,1,1,1,2,2,0,0,0,0,0,IOK(494))
            CALL EDTFIL('BLOCK83.DAT', 09, 09, 4, 1, IFLAG, IOUT,
     .                   VAL(494), VALMN(494), VALMX(494), IOK(494), 1)
 
         CASE (3)
            num3 = 10
            CALL SETINT(2,2,2,2,2,2,2,2,2,2,0,0,0,0)
            CALL SETIOK(0,0,0,1,0,0,0,0,1,0,0,0,0,0,IOK(279))
            CALL EDTFIL('BLOCK25.DAT', 10, 10, 1, 0, IFLAG, IOUT,
     .                   VAL(279), VALMN(279), VALMX(279), IOK(279), 1)
 
         CASE (4)
            num3 = 11
            CALL SETINT(2,2,2,2,2,2,2,2,2,2,2,0,0,0)
            CALL SETIOK(0,0,0,1,0,0,0,0,1,2,2,0,0,0,IOK(279))
            CALL EDTFIL('BLOCK84.DAT', 11, 11, 4, 2, IFLAG, IOUT,
     .                   VAL(279), VALMN(279), VALMX(279), IOK(279), 1)
 
         CASE (5)
            num3 = 11
            CALL SETINT(2,2,2,2,2,2,2,2,2,2,2,0,0,0)
            CALL SETIOK(0,0,0,1,0,0,0,0,1,2,2,0,0,0,IOK(279))
            CALL EDTFIL('BLOCK84.DAT', 11, 11, 4, 1, IFLAG, IOUT,
     .                   VAL(279), VALMN(279), VALMX(279), IOK(279), 1)
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 170
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT  .EQ. 4) GO TO 180
      IF(ICYCL .NE. 3) GO TO 190
C
C          COMPRESSOR: FREEZER LOOP
C
  180 CONTINUE
      IF(ICYCL .NE. 3 .AND. nfix(VAL(458)) .EQ. 0) GO TO 195
      IF(ICYCL .NE. 3) GO TO 90
 
      IF(nfix(VAL(458)) .EQ. 0) THEN
         CALL errhan(0)
         IF5 = 0
         CALL GETFIL(9, filmap, filmap2, icod, IF5)
         IF(icod .EQ. 0) filmap2 = filmap
         CALL errhan(1)
         GO TO 195
      END IF
 
      WRITE(HEAD_LINE, '(''FREEZER COMPRESSOR DATA'')')
      CALL BLOCK_HEAD(HEAD_LINE)
 
      IF(NFIX(VAL(458)) .EQ. 1) THEN
        NUM3 = 8
        CALL SETINT(2,2,2,2,0,0,2,2,0,0,0,0,0,0)
        CALL SETIOK(2,2,2,2,1,1,1,2,0,0,0,0,0,0,IOK(594))
        CALL EDTFIL('BLOCK49.DAT', 08, 08, 1, 0, IFLAG, IOUT,
     .               VAL(594), VALMN(594), VALMX(594), IOK(594), 1)
      ELSE
        num3 = 10
        CALL SETINT(2,2,2,2,2,2,2,2,2,2,0,0,0,0)
        CALL SETIOK(0,0,0,1,0,0,0,0,1,0,0,0,0,0,IOK(329))
        CALL EDTFIL('BLOCK25.DAT', 10, 10, 1, 0, IFLAG, IOUT,
     .               VAL(329), VALMN(329), VALMX(329), IOK(329), 1)
      END IF
 
 
  185 CONTINUE
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 175
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT  .EQ. 4) GO TO 90
      GO TO 190
C
C          ESTIMATE REFRIGERANT MASS FLOW (COMPRESSOR MAP ONLY)
C
  195 CONTINUE
 
      WRITE(HEAD_LINE, '(''COMPRESSOR OPERATION'')')
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
      CALL SETIOK(0,0,0,0,0,0,1,2,2,0,0,0,0,0,IOK(785))
 
      ibranch = 0
      IF (ICYCL .EQ. 3) ibranch = 1
      IF ((ICYCL .EQ. 2 .OR. ICYCL .EQ. 4)
     .                  .AND. nfix(VAL(405)) .EQ. 4) ibranch = 1
      IF ((ICYCL .EQ. 2 .OR. ICYCL .EQ. 4)
     .                  .AND. nfix(VAL(405)) .EQ. 5) ibranch = 2
 
      SELECT CASE (ibranch)
         CASE (0)
            num3 = 1
            CALL EDTFIL('BLOCK81.DAT', 02, 03, 4, 1, IFLAG, IOUT,
     .                   VAL(791), VALMN(791), VALMX(791), IOK(791), 1)
 
         CASE (1)
            num3 = 2
            CALL EDTFIL('BLOCK82.DAT', 03, 03, 1, 0, IFLAG, IOUT,
     .                   VAL(791), VALMN(791), VALMX(791), IOK(791), 1)
 
         CASE (2)
            num3 = 2
            CALL EDTFIL('BLOCK81.DAT', 02, 03, 4, 2, IFLAG, IOUT,
     .                   VAL(791), VALMN(791), VALMX(791), IOK(791), 1)
 
      END SELECT
 
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 175
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT  .EQ. 4) GO TO 90
C
C          INTERCHANGER DATA: FRESH FOOD LOOP
C
  190 CONTINUE
      IF(ICYCL .NE. 3) THEN
        WRITE(HEAD_LINE, '(''INTERCHANGER DATA'')')
      ELSE
        WRITE(HEAD_LINE, '(''FRESH FOOD INTERCHANGER DATA'')')
      END IF
 
      num3 = 5
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(2,0,0,2,2,0,0,0,0,0,0,0,0,0)
      IF(ICYCL .NE. 2) THEN
        CALL SETIOK(0,1,2,1,1,1,1,0,0,0,0,0,0,0,IOK(310))
        CALL EDTFIL('BLOCK26.DAT', 04, 01, 4, 1, IFLAG, IOUT,
     .               VAL(310), VALMN(310), VALMX(310), IOK(310), 1)
      ELSE
        CALL SETIOK(0,1,2,1,1,1,1,0,0,0,0,0,0,0,IOK(310))
        CALL EDTFIL('BLOCK26.DAT', 05, 05, 1, 0, IFLAG, IOUT,
     .               VAL(310), VALMN(310), VALMX(310), IOK(310), 1)
      END IF
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 700
      IF(IFLAG .EQ. 3) GO TO 90
      IF(IOPT  .EQ. 4) GO TO 200
      IF(ICYCL .NE. 3) GO TO 210
C
C          INTERCHANGER DATA: FREEZER LOOP
C
  200 CONTINUE
      IF(ICYCL .NE. 3) GO TO 210
 
      WRITE(HEAD_LINE, '(''FREEZER INTERCHANGER DATA'')')
      num3 = 5
      CALL BLOCK_HEAD(HEAD_LINE)
 
      CALL SETINT(2,0,0,2,2,0,0,0,0,0,0,0,0,0)
      IF(ICYCL .NE. 2) THEN
        CALL SETIOK(0,1,2,1,1,1,1,0,0,0,0,0,0,0,IOK(340))
        CALL EDTFIL('BLOCK26.DAT', 04, 01, 4, 1, IFLAG, IOUT,
     .               VAL(340), VALMN(340), VALMX(340), IOK(340), 1)
      ELSE
        CALL SETIOK(0,1,2,1,1,1,1,0,0,0,0,0,0,0,IOK(340))
        CALL EDTFIL('BLOCK26.DAT', 05, 05, 1, 0, IFLAG, IOUT,
     .               VAL(340), VALMN(340), VALMX(340), IOK(340), 1)
      END IF
      IF(IFLAG .EQ. 1) GO TO 20
      IF(IFLAG .EQ. 2) GO TO 190
      IF(IFLAG .EQ. 3) GO TO 90
C
C          CHECK ON PRINT OUT OR WRITING OR INPUT FILE
C
  210 CONTINUE
      IF(IPRINT .EQ. 1) RETURN
      GO TO 90
C
C          HANDLE <PgUp> COMMAND
C
  700 CONTINUE
      IBYPAS = -1
      GO TO 90
C
C          A BRANCH BACK TO 20 IS GIVEN IRET = 2
C
   20 CONTINUE
      IRET = 2
      RETURN
C
C          A BRANCH BACK TO 30 IS GIVEN IRET = 3
C
   30 CONTINUE
      IRET = 3
      RETURN
 
      END
