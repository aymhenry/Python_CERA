      SUBROUTINE FINDUS(LOC, IFND, AEX, USC, U2P, U1P, PDROP)
C     ******************************************************************
C     *         CALCULATE THE U VALUE FOR THE EVAPORATOR AND           *
C     *         DETERMINE THE PRESSURE DROP                            *
C     ******************************************************************
 
      LOGICAL         exists, change_window
 
      CHARACTER*72    head_line
 
      INTEGER*2       IOK
      INTEGER         ref_code
 
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
      COMMON /DISPLY/ change_window
      COMMON /EDTCOM/ icodet, inote
      COMMON /LIMITS/ IOK(800)
      COMMON /VALUES/ VAL(800), VALMN(800), VALMX(800)
C
C          CHECK ON AVAILABLE SIZE
C
      CALL MEMORY(ISIZE)
      IF (ISIZE .LE. 4900) THEN
         CALL error (2)
         CALL gotoxy(21, 11)
         CALL print('Insufficient Memory to Perform Anaysis', 38, -2)
         CALL gotoxy(21, 12)
         CALL print('See Manual for Suggestions on Memory', 36, -2)
 
         CALL inchr(1,j,key)
         ifnd = 1
         RETURN
       END IF
C
C          BRANCH ON THE CALL VARIABLE LOC
C
      iCYCL = nfix(VAL(170))
      IRFTYP = NFIX(VAL(1))
 
      DO WHILE (.TRUE.)
        ifnd = 1
        SELECT CASE (loc)
          CASE (1, 2, 5, 6)                                 !Evaporator
 
            IF(loc .LE. 2) THEN
              inote = 1
              WRITE(HEAD_LINE,
     .            '(''FORCED FLOW EVAPORATOR DESIGN DATA'')')
            ELSE
              inote = 2
              WRITE(HEAD_LINE,
     .            '(''NATURAL CONVECTION EVAPORATOR DESIGN DATA '')')
            END IF
 
            CALL BLOCK_HEAD(HEAD_LINE)
            inote = 0
 
            IF(loc .EQ. 1) THEN
              VAL(429) = VAL(216)
            END IF
 
            IF (loc .EQ. 2) THEN
              VAL(479) = VAL(236)
            END IF
 
            IF(loc .LE. 2) THEN
              ival = 420 + 50*(loc - 1)
              CALL setint(2,2,0,2,0,2,2,2,2,2,2,2,2,2)
              CALL setiok(2,2,2,2,2,2,2,1,0,2,2,2,1,1,IOK(ival))
 
              icodet = -1
              valmx(ival+12) = 0.8
              CALL edtfil('BLOCK54.DAT', 14, 14, 1, 0, iflag, iout,
     .                     VAL(ival), VALMN(ival), VALMX(ival),
     .                     IOK(ival) ,1)
              icodet = 0
              IF(iflag .NE. 0) RETURN
 
              WRITE(HEAD_LINE, '(''EVAPORATOR PLATE-FIN DESIGN DATA'')')
              inote = 2
              CALL BLOCK_HEAD(HEAD_LINE)
              inote = 0
 
              jval = 437 + 50*(loc - 1)
              CALL setint(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
              CALL setiok(2,2,2,1,1,1,1,0,0,0,0,0,0,0,IOK(jval))
              icodet = -1
              CALL edtfil('BLOCK55.DAT', 07, 07, 1, 0, iflag, iout,
     .                     VAL(jval), VALMN(jval), VALMX(jval),
     .                     IOK(jval), 1)
              icodet = 0
 
            ELSE
 
              ival = 628 + 16*(loc - 5)
              CALL setint(0,0,0,0,2,2,2,2,0,2,2,2,2,2)
              CALL setiok(1,1,1,1,2,2,2,2,1,2,2,1,1,1,IOK(ival))
 
              icodet = -1
              CALL edtfil('BLOCK62.DAT', 14, 14, 1, 0, iflag, iout,
     .                     VAL(ival), VALMN(ival), VALMX(ival),
     .                     IOK(ival) ,1)
              icodet = 0
            END IF
 
            IF(iflag .EQ. 1) RETURN
            IF(iflag .EQ. 2) CYCLE
            IF(iflag .EQ. 3) RETURN
C
C          FIND POINTER TO REFRIGERANT PROPERTY DATA
C
            num_ref = nfix(VAL(175))
            SELECT CASE (icycl)
              CASE (3)
                IF(loc .EQ. 1 .AND. num_ref .NE. 2) ipnt = 180
                IF(loc .EQ. 2 .AND. num_ref .NE. 2) ipnt = 190
                IF(loc .EQ. 1 .AND. num_ref .EQ. 2) ipnt = 320
                IF(loc .EQ. 2 .AND. num_ref .EQ. 2) ipnt = 325
 
              CASE DEFAULT
                IF(num_ref .NE. 2) ipnt = 180
                IF(num_ref .EQ. 2) ipnt = 320
 
            END SELECT
C
C          HANDLE PURE REFRIGERANTS EXACTLY
C
            change_window = .TRUE.
            IF(num_ref .EQ. 1) THEN
              ref_code = nfix(VAL(ipnt))
              SELECT CASE (ref_code)
                CASE (2)                                      !CFC-12
                  INQUIRE (FILE='CFC12SAT.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY CFC12SAT.PRP REFRIG.PRP$')
                  END IF
 
                CASE (6)                                      !HCFC-22
                  INQUIRE (FILE='HCFC22SA.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY HCFC22SA.PRP REFRIG.PRP$')
                  END IF
 
                CASE (11)                                     !R152a
                  INQUIRE (FILE='HFC152AS.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY HFC152AS.PRP REFRIG.PRP$')
                  END IF
 
                CASE (15)                                     !R134a
                  INQUIRE (FILE='HFC134AS.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY HFC134AS.PRP REFRIG.PRP$')
                  END IF
 
                CASE (19)                                     !Cyclopropane
                  INQUIRE (FILE='HC270SAT.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY HC270SAT.PRP REFRIG.PRP$')
                  END IF
 
                CASE (22)                                     !Propane
                  INQUIRE (FILE='HC290SAT.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY HC290SAT.PRP REFRIG.PRP$')
                  END IF
 
                CASE DEFAULT                                  !Pure fluid
                  CALL makeprp(loc)
 
              END SELECT
 
            ELSE
 
              CALL makeprp(loc)
 
            END IF
 
 
            OPEN(1, FILE='ERAEVAP.DAT', STATUS='UNKNOWN')
 
            IF(loc .LE. 2) THEN
              tsat_evap = VAL(ival+8) - 20.0
              WRITE(1, '(//9X,''1''////F10.2/F10.2/I10/F10.2/I10)')
     .                  VAL(ival), VAL(ival+1), nfix(VAL(ival+2)),
     .                  VAL(ival+3), nfix(VAL(ival+4))
 
              WRITE(1, '(F10.3/F10.3/F10.2)') VAL(ival+5),
     .                  VAL(ival+6), VAL(ival+7)
 
              WRITE(1, '(F10.2/F10.2/F10.2/I10//I10/)') VAL(jval),
     .                  VAL(jval+1), VAL(jval+2), nfix(VAL(jval+3)),
     .                  nfix(VAL(jval+5))
              WRITE(1, '(F10.1/////////////5(F10.2/),F10.2)')
     .                  VAL(IVAL+9), VAL(ival+8), tsat_evap,
     .                  VAL(ival+10), VAL(ival+11), VAL(ival+12),
     .                  VAL(ival+13)
 
            ELSE
              tsat_evap = VAL(ival+9) - 20.0
              tube_width = 100.0*SQRT(VAL(ival+4))
              WRITE(1, '(//9X,''2'',22(/),I10///5(F10.2/))')
     .                  nfix(VAL(ival)), VAL(ival+4), VAL(ival+5),
     .                  tube_width, VAL(ival+6), VAL(ival+7)
 
              WRITE(1, '(/5(F10.2/),F10.2)') VAL(ival+9), tsat_evap,
     .                  VAL(ival+10), VAL(ival+11), VAL(ival+12),
     .                  VAL(ival+13)
 
            END IF
 
            CLOSE (1)
 
            CALL setatr(79)
            CALL window(12, 18, 16, 63, 32, 0)
            CALL window(13, 17, 20, 59, 32, 1)
 
            CALL setatr(01)                                !Shadow
            IF(icolr .EQ. 1) THEN
              CALL window(19, 19, 18, 65, 219, 3)
              CALL window(13, 18, 64, 65, 219, 3)
            END IF
            CALL setatr(0)
            CALL setatr(79)
 
            CALL gotoxy(30,15)
            CALL print('Analyzing Evaporator$',20,-2)
            CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE., 'ERAEVAP$')
            CALL setatr(30)
 
            OPEN(1, FILE='ERAEVAP.OUT')
            READ(1, '(F10.3///F10.3/F10.3/F10.3)') aex, u1p, u2p, pdrop
            CLOSE (1)
 
            aex = aex/10.7584
            u1p = 5.6783*u1p
            u2p = 5.6783*u2p
            pdrop = 6.89*pdrop
 
            IF(loc .EQ. 1) THEN
              VAL(216) = VAL(429)
            END IF
 
            IF(loc .EQ. 2) THEN
              VAL(236) = VAL(479)
            END IF
 
          CASE (3, 4, 7, 8)                                 !Condenser
            iheading = -1
 
            IF(loc .LE. 4) THEN
              inote = 1
              WRITE(HEAD_LINE,
     .             '(''FORCED FLOW CONDENSER DESIGN DATA'')')
            ELSE
              inote = 1
 
              iheading = 0
              if (LOC .EQ. 7 .and. VAL(246) .EQ. 3) iheading = 1
              if (LOC .EQ. 8 .and. VAL(266) .EQ. 3) iheading = 1
 
              IF (iheading .eq. 0) THEN
                 WRITE(HEAD_LINE,
     .                '(''HOT-WALL CONDENSER DESIGN'')')
              ELSE
                 WRITE(HEAD_LINE,
     .                '(''STATIC CONDENSER DESIGN'')')
              END IF
            END IF
            CALL BLOCK_HEAD(HEAD_LINE)
            inote = 0
 
            IF(loc .EQ. 3) THEN
              VAL(542) = VAL(255)
            END IF
 
            IF(loc .EQ. 4) THEN
              VAL(572) = VAL(273)
            END IF
 
            IF(loc .LE. 4) THEN
              ival = 530 + 30*(loc - 3)
              CALL setint(0,0,2,2,0,2,0,2,2,2,2,2,2,2)
              CALL setiok(1,1,2,2,2,2,2,2,2,2,1,0,0,0,IOK(ival))
              icodet = -1
              CALL edtfil('BLOCK51.DAT', 14, 14, 1, 0, iflag, iout,
     .                     VAL(ival), VALMN(ival), VALMX(ival),
     .                     IOK(ival), 1)
              icodet = 0
              IF(iflag .NE. 0) RETURN
 
              ifin = nfix(VAL(ival))
              SELECT CASE (ifin)
                CASE (1)                                  !Wire tube
                  WRITE(HEAD_LINE,
     .                '(''CONDENSER WIRE-FIN DESIGN DATA'')')
 
                  inote = 2
                  CALL BLOCK_HEAD(HEAD_LINE)
                  inote = 1
 
                  jval = 547 + 30*(loc - 3)
                  CALL setint(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
                  CALL setiok(2,2,2,1,1,1,1,0,0,0,0,0,0,0,IOK(jval))
                  icodet = -1
                  CALL edtfil('BLOCK52.DAT', 07, 07, 1, 0, iflag, iout,
     .                         VAL(jval), VALMN(jval), VALMX(jval),
     .                         IOK(jval) ,1)
                  icodet = 0
 
                CASE (2)                                  !Tube-fin
                  WRITE(HEAD_LINE,
     .                '(''CONDENSER PLATE-FIN DESIGN DATA'')')
 
                  inote = 2
                  CALL BLOCK_HEAD(HEAD_LINE)
                  inote = 1
 
                  jval = 547 + 30*(loc - 3)
                  CALL setint(2,2,2,0,0,0,0,0,0,0,0,0,0,0)
                  CALL setiok(2,2,2,1,1,1,1,0,0,0,0,0,0,0,IOK(jval))
                  icodet = -1
                  CALL edtfil('BLOCK53.DAT', 07, 07, 1, 0, iflag, iout,
     .                         VAL(jval), VALMN(jval), VALMX(jval),
     .                         IOK(jval), 1)
                  icodet = 0
 
              END SELECT
 
            ELSE
             IF (iheading .eq. 1) THEN
              ival = 660 + 16*(loc - 7)
              CALL setint(2,2,2,2,0,2,2,2,2,0,0,0,0,0)
              CALL setiok(2,2,2,2,1,2,2,2,2,0,0,0,0,0,IOK(ival+5))
              icodet = -1
              val(ival) = 2.0
              val(ival+4) = 1.0
              CALL edtfil('BLOCK61.DAT', 9, 9, 1, 0, iflag, iout,
     .                     VAL(ival+5), VALMN(ival+5), VALMX(ival+5),
     .                     IOK(ival+5), 1)
              icodet = 0
             ELSE
 
              ival = 660
              kval = 503
              CALL setint(2,0,2,2,0,2,2,0,2,0,0,0,0,0)
              CALL setiok(2,1,2,2,1,2,2,1,2,0,0,0,0,0,IOK(kval))
              icodet = -1
 
              IF (IRFTYP .EQ. 4) THEN
                 CALL edtfil('BLOCK86.DAT', 3, 6, 4, 1, iflag, iout,
     .                        VAL(kval), VALMN(kval), VALMX(kval),
     .                        IOK(kval), 1)
              ELSE
                 CALL edtfil('BLOCK86.DAT', 9, 9, 1, 0, iflag, iout,
     .                        VAL(kval), VALMN(kval), VALMX(kval),
     .                        IOK(kval), 1)
              END IF
 
              icodet = 0
 
              IF(iflag .NE. 0) RETURN
 
              WRITE(HEAD_LINE,
     .            '(''CONDENSER TUBING DESIGN DATA'')')
 
              inote = 2
              CALL BLOCK_HEAD(HEAD_LINE)
              inote = 1
 
              kval = 345
              CALL setint(2,2,2,2,0,0,0,0,0,0,0,0,0,0)
              CALL setiok(2,2,2,2,0,0,0,0,0,0,0,0,0,0,IOK(kval))
              icodet = -1
              CALL edtfil('BLOCK87.DAT', 04, 04, 1, 0, iflag, iout,
     .                     VAL(kval), VALMN(kval), VALMX(kval),
     .                     IOK(kval), 1)
              icodet = 0
             END IF
            END IF
 
            IF(iflag .EQ. 1) RETURN
            IF(iflag .EQ. 2) CYCLE
            IF(iflag .EQ. 3) RETURN
C
C          FIND POINTER TO FREFRIGERANT PROPERTY DATA
C
            num_ref = nfix(VAL(175))
            SELECT CASE (icycl)
              CASE (3)
                IF(loc .EQ. 3 .AND. num_ref .NE. 2) ipnt = 180
                IF(loc .EQ. 4 .AND. num_ref .NE. 2) ipnt = 190
                IF(loc .EQ. 3 .AND. num_ref .EQ. 2) ipnt = 320
                IF(loc .EQ. 4 .AND. num_ref .EQ. 2) ipnt = 325
 
              CASE DEFAULT
                IF(num_ref .NE. 2) ipnt = 180
                IF(num_ref .EQ. 2) ipnt = 320
 
            END SELECT
C
C          HANDLE PURE REFRIGERANTS EXACTLY
C
            change_window = .TRUE.
            IF(num_ref .EQ. 1) THEN
              ref_code = nfix(VAL(ipnt))
              SELECT CASE (ref_code)
                CASE (2)                                      !CFC-12
                  INQUIRE (FILE='CFC12SAT.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY CFC12SAT.PRP REFRIG.PRP$')
                  END IF
 
                CASE (6)                                      !HCFC-22
                  INQUIRE (FILE='HCFC22SA.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY HCFC22SA.PRP REFRIG.PRP$')
                  END IF
 
                CASE (11)                                     !R152a
                  INQUIRE (FILE='HFC152AS.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY HFC152AS.PRP REFRIG.PRP$')
                  END IF
 
                CASE (15)                                     !R134a
                  INQUIRE (FILE='HFC134AS.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY HFC134AS.PRP REFRIG.PRP$')
                  END IF
 
                CASE (19)                                     !Cyclopropane
                  INQUIRE (FILE='HC270SAT.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                            'COPY HC270SAT.PRP REFRIG.PRP$')
                  END IF
 
                CASE (22)                                     !Propane
                  INQUIRE (FILE='HC290SAT.PRP', EXIST=exists)
                  IF(exists) THEN
                    CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,
     .                              'COPY HC290SAT.PRP REFRIG.PRP$')
                  END IF
 
                CASE DEFAULT                                  !Pure fluid
                  CALL makeprp(loc)
 
              END SELECT
 
            ELSE
 
              CALL makeprp(loc)
 
            END IF
 
            !! save these because they will be written over
            oldval4 = VAL(ival+4)
            oldval5 = VAL(ival+5)
            oldval6 = VAL(ival+6)
 
            oldval7 = VAL(ival+7)
            oldval8 = VAL(ival+8)
            oldval12 = VAL(ival+12)
            oldval13 = VAL(ival+13)
 
            OPEN(1, FILE='ERACOND.DAT', STATUS='UNKNOWN')
 
            IF(loc .LE. 4) THEN     !! forced convection
              tsat_cond = VAL(ival+11) + 20.0
              WRITE(1, '(//9X,''1''////I10//F10.2/F10.2/I10/F10.2)')
     .                  nfix(VAL(ival)), VAL(ival+2), VAL(ival+3),
     .                  nfix(VAL(ival+4)), VAL(ival+5)
 
              WRITE(1, '(I10/F10.3/F10.2/F10.2)') nfix(VAL(ival+6)),
     .                  VAL(ival+7), VAL(ival+8), VAL(ival+9)
 
              WRITE(1, '(F10.2/F10.2/F10.2/I10//I10/)') VAL(jval),
     .                  VAL(jval+1), VAL(jval+2), nfix(VAL(jval+3)),
     .                  nfix(VAL(jval+5))
 
              WRITE(1, '(F10.1////////////////F10.2/F10.2/F10.2//)')
     .                  VAL(ival+12)*Val(ival+10), VAL(ival+11),
     .                  tsat_cond, VAL(ival+13)
 
              AEX1 = 1.0  !! handle if statement later that should apply
                          !! only to a free convective condenser
 
            ELSE                   !! free convection
              tsat_cond = VAL(ival+12) + 20.0
 
              AEX1 = 1.0
              if (loc. EQ. 7.and. iheading .eq. 0) THEN
                 AEX1 = 0.0
                 fTUBE0 = 0.0
                 fLPASS = 0.0
                 nTubes = 0
                 fWidth = 0
 
                 VAL(589) = 0.0
                 VAL(590) = 0.0
                 VAL(591) = 0.0
 
                 !! top surface
                 IF ((IRFTYP .LT. 3 .OR. IRFTYP. GT. 4)
     .                             .AND. VAL(510) .GT. 0.0
     .                             .AND. VAL(511) .GT. 0.0) THEN
                    CALL FIN(VAL(509), NFIX(VAL(510)), VAL(511), fSum)
                    AEX1 = AEX1 + fSum
                    nTubes = nTubes + VAL(510)
                    fTUBE0 = fTUBE0 + VAL(511)
                    fWidth = fWidth + VAL(509)
 
                    VAL(591) = fSum
                 END IF
 
                 !! back surface
                 IF ((IRFTYP .LT. 3 .OR. IRFTYP. GT. 4)
     .                             .AND. VAL(507) .GT. 0.0
     .                             .AND. VAL(508) .GT. 0.0) THEN
                    CALL FIN(VAL(506), NFIX(VAL(507)), VAL(508), fSum)
                    AEX1 = AEX1 + fSum
                    nTubes = nTubes + VAL(507)
                    fTUBE0 = fTUBE0 + VAL(508)
                    fWidth = fWidth + VAL(506)
 
                    VAL(590) = fSum
                 END IF
 
                 !! side surface
                 IF ((IRFTYP .NE. 3 .AND. VAL(504) .GT. 0.0)
     .                             .AND. VAL(505) .GT. 0.0) THEN
                    CALL FIN(VAL(503), NFIX(VAL(504)), VAL(505), fSum)
                    AEX1 = AEX1 + 2.0 * fSum
                    nTubes = nTubes + 2.0 * VAL(504)
                    fTUBE0 = fTUBE0 + 2.0 * VAL(505)
                    fWidth = fWidth + 2.0 * VAL(503)
 
                    VAL(589) = 2.0 * fSum
                 END IF
 
                 !! wall fractions
                 IF (AEX1 .GT. 0.0) THEN
                    VAL(589) = VAL(589) / AEX1
                    VAL(590) = VAL(590) / AEX1
                    VAL(591) = VAL(591) / AEX1
                 END IF
 
                 !! load VALs for the hot-wall
                 VAL(ival+4) = AEX1
                 VAL(ival+5) = fTUBE0 / 100.0
                 VAL(ival+6) = 10.0 * fWidth / FLOAT(nTubes+1)
 
                 VAL(ival+7) = VAL(345)
                 VAL(ival+8) = VAL(346)
                 VAL(ival+12) = VAL(347)
                 VAL(ival+13) = VAL(348)
              END IF
 
              tsat_cond = VAL(ival+12) + 20.0
 
              WRITE(1, '(//9X,''2'',24(/),I10///8(F10.2/))')
     .                  nfix(VAL(ival)), VAL(ival+4), VAL(ival+5),
     .                  VAL(ival+6), VAL(ival+7), VAL(ival+8),
     .                  VAL(ival+9), VAL(ival+10), VAL(ival+11)
 
              WRITE(1, '(/2(F10.2/),F10.2)') VAL(ival+12), tsat_cond,
     .                  VAL(ival+13)
 
            END IF
 
            !! resore old values
            VAL(ival+4) = oldval4
            VAL(ival+5) = oldval5
            VAL(ival+6) = oldval6
 
            VAL(ival+7) = oldval7
            VAL(ival+8) = oldval8
            VAL(ival+12) = oldval12
            VAL(ival+13) = oldval13
 
            CLOSE (1)
 
            CALL setatr(79)
            CALL window(12, 18, 16, 63, 32, 0)
            CALL window(13, 17, 20, 59, 32, 1)
 
            CALL setatr(01)                                !Shadow
            IF(icolr .EQ. 1) THEN
              CALL window(19, 19, 18, 65, 219, 3)
              CALL window(13, 18, 64, 65, 219, 3)
            END IF
            CALL setatr(0)
            CALL setatr(30)
 
            !! handle error condition of no area
            IF (AEX1 .GT. 0.0) THEN
 
               CALL gotoxy(30,15)
               CALL print('Analyzing Condenser$',19,-2)
               CALL DOSCAL(.FALSE., .TRUE., .FALSE., .FALSE.,'ERACOND$')
 
               OPEN(1, FILE='ERACOND.OUT')
               READ(1, '(F10.3///F10.3/F10.3/F10.3/F10.3)') aex, u1p,
     .                                                 usc, u2p, pdrop
               CLOSE (1)
            END IF
 
            aex = aex/10.7584
            usc = 5.6783*usc
            u1p = 5.6783*u1p
            u2p = 5.6783*u2p
            pdrop = 6.89*pdrop
 
            IF(loc .EQ. 3) THEN
              VAL(255) = VAL(542)
            END IF
 
            IF(loc .EQ. 4) THEN
              VAL(273) = VAL(572)
            END IF
 
            IF (loc. EQ. 7 .and. iheading .eq. 0) AEX = AEX1
 
        END SELECT
 
        ifnd = 0
        RETURN
 
      END DO
      END
C
      SUBROUTINE FAN(LOC, IRET_FAN, FAN_PWR, FAN_AIR)
C     ******************************************************************
C     *         CALCULATE THE FAN POWER AND AIR FLOW FROM FAN LAWS     *
C     ******************************************************************
C
      CHARACTER*72    head_line
 
      INTEGER*2       IOK
 
      COMMON /EDTCOM/ icodet, inote
      COMMON /LIMITS/ IOK(800)
      COMMON /VALUES/ VAL(800), VALMN(800), VALMX(800)
C
C          EDIT THE DATA FILE AND SET THE FLAGS
C
      iret_fan = 1
      inote = 2
 
      WRITE(HEAD_LINE, '(''VARIABLE SPEED FAN'')')
      CALL BLOCK_HEAD(HEAD_LINE)
      inote = 0
 
      ival = 395 + 50*(loc - 1)
 
      icodet = 12
 
      CALL setint(2,2,2,2,2,2,2,0,0,0,0,0,0,0)
      CALL setiok(1,2,2,2,1,1,2,0,0,0,0,0,0,0,IOK(ival))
      CALL edtfil('BLOCK42.DAT', 07, 02, 4, 1, iflag, iout,
     .             VAL(ival), VALMN(ival), VALMX(ival), IOK(ival), 1)
      icodet = 0
 
      IF(iflag .EQ. 1 .OR. iflag .EQ. 3) RETURN
 
      iret_fan = 0
      fan_pwr = VAL(ival+2)*(VAL(ival+6)/VAL(ival+1))**3
      fan_air = VAL(ival+3)*(VAL(ival+6)/VAL(ival+1))
 
      RETURN
      END
 
      SUBROUTINE FIN(Width, nTubes, fLen, fSum)
C     ******************************************************************
C     *         CALCULATE THE EFFECTIVE AREA FOR A HOT-WALL CONDENSER  *
C     ******************************************************************
      COMMON /VALUES/ VAL(800), VALMN(800), VALMX(800)
 
      DATA h /1.47/
 
      fWidth = Width / (12.0 * 2.54)   !! ft
      fDelta = VAL(786) / (12.0 * 25.4)   !! ft
      fLength = 0.0328 * fLen / float(nTubes)
      fCond = 0.5781 * VAL(787)
 
      !! find area per tube (not including fin effect yet)
      fM = SQRT(h / (fCond * fDelta))
      Acond = 2.0 * SQRT(fDelta * fCond / h) / 10.758 * fLength
 
      !! do the end pieces first
      fML = fM * fWidth / (nTubes + 1.0)
      tanh = (EXP(fML) - EXP(-fML)) / (EXP(fML) + EXP(-fML))
      fSum = Acond * tanh
 
      !! remaining tubes
      IF (nTubes .GT. 1) THEN
         fML = fML / 2.0
         tanh = (EXP(fML) - EXP(-fML)) / (EXP(fML) + EXP(-fML))
         fSum = fSum + (nTubes - 1) * Acond * tanh
      END IF
 
      RETURN
      END
