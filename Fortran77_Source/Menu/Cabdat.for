      SUBROUTINE CABDAT(IPASS,TITLE,VAL)
C     ******************************************************************
C     *    WRITE OUT THE INPUT DATA FOR THE CABINET LOADS PROGRAM      *
C     ******************************************************************
C
C     IPASS: FILE NUMBER FOR FILE 'CABDAT.DAT'
C
      CHARACTER TITLE(68,5)
      CHARACTER*10 LINE
C
      DIMENSION LINE(8)
      DIMENSION VAL(1)
C
      DATA ICBNT/4/
C
C          OPEN THE DUMMY FILE 'CABINET.FRM'
C
      IRFTYP = NFIX(VAL(1))
      IF (irftyp .NE. 6) THEN
         OPEN (ICBNT,FILE='CABINET.FRM',STATUS='UNKNOWN')
      ELSE
         OPEN (ICBNT,FILE='REFRIG.FRM',STATUS='UNKNOWN')
      END IF
 
      READ (ICBNT,'(5(1X/))')
C
C          WRITE OUT THE TITLE LINES
C
      DO J = 1,5
         WRITE(IPASS,1000) (TITLE(I,J),I = 1,68)
      END DO
C
C          CABINET TYPE DATA
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(4,IRFTYP-1,VAR,  LINE)
      CALL CABIO(2,IPASS,VAL(1),  LINE)
      CALL CABIO(4,7-IRFTYP,VAR,  LINE)
C
C          CABINET DIMENSIONS
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1:3, 7)                                     !Two compartments
            CALL CABIO(3,IPASS,VAL(2),  LINE)
            CALL CABIO(3,IPASS,VAL(3),  LINE)
            CALL CABIO(3,IPASS,VAL(4),  LINE)
            CALL CABIO(3,IPASS,VAL(622),LINE)
            CALL CABIO(3,IPASS,VAL(623),LINE)
            CALL CABIO(3,IPASS,VAL(625),LINE)
            CALL CABIO(3,IPASS,VAL(626),LINE)
            CALL CABIO(3,IPASS,VAL(007),LINE)
            CALL CABIO(3,IPASS,VAL(006),LINE)
 
         CASE (4)                                          !Chest freezer
            CALL CABIO(3,IPASS,VAL(2),  LINE)
            CALL CABIO(3,IPASS,VAL(3),  LINE)
            CALL CABIO(3,IPASS,VAL(4),  LINE)
            CALL CABIO(4,4,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(007),LINE)
            CALL CABIO(3,IPASS,VAL(006),LINE)
 
         CASE (5, 6)                                       !Upright freezer
            CALL CABIO(3,IPASS,VAL(2),  LINE)
            CALL CABIO(3,IPASS,VAL(3),  LINE)
            CALL CABIO(3,IPASS,VAL(4),  LINE)
            CALL CABIO(3,IPASS,VAL(17),  LINE)
            CALL CABIO(3,IPASS,VAL(18),  LINE)
            CALL CABIO(4,2,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(007),LINE)
            CALL CABIO(3,IPASS,VAL(006),LINE)
 
      END SELECT
C
C          COMPRESSOR ENCLOSURE FOR CHEST FREEZER
C
      SELECT CASE (IRFTYP)
         CASE (1, 2, 3, 5, 6, 7)                                          !!!
            CALL CABIO(4,2,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(57), LINE)
            CALL CABIO(3,IPASS,VAL(58), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(59), LINE)
                                                                          !!!
         CASE (4)
            CALL CABIO(4,2,    VAR,     LINE)
            CALL CABIO(4,2,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(35), LINE)
            CALL CABIO(3,IPASS,VAL(36), LINE)
 
      END SELECT
C
C          CABINET LINER DATA
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(3,IPASS,VAL(786),LINE)
      CALL CABIO(3,IPASS,VAL(787),LINE)
      CALL CABIO(3,IPASS,VAL(789),LINE)
      CALL CABIO(3,IPASS,VAL(790),LINE)
C
C          MULLION DATA
C
      IF (IRFTYP .LE. 3 .OR. IRFTYP .EQ. 7) THEN
         CALL CABIO(1,IPASS,VAR,     LINE)
         CALL CABIO(1,IPASS,VAR,     LINE)
         SELECT CASE (IRFTYP)
            CASE (1, 2, 7)
               CALL CABIO(3,IPASS,VAL(31), LINE)
               CALL CABIO(4,1,    VAR,     LINE)
               CALL CABIO(3,IPASS,VAL(32), LINE)
            CASE (3)
               CALL CABIO(4,1,    VAR,     LINE)
               CALL CABIO(3,IPASS,VAL(31), LINE)
               CALL CABIO(3,IPASS,VAL(32), LINE)
         END SELECT
      ELSE
         CALL CABIO(4,5,    VAR,     LINE)
      END IF
C
C          FREEZER INSULATION THICKNESS
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1)
            CALL CABIO(3,IPASS,VAL(41), LINE)
            CALL CABIO(3,IPASS,VAL(42), LINE)
            CALL CABIO(3,IPASS,VAL(42), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(44), LINE)
            CALL CABIO(3,IPASS,VAL(43), LINE)
 
         CASE (2)
            CALL CABIO(4,3,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(42), LINE)
            CALL CABIO(3,IPASS,VAL(45), LINE)
            CALL CABIO(3,IPASS,VAL(43), LINE)
 
         CASE (3)
            CALL CABIO(3,IPASS,VAL(41), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(42), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(45), LINE)
            CALL CABIO(3,IPASS,VAL(43), LINE)
 
         CASE (4)
            CALL CABIO(3,IPASS,VAL(11), LINE)
            CALL CABIO(4,2,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(12), LINE)
            CALL CABIO(3,IPASS,VAL(12), LINE)
            CALL CABIO(3,IPASS,VAL(12), LINE)
 
         CASE (5, 6)
            CALL CABIO(3,IPASS,VAL(41), LINE)
            CALL CABIO(3,IPASS,VAL(42), LINE)
            CALL CABIO(3,IPASS,VAL(42), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(45), LINE)
            CALL CABIO(3,IPASS,VAL(43), LINE)
 
         CASE (7)
            CALL CABIO(3,IPASS,VAL(41), LINE)
            CALL CABIO(3,IPASS,VAL(42), LINE)
            CALL CABIO(3,IPASS,VAL(42), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(55), LINE)
            CALL CABIO(3,IPASS,VAL(43), LINE)
 
      END SELECT
C
C          FRESH FOOD SECTION INSULATION THICKNESS
C
      IF (IRFTYP .LE. 3 .OR. IRFTYP .EQ. 7) THEN
         CALL CABIO(1,IPASS,VAR,     LINE)
         CALL CABIO(1,IPASS,VAR,     LINE)
         SELECT CASE (IRFTYP)
            CASE (1, 7)
               CALL CABIO(4,1,    VAR,     LINE)
               CALL CABIO(3,IPASS,VAL(52), LINE)
               CALL CABIO(3,IPASS,VAL(52), LINE)
               CALL CABIO(4,1,    VAR,     LINE)
               CALL CABIO(3,IPASS,VAL(55), LINE)
               CALL CABIO(3,IPASS,VAL(53), LINE)
            CASE (2)
               CALL CABIO(3,IPASS,VAL(51), LINE)
               CALL CABIO(4,2,    VAR,     LINE)
               CALL CABIO(3,IPASS,VAL(52), LINE)
               CALL CABIO(3,IPASS,VAL(54), LINE)
               CALL CABIO(3,IPASS,VAL(53), LINE)
            CASE (3)
               CALL CABIO(3,IPASS,VAL(51), LINE)
               CALL CABIO(3,IPASS,VAL(52), LINE)
               CALL CABIO(4,2,    VAR,     LINE)
               CALL CABIO(3,IPASS,VAL(55), LINE)
               CALL CABIO(3,IPASS,VAL(53), LINE)
         END SELECT
      ELSE
         CALL CABIO(4,8,    VAR,     LINE)
      END IF
C
C          COMPRESSOR HOUSING AND BOTTOM INSULATION
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1, 7)
            CALL CABIO(3,IPASS,VAL(54), LINE)
            CALL CABIO(3,IPASS,VAL(54), LINE)
            CALL CABIO(4,3,    VAR,     LINE)
 
         CASE (2)
            CALL CABIO(4,2,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(44), LINE)
            CALL CABIO(4,2,    VAR,     LINE)
 
         CASE (3)
            CALL CABIO(3,IPASS,VAL(54), LINE)
            CALL CABIO(3,IPASS,VAL(54), LINE)
            CALL CABIO(3,IPASS,VAL(44), LINE)
            CALL CABIO(4,2,    VAR,     LINE)
 
         CASE (4)
            CALL CABIO(3,IPASS,VAL(13), LINE)
            CALL CABIO(3,IPASS,VAL(13), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(14), LINE)
            CALL CABIO(3,IPASS,VAL(15), LINE)
 
         CASE (5, 6)
            CALL CABIO(3,IPASS,VAL(44), LINE)
            CALL CABIO(3,IPASS,VAL(44), LINE)
            CALL CABIO(4,3,    VAR,     LINE)
 
      END SELECT
C
C          MANUFACTURER'S VOLUMES
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1:3, 7)
            HXVUZ = VAL(131) + VAL(132) + VAL(133)
            HXVUR = VAL(136) + VAL(137) + VAL(138)
            CALL CABIO(3,IPASS,HXVUZ,   LINE)
            CALL CABIO(3,IPASS,VAL(134),LINE)
            CALL CABIO(3,IPASS,HXVUR,   LINE)
            CALL CABIO(3,IPASS,VAL(139),LINE)
 
         CASE (4)
            HXVUZ = VAL(37) + VAL(38)
            CALL CABIO(3,IPASS,HXVUZ,   LINE)
            CALL CABIO(3,IPASS,VAL(39), LINE)
            CALL CABIO(4,2,    VAR,     LINE)
 
         CASE (5)
            HXVUZ = VAL(131) + VAL(132) + VAL(133)
            CALL CABIO(3,IPASS,HXVUZ,   LINE)
            CALL CABIO(3,IPASS,VAL(134),LINE)
            CALL CABIO(4,2,    VAR,     LINE)
 
         CASE (6)
            HXVUZ = 0
            HXVUR = VAL(158) + VAL(159)
            volfz = VAL(155) * VAL(156) * VAL(157) / 1000.0
            CALL CABIO(3,IPASS,HXVUZ,   LINE)
            CALL CABIO(3,IPASS,volfz,   LINE)
            CALL CABIO(3,IPASS,HXVUR,   LINE)
            CALL CABIO(3,IPASS,VAL(160),LINE)
 
      END SELECT
C
C          SINGLE-DOOR REFRIGERATOR FREEZER CABINET
C
      IF (irftyp .NE. 6) THEN
         CALL CABIO (4, 5, VAR, LINE)
      ELSE
         CALL CABIO (1,IPASS, VAR, LINE)
         CALL CABIO (1,IPASS, VAR, LINE)
         CALL CABIO (3, IPASS, VAL(155), LINE)
         CALL CABIO (3, IPASS, VAL(156), LINE)
         CALL CABIO (3, IPASS, VAL(157), LINE)
      END IF
C
C          AIR AND CABINET TEMPERATURES
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1, 2, 3, 6, 7)
            CALL CABIO(3,IPASS,VAL(81), LINE)
            CALL CABIO(3,IPASS,VAL(82), LINE)
            CALL CABIO(3,IPASS,VAL(83), LINE)
            CALL CABIO(3,IPASS,VAL(84), LINE)
 
         CASE (4, 5)
            CALL CABIO(3,IPASS,VAL(86), LINE)
            CALL CABIO(3,IPASS,VAL(87), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(88), LINE)
 
      END SELECT
C
C          INSULATION RESISTIVITIES
C
      CALL WALEVP(2, RAT_FZ, RAT_ML)
      CALL WALEVP(3, RAT_FF, DUMMY)
 
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1, 2, 7)
            CALL CABIO(3,IPASS,RAT_FF,  LINE)
            CALL CABIO(3,IPASS,RAT_FZ,  LINE)
            CALL CABIO(3,IPASS,VAL(026),LINE)
            CALL CABIO(3,IPASS,VAL(100),LINE)
            CALL CABIO(3,IPASS,VAL(25), LINE)
            CALL CABIO(3,IPASS,VAL(99), LINE)
            CALL CABIO(3,IPASS,RAT_ML,  LINE)
 
         CASE (3)
            CALL CABIO(3,IPASS,RAT_FF,  LINE)
            CALL CABIO(3,IPASS,RAT_FZ,  LINE)
            CALL CABIO(3,IPASS,VAL(027),LINE)
            CALL CABIO(3,IPASS,VAL(101),LINE)
            CALL CABIO(3,IPASS,VAL(26), LINE)
            CALL CABIO(3,IPASS,VAL(100),LINE)
            CALL CABIO(3,IPASS,RAT_ML,  LINE)
 
         CASE (4)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,RAT_FF,  LINE)
            CALL CABIO(4,3,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(93), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
 
         CASE (5, 6)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,RAT_FF,  LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(94), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(93), LINE)
            CALL CABIO(4,1,    VAR,     LINE)
 
      END SELECT
 
C
C          GASKET HEAT LOSSES AND TYPE 5 INSULATION RESISTIVITY
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1:3)
            VAL105 = VAL(105)/100.0
            VAL104 = VAL(104)/100.0
            CALL CABIO(3,IPASS,VAL105,  LINE)
            CALL CABIO(3,IPASS,VAL104,  LINE)
 
         CASE (4, 5, 6)
            VAL106 = VAL(106)/100.0
            CALL CABIO(3,IPASS,VAL106,  LINE)
            CALL CABIO(4,1,    VAR,     LINE)
 
         CASE (7)
            VAL106 = VAL(106)/100.0
            CALL CABIO(3,IPASS,VAL106,  LINE)
            CALL CABIO(3,IPASS,VAL106,  LINE)
 
      END SELECT
C
C          DOOR OPENING SCHEDULES
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1:3)
            CALL CABIO(3,IPASS,VAL(81), LINE)
            CALL CABIO(3,IPASS,VAL(108),LINE)
            CALL CABIO(3,IPASS,VAL(109),LINE)
            CALL CABIO(3,IPASS,VAL(110),LINE)
            CALL CABIO(3,IPASS,VAL(111),LINE)
            CALL CABIO(3,IPASS,VAL(112),LINE)
 
         CASE (4, 5)
            CALL CABIO(3,IPASS,VAL(86), LINE)
            CALL CABIO(3,IPASS,VAL(114),LINE)
            CALL CABIO(4,2,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(115),LINE)
            CALL CABIO(3,IPASS,VAL(116),LINE)
 
         CASE (6)
            CALL CABIO(3,IPASS,VAL(81), LINE)
            CALL CABIO(3,IPASS,VAL(114),LINE)
            CALL CABIO(4,2,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(115),LINE)
            CALL CABIO(3,IPASS,VAL(116),LINE)
 
         CASE (7)
            CALL CABIO(3,IPASS,VAL(81), LINE)
            CALL CABIO(3,IPASS,VAL(114),LINE)
            CALL CABIO(3,IPASS,VAL(115),LINE)
            CALL CABIO(3,IPASS,VAL(116),LINE)
            CALL CABIO(3,IPASS,VAL(115),LINE)
            CALL CABIO(3,IPASS,VAL(116),LINE)
 
      END SELECT
C
C          ANCILLARY ELECTRICAL ENERGIES
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1:3)
            IF((VAL(735) + VAL(736) ) .NE. 0.0) THEN
               FF_FRACT = VAL(736)/(VAL(735)+VAL(736))
            ELSE
               FF_FRACT = 0
            END IF
            IF((VAL(735) + VAL(736) ) .NE. 0.0) THEN
               FZ_FRACT = VAL(735)/(VAL(735)+VAL(736))
            ELSE
               FZ_FRACT = 0
            END IF
  !         FF_FRACT = VAL(735)
  !         FZ_FRACT = VAL(736)
 
            FF_ELECT = VAL(730) + VAL(734)*FF_FRACT
            FZ_ELECT = VAL(726) + VAL(734)*FZ_FRACT
 
            CALL CABIO(3,IPASS,FF_ELECT,LINE)
            CALL CABIO(3,IPASS,FZ_ELECT,LINE)
            CALL CABIO(3,IPASS,VAL(720),LINE)
            CALL CABIO(3,IPASS,VAL(719),LINE)
            CALL CABIO(3,IPASS,VAL(721),LINE)
 
         CASE (4, 5, 6)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(737),LINE)
            CALL CABIO(4,1,    VAR,     LINE)
            CALL CABIO(3,IPASS,VAL(763),LINE)
            CALL CABIO(3,IPASS,VAL(764),LINE)
 
         CASE (7)
            FZ_FRACT = (VAL(31) + 0.5 * VAL(32)) / VAL(02)
            FF_FRACT = 1.0 - FZ_FRACT
 
            FF_ELECT = VAL(737)*FF_FRACT
            FZ_ELECT = VAL(737)*FZ_FRACT
 
            CALL CABIO(3,IPASS,FF_ELECT,LINE)
            CALL CABIO(3,IPASS,FZ_ELECT,LINE)
            CALL CABIO(3,IPASS,VAL(720),LINE)
            CALL CABIO(3,IPASS,VAL(719),LINE)
            CALL CABIO(3,IPASS,VAL(721),LINE)
 
      END SELECT
C
C          PENETRATIONS
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1:3)
            CALL CABIO(3,IPASS,VAL(556), LINE)
            CALL CABIO(3,IPASS,VAL(555), LINE)
 
         CASE (4, 5, 6)
            CALL CABIO(4,1,    VAR,      LINE)
            CALL CABIO(3,IPASS,VAL(557), LINE)
 
         CASE (7)
            CALL CABIO(3,IPASS,VAL(557)*FF_FRACT, LINE)
            CALL CABIO(3,IPASS,VAL(557)*FZ_FRACT, LINE)
 
      END SELECT
C
C          ANCILLARY THERMAL HEAT INPUTS
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      CALL CABIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1:3)
            FF_ELECT = VAL(730)*VAL(731) + VAL(734)*VAL(736)
            FZ_ELECT = VAL(726)*VAL(727) + VAL(734)*VAL(735)
            FF_REFRG = VAL(747)*VAL(748) + VAL(751)*VAL(753)
            FZ_REFRG = VAL(743)*VAL(744) + VAL(751)*VAL(752)
 
            IF(NFIX(VAL(740)) .EQ. 0) THEN
               FF_REFRG = 0
               FZ_REFRG = 0
            END IF
 
            CALL CABIO(3,IPASS,FF_ELECT, LINE)
            CALL CABIO(3,IPASS,FZ_ELECT, LINE)
            CALL CABIO(3,IPASS,FF_REFRG, LINE)
            CALL CABIO(3,IPASS,FZ_REFRG, LINE)
            CALL CABIO(3,IPASS,VAL(720), LINE)
            CALL CABIO(3,IPASS,VAL(719), LINE)
 
         CASE (4, 5, 6)
            FZ_ELECT = VAL(737)*VAL(738)
            FZ_REFRG = VAL(771)*VAL(772)
            IF(NFIX(VAL(770)) .EQ. 0) THEN
               FF_REFRG = 0
               FZ_REFRG = 0
            END IF
 
            CALL CABIO(4,1,    VAR,      LINE)
            CALL CABIO(3,IPASS,FZ_ELECT, LINE)
            CALL CABIO(4,1,    VAR,      LINE)
            CALL CABIO(3,IPASS,FZ_REFRG, LINE)
            CALL CABIO(4,1,    VAR,      LINE)
            CALL CABIO(3,IPASS,VAL(763), LINE)
 
         CASE (7)
            FF_ELECT = VAL(737)*VAL(738) * FF_FRACT
            FZ_ELECT = VAL(737)*VAL(738) * FZ_FRACT
            FF_REFRG = VAL(771)*VAL(772) * FF_FRACT
            FZ_REFRG = VAL(771)*VAL(772) * FZ_FRACT
 
            IF(NFIX(VAL(770)) .EQ. 0) THEN
               FF_REFRG = 0
               FZ_REFRG = 0
            END IF
 
            CALL CABIO(3,IPASS,FF_ELECT, LINE)
            CALL CABIO(3,IPASS,FZ_ELECT, LINE)
            CALL CABIO(3,IPASS,FF_REFRG, LINE)
            CALL CABIO(3,IPASS,FZ_REFRG, LINE)
            CALL CABIO(3,IPASS,VAL(720), LINE)
            CALL CABIO(3,IPASS,VAL(719), LINE)
 
      END SELECT
C
C          OUTPUT THE END DATA CARD
C
      CALL CABIO(1,IPASS,VAR,     LINE)
      RETURN
C
C
C          FORMAT STATEMENTS
C
 1000 FORMAT(68A1)
 1018 FORMAT('END')
      END
 
      SUBROUTINE CABIO(LOC,IPASS,VAR,LINE)
C     ******************************************************************
C     *    READ AND WRITE DATA FOR CABDAT.FOR SUBROUTINE               *
C     ******************************************************************
C
      CHARACTER*10 LINE
      DIMENSION LINE(8)
      DATA ICBNT /4/
C
C          BRANCH ON LOC
C
      SELECT CASE (LOC)
           CASE (1)
                READ (ICBNT,801) LINE
                WRITE(IPASS,801) LINE
           CASE (2)
                READ (ICBNT,801) LINE
                WRITE(IPASS,802) NFIX(VAR), (LINE(I), I = 2,8)
           CASE (3)
                READ (ICBNT,801) LINE
                WRITE(IPASS,803) VAR, (LINE(I), I = 2,8)
           CASE (4)
                NUMRED = IPASS
                DO WHILE (NUMRED .NE. 0)
                     READ(ICBNT,801) LINE
                     NUMRED = NUMRED - 1
                END DO
      END SELECT
      RETURN
C
C          FORMATS
C
  801 FORMAT(8A10)
  802 FORMAT(I10,7A10)
  803 FORMAT(F10.4,7A10)
      END
