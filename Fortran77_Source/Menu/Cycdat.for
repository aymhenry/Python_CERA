      SUBROUTINE CYCDAT(IPASS)
C     ******************************************************************
C     *    OUTPUT THE DATA FILE NEEDED TO RUN THE CYCLE PROGRAM        *
C     ******************************************************************
C
C     IPASS: FILE NUMBER FOR FILE 'CABDAT.CYC'
C
      CHARACTER TITLE
      CHARACTER*10 LINE
      CHARACTER*13 FILERA
      INTEGER CYCTYP,CYTYPS
      REAL MLEFT, LIQUID_HEAT
C
      DIMENSION LINE(8)
      COMMON/TITL/ITITL,TITLE(68,5)
      COMMON/VALUES/VAL(800),VALMN(800),VALMX(800)
      COMMON/FILINF/FILERA
      COMMON /skipwrit/ iwrite
C
      DATA ICYCL /4/
      DATA INEXT /10/
C
C          DEFINE REFRIGERATOR CABINET TYPE AND CYCLE CONFIGURATION
C
      iwrite = ipass
 
      IRFTYP = NFIX(VAL(1))
      if (irftyp .eq. 6) then
         val(209) = 2.0                          !natural conv evap
         val(117) = 1.0                          !manual defrost
      end if
 
      NUMCYC = 1
      CYCTYP = NFIX(VAL(170))
      CYTYPS = CYCTYP
      IF(CYCTYP .EQ. 4) CYCTYP = 2
      IF(CYCTYP .EQ. 3) NUMCYC = 2
C
C          OPEN THE DUMMY FILE 'CYCLE.FRM'
C
      OPEN (ICYCL,FILE='CYCLE.FRM',STATUS='UNKNOWN')
      READ (ICYCL,'(5(1X/))')
C
C          OPEN THE FILE LAST.BIN
C
      OPEN (INEXT, FILE='LAST.BIN',STATUS='UNKNOWN', FORM='BINARY')
C
C          OUTPUT THE PROBLEM TITLE AND R/F TYPE
C
      IF (IPASS .NE. 0) WRITE(IPASS,800) TITLE
C
C          OUTPUT THE FILE NAME
C
      IF (IPASS .NE. 0) WRITE(IPASS,807) FILERA
C
C          WRITE OUT THE R/F CONFIGURATION DATA
C
      CALL CYCIO(1,IPASS,VAR,     LINE)
      CALL CYCIO(2,IPASS,VAL(001),LINE)
      CALL CYCIO(2,IPASS,VAL(170),LINE)
      WRITE (INEXT) VAL(170)
C
C          MOISTURE CONTROL INDEX
C
      CALL CYCIO(1,IPASS,VAR,     LINE)
      CALL CYCIO(1,IPASS,VAR,     LINE)
      SELECT CASE (IRFTYP)
         CASE (1:3, 7)
            CALL CYCIO(2,IPASS,VAL(113),LINE)
 
         CASE DEFAULT
            CALL CYCIO(2,IPASS,VAL(117),LINE)
      END SELECT
      IF (IRFTYP .EQ. 6) THEN
         CALL CYCIO(3,IPASS,VAL(765),LINE)
      ELSE
         CALL CYCIO(3,IPASS,0.0,LINE)
      END IF
C
C          CONTROL OPTION FOR A LORENZ CYCLE OR DUAL EVAPORATOR CYCLE
C
      IF(CYCTYP .EQ. 2) THEN
         CALL CYCIO(1,IPASS,VAR,     LINE)
         CALL CYCIO(1,IPASS,VAR,     LINE)
         CALL CYCIO(2,IPASS,VAL(405),LINE)
         CALL CYCIO(1,IPASS,VAR,     LINE)
      ELSE
         READ (ICYCL,'(3(1X/))')
      END IF
C
C          COMPRESSOR OPTIONS
C
      CALL CYCIO(1,IPASS,VAR,LINE)
      CALL CYCIO(1,IPASS,VAR,LINE)
      CALL CYCIO(2,IPASS,VAL(455),LINE)
      CALL CYCIO(2,IPASS,VAL(458),LINE)
      CALL CYCIO(2,IPASS,VAL(462),LINE)
      CALL CYCIO(3,IPASS,VAL(464),LINE)
      CALL CYCIO(2,IPASS,VAL(466),LINE)
      WRITE (INEXT) VAL(455)
      WRITE (INEXT) VAL(458)
C
C          OUTPUT A LIST OF THE REFRIGERANT CODES
C
      IF (IPASS .NE. 0) WRITE(IPASS,806)
C
C          NEXT FOUR TITLE LINES
C
      CALL CYCIO(1,IPASS,VAR,LINE)
      READ (ICYCL,801) LINE
      IF(NUMCYC .EQ. 1) THEN
         IF (IPASS .NE. 0) WRITE(IPASS,801) LINE
      ELSE
         IF (IPASS .NE. 0)
     .      WRITE(IPASS,'(''INPUT DATA FOR THE FREEZER LOOP'')')
      END IF
      CALL CYCIO(1,IPASS,VAR,LINE)
      CALL CYCIO(1,IPASS,VAR,LINE)
C
C          LOOP OVER THE NUMBER OF REFRIGERATION SYSTEMS
C
      DO WHILE (NUMCYC .NE. 0)
C
C               REFRIGERATION DATA: REFRIGERANT CODES
C
         NUMREF = NFIX(VAL(175))
         IF(NUMCYC .EQ. 1) THEN
            IPNT = 180
            IF(NUMREF .EQ. 2) IPNT = 320
         ELSE
            IPNT = 190
            IF(NUMREF .EQ. 2) IPNT = 325
         END IF
         CALL CYCIO(2,IPASS,VAL(IPNT),LINE)
         WRITE (INEXT) VAL(IPNT)
         IF(NUMREF .NE. 1) THEN
            CALL CYCIO(2,IPASS,VAL(IPNT+2),LINE)
            WRITE (INEXT) VAL(IPNT+2)
         ELSE
            CALL CYCIO(1,IPASS,VAR,LINE)
            WRITE (INEXT) 0.0
         END IF
         IF(NUMREF .EQ. 3) THEN
            CALL CYCIO(2,IPASS,VAL(IPNT+4),LINE)
            WRITE (INEXT) VAL(IPNT+4)
         ELSE
            CALL CYCIO(1,IPASS,VAR,LINE)
            WRITE (INEXT) 0.0
         END IF
         READ (ICYCL,801) LINE
         IF (IPASS .NE. 0) WRITE(IPASS,802) NUMREF, (LINE(I), I = 2,8)
         WRITE (INEXT) NUMREF
C
C               REFRIGERATION DATA: MIXTURE INTERACTION COEFFICIENTS
C
         SELECT CASE (NUMREF)
            CASE (1)
               CALL CYCIO(1,IPASS,VAR,LINE)
               CALL CYCIO(1,IPASS,VAR,LINE)
               CALL CYCIO(1,IPASS,VAR,LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+1),LINE)
               CALL CYCIO(1,IPASS,VAR,LINE)
               CALL CYCIO(1,IPASS,VAR,LINE)
 
               WRITE (INEXT) 0.0
               WRITE (INEXT) 0.0
               WRITE (INEXT) 0.0
               WRITE (INEXT) VAL(IPNT+1)
               WRITE (INEXT) 0.0
               WRITE (INEXT) 0.0
 
            CASE (2)
               CALL CYCIO(3,IPASS,VAL(IPNT+3),LINE)
               CALL CYCIO(1,IPASS,VAR,LINE)
               CALL CYCIO(1,IPASS,VAR,LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+1),LINE)
               VAR = 1.0 - VAL(IPNT+1)
               CALL CYCIO(3,IPASS,VAR,LINE)
               CALL CYCIO(1,IPASS,VAR,LINE)
 
               WRITE (INEXT) VAL(IPNT+3)
               WRITE (INEXT) 0.0
               WRITE (INEXT) 0.0
               WRITE (INEXT) VAL(IPNT+1)
               WRITE (INEXT) VAR
               WRITE (INEXT) 0.0
 
            CASE (3)
               CALL CYCIO(3,IPASS,VAL(IPNT+6),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+7),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+8),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+1),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+3),LINE)
               MLEFT = 1.0 - VAL(IPNT+1) - VAL(IPNT+3)
               CALL CYCIO(3,IPASS,MLEFT,LINE)
 
               WRITE (INEXT) VAL(IPNT+6)
               WRITE (INEXT) VAL(IPNT+7)
               WRITE (INEXT) VAL(IPNT+8)
               WRITE (INEXT) VAL(IPNT+1)
               WRITE (INEXT) VAL(IPNT+3)
               WRITE (INEXT) MLEFT
         END SELECT
C
C               CONDENSER DATA
C
         IPNT  = 241 + 20*(NUMCYC - 1)
         IPNT2 = 254 + 18*(NUMCYC - 1)
         IPNT3 = 512 + 04*(NUMCYC - 1)
         IPNT4 = 700 + 05*(NUMCYC - 1)
 
         CALL CYCIO(1,IPASS,VAR,LINE)
         IF(NFIX(VAL(IPNT+5)) .EQ. 1) THEN
            CALL CYCIO(2,IPASS,VAL(IPNT+2),LINE)
         ELSE
            CALL CYCIO(2,IPASS,0.0,LINE)
            VAL(IPNT2)   = VAL(IPNT3)
            VAL(IPNT2+5) = VAL(IPNT3+1)
            VAL(IPNT2+2) = 0
            VAL(IPNT2+3) = 0
            VAL(IPNT2+4) = 0
            VAL(IPNT4) = 0       ! UA option for forced convection
         END IF
         CALL CYCIO(1,IPASS,VAR,LINE)
         IF(IRFTYP .EQ. 4 .OR. IRFTYP .EQ. 5) THEN
            IF(NFIX(VAL(IPNT+5)) .EQ. 1) THEN
               CALL CYCIO(3,IPASS,VAL(89),LINE)
            ELSE
  !!!          CALL CYCIO(3,IPASS,VAL(86),LINE)   !! 1/17/95
               CALL CYCIO(3,IPASS,VAL(89),LINE)
            END IF
         ELSE
            IF(NFIX(VAL(IPNT+5)) .EQ. 1) THEN
               CALL CYCIO(3,IPASS,VAL(85),LINE)
            ELSE
  !!!          CALL CYCIO(3,IPASS,VAL(81),LINE)  !! 1/17/95
               CALL CYCIO(3,IPASS,VAL(85),LINE)
            END IF
         END IF
         IF(NFIX(VAL(IPNT+5)) .EQ. 1) THEN
            CALL CYCIO(3,IPASS,VAL(IPNT2+1),LINE)
            CALL CYCIO(3,IPASS,VAL(IPNT+8),LINE)
         ELSE
            CALL CYCIO(3,IPASS,5.0,LINE)
            CALL CYCIO(3,IPASS,0.0,LINE)
         END IF
         CALL CYCIO(3,IPASS,VAL(IPNT2+5),LINE)
         IF(nfix(VAL(ipnt4)) .EQ. 0) THEN
            CALL CYCIO(3,IPASS,VAL(IPNT2+4),LINE)
            CALL CYCIO(3,IPASS,VAL(IPNT2+3),LINE)
            CALL CYCIO(3,IPASS,VAL(IPNT2+2),LINE)
            CALL CYCIO(3,IPASS,VAL(IPNT2),LINE)
         ELSE
            CALL CYCIO(3,IPASS,10.0,LINE)
            CALL CYCIO(3,IPASS,10.0,LINE)
            CALL CYCIO(3,IPASS,10.0,LINE)
            CALL CYCIO(3,IPASS,VAL(IPNT4+2)/10.0,LINE)
         END IF
         CALL CYCIO(3,IPASS,VAL(IPNT+1),LINE)
 
         VAPOR_HEAT = 0
         LIQUID_HEAT = 0
 
         IF(NUMCYC .EQ. 2 .OR. CYCTYP .NE. 3) THEN
            IF(IRFTYP .LE. 3) THEN
               IF(NFIX(VAL(740)) .EQ. 1) THEN
                  VAPOR_HEAT = VAL(743) + VAL(747) + VAL(751)
               ELSE IF(NFIX(VAL(740)) .EQ. 2) THEN
                  LIQUID_HEAT = VAL(743) + VAL(747) + VAL(751)
               END IF
            ELSE
               IF(NFIX(VAL(770)) .EQ. 1)  VAPOR_HEAT = VAL(771)
               IF(NFIX(VAL(770)) .EQ. 2)  LIQUID_HEAT = VAL(771)
            END IF
         END IF
 
         CALL CYCIO(3,IPASS,LIQUID_HEAT, LINE)          !Liquid line
         CALL CYCIO(3,IPASS,VAPOR_HEAT, LINE)           !Vapor line
C
C               SINGLE EVAPORATOR OR FRESH FOOD EVAPORATOR
C
         READ (ICYCL,801) LINE
         IF(CYCTYP .EQ. 2) THEN
            IF (IPASS .NE. 0) WRITE(IPASS,801) LINE
         ELSE
            IF (IPASS. NE. 0) WRITE(IPASS,'(''EVAPORATOR PARAMETERS'')')
         END IF
 
         IPNT  = 200 + 20*(NUMCYC - 1)
         IPNT2 = 412 + 04*(NUMCYC - 1)
         IPNT4 = 690 + 05*(NUMCYC - 1)
         CALL CYCIO(2,IPASS,VAL(IPNT+1),LINE)
         CALL CYCIO(1,IPASS,VAR,LINE)
 
         IF(NFIX(VAL(IPNT+9)) .EQ. 1) THEN
            CALL CYCIO(2,IPASS,VAL(IPNT+6),LINE)
         ELSE
            CALL CYCIO(2,IPASS,0.0,LINE)
            VAL(IPNT+15) = VAL(IPNT2)
            VAL(IPNT+19) = VAL(IPNT2+1)
 
            if (irftyp .EQ. 6) val(219) = val(781) !!! 1/8/95 correction
                                                   !!! for type 6 evaporator
                                                   !!! pressure drop
 
           VAL(IPNT+17) = 0
            VAL(IPNT+18) = 0
         END IF
 
         CALL CYCIO(1,IPASS,VAR,LINE)
 
         IF(IRFTYP .Eq. 4 .or. irftyp .Eq. 5) THEN
            CALL CYCIO(3,IPASS,VAL(87),LINE)
         ELSE
            IF(CYCTYP .EQ. 1) THEN
               TEVAP = 0.8*VAL(82) + 0.2*VAL(83)
               if (irftyp .eq. 6) tevap = val(83)
               CALL CYCIO(3,IPASS,TEVAP,LINE)
            END IF
            IF(CYCTYP .EQ. 2)
     .         CALL CYCIO(3,IPASS,VAL(83),LINE)
            IF(CYCTYP .EQ. 3) THEN
               IF(NUMCYC .EQ. 2) THEN
                  CALL CYCIO(3,IPASS,VAL(82),LINE)
               ELSE
                  CALL CYCIO(3,IPASS,VAL(83),LINE)
               END IF
            END IF
         END IF
 
         IF(NFIX(VAL(IPNT+9)) .EQ. 1) THEN
            CALL CYCIO(3,IPASS,VAL(IPNT+16),LINE)
            CALL CYCIO(3,IPASS,VAL(IPNT+12),LINE)
         ELSE
            CALL CYCIO(3,IPASS,5.0,LINE)
            CALL CYCIO(3,IPASS,0.0,LINE)
         END IF
 
         CALL CYCIO(3,IPASS,VAL(IPNT+19),LINE)
         IF(nfix(VAL(IPNT4)) .eq. 0) THEN
            CALL CYCIO(3,IPASS,VAL(IPNT+17),LINE)
            CALL CYCIO(3,IPASS,VAL(IPNT+18),LINE)
            if (irftyp .ne. 6) then
               CALL CYCIO(3,IPASS,VAL(IPNT+15),LINE)
            else
               CALL CYCIO(3,IPASS,0.5 * VAL(775) + val(776) ,LINE)
            end if
         ELSE
            CALL CYCIO(3,IPASS,10.0,LINE)
            CALL CYCIO(3,IPASS,10.0,LINE)
            CALL CYCIO(3,IPASS,VAL(IPNT4+2)/10.0,LINE)
         END IF
 
         IF(NFIX(VAL(IPNT+1)) .NE. 2) THEN
            CALL CYCIO(3,IPASS,VAL(IPNT+5),LINE)
         ELSE
            CALL CYCIO(3,IPASS,0.0,LINE)
         END IF
 
C
C          LORENZ CYCLE FREEZER EVAPORATOR
C
         IF(CYCTYP .EQ. 2) THEN
            CALL CYCIO(1,IPASS,VAR,LINE)
            IF(NFIX(VAL(229)) .EQ. 1) THEN
               CALL CYCIO(2,IPASS,VAL(226),LINE)
            ELSE
               CALL CYCIO(2,IPASS,0.0,LINE)
               VAL(237) = 0
               VAL(238) = 0
               VAL(235) = VAL(416)
               VAL(239) = VAL(417)
            END IF
            CALL CYCIO(1,IPASS,VAR,LINE)
            CALL CYCIO(3,IPASS,VAL(82),LINE)
            IF(NFIX(VAL(229)) .EQ. 1) THEN
               CALL CYCIO(3,IPASS,VAL(236),LINE)
               CALL CYCIO(3,IPASS,VAL(232),LINE)
               IF(nfix(VAL(IPNT4+5)) .EQ. 0) THEN
                  CALL CYCIO(3,IPASS,VAL(235),LINE)
                  UA = VAL(237)*VAL(235)
                  CALL CYCIO(3,IPASS,UA,LINE)
               ELSE
                  CALL CYCIO(3,IPASS,VAL(697)/10.0,LINE)
                  CALL CYCIO(3,IPASS,VAL(697),LINE)
               END IF
            ELSE
               CALL CYCIO(3,IPASS,5.0,LINE)
               CALL CYCIO(3,IPASS,0.0,LINE)
               CALL CYCIO(3,IPASS,VAL(235),LINE)
               CALL CYCIO(3,IPASS,VAL(235),LINE)
            END IF
            CALL CYCIO(3,IPASS,VAL(239),LINE)
         ELSE
            CALL CYCIO(4,9,VAR,LINE)
         END IF
C
C          COMPRESSOR
C
         ibranch = 0
         IF (CYCTYP .EQ. 2 .AND. nfix(VAL(405)) .EQ. 4) ibranch = 1
         IF (CYCTYP .EQ. 2 .AND. nfix(VAL(405)) .EQ. 5) ibranch = 2
         IF (CYCTYP .EQ. 3) ibranch = 3
 
         ICOMP_TYP = NFIX(VAL(458))
         SELECT CASE (ICOMP_TYP)
            CASE (0)                                       !Map
               CALL CYCIO(1,IPASS,VAR,LINE)
 
               SELECT CASE (ibranch)
                  CASE (0)                       ! standard
                     CALL CYCIO(3,IPASS,VAL(792),LINE)
                     CALL CYCIO(4,23,VAR,LINE)
                     CALL CYCIO(3,IPASS,VAL(791),LINE)
 
                  CASE (1)                       ! dual-evap, control 4
                     CALL CYCIO(4,1,VAR,LINE)
                     CALL CYCIO(3,IPASS,VAL(793),LINE)
                     CALL CYCIO(3,IPASS,VAL(792),LINE)
                     CALL CYCIO(4,21,VAR,LINE)
                     CALL CYCIO(3,IPASS,VAL(791),LINE)
 
                  CASE (2)                       ! dual-evap, control 5
                     CALL CYCIO(4,3,VAR,LINE)
                     CALL CYCIO(3,IPASS,VAL(792),LINE)
                     CALL CYCIO(3,IPASS,VAL(793),LINE)
                     CALL CYCIO(4,19,VAR,LINE)
                     CALL CYCIO(3,IPASS,VAL(791),LINE)
 
                  CASE (3)                       ! dual-loop cycle
                     IF(NUMCYC .EQ. 2) THEN
                        CALL CYCIO(4,1,VAR,LINE)
                        CALL CYCIO(3,IPASS,VAL(793),LINE)
                        CALL CYCIO(4,22,VAR,LINE)
                        CALL CYCIO(3,IPASS,VAL(791),LINE)
                     ELSE
                        CALL CYCIO(4,2,VAR,LINE)
                        CALL CYCIO(3,IPASS,VAL(792),LINE)
                        CALL CYCIO(4,21,VAR,LINE)
                        CALL CYCIO(3,IPASS,VAL(791),LINE)
                     END IF
 
               END SELECT
 
               IF(NUMCYC .EQ. 2) THEN
                  CALL CYCIO(3,IPASS,VAL(340),LINE)
               ELSE
                  CALL CYCIO(3,IPASS,VAL(310),LINE)
               END IF
 
            CASE (1)                                       !EER and capacity
               IPNT = 494 + 100*(NUMCYC - 1)
               CALL CYCIO(1,IPASS,VAR,LINE)
               CALL CYCIO(4,14,VAR,LINE)
 
               SELECT CASE (ibranch)
                  CASE (0)                       !! standard cycle
                     CALL CYCIO(3,IPASS,VAL(IPNT+7),LINE)
                     CALL CYCIO(4,4,VAR,LINE)
 
                  CASE (1)                       ! dual-evap, control 4
                     CALL CYCIO(4,1,VAR,LINE)
                     CALL CYCIO(3,IPASS,VAL(IPNT+8),LINE)
                     CALL CYCIO(3,IPASS,VAL(IPNT+7),LINE)
                     CALL CYCIO(4,2,VAR,LINE)
 
                  CASE (2)                       ! dual-evap, control 5
                     CALL CYCIO(4,3,VAR,LINE)
                     CALL CYCIO(3,IPASS,VAL(IPNT+7),LINE)
                     CALL CYCIO(3,IPASS,VAL(IPNT+8),LINE)
 
                  CASE (3)                       ! dual-loop cycle
                     IF(NUMCYC .EQ. 2) THEN
                        CALL CYCIO(4,1,VAR,LINE)
                        CALL CYCIO(3,IPASS,VAL(IPNT+7),LINE)
                        CALL CYCIO(4,3,VAR,LINE)
                     ELSE
                        CALL CYCIO(4,2,VAR,LINE)
                        CALL CYCIO(3,IPASS,VAL(IPNT+7),LINE)
                        CALL CYCIO(4,2,VAR,LINE)
                     END IF
 
               END SELECT
 
               CALL CYCIO(3,IPASS,VAL(IPNT),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+1),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+2),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+3),LINE)
               CALL CYCIO(2,IPASS,VAL(IPNT+4),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+6),LINE)
 
               !! compressor parameters
               WRITE (INEXT) VAL(IPNT)
               WRITE (INEXT) VAL(IPNT+1)
               WRITE (INEXT) VAL(IPNT+2)
               WRITE (INEXT) VAL(IPNT+4)
               WRITE (INEXT) VAL(IPNT+6)
               WRITE (INEXT) VAL(IPNT+7)
               WRITE (INEXT) VAL(IPNT+8)
 
               IF(NUMCYC .EQ. 2) THEN
                  CALL CYCIO(3,IPASS,VAL(340),LINE)
               ELSE
                  CALL CYCIO(3,IPASS,VAL(310),LINE)
               END IF
 
            CASE (2)                                       !Efficiency model
               IPNT = 279 + 50*(NUMCYC - 1)
               CALL CYCIO(1,IPASS,VAR,LINE)
 
               SELECT CASE (ibranch)
                  CASE (0)                       !! standard cycle
                     CALL CYCIO(3,IPASS,VAL(IPNT+9),LINE)
                     CALL CYCIO(4,4,VAR,LINE)
 
                  CASE (1)                       ! dual-evap, control 4
                     CALL CYCIO(4,1,VAR,LINE)
                     CALL CYCIO(3,IPASS,VAL(IPNT+10),LINE)
                     CALL CYCIO(3,IPASS,VAL(IPNT+9),LINE)
                     CALL CYCIO(4,2,VAR,LINE)
 
                  CASE (2)                       ! dual-evap, control 5
                     CALL CYCIO(4,3,VAR,LINE)
                     CALL CYCIO(3,IPASS,VAL(IPNT+9),LINE)
                     CALL CYCIO(3,IPASS,VAL(IPNT+10),LINE)
 
                  CASE (3)                       ! dual-loop cycle
                     IF(NUMCYC .EQ. 2) THEN
                        CALL CYCIO(4,1,VAR,LINE)
                        CALL CYCIO(3,IPASS,VAL(IPNT+9),LINE)
                        CALL CYCIO(4,3,VAR,LINE)
                     ELSE
                        CALL CYCIO(4,2,VAR,LINE)
                        CALL CYCIO(3,IPASS,VAL(IPNT+9),LINE)
                        CALL CYCIO(4,2,VAR,LINE)
                     END IF
 
               END SELECT
 
               CALL CYCIO(3,IPASS,VAL(IPNT),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+1),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+2),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+4),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+5),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+8),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+6),LINE)
               CALL CYCIO(3,IPASS,VAL(IPNT+7),LINE)
 
               IF(NUMCYC .EQ. 2) THEN
                  CALL CYCIO(3,IPASS,VAL(340),LINE)
               ELSE
                  CALL CYCIO(3,IPASS,VAL(310),LINE)
               END IF
               CALL CYCIO(4,12,VAR,LINE)
         END SELECT
C
C          INTERCHANGERS
C
         CALL CYCIO(1,IPASS,VAR,LINE)
 
         IPNT = 201 + 20*(NUMCYC - 1)
         IF(NFIX(VAL(IPNT)) .EQ. 2) THEN
            CALL CYCIO(3,IPASS,VAL(IPNT+4),LINE)
         ELSE
            CALL CYCIO(3,IPASS,0.0,LINE)
         END IF
 
         IF(NUMCYC .EQ. 2) THEN
            CALL CYCIO(3,IPASS,VAL(343),LINE)
         ELSE
            CALL CYCIO(3,IPASS,VAL(313),LINE)
         END IF
         SELECT CASE (CYTYPS)
            CASE (1, 3)
               CALL CYCIO(4,1,VAR,LINE)
            CASE (2)
               CALL CYCIO(3,IPASS,VAL(314),LINE)
            CASE (4)
               CALL CYCIO(3,IPASS,0.0,LINE)
         END SELECT
C
C          LOCATION OF NATURAL CONVECTION EVAPORATORS
C
 
         IF(NUMCYC .EQ. 1) THEN
            CALL CYCIO(1,IPASS,VAR,LINE)
 
            CALL WALEVP(1, FRACT_FF, FRACT_FZ)
 
            CALL CYCUAS(UA_FF_ENV, UA_FZ_ENV, UA_ML, UA_FF_CND,
     .                  UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS)
 
 
            CALL CYCIO(3,IPASS,UA_FF_ENV,LINE)
            CALL CYCIO(3,IPASS,UA_FZ_ENV,LINE)
            CALL CYCIO(3,IPASS,UA_ML,LINE)
            CALL CYCIO(3,IPASS,UA_FF_CND,LINE)
            CALL CYCIO(3,IPASS,UA_FZ_CND,LINE)
            CALL CYCIO(3,IPASS,UA_FF_HXS,LINE)
            CALL CYCIO(3,IPASS,UA_FZ_HXS,LINE)
 
            CALL CYCIO(3,IPASS,FRACT_FF,LINE)
            CALL CYCIO(3,IPASS,FRACT_FZ,LINE)
 
            IF(nfix(VAL(372)) .EQ. 2 .AND. VAL(209) .EQ. 2) THEN
               CALL CYCIO(2,IPASS,1.0,LINE)
            ELSE
               CALL CYCIO(2,IPASS,0.0,LINE)
            END IF
 
            IF(nfix(VAL(362)) .EQ. 2 .AND. VAL(229) .EQ. 2) THEN
               CALL CYCIO(2,IPASS,1.0,LINE)
            ELSE
               CALL CYCIO(2,IPASS,0.0,LINE)
            END IF
 
         END IF
C
C          CYCLE-DEPENDENT ELECTRICAL LOADS
C
         IF(NUMCYC .EQ. 1) THEN
            CALL CYCIO(1,IPASS,VAR,LINE)
            DEFROST = 0
            IF(IRFTYP .LE. 3 .OR. IRFTYP .EQ. 7) THEN
               IF (val(711) .ne. 0.0)
     .            DEFROST = VAL(713)*(VAL(712)/60.0)/VAL(711)
               CALL CYCIO(3,IPASS,DEFROST,LINE)
               CALL CYCIO(3,IPASS,VAL(716),LINE)
               CALL CYCIO(3,IPASS,VAL(715),LINE)
               CALL CYCIO(3,IPASS,VAL(717),LINE)
 
            ELSE
               if (val(756) .NE. 0.0)
     .            DEFROST = VAL(758)*(VAL(757)/60.0)/VAL(756)
               if (irftyp .eq. 6) defrost = 0
               CALL CYCIO(3,IPASS,DEFROST,LINE)
               CALL CYCIO(4,1,VAR,LINE)
               CALL CYCIO(3,IPASS,VAL(760),LINE)
               CALL CYCIO(3,IPASS,VAL(761),LINE)
            END IF
         END IF
C
C          REWIND CYCLE.DAT FILE AND RESET UP HEADERS FOR
C          DUAL LOOP CYCLE
C
         IF(NUMCYC .EQ. 2) THEN
            REWIND ICYCL
            IF (IPASS .NE. 0) CALL CYCIO(4,26,VAR,LINE)
            IF (IPASS .NE. 0)
     .         WRITE(IPASS,'(/''INPUT DATA FOR THE FRESH FOOD LOOP'')')
            CALL CYCIO(1,IPASS,VAR,LINE)
            CALL CYCIO(1,IPASS,VAR,LINE)
         END IF
      NUMCYC = NUMCYC - 1
      END DO
 
      CLOSE (INEXT)
      CLOSE (ICYCL)
 
      RETURN
C
C          FORMATS
C
  800 FORMAT(5(68A1/))
  801 FORMAT(8A10)
  802 FORMAT(I10,7A10)
  803 FORMAT(F10.3,7A10)
  805 FORMAT(I10,8X,'CONTROL OPTION (1=FAN, 2=DAMPER)')
  806 FORMAT(/'REFRIGERANT CODES (1 - 34):'/
     .  '      1 = R11,       2 = R12,       3 = R13,       4 = n-C5,'/
     .  '      5 = R14,       6 = R22,       7 = R23,       8 = R113,'/
     .  '      9 = R114,     10 = R142b,    11 = R152a,    12 = R216a,'/
     .  '     13 = R125,     14 = R143a,    15 = R134a,    16 = R123,'/
     .  '     17 = RC318,    18 = R134,     19 = RC270,    20 = R141b'/
     .  '     21 = i-C5,     22 = R290,     23 = R600,     24 = R600a,'/
     .  '     25 = R32,      26 = R1270,    27 = R124,     28 = R115,'/
     .  '     29 = CE216     30 = E125,     31 = R123a,    32 = R143,'/
     .  '     33 = R218,     34 = E134')
  807 FORMAT('FILE NAME: ',A13/)
      END
C
      SUBROUTINE CYCIO(LOC,IPASS,VAR,LINE)
C     ******************************************************************
C     *    READ AND WRITE DATA FOR CYCDAT.FOR SUBROUTINE               *
C     ******************************************************************
C
      CHARACTER*10 LINE
      DIMENSION LINE(8)
      COMMON /skipwrit/ iwrite
      DATA ICYCL /4/
C
C          BRANCH ON LOC
C
      IF (iwrite .EQ. 0) RETURN
 
      SELECT CASE (LOC)
           CASE (1)
                READ (ICYCL,801) LINE
                WRITE(IPASS,801) LINE
           CASE (2)
                READ (ICYCL,801) LINE
                WRITE(IPASS,802) NFIX(VAR), (LINE(I), I = 2,8)
           CASE (3)
                READ (ICYCL,801) LINE
                WRITE(IPASS,803) VAR, (LINE(I), I = 2,8)
           CASE (4)
                NUMRED = IPASS
                DO WHILE (NUMRED .NE. 0)
                     READ(ICYCL,801) LINE
                     NUMRED = NUMRED - 1
                END DO
      END SELECT
      RETURN
C
C          FORMATS
C
  801 FORMAT(8A10)
  802 FORMAT(I10,7A10)
  803 FORMAT(F10.3,7A10)
      END
