      SUBROUTINE CHKCAB(LOC, VAL, IFLAG)
C     ******************************************************************
C     *    CHECK CABINET DATA FOR INTERNAL CONSISTENCY                 *
C     *    CALLED FROM CYCDAT TO ENSURE CYCLE CONSISTENT WITH CABINET  *
C     ******************************************************************
C
      CHARACTER      key
      CHARACTER*13    filmap1, filmap2
 
      COMMON /MAPNAM/ filmap1, filmap2
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore, iold_data,
     .                icompr
 
      DIMENSION      VAL(1)
C
C         INITIALIZE
C
      irftyp = nfix(VAL(1))
      icycl  = nfix(VAL(170))
      iflag = 0
      icomp = 0
 
      VAL02 = VAL(02) - 0.2 * VAL(786) - 0.2 * VAL(789) ! liner in mm
      VAL03 = VAL(03) - 0.2 * VAL(786) - 0.2 * VAL(789) ! width in cm
      VAL04 = VAL(04) - 0.2 * VAL(786) - 0.2 * VAL(789)
      T_1L = 0.1 * (VAL(786) + VAL(789))
 
      GO TO (20, 10, 30, 40), loc
C
C          CHECK CONSISTENCY BETWEEN SYSTEM TYPE AND CABINET DESIGN
C
   10 CONTINUE
   !! check on dual loop -- do not allow a natural convection condenser
      INAT = VAL(246) + VAL(266)
      IF (ICYCL .EQ. 3 .AND. INAT .GT. 2) THEN
         iflag = 1
         CALL error (2)
         CALL gotoxy(20, 11)
         CALL print('Natural Convection Condenser Not Allowed ',41,-2)
         CALL gotoxy(20, 12)
         CALL print('in Dual Loop Systems          ',30,-2)
 
         CALL inchr(1,j,key)
         RETURN
      END IF
 
 
      SELECT CASE (irftyp)
        CASE (1, 2, 3, 7)
          !All systems are ok
 
        CASE (4, 5, 6)                                     !Freezers or
          IF(icycl .NE. 1) THEN                            !  refrigerator
            iflag = 1
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
            RETURN
          END IF
      END SELECT
C
C          SINGLE-DOOR REFRIGERATOR MUST USE NATURAL CONVECTION EVAPORATOR
C
 !    IF (IRFTYP .EQ. 6 .AND. NFIX(VAL(209)) .NE. 2) THEN
 !       iflag = 1
 !       CALL error (2)
 !       CALL gotoxy(20, 11)
 !       CALL print('A Natural Convection Evaporator Must be ',40,-2)
 !       CALL gotoxy(20, 12)
 !       CALL print('Used with Single-Door Refrigerator.',35,-2)
 
 !       CALL inchr(1,j,key)
 !       RETURN
 !    END IF
 
      IF (IRFTYP .EQ. 6) VAL(209) = 2.0
C
C          COVERAGE OF WALLS BY NATURAL CONVECTION EVAPORATOR
C
      err_top = 0
      err_back = 0
      err_side = 0
      err_mul  = 0
      err_botm = 0
 
      SELECT CASE (irftyp)
        CASE (1, 2, 3, 7)                                 !Two cabinets
          iloc_fz = nfix(VAL(362))
          IF(icycl .EQ. 1) iloc_fz = 0
          iloc_ff = nfix(VAL(372))
          IF(nfix(VAL(209)) .EQ. 1) iloc_ff = 0
          IF(nfix(VAL(229)) .EQ. 1) iloc_fz = 0
          IF((iloc_fz .NE. 0 .OR. iloc_ff .NE. 0)
     .                       .AND. icycl .EQ. 1 )THEN
            iflag = 1
            CALL error (3)
            CALL gotoxy(20, 11)
            CALL print('An In-the-Wall Evaporator is Not Modeled',40,-2)
            CALL gotoxy(20, 12)
            CALL print('for a Refrigerator/Freezer with a Single',40,-2)
            CALL gotoxy(20, 13)
            CALL print('Evaporator Cycle.',17,-2)
 
            CALL inchr(1,j,key)
            RETURN
          END IF
 
          IF(iloc_fz .NE. 0  .AND. icycl .EQ. 3) THEN
            iflag = 1
            CALL error (2)
            CALL gotoxy(20, 11)
            CALL print('An In-the-Wall Freezer Evaporator is Not',40,-2)
            CALL gotoxy(20, 12)
            CALL print('Modeled in a Dual-Loop Cycle',28,-2)
 
            CALL inchr(1,j,key)
            RETURN
          END IF
 
        CASE(4)                                         !Chest Freezer
          iloc_fz = nfix(VAL(372))
          IF(nfix(VAL(209)) .EQ. 1) iloc_fz = 0
 
        CASE(5)                                         !Upright Freezer
          iloc_fz = nfix(VAL(362))
          IF(nfix(VAL(209)) .EQ. 1) iloc_fz = 0
 
        CASE(6)
          areafz = 2.0 * (val(155) * val(156) + val(155) * val(157)
     .                                        + val(156) * val(157))
          if (areafz/10000.0 .LT. val(775)) then
            iflag = 1
            CALL error (2)
            CALL gotoxy(21, 11)
            CALL print('The Freezer Surface Area is Less Than',37,-2)
            CALL gotoxy(21, 12)
            CALL print('the Freezer Evaporator Area.',28,-2)
 
            CALL inchr(1,j,key)
            RETURN
          END IF
 
 
      END SELECT
 
      SELECT CASE (irftyp)
        CASE (1, 2, 3, 7)                                  !Two-cabinets
          IF(iloc_fz .NE. 0) THEN
            area_evap = VAL(416)
            aside = 10000.0*VAL(367)*area_evap
            aback = 10000.0*VAL(368)*area_evap
            atop  = 10000.0*VAL(369)*area_evap
            amul  = 10000.0*VAL(370)*area_evap
 
            IF(irftyp .EQ. 1 .or. irftyp .eq. 7) THEN
              top = (VAL03-2.0*VAL(42))*(VAL04-VAL(44)-VAL(43))
              side = 2.0*(VAL(31)-VAL(41)-T_1L)*(VAL04-VAL(44)-VAL(43))
              back = (VAL(31)-VAL(41)-T_1L)*(VAL03-2.0*VAL(42))
              dmul = top
 
            ELSE IF(irftyp .EQ. 2) THEN
              top = (VAL03-2.0*VAL(42))*(VAL04-VAL(45)-VAL(43))
              side = 2.0*(VAL(02)-VAL(31)-VAL(44)-VAL(32)-T_1L)
     .                  *(VAL04-VAL(45)-VAL(43))
              back = (VAL(02)-VAL(31)-VAL(32)-VAL(44)-T_1L)
     .                  *(VAL03-2.0*VAL(42))
              dmul = top
 
            ELSE
              top = (VAL(03)-VAL(31)-VAL(32)-VAL(42)-T_1L)
     .              *(VAL04-VAL(45)-VAL(43))
              side = (VAL02-VAL(41)-VAL(44))
     .               *(VAL04-VAL(45)-VAL(43))
              back = (VAL(03)-VAL(31)-VAL(32)-VAL(42)-T_1L)
     .               *(VAL02-VAL(41)-VAL(44))
              dmul = side
            END IF
 
            err_top = (atop - top)/top
            err_back = (aback-back)/back
            err_side = (aside-side)/side
            err_mul  = (amul-dmul)/dmul
          END IF
 
        CASE (4)                                           !Chest freezer
          IF(iloc_fz .NE. 0) THEN
            area_evap = VAL(412)
            aside = 10000.0*VAL(377)*area_evap
            abotm = 10000.0*VAL(378)*area_evap
            side = 2.0*(VAL02-VAL(11)-VAL(13))*
     .                 (VAL03+VAL04-4.0*VAL(12))
     .           - 2.0*(VAL(36)+VAL(15))*(VAL(35)+VAL(14))
     .           - (VAL(36)+VAL(15))*(VAL04-2.0*VAL(12))
            botm = (VAL04 - 2.0*VAL(12))*
     .                 (VAL03 - VAL(12) - VAL(35)- VAL(14))
            err_side = (aside-side)/side
            err_botm = (abotm-botm)/botm
          END IF
 
        CASE (5)                                           !Upright freezer
          IF(iloc_fz .NE. 0) THEN
            area_evap = VAL(412)
            aside = 10000.0*VAL(367)*area_evap
            aback = 10000.0*VAL(368)*area_evap
            atop  = 10000.0*VAL(369)*area_evap
            abotm = 10000.0*VAL(370)*area_evap
 
            side = 2.0*(VAL04 - VAL(45) - VAL(43))*
     .                 (VAL02 - VAL(41) - VAL(44))
            botm = (VAL04 - VAL(45) - VAL(43))*
     .                 (VAL03 - VAL(42) - VAL(42))
            top = botm
            back = (VAL02 - VAL(41) - VAL(44))*
     .                 (VAL03 - VAL(42) - VAL(42))
 
            err_top  = (atop -top)/top
            err_side = (aside-side)/side
            err_back = (aback-back)/back
            err_botm = (abotm-botm)/botm
          END IF
 
      END SELECT
 
      IF(err_top .GT. 0.01 .OR. err_back .GT. 0.01
     .                     .OR. err_side .GT. 0.01
     .                     .OR. err_botm .GT. 0.01
     .                     .OR. err_mul  .GT. 0.01) THEN
 
        iflag = 1
        CALL error (2)
        CALL gotoxy(20, 11)
        CALL print('The In-the-Wall Evaporator is Larger Than',41,-2)
        CALL gotoxy(20, 12)
        CALL print('the Freezer Inside Dimensions.',30,-2)
 
        CALL inchr(1,j,key)
        RETURN
      END IF
C
C          OPTION 5 DUAL-EVAPORATOR CONTROL RESTRICTIONS
C
      IF(icycl .EQ. 2 .OR. icycl .EQ. 4) THEN
        IF(VAL(405) .EQ. 5) THEN
          SELECT CASE (irftyp)
            CASE (1:3, 7)                                  !R/F
              IF(VAL(113) .EQ. 0) iflag = 1
 
              IF(VAL(715) .NE. 0 .OR. VAL(716) .NE. 0.0) iflag = 2
 
            CASE (4, 5)                                    !Freezer
               IF(VAL(117) .EQ. 0) iflag = 1
               IF(VAL(760) .NE. 0) iflag = 2
 
          END SELECT
 
          IF(iflag .NE. 0) THEN
            CALL error(3)
            SELECT CASE (iflag)
              CASE (1)                                     !Defrost
                CALL gotoxy(22, 11)
                CALL print('Automatic Defrost is Not Modeled for',36,-2)
                CALL gotoxy(22, 12)
                CALL print('Dual Evaporator Cycle Control Option 5.',
     .                                                            39,-2)
                CALL gotoxy(26, 13)
                CALL print('(See DOOR OPENING SCHEDULES)',28,-2)
 
              CASE (2)                                     !Run-time energy
                CALL gotoxy(20, 11)
                CALL print('Compressor Run-Time Dependent Heat Input',
     .                                                           40,-2)
                CALL gotoxy(20, 12)
                CALL print('Not Modeled for Cycle Control Option 5.',
     .                                                           39,-2)
                CALL gotoxy(24, 13)
                CALL print('(See DEFROST AND CONTROL ENERGY)',32,-2)
 
            END SELECT
 
            CALL inchr(1,j,key)
            iflag = 1
 
          END IF
 
          RETURN
        END IF
      END IF
C
C          COVERAGE OF WALLS BY NATURAL CONVECTION CONDENSER
C
      IF(nfix(VAL(246)) .EQ. 2 .AND. nfix(VAL(585)) .EQ. 1) THEN
        IF(irftyp .EQ. 3) iflag = 1
        IF(icycl  .EQ. 3) iflag = 2
        IF(iflag .NE. 0) THEN
          CALL error (2)
          IF(iflag .EQ. 1) THEN
            CALL gotoxy(20, 11)
            CALL print('An In-the-Wall Condenser is not Modeled',39,-2)
            CALL gotoxy(20, 12)
            CALL print('for a Side-by-Side Cabinet.',27,-2)
          ELSE
            CALL gotoxy(20, 11)
            CALL print('An In-the-Wall Condenser is not Modeled',39,-2)
            CALL gotoxy(20, 12)
            CALL print('for Dual-Loop Cycle.',20,-2)
            iflag = 1
          END IF
 
          CALL inchr(1,j,key)
          RETURN
        END IF
 
        err_top = 0
        err_back = 0
        err_side = 0
 
        area_cond = VAL(512)
        aside = 10000.0*VAL(589)*area_cond
        aback = 10000.0*VAL(590)*area_cond
        atop  = 10000.0*VAL(591)*area_cond
 
        SELECT CASE (irftyp)
          CASE (1, 7)                                      !Top mount
            top = VAL(03)*(VAL(04)-VAL(06)-VAL(07))
            side = 2.0*VAL(02)*(VAL(04)-VAL(06)-VAL(07))
            back = VAL(02)*VAL(03)
 
            err_top = (atop - top)/top
            err_back = (aback-back)/back
 
          CASE (2, 5, 6)                                   !Bottom and Freezer
            top = VAL(03)*(VAL(04)-VAL(06)-VAL(07))
            side = 2.0*VAL(02)*(VAL(04)-VAL(06)-VAL(07))
            back = VAL(02)*VAL(03)
 
            err_top = (atop - top)/top
            err_side= (aside - side)/side
            err_back = (aback-back)/back
 
          CASE (4)                                         !Chest freezer
            side = 2.0*(VAL(02)-VAL(06)-VAL(07))*(VAL(03)+VAL(04))
     .           - 2.0*VAL(36)*VAL(35) - VAL(36)*VAL(04)
 
        END SELECT
 
        err_side = (aside-side)/side
 
 
        IF(err_top .GT. 0.01 .OR. err_back .GT. 0.01
     .                       .OR. err_side .GT. 0.01) THEN
          iflag = 1
          CALL error (2)
          CALL gotoxy(20, 11)
          CALL print('The In-the-Wall Condenser is Larger Than',40,-2)
          CALL gotoxy(20, 12)
          CALL print('the Cabinet Outside Dimensions.',31,-2)
 
          CALL inchr(1,j,key)
          RETURN
        END IF
 
      END IF
 
      RETURN
C
C          SET UP VALUES FOR THE DISTRIBUTION OF AN IN-WALL CONDENSER
C          BETWEEN THE TWO CABINETS
C
   20 CONTINUE
      IF(nfix(VAL(585)) .EQ. 0) RETURN
      SELECT CASE (irftyp)
        CASE (1, 7)
          fraction_fz = VAL(31)/VAL(02)
          VAL(592) = (1.0 - fraction_fz)*VAL(589)
          VAL(593) = (1.0 - fraction_fz)*VAL(590)
          VAL(594) = fraction_fz*VAL(589)
          VAL(595) = fraction_fz*VAL(590)
          VAL(596) = 0
          VAL(597) = VAL(591)
 
        CASE (2)
          fraction_ff = VAL(31)/VAL(02)
          VAL(592) = fraction_ff*VAL(589)
          VAL(593) = fraction_ff*VAL(590)
          VAL(594) = (1.0 - fraction_ff)*VAL(589)
          VAL(595) = (1.0 - fraction_ff)*VAL(590)
          VAL(596) = VAL(591)
          VAL(597) = 0
 
        CASE (4)
          VAL(592) = VAL(589)
          VAL(593) = 0
          VAL(596) = 0
 
        CASE (5, 6)
          VAL(592) = VAL(589)
          VAL(593) = VAL(590)
          VAL(596) = VAL(591)
 
      END SELECT
 
      RETURN
C
C          CHECK FLANGE WIDTH FOR CABINETS
C
   30 CONTINUE
 
      flange = VAL(18)
      WEDGE_FF  = VAL(625)
      flange_fz = VAL(623)
      flange_ff = VAL(626)
 
      SELECT CASE (irftyp)
        CASE (1, 7)
          thick_fz = AMIN1(VAL(41), VAL(42))
          thick_FF = AMIN1(VAL(52), VAL(54))
          IF(flange_fz .GE. thick_fz) iflag = 1
          IF(flange_ff .GE. thick_ff .AND. WEDGE_FF .NE. 0.0) iflag = 1
 
        CASE (2)
          thick_fz = AMIN1(VAL(42), VAL(44))
          thick_FF = AMIN1(VAL(51), VAL(52))
          IF(flange_fz .GE. thick_fz) iflag = 1
          IF(flange_ff .GE. thick_ff .AND. WEDGE_FF .NE. 0.0) iflag = 1
 
        CASE (3)
          thick_fz = AMIN1(VAL(41), VAL(42), VAL(44))
          thick_FF = AMIN1(VAL(52), VAL(54))
          IF(flange_fz .GE. thick_fz) iflag = 1
          IF(flange_ff .GE. thick_ff .AND. WEDGE_FF .NE. 0.0) iflag = 1
 
        CASE (4)
          !No flange on chest freezer
 
        CASE (5, 6)
          thick = AMIN1(VAL(41), VAL(42), VAL(44))
          IF(flange .GE. thick) iflag = 1
 
      END SELECT
 
      IF(iflag .NE. 0) THEN
        iflag = 1
        CALL error (2)
        CALL gotoxy(20, 11)
        CALL print('The Cabinet Flange Must be Less than the',40,-2)
        CALL gotoxy(20, 12)
        CALL print('Minimum Thickness of the Insulation.',36,-2)
 
        CALL inchr(1,j,key)
        RETURN
      END IF
C
C          CHECK FREEZER WEDGE FOR CABINETS
C
      SELECT CASE (IRFTYP)
         CASE (1, 2, 3, 7)
            WEDGE_FZ = VAL(622)
         CASE (5, 6)
            WEDGE_FZ = VAL(17)
      END SELECT
      IF (WEDGE_FZ .EQ. 0.0 .AND. IRFTYP .NE. 4
     .                      .and. irftyp .ne. 6) THEN
        IFLAG = 1
        CALL error (1)
        CALL gotoxy(19, 11)
        CALL PRINT('Freezer Wedge Should not Have Zero Depth', 40,-2)
        CALL inchr(1,j,key)
        RETURN
      END IF
      IF (IRFTYP .NE. 6) RETURN
C
C          CHECK FREEZER COMPARTMENT DIMENSIONS FOR A SINGLE-DOOR REFRIGERATOR
C
      IF (VAL03 - 2.0 * VAL(42) .LT. VAL(156)) IFLAG = 1
      IF (VAL04 - VAL(43) - VAL(45) . LE. VAL(157)) IFLAG = 2
      IF (VAL02 - VAL(41) - VAL(44) .LE. 3.0 * VAL(155)) IFLAG = 3
      IF (IFLAG .NE. 0) THEN
        CALL error (1)
        CALL gotoxy(23, 11)
        SELECT CASE (IFLAG)
           CASE (1)
              CALL PRINT('The Freezer Compartment is too Wide', 35, -2)
 
           CASE (2)
              CALL PRINT('The Freezer Compartment is too Deep', 35, -2)
 
           CASE (3)
              CALL PRINT('The Freezer Compartment is too High', 35, -2)
        END SELECT
 
        IFLAG = 1
        CALL inchr(1,j,key)
      END IF
      RETURN
C
C          CHECK SELECTION OF COMPRESSOR MAP FILES
C
   40 CONTINUE
      IF(nfix(VAL(458)) .NE. 0) RETURN
      OPEN(1, FILE='COMPMAP.NAM', STATUS='UNKNOWN')
      IF(icycl .NE. 3) THEN
         IF(filmap1 .EQ. '             ') THEN
            iflag = 1
 
            IF (icompr .EQ. 0) THEN
              CALL error (1)
              CALL gotoxy(21, 11)
              CALL print('A Compressor Map File Must be Selected',38,-2)
 
              CALL inchr(1,j,key)
              RETURN
            END IF
 
            icomp = 0
            iflag = 0
         ELSE
            WRITE(1, '(A13)') filmap1
            icomp = 1
         END IF
 
      ELSE
         IF(filmap1 .EQ. '             ') THEN
            iflag = 1
            CALL error (2)
            CALL gotoxy(21, 11)
            CALL print('A Compressor Map File Must be Selected',38,-2)
            CALL gotoxy(21, 12)
            CALL print('for the Fresh Food Section',26,-2)
 
            CALL inchr(1,j,key)
            RETURN
         ELSE
            WRITE(1, '(A13)') filmap1
            icomp = 1
         END IF
 
         IF(filmap2 .EQ. '             ') THEN
            iflag = 1
            CALL error (2)
            CALL gotoxy(21, 11)
            CALL print('A Compressor Map File Must be Selected',38,-2)
            CALL gotoxy(21, 12)
            CALL print('for the Freezer Section',23,-2)
 
            CALL inchr(1,j,key)
            RETURN
         ELSE
            WRITE(1, '(A13)') filmap2
            icomp = 2
         END IF
 
      END IF
 
      CLOSE (1)
 
      IF (icomp .EQ. 1) THEN
         CALL DOSFUN(1,filmap1,'COMPMAP.DAT ',DOSCHR)
         CALL DOSCAL(.FALSE., .TRUE., .FALSE.,.FALSE.,DOSCHR)
      ENDIF
 
      IF (icomp .EQ. 2) THEN
         CALL DOSFUN(1,filmap2,'COMPMAP.DAT ',DOSCHR)
         CALL DOSCAL(.FALSE., .TRUE., .FALSE.,.FALSE.,DOSCHR)
      END IF
 
      RETURN
 
      END
