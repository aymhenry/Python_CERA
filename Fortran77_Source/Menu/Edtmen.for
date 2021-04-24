      SUBROUTINE EDTMEN(IMEN)
C     ******************************************************************
C     *              CABINET DAT EDIT MENU DISPLAY                     *
C     ******************************************************************
C
      LOGICAL first_call, getting_command
      CHARACTER key
 
      DIMENSION IROW(13), ICOL(13), IEND(13)
      DIMENSION LR(13), UP(13), DN(13)
 
      COMMON /CHANGE/ isav, ilpt1, numsel, ibypas, numpat, ibuf, ifile
      COMMON /VALUES/ VAL(800), VALMN(800), VALMX(800)
 
      DATA IROW/10, 11, 12, 13, 14, 15, 10, 11, 12, 13, 14, 15, 16/
      DATA ICOL/07, 07, 07, 07, 07, 07, 43, 43, 43, 43, 43, 43, 43/
      DATA IEND/36, 36, 36, 36, 36, 36, 72, 72, 72, 72, 72, 72, 72/
 
      DATA UP /13, 1, 2, 3, 4, 5, 6, 7, 8, 9 ,10, 11, 12/
      DATA DN /2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1/
      DATA LR /7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 6/
 
      DATA icold, irold, ieold /05, 10, 34/
 
C
C          DISPALY THE MENU STRUCTURE
C
      irftyp = nfix(val(1))
 
      CALL cursor(1)
      CALL display(4, 4, 22, 3, 76)
      CALL gotoxy(28, 3)
      CALL atrbut(24, 79)
      CALL gotoxy(28, 3)
      CALL print(' Cabinet Data Edit Menu $',24,-2)
C
C          DISPLAY CABINET TYPE TO HELPINPUT DATA
C
      IRFTYP = VAL(1)
      SELECT CASE (IRFTYP)
         CASE (1)
           CALL gotoxy(21, 7)
           CALL print('Two-Door Top-Mount Refrigerator/Frezer', 38, -2)
 
         CASE (2)
           CALL gotoxy(19, 7)
           CALL print('Two-Door Bottom-Mount Refrigerator/Frezer',
     .                                                           41, -2)
 
         CASE (3)
           CALL gotoxy(24, 7)
           CALL print('Side-by-Side Refrigerator/Frezer', 32, -2)
 
         CASE (4)
           CALL gotoxy(34, 7)
           CALL print('Chest Frezer', 12, -2)
 
         CASE (5)
           CALL gotoxy(33, 7)
           CALL print('Upright Frezer', 14, -2)
 
         CASE (6)
           CALL gotoxy(29, 7)
           CALL print('One-Door Refrigerator', 21, -2)
 
         CASE (7)
           CALL gotoxy(25, 7)
           CALL print('One-Door Refrigerator/Freezer', 29, -2)
 
      END SELECT
      CALL gotoxy(19, 7)
      CALL atrbut(41, 28)
C
C          OUTPUT MENU CHOICES
C
      CALL gotoxy(08, 10)
      CALL print('Title of Analysis$',17,-2)
      CALL gotoxy(44, 10)
      CALL print('Air and Cabinet Temperatures$',28,-2)
 
      CALL gotoxy(08, 11)
      CALL print('Cabinet Type and Dimensions$',27,-2)
      CALL gotoxy(44, 11)
      CALL print('Door Opening Schedules$',22,-2)
 
      CALL gotoxy(08, 12)
      CALL print('Refrigerated Volumes$',20,-2)
      CALL gotoxy(44, 12)
      CALL print('Gasket Heat Leaks$',17,-2)
 
      CALL gotoxy(08, 13)
      IF (irftyp .EQ. 6) THEN
         CALL print('Refrigerator Compartment$',24,-2)
      ELSE
         CALL print('Freezer Compartment$',19,-2)
      END IF
      CALL gotoxy(44, 13)
      IF (irftyp .NE. 6) THEN
         CALL print('Defrost and Controls Energy$',27,-2)
      ELSE
         CALL print('Controls Energy$',16,-2)
      END IF
 
      CALL gotoxy(08, 14)
      IF(irftyp .LE. 3 .or. irftyp .eq. 7) THEN
         LR(11) = 5
         DN(4) = 5
         UP(6) = 5
         CALL print('Fresh Food Compartment$',22,-2)
      ELSE
         LR(11) = 4
         DN(4) = 7
         IF (irftyp .EQ. 6) THEN
            CALL print('[Freezer Not Insulated]$',23,-2)
         ELSE
            CALL print('[No Fresh Food Section]$',23,-2)
         END IF
      END IF
      CALL gotoxy(44, 14)
      CALL print('Electric Anti-Sweat Heat$',28,-2)
 
      CALL gotoxy(08, 15)
      IF(irftyp .LE. 3 .or. irftyp .eq. 7) THEN
         DN(5) = 6
         UP(7) = 6
         LR(12) = 6
         LR(13) = 6
 
         CALL print('Mullion$',07,-2)
      ELSE
         DN(5) = 7
         UP(7) = 4
         LR(12) = 4
         LR(13) = 4
         CALL print('[No Mullion]$',12,-2)
      END IF
      CALL gotoxy(44, 15)
      CALL print('Refrigerant Line Anti-Sweat $',28,-2)
 
      CALL gotoxy(44, 16)
      CALL print('Penetration Heat Input  $',24,-2)
 
      CALL line (4, 75, 19)
 
      nummax = 13
      IF(numsel .EQ. 14) numsel = 13
 
      IF(IBYPAS .EQ. -1) THEN
         j = 72
      ELSE
         j = 80
      END IF
 
      first_call = .TRUE.
      getting_command = .TRUE.
C
C          LOOK FOR KEYPRESS
C
      DO WHILE (getting_command)
         IF (.NOT. first_call) CALL inchr(1, j, key)
 
         SELECT CASE (j)
            CASE (1, 73)                                   !<Esc> of <PgUp>
               numsel = 14
               getting_command = .FALSE.
               CYCLE
 
            CASE (28, 81)                                  !<Enter> or <PgDn>
               getting_command = .FALSE.
               CYCLE
 
            CASE (59)                                      !<F1>
               CALL help(5)
               CALL cursor(1)
               CYCLE
 
            CASE (67)                                      !<F9>
               CALL help(6)
               CALL cursor(1)
               CYCLE
 
            CASE (71)                                      !<Home>
               newsel = 1
 
            CASE (79)                                      !<End>
               newsel = 13
 
            CASE (75, 77)                                  !Left/Right cursor
               newsel = LR(numsel)
 
            CASE (72)                                      !Up cursor
               newsel = UP(numsel)
               first_call = .FALSE.
 
            CASE (80)                                      !Dn cursor
               newsel = DN(numsel)
               first_call = .FALSE.
 
            CASE DEFAULT
               CALL warble
               CYCLE
 
         END SELECT
C
C          SELECTION MADE.  MOVE THE MARKER.
C
         IF(newsel .EQ. numsel) cycle
 
         CALL gotoxy(icold, irold)
         CALL light(ieold, 6)
 
         icold = ICOL(newsel)
         irold = IROW(newsel)
         ieold = IEND(newsel)
 
         CALL gotoxy(icold, irold)
         CALL light(ieold, 3)
         numsel = newsel
 
         CALL window(20, 21, 6, 73, 32, 0)
 
         CALL gotoxy(6,20)
 
         SELECT CASE (NUMSEL)
            CASE (1)                                       !Title
               CALL PRINT('Edit the run title.  A full screen $',35,-2)
               CALL PRINT('text editor is built into ERA.  $',32,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('The run title will appear on  $',30,-2)
               CALL PRINT('subsequent output files.  $',26,-2)
 
            CASE (2)                                       !Cabinet type
               CALL PRINT('Define the type of cabinet and $',31,-2)
               CALL PRINT('the overall dimensions.  $',25,-2)
 
            CASE (3)                                       !Volumes
               CALL PRINT('Input values for the internal $',30,-2)
               CALL PRINT('volumes.  ERA also calculates the$',33,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('food storage volume to provide $',31,-2)
               CALL PRINT('a check of the specified dimensions.$',36,-2)
 
            CASE (4)                                       !Freezer
               IF (irftyp .NE. 6) THEN
                  CALL PRINT('Input dimensions and resistivities of',
     .                                                          37,-2)
                  CALL PRINT(' walls and door of the freezer.$',31,-2)
                  CALL GOTOXY(6,21)
                  CALL PRINT('Advanced insulation panel $',26,-2)
                  CALL PRINT('construction may be specified.$',30,-2)
               ELSE
                  CALL PRINT('Input dimensions and resistivities of',
     .                                                          37,-2)
                  CALL PRINT(' walls and door of the refri-$', 29, -2)
                  CALL GOTOXY(6,21)
                  CALL PRINT('gerator.  Advanced insulation panel $',
     .                                                          36,-2)
                  CALL PRINT('construction may be specified.$',30,-2)
               END IF
 
            CASE (5)                                       !Fresh Food
               CALL PRINT('Input dimensions and resistivities of',37,-2)
               CALL PRINT(' walls and door of fresh food$',29,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('section.  Advanced insulation $',30,-2)
               CALL PRINT('panel construction may be specified.$',36,-2)
 
            CASE (6)                                       !Mullion
               CALL PRINT('The mullion separates the fresh food$',36,-2)
               CALL PRINT(' and freezer sections.  Define $',31,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('the dimensions and resistivity $',31,-2)
               CALL PRINT('of the mullion.$',15,-2)
 
            CASE (7)                                       !Air/cabinet temps
               CALL PRINT('Define the temperatures of the room,$',36,-2)
               CALL PRINT(' the air entering the condenser, $',33,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('underneath the cabinet and the $',31,-2)
               CALL PRINT('control settings.$',16,-2)
 
            CASE (8)                                       !Door openings
               CALL PRINT('Door openings affect the heat $',30,-2)
               CALL PRINT('loads in the cabinet.  Moisture $',32,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('transport will result in defrost $',33,-2)
               CALL PRINT('loads.  Enter the schedule here.$',32,-2)
 
            CASE (9)                                       !Gasket loads
               CALL PRINT('Gasket heat leaks are dependent on $',35,-2)
               CALL PRINT('the gasket width and door $',26,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('perimeter. $',35,-2)
 
            CASE (10)                                      !Defrost
               IF (irftyp .NE. 6) THEN
                  CALL PRINT('Defrost and controls energy use may$',
     .                                                          35,-2)
                  CALL PRINT(' be defined here as constant or$',31,-2)
                  CALL GOTOXY(6,21)
                  CALL PRINT('compressor run time dependent.   $',33,-2)
               ELSE
                  CALL PRINT('Controls energy use may be defined $',
     .                                                          35, -2)
                  CALL PRINT('here as constant or or compressor$',33,-2)
                  CALL GOTOXY(6,21)
                  CALL PRINT('run time dependent.   $',23,-2)
 
               END IF
 
            CASE (11)                                      !Electric Anti-sweat
               CALL PRINT('Electric heaters may be mounted$',35,-2)
               CALL PRINT(' under the cabinet flanges and $',31,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('mullion for moisture control.$',29,-2)
 
            CASE (12)                                      !Refrigerant lines
               CALL PRINT('Vapor or liquid refrigerant lines$',36,-2)
               CALL PRINT(' may be used for cabinet flange$',31,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('moisture control.$',29,-2)
 
            CASE (13)                                      !Penetrations
               CALL PRINT('Penetrations may account for heat$',33,-2)
               CALL PRINT(' inputs to the cabinet.  For exam-$',34,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('ple a through-the-door ice service $',35,-2)
               CALL PRINT('can be represented this way.$',28,-2)
 
        END SELECT
 
      END DO
 
      imen = numsel
      CALL cursor(0)
      ibypas = 0
 
      RETURN
      END
