      SUBROUTINE EDTCYC(IMEN)
C     ******************************************************************
C     *               CYCLE DATA EDIT MENU DISPLAY                     *
C     ******************************************************************
C
      LOGICAL         first_call, getting_command
      CHARACTER       key
 
      DIMENSION       IROW(6), ICOL(6), IEND(6)
      DIMENSION       LR(6), UP(6), DN(6)
 
      COMMON /CHANGE/ isav, ilpt1, numsel, ibypas, numpat, ibuf, ifile
      COMMON /VALUES/ VAL(800), VALMN(800), VALMX(800)
 
      DATA            IROW/11, 12, 13, 11, 12, 13/
      DATA            ICOL/14, 14, 14, 43, 43, 43/
      DATA            IEND/32, 32, 32, 61, 61, 61/
 
      DATA            UP /6, 1, 2, 3, 4, 5/
      DATA            DN /2, 3, 4, 5, 6, 1/
      DATA            LR /4, 5, 6, 1, 2, 3/
 
      DATA            icold, irold, ieold /13, 11, 28/
 
C
C          DISPALY THE MENU STRUCTURE
C
      CALL cursor(1)
      CALL display(4, 4, 22, 3, 76)
      CALL gotoxy(29, 3)
      CALL atrbut(22, 79)
      CALL gotoxy(29, 3)
      CALL print(' Cycle Data Edit Menu $',22,-2)
C
C          DISPLAY CABINET TYPE TO REDUCE CYCLE INPUT ERRORS
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
C          DISPLAY CYCLE TYPE
C
      ICYC = VAL(170)
      SELECT CASE (ICYC)
         CASE (1)
           CALL gotoxy(28, 8)
           CALL print('Single Evaporator Cycle', 23, -2)
 
         CASE (2)
           CALL gotoxy(34, 8)
           CALL print('Lorenz Cycle', 12, -2)
 
         CASE (3)
           CALL gotoxy(32, 8)
           CALL print('Dual Loop Cycle', 15, -2)
 
         CASE (4)
           CALL gotoxy(29, 8)
           CALL print('Dual Evaporator Cycle', 21, -2)
      END SELECT
      CALL gotoxy(28, 8)
      CALL atrbut(23, 28)
C
C          OUTPUT MENU CHOICES
C
      CALL gotoxy(15, 11)
      CALL print('Cycle Type$',10,-2)
      CALL gotoxy(44, 11)
      CALL print('Condenser Design $',17,-2)
 
      CALL gotoxy(15, 12)
      CALL print('Refrigerants$',12,-2)
      CALL gotoxy(44, 12)
      CALL print('Compressor Design$',17,-2)
 
      CALL gotoxy(15, 13)
      CALL print('Evaporator Design$',17,-2)
      CALL gotoxy(44, 13)
      CALL print('Interchanger(s)  $',17,-2)
 
      CALL line (4, 75, 19)
 
      nummax = 6
      IF(numsel .GT. 6) numsel = 6
 
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
               numsel = 7
               getting_command = .FALSE.
               CYCLE
 
            CASE (28, 81)                                  !<Enter> or <PgDn>
               getting_command = .FALSE.
               CYCLE
 
            CASE (59)                                      !<F1>
               CALL help(8)
               CALL cursor(1)
               CYCLE
 
            CASE (67)                                      !<F9>
               CALL help(6)
               CALL cursor(1)
               CYCLE
 
            CASE (71)                                      !<Home>
               newsel = 1
 
            CASE (79)                                      !<End>
               newsel = 6
 
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
            CASE (1)                                       !Cycle type
               CALL PRINT('Select the type of refrigeration $',33,-2)
               CALL PRINT('cycle.  The remaining menus are $',32,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('determined by the cycle choice.$',31,-2)
 
            CASE (2)                                       !Refrigerants
               CALL PRINT('Select the refrigerant composition. $',36,-2)
               CALL PRINT('Pure fluids or blends may$',25,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('be assumed.  Each refrigerant$',29,-2)
               CALL PRINT(' is assigned an identifying code.$',33,-2)
 
            CASE (3)                                       !Evaporator
               CALL PRINT('Define the type and design of the $',34,-2)
               CALL PRINT('evaporator(s).  Calculate heat $',31,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('exchanger properties and pressure$',33,-2)
               CALL PRINT(' drops.$',07,-2)
 
            CASE (4)                                       !Condenser
               CALL PRINT('Define the type and design of the $',34,-2)
               CALL PRINT('condenser(s).  Calculate heat $',30,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('exchanger properties and pressure$',33,-2)
               CALL PRINT(' drops.$',07,-2)
 
            CASE (5)                                       !Compressor
               CALL PRINT('Select compressor type and define $',34,-2)
               CALL PRINT('the performance parameters.  Both$',33,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('reciprocating and rotary designs$',32,-2)
               CALL PRINT(' are represented.$',17,-2)
 
            CASE (6)                                       !Mullion
               CALL PRINT('Define the performance of the inter$',35,-2)
               CALL PRINT('changer(s).  A standard design$',30,-2)
               CALL GOTOXY(6,21)
               CALL PRINT('has one interchanger.  The Lorenz $',34,-2)
               CALL PRINT('cycle has two.$',14,-2)
 
        END SELECT
 
      END DO
 
      imen = numsel
      CALL cursor(0)
      ibypas = 0
 
      RETURN
      END
