      SUBROUTINE GETMEN (IBUF, IMEN)
C     ******************************************************************
C     *                      MAIN MENU DISPLAY                         *
C     ******************************************************************
C
      LOGICAL getting_command
 
      CHARACTER chr
 
      INTEGER*2 MSET,MSET1,MSET2,MSET3,MSET4
 
      DIMENSION MSET(230), MSET1(80), MSET2(50), MSET3(50), MSET4(50)
      DIMENSION IROW(11), ICOL(11), IEND(11), IPOS(11)
      DIMENSION LF(11), RT(11), UP(11), DN(11)
 
      EQUIVALENCE (MSET(1),   MSET1(1)),     (MSET(81),  MSET2(1)),
     .            (MSET(131), MSET3(1)),     (MSET(181), MSET4(1))
 
      DATA IROW/07, 08, 09, 10, 11, 15, 16, 07, 08, 15, 16/
      DATA ICOL/07, 07, 07, 07, 07, 07, 07, 44, 44, 44, 44/
      DATA IEND/35, 35, 35, 35, 35, 35, 35, 72, 72, 72, 72/
      DATA IPOS/1, 2, 3, 4, 11, 10, 9, 5, 6, 7, 8/
 
      DATA UP / 11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /
      DATA DN / 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1 /
      DATA LF / 8, 9, 9, 9, 9, 10, 11, 1, 2, 6, 7 /
      DATA RT / 8, 9, 9, 10, 10, 10, 11, 1, 2, 6, 7 /
 
      DATA MSET1/07,07,218,07,01, 08,07,196,07,29, 37,07,191,07,01,
     .           07,08,179,07,01, 37,08,179,07,01, 07,09,179,07,01,
     .           37,09,179,07,01, 07,10,179,07,01, 37,10,179,07,01,
     .           07,11,179,07,01, 37,11,179,07,01, 07,12,179,07,01,
     .           37,12,179,07,01, 07,13,192,07,01, 08,13,196,07,29,
     .           37,13,217,07,01/
 
      DATA MSET2/44,07,218,07,01, 45,07,196,07,29, 74,07,191,07,01,
     .           44,08,179,07,01, 74,08,179,07,01, 44,09,179,07,01,
     .           74,09,179,07,01, 44,10,192,07,01, 45,10,196,07,29,
     .           74,10,217,07,01/
 
      DATA MSET3/07,15,218,07,01, 08,15,196,07,29, 37,15,191,07,01,
     .           07,16,179,07,01, 37,16,179,07,01, 07,17,179,07,01,
     .           37,17,179,07,01, 07,18,192,07,01, 08,18,196,07,29,
     .           37,18,217,07,01/
 
      DATA MSET4/44,15,218,07,01, 45,15,196,07,29, 74,15,191,07,01,
     .           44,16,179,07,01, 74,16,179,07,01, 44,17,179,07,01,
     .           74,17,179,07,01, 44,18,192,07,01, 45,18,196,07,29,
     .           74,18,217,07,01/
 
      DATA menset/46/
 
C
C          INITIALIZE
C
   10 continue
 
      irold = 7
      icold = 7
      ieold = 35
 
      numsel = 1
      nummax = 11
 
      CALL cursor(1)
C
C
C          DISPLAY MENU CHOICE BOXES
C
 
      CALL display(1, 4, 22, 3, 76)
      CALL gotoxy(34, 3)
      CALL atrbut(11, 79)
      CALL gotoxy(34, 3)
      CALL print(' Main Menu $',11,-2)
      CALL pictre(menset,MSET)
 
      CALL gotoxy(6,20)
      CALL print('Read a new data file into memory.  $',35,-2)
      CALL print('The command will lead to a data $',32,-2)
      CALL gotoxy(6,21)
      CALL print('file selection menu.  A return $',31,-2)
      CALL print('will be made to this menu.$',26, -2)
 
      CALL gotoxy(8,7)
      CALL print('Read in a Data File$',29,-2)
      CALL gotoxy(7,7)
      CALL light(35,3)
      CALL gotoxy(45,7)
      CALL print('Calculate Cabinet Loads$',23,-2)
 
      CALL gotoxy(8,08)
      CALL print('Edit Cabinet Data$',27,-2)
      CALL gotoxy(45,08)
      CALL print('Calculate Energy$',26,-2)
 
      CALL gotoxy(8,09)
      CALL print('Edit Cycle Data$',15,-2)
 
      CALL gotoxy(8,10)
      CALL print('Save Data to a File$',19,-2)
 
      CALL gotoxy(8,11)
      CALL print('Delete Data File$',16,-2)
 
      CALL gotoxy(8,15)
      CALL print('Temporary Exit to DOS$',21,-2)
      CALL gotoxy(45,15)
      CALL print('Print Data Menus$',17,-2)
 
      CALL gotoxy(8,16)
      CALL print('Quit Program$',12,-2)
      CALL gotoxy(45,16)
      CALL print('Write Data Menus to a File$',26,-2)
 
      CALL line (4, 75, 19)
      CALL gotoxy(12, 19)
      CALL print(' <F6> Limit Checks ', 19, -2)
      CALL gotoxy(12, 19)
      CALL atrbut(19, 79)
      CALL gotoxy(48, 19)
      CALL PRINT(' <F7> Printer Port ', 19, -2)
      CALL gotoxy(48, 19)
      CALL atrbut(19, 79)
C
C          MOVE THE CURSOR TO SELECT THE ACTION
C
      getting_command = .TRUE.
      DO WHILE(getting_command)
         CALL inchr(1, j, chr)
         SELECT CASE (j)
            CASE (1)                                       !<Esc>
               newsel = 7
 
            CASE (72)                                      !Up cursor
               newsel = UP(numsel)
 
            CASE (80)                                      !Dn cursor
               newsel = DN(numsel)
 
            CASE (75)                                      !Left cursor
               newsel = LF(numsel)
 
            CASE (77)                                      !Right cursor
               newsel = RT(numsel)
 
            CASE (71)                                      !<Home>
               newsel = 1
 
            CASE (79)                                      !<End>
               newsel = nummax
 
            CASE (59)                                      !<F1>
               call help(3)
               newsel = numsel
               CALL cursor(1)
 
            CASE (67)                                      !<F9>
               CALL help(6)
               newsel = numsel
               CALL cursor(1)
 
            CASE (28, 81)                                  !Select command
               IF(newsel .EQ. 4 .AND. ibuf .EQ. 0) CYCLE
               CALL cursor(0)
               getting_command = .FALSE.
               CYCLE
 
            CASE (64)                                      !<F6> key
               CALL mainfunc(1)
               go to 10
 
            CASE (65)                                      !<F7> key
               CALL mainfunc(2)
               go to 10
 
            CASE DEFAULT
               CALL warble
               CYCLE
 
         END SELECT
 
C
C          MOVE THE HIGHLIGHTING
C
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
 
         SELECT CASE (numsel)
            CASE (1)                                       !Read file
               CALL print('Read a new data file into memory.  $',35,-2)
               CALL print('The command will lead to a data $',32,-2)
               CALL gotoxy(6,21)
               CALL print('file selection menu.  A return $',31,-2)
               CALL print('will be made to this menu.$',26,-2)
 
            CASE (2)                                       !Edit cab data
               CALL print('Edit the cabinet data.  Branch $',31,-2)
               CALL print('to the CABINET EDIT MENU.$',25,-2)
 
            CASE (3)                                       !Edit cycle data
               CALL print('Edit the cycle data.  Branch $',29,-2)
               CALL print('to the CYCLE EDIT MENU.$',23,-2)
 
            CASE (4)                                       !Save data
               CALL print('Save the data to a file.  Any changes',37,-2)
               CALL print(' made to the data will be lost$',30,-2)
               CALL gotoxy(6,21)
               CALL print('once simulation of the cabinet loads$',37,-2)
               CALL print(' or refrigeration cycle begins.$',31,-2)
 
            CASE (5)                                       !Make backup
               CALL print('Delete a data file to create space ',35,-2)
               CALL print('for a new file.  The file is $',29,-2)
               CALL gotoxy(6,21)
               CALL print('renamed NAME.BAK, where NAME is the$',36,-2)
               CALL print(' original name of the file.$',27,-2)
 
            CASE (6)                                       !DOS
               CALL print('A temporary return to DOS allows $',33,-2)
               CALL print('access to DOS functions such as $',32,-2)
               CALL gotoxy(6,21)
               CALL print('a directory search, printing, or $',33,-2)
               CALL print('other operations.$',17,-2)
 
            CASE (7)                                       !Quit
               CALL print('Leave ERA and return to DOS.$',28,-2)
 
            CASE (8)                                       !Calculate cab
               CALL print('Calculate the cabinet loads.$',28,-2)
 
            CASE (9)                                       !Calculate cycle
               CALL print('Calculate the both the cabinet $',31,-2)
               CALL print('loads and the refrigeration cycle$',33,-2)
               CALL gotoxy(6,21)
               CALL print('performance.  Output the daily $',31,-2)
               CALL print('energy use.$',11,-2)
 
            CASE (10)                                       !Print input
               CALL print('Send a copy of the input data menu $',35,-2)
               CALL print('and values to the printer.$',26,-2)
 
            CASE (11)                                      !Write input
               CALL print('Send a copy of the input data menu $',35,-2)
               CALL print('and values to a file for later $',31,-2)
               CALL gotoxy(6,21)
               CALL print('printing.  The file name will have $',35,-2)
               CALL print('the extension INP.$',18,-2)
 
        END SELECT
 
      END DO
 
      imen = IPOS(numsel)
      RETURN
 
      END
 
      SUBROUTINE MAINFUNC (LOC)
C     ******************************************************************
C     *             HANDLE F6 AND F7 REQUESTS IN MAIN MENU             *
C     ******************************************************************
C
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore, iold_data
 
      LOGICAL         CHANGED
      CHARACTER       key
 
      CHANGED = .TRUE.
      SELECT CASE (LOC)
         CASE (1)                                          !ignore limits
            DO WHILE (CHANGED)
               IF (ignore .EQ. 1) THEN
                  CALL setatr (78)
               ELSE
                  CALL setatr (63)
               END IF
               CALL window (8, 17, 11, 68, 32, 0)
               CALL window (9, 16, 15, 64, 32, 1)
 
               CALL setatr (48)
               CALL window (18, 18, 13, 70, 219, 0)
               CALL window (09, 17, 69, 70, 219, 0)
 
               CALL gotoxy (26, 11)
               IF (ignore .EQ. 1) THEN
                  CALL print('Data Limits will be ignored$', 27, -2)
               ELSE
                  CALL print('Data Limits will be checked$', 27, -2)
               END IF
 
               CALL gotoxy (26, 13)
               CALL print('Press space bar to change$', 25, -2)
               CALL gotoxy (26, 14)
               CALL print('Press any key to continue$', 25, -2)
 
               CALL inchr(1, j, key)
               IF (j .EQ. 57) THEN
                  ignore = 1 - ignore
               ELSE
                  CHANGED = .FALSE.
               END IF
 
            END DO
 
           CALL setatr(30)
 
         CASE (2)                                          !
            CALL setatr (63)
            CALL window (8, 17, 11, 68, 32, 0)
            CALL window (9, 16, 15, 64, 32, 1)
 
            CALL setatr (48)
            CALL window (18, 18, 13, 70, 219, 0)
            CALL window (09, 17, 69, 70, 219, 0)
 
            DO WHILE (CHANGED)
               CALL gotoxy (26, 11)
               IF (iport .EQ. 0) THEN
                  CALL print('Printer port LPT1 is active$', 27, -2)
               ELSE
                  CALL print('Printer port LPT2 is active$', 27, -2)
               END IF
 
               CALL gotoxy (26, 13)
               CALL print('Press space bar to change$', 25, -2)
               CALL gotoxy (26, 14)
               CALL print('Press any key to continue$', 25, -2)
 
               CALL inchr(1, j, key)
               IF (j .EQ. 57) THEN
                  iport = 1 - iport
               ELSE
                  CHANGED = .FALSE.
               END IF
 
            END DO
 
           CALL setatr(30)
 
      END SELECT
 
      RETURN
      END
