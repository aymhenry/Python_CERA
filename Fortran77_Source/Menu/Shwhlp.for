      SUBROUTINE SHWHLP(chrhlp, iret)
C     ******************************************************************
C     *             DISPLAY THE HELP MENU ON THE SCREEN                *
C     ******************************************************************
C
      LOGICAL no_error, no_exit, show_help
 
      CHARACTER    chr, chrhlp, null
      CHARACTER*72 lines(100)
      DIMENSION    chrhlp(1)
 
C
C          READ THE BINARY FILE CHRHLP AND SET UP LINES
C
      null = CHAR(0)
      no_error = .TRUE.
 
      CALL chrlns(chrhlp, lines, lmax)
C
C          SET UP THE BASIC SCREEN
C
      CALL setatr(112)
      CALL gotoxy(0,0)
      CALL screen(0)
 
      CALL gotoxy(03,0)
      CALL print('<Esc>: Return$',13,-2)
      CALL gotoxy(35,0)
      CALL print('Help Menu$',09,-2)
      CALL gotoxy(34,0)
      CALL atrbut(11,79)
      CALL gotoxy(63,0)
      CALL print('<Alt-P>: Print$',14,-2)
 
      CALL setatr(57)
      CALL window(1, 24, 0, 79, 219, 0)
 
      CALL setatr(31)
      CALL window(4, 21, 2, 77, 32, 0)
 
 
      CALL gotoxy(4, 3)
      CALL atrbut(72, 112)
      CALL gotoxy(4, 3)
      CALL print(lines(1),72, -2)
 
      CALL gotoxy(4, 22)
      CALL print(lines(2),72, -2)
!     CALL print('                                    ', 36, -2)
!     CALL gotoxy(40, 22)
!     CALL print('                                    ', 36, -2)
      CALL gotoxy(4, 22)
      CALL atrbut(72, 112)
      CALL gotoxy(37, 22)
      CALL gotoxy(38, 22)
      CALL print('ERA$', 3, -2)
 
      lend = 17
      IF(lmax .LT. 17) lend = lmax
      DO l = 2, lend
         CALL gotoxy(4, l+3)
         CALL print(lines(l), 72, -2)
      END DO
 
      marker = 0
      IF(lmax .GT. 17) THEN
         CALL gotoxy(78,5)
         CALL atrbut(1, 79)
         CALL gotoxy(78,5)
         CALL print(CHAR(24),1,-2)
 
         CALL gotoxy(1,5)
         CALL atrbut(1, 79)
         CALL gotoxy(1,5)
         CALL print(CHAR(24),1,-2)
 
         CALL gotoxy(78,20)
         CALL atrbut(1, 79)
         CALL gotoxy(78,20)
         CALL print(CHAR(25),1,-2)
 
         CALL gotoxy(1,20)
         CALL atrbut(1, 79)
         CALL gotoxy(1,20)
         CALL print(CHAR(25),1,-2)
 
         marker = 6
 
         CALL setatr(79)
         CALL window(7, 19, 78, 78, 186, 0)
         CALL window(7, 19, 01, 01, 186, 0)
         CALL setatr(31)
 
         CALL gotoxy(78, 6)
         CALL print(CHAR(176), 1, -2)
         CALL gotoxy(78, 6)
         CALL atrbut(1, 79)
 
         CALL gotoxy(1, 6)
         CALL print(CHAR(176), 1, -2)
         CALL gotoxy(1, 6)
         CALL atrbut(1, 79)
 
      END IF
 
      l_offset = 0
      no_exit = .TRUE.
      DO WHILE (no_exit)
         l_change = 0
         show_help = .FALSE.
         CALL inchr(1,j, chr)
         iret = j
 
         IF(j .EQ. 25 .AND. chr .EQ. null) RETURN
 
         SELECT CASE (j)
            CASE (1)                                       !<Esc>
               no_exit = .FALSE.
 
            CASE (72)                                      !Up cursor
               IF(l_offset .GT. 0) THEN
                  l_offset = l_offset - 1
                  l_change = -1
                  show_help = .TRUE.
               END IF
 
            CASE (80)                                      !Dn cursor
               IF((l_offset + 17) .LT. lmax) THEN
                  l_offset = l_offset + 1
                  l_change = +1
                  show_help = .TRUE.
               END IF
 
            CASE (71)                                      !<home>
               IF(l_offset .GT. 0) THEN
                  l_offset = 0
                  show_help = .TRUE.
               END IF
 
            CASE (79)                                      !<End>
               IF((l_offset + 17) .LT. lmax) THEN
                  l_offset = lmax - 17
                  show_help = .TRUE.
               END IF
 
            case (73)                                      !<PgUp>
               IF(l_offset .GT. 0) THEN
                 l_offset = l_offset - 16
                 IF(l_offset .LT. 0) l_offset = 0
                 show_help = .TRUE.
               END IF
 
            CASE (81)                                      !<PgDn>
               IF((l_offset + 17) .LT. lmax) THEN
                 l_offset = l_offset + 16
                 IF(l_offset .GT. lmax - 17) l_offset = lmax - 17
                 show_help = .TRUE.
               END IF
 
         END SELECT
 
         IF(show_help) THEN
            IF(marker .NE. 0) THEN
                  dmove = 6.0 + 0.9*(19.0 - FLOAT(marker))/13.0
                  move = 13.0*FLOAT(l_offset)/FLOAT(lmax-17) + dmove
 
               IF(move .EQ. 6 .AND. l_offset .GT. 0) move = 7
               IF(move .NE. marker .AND. move .LT. 20) THEN
                  CALL setatr(79)
                  CALL window(move, 19, 78, 78, 186, 0)
                  CALL window(move, 19, 01, 01, 186, 0)
 
                  CALL window(6, move, 78, 78, 176, 0)
                  CALL window(6, move, 01, 01, 176, 0)
                  CALL setatr(31)
 
               END IF
 
               marker = move
            END IF
 
            IF(l_change .EQ. 0) THEN
               CALL window(4, 21, 2, 77, 32, 0)
               DO l = 2, lend
                  CALL gotoxy(4, l+3)
                  CALL print(lines(l+l_offset), 72, -2)
               END DO
            ELSE IF (l_change .EQ. 1) THEN
               CALL window(5, 20, 2, 77, 01, -1)
               CALL gotoxy(4, lend+3)
               CALL print(lines(lend+l_offset), 72, -2)
            ELSE
               CALL window(5, 20, 2, 77, 01, -2)
               CALL gotoxy(4, 5)
               CALL print(lines(2+l_offset), 72, -2)
            END IF
 
         END IF
 
      END DO
 
      RETURN
      END
