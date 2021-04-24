      SUBROUTINE DISPLAY (i_dir, i_top, i_bot, i_lft, i_rgt)
C     ******************************************************************
C     *                 DRAW THE BASIC MENU BACKGROUND                 *
C     ******************************************************************
C
      LOGICAL         same_window, change_window
 
      COMMON /DISPLY/ change_window
 
      DATA            i_top_s /0/,
     .                i_bot_s /0/,
     .                i_rgt_s /0/,
     .                i_lft_s /0/
 
C          COMPARE WITH LAST CALL
 
      same_window = .FALSE.
      IF(i_top .EQ. i_top_s .AND. i_bot .EQ. i_bot_s .AND.
     .   i_lft .EQ. i_lft_s .AND. i_rgt .EQ. i_rgt_s) THEN
         same_window = .TRUE.
      END IF
 
      IF(change_window) same_window = .FALSE.
 
      i_top_s = i_top
      i_bot_s = i_bot
      i_lft_s = i_lft
      i_rgt_s = i_rgt
 
C          MAKE TOP LINE
 
      CALL setatr(48)
      IF(same_window) THEN
         CALL setatr(57)
         CALL window(1, i_top-1, 0, 79, 219, 0)
         CALL setatr(48)
      ELSE
         WRITE(*, '('' ''\)')
         CALL gotoxy(0,0)
         CALL screen(0)
 
         CALL gotoxy(04,0)
         CALL print('F1=Help',07,-2)
         CALL gotoxy(37,0)
         CALL print(' ERA ',05,-2)
         CALL gotoxy(37,0)
         CALL atrbut(05,79)
         CALL gotoxy(69,0)
         CALL print('F9=Keys',07,-2)
      END IF
 
  !   CALL window(24, 24, 0, 79, 32, 0)
      CALL gotoxy(0,24)
      CALL atrbut(80,112)
 
      SELECT CASE (i_dir)
         CASE (1)                                          !Main menu
            CALL gotoxy(04,24)
            CALL PRINT('<Esc>=Quit      ',16,-2)
            CALL gotoxy(25,24)
            CALL print('Move Cursor and Select Option',29,-2)
            CALL gotoxy(60,24)
            CALL print('   <PgDn>=Select',16,-2)
 
         CASE (2)                                          !File select
            CALL gotoxy(04,24)
            CALL PRINT('<Esc>=Main Menu ',16,-2)
            CALL gotoxy(25,24)
            CALL print(' Move Cursor and Select File ',29,-2)
            CALL gotoxy(60,24)
            CALL print('   <PgDn>=Scroll',16,-2)
 
         CASE (3)                                          !Save File
            CALL gotoxy(04,24)
            CALL PRINT('<Esc>=Main Menu ',16,-2)
            CALL gotoxy(25,24)
            CALL print('                             ',29,-2)
            CALL gotoxy(60,24)
            CALL print('   <PgDn>=Scroll',16,-2)
 
         CASE (4)                                          !Edit Menu
            CALL gotoxy(04,24)
            CALL PRINT('<Esc>=Main Menu ',16,-2)
            CALL gotoxy(25,24)
            CALL print('Move Cursor and Select Option',29,-2)
            CALL gotoxy(60,24)
            CALL print('   <PgDn>=Select',16,-2)
 
         CASE (5)                                          !Data menu
            CALL gotoxy(04,24)
            CALL PRINT('<PgUp>=Last Menu',16,-2)
            CALL gotoxy(25,24)
            CALL print('  Move Cursor and Edit Data  ',29,-2)
            CALL gotoxy(60,24)
            CALL print('<PgDn>=Next Menu',16,-2)
 
         CASE (6)                                          !1st page of <F5>
            CALL gotoxy(04,24)                             !  calculation
            CALL PRINT('<Esc>=Cancel    ',16,-2)
            CALL gotoxy(25,24)
            CALL print('  Move Cursor and Edit Data  ',29,-2)
            CALL gotoxy(60,24)
            CALL print('<PgDn>=Next Menu',16,-2)
 
         CASE (7)                                          !2nd page of <F5>
            CALL gotoxy(04,24)                             !  calculation
            CALL PRINT('<Esc>=Cancel    ',16,-2)
            CALL gotoxy(25,24)
            CALL print('  Move Cursor and Edit Data  ',29,-2)
            CALL gotoxy(60,24)
            CALL print('<PgDn>=Calculate',16,-2)
 
         CASE (8, 9, 10)                                   !File select
            CALL gotoxy(04,24)
            CALL PRINT('<Esc>=Exit Menu ',16,-2)
            CALL gotoxy(25,24)
            CALL print(' Move Cursor and Select File ',29,-2)
            CALL gotoxy(60,24)
            CALL print('  <Enter>=Select',16,-2)
 
      END SELECT
 
      IF(same_window) THEN
         CALL setatr(30)
         CALL window(i_top+1, i_bot-1, i_lft+1, i_rgt-1, 32, 0)
         CALL window(i_bot, i_bot, i_lft, i_rgt, 32, 1)
      ELSE
         CALL setatr(57)
         CALL window(1, 23, 0, 79, 219, 0)
         CALL setatr(30)
         CALL window(i_top, i_bot, i_lft, i_rgt, 32, 1)
      END IF
 
      change_window = .FALSE.
      if(i_dir .GE. 8) change_window = .TRUE.
 
      RETURN
      END
