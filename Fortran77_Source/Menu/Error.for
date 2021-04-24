      SUBROUTINE ERROR(LINES)
C     ******************************************************************
C     *    OUTPUT AN ERROR MEASSAGE WITH EXPLODING WINDOW AND WARBLE   *
C     ******************************************************************
C
      LOGICAL         change_window
      COMMON /DISPLY/ change_window
 
      CALL warble                                          !Sound alarm
      CALL warble
 
      CALL gotoxy(0,0)                                     !Clear screen
      CALL screen(0)
 
      CALL setatr(48)                                      !Set up panel
      CALL window(0, 0, 0, 79, 32, 0)
      CALL gotoxy(37,0)
      CALL print(' ERA ',05,-2)
      CALL gotoxy(37,0)
      CALL atrbut(05,79)
 
      CALL gotoxy(0,24)                                    !Bottom line
      CALL atrbut(80,112)
 
      CALL setatr(78)                                      !Exploding window
      CALL window(11, 12, 37, 42, 32, 0)
      CALL wait(1)
      CALL window(10, 13, 29, 50, 32, 0)
      CALL wait(1)
      CALL window(9, 14, 21, 58, 32, 0)
      CALL wait(1)
 
      SELECT CASE (lines)
        CASE (1)                                           ! 1 line
          CALL window(8, 14, 11, 68, 32, 0)
          CALL window(9, 13, 15, 64, 32, 1)
 
          CALL setatr(48)
          CALL window(15, 15, 13, 70, 219, 0)
          CALL window(09, 14, 69, 70, 219, 0)
 
 
        CASE (2)                                           ! 2 lines
          CALL window(8, 15, 11, 68, 32, 0)
          CALL window(9, 14, 15, 64, 32, 1)
 
          CALL setatr(48)
          CALL window(16, 16, 13, 70, 219, 0)
          CALL window(09, 15, 69, 70, 219, 0)
 
        CASE (3)                                           ! 3 lines
          CALL window(8, 16, 11, 68, 32, 0)
          CALL window(9, 15, 15, 64, 32, 1)
 
          CALL setatr(48)
          CALL window(17, 17, 13, 70, 219, 0)
          CALL window(09, 16, 69, 70, 219, 0)
 
        CASE (4)                                           ! 4 lines
          CALL window(8, 17, 11, 68, 32, 0)
          CALL window(9, 16, 15, 64, 32, 1)
 
          CALL setatr(48)
          CALL window(18, 18, 13, 70, 219, 0)
          CALL window(09, 17, 69, 70, 219, 0)
 
      END SELECT
 
      CALL gotoxy(27,24)
      CALL print('Press any key to continue$',25,-2)
 
      CALL setatr(30)                                     !Reset color
      change_window = .TRUE.
 
      RETURN
      END
