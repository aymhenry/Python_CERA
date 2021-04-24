      SUBROUTINE DOSCAL(LPAUSE,LCOL,LKEY,LSCRN,STRING)
C     ******************************************************************
C     *    GENERALIZED CALL TO DOS FROM FORTRAN PROGRAM.               *
C     ******************************************************************
C
C          LPAUSE              LOGICAL VARIABLE THAT DETERMINES IF AN
C                              IMMEDIATE RETURN IS MADE WITHOUT A
C                              PAUSE (.FALSE.) OR IF THE USE IS
C                              INSTRUCTED TO PRESS A KEY OR TP TYPE
C                              'EXIT' (.TRUE.).
C
C          LOCL                LOGICAL VARIABLE THAT DEFINES IF COLOR
C                              IS BEING USED.  SET = .TRUE. IF SETATR
C                              HAS BEEN USED TO SET UP COLOR.  IT IS
C                              ASSUMED THAT "TRAP' HAS BEEN CALLED TO
C                              PREVENT A COMPUTER CRASH WITH THE VIDEO
C                              NOT RE-INITIALIZED.  IT IS FURTHER ASSUMED
C                              THAT "TRAP IS CALLED ONLY IF SETATR HAS
C                              BEEN CALLED.
C          LKEY                LOGICAL VARIABLE THAT DETERMINES IF A
C                              KEY PRESS IS DESIRED BEFORE RETURNING TO
C                              THE CALLING ROUTINE.  THIS MAY BE USEFUL
C                              WHEN A DIRECTORY SEARCH HAS BEEN REQUESTED.
C          LSCRN               LOGICAL CHARACTER THAT DETERMINES IF THE
C                              SCREEN IS TO BE SAVED PRIOR TO THE DOS CALL.
C                              IF SO, THE CURSOR POSITION WILL BE SAVED ALSO.
C          STRING              A CHARACTER STRING CONTAINING THE DOS
C                              COMMAND, TERMINATED BY A $ SIGN.  FOR
C                              EXAMPLE, A CLEAR SCREEN COULD BE CALLED
C                              BY DEFINING STRING "CLS$".  TO TEMPORARALLY
C                              EXIT TO DOS, PASS THE NULL STRING "$".
C
      LOGICAL         lcol, lkey, lscrn, lpause, lcolr
      CHARACTER       key
      CHARACTER*64    string
      INTEGER*2       icol, irow
 
      COMMON /HLPCOL/ iatr
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
C
C          CHECK AVAILABLE MEMORY FOR DOS CALL
C
      CALL MEMORY(ISIZE)
      IF (ISIZE .LE. 4000) THEN
         CALL error (2)
         CALL gotoxy(22, 11)
         CALL print('Insufficient Memory to Perform Task ', 36, -2)
         CALL gotoxy(22, 12)
         CALL print('See Manual for Suggestions on Memory', 36, -2)
         CALL inchr(1,j,key)
         ifnd = 1
         RETURN
       END IF
C
C          IF COLOR SET, UNTRAP THE VIDEO AND DOS EXIT ROUTINES
C
      lcolr = lcol
      IF(icolr .EQ. 0) lcolr = .FALSE.
      IF(lcolr) CALL untrap
C
C          SAVE THE screen?
C
      IF(lscrn) THEN
           CALL getxy(icol,irow)
           CALL screen(9)
      END IF
C
C          CALL THE DOS ROUTINE
C
      IF (lpause) THEN
         CALL dos('CLS$')
      END IF
 
      CALL gotoxy(0,0)
 
      IF(.NOT. lkey .AND. lpause) THEN
         CALL print('Type EXIT to return from DOS$',28,-2)
      END IF
      CALL dos(string)
C
C          WAIT FOR A key PRESS IF NEEDED
C
      IF(lkey .AND. lpause) THEN
           WRITE(*,'()')
           CALL gotoxy(0,24)
           CALL print('Press any Key to Return$',23,-2)
           CALL inchr(1,j,key)
      END IF
C
C          CLEAR screen
C
      IF(lpause) THEN
          CALL dos('CLS$')
      END IF
C
C          IF COLOR, RESET IT USING trap AND SETATR
C
      IF(lcolr) THEN
           CALL trap
           CALL setatr(0)
           CALL setatr(30)
      END IF
C
C          IF SCREEN SAVED, RESTORE IT
C
      IF(lscrn) THEN
           CALL screen(8)
           CALL gotoxy(icol,irow)
      END IF
C
      RETURN
      END
