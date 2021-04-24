      SUBROUTINE HELP(LOC)
C     *****************************************************************
C     *     HELP MESSAGE MASTER ROUTINE                               *
C     *****************************************************************
C
      LOGICAL         looking_for_return
      CHARACTER       CHRMNM, CHRMNG, CHRMNE, CHRKEY, CHRSUB, CHRSAV,
     .                CHRCYC, CHRREF, CHRDOE
 
      INTEGER*2       icol, irow
      DIMENSION       IPPNT(9)
 
      COMMON /CHRHLP/ CHRMNM(3600), CHRMNG(1100), CHRMNE(1100),
     .                CHRKEY(1000), CHRSUB(9000), CHRSAV(1100),
     .                CHRCYC(1100), CHRREF(8200), CHRDOE(4000)
      COMMON /CHANGE/ isav, ilpt1, numsel, ibypas, numpat, ibuf, ifile
 
      COMMON /HLPCOL/ iatr
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
 
      DATA idisk/6/
      DATA IPPNT /6801, 1, 3601, 4701, 5801, 15801, 16901, 18001, 26201/
C
C          INITIALIZE
C
      SELECT CASE (loc)
         CASE (1)                                          !Read and store
            READ(idisk, IOSTAT=iocheck) CHRSUB
            RETURN
 
         CASE DEFAULT
            ibufsv = ibuf
            ibuf = 0
            CALL getxy(icol,irow)
            CALL screen(9)
            CALL cursor(1)
            IF(icolr .EQ. 1) CALL setatr(iatr)
 
            looking_for_return = .TRUE.
            DO WHILE (looking_for_return)
               SELECT CASE(loc)
                  CASE (2)
                     CALL shwhlp(CHRSUB, iret)
 
                  CASE (3)
                     CALL shwhlp(CHRMNM, iret)
 
                  CASE (4)
                     CALL shwhlp(CHRMNG, iret)
 
                  CASE (5)
                     CALL shwhlp(CHRMNE, iret)
 
                  CASE (6)
                     CALL shwhlp(CHRKEY, iret)
 
                  CASE (7)
                     CALL shwhlp(CHRSAV, iret)
 
                  CASE (8)
                     CALL shwhlp(CHRCYC, iret)
 
                  CASE (9)
                     CALL shwhlp(CHRREF, iret)
 
                  CASE (10)
                     CALL shwhlp(CHRDOE, iret)
 
               END SELECT
 
               IF(iret .EQ. 25) THEN
                  lpnt = IPPNT(loc-1)
                  CALL prnhlp(CHRMNM(lpnt))
               ELSE
                  looking_for_return = .FALSE.
               END IF
 
            END DO
 
      END SELECT
C
C          RESTORE ORIGINAL SCREEN
C
      IF(icolr .EQ. 1) CALL setatr(30)
      CALL screen(8)
      CALL gotoxy(ICOL,IROW)
      CALL cursor(0)
      ibuf = ibufsv
 
      RETURN
 
      END
 
      SUBROUTINE PRNHLP(CHRSUB)
C     *****************************************************************
C     *     PRINT OUT THE HELP MENU                                   *
C     *****************************************************************
C
      LOGICAL         not_end_of_file
 
      CHARACTER       CHRSUB, form_feed, HELP_CHRS
      CHARACTER*4     file_unit
      CHARACTER*72    help_line
 
      INTEGER*4       prnchr
 
      DIMENSION CHRSUB(1), PRNCHR(256), HELP_CHRS(72)
 
      COMMON /CHANGE/ isav, ilpt1, numsel, ibypas, numpat, ibuf, ifile
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
 
      EQUIVALENCE     (help_line,  HELP_CHRS(1))
C
      DATA iprnt/5/
      DATA PRNCHR /000, 001, 002, 003, 004, 005, 006, 007, 008, 009,
     .             010, 011, 012, 013, 014, 015, 045, 045, 124, 019,
     .             020, 021, 022, 023, 124, 124, 026, 027, 028, 029,
     .             030, 031, 032, 033, 034, 035, 036, 037, 038, 039,
     .             040, 041, 042, 043, 044, 045, 046, 047, 048, 049,
     .             050, 051, 052, 053, 054, 055, 056, 057, 058, 059,
     .             060, 061, 062, 063, 064, 065, 066, 067, 068, 069,
     .             070, 071, 072, 073, 074, 075, 076, 077, 078, 079,
     .             080, 081, 082, 083, 084, 085, 086, 087, 088, 089,
     .             090, 091, 092, 093, 094, 095, 096, 097, 098, 099,
     .             100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
     .             110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
     .             120, 121, 122, 123, 124, 125, 126, 127, 046, 046,
     .             046, 046, 046, 046, 046, 046, 046, 046, 046, 046,
     .             046, 046, 046, 046, 046, 046, 046, 046, 046, 046,
     .             046, 046, 046, 046, 046, 099, 046, 046, 046, 046,
     .             046, 046, 046, 046, 046, 046, 046, 046, 046, 046,
     .             046, 046, 046, 046, 046, 046, 046, 046, 046, 124,
     .             046, 046, 046, 046, 046, 046, 124, 046, 046, 046,
     .             046, 046, 046, 046, 046, 046, 045, 046, 046, 046,
     .             046, 046, 046, 046, 046, 046, 046, 046, 046, 046,
     .             046, 046, 046, 046, 046, 046, 046, 046, 046, 046,
     .             046, 046, 046, 046, 046, 046, 046, 046, 046, 046,
     .             046, 046, 046, 046, 046, 046, 046, 046, 046, 046,
     .             046, 046, 046, 046, 046, 046, 046, 046, 046, 046,
     .             046, 046, 046, 050, 046, 046/
C
C          OPEN THE LINE PRINTER
C
      IF(iport .EQ. 1) THEN
         file_unit = 'lpt2'
      ELSE
         file_unit = 'lpt1'
      END IF
 
      OPEN(iprnt, FILE=file_unit, IOSTAT=iocheck, STATUS='OLD')
      IF(iocheck .NE. 0) RETURN
 
      form_feed = CHAR(12)
      if(iform .EQ. 0) form_feed = CHAR(49)
C
C          OUTPUT MENU CHARACTER BY CHARACTER
C
      CALL gotoxy(24,23)
      CALL light(58,2)
      CALL gotoxy(36,23)
      CALL print('Printing$',8,-2)
      CALL gotoxy(33,23)
      CALL light(46,8)
 
      not_end_of_file = .TRUE.
 
      DO l = 1, 72
         help_chrs(l) = ' '
      END DO
 
      k = 0
      l = 0
 
      DO WHILE(not_end_of_file)
         k = k + 1
         l = l + 1
         idum = ICHAR(CHRSUB(k))
 
         SELECT CASE (idum)
            CASE (26)                                      !End of file
               not_end_of_file = .FALSE.
 
            CASE (10)                                      !Line-feed
               WRITE (iprnt,'(1X, A72)') help_line
 
               DO l = 1, 72
                 help_chrs(l) = ' '
               END DO
               l = 0
 
            CASE (13)                                      !Carriage return
               !Nothing
 
            CASE DEFAULT
               HELP_CHRS(l) =  CHAR(PRNCHR(idum+1))
 
         END SELECT
      END DO
 
      WRITE(iprnt, '(A1)') form_feed
      CLOSE(iprnt)
 
      CALL gotoxy(24,23)
      CALL light(58,2)
      CALL gotoxy(31,23)
      CALL print('Printout Completed$',18,-2)
 
      RETURN
      END
