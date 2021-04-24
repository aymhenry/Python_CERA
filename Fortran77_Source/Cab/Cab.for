$debug
      PROGRAM CAB
C     ******************************************************************
C     *               REFRIGERATOR CABINET LOADS                       *
c     *                                                                *
C     *          REVISED FOR USE WITH EPA PROGRAM 'ERA'                *
C     ******************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL EXISTS
      CHARACTER*11 FILIN, FILOUT
      INTEGER*2 VSET(300)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON            Q(4,12)
      COMMON / AIRCAB / QRS, QZS, EVAPCA, RIR, RIZ
      COMMON / EVAPHX / FEAREA, FEAF, FEUA, FECA, REAREA,
     .                  REAF, REUA, RECA, ITPFEV, ITPREV, IHWP
      COMMON / TRIAL /  NTRIAL(8), GRL, HNC, HA, HIN,
     .                  ETA, H1SAVE, H3SAVE, H10SAV
      COMMON / QUALTY / DHEVAP, CPT9, QUAL, T7SHT
      COMMON / LIMIT /  FINISH, DAYKWH, T3, T6, T10, RUN, RUNR, RUNZ,
     .                  TWP, W, PCOMP, QCOMP, TMA
      COMMON / CNDNSR / TACOND
      COMMON / OUTLST / QENEW
      COMMON / INPT /   ISIENG, IC, ICC
      COMMON / INPTAB / EDH, TCYDEF, EHTR, EFAN, EMISC, TCOMON
      COMMON / ERA /    IRFTYP, FZHEAT, FFHEAT, WATERZ, WATERF, HXVUR,
     .                  HXVUZ
      COMMON / OPEND /  QDFFCS, QDFFFS, QDFZCS, QDFZFS, QDFFCL, QDFFFL,
     .                  QDFZCL, QDFZFL
C
      DATA FILIN, FILOUT / 'CABINET.DAT', 'CABINET.OUT' /
C
C           SCREEN OUTLINE DATA
C
      DATA VSET/1,1,201,7,1, 2,1,205,7,35, 37,1,181,7,1, 38,1,32,120,1,
     .     39,1,69,120,1, 40,1,82,120,1, 41,1,65,120,1, 42,1,32,120,1,
     .     43,1,198,7,1,  44,1,205,7,36,
     .     80,1,187,7,1,  1,2,186,7,1,   80,2,186,7,1,  1,3,186,7,1,
     .     80,3,186,7,1,  1,4,186,7,1,   80,4,186,7,1,  1,5,186,7,1,
     .     80,5,186,7,1, 1,6,186,7,1,  80,6,186,7,1, 1,7,186,7,1,
     .     80,7,186,7,1,  1,8,186,7,1,   80,8,186,7,1,  1,9,186,7,1,
     .     80,9,186,7,1,  1,10,186,7,1,  80,10,186,7,1, 1,11,186,7,1,
     .     80,11,186,7,1, 1,12,186,7,1,  80,12,186,7,1, 1,13,186,7,1,
     .     80,13,186,7,1, 1,14,186,7,1,  80,14,186,7,1, 1,15,186,7,1,
     .     80,15,186,7,1, 1,16,186,7,1,  80,16,186,7,1, 1,17,186,7,1,
     .     80,17,186,7,1, 1,18,186,7,1,  80,18,186,7,1, 1,19,186,7,1,
     .     80,19,186,7,1, 1,20,186,7,1,  80,20,186,7,1, 1,21,186,7,1,
     .     80,21,186,7,1, 1,22,186,7,1,  80,22,186,7,1,
     .     1,23,186,7,1,  80,23,186,7,1, 1,24,186,7,1,  80,24,186,7,1,
     .     1,25,200,7,1,  2,25,205,7,78, 80,25,188,7,1/
C
C           SET UP SCREEN COLOR IF COLOR MONITOR PRESENT
C
C
      CALL CURSOR(1)
      CALL GETCOL('MONO_SCREEN=YES$','SETUP.DAT ',IRET)
      IRET = 1 - IRET
      IATR = 30
C
C           DEFINE THE READ AND WRITE UNITS
C
      ICYCL = 0
      IN    = 5
      IO    = 6
      OPEN(IN, FILE=FILIN)
      OPEN(IO, FILE=FILOUT, STATUS='UNKNOWN')
C
C           OPEN OUTPUT FILE FOR CYCLE MODEL IF IT IS TO BE RUN
C
      INQUIRE (FILE='CYCLE.DAT', EXIST=EXISTS)
      IF(EXISTS) THEN
         ICYCL = 4
         OPEN(ICYCL, FILE='CYCLE.DAT', ACCESS='APPEND')
      END IF
C
C          IF A COLOR MONITOR SET THE ATTRIBUTE
C
      IF(IRET .EQ. 1) THEN
         IATR = 113
         CALL TRAP
         CALL SETATR1(0)
      END IF
C
C          SET UP THE SCREEN MESSAGE
C
      CALL GOTOXY(0,0)
      CALL SCREEN(0)
      CALL PICTRE(60,VSET)
      CALL GOTOXY(37,0)
      IF(IATR .EQ. 113) CALL ATRBUT1(5,79)
 
      CALL GOTOXY(28,12)
      CALL PRINT('CABINET LOADS CALCULATION$',25,-2)
 
C
C           THIS IS A CABINET ONLY MODEL
C
      CABYN = .TRUE.
C
C           START THE PROGRAM
C
      CALL START
C
C          COMPLETE TERMINATION OF ROUTINE
C
      CLOSE(IN)
      CLOSE(IO)
      IF(EXISTS) CLOSE(ICYCL)
 
      CALL CURSOR(0)
      IF(IATR .EQ. 113) CALL SETATR1(1)
      CALL EXIT
      END
