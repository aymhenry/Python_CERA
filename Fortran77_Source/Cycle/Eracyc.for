      PROGRAM ERACYC
C     *****************************************************************
C     *    CYCLE PROGRAM FOR ERA MODEL.  DEVELOPED BY A. D. LITTLE    *
C     *****************************************************************
C
      CHARACTER       key, TITLE
      CHARACTER*13    fildat, filera
      CHARACTER*13    filmap, filmap1, filmap2
      CHARACTER*64    doschr
 
      REAL            meff, mref, mrefi
      LOGICAL         exists, found_data
 
      DIMENSION       TS1(2), ICONDI(2), CFMCI(2), UDSCI(2), UTPCI(2),
     .                USCCI(2), FNPWRC(2), ATOTCI(2), DTSBCI(2),
     .                IFRSHI(2), TS3(2), CFMEI(2), DPE(2), UTPEI(2),
     .                USUPEI(2), ATOTEI(2), DTSPEI(2), IFREZI(2),
     .                FNPWRE(2), MREFI(2), DISPLC(2), SPEEDI(2), CEI(2),
     .                SEFFI(2), MEFF(2), QCAN(2), QHILO(2),
     .                TSPECI(2), ELOSS(2), ETHX(2), DPC(2)
      DIMENSION       DUTYN(2), WCOMP(2), QEL(2)
      DIMENSION       ISPECI(2), QUALTY(2), SUPIHX(2)
      DIMENSION       TITLE(68,5)
      DIMENSION       ICOOLN(2), SIZEN(2), EERN(2), SPDNOM(2)
C
C          COMMON BLOCKS
C
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
      COMMON/PARMS2/TSPEC,I_LIQUID_LINE
      COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
      COMMON/FEVAP/UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
      COMMON/CONDEN/UDSC,UTPC,USCC,ATOTC,UACOND
      COMMON/SPECS/DTSUPE,DTSUBC
      COMMON /SPECS2/ ISPEC,XEXITE,DTSUPI
      COMMON/EVAPS/ITYPE, FRACT_FF, FRACT_FZ
      COMMON/CABLOD/FFASH,FFAUX,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
     .              FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
     .              FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
      COMMON/FANS/FANE,FANZ,FANC,DUTYC,W,COPR
      COMMON/LORENZ/DUTYE,DUTYZ,PWRL,PWRE,CAPE,CAPZ,DUTYL,DUTYS,
     .              FANEL,FANCL,FANES,FANCS
      COMMON/FIGURE/IEVAP
      COMMON / RESULT / QE, QZ, FLOW, QEN(2), FLOWN(2), COPRN(2)
      COMMON / CYCLNG / CORR_COP, COPCYC(2), I_CYCLE, I_VALVE, T_CYCLE
      COMMON /REFRIG/ NC(2), IR(5,2), X(5,2), F(5,5,2)
      COMMON / FILINF / FILERA
      COMMON / CHINA / INCTRL, HRSOFF
      COMMON / BALNCE / IBLNCE, BAFFLF, BAFFLZ, AREAFZ, ATOTE_S,
     .                  AREAFZ_S, ATOTE_A, AREAFZ_A, FFTEMP_A, FZTEMP_A
      COMMON / INWALL / UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
     .                  Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
     .                  CAPZ_IN_WALL, Q_FZ_FF
      COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
     .                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
     .                  CONDF_IN_WALL, CONDZ_IN_WALL
      COMMON / LIQLIN / FFREFQ, FZREFQ, CONDHT(2), CONDVP(2)
      COMMON / CYCLIC / DFSTCYC, FFCYC, FZCYC, OUTCYC
 
      COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
     .              N_EVAP, N_COND
      COMMON / MAPDAT / IMAP, ICOMP, ICOOL, EER, SIZE, DISPL, EFFC,
     .                  SPEEDN, IREAD
      COMMON / PLSTIC / IWALL_FF, IWALL_FZ
      COMMON / MAPNAM / filmap, filmap1, filmap2
      COMMON / PENAT / FFPENA, FZPENA
C
C          DATA STATEMENTS
C
      DATA TS5 /256.0/
      DATA IN /5/
      DATA FILDAT/'ERROR.OUT    '/
C
C          SET UP THE SCREEN
C
      CALL SETPIC
C
C          INITIALIZE ERROR CODE FOR LIQUID LINE ANTI-SWEAT HEAT
C
      I_LIQUID_LINE = 0
C
C          READ IN SOLUTION TOLERANCE DATA (IF FILE EXISTS)
C
      INQUIRE (FILE='CYCLE.TOL', EXIST=EXISTS)
      IF(EXISTS) THEN
           OPEN(IN,FILE='CYCLE.TOL', STATUS='UNKNOWN')
           READ(IN,*) TOL_FRSH
           READ(IN,*) TOL_FRZ
           READ(IN,*) TOL_COND
           READ(IN,*) TOL_MASS
           READ(IN,*) TOL_HX
           READ(IN,*) N_EVAP
           READ(IN,*) N_COND
           CLOSE (IN)
      ELSE
           TOL_FRSH = 0.1
           TOL_FRZ = 0.1
           TOL_COND = 0.075
           TOL_MASS = 0.01
           TOL_HX = 0.001
           N_EVAP = 1
           N_COND = 1
      END IF
C
C          CLEAR KEYBOARD AND OPEN THE DATA FILE 'CYCLE.DAT'
C
      CALL INCHR(3,J,KEY)
      INQUIRE (FILE='CYCLE.DAT', EXIST=EXISTS)
      IF(.NOT. EXISTS) THEN
           CALL GOTOXY(15,12)
           CALL PRINT('File CYCLE.DAT Not Found.  ',27,-2)
           CALL PRINT('Execution Terminated.',21,-2)
 
           CALL GETCOL('MONO_SCREEN=YES$','SETUP.DAT ',IRET)
           IRET = 1 - IRET
           IF(IRET .EQ. 1) THEN
              CALL SETATR(1)
           END IF
             STOP ' '
      END IF
      OPEN(IN,FILE='CYCLE.DAT',STATUS='UNKNOWN')
C
C          READ HEADER DATA AND SAVE
C
      READ(IN,'(5(68A1/))') TITLE
C
C          READ FILE INFORMATION
C
      READ(IN,'(11X,A13)') FILERA
      CALL READLN(1,IN,2,IOCHECK)
C
C          CABINET AND CYCLE DEFINITIONS
C
      READ (IN,*,END=400,ERR=400) IRFTYP
      READ (IN,*,END=400,ERR=400) ICYCL
      CALL READLN(1,IN,2,IOCHECK)
C
C          HANDLE DUAL EVAP CYCLE
C
      ICYCLS = ICYCL
      IF(ICYCL .EQ. 4) ICYCL = 2
C
C          READ IN MANUAL DEFROST CONTROL
C
      READ (IN,*,END=400,ERR=400) IDFRST
      READ (IN,*,END=400,ERR=400) HRSOFF
      CALL READLN(1,IN,2,IOCHECK)
C
C          READ IN CONTROL OPTION IF LORENZ OR DUAL EVAPORATOR CYCLE
C
      IF(ICYCL .EQ. 2) THEN
         READ (IN,*,END=400,ERR=400) INCTRL
         if(inctrl .eq. 5) idfrst = 1
         CALL READLN(1,IN,3,IOCHECK)
      END IF
C
C          READ IN COMPRESSOR OPTIONS
C
      READ (IN,*,END=400,ERR=400) ICOMP
      READ (IN,*,END=400,ERR=400) IMAP
      READ (IN,*,END=400,ERR=400) I_CYCLE
      READ (IN,*,END=400,ERR=400) T_CYCLE
      READ (IN,*,END=400,ERR=400) I_VALVE
      CALL READLN(1,IN,2,IOCHECK)
C
C          PROCESS COMPRESSOR MAP FILE DATA
C
      IFLAG = 0
      IF(IMAP .EQ. 0) THEN
         INQUIRE (FILE='COMPMAP.NAM', EXIST=EXISTS)
         IF(.NOT. EXISTS) THEN
            IFLAG = 1
         ELSE
            OPEN(1, FILE='COMPMAP.NAM', STATUS='UNKNOWN')
            READ(1, '(A13)', IOSTAT=IOCHECK) filmap1
            IF(IOCHECK .NE. 0) IFLAG = 1
            IF(ICYCL .EQ. 3) THEN
               READ(1, '(A13)', IOSTAT=IOCHECK) filmap2
               IF(IOCHECK .NE. 0) IFLAG = 1
            END IF
 
            CLOSE(1)
 
         END IF
 
      END IF
 
      IF(IFLAG .EQ. 1) THEN
         CALL GOTOXY(23,12)
         CALL PRINT('Compressor Map Files Not Defined.',33,-2)
         CALL GOTOXY(23,13)
         CALL PRINT('Execution Terminated.',21,-2)
         READ (*,*)
         CALL FINISH
      END IF
C
C          SET UP A READ DATA LOOP WHICH DEPENDS ON THE CYCLE TYPE
C
      CALL READLN(1,IN,9,IOCHECK)
      IF(ICYCL .EQ. 3) THEN
           NDATA = 2
      ELSE
           NDATA = 1
      END IF
      N = 1
      DO WHILE (N .LE. NDATA)
C
C               REFRIGERANT DATA
C
           CALL READLN(1,IN,4,IOCHECK)
           READ (IN,*,END=400,ERR=400) IR(1,N)
           READ (IN,*,END=400,ERR=400) IR(2,N)
           READ (IN,*,END=400,ERR=400) IR(3,N)
           READ (IN,*,END=400,ERR=400) NC(N)
           READ (IN,*,END=400,ERR=400) F(1,2,N)
           READ (IN,*,END=400,ERR=400) F(1,3,N)
           READ (IN,*,END=400,ERR=400) F(2,3,N)
           F(2,1,N) = F(1,2,N)
           F(3,1,N) = F(1,3,N)
           F(3,2,N) = F(2,3,N)
           READ (IN,*,END=400,ERR=400) X(1,N)
           READ (IN,*,END=400,ERR=400) X(2,N)
           READ (IN,*,END=400,ERR=400) X(3,N)
C
C               CONDENSER DATA
C
           CALL READLN(1,IN,1,IOCHECK)
           READ (IN,*,END=400,ERR=400) ICONDI(N)
           CALL READLN(1,IN,1,IOCHECK)
           READ (IN,*,END=400,ERR=400) TS1(N)
           READ (IN,*,END=400,ERR=400) CFMCI(N)
           READ (IN,*,END=400,ERR=400) FNPWRC(N)
           READ (IN,*,END=400,ERR=400) DPC(N)
           READ (IN,*,END=400,ERR=400) UDSCI(N)
           READ (IN,*,END=400,ERR=400) UTPCI(N)
           READ (IN,*,END=400,ERR=400) USCCI(N)
           READ (IN,*,END=400,ERR=400) ATOTCI(N)
           READ (IN,*,END=400,ERR=400) DTSBCI(N)
 
           READ (IN,*,END=400,ERR=400) CONLIQ
           READ (IN,*,END=400,ERR=400) CONVAP
           CONDHT(N) = 3.6*CONLIQ
           CONDVP(N) = 3.6*CONVAP
C
C               FRESH FOOD SECTION DATA
C
           CALL READLN(1,IN,1,IOCHECK)
           READ (IN,*,END=400,ERR=400) ISPECI(N)
           CALL READLN(1,IN,1,IOCHECK)
           READ (IN,*,END=400,ERR=400) IFRSHI(N)
           CALL READLN(1,IN,1,IOCHECK)
           READ (IN,*,END=400,ERR=400) TS3(N)
           READ (IN,*,END=400,ERR=400) CFMEI(N)
           READ (IN,*,END=400,ERR=400) FNPWRE(N)
           READ (IN,*,END=400,ERR=400) DPE(N)
           READ (IN,*,END=400,ERR=400) UTPEI(N)
           READ (IN,*,END=400,ERR=400) USUPEI(N)
           READ (IN,*,END=400,ERR=400) ATOTEI(N)
           READ (IN,*,END=400,ERR=400) DTSPEI(N)
           QUALTY(N) = DTSPEI(N)
C
C               FREEZER SECTION DATA
C
           IF(ICYCL .EQ. 2) THEN
                CALL READLN(1,IN,1,IOCHECK)
                READ (IN,*,END=400,ERR=400) IFREZI(N)
                CALL READLN(1,IN,1,IOCHECK)
                READ (IN,*,END=400,ERR=400) TS5
                READ (IN,*,END=400,ERR=400) CFMF
                READ (IN,*,END=400,ERR=400) FANZ
                READ (IN,*,END=400,ERR=400) AREAFZ
                READ (IN,*,END=400,ERR=400) UAF
                READ (IN,*,END=400,ERR=400) DPF
           END IF
C
C               COMPRESSOR DATA
C
           CALL READLN(1,IN,1,IOCHECK)
           IF(IMAP .EQ. 0) THEN
              IF (ICYCL .EQ. 2 .AND. INCTRL .GE. 4) THEN  !!!9-27-94
                 READ (IN,*,END=400,ERR=400) MREFI(1)   !!!6-10-94
                 READ (IN,*,END=400,ERR=400) MREFI(2)   !!!6-10-94
              ELSE
                 READ (IN,*,END=400,ERR=400) MREFI(N)   !!!6-10-94
              END IF
              READ (IN,*,END=400,ERR=400) SPEEDI(N)
              READ (IN,*,END=400,ERR=400) TSPECI(N)
           END IF
 
           IF(IMAP .EQ. 1) THEN
              IF (ICYCL .EQ. 2 .and. inctrl .ge. 4) THEN !!!9-27-94
                 READ (IN,*,END=400,ERR=400) MREFI(1)
                 READ (IN,*,END=400,ERR=400) MREFI(2)
              ELSE
                 READ (IN,*,END=400,ERR=400) MREFI(N)
              END IF
 
              READ (IN,*,END=400,ERR=400) DISPLC(N)
              READ (IN,*,END=400,ERR=400) SIZEN(N)
              READ (IN,*,END=400,ERR=400) SPDNOM(N)
              READ (IN,*,END=400,ERR=400) EERN(N)
              READ (IN,*,END=400,ERR=400) ICOOLN(N)
              READ (IN,*,END=400,ERR=400) SPEEDI(N)
              READ (IN,*,END=400,ERR=400) TSPECI(N)
           END IF
 
           IF(IMAP .EQ. 2) THEN
              IF (ICYCL .EQ. 2 .and. inctrl .ge. 4) THEN !!!9-27-94
                 READ (IN,*,END=400,ERR=400) MREFI(1)
                 READ (IN,*,END=400,ERR=400) MREFI(2)
              ELSE
                 READ (IN,*,END=400,ERR=400) MREFI(N)
              END IF
              READ (IN,*,END=400,ERR=400) DISPLC(N)
              READ (IN,*,END=400,ERR=400) SPEEDI(N)
              READ (IN,*,END=400,ERR=400) CEI(N)
              READ (IN,*,END=400,ERR=400) SEFFI(N)
              READ (IN,*,END=400,ERR=400) MEFF(N)
              READ (IN,*,END=400,ERR=400) ELOSS(N)
              READ (IN,*,END=400,ERR=400) QCAN(N)
              READ (IN,*,END=400,ERR=400) QHILO(N)
              READ (IN,*,END=400,ERR=400) TSPECI(N)
           END IF
C
C               INTERCHANGER DATA
C
           CALL READLN(1,IN,1,IOCHECK)
           READ (IN,*,END=400,ERR=400) SUPIHX(N)
           IF(ICYCL .EQ. 2) THEN
                READ (IN,*,END=400,ERR=400) ETHX1
                READ (IN,*,END=400,ERR=400) ETHX2
           ELSE
                READ (IN,*,END=400,ERR=400) ETHX(N)
           END IF
C
C               IN-WALL EVAPORATOR DATA
C
           IF(N .EQ. NDATA) THEN
              CALL READLN(1,IN,1,IOCHECK)
              READ (IN,*,END=400,ERR=400) UA_FF
              READ (IN,*,END=400,ERR=400) UA_FZ
              READ (IN,*,END=400,ERR=400) UA_ML
 
              READ (IN,*,END=400,ERR=400) UA_FF_CND
              READ (IN,*,END=400,ERR=400) UA_FZ_CND
 
              READ (IN,*,END=400,ERR=400) UA_FF_HXS
              READ (IN,*,END=400,ERR=400) UA_FZ_HXS
 
              READ (IN,*,END=400,ERR=400) FRACT_FF
              READ (IN,*,END=400,ERR=400) FRACT_FZ
 
              READ (IN,*,END=400,ERR=400) IWALL_FF
              READ (IN,*,END=400,ERR=400) IWALL_FZ
 
              UA_FF = UA_FF*1.8961
              UA_FZ = UA_FZ*1.8961
              UA_ML = UA_ML*1.8961
 
              UA_FF_CND = UA_FF_CND*1.8961
              UA_FZ_CND = UA_FZ_CND*1.8961
 
              UA_FF_HXS = UA_FF_HXS*1.8961
              UA_FZ_HXS = UA_FZ_HXS*1.8961
 
              CALL READLN(1,IN,1,IOCHECK)
              READ (IN,*,END=400,ERR=400) DFSTCYC
              IF(IDFRST .EQ. 1) DFSTCYC = 0
 
              IF(IRFTYP .LE. 3 .or. irftyp .eq. 7) THEN
                 READ (IN,*,END=400,ERR=400) FFCYC
                 READ (IN,*,END=400,ERR=400) FZCYC
              ELSE
                 READ (IN,*,END=400,ERR=400) FZCYC
              END IF
 
              READ (IN,*,END=400,ERR=400) OUTCYC
 
           END IF
C
C               PROCESS DATA FOR DUAL LOOP CIRCUIT
C
           N = N + 1
      END DO
C
C          ZERO CONDENSER HEAT LOADS TO CABINET AND EVAPORATORS
C
      Q_CND_FF = 0
      Q_CND_FZ = 0
      Q_HXS_FF = 0
      Q_HXS_FZ = 0
      CONDF_IN_WALL = 0
      CONDZ_IN_WALL = 0
C
C          READ CABINET LOADS DATA (IF PRESENT)
C
      ICAB = 0
      CALL READLN(2,IN,2,IOCHECK)
      IF(IOCHECK .EQ. 0) THEN
           ICAB = 1
           READ (IN,*,END=400,ERR=400) FFASH
           READ (IN,*,END=400,ERR=400) FFAUX
           READ (IN,*,END=400,ERR=400) FZASH
           READ (IN,*,END=400,ERR=400) FZAUX
           READ (IN,*,END=400,ERR=400) OTHERW
 
           READ (IN,*,END=400,ERR=400) TROOM
           READ (IN,*,END=400,ERR=400) FFTEMP
           READ (IN,*,END=400,ERR=400) FZTEMP
 
           READ (IN,*,END=400,ERR=400) FFQ
           READ (IN,*,END=400,ERR=400) FZQOFF
 
           READ (IN,*,END=400,ERR=400) FFSEN
           READ (IN,*,END=400,ERR=400) FFLAT
           READ (IN,*,END=400,ERR=400) FROSTF
 
           READ (IN,*,END=400,ERR=400) FZSEN
           READ (IN,*,END=400,ERR=400) FZLAT
           READ (IN,*,END=400,ERR=400) FROSTZ
 
           READ (IN,*,END=400,ERR=400) FFPENA
           READ (IN,*,END=400,ERR=400) FZPENA
 
           READ (IN,*,END=400,ERR=400) FFHTQ
           READ (IN,*,END=400,ERR=400) FZHTQ
 
           READ (IN,*,END=400,ERR=400) FFREFQ
           READ (IN,*,END=400,ERR=400) FZREFQ
 
           READ (IN,*,END=400,ERR=400) QMUL
C
C          CHANGE UNITS ON REFRIGERATION LOAD DATA
C
           TROOM  = 1.8*TROOM  + 32.0
           FFTEMP = 1.8*FFTEMP + 32.0
           FZTEMP = 1.8*FZTEMP + 32.0
 
           FFQ = FFQ*3.413
           FZQOFF = FZQOFF*3.413
           FZQON = FZQOFF
           FZQ   = FZQOFF
 
           FFSEN = FFSEN*3.413
           FFLAT = FFLAT*3.413
           FROSTF = FROSTF*3.413
           FROSTFS = FROSTF
 
           FZSEN = FZSEN*3.413
           FZLAT = FZLAT*3.413
           FROSTZ = FROSTZ*3.413
           FROSTZS = FROSTZ
 
           FFHTQ = FFHTQ*3.413
           FZHTQ = FZHTQ*3.413
 
           FFPENA = FFPENA*3.413
           FZPENA = FZPENA*3.413
 
           QMUL = QMUL*3.413
 
           FFREFQ = FFREFQ*3.413
           FZREFQ = FZREFQ*3.413
 
           CONDF = FFQ - FFSEN - FFLAT - FFHTQ - FROSTF - FFREFQ
     .                 - FFPENA
           CONDZ = FZQ - FZSEN - FZLAT - FZHTQ - FROSTZ - FZREFQ
     .                 - FZPENA
      END IF
      CLOSE(IN)
C
C          NULL OUT THE ERROR FILE
C
           CALL BACKUP (FILDAT)
C
C          LOOP OVER THE NUMBER OF REFRIGERATION CIRCUITS
C
      FF_AIR = 0
      N = 1
      DO WHILE (N .LE. NDATA)
C
C               CONVERT UNITS
C
           TS1(N)   = TS1(N) + 273.11
           TS3(N)   = TS3(N) + 273.11
           TS5      = TS5    + 273.11
 
           RHOCPC   = 316.8/TS1(N)
           RHOCPE   = 316.8/TS3(N)
           RHOCPF   = 316.8/TS5
 
           CFMC     = 1.8961*(RHOCPC*CFMCI(N))/0.4720
           CFME     = 1.8961*(RHOCPE*CFMEI(N))/0.4720
           CFMF     = 1.8961*(RHOCPF*CFMF)/0.4720
 
           IF(IFREZI(N) .EQ. 0) THEN
                !Nothing - already m2
           ELSE
                UAF = 3.600*UAF
           END IF
 
           UTPE     = UTPEI(N)*3.600
           USUPE    = USUPEI(N)*3.600
           ATOTE    = ATOTEI(N)
 
           DTSUPE   = DTSPEI(N)
 
           UDSC     = UDSCI(N)*3.600
           UTPC     = UTPCI(N)*3.600
           USCC     = USCCI(N)*3.600
           ATOTC    = ATOTCI(N)
 
           DTSUBC = DTSBCI(N)
 
           TSPEC = TSPECI(N)
           IF(TSPECI(N) .GT. 0.0) TSPEC = TSPECI(N) + 273.11
 
           CEI(N)   = CEI(N)/100.0
           SEFFI(N) = SEFFI(N)/100.0
           MEFF(N)  = MEFF(N)/100.0
           QCAN(N)  = QCAN(N)/100.0
           QHILO(N) = QHILO(N)/100.0
 
           SIZE = SIZEN(N)/0.252
           EER = EERN(N)
           ICOOL = ICOOLN(N)
C
C               FILL THE COMMON BLOCK FOR REFRIGERATION LOOP N
C
           ICOND = ICONDI(N)
           IFRSH = IFRSHI(N)
           IFREZ = IFREZI(N)
 
           MREF  = MREFI(N)*2.20462
           SPEEDN = SPDNOM(N)
           SPEED  = SPEEDI(N)
           IF(IMAP .EQ. 1) SPEED = SPEEDN*SPEEDI(N)
           DISPLC(N) = DISPLC(N)/16.3871
           CE    = CEI(N)
 
           SEFF  = SEFFI(N)
           FANE  = FNPWRE(N)
           FANC  = FNPWRC(N)
           DUTYC = 0.5
 
           IRFTPL = IRFTYP
           IFAN = 0
C
C               COMPRESSOR MAP FILE
C
           IF(IMAP .EQ. 0) THEN
              filmap = filmap1
              IF(n .EQ. 1 .AND. icycl .EQ. 3) filmap = filmap2
 
              CALL DOSFUN(1,filmap,'COMPMAP.DAT ',DOSCHR)
              CALL DOSCAL(.FALSE., .TRUE., .FALSE.,.FALSE.,DOSCHR)
              CALL CURSOR(1)
 
              OPEN(IN, FILE='COMPMAP.DAT', STATUS='OLD')
 
              FOUND_DATA = .FALSE.
              READ(IN, *, IOSTAT=IOCHECK)
 
              DO WHILE(.NOT. FOUND_DATA)
              READ(IN, *, IOSTAT=IOCHECK) MREF
                 IF(IOCHECK .NE. 0) CYCLE
                 FOUND_DATA = .TRUE.
                 READ(IN, *)                     !NEVAP
                 READ(IN, *)                     !NCOND
                 READ(IN, *)                     !ICOMP
                 READ(IN, *) IUNITS
                 IF(IUNITS .NE. 1) MREF = 2.20462*MREF
             END DO
             CLOSE (IN)
             MREF = 2.20462*MREFI(N)                     !!!6-10-94
 
           END IF
C
C               CYCLE SOLUTION CONTROL INDICES:
C
C               INCTRL: 0 = none, 1 = adjust evaporator areas,
C                       2 = adjust fresh food section tempeature,
C                       3 = adjust freezer    section tempeature,
C                       4 = switching valve (only one section is cooled
C                                            at a time)
C                       5 = solenoid valve or fan control provides
C                           evaporator capacity to only one cabinet
C                           during part of the cycle
 
C               ITYPE:  1 = standard cycle or dual-loop cycle;
C                           also used in switching valve cycle
C                       2 = LORENZ cycle
C                       3 = 2nd call to CYCLE in switching valve cycle
 
C               IFAN:   0 = all cycles without switching valves
C                       1 = switching valve cycle
C                       2 = dual evaporator or LORENZ cycle with fan
C                           or valve control (INCTRL = 5).
C
C               Strategy for Control Option 4: Redefine the cabinet as
C                       a freezer (one evaporator), which uses load QFZ,
C                       which is set equal to QFZOFF in DUTFND.  Set up
C                       inputs twice for a call to a freezeer analysis.
C
C               Strategy for Control Option 5: Solve first as an
C                       uncontrolled Lorenz cycle, and then call
C                       again for a cycle analysis for the cabinet with
C                       the highest duty cycle.  The second analysis
C                       uses the freezer model (IRFTYP=4), with the
C                       appropriate variables for the load and evaporator
C
C                       Option 5 requires a manual defrost R/F, with no
C                       cyclic dependent heater powers.  Fans may be
C                       used in the system.  Refrigerant line anti-sweat
C                       heat may be used, but it is assume (for simplicity)
C                       that all the heat is provided during the time
C                       when refrigerant flows through both evaporators.
C
C               CALL THE REFRIGERATION CYCLE ANALYSIS
C
           IF(ICYCL .EQ. 2) THEN
C
C                    LORENZ CYCLE ANALYSIS
C
                ITYPE = 2
 
                FZQOFF_S = FZQOFF
                FROSTZ_S = FROSTZ
                DFSTCYC_S = DFSTCYC
 
                IF(INCTRL .EQ. 4) THEN
                   ITYPE = 1
                   ICYCL = 1
                   IEVAP = 1
                   IRFTPL = 4
 
                   TS3(N) = (FFTEMP + 459.6)/1.8
                   TS5 = -300.0
                   MREF = 2.20461*MREFI(2)                 !!!6-10-94
 
                   FZQOFF = FFQ - FROSTF
                   FROSTZ = 0
                   DFSTCYC = 0
                ELSE
                   IF(ICYCLS .EQ. 2) THEN
                      IEVAP = 1
                   ELSE
                      IEVAP = 2
                   END IF
                END IF
 
                IF(inctrl .EQ. 5) THEN                     !Restriction on
                   ffcyc = 0                               !  option 5
                   fzcyc = 0
                END IF
 
                ISPEC = ISPECI(N)
                XEXITE = QUALTY(N)
                DTSUPI = SUPIHX(N)
 
                CALL CYCLE (NC(N),IR(1,N),X(1,N),F(1,1,N),TS1(N),
     .               TS3(N),TS5,MEFF(N),QHILO(N),QCAN(N),
     .               DPC(N),DPE(N),DPF,ETHX1,ETHX2,DISPLC(N),N,
     .               FROSTF,FROSTZ,ICAB,IRFTPL,ICYCL,ICYCLS,IDFRST)
C
                PWRL = W/CORR_COP/1.0548
                DUTYL = DUTYC
                DUTYS = DUTYL
                FANEL = FNPWRE(1)
                FANCL = FNPWRC(1)
                QEN(1) = QE + QZ
                QE_NET = QE - Q_HXS_FF
                QZ_NET = QZ - Q_HXS_FZ
                QEL(1) = QE_NET + QZ_NET
                FLOWN(1) = FLOW
                COPRN(1) = COPR
                COPCYC(1) = CORR_COP
C
C                         CALL AGAIN AS IF A STANDARD CYCLE IF FLOW
C                         CONTROL OPTION USED
C
                ICYCL = 1
                ITYPE = 3
                TS5 = -300.0
                DPF = 0
 
                ISPEC = ISPECI(N)
                XEXITE = QUALTY(N)
                DTSUPI = SUPIHX(N)
C
C                         CALL CYCLE ANALYSIS FOR CONTROL MODES 4
C                         OR 5.
C
                IF(INCTRL .EQ. 4) THEN                     !Switching valve
                   IBLNCE = 0
                   IFAN = 1
 
                   TS3(N) = (FZTEMP + 459.6)/1.8
                   ETHX(N) = ETHX1
                   CFMEI(N) = CFMF
                   FNPWRE(N) = FANZ
                   DPE(N) = DPF
                   IFRSHI(N) = IFREZI(N)
                   ATOTEI(N) = UAF
                   UTPEI(N) = 1
 
                   MREF = 0.75*FLOW                        !Next guess for FZ
                   MREF = 2.20461*MREFI(1)                 !!!6-10-94
                   IEVAP = 2
                   CFME  = CFMF
                   IFRSH = IFREZ
                   FANE  = FANZ
                   ATOTE = UAF
                   UTPE = 1.0
                   IFANE = IFRSHI(N)
 
                   FROSTZ = FROSTZ_S
                   FZQOFF = FZQOFF_S
                   DFSTCYC = DFSTCYC_S
 
                   UA_FF = UA_FZ
                   UA_FF_HXS = UA_FZ_HXS
                   UA_FF_CND = UA_FZ_CND
 
                   Q_FF_IN_WALL_S = Q_FF_IN_WALL
                   CAPE_IN_WALL_S = CAPE_IN_WALL
                   CONDF_IN_WALL_S = CONDF_IN_WALL
                   Q_HXS_FF_S = Q_HXS_FF
 
                   DUTYS = 0
                   CALL CYCLE (NC(N),IR(1,N),X(1,N),F(1,1,N),
     .                TS1(N),TS3(N),TS5,MEFF(N),QHILO(N),
     .                 QCAN(N),DPC(N),DPE(N),DPF,ETHX(N),ETHX(N),
     .                 DISPLC(N),N,FROSTF,FROSTZ,ICAB,IRFTPL,ICYCL,
     .                 ICYCLS,IDFRST)
 
                   PWRE  = W/CORR_COP/1.0548
                   DUTYS = DUTYC
                   FANES = FNPWRE(1)
                   FANCS = FNPWRC(1)
                   QEN(2) = QE + QZ
                   QE_NET = QE - Q_HXS_FF
                   QZ_NET = QZ - Q_HXS_FZ
                   QEL(2) = QE_NET + QZ_NET
 
                   FLOWN(2) = FLOW
                   COPRN(2) = COPR
                   COPCYC(2) = CORR_COP
 
                   Q_FZ_IN_WALL = Q_FF_IN_WALL
                   Q_FF_IN_WALL = Q_FF_IN_WALL_S
 
                   CAPZ_IN_WALL = CAPE_IN_WALL
                   CAPE_IN_WALL = CAPE_IN_WALL_S
 
                   CONDZ_IN_WALL = CONDF_IN_WALL
                   CONDF_IN_WALL = CONDF_IN_WALL_S
 
                   Q_HXS_FZ = Q_HXS_FF
                   Q_HXS_FF = Q_HXS_FF_S
                END IF
 
                IF(inctrl .EQ. 5) THEN                     !Refrigerant bypass
                   ifan = 2
                   iblnce= 0
                   dutye = AMIN1(dutye,1.0)
                   dutyz = AMIN1(dutyz,1.0)
 
                   IF(ABS(dutye -dutyz) .LE. 0.01) THEN    !Loads balanced
                      inctrl = 0
                      ifan = 0
                   END IF
 
                END IF
 
                IF(inctrl .EQ. 5) THEN                     !Refrigerant bypass
                   icycl = 1
                   irftpl = 4
                   ts5 = -300.0
                   mref = 0.75*flow
                   MREF = 2.20461*MREFI(2)                 !!!6-10-94
 
                   condht(n) = 0                           !No anti-sweat heat
                   condvp(n) = 0
 
                   IF (dutye .GT. dutyz) THEN              !Call FF cycle
                      ievap = 1
                      itype = 4
 
                      cycle_heat = dutyl*(cape_in_wall - q_ff_in_wall)
     .                           + dutyl*(condf_in_wall - q_fz_ff)
     .                           + dutyl*3.413*fnpwre(1)
 
                      TS3(n) = (fftemp + 459.6)/1.8
                      ETHX(N) = ethx1
                      fzqoff = ffq + cycle_heat - qe_net*dutyz/1.0548
 
                   ELSE                                    !Call FZ cycle
                      ievap = 2
                      itype = 3
 
                      TS3(N) = (fztemp + 459.6)/1.8
                      ETHX(N) = ethx1
 
                      cycle_heat = dutyl*(capz_in_wall - q_fz_in_wall)
     .                           + dutyl*(condz_in_wall + q_fz_ff)
     .                           + dutyl*3.413*fanz
 
                      fzqoff = fzqoff + cycle_heat -qz_net*dutye/1.0548
 
                      CFMEI(N) = cfmf
                      FNPWRE(N) = fanz
                      DPE(N) = dpf
                      IFRSHI(N) = IFREZI(N)
                      ATOTEI(N) = uaf
                      UTPEI(N) = 1
 
                      cfme  = cfmf
                      ifrsh = ifrez
                      fane  = fanz
                      atote = uaf
                      utpe = 1.0
                      ifane = IFRSHI(N)
 
                      UA_FF = UA_FZ
                      UA_FF_HXS = UA_FZ_HXS
                      UA_FF_CND = UA_FZ_CND
 
                   END IF
 
                   Q_FF_IN_WALL_S = Q_FF_IN_WALL
                   CAPE_IN_WALL_S = CAPE_IN_WALL
                   CONDF_IN_WALL_S = CONDF_IN_WALL
 
                   Q_FZ_IN_WALL_S = Q_FZ_IN_WALL
                   CAPZ_IN_WALL_S = CAPZ_IN_WALL
                   CONDZ_IN_WALL_S = CONDZ_IN_WALL
 
                   Q_FZ_FF_S = Q_FZ_FF
 
                   DUTYS = 0
                   CALL CYCLE (NC(N),IR(1,N),X(1,N),F(1,1,N),
     .                TS1(N),TS3(N),TS5,MEFF(N),QHILO(N),
     .                 QCAN(N),DPC(N),DPE(N),DPF,ETHX(N),ETHX(N),
     .                 DISPLC(N),N,FROSTF,FROSTZ,ICAB,IRFTPL,ICYCL,
     .                 ICYCLS,IDFRST)
 
                   PWRE  = W/CORR_COP/1.0548
                   DUTYS = DUTYC
                   FANES = FNPWRE(1)
                   FANCS = FNPWRC(1)
                   QEN(2) = QE + QZ
                   QE_NET = QE - Q_HXS_FF
                   QZ_NET = QZ - Q_HXS_FZ
                   QEL(2) = QE_NET + QZ_NET
 
                   FLOWN(2) = FLOW
                   COPRN(2) = COPR
                   COPCYC(2) = CORR_COP
 
                   dutyn(1) = dutyl
                   dutyn(2) = dutys
 
                   IF (ievap .EQ. 1) THEN                  !Correct FF cycle
                      CONDF_IN_WALL = (CONDF_IN_WALL_S*DUTYL
     .                               + CONDF_IN_WALL*DUTYS)/DUTYN(1)
                      CONDZ_IN_WALL = CONDZ_IN_WALL_S*DUTYL/DUTYN(1)
 
                      CAPE_IN_WALL = (CAPE_IN_WALL_S*DUTYL
     .                              + CAPE_IN_WALL*DUTYS)/DUTYN(1)
                      CAPZ_IN_WALL = CAPZ_IN_WALL_S*DUTYL/DUTYN(1)
 
                      Q_FF_IN_WALL = (Q_FF_IN_WALL_S*DUTYL
     .                              + Q_FF_IN_WALL*DUTYS)/DUTYN(1)
                      Q_FZ_IN_WALL = Q_FZ_IN_WALL_S*DUTYL/DUTYN(1)
 
                      Q_FZ_FF = Q_FZ_FF_S*DUTYL/DUTYN(1)
 
                   ELSE                                    !Correct FZ cycle
                      CONDZ_IN_WALL = (CONDZ_IN_WALL_S*DUTYL
     .                               + CONDF_IN_WALL*DUTYS)/DUTYN(1)
                      CONDF_IN_WALL = CONDF_IN_WALL_S*DUTYL/DUTYN(1)
 
                      CAPZ_IN_WALL = (CAPZ_IN_WALL_S*DUTYL
     .                              + CAPE_IN_WALL*DUTYS)/DUTYN(1)
                      CAPE_IN_WALL = CAPE_IN_WALL_S*DUTYL/DUTYN(1)
 
                      Q_FZ_IN_WALL = (Q_FZ_IN_WALL_S*DUTYL
     .                              + Q_FF_IN_WALL*DUTYS)/DUTYN(1)
                      Q_FF_IN_WALL = Q_FF_IN_WALL_S*DUTYL/DUTYN(1)
 
                      Q_FZ_FF = Q_FZ_FF_S*DUTYL/DUTYN(1)
 
                   END IF
 
                END IF
 
                ICYCL = 2
           ELSE
C
C                    STANDARD CYCLE OR DUAL LOOP CYCLE
C
                ITYPE = 1
                TS5 = -300.0
                DPF = 0
                IF(ICYCL .EQ. 1) THEN
                     IEVAP = 0
                ELSE
                     IF(N .EQ. 1) THEN
                          IEVAP = 2
                     ELSE
                          IEVAP = 1
                     END IF
                END IF
 
                ISPEC = ISPECI(N)
                XEXITE = QUALTY(N)
                DTSUPI = SUPIHX(N)
 
                CALL CYCLE (NC(N),IR(1,N),X(1,N),F(1,1,N),TS1(N),
     .               TS3(N),TS5,MEFF(N),QHILO(N),QCAN(N),
     .               DPC(N),DPE(N),DPF,ETHX(N),ETHX(N),DISPLC(N),N,
     .               FROSTF,FROSTZ,ICAB,IRFTPL,ICYCL,ICYCLS,IDFRST)
           END IF
C
C               SET UP PARAMETERS FOR SUMMARY OUTPUT
C
           IF(ifan .NE. 2) THEN
              DUTYN(N) = DUTYC
              WCOMP(N) = W/CORR_COP/1.0548
           END IF
 
           IF(ICYCL .NE. 2) THEN
                 QEN(N) = QE
                 QE_NET = QE - Q_HXS_FF
                 QEL(N) = QE_NET
                 FLOWN(N) = FLOW
                 COPRN(N) = COPR
                 COPCYC(N) = CORR_COP
           END IF
           N = N + 1
      END DO
C
C          OUTPUT A SUMMARY PAGE IF CABINET LOADS ANALYSIS DONE
C
      IF(ICAB .EQ. 1) THEN
           CALL SUMMRY(TITLE,IRFTYP,ICYCLS,DUTYN,FNPWRE,FNPWRC,
     .                 ELOSS,WCOMP,IFAN,IDFRST,FROSTFS,FROSTZS,
     .                 QEL,QE_NET,QZ_NET)
      END IF
C
C          FINISH UP HOUSEKEEPING DETAILS AND EXIT
C
      CALL FINISH
C
C          HANDLE READ ERROR OR END OF FILE
C
  400 CONTINUE
      CALL GOTOXY(21,12)
      CALL PRINT('End of File or Read Error Encountered',37,-2)
      CALL GOTOXY(21,14)
      CALL PRINT('Program Execution Terminated',28,-2)
      CALL WAIT(3)
      CALL FINISH
      END
C
      SUBROUTINE READLN(LOC,IN,NUM,IOCHECK)
C     *****************************************************************
C     *    READ NUM LINES OF DATA.  RETURN ERROR CODE IOCHECK         *
C     *****************************************************************
C
      CHARACTER ADUM
      N = 1
      DO WHILE (N .LE. NUM)
           READ(IN,'(A1)',IOSTAT=IOCHECK) ADUM
           IF(IOCHECK .EQ. -1) THEN
                IF(LOC .EQ. 2) RETURN
                CALL GOTOXY(13,12)
                CALL PRINT('End of File Encountered - Program ',34,-2)
                CALL PRINT('Execution Terminated',20,-2)
                CALL WAIT(3)
                CALL FINISH
           END IF
           N = N + 1
      END DO
      RETURN
      END
C
      SUBROUTINE FINISH
C     *****************************************************************
C     *    RETURN TO DOS AFTER CLEARING THE SCREEN                    *
C     *****************************************************************
C
      character key
      CALL GETCOL('MONO_SCREEN=YES$','SETUP.DAT ',IRET)
      IRET = 1 - IRET
      IF(IRET .EQ. 1) THEN
           CALL SETATR(1)
      END IF
      call inchr(3, j, key)
      CALL GOTOXY(0,0)
      CALL SCREEN(0)
      CALL GOTOXY(0,0)
      CALL CURSOR(0)
      STOP ' '
      END
C
      SUBROUTINE SETPIC
C     *****************************************************************
C     *    SET UP THE SCREEN TO INDICATE CYCLE ANALYSIS AND SET UP    *
C     *    FOR COLOR IF COLOR MONITOR PRESENT                         *
C     *****************************************************************
C
      INTEGER*2 VSET(300)
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
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
      CALL CURSOR(-99)
      CALL GETCOL('MONO_SCREEN=YES$','SETUP.DAT ',IRET)
      IRET = 1 - IRET
      ICOLR = IRET
      IF(IRET .EQ. 1) CALL TRAP
      IF(IRET .EQ. 1) CALL SETATR(0)
      CALL GOTOXY(0,0)
      WRITE(*,'(1X\)')
      CALL SCREEN(0)
      CALL PICTRE(60,VSET)
      CALL GOTOXY(37,0)
      IF(IRET .EQ. 1) CALL ATRBUT(5,79)
 
      CALL GOTOXY(28,12)
      CALL PRINT('SETTING UP CYCLE ANALYSIS',25,-2)
      CALL WAIT(1)
      RETURN
      END
C
      SUBROUTINE SUMMRY(TITLE,IRFTYP,ICYCL,DUTYN,FNPWRE,FNPWRC,
     .                  ELOSS,WCOMP,IFAN,IDFRST,FROSTF,FROSTZ,
     .                  QEL,QE_NET,QZ_NET)
C     *****************************************************************
C     *    OUTPUT A SUMMARY OF THE RESULTS FOR THE ENTIRE ANALYSIS    *
C     *****************************************************************
C
      CHARACTER TITLE
      CHARACTER*2 CHOUR,CMIN,CSEC,CDAY,CMONN,CYEAR
      CHARACTER*3 CMON
      CHARACTER*6 HREF(34),REFH(34)
      CHARACTER*13 FILERA
      CHARACTER*13    filmap, filmap1, filmap2
 
      DIMENSION FNPWRE(2),FNPWRC(2),ELOSS(2),DUTYN(2),WCOMP(2),QEL(2)
      DIMENSION TITLE(68,5)
 
      COMMON/PARMS2/TSPEC,I_LIQUID_LINE
      COMMON/CABLOD/FFASH,FFAUX,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
     .              FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
     .              FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
      COMMON/FANS/FANE,FANZ,FANC,DUTYC,W,COPR
      COMMON/LORENZ/DUTYE,DUTYZ,PWRL,PWRE,CAPE,CAPZ,DUTYL,DUTYS,
     .              FANEL,FANCL,FANES,FANCS
      COMMON / TIME / CHOUR,CMIN,CSEC,CDAY,CMON,CMONN,IYEAR,CYEAR
      COMMON / RESULT / QE, QZ, FLOW, QEN(2), FLOWN(2), COPRN(2)
      COMMON / CYCLNG / CORR_COP, COPCYC(2), I_CYCLE, I_VALVE, T_CYCLE
      COMMON /HREF1/ HREF,REFH
      COMMON /REFRIG/ NC(2), IR(5,2), X(5,2), F(5,5,2)
      COMMON / FILINF / FILERA
      COMMON /FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
      COMMON / BALNCE / IBLNCE, BAFFLF, BAFFLZ, AREAFZ, ATOTE_S,
     .                  AREAFZ_S, ATOTE_A, AREAFZ_A, FFTEMP_A, FZTEMP_A
 
      COMMON /FIGURE / IEVAP
      COMMON / CHINA / INCTRL, HRSOFF
      COMMON / INWALL / UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
     .                  Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
     .                  CAPZ_IN_WALL, Q_FZ_FF
      COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
     .                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
     .                  CONDF_IN_WALL, CONDZ_IN_WALL
      COMMON / LIQLIN / FFREFQ, FZREFQ, CONDHT(2), CONDVP(2)
      COMMON / CYCLIC / DFSTCYC, FFCYC, FZCYC, OUTCYC
      COMMON / MAPNAM / filmap, filmap1, filmap2
      COMMON / MAPDAT / IMAP, ICOMP, ICOOL, EER, SIZE, DISPL, EFFC,
     .                  SPEEDN, IREAD
      COMMON / PENAT / FFPENA, FZPENA
C
      DATA IO /8/
C
C          OPEN OUTFILE FILE AND WRITE THE TITLE
C
      OPEN (IO,FILE='ERA.OUT',STATUS='UNKNOWN')
      IYEAR = IYEAR - 1900
      IF (IYEAR .GE. 100) IYEAR = IYEAR - 100
      WRITE(IO,810) FILERA, CMONN, CDAY, CYEAR, CHOUR, CMIN
      WRITE(IO,800) TITLE
C
C          OUTPUT INFORMATION ON TYPE OF CABINET AND CYCLE
C
      SELECT CASE (IRFTYP)
         CASE (1)
           WRITE(IO,'(''TWO-DOOR TOP-MOUNT REFRIGERATOR/FREEZER''/)')
 
         CASE (2)
           WRITE(IO,'(''TWO-DOOR BOTTOM-MOUNT BEFRIGERATOR/FREEZER''/)')
 
         CASE (3)
           WRITE(IO,'(''SIDE-BY-SIDE REFRIGERATOR/FREEZER''/)')
 
         CASE (4)
           WRITE(IO,'(''CHEST FREEZER''/)')
 
         CASE (5)
            WRITE(IO,'(''UPRIGHT FREEZER''/)')
 
         CASE (6)
            WRITE(IO,'(''ONE-DOOR REFRIGERATOR''/)')
 
         CASE (7)
            WRITE(IO,'(''ONE-DOOR REFRIGERATOR/FREEZER''/)')
 
      END SELECT
C
      SELECT CASE (ICYCL)
           CASE (1)
                WRITE(IO,'(''STANDARD SINGLE EVAPORATOR CYCLE''/)')
           CASE (2)
                WRITE(IO,'(''LORENZ CYCLE''/)')
           CASE (3)
                WRITE(IO,'(''DUAL LOOP CYCLE''/)')
           CASE (4)
                WRITE(IO,'(''DUAL EVAP CYCLE''/)')
      END SELECT
C
C          COMPRESSOR MAP USED
C
      IF(IMAP .EQ. 0) THEN
         IF(ICYCL .NE. 3) THEN
            WRITE(IO,'(''COMPRESSOR MAP FILE: '', A13/)') filmap1
         ELSE
            WRITE(IO,'(''FRESH FOOD COMPRESSOR MAP FILE: '', A13)')
     .                                                       filmap1
            WRITE(IO,'(''FREEZER    COMPRESSOR MAP FILE: '', A13/)')
     .                                                       filmap2
         END IF
 
      END IF
C
C          HANDLE DUAL EVAP CYCLE
C
      ICYCLS = ICYCL
      IF(ICYCL .EQ. 4) ICYCL = 2
C
C          REFRIGERANT DATA
C
      MAXLIN = NC(1)
      IF(ICYCL .EQ. 3 .AND. NC(2) .GT. NC(1)) MAXLIN = NC(2)
      IF(IRFTYP .LE. 3 .OR. IRFTYP .EQ. 7) THEN
         WRITE(IO,811)
      ELSE
         WRITE(IO,814)
      END IF
      SELECT CASE (ICYCL)
           CASE(1,2)
                X2 = X(1,1)
                IF(MAXLIN .EQ. 1) THEN
                   IF(IRFTYP .LE. 3 .OR. IRFTYP .EQ. 7) THEN
                      WRITE(IO,812) HREF(IR(1,1)),HREF(IR(1,1))
                   ELSE
                      WRITE(IO,812) HREF(IR(1,1))
                   END IF
                ELSE
                   IF(IRFTYP .LE. 3 .OR. IRFTYP .EQ. 7) THEN
                      WRITE(IO,813) X2,HREF(IR(1,1)),X2,HREF(IR(1,1))
                   ELSE
                      WRITE(IO,813) X2,HREF(IR(1,1))
                   END IF
                   I = 2
                   DO WHILE (I .LE. MAXLIN)
                      X2 = X(I,1)
                      IF(IRFTYP .LE. 3 .OR. IRFTYP .EQ. 7) THEN
                         WRITE(IO,813) X2,HREF(IR(I,1)),
     .                                 X2,HREF(IR(I,1))
                      ELSE
                         WRITE(IO,813) X2,HREF(IR(I,1))
                      END IF
                      I = I + 1
                   END DO
                END IF
 
           CASE(3)
                WRITE(IO,812) HREF(IR(1,2)),HREF(IR(1,1))
 
      END SELECT
 
C
C          CABINET LOADS SUMMARY
C
      FFTEMP = (FFTEMP - 32.0)/1.8
      FFTEMP_A = (FFTEMP_A - 32.0)/1.8
      FZTEMP = (FZTEMP - 32.0)/1.8
      FZTEMP_A = (FZTEMP_A - 32.0)/1.8
      SELECT CASE (IRFTYP)
C
C               REFRIGERATOR - FREEZERS
C
           CASE (1:3, 7)
                IF(FFTEMP .NE. FFTEMP_A .OR. FZTEMP .NE. FZTEMP_A) THEN
                   WRITE(IO,822) FFTEMP,   FZTEMP
                   WRITE(IO,823) FFTEMP_A, FZTEMP_A
                ELSE
                   WRITE(IO,818) FFTEMP, FZTEMP
                END IF
C
C          HANDLE THE CASE WHERE THE AREAS WERE ADJUSTED
C
                IF(AREAFZ .NE. AREAFZ_S) THEN
                   WRITE(IO,820) ATOTE_S, AREAFZ_S
                   WRITE(IO,821) ATOTE_A, AREAFZ_A
                END IF
 
                DOORFF = FFSEN + FFLAT
                DOORFZ = FZSEN + FZLAT
 
C
C          CORRECTION TO CABINET LOADS DUE TO IN-WALL EVAPORATOR
C
                WALL_FF = DUTYN(1)*(CAPE_IN_WALL - Q_FF_IN_WALL)
                WALL_ML = DUTYN(1)*Q_FZ_FF
                WALL_FZ = DUTYN(1)*(CAPZ_IN_WALL - Q_FZ_IN_WALL)
 
                WALL_CND_FF = DUTYN(1)*CONDF_IN_WALL
                WALL_CND_FZ = DUTYN(1)*CONDZ_IN_WALL
 
                IF(ICYCL .EQ. 2 .AND. INCTRL .EQ. 4) THEN
                   WALL_FF = DUTYL*(CAPE_IN_WALL - Q_FF_IN_WALL)
                   WALL_ML = 0
                   WALL_FZ = DUTYS*(CAPZ_IN_WALL - Q_FZ_IN_WALL)
 
                   WALL_CND_FF = DUTYL*CONDF_IN_WALL
                   WALL_CND_FZ = DUTYS*CONDZ_IN_WALL
                END IF
C
C          FAN POWERS
C
                IF(ICYCL .EQ. 1) THEN
                     FFFAN = 3.413*DUTYN(1)*FNPWRE(1)*FF_AIR
                     FZFAN = 3.413*DUTYN(1)*FNPWRE(1) *(1.0 - FF_AIR)
 
                     DFSTCYC = 3.413*DFSTCYC*DUTYN(1)
                     FFHTQ = FFHTQ + 3.413*FFCYC*DUTYN(1)
                     FZHTQ = FZHTQ + 3.413*FZCYC*DUTYN(1)
                END IF
 
                IF(ICYCL .EQ. 2) THEN
                     FFFAN = 3.413*DUTYL*FANEL
                     FZFAN = 3.413*DUTYS*FANZ
 
                     DFSTCYC = 3.413*DFSTCYC*DUTYS
                     FFHTQ = FFHTQ + 3.413*FFCYC*DUTYL
                     FZHTQ = FZHTQ + 3.413*FZCYC*DUTYS
                END IF
 
                IF(IFAN .EQ. 2) THEN
                     DUTYC = AMAX1(DUTYE,DUTYZ)
                     IF(IEVAP .EQ. 1) then                 !FF cycle
                        fffan = 3.413*fane*dutyl + fanes*dutys
                        fzfan = 3.413*fanz*dutyl
                     ELSE
                        fffan = 3.413*fanel*dutyl
                        fzfan = 3.413*fanz*dutyl + 3.413*fanes*dutys
                     END IF
                END IF
 
                IF(ICYCL .EQ. 3) THEN
                     FZFAN = 3.413*DUTYN(1)*FNPWRE(1)
                     FFFAN = 3.413*DUTYN(2)*FNPWRE(2)
 
                     DFSTCYC = 3.413*DFSTCYC*DUTYN(1)
                     FZHTQ = FZHTQ + 3.413*FZCYC*DUTYN(1)
                     FFHTQ = FFHTQ + 3.413*FFCYC*DUTYN(2)
                END IF
 
                IF(ICYCL .NE. 1) FROSTF = 0
 
                FFQ = CONDF + DOORFF + FFHTQ  + WALL_CND_FF + FFREFQ
     .                      + FFFAN  + FROSTF + WALL_FF - WALL_ML
     .                      + FFPENA
 
                FZQ = CONDZ + DOORFZ + FZHTQ  + WALL_CND_FZ + FZREFQ
     .                      + FZFAN  + FROSTZ + WALL_FZ + WALL_ML
     .                      + FZPENA
C
C                    OUTPUT THE LOADS
C
                CONDT  = CONDF  + CONDZ
                DOORT  = DOORFF + DOORFZ
                TPENA  = FFPENA + FZPENA
                THTQ   = FFHTQ  + FZHTQ
                TREFQ  = FFREFQ + FZREFQ
                TFAN   = FFFAN  + FZFAN
                FROSTT = FROSTF + FROSTZ
                TQ     = FFQ    + FZQ
 
                DFRSTF = FROSTF/3.413
                DFRSTZ = FROSTZ/3.413
 
                IF(IDFRST .EQ. 1) THEN
                   DFRSTF = 0
                   DFRSTZ = 0
                ELSE
                   FFQ = FFQ + FROSTF
                   FZQ = FZQ + FROSTZ
                   TQ  = TQ  + FROSTF + FROSTZ
                END IF
 
                DFRSTZ = DFRSTZ + DFSTCYC/3.413
                DFRSTT = DFRSTF + DFRSTZ
                FZQ = FZQ + DFSTCYC
                TQ  = TQ  + DFSTCYC
 
                WRITE(IO,801) CONDF/3.413,  CONDZ/3.413,  CONDT/3.413,
     .                        DOORFF/3.413, DOORFZ/3.413, DOORT/3.413,
     .                        FROSTF/3.413, FROSTZ/3.413, FROSTT/3.413,
     .                        DFRSTF,       DFRSTZ,       DFRSTT,
     .                        FFPENA/3.413, FZPENA/3.413, TPENA/3.413,
     .                        FFHTQ/3.413,  FZHTQ/3.413,  THTQ/3.413,
     .                        FFFAN/3.413,  FZFAN/3.413,  TFAN/3.413,
     .                        FFREFQ/3.413, FZREFQ/3.413, TREFQ/3.413,
     .                        WALL_FF/3.413, WALL_FZ/3.413,
     .                                       (WALL_FF + WALL_FZ)/3.413,
     .                       -WALL_ML/3.413, WALL_ML/3.413, 0.0,
     .                        WALL_CND_FF/3.413, WALL_CND_FZ/3.413,
     .                               (WALL_CND_FF + WALL_CND_FZ)/3.413,
     .                        FFQ/3.413,    FZQ/3.413,    TQ/3.413
 
                IF(IBLNCE .EQ. 1) WRITE(IO,819)
C
C                    FREEZERS
C
           CASE DEFAULT
                IF (IRFTYP .EQ. 6) THEN
                   WRITE(IO, 828) FFTEMP, FZTEMP
                ELSE
                   WRITE(IO,818) FZTEMP
                END IF
 
                WALL_FF = DUTYN(1)*(CAPE_IN_WALL - Q_FF_IN_WALL)
                WALL_CND_FF = DUTYN(1)*CONDF_IN_WALL
                DOORFZ = FZSEN + FZLAT
                FZQ = FZQOFF
                CONDZ = FZQ - DOORFZ - FZHTQ - FROSTZ - FZREFQ - FZPENA
 
                FZFAN = 3.413*DUTYN(1)*FNPWRE(1)
                FZHTQ = FZHTQ + 3.413*DUTYN(1)*FZCYC
                DFSTCYC = 3.413*DUTYN(1)*DFSTCYC
 
                FZQ = FZQ + FZFAN + WALL_FF + WALL_CND_FF
     .                    + 3.413*DUTYN(1)*FZCYC
 
                DFRSTZ = FROSTZ/3.413
                IF (IDFRST .EQ. 1) THEN
                   DFRSTZ = 0
                ELSE
                   FZQ = FZQ + FROSTZ
                END IF
 
                DFRSTZ = DFRSTZ + DFSTCYC/3.413
                FZQ = FZQ + DFSTCYC
C
C                    OUTPUT THE LOADS
C
                WRITE(IO,802) CONDZ/3.413,  DOORFZ/3.413, FROSTZ/3.413,
     .                        DFRSTZ, FZPENA/3.413, FZHTQ/3.413,
     .                        FZFAN/3.413, FZREFQ/3.413, WALL_FF/3.413,
     .                        WALL_CND_FF/3.413, FZQ/3.413
C
      END SELECT
C
C          OUTPUT ERROR MESSAGE ABOUT LIQUID LINE ANTI-SWEAT
C
      IF(I_LIQUID_LINE .NE. 0) THEN
         WRITE(IO,824)
      END IF
C
C          OUTPUT MESSAGE ABOUT EVAPORATOR BYPASS
C
      IF(IFAN .EQ. 2 .AND. IEVAP .EQ. 1) WRITE (IO,825)
      IF(IFAN .EQ. 2. AND. IEVAP .EQ. 2) WRITE (IO,826)
C
C          OUTPUT THE CYCLE ENERGIES
C
      SELECT CASE (IRFTYP)
C
C               REFRIGERATOR - FREEZERS
C
           CASE (1, 2, 3, 7)
C
C                         STANDARD CYCLE
C
                IF(ICYCL .EQ. 1) THEN
                     POWERF = 0.024*(DUTYN(1)*WCOMP(1)/3.413)
                     FANEF  = 0.024*(DUTYN(1)*(FNPWRE(1) + FNPWRC(1)))
                     ELECF  = 0.024*(DUTYN(1)*ELOSS(1))
                     HTCNEF =  FFASH + FFAUX
     .                      +  FZASH + FZAUX + OTHERW
     .                      +  DUTYN(1)*(FFCYC + FZCYC + OUTCYC)
                     HTCNEF = 0.024*HTCNEF
                     IF(IDFRST .EQ. 1) THEN
                        DFRSTF = 0.0
                     ELSE
                        DFRSTF = 0.024*((FROSTF + FROSTZ)/3.413)
     .                         + 0.024*DFSTCYC/3.413
                     END IF
                     TOTEF  = POWERF + FANEF + ELECF + HTCNEF + DFRSTF
C
C                         OUTPUT THE ELECTRICAL BUDGET
C
                    WRITE(IO,804) QEN(1)/3.6, QEL(1)/3.6,
     .                            FLOWN(1)/2.2046, COPRN(1),
     .                            COPRN(1)*COPCYC(1), DUTYN(1),
     .                            POWERF, FANEF, ELECF, HTCNEF,
     .                            DFRSTF, TOTEF
                END IF
C
C                         LORENZ CYCLE
C
                IF(ICYCL .EQ. 2 .AND. IFAN .NE. 2) THEN
                     POWERF = 0.024*DUTYL*PWRL/3.413
                     FANEF  = 0.024*DUTYL*(FANEL + FANCL)
                     ELECF  = 0.024*DUTYL*ELOSS(1)
                     HTCNEF = FFASH + FFAUX + OTHERW/2.0 + DUTYL*FFCYC
 
                     POWERZ = 0.024*DUTYS*PWRE/3.413
                     FANEZ  = 0.024*DUTYS*(FANES + FANCS)
                     ELECZ  = 0.024*DUTYS*ELOSS(1)
                     HTCNEZ = FZASH + FZAUX + OTHERW/2.0 + DUTYL*FZCYC
 
                     IF(IDFRST .EQ. 1) THEN
                        DFRSTZ = 0.0
                     ELSE
                        DFRSTZ = 0.024*(FROSTZ + DFSTCYC)/3.413
                     END IF
 
                     IF(INCTRL .LT. 4) THEN
                        HTCNEF = HTCNEF + HTCNEZ + DUTYL*OUTCYC
                        FANEF = FANEF + 0.024*DUTYL*FANZ
                        DFRSTF = DFRSTZ
                     END IF
 
                     HTCNEF = 0.024*HTCNEF
                     HTCNEZ = 0.024*HTCNEZ
 
                     TOTEF  = POWERF + FANEF + ELECF + HTCNEF + DFRSTF
                     TOTEZ  = POWERZ + FANEZ + ELECZ + HTCNEZ + DFRSTZ
C
C                         SUM THE ELECTRICAL BUDGET (SWITCHING VALVE CONTROL)
C
                     DUTYT  = DUTYL  + DUTYS
                     POWERT = POWERF + POWERZ
                     FANET  = FANEF  + FANEZ
                     ELECT  = ELECF  + ELECZ
                     HTCNET = HTCNEF + HTCNEZ
                     DFRSTT = DFRSTF + DFRSTZ
                     TOTET  = TOTEF  + TOTEZ
 
                     IF(IFAN .EQ. 0) THEN
                        WRITE(IO,804) QEN(1)/3.6, QEL(1)/3.6,
     .                                FLOWN(1)/2.2046, COPRN(1),
     .                                COPRN(1)*COPCYC(1), DUTYN(1),
     .                                POWERF, FANEF, ELECF, HTCNEF,
     .                                DFRSTF, TOTEF
                     END IF
 
                     IF(IFAN .EQ. 1) THEN
                        IF (DUTYT .GT. 1.0) WRITE(IO, 827)
                        DUTMAX = AMAX1(DUTYL, DUTYS)
                        HTCNEF = HTCNEF + 0.024*0.5*DUTMAX*OUTCYC
                        HTCNEZ = HTCNEZ + 0.024*0.5*DUTMAX*OUTCYC
                        HTCNET = HTCNET + 0.024*1.0*DUTMAX*OUTCYC
                        TOTEF = TOTEF + 0.024*0.5*DUTMAX*OUTCYC
                        TOTEZ = TOTEZ + 0.024*0.5*DUTMAX*OUTCYC
                        TOTET = TOTEF + TOTEZ
 
                        WRITE(IO,805) QEN(1)/3.6,   QEN(2)/3.6,
     .                                QEL(1)/3.6,   QEL(2)/3.6,
     .                                FLOWN(1)/2.2046,FLOWN(2)/2.2046,
     .                                COPRN(1), COPRN(2),
     .                                COPRN(1)*COPCYC(1),
     .                                COPRN(2)*COPCYC(2),
     .                                DUTYL,    DUTYS,  DUTYT,
     .                                POWERF,   POWERZ, POWERT,
     .                                FANEF,    FANEZ,  FANET,
     .                                ELECF,    ELECZ,  ELECT,
     .                                HTCNEF,   HTCNEZ, HTCNET,
     .                                DFRSTF,   DFRSTZ, DFRSTT,
     .                                TOTEF,    TOTEZ,  TOTET
                     END IF
 
                END IF
C
C                         REFRIGERANT BYPASS CYCLE
C
                IF(IFAN .EQ. 2) THEN
                     POWERF = 0.024*DUTYL*PWRL/3.413
                     FANEF  = 0.024*DUTYL*(FANEL + FANCL + FANZ)
                     ELECF  = 0.024*DUTYL*ELOSS(1)
 
                     POWERZ = 0.024*DUTYS*PWRE/3.413
                     FANEZ  = 0.024*DUTYS*(FANES + FANCS)
                     ELECZ  = 0.024*DUTYS*ELOSS(1)
 
                     HTCNEF = FFASH + FFAUX + OTHERW + DUTYC*OUTCYC
     .                      + FZASH + FZAUX
 
                     DFRSTF = 0.0
                     DFRSTZ = 0.0
 
                     HTCNEF = 0.024*HTCNEF
                     HTCNEZ = 0
 
                     TOTEF  = POWERF + FANEF + ELECF + HTCNEF + DFRSTF
                     TOTEZ  = POWERZ + FANEZ + ELECZ + HTCNEZ + DFRSTZ
C
C                         SUM THE ELECTRICAL REFRIGERANT BYPASS CONTROL)
C
                     DUTYT  = DUTYL  + DUTYS
                     POWERT = POWERF + POWERZ
                     FANET  = FANEF  + FANEZ
                     ELECT  = ELECF  + ELECZ
                     HTCNET = HTCNEF + HTCNEZ
                     DFRSTT = DFRSTF + DFRSTZ
                     TOTET  = TOTEF  + TOTEZ
C
                     IF(ICYCLS .EQ. 2 .AND. IEVAP .EQ. 1)
     .                  WRITE(IO,808)
 
                     IF(ICYCLS .EQ. 2 .AND. IEVAP .EQ. 2)
     .                  WRITE(IO,806)
 
                     IF(ICYCLS .EQ. 4 .AND. IEVAP .EQ. 1)
     .                  WRITE(IO,809)
 
                     IF(ICYCLS .EQ. 4 .AND. IEVAP .EQ. 2)
     .                  WRITE(IO,807)
 
                     IF (DUTYT .GT. 1.0) WRITE(IO, 827)
 
                     WRITE(IO,815) QEN(1)/3.6,   QEN(2)/3.6,
     .                             QEL(1)/3.6,   QEL(2)/3.6,
     .                             FLOWN(1)/2.2046,FLOWN(2)/2.2046,
     .                             COPRN(1), COPRN(2),
     .                             COPRN(1)*COPCYC(1),
     .                             COPRN(2)*COPCYC(2),
     .                             DUTYL,    DUTYS,  DUTYT,
     .                             POWERF,   POWERZ, POWERT,
     .                             FANEF,    FANEZ,  FANET,
     .                             ELECF,    ELECZ,  ELECT,
     .                             HTCNEF,   HTCNEZ, HTCNET,
     .                             DFRSTF,   DFRSTZ, DFRSTT,
     .                             TOTEF,    TOTEZ,  TOTET
                END IF
 
C                         DUAL LOOP CYCLE
C
                IF(ICYCL .EQ. 3) THEN
                     POWERZ = 0.024*(DUTYN(1)*WCOMP(1)/3.413)
                     FANEZ  = 0.024*(DUTYN(1)*(FNPWRE(1) + FNPWRC(1)))
                     ELECZ  = 0.024*(DUTYN(1)*ELOSS(1))
 
                     HTCNEZ = FZASH + FZAUX + DUTYN(1)*FZCYC + OTHERW/2
                     IF(DUTYN(1) .GE. DUTYN(2)) THEN
                        HTCNEZ = HTCNEZ + 0.5*DUTYN(1)*OUTCYC
                     ELSE
                        HTCNEZ = HTCNEZ + 0.5*DUTYN(2)*OUTCYC
                     END IF
                     HTCNEZ = 0.024*HTCNEZ
 
                     IF(IDFRST .EQ. 1) THEN
                        DFRSTZ = 0.0
                     ELSE
                        DFRSTZ = 0.024*(FROSTZ/3.413 + DFSTCYC/3.413)
                     END IF
 
                     TOTEZ  = POWERZ + FANEZ + ELECZ + HTCNEZ + DFRSTZ
C
                     POWERF = 0.024*(DUTYN(2)*WCOMP(2)/3.413)
                     FANEF  = 0.024*(DUTYN(2)*(FNPWRE(2) + FNPWRC(2)))
                     ELECF  = 0.024*(DUTYN(2)*ELOSS(2))
 
                     HTCNEF = FFASH + FFAUX + DUTYN(2)*FFCYC + OTHERW/2
                     IF(DUTYN(1) .GE. DUTYN(2)) THEN
                        HTCNEF = HTCNEF + 0.5*DUTYN(1)*OUTCYC
                     ELSE
                        HTCNEF = HTCNEF + 0.5*DUTYN(2)*OUTCYC
                     END IF
                     HTCNEF = 0.024*HTCNEF
 
                     DFRSTF = 0
                     TOTEF  = POWERF + FANEF + ELECF + HTCNEF + DFRSTF
C
C                         OUTPUT THE ELECTRICAL ENERGY ON A DAILY BASIS
C
                     POWERT = POWERF + POWERZ
                     FANET  = FANEF  + FANEZ
                     ELECT  = ELECF  + ELECZ
                     HTCNET = HTCNEF + HTCNEZ
                     DFRSTT = DFRSTF + DFRSTZ
                     TOTET  = TOTEF  + TOTEZ
                     WRITE(IO,803) QEN(2)/3.6,   QEN(1)/3.6,
     .                             QEL(2)/3.6,   QEL(1)/3.6,
     .                             FLOWN(2)/2.2046, FLOWN(1)/2.2046,
     .                             COPRN(2), COPRN(1),
     .                             COPRN(2)*COPCYC(2),
     .                             COPRN(1)*COPCYC(1),
     .                             DUTYN(2), DUTYN(1),
     .                             POWERF, POWERZ, POWERT,
     .                             FANEF,  FANEZ,  FANET,
     .                             ELECF,  ELECZ,  ELECT,
     .                             HTCNEF, HTCNEZ, HTCNET,
     .                             DFRSTF, DFRSTZ, DFRSTT,
     .                             TOTEF,  TOTEZ,  TOTET
                END IF
C
C                    FREEZERS
C
           CASE DEFAULT
                dhrs = 0.024 * (24.0 - HRSOFF) / 24.0
                POWERZ = dhrs*(DUTYN(1)*WCOMP(1)/3.413)
                FANEZ  = dhrs*(DUTYN(1)*(FNPWRE(1) + FNPWRC(1)))
                ELECZ  = dhrs*(DUTYN(1)*ELOSS(1))
                HTCNEZ = FZASH + FZAUX + DUTYN(1)*(FZCYC + OUTCYC)
     .                         + OTHERW
                HTCNEZ = dhrs*HTCNEZ
 
                DFRSTZ = dhrs*(FROSTZ/3.413 + DFSTCYC/3.413)
                IF(IDFRST .EQ. 1) DFRSTZ = 0
                TOTEZ  = POWERZ + FANEZ + ELECZ + HTCNEZ + DFRSTZ
C
C                    OUTPUT THE ELECTRICAL BUDGET
C
                WRITE(IO,804) QEN(1)/3.6, QEL(1)/3.6, FLOWN(1)/2.2046,
     .                        COPRN(1), COPRN(1)*COPCYC(1), DUTYN(1),
     .                        POWERZ, FANEZ, ELECZ, HTCNEZ, DFRSTZ,
     .                        TOTEZ
C
      END SELECT
      CLOSE(IO)
      RETURN
C
C          FORMAT STATEMENTS
C
  800 FORMAT(/32X,'SUMMARY RESULTS'//5(68A1/)/)
  801 FORMAT('CABINET LOADS (W)'/
     .     30X,'FRESH FOOD',10X,'FREEZER',10X,'TOTAL'/
     .     'CONDUCTION',        T31,F7.3,T51,F7.3,T66,F7.3/
     .     'DOOR OPENINGS',     T31,F7.3,T51,F7.3,T66,F7.3/
     .     'FROSTING',          T31,F7.3,T51,F7.3,T66,F7.3/
     .     'ELEC DEFROST',      T31,F7.3,T51,F7.3,T66,F7.3/
     .     'PENETRATIONS',      T31,F7.3,T51,F7.3,T66,F7.3/
     .     'HEATERS & CONTROLS',T31,F7.3,T51,F7.3,T66,F7.3/
     .     'FAN HEAT',          T31,F7.3,T51,F7.3,T66,F7.3/
     .     'REFRIGERANT LINE',  T31,F7.3,T51,F7.3,T66,F7.3/
     .     'IN_WALL EVAP',      T31,F7.3,T51,F7.3,T66,F7.3/
     .     'IN_MULLION EVAP',   T31,F7.3,T51,F7.3,T66,F7.3/
     .     'IN_WALL COND',      T31,F7.3,T51,F7.3,T66,F7.3//
     .     'TOTAL LOADS',       T31,F7.3,T51,F7.3,T66,F7.3//)
  802 FORMAT(/'CABINET LOADS (W)'/
     .     'CONDUCTION',        T31,F7.3/
     .     'DOOR OPENINGS',     T31,F7.3/
     .     'FROSTING',          T31,F7.3/
     .     'ELEC DEFROST',      T31,F7.3/
     .     'PENETRATIONS',      T31,F7.3/
     .     'HEATERS & CONTROLS',T31,F7.3/
     .     'FAN HEAT',          T31,F7.3/
     .     'REFRIGERANT LINE',  T31,F7.3/
     .     'IN_WALL EVAP',      T31,F7.3/
     .     'IN_WALL COND',      T31,F7.3//
     .     'TOTAL LOADS',       T31,F7.3//)
  803 FORMAT('REFRIGERATION CYCLE '//
     .     30X,'FRESH FOOD',10X,'FREEZER',10X,'TOTAL'/
     .     'EVAP LOAD (W)',            T31,F7.3,T51,F7.3,T67,'   N/A'/
     .     'NET CAPACITY (W)',         T31,F7.3,T51,F7.3,T67,'   N/A'/
     .     'MASS FLOW (KG/HR)',        T31,F7.3,T51,F7.3,T67,'   N/A'/
     .     'COP - STEADY STATE',       T31,F7.3,T51,F7.3,T67,'   N/A'/
     .     'COP - CYCLING',            T31,F7.3,T51,F7.3,T67,'   N/A'/
     .     'DUTY CYCLE',               T31,F7.3,T51,F7.3,T67,'   N/A'//
     .     'COMPRESSOR (KWH/DAY)',     T31,F7.3,T51,F7.3,T66,F7.3/
     .     'FANS (KWH/DAY)',           T31,F7.3,T51,F7.3,T66,F7.3/
     .     'ELECTRONICS (KWH/DAY)',    T31,F7.3,T51,F7.3,T66,F7.3/
     .     'HEATERS & CONTROLS (KWH/DAY)',
     .                                 T31,F7.3,T51,F7.3,T66,F7.3/
     .     'DEFROST (KWH/DAY)',        T31,F7.3,T51,F7.3,T66,F7.3//
     .     'TOTAL ENERGY USE (KWH/DAY)',
     .                                 T31,F7.3,T51,F7.3,T66,F7.3)
  804 FORMAT('REFRIGERATION CYCLE '//
     .     'EVAP LOAD (W)',            T31,F7.3/
     .     'NET CAPACITY (W)',         T31,F7.3/
     .     'MASS FLOW (KG/HR)',        T32,F6.3/
     .     'COP - STEADY STATE',       T32,F6.3/
     .     'COP - CYCLING',            T32,F6.3/
     .     'DUTY CYCLE'                T32,F6.3//
     .     'COMPRESSOR (KWH/DAY)',     T32,F6.3/
     .     'FANS (KWH/DAY)',           T32,F6.3/
     .     'ELECTRONICS (KWH/DAY)',    T32,F6.3/
     .     'HEATERS & CONTROLS (KWH/DAY)',
     .                                 T32,F6.3/
     .     'DEFROST (KWH/DAY)',        T32,F6.3//
     .     'TOTAL ENERGY USE (KWH/DAY)',
     .                                 T32,F6.3)
  805 FORMAT('REFRIGERATION CYCLE '//
     .     30X,'FRESH FOOD',10X,'FREEZER',10X,'TOTAL'/
     .     'EVAP LOAD (W)',            T31,F7.3,T51,F7.3,T67,'   N/A'/
     .     'NET CAPACITY (W)',         T31,F7.3,T51,F7.3,T67,'   N/A'/
     .     'MASS FLOW (KG/HR)',        T32,F6.3,T52,F6.3,T67,'   N/A'/
     .     'COP - STEADY STATE',       T32,F6.3,T52,F6.3,T67,'   N/A'/
     .     'COP - CYCLING',            T32,F6.3,T52,F6.3,T67,'   N/A'/
     .     'DUTY CYCLE',               T32,F6.3,T52,F6.3,T67,F6.3//
     .     'COMPRESSOR (KWH/DAY)',     T32,F6.3,T52,F6.3,T67,F6.3/
     .     'FANS (KWH/DAY)',           T32,F6.3,T52,F6.3,T67,F6.3/
     .     'ELECTRONICS (KWH/DAY)',    T32,F6.3,T52,F6.3,T67,F6.3/
     .     'HEATERS & CONTROLS (KWH/DAY)',
     .                                 T32,F6.3,T52,F6.3,T67,F6.3/
     .     'DEFROST (KWH/DAY)',        T32,F6.3,T52,F6.3,T67,F6.3//
     .     'TOTAL ENERGY USE (KWH/DAY)',
     .                                 T32,F6.3,T52,F6.3,T67,F6.3)
  806 FORMAT('REFRIGERATION CYCLE '//
     .     30X,' LORENZ   ',10X,'FREEZER',10X,'TOTAL')
  807 FORMAT('REFRIGERATION CYCLE '//
     .     30X,'DUAL EVAP ',10X,'FREEZER',10X,'TOTAL')
  808 FORMAT('REFRIGERATION CYCLE '//
     .     30X,' LORENZ   ',07X,'FRESH FOOD',10X,'TOTAL')
  809 FORMAT('REFRIGERATION CYCLE '//
     .     30X,'DUAL EVAP ',07X,'FRESH FOOD',10X,'TOTAL')
  810 FORMAT('RUN NUMBER: ____',T29,'FILE NAME: 'A13,
     .     T65,'DATE: ',A2,'/',A2,'/',A2/T65,'TIME: ',A2,':',A2/)
  811 FORMAT('REFRIGERANT DATA',T31,'FRESH FOOD',10X,'FREEZER'/)
  812 FORMAT(T33,A6,T52,A6)
  813 FORMAT(T30,F4.3,1X,A6,8X,F4.3,1X,A6)
  814 FORMAT('REFRIGERANT DATA',T32,'CABINET'/)
  815 FORMAT('EVAP LOAD (W)',            T31,F7.3,T51,F7.3,T67,'   N/A'/
     .     'NET CAPACITY (W)',         T31,F7.3,T51,F7.3,T67,'   N/A'/
     .     'MASS FLOW (KG/HR)',        T32,F6.3,T52,F6.3,T67,'   N/A'/
     .     'COP - STEADY STATE',       T32,F6.3,T52,F6.3,T67,'   N/A'/
     .     'COP - CYCLING',            T32,F6.3,T52,F6.3,T67,'   N/A'/
     .     'DUTY CYCLE',               T32,F6.3,T52,F6.3,T67,F6.3//
     .     'COMPRESSOR (KWH/DAY)',     T32,F6.3,T52,F6.3,T67,F6.3/
     .     'FANS (KWH/DAY)',           T32,F6.3,T52,F6.3,T67,F6.3/
     .     'ELECTRONICS (KWH/DAY)',    T32,F6.3,T52,F6.3,T67,F6.3/
     .     'HEATERS & CONTROLS (KWH/DAY)',
     .                                 T32,F6.3,T52,F6.3,T67,F6.3/
     .     'DEFROST (KWH/DAY)',        T32,F6.3,T52,F6.3,T67,F6.3//
     .     'TOTAL ENERGY USE (KWH/DAY)',
     .                                 T32,F6.3,T52,F6.3,T67,F6.3)
  818 FORMAT(/'SET POINTS (C)',T31,F7.3,T51,F7.3/)
  828 FORMAT(/'FRESH FOOD TEMP (C)',T31,F7.3,
     .       /'FREEZER TEMP (C)',T31,F7.3/)
 
  819 FORMAT('NOTE: THE CABINET LOADS ARE OUT OF BALANCE WITH THE ',
     .       'EVAPORATOR CAPACITIES'/)
  820 FORMAT('DEFINED  EVAP AREAS',   T31,F7.3,T51,F7.3)
  821 FORMAT('ADJUSTED EVAP AREAS',   T31,F7.3,T51,F7.3/)
  822 FORMAT('DEFINED  SET POINTS (C)',   T31,F7.3,T51,F7.3)
  823 FORMAT('ADJUSTED SET POINTS (C)',   T31,F7.3,T51,F7.3/)
  824 FORMAT('WARNING: SPECIFIED LIQUID-LINE ANTI-SWEAT MAY ',
     .       'BE EXCESSIVE'/)
  825 FORMAT('FREEZER EVAPORATOR BYPASSED DURING PART OF THE CYCLE'/)
  826 FORMAT('FRESH FOOD EVAPORATOR BYPASSED DURING PART OF THE CYCLE'/)
  827 FORMAT('WARNING: TOTAL DUTY CYCLE IS GREATER THAN 1'/)
      END
C
      SUBROUTINE ATRBUT(NUMBER, IATR)
C     ******************************************************************
C     *     SCREEN OUT COLOR ATRIBUTE CALLS FOR MONOCHROME MONITOR     *
C     ******************************************************************
C
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
 
      IF(icolr .EQ. 0) RETURN
 
      CALL atrbut1(number, iatr)
 
      RETURN
      END
C
      SUBROUTINE SETATR(IATR)
C     ******************************************************************
C     *     SCREEN OUT COLOR ATRIBUTE CALLS FOR MONOCHROME MONITOR     *
C     ******************************************************************
C
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
 
      IF(icolr .EQ. 0) RETURN
 
      CALL setatr1(iatr)
 
      RETURN
      END
C
      SUBROUTINE DOSFUN(LOC,FILIN,FILOUT,DOSCHR)
C     *****************************************************************
C     *                CREATE STRING TO MANIPULATE FILE               *
C     *****************************************************************
C
      LOGICAL DONE, DOT
      CHARACTER CHRBIT(64), CHRTST(13)
      CHARACTER*13 FILIN, FILOUT, FILTST
      CHARACTER*64 DOSCHR, WRKCHR
 
      EQUIVALENCE (WRKCHR, CHRBIT(1)),        (FILTST, CHRTST(1))
C
C          SELECT FUNCTION AND BUILD UP THE COMMAND LINE
C
      N = 1
      DO WHILE (N .LE. 60)
         CHRBIT(N) = CHAR(32)
         N = N + 1
      END DO
 
      SELECT CASE (LOC)
         CASE (1, 3)                                       !Copy or erase
            CHRBIT(1) = 'C'
            CHRBIT(2) = 'O'
            CHRBIT(3) = 'P'
            CHRBIT(4) = 'Y'
            CHRBIT(5) = ' '
            m = 0
 
            if(loc .eq. 3) then
               chrbit(1) = 'E'
               chrbit(2) = 'R'
               chrbit(3) = 'A'
               chrbit(4) = 'S'
               chrbit(5) = 'E'
               m = 1
            end if
 
            DONE = .FALSE.
            DOT = .FALSE.
 
            N = 1
            FILTST = FILIN
            DO WHILE (.NOT. DONE)
               IF(ICHAR(CHRTST(N)) .LE. 32 .AND. DOT) THEN
                  DONE = .TRUE.
                  CYCLE
               END IF
 
               CHRBIT(5+N+m) = CHRTST(N)
               IF(CHRTST(N) .EQ. '.') DOT = .TRUE.
               N = N + 1
            END DO
 
            if(loc .eq. 1) then
 
               N = N + 1
               CHRBIT(5+N+m) = CHAR(32)
 
               FILTST = FILOUT
               L = 1
               DONE = .FALSE.
               DOT = .FALSE.
               DO WHILE (.NOT. DONE)
                  IF(ICHAR(CHRTST(L)) .LE. 32 .AND. DOT) THEN
                     DONE = .TRUE.
                     CYCLE
                  END IF
 
                  CHRBIT(5+N+m) = CHRTST(L)
                  IF(CHRTST(L) .EQ. '.') DOT = .TRUE.
                  N = N + 1
                  L = L + 1
               END DO
            end if
 
            CHRBIT(5+N+m) = '$'
 
         CASE (2)                                          !Change .ext
            DONE = .FALSE.
            FILTST = FILIN
            N = 1
            DO WHILE (.NOT. DONE)
               CHRBIT(N) = CHRTST(N)
               IF(CHRTST(N) .EQ. '.') THEN
                  DONE = .TRUE.
                  CYCLE
               ELSE
                  N = N + 1
               END IF
            END DO
 
            FILTST = FILOUT
            CHRBIT(N+1) = CHRTST(10)
            CHRBIT(N+2) = CHRTST(11)
            CHRBIT(N+3) = CHRTST(12)
 
            N = 1
            DO WHILE (N .LE. 12)
               CHRTST(N) = CHRBIT(N)
               N = N + 1
            END DO
 
            FILIN = FILTST
 
      END SELECT
 
      DOSCHR = WRKCHR
      RETURN
      END
C
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