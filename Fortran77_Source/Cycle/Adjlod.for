$DEBUG
      SUBROUTINE ADJLOD(ICYCL,IC,TS3,TS5,FROSTF,FROSTZ,IDFRST)
C     ******************************************************************
C     *    ADJUST THE CABINET LOADS AND SET POINT TEMPERATURES         *
C     ******************************************************************
C
C
C          COMMON BLOCKS
C
      REAL MREF
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
      COMMON/PARMS2/TSPEC,TDROPC
      COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
      COMMON/FEVAP/UTPE,USUPE,ATOTE
      COMMON/CONDEN/UDSC,UTPC,USCC,ATOTC,UACOND
      COMMON/SPECS/DTSUPE,DTSUBC
      COMMON /SPECS2/ ISPEC,XEXITE,DTSUPI
      COMMON/CABLOD/FFASH,FFAUX,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
     .              FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
     .              FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
      COMMON/FANS/FANE,FANZ,FANC,DUTYC,W,COPR
      COMMON/LORENZ/DUTYE,DUTYZ,PWRL,PWRE,CAPE,CAPZ,DUTYL,DUTYS,
     .              FANEL,FANCL,FANES,FANCS
      COMMON/FIGURE/IEVAP
      COMMON / RESULT / QE, QZ, FLOW, QEN(2), FLOWN(2), COPRN(2)
      COMMON / CHINA / INCTRL
      COMMON / BALNCE / IBLNCE, BAFFLF, BAFFLZ, AREAFZ, ATOTE_S,
     .                  AREAFZ_S, ATOTE_A, AREAFZ_A, FFTEMP_A, FZTEMP_A
      COMMON / INWALL / UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
     .                  Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
     .                  CAPZ_IN_WALL, Q_FZ_FF
      COMMON / PENAT / FFPENA, FZPENA
C
C          BRANCH ON THE VALUE IC.  AVE VARIABLES AND INITIALIZE ON THE
C          FIRST CALL AND THEN WAIT UNTIL THE 4TH CALL TO MAKE ADJUSTMENTS
C
      SELECT CASE (IC)
         CASE (1)                                          !Initialize
            IBLNCE = 0
            IRET = 0
            IF(ICYCL .NE. 2) IRET = 1
      !     IF(IFRSH .NE. 0 .AND. IFREZ .NE. 0) IRET = 1
 
            FFTEMP_S = FFTEMP
            FZTEMP_S = FZTEMP
            FFQ_S = FFQ
            FZQON_S = FZQON
            FZQOFF_S = FZQOFF
            FFLAT_S = FFLAT
            FZLAT_S = FZLAT
            FFSEN_S = FFSEN
            FZSEN_S = FZSEN
            FFHTQ_S = FFHTQ
            FZHTQ_S = FZHTQ
            FROSTF_S = FROSTF
            FROSTZ_S = FROSTZ
 
            CONDF_S = CONDF
            CONDZ_S = CONDZ
 
            ATOTE_S = ATOTE
            AREAFZ_S = AREAFZ
            UAF_S = UAF
            ATOTE_A = ATOTE
            AREAFZ_A = AREAFZ
 
            UFF = (FFQ - FFLAT - FFPENA - FFHTQ - FROSTF + QMUL)
     .            /(TROOM - FFTEMP)
 
            FZQ = FZQON
            FZQ_S = FZQ
 
            UFZ = (FZQ - FZLAT - FZPENA - FZHTQ - FROSTZ - QMUL)
     .            /(TROOM - FZTEMP)
 
            UFF_SEN = FFSEN_S/(TROOM - FFTEMP_S)
            UFZ_SEN = FZSEN_S/(TROOM - FZTEMP_S)
 
            UCND_F = (CONDF + QMUL)/(TROOM - FFTEMP_S)
            UCND_Z = (CONDZ - QMUL)/(TROOM - FZTEMP_S)
 
            TS3_S = TS3
            TS5_S = TS5
            FFTEMP_A = FFTEMP
            FZTEMP_A = FZTEMP
 
            DELTS5_OLD = 0
 
            UA_FZ_S = UA_FZ
            UA_ML_S = UA_ML
            UA_FF_S = UA_FF
 
         CASE (2, 3)
 
         CASE DEFAULT
            IF (IRET .EQ. 1) RETURN
C
C              DETERMINE NEEDED REBALANCING OF CABINET LOADS
C
            FFLOAD = DUTYE*CAPE + DUTYZ*Q_FZ_FF
            FZLOAD = DUTYZ*CAPZ
            DELLOD = (FFLOAD*CAPZ - FZLOAD*CAPE)/(CAPE + CAPZ)
 
            DUTMAX = MAX(DUTYE, DUTYZ)
            DUTMIN = MIN(DUTYE, DUTYZ)
            DUTDIF = DUTMAX - DUTMIN
 
            IBLNCE = 0
            DUTERR = DUTDIF/DUTMAX
            IF(DUTERR .LE. 0.001) RETURN
            IF(DUTDIF .GE. 0.025) IBLNCE = 1
 
            SELECT CASE (INCTRL)
               CASE (0)                                    !No control
                  RETURN
 
               CASE (1)                                    !Evap area ratio
                  FFNEW = FFLOAD + DELLOD
                  FZNEW = FZLOAD - DELLOD
                  DAREAF = (FFNEW/FFLOAD - 1.0)*ATOTE
                  DAREAZ = (FZNEW/FZLOAD - 1.0)*AREAFZ
 
                  DUTY_AVE = (DUTYE + DUTYZ)/2.0
                  DAREAF = (DUTYE/DUTY_AVE - 1.0)*ATOTE
                  DAREAZ = (DUTYZ/DUTY_AVE - 1.0)*AREAFZ
 
                  RATIOF = DAREAF/ATOTE
                  IF(RATIOF .LT. -0.5) RATIOF = -0.5
                  DAREAF = RATIOF*ATOTE*0.5
                  RATIOZ = DAREAZ/AREAFZ
                  IF(RATIOZ .LT. -0.5) RATIOZ = -0.5
                  DAREAZ = RATIOZ*AREAFZ*0.5
 
                  IF(ABS(DAREAF) .LT. ABS(DAREAZ)) THEN
                     ATOTE = ATOTE + DAREAF
                     AREAFZ = AREAFZ - DAREAF
                  ELSE
                     AREAFZ = AREAFZ + DAREAZ
                     ATOTE = ATOTE - DAREAZ
                  END IF
 
                  UAF = UAF_S*AREAFZ/AREAFZ_S
                  ATOTE_A = ATOTE
                  AREAFZ_A = AREAFZ
 
                  UA_FZ = UA_FZ_S*AREAFZ/AREAFZ_S
                  UA_ML = UA_ML_S*AREAFZ/AREAFZ_S
                  UA_FF = UA_FF_S*ATOTE/ATOTE_S
 
               CASE (2)                                    !FF Cabinet temp
                  DUTYN = 0.5*(DUTYE + DUTYZ)
                  FFQ = DUTYN*CAPE + DUTYN*Q_FZ_FF + FROSTF_S
                  DELTS3 = (FFQ - FFQ_S)/UFF
 
                  TS3 = TS3_S - DELTS3/1.8
                  FFTEMP_A = 1.8*TS3 - 459.6
 
                  FFSEN = UFF_SEN*(TROOM - FFTEMP_A)
                  CONDF = UCND_F*(TROOM - FFTEMP_A) - QMUL
 
               CASE (3)                                    !Freezer temp
                  DUTYN = 0.25*DUTYE + 0.75*DUTYZ
                  FZQ = DUTYN*CAPZ
                  IF(IDFRST .EQ. 0) FZQ = FZQ - FROSTZ_S
 
                  DELTS5 = 0.3*(FZQ - FZQ_S)/UFZ + 0.7*DELTS5_OLD
                  DELTS5_OLD = DELTS5
 
                  FZQ = FZQ_S + UFZ*DELTS5
                  FZQON = FZQ
                  FZQOFF = FZQ
 
                  TS5 = TS5_S - DELTS5/1.8
                  FZTEMP_A = 1.8*TS5 - 459.6
 
                  FZSEN = UFZ_SEN*(TROOM - FZTEMP_A)
                  CONDZ = UCND_Z*(TROOM - FZTEMP_A) + QMUL
 
               CASE (4)                                    !Control valve
                  !Nothing
 
               CASE (5)                                    !Lorenz with fan
                  !Nothing
 
            END SELECT
      END SELECT
 
      RETURN
      END
