$debug
      SUBROUTINE CCOUNT(TS1,T2,H2,TDEW_S,HDEW_S,TBUB_S,HBUB_S,CPRLIQ,
     .     CPRVAP,PIN,POUT,X,NUM_ZONE,QDSC,QTPC,QSCC,QTOTC,FSUP,FSUB)
C     ******************************************************************
C     *    SUBROUTINE CCOUNT - CALCULATES THE CONDENSER HEAT EXCHANGE  *
C     *    FOR COUNTERFLOW HEAT EXCHANGER                              *
C     ******************************************************************
C
      LOGICAL LOOKING_FOR_AREA, HAVE_NOT_USED_FULL_AREA, ENTERS_WET
 
      REAL MDOTR
 
      DIMENSION X(5),XL(5),XV(5)
 
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MDOTR,ETAV,SEFF
      COMMON/HTEXS/CFMC,CFME,CMFF,UAF,ETAC,ETAE,ETAF
      COMMON/CONDEN/UDSC,UTPC,USCC,ATOTC,UACOND
      COMMON/SPECS/DTSUPE,DTSUBC
      COMMON / DIAG / IM, IC,IE
 
      COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
     .              N_EVAP, N_COND
 
      DATA AREA_TOL / 0.001 /
C
C          INITIALIZE
C
      TBUB = TBUB_S
      HBUB = HBUB_S
 
      IF(H2 .LE. HDEW_S) THEN
         HDEW = H2
         TDEW = T2
         ENTERS_WET = .TRUE.
      ELSE
         HDEW = HDEW_S
         TDEW = TDEW_S
         ENTERS_WET = .FALSE.
      END IF
 
      HDEW_START = HDEW
      TDEW_START = TDEW
 
      DELP = (PIN  - POUT)/FLOAT(NUM_ZONE)
      DELH = (HDEW - HBUB)/FLOAT(NUM_ZONE)
      PBUB = POUT
 
      QDSC = 0.0
      QTPC = 0.0
      QSCC = 0.0
 
      ASCC = 0
      ATPC = 0
      ADSC = 0
 
      TAIR = TS1
      CAIR = CFMC
      HAVE_NOT_USED_FULL_AREA = .TRUE.
C
C          START OFF WITH THE SUBCOOLING AREA.
C
      IF(DTSUBC .GT. 0.0) THEN
         TCSUB = TBUB - DTSUBC
         CRSC = MDOTR*CPRLIQ
 
         IF(CAIR .LE. CRSC) THEN
            CMINSC = CAIR
            CMAXSC = CRSC
         ELSE
           CMINSC = CRSC
           CMAXSC = CAIR
         END IF
C
C          IS AREA BIG ENOUGH FOR SUBCOOLING
C
         QMAX = CMINSC*(TBUB - TAIR)
         QSUB = CRSC  *(TBUB - TCSUB)
 
         EFF_SUB = QSUB/QMAX
         CALL EXF(1,ATOTC,USCC,CMINSC,CMAXSC,EFFSCC,DEXDAR)
 
         IF(EFFSCC .LE. EFF_SUB) THEN                      !Need more area
            ASCC = ATOTC
            HAVE_NOT_USED_FULL_AREA = .FALSE.
C
C          BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
C          SUBCOOLED REGION
C
C          INITIALIZE VARIABLES
C
         ELSE
            ASCC = ATOTC/10.0
            LOOKING_FOR_AREA = .TRUE.
 
            DO WHILE (LOOKING_FOR_AREA)
               CALL EXF(1,ASCC,USCC,CMINSC,CMAXSC,EFFSCC,DEXDAR)
               ERROR = ABS(EFFSCC - EFF_SUB)
               IF(ERROR .LE. AREA_TOL) THEN
                  LOOKING_FOR_AREA = .FALSE.
                  CYCLE
               END IF
 
               DAREA = - (EFFSCC - EFF_SUB)/DEXDAR
               DAREA_MIN = -0.50*ASCC
               DAREA_MAX =  0.50*(ATOTC - ASCC)
 
               IF(DAREA .LT. DAREA_MIN) DAREA = DAREA_MIN
               IF(DAREA .GT. DAREA_MAX) DAREA = DAREA_MAX
 
               ASCC  = ASCC + DAREA
            END DO
 
         END IF
 
         QSCC = EFFSCC*QMAX
         TAIR = TAIR + QSCC/CAIR
      END IF
C
C          CONTINUE WITH TWO-PHASE AREA
C
      ALEFT = ATOTC - ASCC
 
      N = 1
      DO WHILE (N .LE. NUM_ZONE)
         PDEW = PBUB + DELP
         HDEW = HBUB + DELH
         CALL HPIN(HDEW,PDEW,X,TDEW,XQ,XL,XV,VL,VV,HL,HV)
 
         IF(HAVE_NOT_USED_FULL_AREA) THEN
            CPRTP = (HDEW-HBUB)/ABS(TDEW-TBUB+0.0001)
            CRTP  = MDOTR*CPRTP
C
C          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
C
            IF(CAIR .LE. CRTP) THEN
               CMINTP = CAIR
               CMAXTP = CRTP
            ELSE
               CMINTP = CRTP
               CMAXTP = CAIR
            END IF
C
C          IS AREA BIG ENOUGH FOR CONDENSATION
C
            QMAX = CMINTP*(TDEW - TAIR)
            QDUM = MDOTR*(HDEW - HBUB)
 
            EFF_TPC = QDUM/QMAX
            CALL EXF(1,ALEFT,UTPC,CMINTP,CMAXTP,EFFTPC,DEXDAR)
 
 
            IF(EFFTPC .LE. EFF_TPC
     .                .OR. (ENTERS_WET .AND. N .EQ. NUM_ZONE)) THEN
               ATPC = ATPC + ALEFT
               HAVE_NOT_USED_FULL_AREA = .FALSE.
C
C          BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
C          TWO PHASE REGION
C
C          INITIALIZE VARIABLES
C
            ELSE
               ADUM = 0.9*ALEFT
               LOOKING_FOR_AREA = .TRUE.
 
               ILOOK = 0
               DO WHILE (LOOKING_FOR_AREA)
                  ILOOK = ILOOK + 1
                  CALL EXF(1,ADUM,UTPC,CMINTP,CMAXTP,EFFTPC,DEXDAR)
                  ERROR = ABS(EFFTPC - EFF_TPC)
                  IF(ERROR .LE. AREA_TOL .OR. ILOOK .GE. 10) THEN
                     LOOKING_FOR_AREA = .FALSE.
                     CYCLE
                  END IF
 
                  DAREA = - (EFFTPC - EFF_TPC)/DEXDAR
                  DAREA_MIN = -0.75*ADUM
                  DAREA_MAX = 0.50*(ALEFT - ADUM)
 
                  IF(DAREA .LT. DAREA_MIN) DAREA = DAREA_MIN
                  IF(DAREA .GT. DAREA_MAX) DAREA = DAREA_MAX
 
                  ADUM  = ADUM + DAREA
               END DO
               ATPC = ATPC + ADUM
 
            END IF
 
            QTPC = QTPC + EFFTPC*QMAX
            TAIR = TAIR + EFFTPC*QMAX/CAIR
         END IF
 
         ALEFT = ATOTC - ASCC - ATPC
         HBUB = HBUB + DELH
         TBUB = TDEW
         PBUB = PBUB + DELP
         N = N + 1
      END DO
      IF(ALEFT .LE. 0.0) HAVE_NOT_USED_FULL_AREA = .FALSE.
C
C          CONTINUE WITH DESUPERHEATING AREA
C
      HDEW = HDEW_START
      TDEW = TDEW_START
 
      IF(HAVE_NOT_USED_FULL_AREA) THEN
         CPRVAP = (H2 - HDEW)/(T2 - TDEW)
         CRDS  = MDOTR*CPRVAP
C
C          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
C
         IF(CAIR .LE. CRDS) THEN
            CMINDS = CAIR
            CMAXDS = CRDS
         ELSE
            CMINDS = CRDS
            CMAXDS = CAIR
         END IF
C
C          DETERMINE THE NET HEAT TRANSFER
C
         CALL EXF(1,ALEFT,UDSC,CMINDS,CMAXDS,EFFDSC,DEXDAR)
         QDSC = CMINDS*EFFDSC*(T2 - TAIR)
 
         ADSC = ALEFT
      END IF
C
C        CALCULATE THE FRACTIONAL SUBCOOLING AND SUPERHEATING REGIONS
C
      FSUP = ADSC/ATOTC
      FSUB = ASCC/ATOTC
 
      QTOTC = QSCC + QTPC + QDSC
      UACOND = ATOTC*(FSUP*UDSC + FSUB*USCC + (1.0 - FSUP - FSUB)*UTPC)
 
      RETURN
      END
 
      SUBROUTINE CCROSS(TS1,T2,H2,TDEW_S,HDEW_S,TBUB_S,HBUB_S,CPRLIQ,
     .     CPRVAP,PIN,POUT,X,NUM_ZONE,QDSC,QTPC,QSCC,QTOTC,FSUP,FSUB)
C     ******************************************************************
C     *    SUBROUTINE CCOUNT - CALCULATES THE CONDENSER HEAT EXCHANGE  *
C     *    FOR COUNTERFLOW HEAT EXCHANGER                              *
C     ******************************************************************
C
      LOGICAL LOOKING_FOR_AREA, HAVE_NOT_USED_FULL_AREA, ENTERS_WET
 
      REAL MDOTR
 
      DIMENSION X(5),XL(5),XV(5)
 
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MDOTR,ETAV,SEFF
      COMMON/HTEXS/CFMC,CFME,CMFF,UAF,ETAC,ETAE,ETAF
      COMMON/CONDEN/UDSC,UTPC,USCC,ATOTC,UACOND
      COMMON/SPECS/DTSUPE,DTSUBC
      COMMON / DIAG / IM, IC,IE
 
      COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
     .              N_EVAP, N_COND
 
      DATA AREA_TOL / 0.001 /
C
C          INITIALIZE
C
      TBUB = TBUB_S
      HBUB = HBUB_S
 
      IF(H2 .LE. HDEW_S) THEN
         HDEW = H2
         TDEW = T2
         ENTERS_WET = .TRUE.
      ELSE
         HDEW = HDEW_S
         TDEW = TDEW_S
         ENTERS_WET = .FALSE.
      END IF
 
      HDEW_START = HDEW
      TDEW_START = TDEW
 
      DELP = (PIN  - POUT)/FLOAT(NUM_ZONE)
      DELH = (HDEW - HBUB)/FLOAT(NUM_ZONE)
      PBUB = POUT
 
      QDSC = 0.0
      QTPC = 0.0
      QSCC = 0.0
 
      ASCC = 0
      ATPC = 0
      ADSC = 0
 
      TAIR = TS1
      CAIR = CFMC
      HAVE_NOT_USED_FULL_AREA = .TRUE.
C
C          START OFF WITH THE SUBCOOLING AREA.
C
      IF(DTSUBC .GT. 0.0) THEN
         TCSUB = TBUB - DTSUBC
         CRSC = MDOTR*CPRLIQ
 
         IF(CAIR .LE. CRSC) THEN
            CMINSC = CAIR
            CMAXSC = CRSC
         ELSE
           CMINSC = CRSC
           CMAXSC = CAIR
         END IF
C
C          IS AREA BIG ENOUGH FOR SUBCOOLING
C
         QMAX = CMINSC*(TBUB - TAIR)
         QSUB = CRSC  *(TBUB - TCSUB)
 
         EFF_SUB = QSUB/QMAX
         CALL EXF(2,ATOTC,USCC,CMINSC,CMAXSC,EFFSCC,DEXDAR)
 
         IF(EFFSCC .LE. EFF_SUB) THEN                      !Need more area
            ASCC = ATOTC
            HAVE_NOT_USED_FULL_AREA = .FALSE.
C
C          BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
C          SUBCOOLED REGION
C
C          INITIALIZE VARIABLES
C
         ELSE
            ASCC = ATOTC/10.0
            LOOKING_FOR_AREA = .TRUE.
 
            ICOUNT = 0
            QTOL = 1.0
            DO WHILE (LOOKING_FOR_AREA)
               ICOUNT = ICOUNT + 1
               IF(ICOUNT .GT. 100) THEN
                  LOOKING_FOR_AREA = .FALSE.
                  CYCLE
               END IF
 
               CAIR = (ASCC/ATOTC)*CFMC
               IF(CAIR .LE. CRSC) THEN
                  CMINSC = CAIR
                  CMAXSC = CRSC
               ELSE
                  CMINSC = CRSC
                  CMAXSC = CAIR
               END IF
 
               QMAX = CMINSC*(TBUB - TAIR)
               EFF_SUB = QSUB/QMAX
 
               CALL EXF(2,ASCC,USCC,CMINSC,CMAXSC,EFFSCC,DEXDAR)
 
               ERROR = ABS(QTOL)
               IF(ERROR .LE. AREA_TOL) THEN
                  LOOKING_FOR_AREA = .FALSE.
                  CYCLE
               END IF
 
               QRAT = EFFSCC*QMAX/QSUB
               QTOL = 1.0 - QRAT
 
               DAREA = ASCC*(1.0 - QRAT)
 
               DAREA_MIN = -0.50*ASCC
               DAREA_MAX =  0.50*(ATOTC - ASCC)
 
               IF(DAREA .LT. DAREA_MIN) DAREA = DAREA_MIN
               IF(DAREA .GT. DAREA_MAX) DAREA = DAREA_MAX
 
               ASCC  = ASCC + DAREA
            END DO
 
         END IF
 
         QSCC = EFFSCC*CMINSC*(TBUB - TAIR)
      END IF
C
C          CONTINUE WITH TWO-PHASE AREA
C
      ALEFT = ATOTC - ASCC
 
      N = 1
      DO WHILE (N .LE. NUM_ZONE)
         PDEW = PBUB + DELP
         HDEW = HBUB + DELH
         CALL HPIN(HDEW,PDEW,X,TDEW,XQ,XL,XV,VL,VV,HL,HV)
 
         IF(HAVE_NOT_USED_FULL_AREA) THEN
            CPRTP = (HDEW-HBUB)/ABS(TDEW-TBUB+0.0001)
            CRTP  = MDOTR*CPRTP
C
C          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
C
            CAIR = (ALEFT/ATOTC)*CFMC
            IF(CAIR .LE. CRTP) THEN
               CMINTP = CAIR
               CMAXTP = CRTP
            ELSE
               CMINTP = CRTP
               CMAXTP = CAIR
            END IF
C
C          IS AREA BIG ENOUGH FOR CONDENSATION
C
            QMAX = CMINTP*(TDEW - TAIR)
            QDUM = MDOTR*(HDEW - HBUB)
 
            EFF_TPC = QDUM/QMAX
            CALL EXF(2,ALEFT,UTPC,CMINTP,CMAXTP,EFFTPC,DEXDAR)
 
 
            IF(EFFTPC .LE. EFF_TPC .OR. ENTERS_WET) THEN      !Need more area
               ATPC = ATPC + ALEFT
               HAVE_NOT_USED_FULL_AREA = .FALSE.
C
C          BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
C          TWO PHASE REGION
C
C          INITIALIZE VARIABLES
C
            ELSE
               ADUM = 0.9*ALEFT
               LOOKING_FOR_AREA = .TRUE.
 
               ICOUNT = 0
               QTOL = 1.0
               DO WHILE (LOOKING_FOR_AREA)
                  ICOUNT = ICOUNT + 1
                  IF(ICOUNT .GT. 100) THEN
                     LOOKING_FOR_AREA = .FALSE.
                     CYCLE
                  END IF
 
 
                  CAIR = (ADUM/ATOTC)*CFMC
                  IF(CAIR .LE. CRTP) THEN
                     CMINTP = CAIR
                     CMAXTP = CRTP
                  ELSE
                     CMINTP = CRTP
                     CMAXTP = CAIR
                  END IF
 
                  QMAX = CMINTP*(TDEW - TAIR)
                  EFF_TPC = QDUM/QMAX
 
                  CALL EXF(2,ADUM,UTPC,CMINTP,CMAXTP,EFFTPC,DEXDAR)
 
                  ERROR = ABS(QTOL)
                  IF(ERROR .LE. AREA_TOL) THEN
                     LOOKING_FOR_AREA = .FALSE.
                     CYCLE
                  END IF
 
                  QRAT  = EFFTPC*QMAX/QDUM
                  QTOL = 1.0 - QRAT
 
                  DAREA = ADUM*(1.0 - QRAT)
 
                  DAREA_MIN = -0.75*ADUM
                  DAREA_MAX = 0.50*(ALEFT - ADUM)
 
                  IF(DAREA .LT. DAREA_MIN) DAREA = DAREA_MIN
                  IF(DAREA .GT. DAREA_MAX) DAREA = DAREA_MAX
 
                  ADUM  = ADUM + DAREA
               END DO
               ATPC = ATPC + ADUM
 
            END IF
 
            QTPC = QTPC + EFFTPC*CMINTP*(TDEW - TAIR)
         END IF
 
         ALEFT = ATOTC - ASCC - ATPC
         HBUB = HBUB + DELH
         TBUB = TDEW
         PBUB = PBUB + DELP
         N = N + 1
      END DO
C
C          CONTINUE WITH DESUPERHEATING AREA
C
      ALEFT = ATOTC - ASCC - ATPC
      IF(ALEFT .LE. 0.0) HAVE_NOT_USED_FULL_AREA = .FALSE.
 
      HDEW = HDEW_START
      TDEW = TDEW_START
 
      IF(HAVE_NOT_USED_FULL_AREA) THEN
         CPRVAP = (H2 - HDEW)/(T2 - TDEW)
         CRDS  = MDOTR*CPRVAP
C
C          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
C
         CAIR = (ALEFT/ATOTC)*CFMC
         IF(CAIR .LE. CRDS) THEN
            CMINDS = CAIR
            CMAXDS = CRDS
         ELSE
            CMINDS = CRDS
            CMAXDS = CAIR
         END IF
C
C          DETERMINE THE NET HEAT TRANSFER
C
         CALL EXF(2,ALEFT,UDSC,CMINDS,CMAXDS,EFFDSC,DEXDAR)
         QDSC = CMINDS*EFFDSC*(T2 - TAIR)
 
         ADSC = ALEFT
      END IF
C
C        CALCULATE THE FRACTIONAL SUBCOOLING AND SUPERHEATING REGIONS
C
      FSUP = ADSC/ATOTC
      FSUB = ASCC/ATOTC
 
      QTOTC = QSCC + QTPC + QDSC
      UACOND = ATOTC*(FSUP*UDSC + FSUB*USCC + (1.0 - FSUP - FSUB)*UTPC)
 
      RETURN
      END
 
      SUBROUTINE EXF(LOC, AREA, U, CMIN, CMAX, EFF, DEFFDA)
C     ******************************************************************
C     *    CALCULATE COUNTER FLOW EFFICIENCY PARAMETERS                *
C     ******************************************************************
C
      REAL NTU
 
      DIMENSION A(4,6), EFF_CROSS(2)
C
      DATA (A(I,1),I=1,4)/2.394292,2.410798,2.399687,2.359642/
      DATA (A(I,2),I=1,4)/-1.19402,-2.23391,-2.96882,-3.37650/
      DATA (A(I,3),I=1,4)/-1.45067,0.825900,2.367080,3.04862/
 
      DATA (A(I,4),I=1,4)/1.938453,0.051006,-1.23009,-1.63421/
      DATA (A(I,5),I=1,4)/-0.81305,-0.11891,0.373338,0.468741/
      DATA (A(I,6),I=1,4)/0.118651,0.023360,-0.04886,-0.05492/
C
C          CALCULATE NTU AND CAPACITY RATIO
C
 
      NTU = AREA*U/CMIN
      CRAT = CMIN/CMAX
 
      SELECT CASE (LOC)
         CASE (1)                                          !Counter-flow
            XX = 1.0 - CRAT
            XXX = EXP(-NTU*XX)
            EFF = (1.0 - XXX)/(1.0 - CRAT*XXX)
 
            DEFFDA = (U/CMIN)*XX*XXX*(1.0 - CRAT*EFF)/(1.0 - CRAT*XXX)
 
         CASE (2)                                          !Cross-flow
            IF(CRAT .GE. 0.00 .AND. CRAT .LE. 0.25) I=1
            IF(CRAT .GT. 0.25 .AND. CRAT .LE. 0.50) I=2
            IF(CRAT .GT. 0.50 .AND. CRAT .LE. 0.75) I=3
            IF(CRAT .GT. 0.75 .AND. CRAT .LE. 1.00) I=4
 
            IF(NTU .LE. 0.0) NTU = 0.0
 
            DO L = 1, 2
               BETA = LOG10(NTU+1.0)
               EFFA = 0.0
               EFFB = 0.0
 
               DO J = 1, 6
                  EX = 1.0*J
                  IF(I .EQ. 1) THEN
                     EFFA = 1.0 - EXP(-NTU)
                  ELSE
                     EFFA = EFFA + A((I-1),J)*BETA**EX
                  END IF
                  EFFB = EFFB + A(I,J)*BETA**EX
               END DO
 
               FRAC = (CRAT-(I-1)*0.25)/(I*0.25-(I-1)*0.25)
               EFFECT = EFFA + FRAC*(EFFB-EFFA)
               IF(EFFECT .GT. 1.0) EFFECT = 1.0
 
               EFF_CROSS(L) = EFFECT
               NTU = 0.9*NTU
            END DO
 
            EFF = EFF_CROSS(1)
            DEFFDA = 10.0*(EFF_CROSS(1) - EFF_CROSS(2))/AREA
 
      END SELECT
 
      IF(DEFFDA .LE. 0.0) DEFFDA = 0.0001
 
      RETURN
      END
C
      SUBROUTINE FFCOUNT(H5_S,T5_S,HDEW_S,TDEW_S,TS3,CPR,PIN,POUT,X,
     .                   NUM_ZONE,QTOTE,FSUPE)
C     ******************************************************************
C     *    SUBROUTINE FFCOUNT - SOLVES FOR THE FRESH FOOD EVAPORATOR   *
C     *    HEAT TRANSFER FOR COUNTERFLOW HEAT EXCHANGER                *
C     ******************************************************************
C
      LOGICAL LOOKING_FOR_AREA, HAVE_NOT_USED_FULL_AREA, CONVERGED
 
      REAL MDOTR
 
      DIMENSION X(5),XL(5),XV(5)
 
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MDOTR,ETAV,SEFF
      COMMON/HTEXS/CFMC,CFME,CMFF,UAF,ETAC,ETAE,ETAF
      COMMON /FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
      COMMON / DIAG / IM, IC,IE
 
      COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
     .              N_EVAP, N_COND
 
      DATA AREA_TOL / 0.001 /, NCALL /0/
C
C          INITIALIZE
C
      ICOUNT = 0
 
      T5 = T5_S
      H5 = H5_S
      HDEW = HDEW_S
      TDEW = TDEW_S
 
      DELP = (POUT - PIN)/FLOAT(NUM_ZONE)
      DELH = (HDEW - H5)/FLOAT(NUM_ZONE)
      P5 = PIN
 
      QSUP = 0.0
      QTPC = 0.0
      QTOTE_LAST = 0
 
      ASUP = 0
      ATPC = 0
 
      IF(NCALL .EQ. 0) THEN
         TAIR = TS3 - 2
         TAIR_GUESS = TAIR
         NCALL = 1
      ELSE
         TAIR = TAIR_GUESS
      END IF
      CAIR = CFME
      HAVE_NOT_USED_FULL_AREA = .TRUE.
C
C          BEGIN WITH TWO-PHASE AREA
C
      CONVERGED = .FALSE.
      DO WHILE (.NOT. CONVERGED)
         ICOUNT = ICOUNT + 1
         ALEFT = ATOTE
 
         N = 1
         DO WHILE (N .LE. NUM_ZONE)
            PDEW = P5 + DELP
            HDEW = H5 + DELH
 
            IF(HAVE_NOT_USED_FULL_AREA) THEN
               CALL HPIN(HDEW,PDEW,X,TDEW,XQ,XL,XV,VL,VV,HL,HV)
               CPRTP = (HDEW-H5)/ABS(TDEW-T5+0.0001)
               CRTP  = MDOTR*CPRTP
C
C          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
C
               IF(CAIR .LE. CRTP) THEN
                  CMINTP = CAIR
                  CMAXTP = CRTP
               ELSE
                  CMINTP = CRTP
                  CMAXTP = CAIR
               END IF
C
C          IS AREA BIG ENOUGH FOR EVAPORATION
C
               QDUM = MDOTR*(HDEW - H5)
               TAIR_END = TAIR + QDUM/CAIR
               QMAX = CMINTP*(TAIR_END - T5)
 
               EFF_TPC = QDUM/QMAX
               CALL EXF(1,ALEFT,UTPE,CMINTP,CMAXTP,EFFTPC,DEXDAR)
 
 
               IF(EFFTPC .LE. EFF_TPC) THEN
                  ATPC = ATPC + ALEFT
                  HAVE_NOT_USED_FULL_AREA = .FALSE.
C
C          BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
C          TWO PHASE REGION
C
C          INITIALIZE VARIABLES
C
               ELSE
                  ADUM = 0.9*ALEFT
                  LOOKING_FOR_AREA = .TRUE.
 
                  ILOOK = 0
                  DO WHILE (LOOKING_FOR_AREA)
                  ILOOK = ILOOK + 1
                     CALL EXF(1,ADUM,UTPE,CMINTP,CMAXTP,EFFTPC,DEXDAR)
                     ERROR = ABS(EFFTPC - EFF_TPC)
                     IF(ERROR .LE. AREA_TOL .OR. ILOOK .GE. 10) THEN
                        LOOKING_FOR_AREA = .FALSE.
                        CYCLE
                     END IF
 
                     DAREA = - (EFFTPC - EFF_TPC)/DEXDAR
                     DAREA_MIN = -0.75*ADUM
                     DAREA_MAX = 0.50*(ALEFT - ADUM)
 
                     IF(DAREA .LT. DAREA_MIN) DAREA = DAREA_MIN
                     IF(DAREA .GT. DAREA_MAX) DAREA = DAREA_MAX
 
                     IF(ABS(DAREA) .LE. 0.001*ATOTE) THEN
                        LOOKING_FOR_AREA = .FALSE.
                        CYCLE
                     END IF
 
                     ADUM  = ADUM + DAREA
                  END DO
                  ATPC = ATPC + ADUM
 
               END IF
 
               QTPC = QTPC + EFFTPC*QMAX
               TAIR = TAIR + EFFTPC*QMAX/CAIR
            END IF
 
            ALEFT = ATOTE - ATPC
            H5 = H5 + DELH
            T5 = TDEW
            P5 = P5 + DELP
            N = N + 1
         END DO
         IF(ALEFT .LE. 0.0) HAVE_NOT_USED_FULL_AREA = .FALSE.
C
C          CONTINUE WITH DESUPERHEATING AREA
C
         HDEW = HDEW_S
         TDEW = TDEW_S
 
         IF(HAVE_NOT_USED_FULL_AREA) THEN
            CR  = MDOTR*CPR
C
C          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
C
            IF(CAIR .LE. CR) THEN
               CMINDS = CAIR
               CMAXDS = CR
            ELSE
               CMINDS = CR
               CMAXDS = CAIR
            END IF
C
C          DETERMINE THE NET HEAT TRANSFER
C
            CALL EXF(1,ALEFT,USUPE,CMINDS,CMAXDS,EFFDSC,DEXDAR)
            QSUP = CMINDS*EFFDSC*(TS3 - TDEW)
            TAIR = TAIR + QSUP/CAIR
 
            ASUP = ALEFT
         END IF
 
         QTOTE = QSUP + QTPC
         ERROR_Q = ABS(1 - QTOTE_LAST/QTOTE)
         QTOTE_LAST = QTOTE
 
         ERROR = ABS(TAIR - TS3)
         IF(ERROR .LT. 0.05 .OR. ERROR_Q .LE. 0.01
     .                      .OR. ICOUNT .GE. 10) THEN
            CONVERGED = .TRUE.
            CYCLE
         ELSE
            DEL_AIR = TAIR - TS3
 
            TAIR_NEW = TAIR_GUESS - 0.5*DEL_AIR
            IF(TAIR_NEW .LE. T5_S) TAIR_NEW = 0.9*T5_S + 0.1*TAIR_GUESS
            TAIR = TAIR_NEW
            TAIR_GUESS = TAIR
 
            T5 = T5_S
            H5 = H5_S
            HDEW = HDEW_S
            TDEW = TDEW_S
            P5 = PIN
 
            QSUP = 0.0
            QTPC = 0.0
            ASUP = 0
            ATPC = 0
 
            HAVE_NOT_USED_FULL_AREA = .TRUE.
         END IF
      END DO
C
C        CALCULATE THE FRACTIONAL SUBCOOLING AND SUPERHEATING REGIONS
C
      FSUPE = ASUP/ATOTE
 
      uaff = atote * fsupe * usupe + atote * (1.0 - fsupe) * utpe
 
      RETURN
      END
C
      SUBROUTINE FFCROSS(H5_S,T5_S,HDEW_S,TDEW_S,TS3,CPR,PIN,POUT,X,
     .                   NUM_ZONE,QTOTE,FSUPE)
C     ******************************************************************
C     *    SUBROUTINE FFCOUNT - SOLVES FOR THE FRESH FOOD EVAPORATOR   *
C     *    HEAT TRANSFER FOR CROSS FLOW HEAT EXCHANGER                 *
C     ******************************************************************
C
      LOGICAL LOOKING_FOR_AREA, HAVE_NOT_USED_FULL_AREA
 
      REAL MDOTR
 
      DIMENSION X(5),XL(5),XV(5)
 
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MDOTR,ETAV,SEFF
      COMMON/HTEXS/CFMC,CFME,CMFF,UAF,ETAC,ETAE,ETAF
      COMMON /FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
      COMMON / DIAG / IM, IC,IE
 
      COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
     .              N_EVAP, N_COND
 
      DATA AREA_TOL / 0.001 /
C
C          INITIALIZE
C
      T5 = T5_S
      H5 = H5_S
      HDEW = HDEW_S
      TDEW = TDEW_S
 
      DELP = (PIN  - POUT)/FLOAT(NUM_ZONE)
      DELH = (HDEW - H5)/FLOAT(NUM_ZONE)
      P5 = POUT
 
      QSUP = 0.0
      QTPC = 0.0
 
      ASUP = 0
      ATPC = 0
 
      TAIR = TS3
      CAIR = CFME
      HAVE_NOT_USED_FULL_AREA = .TRUE.
C
C          BEGIN  WITH TWO-PHASE AREA
C
      ALEFT = ATOTE
 
      N = 1
      DO WHILE (N .LE. NUM_ZONE)
         PDEW = P5 + DELP
         HDEW = H5 + DELH
         CALL HPIN(HDEW,PDEW,X,TDEW,XQ,XL,XV,VL,VV,HL,HV)
 
         IF(HAVE_NOT_USED_FULL_AREA) THEN
            CPRTP = (HDEW-H5)/ABS(TDEW-T5+0.0001)
            CRTP  = MDOTR*CPRTP
C
C          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
C
            CAIR = (ALEFT/ATOTE)*CFME
            IF(CAIR .LE. CRTP) THEN
               CMINTP = CAIR
               CMAXTP = CRTP
            ELSE
               CMINTP = CRTP
               CMAXTP = CAIR
            END IF
C
C          IS AREA BIG ENOUGH FOR CONDENSATION
C
            QMAX = CMINTP*(TAIR - T5)
            QDUM = MDOTR*(HDEW - H5)
 
            EFF_TPC = QDUM/QMAX
            CALL EXF(2,ALEFT,UTPE,CMINTP,CMAXTP,EFFTPC,DEXDAR)
 
 
            IF(EFFTPC .LE. EFF_TPC) THEN                   !Need more area
               ATPC = ATPC + ALEFT
               HAVE_NOT_USED_FULL_AREA = .FALSE.
C
C          BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
C          TWO PHASE REGION
C
C          INITIALIZE VARIABLES
C
            ELSE
               ADUM = 0.9*ALEFT
               LOOKING_FOR_AREA = .TRUE.
 
               ICOUNT = 0
               QTOL = 1.0
               DO WHILE (LOOKING_FOR_AREA)
                  ICOUNT = ICOUNT + 1
                  IF(ICOUNT .GT. 100) THEN
                     LOOKING_FOR_AREA = .FALSE.
                     CYCLE
                  END IF
 
 
                  CAIR = (ADUM/ATOTE)*CFME
                  IF(CAIR .LE. CRTP) THEN
                     CMINTP = CAIR
                     CMAXTP = CRTP
                  ELSE
                     CMINTP = CRTP
                     CMAXTP = CAIR
                  END IF
 
                  QMAX = CMINTP*(TAIR - T5)
                  EFF_TPC = QDUM/QMAX
 
                  CALL EXF(2,ADUM,UTPE,CMINTP,CMAXTP,EFFTPC,DEXDAR)
 
                  ERROR = ABS(QTOL)
                  IF(ERROR .LE. AREA_TOL) THEN
                     LOOKING_FOR_AREA = .FALSE.
                     CYCLE
                  END IF
 
                  QRAT  = EFFTPC*QMAX/QDUM
                  QTOL = 1.0 - QRAT
 
                  DAREA = ADUM*(1.0 - QRAT)
 
                  DAREA_MIN = -0.75*ADUM
                  DAREA_MAX = 0.50*(ALEFT - ADUM)
 
                  IF(DAREA .LT. DAREA_MIN) DAREA = DAREA_MIN
                  IF(DAREA .GT. DAREA_MAX) DAREA = DAREA_MAX
 
                  ADUM  = ADUM + DAREA
               END DO
               ATPC = ATPC + ADUM
 
            END IF
 
            QTPC = QTPC + EFFTPC*CMINTP*(TAIR - T5)
         END IF
 
         ALEFT = ATOTE - ATPC
         H5 = H5 + DELH
         T5 = TDEW
         P5 = P5 + DELP
         N = N + 1
      END DO
      IF(ALEFT .LE. 0.0) HAVE_NOT_USED_FULL_AREA = .FALSE.
C
C          CONTINUE WITH DESUPERHEATING AREA
C
 
      HDEW = HDEW_S
      TDEW = TDEW_S
 
      IF(HAVE_NOT_USED_FULL_AREA) THEN
         CR  = MDOTR*CPR
C
C          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
C
         CAIR = (ALEFT/ATOTE)*CFME
         IF(CAIR .LE. CR) THEN
            CMINDS = CAIR
            CMAXDS = CR
         ELSE
            CMINDS = CR
            CMAXDS = CAIR
         END IF
C
C          DETERMINE THE NET HEAT TRANSFER
C
         CALL EXF(2,ALEFT,USUPE,CMINDS,CMAXDS,EFFDSC,DEXDAR)
         QSUP = CMINDS*EFFDSC*(TS3 - TDEW)
 
         ASUP = ALEFT
      END IF
C
C        CALCULATE THE FRACTIONAL SUBCOOLING AND SUPERHEATING REGIONS
C
      FSUPE = ASUP/ATOTE
 
      QTOTE = QSUP + QTPC
 
      uaff = atote * fsupe * usupe + atote * (1.0 - fsupe) * utpe
 
      RETURN
      END
