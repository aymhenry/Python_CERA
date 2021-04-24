$DEBUG
      SUBROUTINE COND(T,H,TBUB,HBUB,TS1,TS2,TC,CPRLIQ,QCONDS,QCONDC,
     .     QSCC,JC,ICONC)
C
C     *****************************************************************
C     *    CALCULATE CONDENSER EXIT TEMPERATURE                       *
C     *****************************************************************
C
      REAL MREF,MROLD
      DIMENSION H(16),T(16),TC(3)
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
      COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
      COMMON/SPECS/DTSUPE,DTSUBC
      COMMON/CONDEN/UDSC,UTPC,USCC,ATOTC,UACOND
      COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX
      COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
     .                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
     .                  CONDF_IN_WALL, CONDZ_IN_WALL
C
C         INITIALIZE
C
      ICONC = 0
      IF(JC .EQ. 1) THEN
           ICNT = 0
           MROLD = 0
      END IF
C
C
C          ESTIMATE NEW VALUE FOR EXIT TEMPERATURE
C
      QREF  = MREF*(H(14) - H(4))
      QCOND = QCONDS + QCONDC + QSCC
      EPS = QREF - QCOND
      DELT = EPS/UACOND
C
      IF(DELT .GT. 5.0) DELT = 5.0
      IF(DELT .LT. -5.0) DELT = -5.0
C
C     TCOUT = TC(JC) + 0.5*DELT
      TCOUT = TC(JC) + DELT
C
      TS2 = TS1 + (QCONDS + QCONDC +QSCC)/CFMC
      IF(ICOND .EQ. 0) TS2 = 0.9*T(4) + 0.1*TS1
C     IF(TCOUT .LT. TS1) TCOUT = TS1
      IF(TCOUT .LT. TS1) TCOUT = (TS1 + TC(JC))/2.0
C
      IF(ICNT .LE. 2) THEN
           TCNEW = TCOUT
      ELSE
           IF((TCOUT .GT. TC(1) .AND. TC(1) .GT. TC(2)) .OR.
     .        (TCOUT .LT. TC(1) .AND. TC(1) .LT. TC(2))) THEN
                TCNEW = 0.5*(TC(1) + TC(2))
C               TCNEW = 0.5*tcout + 0.5*TCNEW
           ELSE
                TCNEW = TCOUT
           END IF
           IF(TCNEW .LT. TS1) TCNEW = (TS1 + TC(JC))/2.0
           TC(1) = TC(2)
      END IF
C
C          CHECK CONVERGENCE
C
      ERRORT = ABS(TCNEW - TC(JC))
      ERRORM = ABS(MREF - MROLD)/MREF
      IF(ERRORT .LT. TOL_COND .AND. ERRORM .LE. TOL_MASS) ICONC = 1
      JC = 2
      ICNT = ICNT + 1
      TC(JC) = TCNEW
      MROLD = MREF
      RETURN
      END
