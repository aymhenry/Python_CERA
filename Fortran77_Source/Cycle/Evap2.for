$debug
      SUBROUTINE LOWEVP(ICYCL,ICNTRL,H,P,X,T,XQ,XL,XV,VL,VV,HL,HV,TS3,
     .     TS5,TS6,DPF,ETHX2,QFREZ,LQUIT)
C     *****************************************************************
C     *    FREEZER EVAPORATOR AND LOWER INTERCHANGER                  *
C     *****************************************************************
C
      CHARACTER KEY
      LOGICAL LCRIT,LQUIT
      REAL MREF
C
      DIMENSION H(16),P(16),X(5),XQ(16),XL(5,16),XV(5,16),VL(16)
      DIMENSION VV(16),T(16)
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
      COMMON/EVAPS/ITYPE, FRACT_FF, FRACT_FZ
      COMMON /FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
      COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
      COMMON/RDATA4/R
      COMMON / INWALL / UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
     .                  Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
     .                  CAPZ_IN_WALL, Q_FZ_FF
      COMMON /CABLOD/ FFASH,FAUXF,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
     .                FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
     .                FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
 
      COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
     .              N_EVAP, N_COND
 
      COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
     .                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
     .                  CONDF_IN_WALL, CONDZ_IN_WALL
 
      COMMON / PLSTIC / IWALL_FF, IWALL_FZ
C
      DATA NCALL/0/
      DATA SIGMA/2.0432E-7/, EPS/0.8/
C
C          SET UP PRESSURES AND QUALITIES
C
      P(10) = P(6)
      P(9) = P(5)
      P(8) = P(9) + DPF
      XQ(10) = 0
      ETHX = ETHX2
      TSAV = T(9)
C
C          FIND BUBBLE AND DEW POINT ENTHALPIES AT FREEZER PRESSURE
C
      CALL BUBLP(P(8),X,XV(1,8),TBUB,VL(8),VV(8),.TRUE.,LCRIT)
      CALL HCVCPS(1,TBUB,VL(8),X,HBUB,CV,CP,VS)
C
      IF(NCALL .EQ. 0) THEN
           CALL BUBLP(P(9),XL(1,9),X,TDEW,VL(9),VV(9),.FALSE.,LCRIT)
           CALL HCVCPS(1,TDEW,VV(9),X,HDEW,CV,CP,VS)
           CREF = MREF*(HDEW-HBUB)/(TDEW-TBUB+0.001)
           IF(CREF .LE. 0.1) then
                             CREF = 1000000.0  !!!!   5/9/94
           end if
           T(10) = TS5
           NCALL = 1
      END IF
C
C          SET FLAG FOR POSSIBLE SOLUTION AND HANDLE SINGLE EVAP CASE
C
      IFREZ2 = 1
      IF(TBUB .GE. TS5) IFREZ2 = 0
      VL(10) = VL(6)
      IF(IFREZ2 .EQ. 0) THEN
           H(9) = H(6)
           H(10) = H(6)
           T(10) = T(6)
      END IF
      IF(ITYPE .EQ. 1)  ETHX = 0
!!    IF(ETHX .EQ. 0.0) H(10) = H(6)
C
C          BEGIN ITERATION FOR TEMPERATURE AT POINT 10
C
      ITER = 1
   10 CONTINUE
      ITER = ITER + 1
      CALL INCHR(0,J,KEY)
      IF(J .EQ. 1) CALL FINISH
      IF(J .EQ. 68) LQUIT = .TRUE.
      TSHOW = T(10) - 273.11
      IF(ICYCL .EQ. 2) THEN
           CALL GOTOXY(2,21)
           CALL PRINT(TSHOW,5,1)
      END IF
      CALL HCVCPS(1,T(10),VL(10),X,H(10),CV,CP,VS)
      CALL HPIN(H(10),P(10),X,T10,XQ(10),XL(1,10),XV(1,10),
     .     VL(10),VV(10),HL,HV)
      H(8) = H(10)
      CALL HPIN(H(8),P(8),X,T(8),XQ(8),XL(1,8),XV(1,8),
     .      VL(8),VV(8),HL,HV)
      TSHOW = T(8) - 273.11
      IF(ICYCL .EQ. 2) THEN
           CALL GOTOXY(14,21)
      ELSE
           CALL GOTOXY(14,15)
      END IF
      CALL PRINT(TSHOW,5,1)
C
C          DETERMINE CMIN AND CMAX
C
      IF(CFMF .LE. CREF) THEN
            CMIN = CFMF
            CMAX = CREF
      ELSE
            CMIN = CREF
            CMAX = CFMF
      END IF
      CAPRAT = CMIN/CMAX
      IF(CMIN .LE. 0.0) CMIN=0.001
      FNTU = UAF/CMIN
      IF(FNTU .LT. 0.0) FNTU = 0.0
C
C          CALCULATE EFFECTIVENESS
C
      uafz = uaf
      IF(IFREZ2 .EQ. 1) THEN
           SELECT CASE (IFREZ)
                CASE (0)
                     TAVE = (T(8) + T(9))/2.0
                     if(t(9) .lt. -1000.0) tave = t(8)   !!! Jan 20, 1993
                     IF(TAVE .GT. TS5) TAVE = TS5 - 1.0
       !!            QMAX = 1.25*MREF*(HDEW - H(8))      !! 5/9/94
                     QMAX = 0.90*MREF*(H(7)- H(6))       !! 5/9/94
                     HRAD = SIGMA*(TAVE + TS5)*(TAVE**2 + TS5**2)*EPS
                     DELTAT = TS5 - TAVE
                     IF(DELTAT .LE. 0.0) DELTAT = 0.0001
                     DELTA = DELTAT*1.8
 
                     TBAR = 0.67*TAVE + 0.33*TS5
                     A_NAT = 0.239 + 3.34E-04*(273.0 - TBAR)
                     HNAT = A_NAT*(DELTA**0.33)*20.44
 
C
C          MAKE APPROXIMATE CORRECTIONS FOR VIEW FACTORS AND ASSUMED
C          ORIENTATION OF THE EVAPORATOR PANELS.
C
      !              HRAD = (1.0 - FRACT_FZ)*HRAD
      !              HNAT = 0.5*HNAT
 
                     UAIR = HRAD + HNAT
                     IF(iwall_fz .EQ. 1) THEN
                        uair = 1.0/(1.0/uair + 0.1389/20.44)
                     END IF
 
                     QFREZ = UAF*UAIR*DELTAT
                     uafz = uaf * uair
 
                     TENV = (TROOM + 459.6)/1.8
 
                     QFREZ = QFREZ + 1.8*UA_FZ*(TENV - TAVE)*1.0548
     .                             + 1.8*UA_ML*(TS3  - TAVE)*1.0548
     .                             + Q_HXS_FZ
 
                     IF(QFREZ .GT. QMAX) QFREZ = QMAX
                CASE (1)
                     CALL EFCROSS(CAPRAT,FNTU,EXFR)
                     QFREZ = EXFR*CMIN*(TS5 - T(8))
                     ETAF = EXFR
                CASE (2)
                     XX = 1.0 - CAPRAT
                     XXX = EXP(-FNTU*XX)
                     EXFR = (1.0-XXX)/(1.0-CAPRAT*XXX)
                     QFREZ = EXFR*CMIN*(TS5 - T(8))
                     ETAF = EXFR
           END SELECT
           TS6 = TS5 - QFREZ/CFMF
           IF(IFREZ .EQ. 0) TS6 = 0.9*TAVE + 0.1*TS5
      ELSE
           QFREZ = 0.0
           TS6 = TS5
      END IF
C
C          UPDATE ENTHALPY ACROSS EVAPORATOR
C
      H(9) = H(8) + QFREZ/MREF
      CALL HPIN(H(9),P(9),X,T(9),XQ(9),XL(1,9),XV(1,9),
     .     VL(9),VV(9),HL,HV)
      TSHOW = T(9) - 273.11
      IF(ICYCL .EQ. 2) THEN
           CALL GOTOXY(52,21)
           CALL PRINT(TSHOW,5,1)
      END IF
      IF(IFREZ2 .EQ. 0) GO TO 20
C
C          GET NEW GUESS FOR TEMPERATURE AT POINT 10
C
      TOLD = T(10)
      IF(ICYCL .NE. 2 .OR. ICNTRL .NE. 2) THEN
           TNEW  = T(6) - ETHX*(T(6) - T(9))
      ELSE
           CALL BUBLP(P(9),XLD,X,TD,VLD,VVD,.FALSE.,LCRIT)
           IF(TD .GT. T(6)) THEN
                TNEW  = T(6) - ETHX*(T(6) - T(9))
           ELSE
                CALL HCVCPS(1,T(9),VL(10),X,HHIGH,CV,CP,VS)
                CALL ESPAR(0,T(6),X,A1,B1)
                VGUESS = R*T(6)/P(9)
                CALL VIT(T(6),P(9),A1,V1,VGUESS,.FALSE.,LCRIT)
                CALL HCVCPS(1,T(6),VGUESS,X,HLOW,CV,CP,VS)
                DH = AMIN1((HLOW - H(9)),(H(6)-HHIGH))
                H(10) = H(6) - DH
                CALL HPIN(H(10),P(10),X,T(10),XQ(10),XL(1,10),
     .               XV(1,10),VL(10),VV(10),HL,HV)
           END IF
      END IF
C
C          CORRECT GUESS IF NECESSARY AND CALCULATE ERROR
C
      IF(T(9) .GT. T(6)) THEN
           TNEW = T(10) - 0.9*ERROR
           T(10) = TNEW
      END IF
      T(10) = TNEW
      ERROR = TSAV - T(9)
      TSAV = T(9)
      IF(ABS(ERROR) .LT. TOL_FRZ) GO TO 20
      CREF = MREF*ABS((H(9) - H(8))/(T(9)-T(8)+0.0001))
           IF(CREF .LE. 0.1) then
                             CREF = 1000000.0  !!!!   /5/9/94
           end if
      ITER = ITER + 1
 
      if (abs(told - tnew) .GT. 2.0) then            !! 5/9/94
         if (told .gt. tnew) tnew = told - 2.0      !! 5/9/94
         if (told .lt. tnew) tnew = told + 2.0      !! 5/9/94
      end if
 
      IF(ITER .GT. 2) T(10) = 0.5*(TOLD + TNEW)      !! 5/9/94
      IF(ITER .GT. 10) GO TO 20
      GO TO 10
C
C          END OF ITERATION.  CALCULATE NEEDED PARAMETERS
C
   20 CONTINUE
      H(5) = H(9) + (H(6) - H(10))
      CALL HPIN(H(5),P(5),X,T(5),XQ(5),XL(1,5),XV(1,5),VL(5),VV(5),
     .     HL,HV)
      CALL HPIN(H(10),P(10),X,T(10),XQ(10),XL(1,10),XV(1,10),VL(10),
     .     VV(10),HL,HV)
      TSHOW = T(5) - 273.11
      IF(ICYCL .EQ. 2) THEN
           CALL GOTOXY(52,11)
           CALL PRINT(TSHOW,5,1)
      END IF
      RETURN
      END
C
      SUBROUTINE FFNAT(T5,H5,T7,TDEW,HDEW,TS3,CPRVAP,QTOTE,FSUPE,
     +                 IRFTYP)
C     ******************************************************************
C     *    SUBROUTINE  FFNAT - CALCULATES THE FRESH FOOD EVAPORATOR    *
C     *    HEAT  TRANSFER FOR A NATURAL CONVECTION  EVAPORATOR         *
C     ******************************************************************
      REAL MDOTR
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MDOTR,ETAV,SEFF
      COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
      COMMON /FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
      COMMON/EVAPS/ITYPE, FRACT_FF, FRACT_FZ
      COMMON/SPECS/DTSUPE,DTSUBC
      COMMON / INWALL / UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
     .                  Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
     .                  CAPZ_IN_WALL, Q_FZ_FF
      COMMON /CABLOD/ FFASH,FAUXF,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
     .                FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
     .                FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
      COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
     .                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
     .                  CONDF_IN_WALL, CONDZ_IN_WALL
      COMMON / PLSTIC / IWALL_FF, IWALL_FZ
 
      DATA SIGMA/2.04326E-7/, EPS/0.8/
C
C          CALCULATE THE RADIATION HEAT TRANSFER HEAT TRANSFER
C          COEFFICIENT USING SMALL DELTA T APPROXIMATION (BLACK BODY)
C          USE THE REFRIGERANT DEW POINT TO EVALUATE H RADIATION
C
      TENV = (TROOM + 459.6)/1.8
      TAVE = (T5+T7)/2.0
 
      TAIR = TS3
      FZTMPK = (FZTEMP + 459.6)/1.8
!! void for now 5/22/00    IF (IRFTYP .EQ. 6) TAIR = (TS3 + FZTMPK) / 2.0
 
      HRAD = SIGMA*(TAVE + TAIR)*(TAVE**2 + TAIR**2)*EPS
 
C
C          GET THE NET EVAPORATOR AREA
C
      AEVAP = ATOTE
      IF (IRFTYP .EQ. 6) AEVAP = ATOTE + ATOTE
C
C          CALCULATE THE NATURAL CONVECTION HEAT TRANSFER COEFFICIENT
C
      DELTAT = TAIR - TAVE
      IF(DELTAT .LE. 0.0) DELTAT = 0.0001
      DELTA = DELTAT*1.8
 
      TBAR = 0.67*TAVE + 0.33*TAIR
      A_NAT = 0.239 + 3.34E-04*(273.0 - TBAR)
 
 !    HRAD = HRAD*(1.0 - FRACT_FF)
      HNAT = A_NAT*DELTA**0.33*20.44
 
C
C          CALCULATE COMBINED AIR-SIDE HEAT TRANSFER COEFFICIENT
C
      UAIR = HRAD + HNAT
 
      IF(iwall_ff .EQ. 1) THEN
        uair = 1.0/(1.0/uair + 0.1389/20.44)
      END IF
 
      Q_IN_WALL = 1.0548*1.8*UA_FF*(TENV - TAVE)/AEVAP
     .          + Q_HXS_FF/AEVAP
C
C          CALCULATE THE HEAT TRANSFER ASSUMING THAT THE AIR SIDE
C          RESISTANCE DOMINATES
C
C          CALCULATE THE ARE NECESSARY TO EVAPORATE THE REFRIGERANT
C
      QTPNEC = MDOTR*(HDEW-H5)
      ATPNEC = QTPNEC/(UAIR*DELTAT + Q_IN_WALL)
C
C          CALCULATE THE SUPERHEATING AREA FRACTION
C
      IF (ATPNEC .LT. AEVAP) THEN
           QTPE = QTPNEC
           ASUPE = AEVAP - ATPNEC
           QSUPMX = MDOTR*CPRVAP*(TAIR-TDEW)
           QSUPE = UAIR*ASUPE*DELTAT + ASUPE*Q_IN_WALL
           IF (QSUPE .GT. QSUPMX) QSUPE = QSUPMX
           QTOTE = QTPE + QSUPE
           FSUPE = ASUPE/AEVAP
      ELSE
           QTOTE = UAIR*AEVAP*DELTAT + AEVAP*Q_IN_WALL
           FSUPE = 0
      END IF
 
      uaff = uair * aevap     !! 1/12/95
 
      RETURN
      END
C
      SUBROUTINE MIXAIR(CAP,QFF,QFZ,TFF,TFZ,CFME,TIN,X)
C     ******************************************************************
C     *     CALCULATE INLET TEMPERATURE TO THE EVAPORATOR              *
C     ******************************************************************
C
C          SET UP THE QUADRATIC EQUATION COEFFICIENTS
C
      COMMON /FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
 
      A = 1.08*CFME*(TFF - TFZ)/CAP
      B = - (A+1.0)
      C = QFF/(QFF+QFZ)
C
C          SOLVE THE QUADRATIC EQUATION
C
      X = - B/(2.0*A) - SQRT(B**2 - 4.0*A*C)/(2.0*A)
      TIN = X*TFF + (1.0 - X)*TFZ
      FF_AIR = X
      RETURN
      END
C
      SUBROUTINE WARBLE
C     ******************************************************************
C     *         MAKE A WARBLE SOUND                                    *
C     ******************************************************************
      INTEGER*2 IPTCH1,IPTCH2,IDUR1,IDUR2
      DATA IPTCH1,IPTCH2,IDUR1,IDUR2/55,40,16,24/
C
C          CALL ASSEMBLY ROUTINE SOUND THREE TIMES
C
      CALL SOUND(IPTCH1,IDUR1)
      CALL SOUND(IPTCH2,IDUR1)
      CALL SOUND(IPTCH1,IDUR2)
      RETURN
      END
