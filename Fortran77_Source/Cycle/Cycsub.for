$DEBUG
      SUBROUTINE COMP(H, P, X, T, CV, CP, HOUT, MEFF, QHILO,
     .           QCAN, VSUC, V, VV2, TSUC, TDISC, TAMB, GAMA, RN, ETAS)
C     *****************************************************************
C     *    COMPRESSOR MODEL                                           *
C     *****************************************************************
C
      LOGICAL LCRIT
      REAL MEFF, MREF
      DIMENSION H(16),P(16),X(5),XQ(16),XL(5,16),XV(5,16),T(16),V(16)
 
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
      COMMON / MAPDAT / IMAP, ICOMP, ICOOL, EER, SIZE, DISPL, EFFC,
     .                  SPEEDN, IREAD
 
      DATA R /8.314/
      DATA TOLS/0.1/
C
C          CONVERSION FUNCTIONS
C
      F_TO_K(TF) = (TF + 459.7)/1.8
C
C          SET UP INITIAL GUESS FOR SUCTION PORT CONDITIONS
C
C
      TSUC = T(1)
      VSUC = V(1)
      CALL HCVCPS (1,TSUC,VSUC,X,HSUC,CV,CP,VS)
      CALL BUBLP(P(2), XL(1,2), X, TDEW, XXX, VDEW, .FALSE., LCRIT)
C
C          CALCULATE ISENTROPIC CONDITIONS BASED ON SHELL INLET
C
      SSUC = ENTROP(TSUC,VSUC,X)
      CALL SPIN (SSUC,P(2),X,T(2),XQ(2),XL(1,2),XV(1,2),VL2,
     .           VV2,SL,SV)
      IF(XQ(2) .LT. 1.0) THEN
         CALL HCVCPS (1,T(2),VL2,XL(1,2),HL2,CV,CP,VS)
         CALL HCVCPS (3,T(2),VV2,XV(1,2),HV2,CV,CP,VS)
         H(2) = XQ(2)*HV2 + (1.0-XQ(2))*HL2
      ELSE
         CALL HCVCPS (3,T(2),VV2,X,H(2),CV,CP,VS)
      END IF
      HISEN = H(2)
C
C          SELECT MODEL
C
      IF(IMAP .EQ. 1) THEN                                 !Map model
         IF(ICOOL .EQ. 0) THEN
            TSUC = 389.59 - 64.815*EER                     !Degrees F
         ELSE
            TSUC = 337.84 - 57.895*EER
         END IF
 
         TIN = 1.8*T(1) - 459.7
         TSUC = TSUC + TIN
 
         T_EVAP = 1.8*T(12) - 459.7
         T_COND = 1.8*TDEW  - 459.7
 
         TSUC = TSUC + 0.2*(T_COND - 130.0) - 0.2*(T_EVAP + 10.0)
 
         IF(ICOMP .EQ. 2) TSUC = TIN + 30.0                !Rotary
 
         TSUC = F_TO_K(TSUC)                               !K
         VSUC = VSUC*TSUC/T(1)                             !Suction density
 
         CALL ESPAR(0, TSUC, X, AMIX, BMIX)
         CALL VIT(TSUC, P(1), AMIX, BMIX, VSUC, .FALSE., LCRIT)
         CALL HCVCPS (3, TSUC, VSUC, X, H_SUC, CV, CP, DUM1)
         SSUC = ENTROP(TSUC, VSUC, X)                      !Suction entropy
 
         CALL SPIN(SSUC, P(2), X, T2S, XQ(2), XL(1,2), XV(1,2),
     .             VL2S, VV2S, SL2S, SV2S)
         CALL HCVCPS (1, T2S, VV2S, X, H2S, DUM1, DUM2, DUM3)
 
         IF(Icomp .EQ. 2) THEN
            ETAS = Effc*(1.0 - 0.0010*(T_COND - 130.0))
     .                 *(1.0 + 0.0030*(T_EVAP + 10))
         ELSE
            ETAS = Effc*(1.0 + 0.0010*(T_COND - 130.0))
     .                 *(1.0 + 0.0020*(T_EVAP + 10))
         END IF
 
 
         W = (H2S - H_SUC)/EFFC
         if(icool .eq. 1) W = (H2S - H_SUC)/Etas
 
         GAMA = CP/CV
         RN = 0.97*GAMA
         RINV = 1.0/RN
         PR = P(2)/P(1)
 
C
C          ESTIMATE CYCLINDER TEMPERATURE AND CAN OUTLET TEMPERATURE
C
         TDISC = TSUC*(P(2)/P(1))**(1.0-1.0/RN)
         CALL ESPAR(0, TDISC, X, AMIX, BMIX)
         VVD = R*TDISC/P(2)
         CALL VIT(TDISC, P(2), AMIX, BMIX, VVD, .FALSE., LCRIT)
         CALL HCVCPS (1, TDISC, VVD, X, HDISC, CV, CP, DUM1)
 
         ETA_ISEN = (H2S - H_SUC)/(HDISC - H_SUC)
 
         IF(ICOOL .EQ. 0) THEN
            RATIO = 0.68 - 0.05*EER
         ELSE
            RATIO = 0.90 - 0.07*EER
         END IF
 
         T(2) = TDISC - RATIO*(TDISC - TAMB)
 
         CALL ESPAR(0, T(2), X, AMIX, BMIX)
         VV2 = R*T(2)/P(2)
         CALL VIT(T(2), P(2), AMIX, BMIX, VV2, .FALSE., LCRIT)
         CALL HCVCPS (1, T(2), VV2, X, H(2), CV, CP, DUM1)
 
         QCAN  = 1.0 - (H(2) - H(1))/W
         QHILO = (HDISC - H(2))/W
      ELSE                                                 !Physical model
C
C          FIND ENTROPY OF SUCTION GAS AND TEMPERATURE FOR DISCHARGE
C          GAS FROM AN ISENTROPIC EXPANSION
C
         ITER = 0
         ERROR = TOLS + 1
 
         TSUC = T(1) + 3.0
         VSUC = V(1)*TSUC/T(1)                             !Suction density
         CALL HCVCPS (1,TSUC,VSUC,X,HSUC,CV,CP,VS)
 
         DO WHILE (ERROR .GT. TOLS .AND. ITER .LT. 10)
            ITER = ITER + 1
            SSUC = ENTROP(TSUC,VSUC,X)
            CALL SPIN (SSUC,P(2),X,T(2),XQ(2),XL(1,2),XV(1,2),VL2,
     .                 VV2,SL,SV)
            IF(XQ(2) .LT. 1.0) THEN
               CALL HCVCPS (1,T(2),VL2,XL(1,2),HL2,CV,CP,VS)
               CALL HCVCPS (3,T(2),VV2,XV(1,2),HV2,CV,CP,VS)
               H(2) = XQ(2)*HV2 + (1.0-XQ(2))*HL2
            ELSE
               CALL HCVCPS (3,T(2),VV2,X,H(2),CV,CP,VS)
            END IF
C
C               DETERMINE ISENTROPIC EFFICIENCY
C
            GAMA = CP/CV
            RN = 0.97*GAMA
            RINV = 1.0/RN
            PR = P(2)/P(1)
            H(2)=HSUC+(H(2)-HSUC)/SEFF
C
C               RE-CALCULATE SUCTION TEMPERATURE AND COMPARE
C               WITH OLD VALUE
C
            COEF1 = (1.0 - MEFF - (MEFF*QCAN-QHILO)/(1.0
     .                   - QCAN))/(1.0 + QHILO/(1.0 - QCAN))
            H1P = H(1) + COEF1*(H(2) - H(1))
            CALL HPIN(H1P, P(1), X, T1P, XQ1, XL(1,2), XV(1,2), VL2,
     .                VV2, HL2, HV2)
 
            IF(ICOMP .EQ. 2) T1P = TSUC
 
            ERROR = ABS(T1P - TSUC)
 
            IF(ICOMP .EQ. 1) THEN
               TSUC = T1P
               HSUC = H1P
               VSUC = VV2
            END IF
 
         END DO
C
C          CORRECT DISCHARGE CONDITION FOR CAN LOSS
C
         HDISC = H(2)
         CALL HPIN(HDISC, P(2), X, TDISC, XQ(2), XL(1,2), XV(1,2), VL2,
     .             VV2, HL2, HV2)
         H(2) = H(1) + ((H(2) - H(1)))/(1.0 + QHILO/(1.0 - QCAN))
         CALL HPIN(H(2), P(2), X, T(2), XQ(2), XL(1,2), XV(1,2), VL2,
     .             VV2, HL2, HV2)
 
      END IF
C
C          CALCULATE MASS FLOW RATE
C
      IF(ICOMP .EQ. 1) THEN
         ETAV = 0.92*(1.0 - CE*(PR**RINV - 1.0))
      ELSE
         ETAV = 1.00*(1.0 - CE*(PR**RINV - 1.0))
      END IF
      DISP = MREF*VSUC/(60.0*SPEED*ETAV)
 
      HOUT = H(2)
  !   ETAS = 100.0*(HISEN - H(1))/(H(2) - H(1))
      ETAS = ETA_ISEN
      RETURN
      END
C
      SUBROUTINE FRSH(H,T,TS3,TS4,TE,JE,QFRSH,ICONE)
C     *****************************************************************
C     *    CALCULATE FRESH FOOD SECTION EXIT TEMPERATURE              *
C     *****************************************************************
C
      REAL MREF
      DIMENSION H(16),T(16),TE(3),FTE(2)
C
      COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
      COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
 
      COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
     .              N_EVAP, N_COND
 
C
C         INITIALIZE
C
      ICONE = 0
      ALPHA = QFRSH/(MREF*(H(7) - H(5)))
      IF(QFRSH .EQ. 0.0) ALPHA = 0.01
C
C          ESTIMATE NEW VALUE FOR EXIT TEMPERATURE
C
      DELT = (TS3 - T(5))*(1.0 - 1.0/ALPHA)
      IF(-DELT .GT. TE(JE)) DELT = -0.5*TE(JE)
      TEOUT = TE(JE) + DELT
      TS4 = TS3 - QFRSH/CFME
      IF(TEOUT .GT. TS3) TEOUT = TS3
      FTE(JE) = TEOUT - TE(JE)
      IF(JE .EQ. 1) THEN
           TENEW = TEOUT
      ELSE
           TENEW = TEOUT
           IF(FTE(2) .NE. FTE(1)) THEN
                TENEW = TE(2) - FTE(2)*(TE(2) - TE(1))/(FTE(2) - FTE(1))
           END IF
           TE(1) = TE(2)
           FTE(1) = FTE(2)
      END IF
      TENEW = (TENEW + TE(JE))/2.
      IF(TENEW .GT. 1.05*TE(JE)) TENEW = 1.05*TE(JE)
      IF(TENEW .LT. 0.95*TE(JE)) TENEW = 0.95*TE(JE)
      IF(TENEW .GT. TS3) TENEW = TS3
      ERROR = ABS(TENEW - TE(JE))
      IF(ERROR .LT. TOL_FRSH) ICONE = 1
 
      jeold = je
      JE = 2
 
 !    if (jeold .eq. 1) then                                   !! 5/10/94
         TE(JE) = TENEW
 !    else
 !       if (abs(te(je) - tenew) .gt. 1.0) then                !! 5/10/94
 !          if(te(je) .gt. tenew) te(je) = te(je) - 1.0        !! 5/10/94
 !          if(te(je) .lt. tenew) te(je) = te(je) + 1.0        !! 5/10/94
 !       end if                                                !! 5/10/94
 !    end if
 
C
C          ADJUST EXIT AIR TEMP TO 90% APPROACH IF NATURAL CONVECTION
C
      IF(IFRSH .EQ. 0) THEN
           TS4 = 0.9*TE(JE) + 0.1*TS3
      END IF
      RETURN
      END
C
      SUBROUTINE PROGRS(ICYCL,H,HOUT,WM,FLOW,QCAN)
C     ******************************************************************
C     *    DISPLAY THE INTERMEDIATE SOLUTIONS ON THE SCREEN            *
C     ******************************************************************
C
      DIMENSION H(16)
C
C          ESTABLISH THE OUTPUT VALUES
C
      QE = ((H(7) - H(5))/WM)*(0.431*FLOW)
      QZ = ((H(9) - H(8))/WM)*(0.431*FLOW)
      W  = ((HOUT - H(1))/WM)*(0.431*FLOW)/(1.0 - QCAN)
      COPR = (QE + QZ)/W
C
C          OUTPUT RESULTS
C
      IF(ICYCL .EQ. 2) THEN
           CALL GOTOXY(62,16)
           CALL PRINT('Mass Flow:',10,-2)
           CALL PRINT(FLOW/2.2046,5,1)
           CALL GOTOXY(64,17)
           CALL PRINT('Freezer:',8,-2)
           CALL PRINT(IFIX(QZ/3.413),5,-1)
           CALL GOTOXY(61,18)
           CALL PRINT('Fresh Food:',11,-2)
           CALL PRINT(IFIX(QE/3.413),5,-1)
           CALL GOTOXY(61,19)
           CALL PRINT('Compressor:',11,-2)
           CALL PRINT(IFIX(W/3.413),5,-1)
           CALL GOTOXY(68,21)
           CALL PRINT('COP:',4,-2)
           CALL PRINT(COPR,5,2)
      ELSE
           CALL GOTOXY(62,18)
           CALL PRINT('Mass Flow:',10,-2)
           CALL PRINT(FLOW/2.2046,5,1)
           CALL GOTOXY(61,19)
           CALL PRINT('Evaporator:',11,-2)
           CALL PRINT(IFIX(QE/3.413),5,-1)
           CALL GOTOXY(61,20)
           CALL PRINT('Compressor:',11,-2)
           CALL PRINT(IFIX(W/3.413),5,-1)
           CALL GOTOXY(68,22)
           CALL PRINT('COP:',4,-2)
           CALL PRINT(COPR,5,2)
      END IF
      RETURN
      END
C
      SUBROUTINE SHWOUT(IROW,ICOL,ILEN,TITLE,VAR)
C     *****************************************************************
C     *     CREATE ONE LINE OF CHARACTER OUTPUT                       *
C     *****************************************************************
C
      CHARACTER TVAR(80),DVAR(6)
      CHARACTER*6 DUMMY,SPACE
      CHARACTER*80 TITLE,WORK
      EQUIVALENCE (WORK,TVAR(1)),     (DUMMY,DVAR(1))
      DATA SPACE /'      '/
C
C          BUILD UP THE CHARACTER VECTOR
C
      WORK = TITLE
      DUMMY = SPACE
      K = ILEN - 3
      SELECT CASE (K)
           CASE (1)
                WRITE (DUMMY,'(F4.1)') VAR
           CASE (2)
                WRITE (DUMMY,'(F5.1)') VAR
           CASE (3)
                WRITE (DUMMY,'(F6.1)') VAR
           CASE(4)
                WRITE (DUMMY,'(F7.1)') VAR
      END SELECT
C
C          TRANSFER CHARACTER BY CHARACTER
C
      K = 1
      DO WHILE (K .NE. 6)
           L = K + ICOL - 1
           TVAR(L) = DVAR(K)
           K = K + 1
      END DO
      TITLE = WORK
      IROW=1
      RETURN
      END
C
      SUBROUTINE EFCROSS(CRAT,NTU,EFFECT)
C     ******************************************************************
C     *     CALCULATES THE HEAT TRANSFER EFFECTIVENESS FOR A CROSS     *
C     *     FLOW HEAT EXCHANGER WITH BOTH FLUIDS UNMIXED               *
C     ******************************************************************
C
      REAL NTU
      DIMENSION A(4,6)
C
      DATA (A(I,1),I=1,4)/2.394292,2.410798,2.399687,2.359642/
      DATA (A(I,2),I=1,4)/-1.19402,-2.23391,-2.96882,-3.37650/
      DATA (A(I,3),I=1,4)/-1.45067,0.825900,2.367080,3.04862/
 
      DATA (A(I,4),I=1,4)/1.938453,0.051006,-1.23009,-1.63421/
      DATA (A(I,5),I=1,4)/-0.81305,-0.11891,0.373338,0.468741/
      DATA (A(I,6),I=1,4)/0.118651,0.023360,-0.04886,-0.05492/
C
C          FIND POSITION IN ARRAY BASED ON THE CAPACITY RATIO
C          OF THE TWO STREAMS
C
      IF(CRAT .GE. 0.00 .AND. CRAT .LE. 0.25) I=1
      IF(CRAT .GT. 0.25 .AND. CRAT .LE. 0.50) I=2
      IF(CRAT .GT. 0.50 .AND. CRAT .LE. 0.75) I=3
      IF(CRAT .GT. 0.75 .AND. CRAT .LE. 1.00) I=4
      IF(NTU .LE. 0.0) NTU = 0.0
      BETA = LOG10(NTU+1.0)
      EFFA = 0.0
      EFFB = 0.0
      J = 1
      DO WHILE (J .LE. 6)
           EX = 1.0*J
           IF(I .EQ. 1) THEN
                EFFA = 1.0 - EXP(-NTU)
           ELSE
                EFFA = EFFA + A((I-1),J)*BETA**EX
           END IF
           EFFB = EFFB + A(I,J)*BETA**EX
           J = J + 1
      END DO
      FRAC = (CRAT-(I-1)*0.25)/(I*0.25-(I-1)*0.25)
      EFFECT = EFFA + FRAC*(EFFB-EFFA)
      IF(EFFECT .GT. 1.0) EFFECT = 1.0
      RETURN
      END
