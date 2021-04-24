$DEBUG
      SUBROUTINE MAP(ICOMP, ICOOL, EER, SIZE, DISPL, ETAC, CE, SPEEDN)
C     ******************************************************************
C     *    USE COMPRESSOR MAP DATA TO ESTIMATE COMPRESSOR BEHAVIOR     *
C     ******************************************************************
C
C     INPUT PARAMETERS
C     ----------------
C        ICOMP                    TYPE OF COMPRESSOR:
C                                   1=RECIPROCATING, 2=ROTARY
C        ICOOL                    TYPE OF COMPRESSOR CAN COOLING:
C                                   0=STATIC, 1=FAN FORCED
C        EER                      EER AT RATING CONDITIONS
C        SIZE                     CAPACITY (BTUH) AT RATING CONDITIONS
C        DISPL                    DISPLACEMENT (CU-IN)
C
C     OUTPUT PARAMETERS
C     ----------------
C        ETAC                     COMPRESSOR ISENTROPIC EFFICIENCY,
C                                 INCLUDING MECHANICAL AND MOTOR LOSSES
C        CE                       CLEARANCE VOLUME (FRACTION)
C
      LOGICAL LCRIT, EXISTS
      REAL K, MASS, MOTOR
 
      DIMENSION X(5),  IR(5),  F(5,5), XL(5,5), XV(5,5)
 
      DIMENSION CAP(3,3), PWR(3,3), ETAS(3,3), ETAV(3,3), MASS(3,3),
     .          CEIJ(3,3)
 
          !Note: first  index = cond temperature from 110 to 130 F
          !      second index = evap temperature from -20 to   0 F
 
      DATA IN / 1 /
      DATA T90 / 305.3889 /                      !90F in Kelvin
      DATA ETAP / 0.97 /                         !Get k from gamma
C
C          CONVERSION FUNCTIONS
C
      F_TO_K(T) = (T + 459.7)/1.8
C
C          NORMALIZE
C
      ITAB = 0
      DO I = 1,5
         X(I) = 0
         DO J = 1,5
            F(I,J) = 0
         END DO
      END DO
 
      X(1) = 1
      IR(1) = 2                                            !Assume CFC-12
 
      CALL BCONST(1,IR,F)
 
      IF(ITAB .EQ. 0) EXISTS = .FALSE.
 
      MOTOR = 3.413*SIZE/EER                     !Motor power in Btuh
      DO I = 1,3
         DO J = 1,3
            IF(.NOT. EXISTS) THEN
               CAP(I,J) = 1.0
               PWR(I,J) = 1.0
            END IF
 
            CAP(I,J) = 1.0548*CAP(I,J)*SIZE       !kJ/hr
            PWR(I,J) = 1.0548*PWR(I,J)*MOTOR      !kj/hr
         END DO
      END DO
C
C          CALCULATE THE MASS FLOWS ASSUMING 90 F LIQUID AND VAPOR TEMPS
C
      DO I = 1, 3
         T_COND = 100 + 10*I
         T_COND = F_TO_K(T_COND)
         CALL BUBLT(T_COND, X, XV, P_COND, VL, VV, .TRUE., LCRIT)
 
         CALL ESPAR(0, T90, X, AMIX, BMIX)
         VS = VL
         CALL VIT(T90, P_COND, AMIX, BMIX, VS, .TRUE., LCRIT)
         CALL HCVCPS (1, T90, VS, X, H_LIQ, DUM1, DUM2, DUM3)
 
         DO J = 1, 3
            T_EVAP = -30 + 10*J
            T_EVAP = F_TO_K(T_EVAP)
            CALL BUBLT(T_EVAP, XL, X, P_EVAP, VL, VV, .FALSE., LCRIT)
 
            CALL ESPAR(0, T90, X, AMIX, BMIX)
            VS = VV*T90/T_EVAP
            CALL VIT(T90, P_EVAP, AMIX, BMIX, VS, .FALSE., LCRIT)
            CALL HCVCPS (1, T90, VS, X, H_VAP, DUM1, DUM2, DUM3)
 
 
            MASS(I,J)  = CAP(I,J)/(H_VAP - H_LIQ)     !kg-mole/hr
C
C          ESTIMATE THE SUCTION GAS TEMPERATURE AND THE EFFICIENCIES
C
            IF(ICOOL .EQ. 0) THEN
               TSUC = 479.59 - 64.815*EER
            ELSE
               TSUC = 427.84 - 57.895*EER
            END IF
 
            TSUC = TSUC - 2.0*(3-I) - 2.0*(J-2)
 
            IF(ICOMP .EQ. 2) TSUC = 120.0                  !Rotary
 
            TSUC = F_TO_K(TSUC)                            !K
            VSUC = VV*T90/TSUC                             !Suction density
 
            CALL ESPAR(0, TSUC, X, AMIX, BMIX)
            CALL VIT(TSUC, P_EVAP, AMIX, BMIX, VSUC, .FALSE., LCRIT)
            CALL HCVCPS (3, TSUC, VSUC, X, H_SUC, CV, CP, DUM1)
            SSUC = ENTROP(TSUC, VSUC, X)                   !Suction entropy
 
            CALL SPIN(SSUC, P_COND, X, T2S, XQ, XL, XV, VL2S, VV2S,
     .                SL2S, SV2S)
            CALL HCVCPS (1, T2S, VV2S, X, H2S, DUM1, DUM2, DUM3)
 
            ETAS(I,J) = MASS(I,J)*(H2S - H_SUC)/PWR(I,J)
 
            ETAV(I,J) = MASS(I,J)*VSUC/(60.0*SPEEDN)/(DISPL/61023.6)
            K = ETAP*CP/CV
            PR = P_COND/P_EVAP
            IF(ICOMP .EQ. 1) THEN
               CEIJ(I,J) = ((0.92 - ETAV(I,J))/0.92)/(PR**(1.0/K) - 1.0)
            ELSE
               CEIJ(I,J) = ((1.00 - ETAV(I,J))/1.00)/(PR**(1.0/K) - 1.0)
            END IF
 
C
C          ESTIMATE CYCLINDER TEMPERATURE AND CAN OUTLET TEMPERATURE
C
            TCYL = TSUC*(P_COND/P_EVAP)**(1.0-1.0/K)
 
            COP = CAP(I,J)/PWR(I,J)
            IF(ICOOL .EQ. 0) THEN
               RATIO = 0.68 - 0.05*3.413*COP
            ELSE
               RATIO = 0.90 - 0.07*3.413*COP
            END IF
 
            TOUT = TCYL - RATIO*(TCYL - T90)
 
         END DO
      END DO
C
C          CALCULATE THE OUTPUT VARIABLES
C
      ETAC = 0
      CE   = 0
      DO I = 1, 3
         DO J = 1, 3
            ETAC = ETAC + ETAS(I,J)
            CE   = CE   + CEIJ(I,J)
         END DO
      END DO
 
      ETAC = ETAC/9.0
      CE   = CE/9.0
 
      IF(.NOT. EXISTS) THEN
         ETAC = ETAS(3,2)
         CE   = CEIJ(3,2)
      END IF
 
      RETURN
      END
