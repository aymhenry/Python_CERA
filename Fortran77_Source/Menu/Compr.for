      SUBROUTINE COMPR(IBRANCH, NC, IR, XM, F, DISPLC, CAPCOMP, SPEEDN,
     .                 EER, ICOMP, ICOOL, FRACTIONAL_SPEED, CAPRATIO,
     .                 DENRATIO)
C     *****************************************************************
C     *    CALCULATE CAPACITY FOR ACTUAL REFRIGERANT AT RATING POINT  *
C     *                                                               *
C     *    IBRANCH            Branch Code (0: find clearance volume,  *
C     *                          1: find capacity for previous fluid  *
C     *                          2: find capacity for new fluid)      *
C     *****************************************************************
C
      LOGICAL LCRIT
      REAL MASS
      DIMENSION X(5), WM(5), XM(5), IR(5), IRC(5), F(5,5), FC(5,5)
 
      COMMON /RDATA2/ WM,TCRIT
 
      DATA T90 / 305.3889 /
C
C          CONVERSION FUNCTIONS
C
      F_TO_K(TF) = (TF + 459.7) / 1.8
 
C
C          SET UP PROPERTY DATA: INITAIL CALL
C
      IF (IBRANCH .EQ. 0) THEN
         DO I = 1, 5
            X(I) = 0
            IRC(i) = 0
            DO J = 1, 5
               FC(I, J) = 0
            END DO
         END DO
 
         X(1) = 1
         IRC(1) = 2     !! CFC-12 refrigerant
         CALL BCONST(1, IRC, FC)
      END IF
 
C
C          SET UP PROPERTY DATA: FOR THE ACTUAL REFRIGERANTS
C
      IF (IBRANCH .NE. 0) THEN
         CALL BCONST (NC,IR,F)
 
         WMSUM=0.0
         I = 1
         DO WHILE (I .LE. NC)
              WMSUM = WMSUM + XM(I) / WM(I)
              I = I + 1
         END DO
 
         I = 1
         DO WHILE (I .LE. NC)
              X(I) = XM(I) / WM(I) / WMSUM
              I = I + 1
         END DO
 
      END IF
C
C          FIND SUCTION AND DISCHARGE ENTHALPIES AT 90 F
C
      T_EVAP = F_TO_K (-10)
      T_COND = F_TO_K (130)
 
      CALL BUBLT(T_COND, X, XV, P_COND, VL, VV, .TRUE., LCRIT)
      CALL ESPAR(0, T90, X, AMIX, BMIX)
      VS = VL
      CALL VIT(T90, P_COND, AMIX, BMIX, VS, .TRUE., LCRIT)
      CALL HCVCPS (1, T90, VS, X, H_LIQ, DUM1, DUM2, DUM3)
 
 
      CALL BUBLT(T_EVAP, XL, X, P_EVAP, VL, VV, .FALSE., LCRIT)
      VS = VV*T90/T_EVAP
      CALL VIT(T90, P_EVAP, AMIX, BMIX, VS, .FALSE., LCRIT)
      CALL HCVCPS (1, T90, VS, X, H_VAP, DUM1, DUM2, DUM3)
 
C
C          SET UP SUCTION PORT CONDITIONS AT RATING POINT
C
      IF(ICOOL .EQ. 0) THEN
         TSUC = 479.59 - 64.815*EER                     !Degrees F
      ELSE
         TSUC = 427.84 - 57.895*EER
      END IF
 
      IF(ICOMP .EQ. 2) TSUC = 120.0                     !Rotary
 
      TSUC = F_TO_K(TSUC)                               !K
      VSUC = VS*TSUC/T90                                !Suction density
 
      CALL ESPAR(0, TSUC, X, AMIX, BMIX)
      CALL VIT(TSUC, P_EVAP, AMIX, BMIX, VSUC, .FALSE., LCRIT)
      CALL HCVCPS (3, TSUC, VSUC, X, H_SUC, CV, CP, DUM1)
 
      GAMA = CP/CV
      RN = 0.97*GAMA
      RINV = 1.0/RN
      PR = P_COND / P_EVAP
 
      SPEED = SPEEDN * FRACTIONAL_SPEED
C
C          BRANCH ON CALL CODE
C
      SELECT CASE (IBRANCH)
         CASE (0)  !!  Initial call -- find clearance volume
            MASS = 1.0548 * CAPCOMP / (H_VAP - H_LIQ)
            ETAV = MASS * VSUC / (60.0 * SPEED) / (DISPLC / 61023.6)
 
            IF(ICOMP .EQ. 1) THEN
               CE = ((0.92 - ETAV) / 0.92) / (PR**RINV - 1.0)
            ELSE
               CE = ((1.00 - ETAV) / 1.00) / (PR**RINV - 1.0)
            END IF
 
         CASE (1)  !!  Previous refrigerant and compressor data
            IF(ICOMP .EQ. 1) THEN
               ETAV = 0.92*(1.0 - CE*(PR**RINV - 1.0))
            ELSE
               ETAV = 1.00*(1.0 - CE*(PR**RINV - 1.0))
            END IF
 
            MASS = 60.0 * SPEED * ETAV * DISPLC / VSUC / 61023.6
            CAPPREV = MASS * (H_VAP - H_LIQ)
 
         CASE (2)  !!  New refrigerant and compressor data
            IF(ICOMP .EQ. 1) THEN
               ETAV = 0.92*(1.0 - CE*(PR**RINV - 1.0))
            ELSE
               ETAV = 1.00*(1.0 - CE*(PR**RINV - 1.0))
            END IF
 
            DENRATIO = 1.0 / MASS
            MASS = 60.0 * SPEED * ETAV * DISPLC / VSUC / 61023.6
 
            DENRATIO = MASS * DENRATIO
            CAPNEW  = MASS * (H_VAP - H_LIQ)
 
            CAPRATIO = CAPPREV / CAPNEW
 
      END SELECT
 
 
 
      RETURN
      END
