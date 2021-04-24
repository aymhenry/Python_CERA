$DEBUG
      SUBROUTINE CYCLOS(I_VALVE, T_CYCLE, T_SET, T_ENV, T_EVAP,
     .                  T_COND, DUTY, CORRECTION)
C     ******************************************************************
C     *    ESTIMATE CORRECTION DUE TO CYCLING BEHAVIOR                 *
C     ******************************************************************
C
C          I_VALVE                        1 IF VALVE USED, 0 IF NOT
C          T_CYCLE                        NUMBER OF CYCLES PER HOUR
C          T_SET                          CABINET CONTROL SETPOINT
C          T_ENV                          ENVIRONMENT TEMPERATURE
C          T_EVAP                         EVAPORATOR TEMPERATURE
C          T_COND                         CONDENSER TEMPERATURE
C          DUTY                           DUTY CYCLE
C
C          CORRECTION                     CORRECTION FACTOR FOR COP
C
 
      TCYCLE = 1.0/T_CYCLE                                 !Cycle time
 
      SELECT CASE (I_VALVE)
         CASE (0)                                          !No valve
  !         CORRECTION = 1.0 - 0.011/TCYCLE
            CORRECTION = 1.0 - 0.010/TCYCLE
 
         CASE (1)                                          !Use microvalve
            CORRECTION = 1.0 + 0.015/TCYCLE
  !         CARNOT_IDEAL = T_EVAP/(T_COND - T_EVAP)
 
  !         DT_EVAP = (T_SET - T_EVAP)*(0.2 + 0.8*DUTY)
  !         DT_COND = (T_COND - T_ENV)*(0.2 + 0.8*DUTY)
 
  !         CARNOT_FULL  =      (T_SET - DT_EVAP)/
  !  .                     (T_ENV+DT_COND - T_SET+DT_EVAP)
 
  !         CORRECTION = CARNOT_FULL/CARNOT_IDEAL
  !         CORRECTION = CORRECTION*(1.0 -EXP(-0.15/TCYCLE))
  !  .                 + EXP(-0.15/TCYCLE)
 
      END SELECT
 
      RETURN
      END
