      SUBROUTINE RADTRN(R1, R2, T1F, T2F, T3F, E1, E2, A3, Q,
     .                  TF, Q3, Q2)
C     ******************************************************************
C     *    DETERMINE SURFACE TEMPERATURES OF CABINET WALLS             *
C     ******************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C     A SUBROUTINE TO CALCULATE CABINET SURFACE RADIATIVE HEAT TRANSFER
C     19 September 1989
C
C     R1 is the resistance of refrigerator wall and inside wall heat trans
C     R2 is the outside wall forced convection heat transfer
C     R3 is the radiative heat transfer from the outside wall to the walls of
C           the room the refrigerator is in.
C     T1F is the inside refrigerator temperature (F)
C     T2F is the outside ambient air temperature (F)
C     T3F is the outside radiative temperature (F)
C     E1  is the emissivity of the refrigerator outer wall
C     E2  is the emissivity of the wall the refrigerator radiates to
C     A3  is the wall area of the refrigerator that is radiating
C     Q is the heat gain (if <0 the heat loss) of the refrigerator wall
C       (BTU/hr)
C     TF is the refrigerator outer wall surface temperature (F)
C
      COMMON /INIO/ IN, IO, IPRINT
C
C
C                     Outer
C                      Wall
C      Radiative
C         T3---- Q3 ----[ Conduction ]
C                       [---- Q1 ----] Inner Wall Surface
C         T2---- Q2 ----[            ]
C   Forced Conv         T            T1
C
C           Q1 = Q2 + Q3      The heat thru the wall (Q1) must equal the
C                             heat removed thru radiation (Q3) and forced
C                             convection (Q2) which act in parallel.
C
C           Q1 = (T - T1)/R1
C           Q2 = (T2 - T)/R2
C           Q3 = (T3 - T)/R3
C
C
C           The routine has been modified to ignore radiation transfer.
C           It is assumed that the convective heat transfer will cover all
C           situations (ie, the radiative component is part of the
C           convection coefficient
C
      TF = (R2*T1F + R1*T2F)/(R1 + R2)
      Q  = (TF - T1F)/R1
      Q2 = Q
      Q3 = 0
      RETURN
      END
