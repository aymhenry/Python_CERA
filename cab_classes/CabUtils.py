# Python import
import math

# User Import

# ----------------------------------------------------------
# Job            : Common utilitess used by Qlxxx modules
#
# Editor       : aymhenry@gmail.com
# ----------------------------------------------------------


class CabUtils:
    # ----------------------------------------------------------
    # Job            :  Calculates the saturation Density of Water - denh2o
    # Input        :  Input:  TF........ Saturation Temperature(Deg F)
    #
    # Output       : Output: RHOS...... Saturation Density(#/ft3)
    # ----------------------------------------------------------
    def getSatWaterDensity(self, TF):
        return 1.677 * self.getSatWaterPressure(TF) / (TF + 460.0)

    # ----------------------------------------------------------
    # Job            :  Calculates the Humidity Ratio of Air - humrat
    # Input        :  Input:    TF........ Saturation Temperature(Deg F)
    #                            RELHUM...... Relative Humidity
    # Output       :  Output:     Humidity Ratio
    # ----------------------------------------------------------
    def getAirHumidity(self, TF, RELHUM):
        PG = RELHUM * self.getSatWaterPressure(TF)
        return 0.622 * PG/(14.7 - PG)

    # ----------------------------------------------------------
    # Job           : Calculates the saturation pressure of Water  satps
    #
    # Input        : Input: TF...... Saturation Temperature(Deg F)
    # Output       : Output: PS...... Saturation Pressure(PSIA)
    # ----------------------------------------------------------
    def getSatWaterPressure(self, flt_tf):
        TR = flt_tf + 460.0
        return math.exp((-9560.8) / TR + 17.0234)

    # ----------------------------------------------------------
    # Job           : Calculates the Enthalpy Change of Moist Air   hwtair
    # Input:  TF1Start     Temperature(Deg F)
    #           TF2         Final Temperature(Deg F)
    #           TFCAB         Final Cabinet Temperature(Deg F)
    #           W1         Start Humidity Ratio
    #           W2         Final Humidity Ratio
    #
    # Output: HVAPS     Sensible Enthalpy Change of the Air/Water Mixture assuming the water remains vapor(BTU/lb)
    #   HVAPL     Latent Enthalpy Change of the Air/Water Mixture assuming the water remains vapor(BTU/lb)
    #   HLIQS     Sensible Enthalpy Change of the Air/Water Mixture assuming the water condenses as liquid(BTU/lb)
    #   HLIQL     Latent Enthalpy Change of the Air/Water Mixture assuming the water condenses as liquid(BTU/lb)
    #   HSLDS     Sensible Enthalpy Change of the Air/Water Mixture assuming the water condenses as ice(BTU/lb)
    #   HSLDL     Latent Enthalpy Change of the Air/Water Mixture assuming the water condenses as ice(BTU/lb)
    #
    #  list [HVAPS, HVAPL, HLIQS, HLIQL, HSLDS, HSLDL]
    # ----------------------------------------------------------
    def getAirMoistEnthalpy(self, TF1, W1, TF2, W2, TFCAB):

        PNDH2O = W1 - W2
        if PNDH2O < 0.0:
            W1 = W2

        HVAPS = 0.24 * TF1 + W1 * (0.444*TF1) - 0.24 * TFCAB - W1 * (0.444*TFCAB)
        HVAPL = 0.0

        HLIQS = HVAPS + PNDH2O * 0.444 * (TFCAB - TF2)
        HLIQL = PNDH2O * (1061.0 + 0.444 * TFCAB)

        HSLDS = HLIQS
        HSLDL = PNDH2O * ((1061.0 + 0.444 * TFCAB) + (158.9 - 0.488*TF2))
        return [HVAPS, HVAPL, HLIQS, HLIQL, HSLDS, HSLDL]

    # ----------------------------------------------------------
    # Job            : determine surface temperatures of cabinet walls
    #         R3 is not used in equation, not like FORTRAN commenter.
    #         T3F, E1, E2, A3 is not used
    # Input        : R1, R2, T1F, T2F, TF
    # Output       : updates Cab.Q, Cab.TF , Cab.Q2, Cab.Q, Cab.Q3 i.e. Q, TF, Q3, Q2
    #                     return list [Q, TF]
    # Input
    #     R1 is the resistance of refrigerator wall and inside wall heat trans
    #     R2 is the outside wall forced convection heat transfer
    #     R3 is the radiative heat transfer from the outside wall to the walls of
    #           the room the refrigerator is in.
    #     T1F is the inside refrigerator temperature(F)
    #     T2F is the outside ambient air temperature(F)
    #     T3F is the outside radiative temperature(F)
    #
    # Output
    #     E1  is the emissivity of the refrigerator outer wall
    #     E2  is the emissivity of the wall the refrigerator radiates to
    #     A3  is the wall area of the refrigerator that is radiating
    #     Q is the heat gain(if <0 the heat loss) of the refrigerator wall(BTU/hr)
    #     TF is the refrigerator outer wall surface temperature(F)
    #                     Outer
    #                      Wall
    #      Radiative
    #         T3---- Q3 ----[ Conduction ]
    #                       [---- Q1 ----] Inner Wall Surface
    #         T2---- Q2 ----[            ]
    #   Forced Conv         T            T1
    #
    #           Q1 = Q2 + Q3      The heat thru the wall(Q1) must equal the
    #                             heat removed thru radiation(Q3) and forced
    #                             convection(Q2) which act in parallel.
    #
    #           Q1 = (T - T1) /R1
    #           Q2 = (T2 - T) /R2
    #           Q3 = (T3 - T) /R3
    #
    # Modified to ignore radiation transfer.
    # It is assumed that the convective heat transfer will cover all situations 
    # (i.e. the radiative component is part of the convection coefficient
    #
    # ---------------------------------------------------------- 
    def getRadHeatFlux(self, R1, R2, T1F, T2F):

        TF = (R2 * T1F + R1 * T2F) / (R1 + R2)
        Q = (TF - T1F) / R1
        # Q2 = Q        # same as FORTRAN code
        # Q3 = 0.0# same as FORTRAN code
        
        return [Q, TF]
    
    # .=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=      

# ___________ End of Cab class _________________________________________________________________
