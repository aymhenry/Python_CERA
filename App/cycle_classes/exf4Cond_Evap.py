# Python import
import math

# User import


class exf4Cond_Evap:

    @staticmethod
    def getHNAT(detal_T_K, A_NAT):
        detal_T_F = detal_T_K * 1.8     # defrance to F
        # old equation is
        # HNAT = 0.19 * DELTA ** 0.33 * 20.44   # W/m2 K
        # A_NAT = 0.239 + 3.34E-04 * (273.0 - TBAR)
        
        HNAT = A_NAT * detal_T_F**0.33 * 20.44   # kW/m2 K
        return HNAT   # W/m2 K

    @staticmethod
    def getHRAD(TAVE_K, TS_K, EPS):
        # old value & equation
        # SIGMA = 2.0432E-7
        SIGMA = 5.670374419E-8   # W/m2 K4   # by Dr Omar
        # EPS = 0.8
        
        # HRAD = SIGMA * (TAVE + TS1) * (TAVE ** 2 + TS1 ** 2)    # W/m2 K
        HRAD = SIGMA * (TAVE_K + TS_K) * (TAVE_K**2 + TS_K**2) * EPS
        return HRAD     # W/m2 K
        
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    @staticmethod
    def exf(LOC, AREA, U, CMIN, CMAX):
        # calculate counter flow efficiency parameters                
        # if is LOC == 1:   Counter-flow
        #                   Cross-flow
        # AREA m2
        # U  W/m2 K
        # CMIN, CMAX watt/K
        DEFFDA = EFF = 0

        EFF_CROSS = [0.0] * (2 + 1)
        coff_A = [
            [2.394292, -1.19402, -1.45067, 1.938453, -0.81305, 0.118651],
            [2.410798, -2.23391, 0.825900, 0.051006, -0.11891, 0.023360],
            [2.399687, -2.96882, 2.367080, -1.23009, 0.373338, -0.04886],
            [2.359642, -3.37650, 3.048620, -1.63421, 0.468741, -0.05492]
        ]
        
        # AREA[m2] * U[watt/m2 K] / CMIN[watt/K] =
        NTU = AREA * U / CMIN   # unit less
        CRAT = CMIN / CMAX      # unit less

        if LOC == 1:  # Counter-flow
            XX = 1.0 - CRAT     # unit less
            XXX = math.exp(-NTU * XX)   # unit less
            EFF = (1.0 - XXX) / (1.0 - CRAT * XXX)  # unit less
            # U[w/m2 K] / CMIN[watt/K] = 1/m2
            DEFFDA = (U / CMIN) * XX * XXX * \
                (1.0 - CRAT * EFF) / (1.0 - CRAT * XXX)

        int_row = 0
        
        if LOC == 2:  # Cross-flow
            if 0.00 <= CRAT <= 0.25:
                int_row = 1
            if 0.25 < CRAT <= 0.50:
                int_row = 2
            if 0.50 < CRAT <= 0.75:
                int_row = 3
            if 0.75 < CRAT <= 1.00:
                int_row = 4

            if NTU <= 0.0:
                NTU = 0.0

            for L in range(1, 2 + 1):
                BETA = math.log10(NTU + 1.0)
                EFFA = 0.0
                EFFB = 0.0

                for J in range(1, 6 + 1): 
                    EX = 1.0 * J
                    if int_row == 1:
                        EFFA = 1.0 - math.exp(-NTU)
                    else:
                        EFFA += coff_A[int_row - 1 - 1][J - 1] * BETA ** EX

                    EFFB += coff_A[int_row - 1][J - 1] * BETA ** EX

                FRAC = (CRAT - (int_row - 1) * 0.25) / \
                    (int_row * 0.25 - (int_row - 1) * 0.25)
                EFFECT = EFFA + FRAC * (EFFB - EFFA)

                if EFFECT > 1.0:
                    EFFECT = 1.0

                EFF_CROSS[L] = EFFECT
                NTU *= 0.9

            EFF = EFF_CROSS[1]
            DEFFDA = 10.0 * (EFF_CROSS[1] - EFF_CROSS[2]) / AREA   # 1/m2

        if DEFFDA <= 0.0:
            DEFFDA = 0.0001

        return [EFF, DEFFDA]    # [unitless]  [1/m2]
