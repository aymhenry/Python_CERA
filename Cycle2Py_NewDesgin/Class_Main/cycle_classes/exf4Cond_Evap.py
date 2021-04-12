# Python import
import math

# User import

class exf4Cond_Evap ():
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def exf(self, LOC, AREA, U, CMIN, CMAX):
        # calculate counter flow efficiency parameters                

        EFF_CROSS = [0.0] * (2 + 1)
        coff_A = [
            [2.394292, -1.19402, -1.45067, 1.938453, -0.81305, 0.118651],
            [2.410798, -2.23391, 0.825900, 0.051006, -0.11891, 0.023360],
            [2.399687, -2.96882, 2.367080, -1.23009, 0.373338, -0.04886],
            [2.359642, -3.37650, 3.048620, -1.63421, 0.468741, -0.05492]
        ]

        NTU = AREA * U / CMIN
        CRAT = CMIN / CMAX

        if LOC == 1:  # Counter-flow
            XX = 1.0 - CRAT
            XXX = math.exp(-NTU * XX)
            EFF = (1.0 - XXX) / (1.0 - CRAT * XXX)
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

            if (NTU <= 0.0):
                NTU = 0.0

            for L in range(1, 2 + 1):
                BETA = math.log10(NTU + 1.0)
                EFFA = 0.0
                EFFB = 0.0

                for J in range(1, 6 + 1): 
                    EX = 1.0 * J
                    if (int_row == 1):
                        EFFA = 1.0 - math.exp(-NTU)
                    else:
                        EFFA = EFFA + coff_A[int_row - 1 - 1][J - 1] * BETA**EX

                    EFFB = EFFB + coff_A[int_row - 1][J - 1] * BETA**EX

                FRAC = (CRAT - (int_row - 1) * 0.25) / \
                    (int_row * 0.25 - (int_row - 1) * 0.25)
                EFFECT = EFFA + FRAC * (EFFB - EFFA)

                if (EFFECT > 1.0):
                    EFFECT = 1.0

                EFF_CROSS[L] = EFFECT
                NTU = 0.9 * NTU

            EFF = EFF_CROSS[1]
            DEFFDA = 10.0 * (EFF_CROSS[1] - EFF_CROSS[2]) / AREA

        if(DEFFDA <= 0.0):
            DEFFDA = 0.0001
        return [EFF, DEFFDA]
