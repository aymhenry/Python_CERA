# Python import
import sys

# User import
from CoolPrp import *
from decorators import *

@show_input_output("IN")
def map(objCP, ICOMP, ICOOL, EER, SIZE, DISPL, SPEEDN):
    # INPUT PARAMETERS
    #  ICOMP type of compressor:  1=reciprocating, 2=rotary
    #  ICOOL type of compressor can cooling:  0=static, 1=fan forced
    #  EER   EER at rating conditions
    #  SIZE  capacity (btuh) at rating conditions
    #  DISPL displacement (cu-in)
    #
    # OUTPUT PARAMETERS
    #  ETAC  compressor isentropic efficiency,
    #        including mechanical and motor losses
    #  CE    clearance volume (fraction)

    T90 = 305.3889			# 90F in Kelvin
    ETAP = 0.97 				# Get k from gamma

    #    CONVERSION FUNCTIONS
    def F_TO_K(T):
        return (T + 459.7) / 1.8

    # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
    ETAS = [[0.0] * (3 + 1) for i in range(3 + 1)]
    # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
    ETAV = [[0.0] * (3 + 1) for i in range(3 + 1)]
    # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
    MASS = [[0.0] * (3 + 1) for i in range(3 + 1)]
    # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
    CEIJ = [[0.0] * (3 + 1) for i in range(3 + 1)]

    # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
    XL = [[0.0] * (5 + 1) for i in range(5 + 1)]
    # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
    XV = [[0.0] * (5 + 1) for i in range(5 + 1)]
    IR = [0.0] * (5 + 1)

    #    NORMALIZE
    #
    ITAB = 0
    # X = [0.0] * (5 + 1)
    # # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
    # F = [[0.0] * (5 + 1) for i in range(5 + 1)]

    # X[1] = 1
    # IR[1] = 2		# Assume CFC-12

    # self.bconst(1, IR, F)  # CALL BCONST(1,IR,F)

    if(ITAB == 0):
        EXISTS = False  # Python comment allways  False

    MOTOR = 3.413 * SIZE / EER 		# Motor power in Btu/hr
    CAP = [[1.0548 * SIZE] * (3 + 1) for i in range(3 + 1)]  # kJ/hr
    PWR = [[1.0548 * MOTOR] * (3 + 1) for i in range(3 + 1)]  # kJ/hr

    # calculate the mass flows assuming 90 f liquid and vapor temps
    #
    for I in range(1, 3 + 1):  # DO I = 1, 3
        T_COND = 100 + 10 * I
        T_COND = F_TO_K(T_COND)

        # [X, XV, P_COND, VL, VV, LCRIT] = self.bublt(T_COND, X, XV, True)
        P_COND = objCP.Property('P', X=1, T=T_COND)  # Pa             
    
        # [AMIX, BMIX] = self.espar(0, T90, X)
        # VS = VL
        # [VS, LCRIT] = self.vit(T90, P_COND, AMIX, BMIX, VS, True)
        # [H_LIQ, DUM1, DUM2, DUM3] = self.hcvcps(1, T90, VS, X)
        H_LIQ = objCP.Property('H', T=T90, P=P_COND)  # j/kg

        for J in range(1, 3 + 1):  # DO J = 1, 3
            T_EVAP = -30 + 10 * J
            T_EVAP = F_TO_K(T_EVAP)

            # [XL, X, P_EVAP, VL, VV, LCRIT] = self.bublt(
                # T_EVAP, XL, X, False)
            P_EVAP = objCP.Property('P', X=1, T=T_EVAP)  # Pa          

            # [AMIX, BMIX] = self.espar(0, T90, X)
            # VS = VV * T90 / T_EVAP
            # [VS, LCRIT] = self.vit(T90, P_EVAP, AMIX, BMIX, VS, False)
            # [H_VAP, DUM1, DUM2, DUM3] = self.hcvcps(1, T90, VS, X)
            H_VAP = objCP.Property('H', T=T90, P=P_EVAP)  # j/kg
        
            # kg/hr    = (kj/hr)/ (kj/kg) 
            MASS[I][J] = CAP[I][J] / (H_VAP - H_LIQ) * 1000  
            
            #
            #    ESTIMATE THE SUCTION GAS TEMPERATURE AND THE EFFICIENCIES
            #
            if(ICOOL == 0):
                TSUC = 479.59 - 64.815 * EER
            else:
                TSUC = 427.84 - 57.895 * EER

            TSUC = TSUC - 2.0 * (3 - I) - 2.0 * (J - 2)

            if(ICOMP == 2):
                TSUC = 120.0  # Rotary

            TSUC = F_TO_K(TSUC)  # K
            # VSUC = VV * T90 / TSUC  # Suction density

            # [AMIX, BMIX] = self.espar(0, TSUC, X)
            # [VSUC, LCRIT] = self.vit(TSUC, P_EVAP, AMIX, BMIX, VSUC, False)
            VSUC = objCP.Property('V', T=TSUC, P=P_EVAP)  # m3/kg
            
            # [H_SUC, CV, CP, DUM1] = self.hcvcps(3, TSUC, VSUC, X)
            H_SUC = objCP.Property('H', T=T90, P=P_EVAP)  # j/kg
            CP = objCP.Property('CP', T=T90, P=P_EVAP)  # j/kg/K
            CV = objCP.Property('CV', T=T90, P=P_EVAP)  # j/kg/K
            
            # SSUC = self.entrop(TSUC, VSUC, X)  # Suction entropy
            SSUC = objCP.Property('S', T=TSUC, P=P_EVAP)  # j/kg/K
            
            # [T2S, XQ, XL, XV, VL2S, VV2S, SL2S,
                # SV2S] = self.spin(SSUC, P_COND, X)
            T2S = objCP.Property('T', S=SSUC, P=P_COND)  # K

            #[H2S, DUM1, DUM2, DUM3] = self.hcvcps(1, T2S, VV2S, X)
            H2S = objCP.Property('H', T=T2S, P=P_COND)  # j/kg
            
            #  None          kg/hr          * (kj/kg)             / (kJ/hr)
            ETAS[I][J] = MASS[I][J] * (H2S - H_SUC) /1000/ PWR[I][J] # 

            # not the ETAV in common
            #      kg/hr  * m3/kg  /rps / m3
            ETAV[I][J] = MASS[I][J] * VSUC /(60.0 * SPEEDN) / (DISPL / 61023.6)
                
            # modification by Dr Omar    
            # Fractional Speed (-) input value - changed from 1 (bad value) to 3450
            #ETAV[I][J] = MASS[I][J] * VSUC / (60.0 * 3450) / (DISPL / 61023.6)

            K = ETAP * CP / CV
            PR = P_COND / P_EVAP

            if(ICOMP == 1):
                CEIJ[I][J] = ((0.92 - ETAV[I][J]) / 0.92) / \
 (PR**(1.0 / K) - 1.0)
            else:
                CEIJ[I][J] = ((1.00 - ETAV[I][J]) / 1.00) / \
 (PR**(1.0 / K) - 1.0)

            #
            # estimate cyclinder temperature and can outlet temperature
            #
            TCYL = TSUC * (P_COND / P_EVAP) ** (1.0 - 1.0 / K)

            COP = CAP[I][J] / PWR[I][J]  # None =  (kJ/hr)  / (kJ/hr)

            if(ICOOL == 0):
                RATIO = 0.68 - 0.05 * 3.413 * COP
            else:
                RATIO = 0.90 - 0.07 * 3.413 * COP
            # end if

            TOUT = TCYL - RATIO * (TCYL - T90)

    #   calculate the output variables
    #
    ETAC = 0
    CE = 0
    for I in range(1, 3 + 1):  # DO I = 1, 3
        for J in range(1, 3 + 1):  # DO J = 1, 3
            ETAC = ETAC + ETAS[I][J]
            CE = CE + CEIJ[I][J]

    ETAC = ETAC / 9.0
    CE = CE / 9.0

    if (not EXISTS):
        ETAC = ETAS[3][2]
        CE = CEIJ[3][2]

    return [ETAC, CE]
    


