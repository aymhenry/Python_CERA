# Python import
import sys

# User import
from CoolPrp import *
from decorators import *

@show_input_output("IN")
def comp(objCP, H1, P1, P2, T1, T12, MEFF, QHILO, QCAN,
         V1, TAMB, EER, SEFF, SPEED, MREF, IMAP, EFFC, CE, ICOOL, ICOMP):
    #     *****************************************************************
    #     *    COMPRESSOR MODEL                                           *
    #     *****************************************************************

    R = 8.314
    TOLS = 0.1
    ETA_ISEN = 0

    # CONVERSION FUNCTIONS
    def F_TO_K(T):
        return (T + 459.7) / 1.8

    #
    # set up initial guess for suction port conditions
    TSUC = T1
    VSUC = V1

    # [HSUC, CV, CP, VS] = self.hcvcps(1, TSUC, VSUC, X)
    HSUC = objCP.Property('H', T=TSUC, V=VSUC)  # j/kg

    # [XL, X, TDEW, XXX, VDEW, LCRIT] = self.bublp(P2, XL, X, False)
    TDEW = objCP.Property('T', P=P2, X=0)  # pressur in Pa
    VDEW = objCP.Property('V', P=P2, X=0)  # Volume in m3/kg

    # calculate isentropic conditions based on shell inlet
    # SSUC = self.entrop(TSUC, VSUC, X)
    SSUC = objCP.Property('S', T=TSUC, V=VSUC)  # S in j/kg/K
        
    # [T2, XQ[2], XL, XV, VL2, VV2, SL, SV] = self.spin(SSUC, P2, X)
    T2 = objCP.Property('T', S=SSUC, P=P2)  # K
    VV2 = objCP.Property('V', S=SSUC, P=P2)  # m3/kg

    # if(XQ[2] < 1.0):   
    if objCP.is_two_phase(objCP.phase_byPressTemp (P2, T2)):
        # [HL2, CV, CP, VS] = self.hcvcps(1, T2, VL2, XL)
        HL2 = objCP.Property('H', T=T2, V=VL2)  # j/kg

        # [HV2, CV, CP, VS] = self.hcvcps(3, T2, VV2, XV)
        HV2 = objCP.Property('H', T=T2, V=VV2)  # j/kg
        CV = objCP.Property('CV', T=T2, V=VV2)  # j/kg/K
        CP = objCP.Property('CP', T=T2, V=VV2)  # j/kg/K

        # In Python Only: calculate quality
        Sgas = objCP.Property('S', P=P2, X=0)  # j/kg/K
        Slqu = objCP.Property('S', P=P2, X=1)  # j/kg/K
        X = (SSUC - Slqu)/(Sgas - Slqu)

        # H2 = XQ[2] * HV2 + (1.0 - XQ[2]) * HL2
        H2 = X * HV2 + (1.0 - X) * HL2
    else:
        # [H2, CV, CP, VS] = self.hcvcps(3, T2, VV2, X)
        H2 = objCP.Property('H', T=T2, V=VV2)  # j/kg
        CV = objCP.Property('CV', T=T2, V=VV2)  # j/kg/K
        CP = objCP.Property('CP', T=T2, V=VV2)  # j/kg/K

    HISEN = H2

    # select model
    if(IMAP == 1):  # Map model
        if(ICOOL == 0):
            TSUC = 389.59 - 64.815 * EER  # Degrees F
        else:
            TSUC = 337.84 - 57.895 * EER

        TIN = 1.8 * T1 - 459.7
        TSUC = TSUC + TIN

        T_EVAP = 1.8 * T12 - 459.7
        T_COND = 1.8 * TDEW - 459.7

        TSUC = TSUC + 0.2 * (T_COND - 130.0) - 0.2 * (T_EVAP + 10.0)

        if(ICOMP == 2):
            TSUC = TIN + 30.0  # Rotary

        TSUC = F_TO_K(TSUC)  # K
        VSUC = VSUC * TSUC / T1  # Suction density

        # [AMIX, BMIX] = self.espar(0, TSUC, X)
        # [VSUC, LCRIT] = self.vit(TSUC, P1, AMIX, BMIX, VSUC, False)
        # [H_SUC, CV, CP, DUM1] = self.hcvcps(3, TSUC, VSUC, X)

        H_SUC = objCP.Property('H', T=TSUC, P=P1)  # j/kg
        CV = objCP.Property('CV', T=TSUC, P=P1)  # j/kg/K
        CP = objCP.Property('CP', T=TSUC, P=P1)  # j/kg/K

        # SSUC = self.entrop(TSUC, VSUC, X)  # Suction entropy
        # [T2S, XQ[2], XL, XV, VL2S, VV2S, SL2S, SV2S] = self.spin(SSUC, P2, X)
        # [H2S, DUM1, DUM2, DUM3] = self.hcvcps(1, T2S, VV2S, X)
        H2S = objCP.Property('H', T=TSUC, V=VSUC)  # j/kg

        if(ICOMP == 2):
            ETAS = EFFC \
                * (1.0 - 0.0010 * (T_COND - 130.0)) \
                * (1.0 + 0.0030 * (T_EVAP + 10))
        else:
            ETAS = EFFC \
                * (1.0 + 0.0010 * (T_COND - 130.0)) \
                * (1.0 + 0.0020 * (T_EVAP + 10))

        W = (H2S - H_SUC) / EFFC
        if(ICOOL == 1):
            W = (H2S - H_SUC) / ETAS

        GAMA = CP / CV
        RN = 0.97 * GAMA
        RINV = 1.0 / RN
        PR = P2 / P1

        #
        # estimate cyclinder temperature and can outlet temperature
        #
        #TDISC = TSUC*(P2/P1)**(1.0-1.0/RN)
        # modificaton by Dr. Omar
        TDISC = TSUC * (P2 / P1)**(1.0 - 0.97 * CV / CP)

        # [AMIX, BMIX] = self.espar(0, TDISC, X)
        # #VVD = R * TDISC / P2
        # # modification by Dr. Omar
        # VVD = R * TDISC / P2 / 1000  # must be in Pa
        # [VVD, LCRIT] = self.vit(TDISC, P2, AMIX, BMIX, VVD, False)
        # [HDISC, CV, CP, DUM1] = self.hcvcps(1, TDISC, VVD, X)
        HDISC = objCP.Property('H', T=TDISC, P=P2)  # j/kg

        ETA_ISEN = (H2S - H_SUC) / (HDISC - H_SUC)

        if(ICOOL == 0):
            RATIO = 0.68 - 0.05 * EER
        else:
            RATIO = 0.90 - 0.07 * EER

        T2 = TDISC - RATIO * (TDISC - TAMB)

        # [AMIX, BMIX] = self.espar(0, T2, X)
        # VV2 = R * T2 / P2
        # [VV2, LCRIT] = self.vit(T2, P2, AMIX, BMIX, VV2, False)
        # [H2, CV, CP, DUM1] = self.hcvcps(1, T2, VV2, X)
        H2 = objCP.Property('H', T=T2, P=P2)  # j/kg

        QCAN = 1.0 - (H2 - H1) / W
        QHILO = (HDISC - H2) / W
    else:  # Physical model
        #
        # find entropy of suction gas and temperature for discharge
        # gas from an isentropic expansion

        ITER = 0
        ERROR = TOLS + 1

        TSUC = T1 + 3.0
        VSUC = V1 * TSUC / T1  # Suction density

        # [HSUC, CV, CP, VS] = self.hcvcps(1, TSUC, VSUC, X)
        # HSUC = objCP.Property('H', T=TSUC, V=VSUC)  # j/kg
        HSUC = objCP.Property('H', T=TSUC, P=P1)  # j/kg
        
        while (ERROR > TOLS and ITER < 10):
            ITER = ITER + 1
            # SSUC = self.entrop(TSUC, VSUC, X)
            
            # use VSUC not P1, to prevent in wet area
            SSUC = objCP.Property('S', T=TSUC, V=VSUC)  # S in j/kg/K
            #SSUC = objCP.Property('S', T=TSUC, P=P1)  # S in j/kg/K             
            # [T2, XQ[2], XL, XV, VL2, VV2, SL, SV] = self.spin(SSUC, P2, X)
            T2 = objCP.Property('T', S=SSUC, P=P2)  # K           

            # if(XQ[2] < 1.0):
            if objCP.is_two_phase(objCP.phase_byPressTemp (P2, T2)):
                # [HL2, CV, CP, VS] = self.hcvcps(1, T2, VL2, XL)
                HL2 = objCP.Property('H', T=T2, V=VL2)  # j/kg
                
                # [HV2, CV, CP, VS] = self.hcvcps(3, T2, VV2, XV)
                HV2 = objCP.Property('H', T=T2, V=VV2)  # j/kg
                CV = objCP.Property('CV', T=T2, V=VV2)  # j/kg/K
                CP = objCP.Property('CP', T=T2, V=VV2)  # j/kg/K

                # In Python Only: calculate quality
                Sgas = objCP.Property('S', P=P2, X=0)  # j/kg/K
                Slqu = objCP.Property('S', P=P2, X=1)  # j/kg/K
                X = (SSUC - Slqu)/(Sgas - Slqu)
        
                # H2 = XQ[2] * HV2 + (1.0 - XQ[2]) * HL2
                H2 = X * HV2 + (1.0 - X) * HL2
            else:
                # [H2, CV, CP, VS] = self.hcvcps(3, T2, VV2, X)
                H2 = objCP.Property('H', T=T2, P=P2)  # j/kg
                CV = objCP.Property('CV', T=T2, P=P2)  # j/kg/K
                CP = objCP.Property('CP', T=T2, P=P2)  # j/kg/K

            # determine isentropic efficiency
            GAMA = CP / CV
            RN = 0.97 * GAMA
            RINV = 1.0 / RN
            PR = P2 / P1
            H2 = HSUC + (H2 - HSUC) / SEFF
 
            # re-calculate suction temperature and compare with old value
            COEF1 = (1.0 - MEFF - (MEFF * QCAN - QHILO) \
                     / (1.0 - QCAN))  \
                     / (1.0 + QHILO / (1.0 - QCAN))
                                 
            H1P = H1 + COEF1 * (H2 - H1)

            # [T1P, XQ1, XL, XV, VL2, VV2, HL2, HV2] = self.hpin(H1P, P1, X)
            T1P = objCP.Property('T', H=H1P, P=P1)  # K
            
            if objCP.isError():
                print ("Error: " + objCP.err_description())
                return [0 for i in range(0,12)]
                    
            if(ICOMP == 2):
                T1P = TSUC
            
            ERROR = abs(T1P - TSUC)

            if(ICOMP == 1):
                TSUC = T1P
                HSUC = H1P
                VSUC = VV2

        # correct discharge condition for can loss

        HDISC = H2
        # [TDISC, XQ[2], XL, XV, VL2, VV2, HL2, HV2] = self.hpin(HDISC, P2, X)
        TDISC = objCP.Property('T', H=HDISC, P=P2)  # K
            
        H2 = H1 + ((H2 - H1)) / (1.0 + QHILO / (1.0 - QCAN))

        # [T2, XQ[2], XL, XV, VL2, VV2, HL2, HV2] = self.hpin(H2, P2, X)
        T2 = objCP.Property('T', H=H2, P=P2)  # K
        VV2 = objCP.Property('T', H=H2, P=P2)  # K

    # calculate mass flow rate
    if(ICOMP == 1):
        ETAV = 0.92 * (1.0 - CE * (PR**RINV - 1.0))
    else:
        ETAV = 1.00 * (1.0 - CE * (PR**RINV - 1.0))

    DISP = MREF * VSUC / (60.0 * SPEED * ETAV)  # SPEED in rpm MREF kg/hr
    
    HOUT = H2
    ETAS = ETA_ISEN

    return [T2, HOUT, QHILO, QCAN, VSUC, VV2, TSUC, TDISC, GAMA, RN, ETAS, DISP]


T1 = 296     # K suction temp.
T12 = 249.5  # K discharge temp.

P1 = 123.05 * 1000  # Suction pressure Pa
P2 = 928.18 * 1000  # Discharge pressire Pa

R12_TABLE_READING = 200.0 - 27.10795349661135 # 26.2257538946007 	# kj/kg   200-app result at 0C need to be 200, valid only for RF12
H1 = 23726 *1000 /120.91 + R12_TABLE_READING # J/kg
V1 = 19.490 / 120.91 # m3/kg

TAMB = 308.11 # K
# MEFF used only in case of IMAP = 0 (MAP) or 2 (Efficiency Model)
MEFF = 0.8  # MEFF - MECHANICAL EFFICIENCY

QHILO = 0.0 # QHILO - NORMALIZED HEAT LOSS FROM DISCHANGE LINE INSIDE
QCAN = 0.0  # QCAN - COMPRESSOR SHELL LOSS NORMALIZED TO POWER INPUT
IMAP = 1 # Compressor Analysis method values 0 MAP, 1 ERR, 2 Efficiency mode

EER = 5.28  # Rated EER used only if IMAP = 1
SEFF =  0.9 # isentropic efficiency

ICOOL =  1  # Fan cooling method 0 Static, 1 Fan-Forced
ICOMP =  1  # Compressor Type 1   Reciprocating, 2   Rotary

SPEED =  3450 # Nominal Speed (rpm =3450)
MREF = 5.8 # kg/hr ===>0.04796956413861551 kmol/hr # Initial Guess For Refrigerant Mas Flow Rate (kg/hr) * 2.20462 lbs/hr

# used only if IMAP =1 useless otherwise
EFFC =  0.6058868684272488 # used only if IMAP =1 useless otherwise

# used only if IMAP =1 useless otherwise
CE =  0.022338000722326633 # CLEARANCE VOLUME (FRACTION) output of map

# Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')

print ("\n\n==Test 1 =========================")
lstRes = comp (objCP, H1=H1, P1=P1, P2=P2, T1=T1, T12=T12   
               ,MEFF=MEFF, QHILO=QHILO, QCAN=QCAN, V1=V1, TAMB=TAMB
               ,EER=EER, SEFF=SEFF, SPEED=SPEED, MREF=MREF, IMAP=IMAP
               ,EFFC=EFFC, CE=CE, ICOOL=ICOOL, ICOMP=ICOMP)

print ("  ................ K")
print ("  T2    ", lstRes[0])
print ("  TSUC  ", lstRes[6])
print ("  TDISC ", lstRes[7])

print ("  ................j/kg ")
print ("  HOUT  ", lstRes[1])
print ("  QHILO ", lstRes[2])
print ("  QCAN  ", lstRes[3])

print ("  ................m3/kg ")
print ("  VV2   ", lstRes[5])
print ("  VSUC  ", lstRes[4])

print ("  ................Other ")
print ("  GAMA  ", lstRes[8])
print ("  RN    ", lstRes[9])
print ("  ETAS  ", lstRes[10])
print ("  DISP  ", lstRes[11])

print ("\n\n==Test 2 =========================")
IMAP = 2

lstRes = comp (objCP, H1=H1, P1=P1, P2=P2, T1=T1, T12=T12   
               ,MEFF=MEFF, QHILO=QHILO, QCAN=QCAN, V1=V1, TAMB=TAMB
               ,EER=EER, SEFF=SEFF, SPEED=SPEED, MREF=MREF, IMAP=IMAP
               ,EFFC=EFFC, CE=CE, ICOOL=ICOOL, ICOMP=ICOMP)

print ("  ................ K")
print ("  T2    ", lstRes[0])
print ("  TSUC  ", lstRes[6])
print ("  TDISC ", lstRes[7])

print ("  ................j/kg ")
print ("  HOUT  ", lstRes[1])
print ("  QHILO ", lstRes[2])
print ("  QCAN  ", lstRes[3])

print ("  ................m3/kg ")
print ("  VV2   ", lstRes[5])
print ("  VSUC  ", lstRes[4])

print ("  ................Other ")
print ("  GAMA  ", lstRes[8])
print ("  RN    ", lstRes[9])
print ("  ETAS  ", lstRes[10])
print ("  DISP  ", lstRes[11])

print ("\n\n==Test 3 =========================")
IMAP = 0
print ("Not Applicable for IMPA=0")