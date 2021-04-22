# Python import
import sys

# User import
from CoolPrp import *
from decorators import *
from read_comp_file import *
from interpolation import *

# =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=


class Comp_Map:
    #IREAD = 0  # indicator data is fetched from copressor file

    IUNITS = None
    TEDATA = None
    TCDATA = None
    CAPAC = None
    POWER = None
    objCP = None

    def __init__(self, objCP):
        Comp_Map.objCP = objCP

    @show_input_output("ALL")
    def compmap(self, PSUCT, PDISC, ICOMP,
                TSUCT, VSUCT, TAMB, GAMA, FRACT_SPEED,
                strFileName, strFolder=None):

        # ******************************************************************
        # calculates compressor performance based on tabular map
        # data and corrects for suction temperature other than 90f
        # ******************************************************************

        if (Comp_Map.IUNITS is None):  # if None then data was not fetched
            [Comp_Map.IUNITS,
             Comp_Map.TEDATA,
             Comp_Map.TCDATA,
             Comp_Map.CAPAC,
             Comp_Map.POWER
             ] = read_comp_file(strFileName, strFolder)

        # Determine the saturation temperatures corresponding to PSUCT, PDISC
        # [XX, X, TEVAPK, VL, VDEW, LCRIT] = self.bublp(PSUCT, XX, X, False)
        # [X, XX, TCONDK, VBUB, VV, LCRIT] = self.bublp(PDISC, X, XX, True)
        TEVAPK = Comp_Map.objCP.Property('T', P=PSUCT, X=0) # K
        TCONDK = Comp_Map.objCP.Property('T', P=PDISC, X=0) # K

        # Determine the enthalpies at each pressure for the following:
        # 	VAPOR - 90F or 32.2222C or 305.372K
        # 	LIQUID - 90F or 32.2222C or 305.372K

        # TEMPV = 305.372  # K (90.0 + 459.67) / 1.8  # convert from Deg-F to K
        # TEMPL = 305.372  # K (90.0 + 459.67) / 1.8  # convert from Deg-F to K
        T90F_in_K = 305.372  # K

        # # FIRST CALCULATE THE SPECIFIC VOLUMES
        # VGUESS = VDEW * T90F_in_K / TEVAPK
        # [A, B] = self.espar(0, T90F_in_K, X)
        # [VGUESS, LCONV] = self.vit(T90F_in_K, PSUCT, A, B, VGUESS, False)
        # VVAP = VGUESS    # vapor specific volume

        # VGUESS = VBUB
        # [A, B] = self.espar(0, T90F_in_K, X)
        # [VGUESS, LCONV] = self.vit(T90F_in_K, PDISC, A, B, VGUESS, True)
        # VLIQ = VGUESS    # liquid specific volume

        # vapor entering the compressor
        # [HIN, CV, CP, VS] = self.hcvcps(1, T90F_in_K, VVAP, X)
        HIN = Comp_Map.objCP.Property('H', T=T90F_in_K, P=PSUCT)  # j/kg

        # liquid leaving condenser
        #[HOUT, CV, CP, VS] = self.hcvcps(1, T90F_in_K, VLIQ, X)
        HOUT = Comp_Map.objCP.Property('H', T=T90F_in_K, P=PDISC)  # j/kg

        # determine isentropic compression enthalpy (HS)
        # SSUC = self.entrop(T90F_in_K, VVAP, X)
        SSUC = Comp_Map.objCP.Property('S', T=T90F_in_K, P=PSUCT)  # j/kg/K

        # [TS, XQS, XLS, XVS, VLS, VVS, SL, SV] = self.spin(SSUC, PDISC, X)
        TS = Comp_Map.objCP.Property('T', S=SSUC, P=PDISC)  # K

        # VGUESS = VVAP
        # [AE, BE] = self.espar(0, TS, X)
        # [VGUESS, LCONV] = self.vit(TS, PDISC, AE, BE, VGUESS, False)
        # [HS, CVF, CPF, VSND] = self.hcvcps(1, TS, VGUESS, X)
        HS = Comp_Map.objCP.Property('H', T=TS, P=PDISC)  # j/kg
        # or HS = Comp_Map.objCP.Property('H', S=SSUC, P=PDISC)  # j/kg
        
        # convert the saturation temperatures to corresspond to map data units
        if (Comp_Map.IUNITS == 2): # i.e temp in F
            TEVAP = TEVAPK * 1.8 - 459.67  # convert from Deg K to F
            TCOND = TCONDK * 1.8 - 459.67
        else:
            TEVAP = TEVAPK - 273.16  # convert from Deg K to C
            TCOND = TCONDK - 273.16

        '''
        # ====================================================
        # check if TCOND and/or TEVAP is off map data
        ICOND = 1  # =1 if in range, =0 less than min. value
        if (TCOND < Comp_Map.TCDATA[1]):
            ICOND = 0
        if (TCOND > Comp_Map.TCDATA[NCOND]):
            ICOND = NCOND

        IEVAP = 1  # =1 if in range, =0 less than min. value
        if (TEVAP < Comp_Map.TEDATA[1]):
            IEVAP = 0
        if (TEVAP > Comp_Map.TEDATA[NEVAP]):
            IEVAP = NEVAP

        # this coding will interpolate if data is within the map or
        #  extropolate if TCOND and/or TEVAP are/is less than map data

        # determine location within map

        if (ICOND <= 1 and IEVAP <= 1):
            if (ICOND == 1):
                I = 1
                while (TCOND > Comp_Map.TCDATA[I]):
                    I = I + 1

                ICOND = I - 1

            else:
                ICOND = 1

            I = 1
            if (IEVAP == 1):
                while (TEVAP > Comp_Map.TEDATA[I]):
                    I = I + 1

                IEVAP = I - 1

            else:
                IEVAP = 1

            # compressor capacity interpolation
            DELTC = Comp_Map.TCDATA[ICOND + 1] - Comp_Map.TCDATA[ICOND]
            DELTE = Comp_Map.TEDATA[IEVAP + 1] - Comp_Map.TEDATA[IEVAP]

            FRAC = (TCOND - Comp_Map.TCDATA[ICOND]) / DELTC
            CAP1 = Comp_Map.CAPAC[ICOND][IEVAP] \
                + (Comp_Map.CAPAC[ICOND + 1][IEVAP] \
                - Comp_Map.CAPAC[ICOND][IEVAP]) * FRAC

            CAP2 = Comp_Map.CAPAC[ICOND][IEVAP + 1] \
                + (Comp_Map.CAPAC[ICOND + 1][IEVAP + 1] \
                - Comp_Map.CAPAC[ICOND][IEVAP + 1]) * FRAC

            FRAC = (TEVAP - Comp_Map.TEDATA[IEVAP]) / DELTE
            CAP = CAP1 + (CAP2 - CAP1) * FRAC

            # compressor power interpolation
            FRAC = (TCOND - Comp_Map.TCDATA[ICOND]) / DELTC
            POW1 = Comp_Map.POWER[ICOND][IEVAP] \
                + (Comp_Map.POWER[ICOND + 1][IEVAP] \
                - Comp_Map.POWER[ICOND][IEVAP]) * FRAC

            POW2 = Comp_Map.POWER[ICOND][IEVAP + 1] \
                + (Comp_Map.POWER[ICOND + 1][IEVAP + 1] \
                - Comp_Map.POWER[ICOND][IEVAP + 1]) * FRAC

            FRAC = (TEVAP - Comp_Map.TEDATA[IEVAP]) / DELTE
            POW = POW1 + (POW2 - POW1) * FRAC

        # TCOND greater than or equal the maximum condensing temp data point
        if (ICOND == NCOND):
            if (IEVAP <= 1):
                I = 1
                if (IEVAP == 1):
                    while (TEVAP > Comp_Map.TEDATA[I]):
                        I = I + 1
                    IEVAP = I - 1
                else:
                    IEVAP = 1

                #  compressor capacity calculation
                DELTC = Comp_Map.TCDATA[ICOND] - Comp_Map.TCDATA[ICOND - 1]
                DELTE = Comp_Map.TEDATA[IEVAP + 1] - Comp_Map.TEDATA[IEVAP]

                FRAC = (TCOND - Comp_Map.TCDATA[ICOND]) / DELTC
                FRAC2 = FRAC

                CAP1 = Comp_Map.CAPAC[ICOND][IEVAP] \
                    + (CAPAC[ICOND][IEVAP] \
                    - Comp_Map.CAPAC[ICOND - 1][IEVAP]) * FRAC

                CAP2 = Comp_Map.CAPAC[ICOND][IEVAP + 1] \
                    + (CAPAC[ICOND][IEVAP + 1]  \
                    - Comp_Map.CAPAC[ICOND - 1][IEVAP + 1]) * FRAC

                FRAC = (TEVAP - Comp_Map.TEDATA[IEVAP]) / DELTE
                CAP = CAP1 + (CAP2 - CAP1) * FRAC

                #  compressor power calculation
                FRAC = (TCOND - Comp_Map.TCDATA[ICOND]) / DELTC

                POW1 = Comp_Map.POWER[ICOND][IEVAP] + \
                    (POWER[ICOND][IEVAP] - POWER[ICOND - 1][IEVAP]) * FRAC

                POW2 = Comp_Map.POWER[ICOND][IEVAP + 1] \
                        + (POWER[ICOND][IEVAP + 1] \
                        - POWER[ICOND - 1][IEVAP + 1]) * FRAC

                FRAC = (TEVAP - Comp_Map.TEDATA[IEVAP]) / DELTE
                POW = POW1 + (POW2 - POW1) * FRAC

            else:
                # COMPRESSOR CAPACITY CALCULATION
                DELTC = Comp_Map.TCDATA[ICOND] - Comp_Map.TCDATA[ICOND - 1]
                DELTE = Comp_Map.TEDATA[IEVAP] - Comp_Map.TEDATA[IEVAP - 1]
                FRAC = (TCOND - Comp_Map.TCDATA[ICOND]) / DELTC

                CAP1 = Comp_Map.CAPAC[ICOND][IEVAP - 1] \
                    + (CAPAC[ICOND][IEVAP - 1] \
                    - CAPAC[ICOND - 1][IEVAP - 1]) * FRAC

                CAP2 = Comp_Map.CAPAC[ICOND][IEVAP] \
                     + (CAPAC[ICOND][IEVAP] \
                     - CAPAC[ICOND - 1][IEVAP]) * FRAC

                FRAC = (TEVAP - Comp_Map.TEDATA[IEVAP]) / DELTE
                CAP = CAP2 + (CAP2 - CAP1) * FRAC

                # compressor power calculation
                FRAC = (TCOND - Comp_Map.TCDATA[ICOND]) / DELTC
                POW1 = Comp_Map.POWER[ICOND][IEVAP - 1] \
                        + (Comp_Map.POWER[ICOND][IEVAP - 1] \
                        - Comp_Map.POWER[ICOND - 1][IEVAP - 1]) * FRAC

                POW2 = Comp_Map.POWER[ICOND][IEVAP] \
                    + (Comp_Map.POWER[ICOND][IEVAP] \
                    - Comp_Map.POWER[ICOND - 1][IEVAP]) * FRAC

                FRAC = (TEVAP - Comp_Map.TEDATA[IEVAP]) / DELTE
                POW = POW2 + (POW2 - POW1) * FRAC

        # condensing temperature not greater than maximum of map data
        #   evaporating temperature greater than maximum of map data
        #
        if (IEVAP == NEVAP and ICOND < NCOND):
            if (ICOND == 1):
                I = 1
                while (TCOND > Comp_Map.TCDATA[I]):
                    I = I + 1
                ICOND = I - 1
            else:
                ICOND = 1

            # compressor capacity calculation
            DELTC = Comp_Map.TCDATA[ICOND + 1] - Comp_Map.TCDATA[ICOND]
            DELTE = Comp_Map.TEDATA[IEVAP] - Comp_Map.TEDATA[IEVAP - 1]
            FRAC = (TCOND - Comp_Map.TCDATA[ICOND]) / DELTC

            CAP1 = Comp_Map.CAPAC[ICOND][IEVAP - 1] \
                    + (Comp_Map.CAPAC[ICOND + 1][IEVAP - 1] \
                    - Comp_Map.CAPAC[ICOND][IEVAP - 1]) * FRAC

            CAP2 = Comp_Map.CAPAC[ICOND][IEVAP] \
                    + (Comp_Map.CAPAC[ICOND + 1][IEVAP] \
                    - Comp_Map.CAPAC[ICOND][IEVAP]) * FRAC

            FRAC = (Comp_Map.TEVAP - Comp_Map.TEDATA[IEVAP]) / DELTE
            CAP = CAP2 + (CAP2 - CAP1) * FRAC

            #  COMPRESSOR POWER CALCULATION
            FRAC = (TCOND - Comp_Map.TCDATA[ICOND]) / DELTC

            POW1 = Comp_Map.POWER[ICOND][IEVAP - 1] \
                    + (Comp_Map.POWER[ICOND + 1][IEVAP - 1] \
                    - Comp_Map.POWER[ICOND][IEVAP - 1]) * FRAC

            POW2 = Comp_Map.POWER[ICOND][IEVAP] \
                    + (Comp_Map.POWER[ICOND + 1][IEVAP] \
                    - Comp_Map.POWER[ICOND][IEVAP]) * FRAC

            FRAC = (TEVAP - Comp_Map.TEDATA[IEVAP]) / DELTE
            POW = POW2 + (POW2 - POW1) * FRAC
        # ==============================================
        '''
        
        CAP = interpolation (x_value=TEVAP, y_value=TCOND,
                             x_series=Comp_Map.TEDATA,
                             y_series=Comp_Map.TCDATA,
                             data=Comp_Map.CAPAC)

        POW = interpolation (x_value=TEVAP, y_value=TCOND,
                             x_series=Comp_Map.TEDATA,
                             y_series=Comp_Map.TCDATA,
                             data=Comp_Map.POWER)

        # handle off-speed operation (use Danfoss variable speed data)
        REL_CAP = -0.046073 \
                  + 1.41364 * FRACT_SPEED \
                  - 0.366744 * FRACT_SPEED * FRACT_SPEED

        CAP = CAP * REL_CAP
        POW = POW * REL_CAP

        # correct for power term based on UI document UILU-ENG_96-4022
        REL_POW = 0.9535 + 0.04565 * FRACT_SPEED
        POW = POW * REL_POW

        # convert the capacity to kj/hr
        if (Comp_Map.IUNITS == 2): # i.e temp in F
            CAP = CAP * 1.0548  # from but/hr to kj/hr
        else:
            CAP = CAP * 4.184   # from kcal/hr to kj/hr

        if (Comp_Map.IUNITS != 1 and Comp_Map.IUNITS != 2):
            print("###CHECK COMPRESSOR MAP UNITS###")

        # WDOT = POW

        # convert to kj/hr
        WDOT90 = POW / 1000.0 * 3600.0  # from watt to kj/hr
        # WDOT90 = WDOT

        # calculate the mass flow rate in kg/hr
        # MDOT = 1000 * CAP / (HIN - HOUT) # kg/hr=(kj/hr)/(j/kg) *1000
        MDOT90 = 1000 * CAP / (HIN - HOUT) # kg/hr=(kj/hr)/(j/kg) *1000

        # MDOT90 = MDOT # kg/hr

        # correct mass flow rate for suction temperature other than 90 f
        # T90 = 305.372  # K equall 90 F  not used

        # --AymanFix---------------This part of code done in Python only
        # Check that point @ PSUCT and T90F_in_K (90 F or 305.372 K)
        #  is not in wet area
        if Comp_Map.objCP.is_two_phase(
                Comp_Map.objCP.phase_byPressTemp (PSUCT, T90F_in_K)):
            VVAP = Comp_Map.objCP.Property('V', T=T90F_in_K, X=1)  # m3/kg
        else:
            VVAP = Comp_Map.objCP.Property('V', T=T90F_in_K, P=PSUCT)  # m3/kg
        # -- -------------End of code by Python

        MDOT = MDOT90 * VVAP / VSUCT  # kg/hr

        # estimate effect on compressor power as ratio of
        #  suction plenum temperatures
        # none =(kg/hr) * (j/kg)     /(kj/hr) /1000
        EFFS = MDOT90 * (HS - HIN) / WDOT90 /1000

        # [HSUCT, CV, CP, VS] = self.hcvcps(1, TSUCT, VSUCT, X)
        # SSUC = self.entrop(TSUCT, VSUCT, X)
        # [TS, XQS, XLS, XVS, VLS, VVS, SL, SV] = self.spin(SSUC, PDISC, X)

        # VGUESS = VSUCT
        # [AE, BE] = self.espar(0, TS, X)
        # [VGUESS, LCONV] = self.vit(TS, PDISC, AE, BE, VGUESS, False)
        # [HS, CVF, CPF, VSND] = self.hcvcps(1, TS, VGUESS, X)

        HSUCT = Comp_Map.objCP.Property('H', T=TSUCT, P=PSUCT)  # j/kg
        SSUC = Comp_Map.objCP.Property('S', T=TSUCT, P=PSUCT)  # j/kg/K
        TS = Comp_Map.objCP.Property('T', S=SSUC, P=PDISC)  # K
        HS = Comp_Map.objCP.Property('H', T=TS, P=PDISC)  # j/kg

        # kj/hr = kg/hr * J/kg /1000
        WDOT = MDOT * (HS - HSUCT) / EFFS / 1000

        # estimate shell heat loss including effect of different ambient
        if (ICOMP == 1):  # Reciprocating compressor
            DELTIN = 67.0
            TSUCTF = TSUCT * 1.8 - 459.67
            DTSUCT = 67.0 - 0.43333 * (TSUCTF - 90.0)
            TSP = TSUCT + DTSUCT / 1.8

        else: # Rotary compressor
            DELTIN = 30.0
            TSP = TSUCT + DELTIN / 1.8

        # EXP = (GAMA - 1.0) / GAMA
        # PRAT = PDISC / PSUCT
        TSP90 = (90.0 + DELTIN + 459.67) / 1.8

        AAA = (PDISC / PSUCT)**((GAMA - 1.0) / GAMA)
        TMAX90 = TSP90 * AAA
        TMAX = TSP * AAA

        T90K = 305.372  # K (90.0 F + 459.67) / 1.8
        # RATIO = (TMAX - TAMB) / (TMAX90 - T90K)

        # QLOSS = 0.90  # useless (found in FORTRAN)
        QLOSS = 0.80
        QSHELL = WDOT * QLOSS * (TMAX - TAMB) / (TMAX90 - T90K)  # kj/hr

        return [TSP, WDOT, MDOT, QSHELL]

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.
    @show_input_output("ALL")
    def compcall( self, objCP, PSUCT, PDISC, TSUCT, VSUCT,
                  TAMB, MREF, FRACT_SPEED, strFileName, ICOMP):
        # ************************************************************
        # true compressor map routine.  applies to refrigerant        *
        # subroutine compcall calculates isentropic compressor        *
        # performance and calls subroutine compmap                    *
        # *************************************************************

        # determine isentropic compressor performance
        # XL = [0.0] * (5 + 1)  # modification in Python
        # XV = [0.0] * (5 + 1)

        # TSUCT = T1
        # PSUCT = P1
        # VSUCT = V1
        H1 = objCP.Property('H', T=TSUCT, V=VSUCT)  # j/kg
        
        # [HVSUCT, CV, CP, VS] = self.hcvcps(3, TSUCT, VSUCT, X)
        CP = objCP.Property('CP', T=TSUCT, V=VSUCT)  # j/kg/K
        CV = objCP.Property('CV', T=TSUCT, V=VSUCT)  # j/kg/K

        GAMA = CP / CV
        # SSUCT = self.entrop(TSUCT, VSUCT, X)
        SSUCT = objCP.Property('S', T=TSUCT, V=VSUCT)  # S in j/kg/K

        # [XL, X, TDEW, XXX, VDEW, LCRIT] = self.bublp(P[2], XL, X, False)
        # [T2S, XQ2S, XL2S, XV2S, VL2, VV2, SL, SV] = self.spin(SSUCT, P[2], X)
        # [A2S, B2S] = self.espar(0, T2S, X)
        # VGUESS = VSUCT * T2S / TSUCT * PSUCT / PDISC
        # [VGUESS, LCRIT] = self.vit(T2S, P[2], A2S, B2S, VGUESS, False)
        # VV2 = VGUESS

        # if (XQ2S < 1.0):
        # if objCP.is_two_phase(objCP.phase_byPressTemp (PDISC, T2S)):
            # # [HL2, CV, CP, VS] = self.hcvcps(1, T2S, VL2, XL2S)
            # HL2 = objCP.Property('H', T=T2S, V=VL2)  # j/kg

            # # [HV2, CV, CP, VS] = self.hcvcps(1, T2S, VV2, XV2S)
            # HV2 = objCP.Property('H', T=T2S, V=VV2)  # j/kg

            # # In Python Only: calculate quality
            # Sgas = objCP.Property('S', P=PDISC, X=0)  # j/kg/K
            # Slqu = objCP.Property('S', P=PDISC, X=1)  # j/kg/K
            # X = (SSUCT - Slqu)/(Sgas - Slqu)

            # H2S = X * HV2 + (1.0 - X) * HL2
        # else:
            # # [HV2, CV, CP, VS] = self.hcvcps(1, T2S, VV2, XV2S)
            # HL2 = objCP.Property('H', T=T2S, V=VV2)  # j/kg
            # H2S = HV2

        H2S = objCP.Property('H', S=SSUCT, P=PDISC)  # j/kg

        # calculate isentropic power requirement
        # moved by Dr-Omar WDOTS = MREF * (H2S - H1)/1000  # kj/hr = kg/hr * (j/kg)/1000

        # determine actual compressor performance [TSP, WDOT, MDOT, QSHELL]
        [TSUC, WDOT, MREF, QSHELL] =\
        self.compmap(PSUCT=PSUCT, PDISC=PDISC, TSUCT=TSUCT,  \
                            VSUCT=VSUCT, \
                            strFileName=strFileName,\
                            GAMA=GAMA, TAMB=TAMB, \
                            FRACT_SPEED=FRACT_SPEED, ICOMP=ICOMP)

        # calculate isentropic power requirement
        WDOTS = MREF * (H2S - H1)/1000  # kj/hr = kg/hr * (j/kg)/1000
        
        # calculate refrigerant exit enthalpy and temperature
        # fact = QSHELL / WDOT  # useless not used anywhere

        # j/kg = (j/kg) + (kj/hr) /(kg/hr) *1000
        HOUT = H1  + (WDOT - QSHELL) / MREF * 1000  # j/kg

        # HOUT = H2  # send to output
        QCAN = QSHELL / WDOT  # no unit 

        # [T2, XQ[2], XL, XV, VL2, VV2, HL2, HV2] = self.hpin(H[2], P[2], X)
        TDISC = objCP.Property('T', H=HOUT, P=PDISC)  # K
        VV2 = objCP.Property('V', H=HOUT, P=PDISC)  # m3/kg

        # calculate isentropic efficiency
        ETAS = WDOTS / WDOT  # none = (kj/hr) / (kj/hr)

        # use call statement arguments to avoid compilier warning

        # MEFF = MEFF useless
        VSUC = VSUCT
        QHILO = 0.0 # use less feedback
        RN = 0.97 * GAMA

        return [ HOUT, QHILO, QCAN, VSUC, VV2, 
                TSUC, TDISC, GAMA, RN, ETAS, MREF]

