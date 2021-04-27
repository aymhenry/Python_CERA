# Python import
# import sys

# User import
from cycle_classes.CoolPrpUtil import *
from cycle_classes.exf4Cond_Evap import *


class CycleUtils(exf4Cond_Evap):
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=

    def enthal(self, objCP, HBUB, HDEW, XSPEC, P):
        # ITERATES TO DETERMINE THE ENTHALPY.
        #  THE PRESSURE AND QUALITY ARE INPUTS

        # MAKE INITIAL GUESS ASSUMING A LINEAR VARIATION IN ENTHALPY
        #   WITH THE EXIT QUALITY
        #
        coolutil = CoolPrpUtil(objCP)
        HGUESS = HBUB + (HDEW - HBUB) * XSPEC
        DELH = 0.0

        ITERH = 0
        XTOL = 1000.0

        while ITERH < 100 and XTOL >= 0.001:
            HGUESS = HGUESS + DELH
            if HGUESS < HBUB:
                HGUESS = HBUB * 1.01
            if HGUESS > HDEW:
                HGUESS = HDEW * 0.99
            
            # [T, XCALC, XL, XV, VL, V, HL, HV] = self.hpin(HGUESS, P, X)
            # not used T = objCP.Property('T', P=P, H=HGUESS)  # K
            # Python only
            H_liq = objCP.Property('H', P=P, X=0)  # j/kg
            H_gas = objCP.Property('H', P=P, X=1)  # j/kg
            XCALC = (H_gas - H_liq) / H_gas
            # End of Python addition
            if XCALC < 0.0:
                XCALC = 0.001
            if XCALC > 1.0:
                XCALC = 0.999
            #
            #        ADJUST ENTHALPY GUESS
            #
            ALPHAH = 1.0 - XCALC / XSPEC
            XTOL = abs(ALPHAH)
            DELH = (HDEW - HBUB) * ALPHAH
            ITERH += 1

        H = HGUESS

        return H    # j/kg
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.

    @staticmethod
    def adjlod(dt, cab, ds, IC, TS3, TS5, FROSTF, FROSTZ, IDFRST, ATOTE, AREAFZ):
        # adjust the cabinet loads and set point temperatures
        # dt input data object.
        # ds CycleSolver object
        # cab input data from Cab application results
        # IC conderser trail number

        # TS3 - K - HTF temperature entering fresh food evaporator
        # TS5 - K - HTF temperature entering freezer evaporator

        # FROSTF: watt,Fresh Food Door Frost Load
        # FROSTZ: watt,Freezer Door Sensible Load

        # IDFRST: #, if 0 Manual Defrost, auto 1

        # BRANCH ON THE VALUE IC.AVE VARIABLES AND INITIALIZE ON THE
        # FIRST CALL AND: WAIT UNTIL THE 4TH CALL TO MAKE ADJUSTMENTS
        # ATOTE   m2  total area for Evap
        # AREAFZ  m2  Area for freezer
        
        ICYCL = 1
        IRET = 0  # in Python only
        
        ATOTE_S = ATOTE
        AREAFZ_S = AREAFZ
            
        if IC == 1:
            dt.IBLNCE = 0
            # untused IRET = 0

            # if ICYCL != 2:
            #    IRET = 1
            
            # keep those equation, as it was set in Fortarn
            # FFTEMP_S = dt.FFTEMP
            # FZTEMP_S = dt.FZTEMP

            # FFQ_S = cab.FFQ
            # FZQON_S = dt.FZQON
            # FZQOFF_S = cab.FZQOFF

            # FFLAT_S = cab.FFLAT
            # FZLAT_S = cab.FZLAT

            # FFSEN_S = cab.FFSEN
            # FZSEN_S = cab.FZSEN

            # FFHTQ_S = dt.FFHTQ
            # FZHTQ_S = cab.FZHTQ

            # FROSTF_S = FROSTF
            # FROSTZ_S = FROSTZ

            # CONDF_S = dt.CONDF
            # CONDZ_S = dt.CONDZ

            ATOTE_S = ATOTE
            AREAFZ_S = AREAFZ

            # UAF_S = dt.UAF  # only in Type = 2 cycle
            ds.ATOTE_A = ATOTE
            ds.AREAFZ_A = AREAFZ

             # keep those equation, as it was set in Fortarn
            # UFF = (cab.FFQ - cab.FFLAT - cab.FFPENA - dt.FFHTQ -
            #       FROSTF + cab.QMUL) / (dt.TROOM - dt.FFTEMP)

            # FZQ = dt.FZQON
            # FZQ_S = FZQ

            # UFZ = (FZQ - cab.FZLAT - cab.FZPENA - cab.FZHTQ -
            #       FROSTZ - cab.QMUL) / (dt.TROOM - dt.FZTEMP)

            # UFF_SEN = FFSEN_S / (dt.TROOM - FFTEMP_S)
            # UFZ_SEN = FZSEN_S / (dt.TROOM - FZTEMP_S)

            # UCND_F = (dt.CONDF + cab.QMUL) / \
            #    (dt.TROOM - FFTEMP_S)
            # UCND_Z = (dt.CONDZ - cab.QMUL) / \
            #    (dt.TROOM - FZTEMP_S)

            # TS3_S = TS3
            # TS5_S = TS5

            ds.FFTEMP_A = cab.FFTEMP
            ds.FZTEMP_A = cab.FZTEMP

            # DELTS5_OLD = 0

            # UA_FZ_S = dt.UA_FZ
            # UA_ML_S = dt.UA_ML
            # UA_FF_S = dt.UA_FF

        elif IC in [2, 3]:
            pass

        else:
            if IRET == 1:
                return [TS3, TS5, ATOTE, AREAFZ]

            # Determine needed rebalancing of cabinet loads
            FFLOAD = dt.DUTYE * dt.CAPE + dt.DUTYZ * dt.Q_FZ_FF
            FZLOAD = dt.DUTYZ * dt.CAPZ

            # DELLOD = (FFLOAD * dt.CAPZ - FZLOAD *
            #          dt.CAPE) / (dt.CAPE + dt.CAPZ)

            DUTMAX = max(dt.DUTYE, dt.DUTYZ)
            DUTMIN = min(dt.DUTYE, dt.DUTYZ)
            DUTDIF = DUTMAX - DUTMIN

            dt.IBLNCE = 0
            DUTERR = DUTDIF / DUTMAX

            if DUTERR <= 0.001:
                return [TS3, TS5, ATOTE, AREAFZ]

            if DUTDIF >= 0.025:
                dt.IBLNCE = 1

            if dt.INCTRL == 0:
                return [TS3, TS5, ATOTE, AREAFZ]

            elif dt.INCTRL == 1:    # Evap area ratio
                # FFNEW = FFLOAD + DELLOD
                # FZNEW = FZLOAD - DELLOD

                # DAREAF = (FFNEW / FFLOAD - 1.0) * ATOTE
                # DAREAZ = (FZNEW / FZLOAD - 1.0) * AREAFZ

                DUTY_AVE = (dt.DUTYE + dt.DUTYZ) / 2.0
                DAREAF = (dt.DUTYE / DUTY_AVE - 1.0) * ATOTE
                DAREAZ = (dt.DUTYZ / DUTY_AVE - 1.0) * AREAFZ

                RATIOF = DAREAF / ATOTE

                if RATIOF < -0.5:
                    RATIOF = -0.5

                DAREAF = RATIOF * ATOTE * 0.5
                RATIOZ = DAREAZ / AREAFZ

                if RATIOZ < -0.5:
                    RATIOZ = -0.5

                DAREAZ = RATIOZ * AREAFZ * 0.5

                if abs(DAREAF) < abs(DAREAZ):
                    ATOTE = ATOTE + DAREAF
                    AREAFZ = AREAFZ - DAREAF

                else:
                    AREAFZ = AREAFZ + DAREAZ
                    ATOTE = ATOTE - DAREAZ

                dt.UAF = dt.UAF_S * AREAFZ / AREAFZ_S   # dt.AREAFZ_S
                dt.ATOTE_A = ATOTE    # m2
                dt.AREAFZ_A = AREAFZ   # m2

                dt.UA_FZ = dt.UA_FZ_S * AREAFZ / AREAFZ_S   # dt.AREAFZ_S
                dt.UA_ML = dt.UA_ML_S * AREAFZ / AREAFZ_S   # dt.AREAFZ_S
                dt.UA_FF = dt.UA_FF_S * ATOTE / ATOTE_S   # dt.ATOTE_S

            elif dt.INCTRL == 2:    # FF Cabinet temp
                DUTYN = 0.5 * (dt.DUTYE + dt.DUTYZ)
                cab.FFQ = DUTYN * dt.CAPE + DUTYN * dt.Q_FZ_FF + dt.FROSTF_S
                DELTS3 = (cab.FFQ - dt.FFQ_S) / dt.UFF

                # check temp units--- dr. Omar
                # TS3 = TS3_S - DELTS3 / 1.8
                # dt.FFTEMP_A = 1.8 * TS3 - 459.6

                TS3 = dt.TS3_S - DELTS3
                dt.FFTEMP_A = TS3

                cab.FFSEN = dt.UFF_SEN * (cab.TROOM - dt.FFTEMP_A)
                dt.CONDF = dt.UCND_F * (cab.TROOM - dt.FFTEMP_A) - cab.QMUL

            elif dt.INCTRL == 3:    # Freezer temp
                DUTYN = 0.25 * dt.DUTYE + 0.75 * dt.DUTYZ
                FZQ = DUTYN * dt.CAPZ

                if IDFRST == 0:
                    FZQ = FZQ - dt.FROSTZ_S

                DELTS5 = 0.3 * (FZQ - dt.FZQ_S) / dt.UFZ + 0.7 * dt.DELTS5_OLD
                # DELTS5_OLD = DELTS5

                FZQ = dt.FZQ_S + dt.UFZ * DELTS5
                dt.FZQON = FZQ
                cab.FZQOFF = FZQ

                # TS5 = dt.TS5_S - DELTS5 / 1.8
                TS5 = dt.TS5_S - DELTS5     # K
                
                # dt.FZTEMP_A = 1.8 * TS5 - 459.6
                dt.FZTEMP_A = TS5   # K

                cab.FZSEN = dt.UFZ_SEN * (cab.TROOM - dt.FZTEMP_A)
                dt.CONDZ = dt.UCND_Z * (cab.TROOM - dt.FZTEMP_A) + cab.QMUL

            elif dt.INCTRL in [4, 5]:
                pass

        return [TS3, TS5, ATOTE, AREAFZ]

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def lowevp(self, dt, ds, cab, objCP, MREF,
               # ICYCL,
               ICNTRL,
               H, P, T,
               TS3, TS5,
               DPF, ETHX2
               ):
        # ICNTRL - CONTROL METHOD FOR EVAPORATOR LOAD
        #  FREEZER EVAPORATOR AND LOWER INTERCHANGER

        coolutil = CoolPrpUtil(objCP)
        NCALL = 0
        
        ICYCL = 1
        
        # SET UP PRESSURES AND QUALITIES
        P[10] = P[6]
        P[9] = P[5]
        P[8] = P[9] + DPF
        #  XQ[10] = 0
        ETHX = ETHX2
        TSAV = T[9]

        TBUB = objCP.Property('T', P=P[8], X=0)  # K
        HBUB = objCP.Property('H', P=P[8], X=0)  # j/kg

        if NCALL == 0:   # Python all times NCALL = 0 !!!
            TDEW = objCP.Property('T', P=P[9], X=1)  # K
            HDEW = objCP.Property('H', P=P[9], X=1)  # j/kg

            # dt.CREF watt/K
            dt.CREF = (MREF / 3600) * (HDEW - HBUB) / (TDEW - TBUB + 0.001)
            if dt.CREF <= 0.1:
                dt.CREF = 1000000.0  # 5/9/94

            T[10] = TS5
            # NCALL = 1

        # set flag for possible solution and handle single evap case

        IFREZ2 = 1
        if TBUB >= TS5:
            IFREZ2 = 0

        # VL[10] = VL[6]
        if IFREZ2 == 0:
            H[9] = H[6]
            H[10] = H[6]
            T[10] = T[6]

        ETHX = 0

        # begin iteration for temperature at point 10
        ITER = 1
        # the next statment by Ayman VL[10] = VL[6]
        VL10 = objCP.Property('V', P=P[6], H=H[6])  # m3/kg
        # 10 CONTINUE

        while True:
            ITER += 1

            H[10] = objCP.Property('H', T=T[10], V=VL10)  # j/kg

            # use T[10]
            T[10] = objCP.Property('T', P=P[10], H=H[10])  # K

            H[8] = H[10]
            T[8] = objCP.Property('T', P=P[8], H=H[8])  # K

            CMIN = min(dt.CREF, dt.CFMF)   # watt/K
            CMAX = max(dt.CREF, dt.CFMF)   # watt/K

            CAPRAT = CMIN / CMAX        # unitless

            # in Python Only
            # define var to prevent var is not defined
            QFREZ = TAVE = ERROR = TNEW = 0

            if CMIN <= 0.0:
                CMIN = 0.001
            
            FNTU = dt.UAF / CMIN    # unitless
            if FNTU < 0.0:
                FNTU = 0.0

            #          CALCULATE EFFECTIVENESS
            #
            dt.UAFZ = dt.UAF    # watt/K
            if IFREZ2 == 1:
                if dt.IFREZ == 0:
                    # TAVE = (T[8] + T[9]) / 2.0   # Dr Omar

                    if T[9] < -1000.0:
                        TAVE = T[8]  # Jan 20, 1993
                    
                    TAVE = (T[8] + T[9]) / 2.0   # Dr Omar moved
                    
                    if TAVE > TS5:
                        TAVE = TS5 - 1.0

                    QMAX = 0.90 * (MREF / 3600) * (H(7) - H[6])   # watt

                    # in Fortran HRAD is converted from kW/m2 K using eq:-
                    # kW/m2 K = 0.04892 Btu/(s ft2 F)
                    # so HRAD in kW/m2 K
                    
                    # W/m2 K modification by Dr omar
                    # HRAD = SIGMA * (TAVE + TS5) * (TAVE**2 + TS5**2) * EPS
                    
                    HRAD = self.getHRAD(TAVE, TS5, 0.8)     # W/m2 K
                    DELTAT = TS5 - TAVE

                    if DELTAT <= 0.0:
                        DELTAT = 0.0001

                    # Dr. Omar Units
                    # DELTA = DELTAT * 1.8
                    # DELTA in K

                    # DELTA = DELTAT * 1.8   # defrance to F

                    TBAR = 0.67 * TAVE + 0.33 * TS5
                    A_NAT = 0.239 + 3.34E-04 * (273.0 - TBAR)
                    # HNAT = A_NAT * (DELTA**0.33) * 20.44   # W/m2 K
                    
                    # Dr. Omar modification
                    HNAT = self.getHNAT(DELTAT, A_NAT)   # W/m2 K

                    #  MAKE APPROXIMATE CORRECTIONS FOR VIEW FACTORS AND ASSUMED
                    #    ORIENTATION OF THE EVAPORATOR PANELS.
                    #
                    #  HRAD = (1.0 - FRACT_FZ)*HRAD
                    #  HNAT = 0.5*HNAT

                    # Dr. Omar
                    UAIR = HRAD + HNAT   # W/m2 K

                    if dt.IWALL_FZ == 1:
                        UAIR = 1.0 / (1.0 / UAIR + 0.1389 / 20.44)    # kW/m2 K

                    # UAF[m2] * UAIR[watt/m2 K] * DELTAT[K]
                    QFREZ = dt.UAF * UAIR * DELTAT      # as Q_HXS_FZ watt

                    # UAF[m2] * UAIR[watt/m2 K]
                    dt.UAFZ = dt.UAF * UAIR     # watt/K

                    # Dr Omar Temp Unit
                    # TENV = (TROOM + 459.6) / 1.8
                    TENV = cab.TROOM

                    # Q_HXS_FZ in watt as given from Condenser class

                    # Dr Omar Heat Unit
                    # QFREZ = QFREZ + 1.8 * UA_FZ * (TENV - TAVE) * 1.0548 \
                    # + 1.8 * UA_ML * (TS3 - TAVE) * 1.0548 + Q_HXS_FZ

                    # UA_FZ Watt/K,  1.0548 btu to j (1 BTU = 1.0548 J)
                    # QFREZ[watt] + UA_FZ[watt/K] * [K] + UA_ML[watt/K]*[K]
                    QFREZ = QFREZ + dt.UA_FZ * (TENV - TAVE)  \
                        + dt.UA_ML * (TS3 - TAVE) + ds.Q_HXS_FZ  # watt

                    if QFREZ > QMAX:
                        QFREZ = QMAX

                elif dt.IFREZ == 1:
                    EXFR = self.efcross(CAPRAT, FNTU)    # unitless

                    QFREZ = EXFR * CMIN * (TS5 - T[8])   # watt
                    dt.ETAF = EXFR      # unitless

                elif dt.IFREZ == 2:
                    XX = 1.0 - CAPRAT    # unitless
                    XXX = math.exp(-FNTU * XX)   # unitless
                    EXFR = (1.0 - XXX) / (1.0 - CAPRAT * XXX)    # unitless
                    QFREZ = EXFR * CMIN * (TS5 - T[8])   # watt
                    dt.ETAF = EXFR      # unitless

                TS6 = TS5 - QFREZ / dt.CFMF     # K

                if dt.IFREZ == 0:
                    TS6 = 0.9 * TAVE + 0.1 * TS5     # K
            else:
                QFREZ = 0.0
                TS6 = TS5     # K

            # UPDATE ENTHALPY ACROSS EVAPORATOR

            H[9] = H[8] + QFREZ / (MREF / 3600)   # MREF kg/hr *3600
            T[9] = objCP.Property('T', P=P[9], H=H[9])  # K

            if IFREZ2 == 0:
                break  # GO TO 20

            # GET NEW GUESS FOR TEMPERATURE AT POINT 10
            TOLD = T[10]
            if ICYCL != 2 or ICNTRL != 2:
                TNEW = T[6] - ETHX * (T[6] - T[9])     # K

            else:
                TD = objCP.Property('T', X=1, P=P[9])  # K
                if TD > T[6]:
                    TNEW = T[6] - ETHX * (T[6] - T[9])

                else:
                    # to be checked later no VL[10]

                    # Dr. Omar to approve
                    # Ayman modification, in case DTSUPI = 0
                    # the given point came to wet area.
                    # check if in wet area, return sat. liquid or sat. vap.
                    # HHIGH = self.objCP.Property('H', V=VL[10], T=T[9])  # j/kg
                    # HLOW = self.objCP.Property('H', X=0, T=T[6])  # j/kg

                    # VL10 = objCP.Property('V', P=P[6], H=H[6])  # m3/kg
                    HHIGH = H[6]
                    # HHIGH = coolutil.getProp(prp='H', V=VL10,   # self.VL[10]
                    #                              T=T[9], X=0)    # j/kg

                    HLOW = coolutil.getProp(P=dt.P[1],
                                            T=T[6])   # j/kg

                    DH = min((HLOW - H[9]), (H[6] - HHIGH))
                    H[10] = H[6] - DH
                    T[10] = objCP.Property('T', P=P[10], H=H[10])   # K

            # correct guess if necessary and calculate error

            if T[9] > T[6]:
                TNEW = T[10] - 0.9 * ERROR
                T[10] = TNEW

            T[10] = TNEW
            ERROR = TSAV - T[9]
            TSAV = T[9]

            if abs(ERROR) < dt.TOL_FRZ:
                break  # GO TO 20

            # j/hr K      = kg/hr . # watt/K
            dt.CREF = MREF / 3600 * abs((H[9] - H[8]) / (T[9] - T[8] + 0.0001))
            # Dr omar units

            if dt.CREF <= 0.1:
                dt.CREF = 1000000.0  # /5/9/94

            ITER += 1

            if abs(TOLD - TNEW) > 2.0:  # 5/9/94
                if TOLD > TNEW:
                    TNEW = TOLD - 2.0  # 5/9/94
                if TOLD < TNEW:
                    TNEW = TOLD + 2.0  # 5/9/94

            if ITER > 2:
                T[10] = 0.5 * (TOLD + TNEW)  # 5/9/94

            if ITER > 10:
                break  # GO TO 20
        # GO TO 10
        # END OF ITERATION.  CALCULATE NEEDED PARAMETERS
        # 20 CONTINUE
        
        H[5] = H[9] + (H[6] - H[10])
        T[5] = objCP.Property('T', P=P[5], H=H[5])  # K

        T[10] = objCP.Property('T', P=P[10], H=H[10])  # K

        # j/kg   pa   K  watt
        return [H, P, T, TS6, QFREZ]

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    @staticmethod
    def efcross(CRAT, NTU):
        # CALCULATES THE HEAT TRANSFER EFFECTIVENESS FOR A CROSS     *
        # FLOW HEAT EXCHANGER WITH BOTH FLUIDS UNMIXED               *

        A = [	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
              [0.0, 2.394292, -1.19402, -1.45067, 1.938453, -0.81305, 0.118651],
              [0.0, 2.410798, -2.23391, 0.8259, 0.051006, -0.11891, 0.02336],
              [0.0, 2.399687, -2.96882, 2.36708, -1.23009, 0.373338, -0.04886],
              [0.0, 2.359642, -3.3765, 3.04862, -1.63421, 0.468741, -0.05492],
              ]

        I_fact = 0

        if 0.00 <= CRAT <= 0.25:
            I_fact = 1

        if 0.25 < CRAT <= 0.50:
            I_fact = 2

        if 0.50 < CRAT <= 0.75:
            I_fact = 3

        if 0.75 < CRAT <= 1.00:
            I_fact = 4

        if NTU <= 0.0:
            NTU = 0.0

        BETA = math.log10(NTU + 1.0)
        EFFA = 0.0
        EFFB = 0.0

        for J in range(1, 6 + 1):
            EX = 1.0 * J
            if I_fact == 1:
                EFFA = 1.0 - math.exp(-NTU)
            else:
                EFFA += A[I_fact - 1][J] * BETA ** EX

            EFFB += A[I_fact][J] * BETA ** EX

        FRAC = (CRAT - (I_fact - 1) * 0.25) / (I_fact * 0.25 - (I_fact - 1) * 0.25)
        EFFECT = EFFA + FRAC * (EFFB - EFFA)

        if EFFECT > 1.0:
            EFFECT = 1.0
        return EFFECT

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    @staticmethod
    def inter2(objCP, PA, TAI, HAI, VAI, PB, HBO, TDEW, HDEW, VDEW, ETA):
        # iterates to solve for interchanger heat transfer knowing
        # the inlet state of one stream and outlet state of the
        # other for a counterflow heat exchanger.
        # equal mass flow rates of the same fluid

        # KNOWN: INLET STATE OF STREAM A
        #        OUTLET STATE OF STREAM B

        # GUESS THE INLET STATE FOR STREAM B
        coolutil = CoolPrpUtil(objCP)

        HBI = HDEW - 5.0
        ITER = 0
        HTOL = 1000.0

        TBI = QACT = 0

        while ITER <= 100 and HTOL > 0.001:
            TBI = objCP.Property('T', H=HBI, P=PB)  # K

            # DETERMINE EXIT STATE OF STREAM A if AT TBI

            # Dr. Omar to approve
            # Ayman modification, in case DTSUPI = 0
            # the given point came to wet area.
            # check if in wet area, return sat. liquid or sat. vap.
            # HAOSTR = self.objCP.Property('H', X=0, T=TBI)  # j/kg

            HAOSTR = coolutil.getProp(P=PA,
                                      T=TBI, X=0)  # j/kg
            DHAMAX = HAI - HAOSTR

            # DETERMINE EXIT STATE OF STREAM B if AT TAI
            # Dr. Omar to approve
            # Ayman modification, in case DTSUPI = 0
            # the given point came to wet area.
            # check if in wet area, return sat. liquid or sat. vap.

            HBOSTR = coolutil.getProp(P=PA,
                                      T=TAI, X=0)  # j/kg

            DHBMAX = HBI - HBOSTR

            # DETERMINE THE HEAT TRANSFER FOR THE GUESSED INLET STATE HBI
            QMAX = min(DHAMAX, DHBMAX)
            QACT = ETA * QMAX

            # ADJUST THE STREAM B ENTHALPY GUESS
            DELTA = QACT / (HBO - HBI)
            HTOL = abs(1.0 - DELTA)
            HBI2 = HBO - (HBO - HBI) * DELTA

            if HBI2 > 1.1 * HBI:
                HBI2 = 1.1 * HBI

            if HBI2 < 0.9 * HBI:
                HBI2 = 0.9 * HBI

            HBI = HBI2

            ITER += 1

        return [TBI, HBI, QACT]

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    @staticmethod
    def dutfnd(dt, cab, 
               FANE,       # watt
               ICAB,       # Use Cab data 0 or 1
               IRFTYP,     # Refrigeration Type
               # ICYCL,      # Cycle Type
               QFRSH,      # watt
               QFREZ,      # watt
               FROSTF,     # Fresh Food Door Frost Load - watt
               FROSTZ,     # Freezer Frost Load - watt
               TS3, TS5, T,        # K
               IDFRST              # Manual Defrost 0 or 1
               ):
        
        ICYCL = 1
        N = 1   # one gas
        # CALCULATE DUTY CYCLE AND THE AVERAGE CABINET LOADS

        if ICAB == 0:
            return [0, 0, 0]
            
        # calculate in-wall heat loads
        TENV = cab.TROOM
        TCND = 0.2 * T[14] + 0.4 * T[3] + 0.4 * T[11]

        if TS5 > -300.0:  # Freezer evaporator
            TRFZ = (T[8] + T[9]) / 2.0

            dt.Q_FZ_IN_WALL = dt.UA_FZ * (TENV - TS5)
            dt.Q_ML_IN_WALL = dt.UA_ML * (dt.TS3 - TS5)
            dt.CAPZ_IN_WALL = dt.UA_FZ * (TENV - TRFZ)
            dt.CAPM_IN_WALL = dt.UA_ML * (dt.TS3 - TRFZ)
            dt.Q_FZ_FF = dt.UA_ML * (TS5 - TRFZ)

        else:
            dt.Q_FZ_IN_WALL = 0
            dt.Q_ML_IN_WALL = 0

            dt.CAPZ_IN_WALL = 0
            dt.CAPM_IN_WALL = 0

            dt.Q_FZ_FF = 0

        TRFF = (T[5] + T[7]) / 2.0

        dt.Q_FF_IN_WALL = dt.UA_FF * (TENV - TS3)
        dt.CAPE_IN_WALL = dt.UA_FF * (TENV - TRFF)
        dt.CONDF_IN_WALL = dt.UA_FF_CND * (TCND - TENV)
        dt.CONDZ_IN_WALL = dt.UA_FZ_CND * (TCND - TENV)

        # branch according to the type of refrigerator
        QFF = cab.FFQ
        QFZ = cab.FZQOFF

        DUTYR = 0

        if IRFTYP in [1, 3]:
            if dt.IDFRST == 0:
                QFF = QFF + FROSTF
                QFZ = QFZ + FROSTF

            # 1 Wh = 3.413 Btu    BTU = 1.0548 kj/hr
            # dt.CAPE = QFRSH / 1.0548 \
                # - 3.413 * dt.FANE - 3.413 * dt.DFSTCYC	\
                # - 3.413 * dt.FFCYC - 3.413 * dt.FZCYC	\
                # - dt.CONDF_IN_WALL - dt.CONDZ_IN_WALL

            # both dt.CONDF_IN_WALL - dt.CONDZ_IN_WALL
            # are zero by defalut

            dt.CAPE = QFRSH \
                - FANE \
                - dt.DFSTCYC \
                - dt.FFCYC  \
                - dt.FZCYC \
                - dt.CONDF_IN_WALL \
                - dt.CONDZ_IN_WALL

            dt.DUTYC = (QFF + QFZ) / dt.CAPE

            if dt.DUTYC > 1.0:
                dt.DUTYC = 1.0

            # DUTYR = dt.DUTYC

            # --------------------
            if dt.IDFRST == 0:
                QFZ = QFZ + FROSTF

            # dt.CAPE = QFRSH / 1.0548 - 3.413 * (FANE
                # + dt.DFSTCYC + dt.FZCYC)	\
                # + dt.Q_FF_IN_WALL - dt.CAPE_IN_WALL	\
                # - dt.CONDF_IN_WALL - dt.Q_HXS_FF / 1.0548

            dt.CAPE = QFRSH \
                - (FANE + dt.DFSTCYC + dt.FZCYC) \
                + dt.Q_FF_IN_WALL - dt.CAPE_IN_WALL \
                - dt.CONDF_IN_WALL - dt.Q_HXS_FF

            dt.DUTYE = QFZ / dt.CAPE
            dt.DUTYC = min(dt.DUTYE, 1.0)
            DUTYR = dt.DUTYC

        return [QFF, QFZ, DUTYR]

    @staticmethod
    def mixair(CAP, QFF, QFZ, TFF, TFZ, CFME):
        #  CALCULATE INLET TEMPERATURE TO THE EVAPORATOR
        #   SET UP THE QUADRATIC EQUATION COEFFICIENTS
        
        # CAP watt
        # CFME watt/K
        # TFF, TFZ   K
        # QFF, QFZ
        
        A = 1.08 * CFME * (TFF - TFZ) / CAP     # unitless
        B = - (A + 1.0)                         # unitless
        C = QFF / (QFF + QFZ)                   # unitless

        # Solve the quadratic equation
        X = - B / (2.0 * A) - math.sqrt(B**2 - 4.0 * A * C) / (2.0 * A)
        TIN = X * TFF + (1.0 - X) * TFZ     # K

        # Ayman not used anywhere Data.obj_cdata.FF_AIR = X

        return [TIN, X]
