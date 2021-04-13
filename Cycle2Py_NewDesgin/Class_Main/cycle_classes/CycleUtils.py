# Python import
import math
import sys

# User import
# from cycle_classes.CoolPrp import *
# from cycle_classes.Trace import *
from cycle_classes.CoolPrpUtil import *


class CycleUtils ():
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def enthal(self, objCP, HBUB, HDEW, XSPEC, P):
        # ITERATES TO DETERMINE THE ENTHALPY.                 
        #  THE PRESSURE AND QUALITY ARE INPUTS                      

        # MAKE INITIAL GUESS ASSUMING A LINEAR VARIATION IN ENTHALPY
        #   WITH THE EXIT QUALITY
        #
        self.coolutil = CoolPrpUtil(objCP)
        HGUESS = HBUB + (HDEW - HBUB) * XSPEC
        DELH = 0.0

        ITERH = 0
        XTOL = 1000.0

        while (ITERH < 100 and XTOL >= 0.001):
            HGUESS = HGUESS + DELH
            if (HGUESS < HBUB):
                HGUESS = HBUB * 1.01
            if (HGUESS > HDEW):
                HGUESS = HDEW * 0.99

            # [T, XCALC, XL, XV, VL, V, HL, HV] = self.hpin(HGUESS, P, X)
            T = objCP.Property('T', P=P, H=HGUESS)  # K
            # Python only
            H_liq = objCP.Property('H', P=P, X=0)  # j/kg
            H_gas = objCP.Property('H', P=P, X=1)  # j/kg
            XCALC = (H_gas - H_liq) / H_gas
            # End of Python addition
            if (XCALC < 0.0):
                XCALC = 0.001
            if (XCALC > 1.0):
                XCALC = 0.999
            #
            #        ADJUST ENTHALPY GUESS
            #
            ALPHAH = 1.0 - XCALC / XSPEC
            XTOL = abs(ALPHAH)
            DELH = (HDEW - HBUB) * ALPHAH
            ITERH = ITERH + 1

        H = HGUESS

        return H
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def adjlod(self, dt, MREF, ICYCL, IC, TS3, TS5, FROSTF, FROSTZ, IDFRST):
        # ADJUST THE CABINET LOADS AND SET POINT TEMPERATURES *

        # IC conderser trail number
        # CYCL : Cycle Type (1 to 5)

        # TS3 - K - HTF temperature entering fresh food evaporator
        # TS5 - K - HTF temperature entering freezer evaporator
         
        # FROSTF : watt,Fresh Food Door Frost Load
        # FROSTZ : watt,Freezer Door Sensible Load

        # IDFRST : #, if 0 Manual Defrost, auto 1     

        # BRANCH ON THE VALUE IC.AVE VARIABLES AND INITIALIZE ON THE
        # FIRST CALL AND : WAIT UNTIL THE 4TH CALL TO MAKE ADJUSTMENTS
        #
        IRET = 0  # in Python only

        if IC == 1:
            # SELECT CASE (IC)
            #   CASE [1]#Initialize

            dt.IBLNCE = 0
            IRET = 0

            if(ICYCL != 2):
                IRET = 1

            FFTEMP_S = dt.FFTEMP
            FZTEMP_S = dt.FZTEMP
            
            FFQ_S = dt.FFQ
            FZQON_S = dt.FZQON
            FZQOFF_S = dt.FZQOFF
            
            FFLAT_S = dt.FFLAT
            FZLAT_S = dt.FZLAT
            
            FFSEN_S = dt.FFSEN
            FZSEN_S = dt.FZSEN
            
            FFHTQ_S = dt.FFHTQ
            FZHTQ_S = dt.FZHTQ
            
            FROSTF_S = FROSTF
            FROSTZ_S = FROSTZ

            CONDF_S = dt.CONDF
            CONDZ_S = dt.CONDZ

            dt.ATOTE_S = dt.ATOTE
            dt.AREAFZ_S = dt.AREAFZ
            
            UAF_S = dt.UAF
            dt.ATOTE_A = dt.ATOTE
            dt.AREAFZ_A = dt.AREAFZ

            UFF = (dt.FFQ - dt.FFLAT - dt.FFPENA - dt.FFHTQ -
                   FROSTF + dt.QMUL) / (dt.TROOM - dt.FFTEMP)

            FZQ = dt.FZQON
            FZQ_S = FZQ

            UFZ = (FZQ - dt.FZLAT - dt.FZPENA - dt.FZHTQ -
                   FROSTZ - dt.QMUL) / (dt.TROOM - dt.FZTEMP)

            UFF_SEN = FFSEN_S / (dt.TROOM - FFTEMP_S)
            UFZ_SEN = FZSEN_S / (dt.TROOM - FZTEMP_S)

            UCND_F = (dt.CONDF + dt.QMUL) / \
                (dt.TROOM - FFTEMP_S)
            UCND_Z = (dt.CONDZ - dt.QMUL) / \
                (dt.TROOM - FZTEMP_S)

            TS3_S = TS3
            TS5_S = TS5
            
            dt.FFTEMP_A = dt.FFTEMP
            dt.FZTEMP_A = dt.FZTEMP

            DELTS5_OLD = 0

            UA_FZ_S = dt.UA_FZ
            UA_ML_S = dt.UA_ML
            UA_FF_S = dt.UA_FF

        elif IC in [2, 3]:
            pass

        else:
            # CASE DEFAULT
            if (IRET == 1):
                return [TS3, TS5]

            # Determine needed rebalancing of cabinet loads
            FFLOAD = dt.DUTYE * dt.CAPE + dt.DUTYZ * dt.Q_FZ_FF
            FZLOAD = dt.DUTYZ * dt.CAPZ
            
            DELLOD = (FFLOAD * dt.CAPZ - FZLOAD * \
                      dt.CAPE) / (dt.CAPE + dt.CAPZ)

            DUTMAX = max(dt.DUTYE, dt.DUTYZ)
            DUTMIN = min(dt.DUTYE, dt.DUTYZ)
            DUTDIF = DUTMAX - DUTMIN

            dt.IBLNCE = 0
            DUTERR = DUTDIF / DUTMAX

            if (DUTERR <= 0.001):
                return [TS3, TS5]
                
            if (DUTDIF >= 0.025):
                dt.IBLNCE = 1

            if dt.INCTRL == 0:
                # SELECT CASE (dt.INCTRL)
                # CASE (0)#No control
                return [TS3, TS5]

            elif dt.INCTRL == 1:
                # CASE [1]#Evap area ratio
                FFNEW = FFLOAD + DELLOD
                FZNEW = FZLOAD - DELLOD
                
                DAREAF = (FFNEW / FFLOAD - 1.0) * dt.ATOTE
                DAREAZ = (FZNEW / FZLOAD - 1.0) * dt.AREAFZ

                DUTY_AVE = (dt.DUTYE + dt.DUTYZ) / 2.0
                DAREAF = (dt.DUTYE / DUTY_AVE - 1.0) * dt.ATOTE
                DAREAZ = (dt.DUTYZ / DUTY_AVE - 1.0) * dt.AREAFZ

                RATIOF = DAREAF / dt.ATOTE

                if (RATIOF < -0.5):
                    RATIOF = -0.5

                DAREAF = RATIOF * dt.ATOTE * 0.5
                RATIOZ = DAREAZ / dt.AREAFZ

                if (RATIOZ < -0.5):
                    RATIOZ = -0.5

                DAREAZ = RATIOZ * dt.AREAFZ * 0.5

                if (abs(DAREAF) < abs(DAREAZ)):
                    dt.ATOTE = dt.ATOTE + DAREAF
                    dt.AREAFZ = dt.AREAFZ - DAREAF
                    
                else:
                    dt.AREAFZ = dt.AREAFZ + DAREAZ
                    dt.ATOTE = dt.ATOTE - DAREAZ

                dt.UAF = dt.UAF_S * dt.AREAFZ / dt.AREAFZ_S
                dt.ATOTE_A = dt.ATOTE
                dt.AREAFZ_A = dt.AREAFZ

                dt.UA_FZ = dt.UA_FZ_S * dt.AREAFZ / dt.AREAFZ_S
                dt.UA_ML = dt.UA_ML_S * dt.AREAFZ / dt.AREAFZ_S
                dt.UA_FF = dt.UA_FF_S * dt.ATOTE / dt.ATOTE_S

            elif dt.INCTRL == 2:
                # CASE (2)#FF Cabinet temp
                DUTYN = 0.5 * (dt.DUTYE + dt.DUTYZ)
                dt.FFQ = DUTYN * dt.CAPE + DUTYN * dt.Q_FZ_FF + dt.FROSTF_S
                DELTS3 = (dt.FFQ - dt.FFQ_S) / dt.UFF

                # check temp units--- dr. Omar
                # TS3 = TS3_S - DELTS3 / 1.8
                # dt.FFTEMP_A = 1.8 * TS3 - 459.6

                TS3 = dt.TS3_S - DELTS3
                dt.FFTEMP_A = TS3 
                
                dt.FFSEN = dt.UFF_SEN * (dt.TROOM - dt.FFTEMP_A)
                dt.CONDF = dt.UCND_F * (dt.TROOM - dt.FFTEMP_A) - dt.QMUL

            elif dt.INCTRL == 3:
                # CASE (3)#Freezer temp
                DUTYN = 0.25 * dt.DUTYE + 0.75 * dt.DUTYZ
                FZQ = DUTYN * dt.CAPZ
                
                if (IDFRST == 0):
                    FZQ = FZQ - dt.FROSTZ_S

                DELTS5 = 0.3 * (FZQ - dt.FZQ_S) / dt.UFZ + 0.7 * dt.DELTS5_OLD
                DELTS5_OLD = DELTS5

                FZQ = dt.FZQ_S + dt.UFZ * DELTS5
                dt.FZQON = FZQ
                dt.FZQOFF = FZQ

                TS5 = dt.TS5_S - DELTS5 / 1.8
                dt.FZTEMP_A = 1.8 * TS5 - 459.6

                dt.FZSEN = dt.UFZ_SEN * (dt.TROOM - dt.FZTEMP_A)
                dt.CONDZ = dt.UCND_Z * (dt.TROOM - dt.FZTEMP_A) + dt.QMUL

            elif dt.INCTRL in [4, 5]:
                pass

        return [TS3, TS5]
    
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def lowevp(self, dt, objCP, MREF, ICYCL, ICNTRL,
               H, P, T,
               # XQ, XL, XV, not used
               # ,VL, VV
               # HL, not clear its use,
               TS3, TS5,
               DPF, ETHX2
               ):
        # ICNTRL - CONTROL METHOD FOR EVAPORATOR LOAD
        #  FREEZER EVAPORATOR AND LOWER INTERCHANGER
        
        self.coolutil = CoolPrpUtil(objCP)
        NCALL = 0
        SIGMA = 2.0432E-7
        EPS = 0.8

        # SET UP PRESSURES AND QUALITIES
        P[10] = P[6]
        P[9] = P[5]
        P[8] = P[9] + DPF
        #  XQ[10] = 0
        ETHX = ETHX2
        TSAV = T[9]
        
        TBUB = objCP.Property('T', P=P[8], X=0)  # K
        HBUB = objCP.Property('H', P=P[8], X=0)  # j/kg

        if(NCALL == 0):   # Python all times NCALL = 0 !!!
            TDEW = objCP.Property('T', P=P[9], X=1)  # K
            HDEW = objCP.Property('H', P=P[9], X=1)  # j/kg
            
            # dt.CREF watt.K
            dt.CREF = MREF * (HDEW - HBUB) / (TDEW - TBUB + 0.001)/3600
            if(dt.CREF <= 0.1):
                dt.CREF = 1000000.0  # 5/9/94

            T[10] = TS5
            NCALL = 1

        # set flag for possible solution and handle single evap case

        IFREZ2 = 1
        if(TBUB >= TS5):
            IFREZ2 = 0

        # VL[10] = VL[6]
        if(IFREZ2 == 0):
            H[9] = H[6]
            H[10] = H[6]
            T[10] = T[6]

        if(dt.ITYPE == 1):
            ETHX = 0
        
        # begin iteration for temperature at point 10

        ITER = 1
        # the next statment by Ayman
        VL10 = objCP.Property('V', P=P[6], H=H[6])  # m3/kg
        # 10 CONTINUE
        
        while (True):
            ITER = ITER + 1

            H[10] = objCP.Property('H', T=T[10], V=VL10)  # j/kg
            
            # I think it is T[10]
            T[10] = objCP.Property('T', P=P[10], H=H[10])  # K
            
            H[8] = H[10]
            T[8] = objCP.Property('T', P=P[8], H=H[8])  # K

            # DETERMINE CMIN AND CMAX
            # Dr Omar
            # Ayman CFMF only on Type 2, in case of type 1 CFMF allways 0
            
            if(dt.CFMF <= dt.CREF):   # both watt. K
                CMIN = dt.CFMF
                CMAX = dt.CREF
            else:
                CMIN = dt.CREF
                CMAX = dt.CFMF

            CAPRAT = CMIN / CMAX
            if(CMIN <= 0.0):
                CMIN = 0.001

            FNTU = dt.UAF / CMIN    # UAF Ayman check input list
            if(FNTU < 0.0):
                FNTU = 0.0

            #          CALCULATE EFFECTIVENESS
            #
            dt.UAFZ = dt.UAF
            if(IFREZ2 == 1):
                if dt.IFREZ == 0:
                    TAVE = (T[8] + T[9]) / 2.0

                    if(T[9] < -1000.0):
                        TAVE = T[8]  # Jan 20, 1993
                    if(TAVE > TS5):
                        TAVE = TS5 - 1.0

                    QMAX = 0.90 * MREF * (H(7) - H[6]) / 3600  # 5/9/94
                    HRAD = SIGMA * (TAVE + TS5) * (TAVE**2 + TS5**2) * EPS
                    DELTAT = TS5 - TAVE

                    if(DELTAT <= 0.0):
                        DELTAT = 0.0001

                    # Dr. Omar Units
                    # DELTA = DELTAT * 1.8
                    # DELTA in K
                    DELTA = DELTAT 
                    
                    TBAR = 0.67 * TAVE + 0.33 * TS5
                    A_NAT = 0.239 + 3.34E-04 * (273.0 - TBAR)
                    HNAT = A_NAT * (DELTA**0.33) * 20.44

                    #  MAKE APPROXIMATE CORRECTIONS FOR VIEW FACTORS AND ASSUMED
                    #    ORIENTATION OF THE EVAPORATOR PANELS.
                    #
                    #  HRAD = (1.0 - FRACT_FZ)*HRAD
                    #  HNAT = 0.5*HNAT
                    
                    # Dr. Omar
                    UAIR = HRAD + HNAT
                    if(dt.IWALL_FZ == 1):
                        UAIR = 1.0 / (1.0 / UAIR + 0.1389 / 20.44)

                    QFREZ = dt.UAF * UAIR * DELTAT
                    dt.UAFZ = dt.UAF * UAIR

                    # Dr Omar Temp Unit
                    # TENV = (TROOM + 459.6) / 1.8
                    TENV = dt.TROOM
                    
                    # Dr Omar Heat Unit
                    # QFREZ = QFREZ + 1.8 * UA_FZ * (TENV - TAVE) * 1.0548 \
                    # + 1.8 * UA_ML * (TS3 - TAVE) * 1.0548 + Q_HXS_FZ
                    
                    # UA_FZ Watt/K,  1.0548 btu to j (1 BTU = 1.0548 J)
                    QFREZ = QFREZ + dt.UA_FZ * (TENV - TAVE)  \
                        + dt.UA_ML * (TS3 - TAVE) + dt.Q_HXS_FZ  # watt

                    if(QFREZ > QMAX):
                        QFREZ = QMAX

                elif dt.IFREZ == 1:
                    EXFR = self.efcross(CAPRAT, FNTU)

                    QFREZ = EXFR * CMIN * (TS5 - T[8])
                    dt.ETAF = EXFR

                elif dt.IFREZ == 2:
                    XX = 1.0 - CAPRAT
                    XXX = math.exp(-FNTU * XX)
                    EXFR = (1.0 - XXX) / (1.0 - CAPRAT * XXX)
                    QFREZ = EXFR * CMIN * (TS5 - T[8])
                    dt.ETAF = EXFR

                TS6 = TS5 - QFREZ / dt.CFMF

                if(dt.IFREZ == 0):
                    TS6 = 0.9 * TAVE + 0.1 * TS5
            else:
                QFREZ = 0.0
                TS6 = TS5

            # UPDATE ENTHALPY ACROSS EVAPORATOR

            H[9] = H[8] + QFREZ / MREF / 3600   # MREF kg/hr *3600
            T[9] = objCP.Property('T', P=P[9], H=H[9])  # K
            
            if(IFREZ2 == 0):
                break  # GO TO 20
  
            #          GET NEW GUESS FOR TEMPERATURE AT POINT 10
     
            TOLD = T[10]
            if(ICYCL != 2 or ICNTRL != 2):
                TNEW = T[6] - ETHX * (T[6] - T[9])
            else:
                TD = objCP.Property('T', X=1, P=P[9])  # K
                if(TD > T[6]):
                    TNEW = T[6] - ETHX * (T[6] - T[9])
                else:
                    # to be checked later no VL[10]

                    # Dr. Omar to approve
                    # Ayman modification, in case DTSUPI = 0
                    # the given point came to wet area.
                    # check if in wet area, return sat. liquid or sat. vap.
                    # HHIGH = self.objCP.Property('H', V=VL[10], T=T[9])  # j/kg
                    # HLOW = self.objCP.Property('H', X=0, T=T[6])  # j/kg

                    HHIGH = self.coolutil.getProp(prp='H', V=VL10,   # self.VL[10]
                                                  T=T[9], X=0)    # j/kg

                    HLOW = self.coolutil.getProp(prp='H', P=dt.P[1],
                                                 T=T[6], X=1)   # j/kg

                    DH = min((HLOW - H[9]), (H[6] - HHIGH))
                    H[10] = H[6] - DH
                    T[10] = objCP.Property('T', P=P[10], H=H[10])   # K
            
            # correct guess if necessary and calculate error
           
            if(T[9] > T[6]):
                TNEW = T[10] - 0.9 * ERROR
                T[10] = TNEW
            
            T[10] = TNEW
            ERROR = TSAV - T[9]
            TSAV = T[9]

            if(abs(ERROR) < dt.TOL_FRZ):
                break  # GO TO 20
            
            # j/hr K      = kg/hr . # watt. K
            dt.CREF = MREF / 3600 * abs((H[9] - H[8]) / (T[9] - T[8] + 0.0001))
            # Dr omar units
 
            if(dt.CREF <= 0.1):
                dt.CREF = 1000000.0  # /5/9/94

            ITER = ITER + 1

            if (abs(TOLD - TNEW) > 2.0):  # 5/9/94
                if (TOLD > TNEW):
                    TNEW = TOLD - 2.0  # 5/9/94
                if (TOLD < TNEW):
                    TNEW = TOLD + 2.0  # 5/9/94

            if(ITER > 2):
                T[10] = 0.5 * (TOLD + TNEW)  # 5/9/94

            if(ITER > 10):
                break  # GO TO 20
        # GO TO 10
        # END OF ITERATION.  CALCULATE NEEDED PARAMETERS
        # 20 CONTINUE

        H[5] = H[9] + (H[6] - H[10])        
        T[5] = objCP.Property('T', P=P[5], H=H[5])  # K
        
        T[10] = objCP.Property('T', P=P[10], H=H[10])  # K

        # return [H, P, X, T, XQ, XL, XV, VL, VV, HL, HV, TS6, QFREZ]
        return [H, P, T, TS6, QFREZ]

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def efcross(self, CRAT, NTU):
        # CALCULATES THE HEAT TRANSFER EFFECTIVENESS FOR A CROSS     *
        # FLOW HEAT EXCHANGER WITH BOTH FLUIDS UNMIXED               *

        A = [	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
              [0.0, 2.394292, -1.19402, -1.45067, 1.938453, -0.81305, 0.118651],
              [0.0, 2.410798, -2.23391, 0.8259, 0.051006, -0.11891, 0.02336],
              [0.0, 2.399687, -2.96882, 2.36708, -1.23009, 0.373338, -0.04886],
              [0.0, 2.359642, -3.3765, 3.04862, -1.63421, 0.468741, -0.05492],
              ]

        #  DATA (A(I,1),I=1,4)/2.394292,2.410798,2.399687,2.359642/
        #  DATA (A(I,2),I=1,4)/-1.19402,-2.23391,-2.96882,-3.37650/
        #  DATA (A(I,3),I=1,4)/-1.45067,0.825900,2.367080,3.04862/

        #  DATA (A(I,4),I=1,4)/1.938453,0.051006,-1.23009,-1.63421/
        #  DATA (A(I,5),I=1,4)/-0.81305,-0.11891,0.373338,0.468741/
        #  DATA (A(I,6),I=1,4)/0.118651,0.023360,-0.04886,-0.05492/
        #
        #          FIND POSITION IN ARRAY BASED ON THE CAPACITY RATIO
        #          OF THE TWO STREAMS
        
        if 0.00 <= CRAT <= 0.25:
            I = 1
        
        if 0.25 < CRAT <= 0.50:
            I = 2
        
        if 0.50 < CRAT <= 0.75:
            I = 3
        
        if 0.75 < CRAT <= 1.00:
            I = 4
        
        if(NTU <= 0.0):
            NTU = 0.0

        BETA = math.log10(NTU + 1.0)
        EFFA = 0.0
        EFFB = 0.0

        for J in range(1, 6 + 1):  # DO WHILE (J  <=  6)
            EX = 1.0 * J
            if (I == 1):
                EFFA = 1.0 - math.exp(-NTU)
            else:
                EFFA = EFFA + A[I - 1][J] * BETA**EX
            
            EFFB = EFFB + A[I][J] * BETA**EX

        FRAC = (CRAT - (I - 1) * 0.25) / (I * 0.25 - (I - 1) * 0.25)
        EFFECT = EFFA + FRAC * (EFFB - EFFA)
        
        if (EFFECT > 1.0):
            EFFECT = 1.0
        return EFFECT

    # -----------------------------------------------------------
    # job interchanger for subcooling condenser liquid
    #     used when the inlet states of both streams specified
    # -----------------------------------------------------------
    def inter1(self, objCP, T4, P4, H4, T7, P7, H7, ETHX1):
        # objCP cool prob object
        # ETHX1 - effectiveness of high temp interchanger
        # T temp in KEY
        # H Enthalpy j/kg
        # 4 - condenser outlet
        # 7 - outlet from fresh food evaporator
        
        #     ******************************************************************
        #     *    INTERCHANGER FOR SUBCOOLING CONDENSER LIQUID                *
        #     *    USED WHEN THE INLET STATES OF BOTH STREAMS SPECIFIED        *
        #     ******************************************************************
        #
        #     STATEPOINTS:  4 = CONDENSER OUTLET,
        #                   6 = LIQUID OUTLET FROM INTERCHANGER
        #                   7 = OUTLET FROM FRESH FOOD EVAPORATOR
        #                  13 = LOW PRESSURE SIDE OUTLET FROM INTERCHANGER

        # Dr. Omar to approve
        # Ayman modification, in case DTSUPI = 0
        # the given point came to wet area.
        # check if in wet area, return sat. liquid or sat. vap.

        # H6STAR = objCP.Property('H', P=P7, T=T7)  # j/kg
        # H13STR = objCP.Property('H', P=P4, T=T4)  # j/kg
        H6STAR = self.coolutil.getProp(prp='H', P=P7,
                                       T=T7, X=0)  # j/kg
        H13STR = self.coolutil.getProp(prp='H', P=P4,
                                       T=T4, X=1)  # j/kg
        #
        #          FIND THE MAXIMUM AND ACTUAL HEAT TRANSFER
        #
        # DELH1 = H4 - H6STAR
        # DELH2 = H13STR - H7
        # QBEST = min(H4 - H6STAR, H13STR - H7)
        # QACT = ETHX1 * QBEST
        # return QACT
        
        return ETHX1 * min(H4 - H6STAR, H13STR - H7)

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def inter2(self, objCP, PA, TAI, HAI, VAI, PB, HBO, TDEW, HDEW, VDEW, ETA):
        # iterates to solve for interchanger heat transfer knowing    
        # the inlet state of one stream and outlet state of the       
        # other for a counterflow heat exchanger.                     
        # equal mass flow rates of the same fluid                     

        LCONV = False

        # KNOWN: INLET STATE OF STREAM A
        #        OUTLET STATE OF STREAM B

        # GUESS THE INLET STATE FOR STREAM B
        
        HBI = HDEW - 5.0
        ITER = 0
        HTOL = 1000.0
        
        while (ITER <= 100 and HTOL > 0.001):
            # [TBI, XQBI, XL, XV, VL, VV, HL, HV] = self.hpin(HBI, PB, X)
            TBI = objCP.Property('T', H=HBI, P=PB)  # K
            
            # DETERMINE EXIT STATE OF STREAM A if AT TBI
            
            # Dr. Omar to approve
            # Ayman modification, in case DTSUPI = 0
            # the given point came to wet area.
            # check if in wet area, return sat. liquid or sat. vap.
            # HAOSTR = self.objCP.Property('H', X=0, T=TBI)  # j/kg
            
            HAOSTR = self.coolutil.getProp(prp='H', P=PA,
                                           T=TBI, X=0)  # j/kg
            DHAMAX = HAI - HAOSTR

            # DETERMINE EXIT STATE OF STREAM B if AT TAI
            # Dr. Omar to approve
            # Ayman modification, in case DTSUPI = 0
            # the given point came to wet area.
            # check if in wet area, return sat. liquid or sat. vap.
            
            # HBOSTR = self.objCP.Property('H', X=0, T=TAI)  # j/kg
            HBOSTR = self.coolutil.getProp(prp='H', P=PA,
                                           T=TAI, X=0)  # j/kg
                                         
            DHBMAX = HBI - HBOSTR

            # DETERMINE THE HEAT TRANSFER FOR THE GUESSED INLET STATE HBI
            QMAX = min(DHAMAX, DHBMAX)
            QACT = ETA * QMAX
            
            # ADJUST THE STREAM B ENTHALPY GUESS
            DELTA = QACT / (HBO - HBI)
            HTOL = abs(1.0 - DELTA)
            HBI2 = HBO - (HBO - HBI) * DELTA

            if(HBI2 > 1.1 * HBI):
                HBI2 = 1.1 * HBI
                
            if(HBI2 < 0.9 * HBI):
                HBI2 = 0.9 * HBI
                
            HBI = HBI2

            ITER = ITER + 1

        return [TBI, HBI, QACT]
    
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def dutfnd(self, dt, ICAB,
               IRFTYP, ICYCL, N,
               QFRSH, QFREZ,
               FROSTF, FROSTZ,
               TS3, TS5, T, IDFRST
               ):

        # CALCULATE DUTY CYCLE AND THE AVERAGE CABINET LOADS

        if (ICAB == 0):
            return [0, 0, 0]
        #
        # CALCULATE IN-WALL HEAT LOADS
        #
        # TENV = (dt.TROOM + 459.6) / 1.8
        TENV = dt.TROOM 
        TCND = 0.2 * T[14] + 0.4 * T[3] + 0.4 * T[11]

        if (TS5 > -300.0):  # Freezer evaporator
            TRFZ = (T[8] + T[9]) / 2.0
            
            # dt.Q_FZ_IN_WALL = 1.8 * dt.UA_FZ * (TENV - TS5)
            # dt.Q_ML_IN_WALL = 1.8 * dt.UA_ML * (dt.TS3 - TS5)
            # dt.CAPZ_IN_WALL = 1.8 * dt.UA_FZ * (TENV - TRFZ)
            # dt.CAPM_IN_WALL = 1.8 * dt.UA_ML * (dt.TS3 - TRFZ)
            # dt.Q_FZ_FF = 1.8 * dt.UA_ML * (TS5 - TRFZ)
            
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
        # End if

        TRFF = (T[5] + T[7]) / 2.0
        
        # dt.Q_FF_IN_WALL = 1.8 * dt.UA_FF * (TENV - dt.TS3)
        # dt.CAPE_IN_WALL = 1.8 * dt.UA_FF * (TENV - TRFF)
        # dt.CONDF_IN_WALL = 1.8 * dt.UA_FF_CND * (TCND - TENV)
        # dt.CONDZ_IN_WALL = 1.8 *  dt.UA_FZ_CND * (TCND - TENV)

        dt.Q_FF_IN_WALL = dt.UA_FF * (TENV - dt.TS3)
        dt.CAPE_IN_WALL = dt.UA_FF * (TENV - TRFF)
        dt.CONDF_IN_WALL = dt.UA_FF_CND * (TCND - TENV)
        dt.CONDZ_IN_WALL = dt.UA_FZ_CND * (TCND - TENV)
        
        #
        # BRANCH ACCORDING TO THE TYPE OF REFRIGERATOR
        #
        QFF = dt.FFQ
        QFZ = dt.FZQOFF
        #

        if IRFTYP in [1, 3]:
            if (ICYCL == 1):
                if (dt.IDFRST == 0):
                    QFF = QFF + dt.FROSTF
                    QFZ = QFZ + dt.FROSTF

                # 1 Wh = 3.413 Btu    BTU = 1.0548 kj/hr
                # dt.CAPE = QFRSH / 1.0548 \
                    # - 3.413 * dt.FANE - 3.413 * dt.DFSTCYC	\
                    # - 3.413 * dt.FFCYC - 3.413 * dt.FZCYC	\
                    # - dt.CONDF_IN_WALL - dt.CONDZ_IN_WALL

                # both dt.CONDF_IN_WALL - dt.CONDZ_IN_WALL
                # are zero by defalut
                
                dt.CAPE = QFRSH \
                    - dt.FANE \
                    - dt.DFSTCYC \
                    - dt.FFCYC  \
                    - dt.FZCYC \
                    - dt.CONDF_IN_WALL \
                    - dt.CONDZ_IN_WALL
                    
                dt.DUTYC = (QFF + QFZ) / dt.CAPE
                
                if (dt.DUTYC > 1.0):
                    dt.DUTYC = 1.0
                DUTYR = dt.DUTYC

            if (ICYCL == 2):
                QFF = QFF - dt.FROSTF
                if (dt.IDFRST == 0):
                    QFZ = QFZ + dt.FROSTF
                
                # Dr. Omar Unit
                # dt.CAPZ = QFREZ / 1.0548 - 3.413 * dt.FANZ \
                    # - 3.413 * dt.DFSTCYC	\
                    # + dt.Q_FZ_IN_WALL + dt.Q_ML_IN_WALL	\
                    # - dt.CAPZ_IN_WALL - dt.CAPM_IN_WALL	\
                    # - dt.CONDZ_IN_WALL - 3.413 * dt.FZCYC	\
                    # - dt.Q_HXS_FZ / 1.0548
                    
                dt.CAPZ = QFREZ \
                    - dt.FANZ \
                    - dt.DFSTCYC \
                    + dt.Q_FZ_IN_WALL \
                    + dt.Q_ML_IN_WALL \
                    - dt.CAPZ_IN_WALL \
                    - dt.CAPM_IN_WALL \
                    - dt.CONDZ_IN_WALL \
                    - dt.FZCYC	\
                    - dt.Q_HXS_FZ 

                if (dt.CAPZ <= 0.0):
                    # self.showError("Incorrect Solution, Check Mass Flow")
                    # self.showError("Solution being Terminated")
                    sys.exit("Incorrect Solution, Check Mass Flow 100")  # STOP ' '

                dt.DUTYZ = QFZ / dt.CAPZ

                # dt.CAPE = QFRSH / 1.0548 - 3.413 * dt.FANE \
                #    # - 3.413 * dt.FFCYC	\
                #    # + dt.Q_FF_IN_WALL - dt.CAPE_IN_WALL	\
                #    # - dt.CONDF_IN_WALL + dt.Q_FZ_FF	\
                #    # - dt.Q_HXS_FF / 1.0548

                dt.CAPE = QFRSH \
                    - dt.FANE \
                    - dt.FFCYC \
                    + dt.Q_FF_IN_WALL \
                    - dt.CAPE_IN_WALL \
                    - dt.CONDF_IN_WALL \
                    + dt.Q_FZ_FF \
                    - dt.Q_HXS_FF

                dt.DUTYE = QFF / dt.CAPE

                dt.DUTYC = min(
                    dt.DUTYE, dt.DUTYZ)

                if (dt.DUTYC > 1.0):
                    dt.DUTYC = 1.0
                    
                DUTYR = max(dt.DUTYE, dt.DUTYZ)

                if (DUTYR > 1.0):
                    DUTYR = 1.0

            if (ICYCL == 3):
                if (N == 1):
                    if (dt.IDFRST == 0):
                        QFZ = QFZ + dt.FROSTF
                   
                    # dt.CAPZ = QFRSH / 1.0548 - 3.413 * dt.FANE - \
                        # 3.413 * (dt.DFSTCYC + dt.FZCYC)

                    dt.CAPZ = QFRSH - dt.FANE - (dt.DFSTCYC + dt.FZCYC)
                        
                    dt.DUTYZ = QFZ / dt.CAPZ

                    dt.DUTYC = min(dt.DUTYZ, 1.0)
                    dt.DUTYZ = dt.DUTYC

                else:
                    # dt.CAPE = QFRSH / 1.0548 - 3.413 * \
                    #    # (dt.FANE + dt.FFCYC)
                    #    # + dt.Q_FF_IN_WALL - dt.CAPE_IN_WALL

                    dt.CAPE = QFRSH \
                        - (dt.FANE + dt.FFCYC) \
                        + dt.Q_FF_IN_WALL - dt.CAPE_IN_WALL
                        
                    QFF = QFF - dt.FROSTF
                    dt.DUTYE = QFF / dt.CAPE
                    dt.DUTYC = min(dt.DUTYE, 1.0)
                    dt.DUTYE = dt.DUTYC

                DUTYR = dt.DUTYC

            else:
                if (dt.IDFRST == 0):
                    QFZ = QFZ + dt.FROSTF
                
                # dt.CAPE = QFRSH / 1.0548 - 3.413 * (dt.FANE 
                    # + dt.DFSTCYC + dt.FZCYC)	\
                    # + dt.Q_FF_IN_WALL - dt.CAPE_IN_WALL	\
                    # - dt.CONDF_IN_WALL - dt.Q_HXS_FF / 1.0548

                dt.CAPE = QFRSH \
                    - (dt.FANE + dt.DFSTCYC + dt.FZCYC) \
                    + dt.Q_FF_IN_WALL - dt.CAPE_IN_WALL \
                    - dt.CONDF_IN_WALL - dt.Q_HXS_FF 

                dt.DUTYE = QFZ / dt.CAPE
                dt.DUTYC = min(dt.DUTYE, 1.0)
                DUTYR = dt.DUTYC

        return [QFF, QFZ, DUTYR]

    def mixair(self, CAP, QFF, QFZ, TFF, TFZ, CFME):
        #     *     CALCULATE INLET TEMPERATURE TO THE EVAPORATOR            
        #          SET UP THE QUADRATIC EQUATION COEFFICIENTS
        #

        A = 1.08 * CFME * (TFF - TFZ) / CAP
        B = - (A + 1.0)
        C = QFF / (QFF + QFZ)

        # Solve the quadratic equation
        X = - B / (2.0 * A) - math.sqrt(B**2 - 4.0 * A * C) / (2.0 * A)
        TIN = X * TFF + (1.0 - X) * TFZ
        
        # Ayman not used anywhere Data.obj_cdata.FF_AIR = X
        
        return [TIN, X]
