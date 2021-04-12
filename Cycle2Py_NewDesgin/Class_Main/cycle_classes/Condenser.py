# Python import
from abc import ABC, abstractmethod

# User import
from .exf4Cond_Evap import exf4Cond_Evap

from .ErrorException import ErrorException
from cycle_classes.Trace import *

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Create Condenser object based on ICOND
#    input: ICOND = 0  Natural Convection
#         : ICOND = 1  Cross-Flow
#         : ICOND = 2  Counter-Flow
#         : objCP = CoolProp object
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Condenser:
    def getObject (self, ICOND, objCP):
        if (ICOND == 0):  # Natural Convection
            objCondType = CondCool_CNat(ICOND, objCP)

        elif (ICOND == 1):  # Cross-Flow
            objCondType = CondCool_CCross(ICOND, objCP)

        elif (ICOND == 2):  # Counter-Flow
            objCondType = CondCool_CCount(ICOND, objCP)

        else:
            objCondType = None
            raise ErrorException('ICOND value error', 'Cond1000')
        return objCondType


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from condenser cooling method (Natural, Cross, Counter-flow)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class CondCool_Abstract (ABC, exf4Cond_Evap):
    DATA_PARA = 0 # if set to 1 parameters was set
    
    def __init__(self, ICOND, objCP):
        self.ICOND = ICOND
        self.objCP = objCP
        self.trace = Trace()

    def setParamters (self, ATOTC, CFMC, TS1, TS3, TS5, DTSUBC, N_COND,
                      USCC, UTPC, UDSC,
                      UA_FF_CND, UA_FZ_CND ,UA_FF_HXS  ,UA_FZ_HXS):
        # ATOTC m2 Total Heat Transfer Surface Area
        
        # UA_FF_CND W/K or j/sec-K , Cond: A/R In Fresh Food Section 
        #    (Or Cabinet Walls) old unit sec-F/Btu(th)
        
        # UA_FZ_CND W/K or j/sec-K, Cond: A/R In Freezer Section Walls
        #    (If Separate Section) old unit sec-F/Btu(th)
        
        # UA_FF_HXS W/K or j/sec-K, Both: A/R In Fresh Food Section 
        #    (Or Cabinet Walls) old unit sec-F/Btu(th)
        
        # UA_FZ_HXS W/K or j/sec-K , Both: A/R In Freezer Section Walls
        #    (If Separate Section)  old unit sec-F/Btu(th)
        
        # CFMC  unit Watt/K
        # by Ayman, Dr Omar approved
        # see comments in CycleSolved 
        # and using PV=RT to get air dencity a\@ given temperature
        # see details in CycleSolver.py
        # j/kg/sec Air load
        
        # DTSUBC Refrigerant Exit Subcooling, Deg C 
        #  used only for 1-CCOUNT and 2-ccross, (NOT useD IN 0-cnat)

        # N_COND
        # 1	Two Door Topmount Refrigerator/Freezer.
        # 2	Two Door Bottommount Befrigerator/Freezer.
        # 3	Side Byside Refrigerator/Freezer.
        # 4	Chest Freezer.
        # 5	Upright Freezer.
        # 6	Onedoor Refrigerator.
        # 7	Onedoor Refrigerator/Freezer

        # TS1 heat transfer fluid (htf) temperature entering condenser
        # TS3 htf temperature entering fresh food evaporator
        # TS5 htf temperature entering freezer evaporator

        # UDSC Desuperheating Heat Transfer Conductance, watt/m2/C
        # USCC Subcooling Heat Transfer Conductance, watt/m2/C  
        # UTPC Two-Phase Heat Transfer Conductance, watt/m2/C

        self.ATOTC = ATOTC
        self.UA_FF_CND = UA_FF_CND # W/K
        self.UA_FZ_CND = UA_FZ_CND # W/K
        self.UA_FF_HXS = UA_FF_HXS # W/K
        self.UA_FZ_HXS = UA_FZ_HXS # W/K
        self.CFMC = CFMC
        self.DTSUBC = DTSUBC  
        self.N_COND = N_COND  
        
        self.TS1 = TS1
        self.TS3 = TS3
        self.TS5 = TS5

        self.UDSC = UDSC
        self.USCC = USCC
        self.UTPC = UTPC
        
        self.Q_CND_FF = None
        self.Q_CND_FZ = None
        self.Q_HXS_FF = None
        self.Q_HXS_FZ = None
        self.UACOND = None

        
        CondCool_Abstract.DATA_PARA = 1

    def getExtarOutputs (self):
        # Q_CND_FF Condenser Heat Fresh Food, 
        # Q_CND_FZ Condenser Heat Freezer,    
        # Q_HXS_FF Heat Exchanger Fresh Food  
        # Q_HXS_FZ Heat Exchanger Freezer     
        # UACOND Condenser UA                    
        # UDSC Desuperheating Heat Transfer Conductance, watt/m2/C 
        # USCC Subcooling Heat Transfer Conductance, watt/m2/C  
        # UTPC Two-Phase Heat Transfer Conductance, Watt/M2-C
        
        return [self.Q_CND_FF, self.Q_CND_FZ,
                self.Q_HXS_FF, self.Q_HXS_FZ,
                self.UACOND, self.UDSC,
                self.USCC, self.UTPC
                ]
    
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def cond(self, T4, H4, H14, TC, JC, QCONDS, QCONDC, QSCC,
             MROLD, MREF, UACOND):
        #     *****************************************************************
        #     *    CALCULATE CONDENSER EXIT TEMPERATURE                       *
        #     *****************************************************************

        TOL_COND = 0.075  # Tolerance for condenser temperature
        TOL_MASS = 0.01  # Tolerance for condenser mass flow
        
        # useless ICNT = 10  # any value but not 0,1,2, Python only to be checked

        ICONC = 0
        if(JC == 1):
            # ICNT = 0
            MROLD = 0.0
            
        # Estimate new value for exit temperature

        QREF = MREF * (H14 - H4) # kg/hr * j/kg = j/hr
        QCOND = QCONDS + QCONDC + QSCC # j/hr
        EPS = QREF - QCOND
        DELT = EPS / UACOND

        if(DELT > 5.0):
            DELT = 5.0
            
        if(DELT < -5.0):
            DELT = -5.0

        TCOUT = TC[JC] + DELT
        TS2 = self.TS1 + (QCONDS + QCONDC + QSCC) / self.CFMC

        if(self.ICOND == 0):
            TS2 = 0.9 * T4 + 0.1 * self.TS1
            
        if(TCOUT < self.TS1):
            TCOUT = (self.TS1 + TC[JC]) / 2.0

        # modification by Ayman if(ICNT <= 2) :
        if(JC < 2):
            TCNEW = TCOUT
        else: # Dr Omar
            # if((TCOUT > TC[1] and TC[1] > TC[2])
                    # or (TCOUT < TC[1] and TC[1] < TC[2])):

            if((TCOUT > TC[1] and TC[1] > TC[2])
                    or (TCOUT < TC[2] and TC[1] > TC[2]) ):
                    
                TCNEW = 0.5 * (TC[1] + TC[2])
            else:
                TCNEW = TCOUT

            if(TCNEW < self.TS1):
                TCNEW = (self.TS1 + TC[JC]) / 2.0

            # ayman TC[1] = TC[2]

        # Check convergence
        # ERRORT = abs(TCNEW - TC[JC])
        ERRORT = abs(TCNEW - TC[1])
        ERRORM = abs(MREF - MROLD) / MREF

        # ==============this block modified by Ayman
        # if(ERRORT < TOL_COND and ERRORM <= TOL_MASS):
            # ICONC = 1    
        if(ERRORT < TOL_COND and ERRORM <= TOL_MASS):
            ICONC = 1
        else:
            ICONC = 0
        # ======================End of Ayman Modification

        JC = 2
        # ICNT = ICNT + 1  # useless
        # TC[JC] = TCNEW
        TC[2] = TC[1]
        TC[1] = TCNEW
        # modification by Ayman - MROLD = MREF to be moved out of class
        # MROLD = MREF

        return [TS2, TC, JC, ICONC]

class CondCool_CNat (CondCool_Abstract):  # Natural Convection= 0
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def cond_balance(self, T14, H14, T3, H3, MREF,
                   TBUB, HBUB, CPRLIQ,
                   T5, T8, T9, T12):

        lstRes = self.cnat(TS1=self.TS1, 
                           TS3=self.TS3,
                           TS5=self.TS5,
                           T14=T14, H14=H14, T3=T3, H3=H3, MREF=MREF,
                           TBUB=TBUB, HBUB=HBUB, CPRLIQ=CPRLIQ,
                           T5=T5, T8=T8, T9=T9, T12=T12)
        
        # Sample output QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB
        dicRes = {'QDSC':lstRes[0]  # Q desuperheating
                 ,'QTPC':lstRes[1]  # Q two phase
                 ,'QSCC':lstRes[2]  # Q subcooling
                 ,'QTOTC':lstRes[3]  # Q total condenser
                 ,'FSUP':lstRes[4]  # Fraction desuperheating
                 ,'FSUB':lstRes[5]  #  Fraction subcooling  
                 }
                 
        return dicRes

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def cnat(self, TS1, TS3, TS5,
                   T14, H14, T3, H3, MREF,
                   TBUB, HBUB, CPRLIQ, T5, T8, T9, T12):
        # ---------Input
        # TS1 heat transfer fluid (htf) temperature entering condenser
        # TS3 htf temperature entering fresh food evaporator
        # TS5 htf temperature entering freezer evaporator

        # T14 Temperature k CONDENSER INLET
        # H14 Enthalpy j/kg CONDENSER INLET
        # T3 Temperature CONDENSER DEW POINT
        # H3 Enthalpy j/kg CONDENSER DEW POINT

        # TBUB Condenser bubble point Temp
        # HBUB J/kg Condenser bubble point Enthalpy
        # CPRLIQ Condenser bubble point CP j/kg/K

        # T5 - INLET TO FRESH FOOD EVAPORATOR
        # T8 - INLET TO FREEZER EVAPORATOR
        # T9 - OUTLET FROM FREEZER EVAPORATOR
        # T12 - FRESH FOOD EVAPORATOR DEW POINT

        # ---------Output
        # QDSC - Q desuperheating
        # QTPC - Q two phase
        # QSCC - Q subcooling
        # QTOTC - Q total condenser

        # FSUP  - Fraction desuperheating
        # FSUB  - Fraction subcooling

        # calculates the condenser heat transfer
        #  for a natural convection condenser

        # Segregate the desuperheating region for the heat transfer coefficient
        #  calculation since the temperature dif ference is high
        SIGMA = 2.04326E-7

        # Set up the HDEW and TDEW parameters.
        # account for a wet gas entering the condenser

        if (H3 < H14):
            TDEW = T3
            HDEW = H3
        else:
            TDEW = T14
            HDEW = H14

        # calculate the radiation heat transfer, heat transfer coefficient
        #  using small delta T approximation (black body)
        #  use the arithmetic average of temperature to evaluate h
        #  radiation in the desuperheating region
        #
        TAVE = (T14 + TDEW) / 2.0
        T1 = TAVE * 1.8 - 459.6

        # HRAD = 4. * SIGMA * TAVE**3  # useless
        HRAD = SIGMA * (TAVE + TS1) * (TAVE**2 + TS1**2)

        # calculate the natural convection heat transfer coefficient
        DELTAT = TAVE - TS1
        if (DELTAT < 0.0):
            DELTAT = 0.0001

        DELTA = DELTAT * 1.8
        HNAT = 0.19 * DELTA**0.33 * 20.44

        #  calculate combined air-side heat transfer coefficient
        UAIR = HRAD + HNAT
        
        self.trace.dr_omar("Unit Adjusted")  # Dr Omar to approve
        # Ayman UAIR was kW/m2 K
        # by Ayman  kW/m2 K = 0.04892 Btu/ (s ft2 F)
        # U1 = UAIR * 0.04892 
        U1 = UAIR * 1000 # W/m2 K

        # calculate the heat transfer assuming that the air side
        #  resistance dominates
        # calculate the area necessary to desuperheat the refrigerant

        TCND = 0.2 * T14 + 0.4 * TDEW + 0.4 * TBUB
        
        # Python: 1.8 C-to-F, then 1.0548 btu to j (1 BTU = 1.0548 J)
        # UA_FF_CND, UA_FZ_CND Unit is W/K or j/sec-K
 
        # self.Q_CND_FF = 1.8 * self.UA_FF_CND * (TCND - TS3) * 1.0548
        # self.Q_CND_FZ = 1.8 * self.UA_FZ_CND * (TCND - TS5) * 1.0548

        self.Q_CND_FF = self.UA_FF_CND * (TCND - TS3) # watt
        self.Q_CND_FZ = self.UA_FZ_CND * (TCND - TS5) # watt

        TFZ = 0.5 * (T8 + T9)
        TFF = 0.5 * (T5 + T12)

        # Python: 1.8 C-to-F, then 1.0548 btu to j
        # UA_FF_HXS, UA_FZ_HXS Unit is W/K or j/sec-K
                
        # self.Q_HXS_FF = 1.8 * self.UA_FF_HXS * (TCND - TFF) * 1.0548
        # self.Q_HXS_FZ = 1.8 * self.UA_FZ_HXS * (TCND - TFZ) * 1.0548

        self.Q_HXS_FF = self.UA_FF_HXS * (TCND - TFF) # watt
        self.Q_HXS_FZ = self.UA_FZ_HXS * (TCND - TFZ) # watt

        # Dr Omar to approve
        # if (TS5 < -290.0): # -290K = 94.2611 K
        if (TS5 < 94.2611): # not sure 290 F to K
            self.Q_CND_FZ = 0 # watt

        self.trace.dr_omar("Wet region issue")  # Dr Omar to approve
        # if (TS5 < -290.0):
        if (TS5 < 94.2611): # not sure 290 F to K
            self.Q_HXS_FZ = 0

        Q_IN_WALL = (self.Q_CND_FF + self.Q_CND_FZ) / self.ATOTC \
             + (self.Q_HXS_FF + self.Q_HXS_FZ) / self.ATOTC # W/m2

        QDSNEC = MREF * (H14 - HDEW) # kg/hr . j/kg = j/hr
        # UAIR W/m2 K
        ADSNEC = QDSNEC / (UAIR * DELTAT + Q_IN_WALL) # unit less

        if (ADSNEC > self.ATOTC):
            ADSNEC = self.ATOTC

        QDSC = UAIR * ADSNEC * DELTAT + ADSNEC * Q_IN_WALL # watt
        FSUP = ADSNEC / self.ATOTC

        QTPC = 0.0
        QSCC = 0.0

        QTOTC = QDSC + QTPC + QSCC

        FSUB = 0.0
        self.UDSC = UAIR # W/m2 K
        self.UACOND = self.ATOTC * self.UDSC

        if (FSUP == 1.0):
            return [QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]

        # calculate the heat transfer coefficients for the two-phase and
        # subcooling regions
        TAVE = (TDEW + TBUB) / 2.0
        T2 = TAVE * 1.8 - 459.6

        # HRAD = 4. * SIGMA * TAVE**3 # useless
        HRAD = SIGMA * (TAVE + TS1) * (TAVE**2 + TS1**2)

        # calculate the natural convection heat transfer coefficient
        DELTAT = TAVE - TS1

        if (DELTAT < 0.0):
            DELTAT = 0.0001

        DELTA = DELTAT * 1.8
        HNAT = 0.19 * DELTA**0.33 * 20.44

        # calculate combined air-side heat transfer coefficient
        UAIR = HRAD + HNAT

        self.trace.dr_omar("Wet region issue")  # Dr Omar to approve
        # Ayman UAIR was kW/m2 K
        # by Ayman  kW/m2 K = 0.04892 Btu/ (s ft2 F)
        # U2 = UAIR * 0.04892 
        U2 = UAIR * 1000 # W/m2 K
        
        # calculate the heat transfer necessary to condense the refrigerant
        QTPNEC = MREF * (HDEW - HBUB)

        # calculate the remaining surface area
        ANET = self.ATOTC - ADSNEC

        # calculate the actual heat transfer in the two-phase region
        #      # (W/m2.K)m2 K  + m2 * # W/m2
        QTPC = UAIR * ANET * DELTAT + ANET * Q_IN_WALL # watt

        if (QTPC > QTPNEC):
            QTPC = QTPNEC

        # calculate the area in the two-phase region
        #       watt / ( (W/m2 K)  K +  W/m2)
        ATPC = QTPC / (UAIR * DELTAT + Q_IN_WALL) # m2
        QSCC = 0.0

        QTOTC = QDSC + QTPC + QSCC
        FSUB = 0.0

        self.UTPC = UAIR # W/m2 K
        self.UACOND = self.ATOTC * \
            (FSUP * self.UDSC + (1.0 - FSUP) * self.UTPC)

        if (QTPC < QTPNEC):
            return [QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]

        # calculate the remaining surface area
        ANET = self.ATOTC - (ADSNEC + ATPC)

        # calculate the heat transfer if the refrigerant exits at TS1
        QSCMAX = MREF * CPRLIQ * (TBUB - TS1)

        # calculate the subcooling heat transfer
        #        W/m2 K * m2
        QSCC = UAIR * ANET * DELTAT + ANET * Q_IN_WALL # watt

        if (QSCC > QSCMAX):
            QSCC = QSCMAX

        QTOTC = QDSC + QTPC + QSCC # watt
        FSUB = ANET / self.ATOTC # unit less

        self.USCC = UAIR # W/m2 K
        self.UACOND = self.ATOTC * \
            (FSUP * self.UDSC + FSUB * self.USCC \
                + (1.0 - FSUP - FSUB) * self.UTPC)

        return [QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]


class CondCool_CCross (CondCool_Abstract):  # Cross-Flow= 1
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def cond_balance(self, T14, H14, T3, H3, MREF,
                   TBUB, HBUB, CPRLIQ,
                   PBUB, P4):
                   
        # replace TBUB with T[11]
        # replace PBUB with P[11]
        # replace HBUB with H[11]
        
        # replace TDEW with T[3]
        # replace HDEW with H[3]
        lstRes = self.ccross(MREF=MREF,
                           TS1=self.TS1,
                           T2=T14,
                           H2=H14,
                           TDEW_S=T3,
                           HDEW_S=H3,
                           TBUB_S=TBUB,
                           HBUB_S=HBUB,
                           CPRLIQ=CPRLIQ,
                           PIN=PBUB,
                           POUT=P4,
                           NUM_ZONE=self.N_COND)

        # Sample output QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB
        dicRes = {'QDSC':lstRes[0]  # Q desuperheating
                 ,'QTPC':lstRes[1]  # Q two phase
                 ,'QSCC':lstRes[2]  # Q subcooling
                 ,'QTOTC':lstRes[3]  # Q total condenser
                 ,'FSUP':lstRes[4]  # Fraction desuperheating
                 ,'FSUB':lstRes[5]  #  Fraction subcooling  
                 }
        return dicRes
        
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def ccross(self, MREF,
            TS1, T2, H2, 
            TDEW_S, HDEW_S, TBUB_S, HBUB_S,
            CPRLIQ, PIN, POUT,
            NUM_ZONE):
        #   ******************************************************************
        #   *    SUBROUTINE CCOUNT - CALCULATES THE CONDENSER HEAT EXCHANGE  *
        #   *    FOR COUNTERFLOW HEAT EXCHANGER                              *
        #   ******************************************************************

        AREA_TOL = 0.001

        # INITIALIZE
        TBUB = TBUB_S
        HBUB = HBUB_S

        if(H2 <= HDEW_S):
            HDEW = H2
            TDEW = T2
            ENTERS_WET = True
        else:
            HDEW = HDEW_S
            TDEW = TDEW_S
            ENTERS_WET = False

        HDEW_START = HDEW
        TDEW_START = TDEW

        DELP = (PIN - POUT) / float(NUM_ZONE)
        DELH = (HDEW - HBUB) / float(NUM_ZONE)
        PBUB = POUT

        QDSC = 0.0
        QTPC = 0.0
        QSCC = 0.0

        ASCC = 0
        ATPC = 0
        ADSC = 0

        TAIR = TS1
        CAIR = self.CFMC
        HAVE_NOT_USED_FULL_AREA = True

        # Start off with the subcooling area.
        if(self.DTSUBC > 0.0):
            TCSUB = TBUB - self.DTSUBC
            CRSC = MREF * CPRLIQ

            if(CAIR <= CRSC):
                CMINSC = CAIR
                CMAXSC = CRSC
            else:
                CMINSC = CRSC
                CMAXSC = CAIR

            # is area big enough for subcooling
            QMAX = CMINSC * (TBUB - TAIR)
            QSUB = CRSC * (TBUB - TCSUB)

            EFF_SUB = QSUB / QMAX

            [EFFSCC, DEXDAR] = self.exf(2, 
                self.ATOTC, self.USCC, CMINSC, CMAXSC)

            if(EFFSCC <= EFF_SUB):  # Need more area
                ASCC = self.ATOTC
                HAVE_NOT_USED_FULL_AREA = False
                #  begin iteration process to determine solution for the
                #  subcooled region

                #  initialize variables
            else:
                ASCC = self.ATOTC / 10.0
                LOOKING_FOR_AREA = True

                ICOUNT = 0
                QTOL = 1.0

                while (LOOKING_FOR_AREA):
                    ICOUNT = ICOUNT + 1
                    if(ICOUNT > 100):
                        LOOKING_FOR_AREA = False
                        continue

                    CAIR = (ASCC / self.ATOTC) * self.CFMC
                    if(CAIR <= CRSC):
                        CMINSC = CAIR
                        CMAXSC = CRSC
                    else:
                        CMINSC = CRSC
                        CMAXSC = CAIR

                    QMAX = CMINSC * (TBUB - TAIR)
                    EFF_SUB = QSUB / QMAX

                    [EFFSCC, DEXDAR] = self.exf(2, 
                            ASCC, self.USCC, CMINSC, CMAXSC)
                            
                    ERROR = abs(QTOL)
                    if(ERROR <= AREA_TOL):
                        LOOKING_FOR_AREA = False
                        continue

                    QRAT = EFFSCC * QMAX / QSUB
                    QTOL = 1.0 - QRAT

                    DAREA = ASCC * (1.0 - QRAT)

                    DAREA_MIN = -0.50 * ASCC
                    DAREA_MAX = 0.50 * (self.ATOTC - ASCC)

                    if(DAREA < DAREA_MIN):
                        DAREA = DAREA_MIN
                    if(DAREA > DAREA_MAX):
                        DAREA = DAREA_MAX

                    ASCC = ASCC + DAREA

            QSCC = EFFSCC * CMINSC * (TBUB - TAIR)

        #  continue with two-phase area
        ALEFT = self.ATOTC - ASCC

        for N in range(1, NUM_ZONE + 1):
            PDEW = PBUB + DELP
            HDEW = HBUB + DELH
            # [TDEW, XQ, XL, XV, VL, VV, HL, HV] = self.hpin(HDEW, PDEW, X)
            TDEW = self.objCP.Property('T', H=HDEW, P=PDEW)  # K
            
            if(HAVE_NOT_USED_FULL_AREA):
                CPRTP = (HDEW - HBUB) / abs(TDEW - TBUB + 0.0001)
                CRTP = MREF * CPRTP

                #  determine cmin and cmax in the two-phase region
                CAIR = (ALEFT / self.ATOTC) * self.CFMC
                if(CAIR <= CRTP):
                    CMINTP = CAIR
                    CMAXTP = CRTP
                else:
                    CMINTP = CRTP
                    CMAXTP = CAIR

                #  is area big enough for condensation
                QMAX = CMINTP * (TDEW - TAIR)
                QDUM = MREF * (HDEW - HBUB)

                EFF_TPC = QDUM / QMAX
                [EFFTPC, DEXDAR] = self.exf(2,
                        ALEFT, self.UTPC, CMINTP, CMAXTP)

                if(EFFTPC <= EFF_TPC or ENTERS_WET):  # Need more area
                    ATPC = ATPC + ALEFT
                    HAVE_NOT_USED_FULL_AREA = False

                    #  begin iteration process to determine solution for the
                    #  two phase region

                    #  initialize variables
                else:
                    ADUM = 0.9 * ALEFT
                    LOOKING_FOR_AREA = True

                    ICOUNT = 0
                    QTOL = 1.0
                    while (LOOKING_FOR_AREA):
                        ICOUNT = ICOUNT + 1
                        if(ICOUNT > 100):
                            LOOKING_FOR_AREA = False
                            continue
      
                        CAIR = (ADUM / self.ATOTC) * self.CFMC
                        if(CAIR <= CRTP):
                            CMINTP = CAIR
                            CMAXTP = CRTP
                        else:
                            CMINTP = CRTP
                            CMAXTP = CAIR
      
                        QMAX = CMINTP * (TDEW - TAIR)
                        EFF_TPC = QDUM / QMAX

                        [EFFTPC, DEXDAR] = self.exf(2,
                            ADUM, self.UTPC, CMINTP, CMAXTP)

                        ERROR = abs(QTOL)
                        if(ERROR <= AREA_TOL):
                            LOOKING_FOR_AREA = False
                            continue

                        QRAT = EFFTPC * QMAX / QDUM
                        QTOL = 1.0 - QRAT

                        DAREA = ADUM * (1.0 - QRAT)

                        DAREA_MIN = -0.75 * ADUM
                        DAREA_MAX = 0.50 * (ALEFT - ADUM)

                        if(DAREA < DAREA_MIN):
                            DAREA = DAREA_MIN
                        if(DAREA > DAREA_MAX):
                            DAREA = DAREA_MAX

                        ADUM = ADUM + DAREA
                    ATPC = ATPC + ADUM

                QTPC = QTPC + EFFTPC * CMINTP * (TDEW - TAIR)

            ALEFT = self.ATOTC - ASCC - ATPC
            HBUB = HBUB + DELH
            TBUB = TDEW
            PBUB = PBUB + DELP

        #  continue with desuperheating area
        ALEFT = self.ATOTC - ASCC - ATPC
        if(ALEFT <= 0.0):
            HAVE_NOT_USED_FULL_AREA = False

        HDEW = HDEW_START
        TDEW = TDEW_START

        if(HAVE_NOT_USED_FULL_AREA):
            CPRVAP = (H2 - HDEW) / (T2 - TDEW)
            CRDS = MREF * CPRVAP

            #  determine cmin and cmax in the two-phase region
            CAIR = (ALEFT / self.ATOTC) * self.CFMC

            if(CAIR <= CRDS):
                CMINDS = CAIR
                CMAXDS = CRDS
            else:
                CMINDS = CRDS
                CMAXDS = CAIR

            #  determine the net heat transfer
            [EFFDSC, DEXDAR] = self.exf(2,
                    ALEFT, self.UDSC, CMINDS, CMAXDS)
  
            QDSC = CMINDS * EFFDSC * (T2 - TAIR)

            ADSC = ALEFT
 
        # calculate the fractional subcooling and superheating regions
        FSUP = ADSC / self.ATOTC
        FSUB = ASCC / self.ATOTC

        QTOTC = QSCC + QTPC + QDSC
        self.UACOND = self.ATOTC * \
            (FSUP * self.UDSC + FSUB * self.USCC 
            + (1.0 - FSUP - FSUB) * self.UTPC)
            
        return [QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]


class CondCool_CCount(CondCool_Abstract):  # Counter-Flow = 2
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def cond_balance(self, T14, H14, T3, H3, MREF,
                   TBUB, HBUB, CPRLIQ,
                   PBUB, P4):
        # replace TBUB with Cycle.obj_data.T[11]
        # replace PBUB with Cycle.obj_data.P[11]
        # replace HBUB with Cycle.obj_data.H[11]
        # replace TDEW with Cycle.obj_data.T[3]
        # replace HDEW with Cycle.obj_data.H[3]
        
        lstRes = self.ccount(MREF=MREF,
                           TS1=self.TS1,
                           T2=T14,
                           H2=H14,
                           TDEW_S=T3,
                           HDEW_S=H3,
                           TBUB_S=TBUB,
                           HBUB_S=HBUB,
                           CPRLIQ=CPRLIQ,
                           PIN=PBUB,
                           POUT=P4,
                           NUM_ZONE=self.N_COND
                           )

        # Sample output QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB
        dicRes = {'QDSC':lstRes[0]  # Q desuperheating
                 ,'QTPC':lstRes[1]  # Q two phase
                 ,'QSCC':lstRes[2]  # Q subcooling
                 ,'QTOTC':lstRes[3]  # Q total condenser
                 ,'FSUP':lstRes[4]  # Fraction desuperheating
                 ,'FSUB':lstRes[5]  #  Fraction subcooling  
                 }
        return dicRes

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def ccount(self, MREF,
               TS1, T2, H2, 
               TDEW_S, HDEW_S, TBUB_S, HBUB_S,
               CPRLIQ, PIN, POUT,
               NUM_ZONE):

        #  subroutine ccount - calculates the condenser heat exchange  
        #  for counterflow heat exchanger                              

        AREA_TOL = 0.001

        #  initialize
        TBUB = TBUB_S
        HBUB = HBUB_S

        if(H2 < HDEW_S):
            HDEW = H2
            TDEW = T2
            ENTERS_WET = True
        else:
            HDEW = HDEW_S
            TDEW = TDEW_S
            ENTERS_WET = False

        HDEW_START = HDEW
        TDEW_START = TDEW

        DELP = (PIN - POUT) / float(NUM_ZONE)
        DELH = (HDEW - HBUB) / float(NUM_ZONE)
        PBUB = POUT

        QDSC = 0.0
        QTPC = 0.0
        QSCC = 0.0

        ASCC = 0
        ATPC = 0
        ADSC = 0

        TAIR = TS1
        CAIR = self.CFMC
        HAVE_NOT_USED_FULL_AREA = True

        #  start off with the subcooling area.
        if(self.DTSUBC > 0.0):
            TCSUB = TBUB - self.DTSUBC
            CRSC = MREF * CPRLIQ

            if(CAIR < CRSC):
                CMINSC = CAIR
                CMAXSC = CRSC
            else:
                CMINSC = CRSC
                CMAXSC = CAIR

            #  is area big enough for subcooling
            QMAX = CMINSC * (TBUB - TAIR)
            QSUB = CRSC * (TBUB - TCSUB)

            EFF_SUB = QSUB / QMAX
            [EFFSCC, DEXDAR] = self.exf(1,
                self.ATOTC, self.USCC, CMINSC, CMAXSC)

            if(EFFSCC < EFF_SUB):  # Need more area
                ASCC = self.ATOTC
                HAVE_NOT_USED_FULL_AREA = False

                #  begin iteration process to determine solution for the
                #  subcooled region

                #  initialize variables
            else:
                ASCC = self.ATOTC / 10.0
                LOOKING_FOR_AREA = True

                while (LOOKING_FOR_AREA):
                    [EFFSCC, DEXDAR] = self.exf(1,
                        ASCC, self.USCC, CMINSC, CMAXSC)

                    ERROR = abs(EFFSCC - EFF_SUB)

                    if(ERROR < AREA_TOL):
                        LOOKING_FOR_AREA = False
                        continue

                    DAREA = - (EFFSCC - EFF_SUB) / DEXDAR
                    DAREA_MIN = -0.50 * ASCC
                    DAREA_MAX = 0.50 * (self.ATOTC - ASCC)

                    if(DAREA < DAREA_MIN):
                        DAREA = DAREA_MIN
                    if(DAREA > DAREA_MAX):
                        DAREA = DAREA_MAX

                    ASCC = ASCC + DAREA

            QSCC = EFFSCC * QMAX
            TAIR = TAIR + QSCC / CAIR

        #  continue with two-phase area
        ALEFT = self.ATOTC - ASCC

        for N in range(1, NUM_ZONE): 
            PDEW = PBUB + DELP
            HDEW = HBUB + DELH
            # [TDEW, XQ, XL, XV, VL, VV, HL, HV] = self.hpin(HDEW, PDEW, X)
            TDEW = self.objCP.Property('T', H=HDEW, P=PDEW)  # K
            
            if(HAVE_NOT_USED_FULL_AREA):
                CPRTP = (HDEW - HBUB) / abs(TDEW - TBUB + 0.0001)
                CRTP = MREF * CPRTP

                #  determine cmin and cmax in the two-phase region
                if(CAIR < CRTP):
                    CMINTP = CAIR
                    CMAXTP = CRTP
                else:
                    CMINTP = CRTP
                    CMAXTP = CAIR

                #  is area big enough for condensation
                QMAX = CMINTP * (TDEW - TAIR)
                QDUM = MREF * (HDEW - HBUB)

                EFF_TPC = QDUM / QMAX
                [EFFTPC, DEXDAR] = self.exf(
                    1, ALEFT, self.UTPC, CMINTP, CMAXTP)


                if(EFFTPC < EFF_TPC or (ENTERS_WET .AND. N == NUM_ZONE)):
                    ATPC = ATPC + ALEFT
                    HAVE_NOT_USED_FULL_AREA = False

                    #  begin iteration process to determine solution for the
                    #  two phase region

                    #  initialize variables
                else:
                    ADUM = 0.9 * ALEFT
                    LOOKING_FOR_AREA = True

                    ILOOK = 0
                    while (LOOKING_FOR_AREA):
                        ILOOK = ILOOK + 1
                        [EFFTPC, DEXDAR] = self.exf(1,
                                ADUM, self.UTPC, CMINTP, CMAXTP)
                                
                        ERROR = abs(EFFTPC - EFF_TPC)

                        if(ERROR < AREA_TOL or ILOOK >= 10):
                            OOKING_FOR_AREA = False
                            continue

                        DAREA = - (EFFTPC - EFF_TPC) / DEXDAR
                        DAREA_MIN = -0.75 * ADUM
                        DAREA_MAX = 0.50 * (ALEFT - ADUM)

                        if(DAREA < DAREA_MIN):
                            DAREA = DAREA_MIN
                        if(DAREA > DAREA_MAX):
                            DAREA = DAREA_MAX

                        ADUM = ADUM + DAREA

                    ATPC = ATPC + ADUM


                QTPC = QTPC + EFFTPC * QMAX
                TAIR = TAIR + EFFTPC * QMAX / CAIR

            ALEFT = self.ATOTC - ASCC - ATPC
            HBUB = HBUB + DELH
            TBUB = TDEW
            PBUB = PBUB + DELP

        if(ALEFT < 0.0):
            HAVE_NOT_USED_FULL_AREA = False
        
        #  continue with desuperheating area
        HDEW = HDEW_START
        TDEW = TDEW_START

        if(HAVE_NOT_USED_FULL_AREA):
            CPRVAP = (H2 - HDEW) / (T2 - TDEW)
            CRDS = MREF * CPRVAP

            #  determine cmin and cmax in the two-phase region
            if(CAIR < CRDS):
                CMINDS = CAIR
                CMAXDS = CRDS
            else:
                CMINDS = CRDS
                CMAXDS = CAIR

            #  determine the net heat transfer
            [EFFDSC, DEXDAR] = self.exf(1,
                    ALEFT, self.UDSC, CMINDS, CMAXDS)

            QDSC = CMINDS * EFFDSC * (T2 - TAIR)

            ADSC = ALEFT


        #   calculate the fractional subcooling and superheating regions

        FSUP = ADSC / self.ATOTC
        FSUB = ASCC / self.ATOTC

        QTOTC = QSCC + QTPC + QDSC
        self.UACOND = self.ATOTC * \
            (FSUP * self.UDSC + FSUB * self.USCC 
                + (1.0 - FSUP - FSUB) * self.UTPC)

        return [QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]
