# Python import


# User import
from common_classes.QData import QData

from cycle_classes.CycleUtils import CycleUtils
from cycle_classes.CoolPrp import *

from cycle_classes.Compressor import *
from cycle_classes.Evaporator import *
from cycle_classes.Condenser import *
from cycle_classes.EvapXCH import *

from cycle_classes.ErrorException import ErrorException
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job             : Create Class object cycle paramters
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class CycleSolver (CycleUtils):
    def __init__(self, objCP, objData, lng_item, NCYC=1):
        self.objCP = objCP
        self.dt = objData
        self.lng_item = lng_item

        # lng_item group number of data.
        # NCYC number of calls to cycle (1=Single or 2= Dual cycle)
        self.NCYC = NCYC

        self.paraCycle()

        # Create Basic objects
        self.setupCond(self.dt.ICONDI[self.lng_item])  # ICOND

        self.setupEvap(self.dt.IFRSHI[self.lng_item]   # IFRSH
                                ,self.dt.ISPECI[self.lng_item]   # ISPEC
                                )

        self.setupComp(ICOMP=self.dt.ICOMP
                                ,TAMB=self.TS1
                                ,FRACT_SPEED=self.SPEEDN
                                ,strFileName=self.dt.FILE_NAME)

    #-- setup basic objects
    def setupEvap(self, IFRSH, ISPEC): # will call paraEvap
        self.IFRSH = IFRSH
        self.ISPEC = ISPEC

        objEvaporatorXCHG = EvapXCH()
        self.objEvapXCH = objEvaporatorXCHG.getObject(objCP=self.objCP
                                                        , ISPEC=ISPEC
                                                        , dt=self.dt)

        objEvaporator = Evaporator()
        self.objEvap = objEvaporator.getObject(objCP=self.objCP, IFRSH=IFRSH)

        #------- Setup paramters
        self.objEvap.setParamters(ATOTE=self.dt.ATOTEI[self.lng_item]
                    , CFME=self.CFME
                    , TS3=self.TS3
                    , N_EVAP=self.dt.N_EVAP
                    , USUPE=self.USUPE
                    , UTPE=self.UTPE
                    , TROOM=self.dt.TROOM
                    , FZTEMP=self.dt.FZTEMP
                    , UA_FF=self.dt.UA_FF
                    , Q_HXS_FF=self.dt.Q_HXS_FF
                    , IWALL_FF=self.dt.IWALL_FF
                    , NUM_ZONE=self.dt.N_EVAP
                    , IRFTYP=self.dt.IRFTYP
                    )

    def setupCond(self, ICOND): # will call paraCond
        self.ICOND = ICOND
        objCondenser = Condenser()
        self.objCond = objCondenser.getObject(objCP=self.objCP, ICOND=ICOND)

        self.objCond.setParamters(ATOTC=self.dt.ATOTCI[self.lng_item]
                    , UA_FF_CND=self.dt.UA_FF_CND
                    , UA_FZ_CND=self.dt.UA_FZ_CND
                    , UA_FF_HXS=self.dt.UA_FF_HXS
                    , UA_FZ_HXS=self.dt.UA_FZ_HXS
                    , CFMC=self.dt.CFMC
                    , DTSUBC=self.DTSUBC
                    , N_COND=self.dt.N_COND
                    , TS1=self.TS1
                    , TS3=self.TS3
                    , TS5=self.dt.TS5
                    , USCC=self.USCC
                    , UTPC=self.UTPC
                    , UDSC=self.UDSC
                    )

    def setupComp(self, ICOMP, TAMB, FRACT_SPEED, strFileName):
        self.ICOMP = ICOMP
        self.objComp = Compressor(objCP=self.objCP,
                                TAMB=TAMB,
                                ICOMP=ICOMP,
                                FRACT_SPEED=FRACT_SPEED,
                                strFileName=strFileName)

    #-- send basic configration
    def paraCycle(self):
        # Evaporator Total Heat Transfer Surface Area m2
        self.ATOTE = self.dt.ATOTEI[self.lng_item]

        # Condenser Total Heat Transfer Surface Area
        self.ATOTC = self.dt.ATOTCI[self.lng_item]

        self.IFRSH = self.dt.IFRSHI[self.lng_item]
        self.ICOND = self.dt.ICONDI[self.lng_item]
        self.ICOOL = self.dt.ICOOLN[self.lng_item]

        # Estimated clearance volume m2
        self.CE = self.dt.CEI[self.lng_item]

        # Air Flow Rate Across Coil (L/S)
        self.CFMC = self.dt.CFMCI[self.lng_item]

        # Compressor Displacement
        self.DISPLC = self.dt.DISPLC[self.lng_item]

        # Refrigerant Exit Superheat (C) Or Quality (0-1)
        self.DTSUPE=self.dt.DTSPEI[self.lng_item]

        # 'Interchanger exit superheat C
        self.DTSUPI=self.dt.SUPIHX[self.lng_item]

        # Liquid-Line Anti-Sweat Heat watt
        self.DTSUBC = self.dt.CONDHT[self.lng_item]

        # Rated EER
        self.ERR = self.dt.EERN[self.lng_item]

        # Condenser Fan Power (watt)
        self.FANCL = self.dt.FNPWRC[self.lng_item]

        # Evaporator Fan Power (watt)
        self.FANES = self.dt.FNPWRE[self.lng_item]

        # Desuperheating Heat Transfer Conductance W/m2-c
        self.UDSC = self.dt.UDSCI[self.lng_item]

        # Two-Phase Heat Transfer Conductance W/m2-c
        self.UTPC  = self.dt.UTPCI[self.lng_item]

        # Subcooling Heat Transfer Conductance W/m2-c
        self.USCC  = self.dt.USCCI[self.lng_item]

        # Mechanical Efficiency
        self.MEFF = self.dt.MEFF[self.lng_item]

        # Initial Guess For Refrigerant Mas Flow Rate kg/hr
        self.MREF = self.dt.MREFI[self.lng_item]

        # self.dt.DTSBCI[self.lng_item]
        # self.dt.ELOSS[self.lng_item]
        # self.dt.CONDVP[self.lng_item]

        # Two-Phase Heat Transfer Conductance W/m2-c
        self.UTPE = self.dt.UTPEI[self.lng_item]

        # Superheat Region Conductance W/m2-c
        self.USUPE = self.dt.USUPEI[self.lng_item]

        # Rated Capacity kcal/hr
        self.SIZE = self.dt.SIZEN[self.lng_item]

        # Nominal Speed rpm
        self.SPEED = self.dt.SPEEDI[self.lng_item]

        #Fractional Speed
        self.SPEEDN = self.dt.SPDNOM[self.lng_item]

        # isentropic efficiency
        self.SEFF = self.dt.SEFFI[self.lng_item]

        # Air Flow Rate Across Coil
        self.CFME = self.dt.CFMEI[self.lng_item]

        # Temp. At Comp., Inlet or -1 If Unspecified
        # converted before from C to K
        self.TSPEC=self.dt.TSPECI[self.lng_item]

        # TS1 - HEAT TRANSFER FLUID (HTF) TEMPERATURE ENTERING CONDENSER
        # TS3 - HTF TEMPERATURE ENTERING FRESH FOOD EVAPORATOR
        # TS5 - HTF TEMPERATURE ENTERING FREEZER EVAPORATOR
        self.TS1 = self.dt.TS1[self.lng_item]
        self.TS3 = self.dt.TS3[self.lng_item]
        # self.TS5 = TS5

        # DPC - PRESSURE DROP THROUGH CONDENSER
        # DPE - PRESSURE DROP THROUGH FRESH FOOD EVAPORATOR
        self.DPC = self.dt.DPC[self.lng_item]
        self.DPE = self.dt.DPE[self.lng_item]

        # ETHX1 - EFFECTIVENESS OF HIGH TEMP INTERCHANGER
        # ETHX2 - EFFECTIVENESS OF LOW  TEMP INTERCHANGER
        self.ETHX1=self.dt.ETHX[self.lng_item] # both same value
        self.ETHX2=self.dt.ETHX[self.lng_item]

        # QCAN - COMPRESSOR SHELL LOSS NORMALIZED TO POWER INPUT
        self.QCAN = self.dt.QCAN[self.lng_item]

        #-- setup basic variables
        #----------------------------------------------
        # 1 - COMPRESSOR INLET (SATURATED VAPOR)
        # 2 - COMPRESSOR DISCHARGE
        # 3 - CONDENSER DEW POINT
        # 4 - CONDENSER OUTLET
        # 5 - INLET TO FRESH FOOD EVAPORATOR
        # 6 - LIQUID LINE OUTLET FROM HIGH TEMP INTERCHANGER
        # 7 - OUTLET FROM FRESH FOOD EVAPORATOR
        # 8 - INLET TO FREEZER EVAPORATOR
        # 9 - OUTLET FROM FREEZER EVAPORATOR
        # 10 - LIQUID LINE OUTLET FROM LOW TEMP INTERCHANGER
        # 11 - CONDENSER BUBBLE POINT
        # 12 - FRESH FOOD EVAPORATOR DEW POINT
        # 13 - SUPERHEATED GAS LEAVING THE HIGH TEMP INTERCHANGER
        # 14 - CONDENSER INLET
        # 15 - INTERNAL VARIABLE (NOT SHOWN) FOR EVAP DEW POINT
        # 16 - LIQUID LINE STATE AFTER HEAT LOSS TO CABINET AND MULLION

        # L - LOGICAL VARIABLE (ERROR FLAG, ETC.)
        # P - PRESSURE
        # T - TEMPERATURE
        # TC - REFRIGERANT AT CONDENSER OUTLET
        # TE - REFRIGERANT AT EVAPORATOR OUTLET
        # TOL - CONVERGENCE TOLERANCE
        # TS - TEMPERATURE OF HEAT TRANSFER FLUID
        # V - VOLUME
        # X - COMPOSITION

        self.T = [0.0] * (16+1) # Temp in Key
        self.P = [0.0] * (16+1) # Pressure in pascal
        self.V = [0.0] * (16+1) # Volume in m3/kg

        self.H = [0.0] * (16+1) # Enthalpy in j/kg
        self.S = [0.0] * (16+1) # Entorpy in j/kg K

        self.TE = [0, 0, 0]
        self.TC = [0, 0, 0]

    #-- Soving actions
    def solveCycle(self):

        print ("\n\n== Starting processing ===")
        #	INITIAL GUESSES FOR TC AND TE
        #	ASSUME TEMP RISE OF COND IS 0.5 F PER LBM

        # DR omar TC and TS1 in K to fix equation
        # MREF kg/hr = MREF/2.20462 LBM /hr
        # N.B (5/9) *(1/2) = (1/3.6)
        #self.TC[1] = self.TS1 + self.dt.MREF / 3.6
        self.TC[1] = self.TS1 + 0.5 * 5/9 * (self.MREF / 2.20462) 

        # -----------------------
        # steps for change MREF from kg/hr to kmole/hr was ignored
        # stepts was simplifed, all vars equal to self.MREF
        self.FLOW = self.FLOW2 = self.FLWREF = self.MREFSV = self.MREF

        # set outer loop data
        self.JC = 1
        self.LCCON = True
        self.LQUIT = False
        self.TEMIN = 210.0 # K

        #	SET UP TEMPERATURES AND CABINET LOADS FOR INLET TEMPERATURE
        #	CALCULATION FOR EVAPORATOR OF A STANDARD DESIGN (TYPE 1)
        self.TFF = self.dt.FFTEMP # Fresh Food Temperature
        self.TFZ = self.dt.FZTEMP # Freezer Temperature

        self.ICONC = 0 # Condenser Flag, =1 if iteration result is good
        self.IC = 1  # Condenser itemation counter
        self.ITMAXC = 100 # Condenser max. number of iterations

        self.DUTYR = 0.5

        self.FSUPC = 0.1
        self.FSUPE = 0 # in python only
        # -----------------------

        self.__solveCycle()

    # basic Solver
    def __solveCycle(self):
        self.T[15] = self.TS3 - (self.DTSUPE + 2.0)

        # Step 01 - Preparation
        # [XL_Temp, X, P[15], VL[15], V[15], LCRIT]
        #       = self.bublt(T[15], XL_Temp, X, False)
        self.P[15] = self.objCP.Property('P', X=1, T=self.T[15])  # pas

        self.TE[1] = self.T[15] + self.DTSUPE
        self.T[7] = self.TE[1] # 7 - OUTLET FROM FRESH FOOD EVAPORATOR
        self.P[7] = self.P[15]

        # Step 02 - condenser itaration loop
        while (self.IC <= self.ITMAXC and self.LCCON):
            print ("Condenser Traial Number ------ self.IC=",self.IC)

            # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            # this block is common for all solvers (1,2, and 3)
            # ICAB - flag to represent presence of cabinet loads in input

            # Dr. Omar to check adjlod
            #ICAB Flag to represent presence of cabinet loads in input, 0 =No
            if (self.dt.ICAB == 1):
                [self.TS3, self.TS5] =  self.adjlod(self.dt, self.MREF
                                # , self.dt.ATOTEI[self.lng_item]
                                , self.dt.ICYCL
                                , self.IC
                                , self.TS3
                                , self.TS5
                                , self.FROSTF
                                , self.FROSTF
                                , self.IDFRST)

            self.T[4] = self.TC[self.JC] # 4 - CONDENSER OUTLET
            
            #	find condenser pressure for current guess of TC
            TBUB4 = self.TC[self.JC] + self.DTSUBC

            # [X,XV_Temp,P[4],VBUB4,VV[4],LCRIT]
            #   = self.bublt(TBUB4, X, XV_Temp, True)
            self.P[4] = self.objCP.Property('P', X=0, T=TBUB4)  # pas

            # determine the specific volume of the liquid
            if self.DTSUBC > 0:
                self.V[4] = self.objCP.Property('V', X=0
                                               , T=self.TC[self.JC])  # m3/kg
            else:
                self.V[4] = self.objCP.Property('V', X=0, T=TBUB4)  # m3/kg

            # condenser dew point
            self.P[3] = self.P[4] + (1 - self.FSUPC) * self.DPC
            
            # check if CRITICAL TEMPERATURE EXCEEDED IN CONDENSER
            # and display error in this case

            # ENTHALPY AT STATE 4 (CONDENSER OUTLET)
            # [H[4], CV, CP,VSND] = self.hcvcps(1, TC[JC],V[4], X)
            self.H[4] = self.objCP.Property('H', V=self.V[4]
                                               , T=self.TC[self.JC])  # j/kg

            # ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
            self.H[16] = self.H[4] \
                - self.dt.CONDHT[self.NCYC] / self.MREF / self.DUTYR

            self.P[16] = self.P[4]

            # [T[16], XQ[16], XL_Temp, XV_Temp, VL[16], VV[16], HL16, HV16]
            # = self.hpin(H[16], P[16], # X)

            # 16 - LIQUID LINE STATE AFTER HEAT LOSS TO CABINET AND MULLION
            self.T[16] = self.objCP.Property('T', P=self.P[16]
                                                , H=self.H[16]) # K
            # if (VL[16] == 0.0):
                # VL[16] = V[4]
            # V[16] = VL[16]

            self.V[16] = self.objCP.Property('V', P=self.P[16]
                                                , H=self.H[16])  # K
            if (self.V[16] == 0.0):
                self.V[16] = self.V[4]
            # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=End of common block

            self.evapIteration()

            #--- Reptead block ----
            self.calc_lowevap()
            # end of repeated block

            #--------------------------END OF NEW CODE (12/29/90)-------------
            #
            #	FIND ENTROPY AT COMPRESSOR INLET AND COMPUTE CONDITIONS AT
            #	COMPRESSOR OUTLET (TSPEC IS DEFINED INLET TEMP TO THE
            #	COMPRESSOR (-1 MEANS NO TEMPERATURE CHANGE)
            #
            self.P[1] = self.P[13]

            if (self.TSPEC >  0.0):
                self.T[1] = self.TSPEC
                # [H[1],CV,CP,VS] = self.hcvcps (1,T[1],V[1],X) # CALL HCVCPS
                self.V[1] = self.objCP.Property('V', X=1, P=self.P[1])  # m3/kg
                self.H[1] = self.objCP.Property('H', X=1, P=self.P[1])  # j/kg
            else:
                self.V[1] = self.V[13] # bug if ISPEC<>2 and TSPEC <=0
                self.T[1] = self.T[13]
                self.H[1] = self.H[13]

            # ----------------Dr Omar this block only in Python
            # fix bug if ISPEC<>2 and TSPEC <=0
            if self.ISPEC != 2 and self.TSPEC < 0.0:
                self.V[1] = self.objCP.Property('V', T=self.T[1]
                    , P=self.P[1])  # m3/kg

            #---------------------End if block to fix bug------
            
            # S[1] = self.entrop(T[1],V[1],X)
            # self.S[1] = self.objCP.Property('S', V=self.V[1]
                                               # , T=self.T[1])  # j/kg K

            self.S[1] = self.objCP.Property('S', X=1
                                               , T=self.T[1])  # j/kg K

            self.P[2] = self.P[3] + self.FSUPC * self.DPC

            #........Compreesor Class..................           
            print ("\n\nInput to compressor -------------------")
            print ("PSUCT = self.P[1] = ", self.P[1])
            print ("TSUCT = self.T[1] = ", self.T[1])
            print ("VSUCT = self.V[1] = ", self.V[1])
            
            print ("PDISC = self.P[2] = ", self.P[2])
            print ("MREF = self.MREF = ", self.MREF)
            print ("\n\n")
                        
            # only one type
            dicRest = self.objComp.comp_balance(PSUCT=self.P[1],
                                   PDISC=self.P[2],
                                   TSUCT=self.T[1],
                                   MREF=self.MREF,
                                   VSUCT=self.V[1])
            print ("compressor output")
            print ("dicRest['TDISC'] = ", dicRest['TDISC'])
            print (dicRest)
            
            # Compressor exit Temp K Dr Omar
            # self.T[1] = dicRest['TSP']

            # Dischare Temp K
            self.T[2] = dicRest['TDISC']

            # Dischare Enthalpy    j/kg
            self.H[2] = dicRest['HOUT']

            # compressor shell loss normalized to power input j/kg
            QCAN  = dicRest['QCAN']

            # Suction sp.volume m3/kg
            VSUC = dicRest['VSUC']

            # Dischare sp.volume m3/kg
            VV2 = dicRest['VV2']

            # Cp/Cv value
            GAMA = dicRest['GAMA']

            # Compressor Efficiency   %
            ETAS = dicRest['ETAC']

            # add by Ayman Dr Omar to check, MROLD used later
            self.MROLD = self.MREF
            
            # Refrigerant Mas Flow Rate  kg/hr
            self.MREF = dicRest['MREF']
            #........End Compreesor Class

            self.FLOW2 = self.FLWREF * self.MREF / self.MREFSV

            # Python why calculate T[2] ??? Dr Omar
            # CONDITIONS OF GAS LEAVING COMPRESSOR SHELL

            self.H[2] = self.objCP.Property('H', P=self.P[2], V=VV2)  # j/kg
            self.T[2] = self.objCP.Property('T', P=self.P[2], V=VV2)  # K
            self.V[2] = self.objCP.Property('V', P=self.P[2], H=self.H[2])  # j/kg

            #	ENTROPY OF GAS LEAVING COMPRESSOR
            self.S[2] = self.objCP.Property('S', T=self.T[2], V=self.V[2])  # K

            if self.ICONC == 1: # flag in Condenser class if iteration is good
                self.LCCON = False # exit loop
            else:
                self.condenser_calc()

            self.IC = self.IC + 1 # do another trail

            if self.dt.INCTRL in [0, 3]:
                if (self.IC <= 4):
                    self.LCCON = True
                    self.ICONC = 0

            if self.dt.INCTRL in [1, 2, 4]:
                if (sel.IC <= 10):
                    self.LCCON = True
                    self.ICONC = 0

        print ("\n----------------------------")
        n = 0
        print ("#\t\tT C\tP kPa\tH kj/kg")
        for n in range (1,17):
            print (n, "\t%9.2f\t%9.2f\t%9.2f"% (self.T[n]-273.11, self.P[n]/1000, self.H[n]/1000) )

        print ("----------------------\n\n\n")

        return

    # condenser calculations
    def condenser_calc(self):
        #	CALCULATE CONDENSER HEAT EXCHANGE
        #	DETERMINE THE DEW POINT CONDITIONS
        self.T[3] = self.objCP.Property('T', P=self.P[3], X=1)  # K
        self.H[3] = self.objCP.Property('H', P=self.P[3], X=1)  # j/kg
        
        #	DETERMINE BUBBLE POINT CONDITIONS
        #	ASSUME A LINEAR PRESSURE DROP THROUGHOUT THE CONDENSER
        
        # Dr Omar change FSUBC to FSUPC
        # self.P[11] = PBUB = self.P[4] + self.DPC * self.FSUBC
        self.P[11] = PBUB = self.P[4] + self.DPC * self.FSUPC
        
        self.T[11] = TBUB = self.objCP.Property('T', P=PBUB, X=0)  # K
        self.H[11] = HBUB = self.objCP.Property('H', P=PBUB, X=0)  # j/kg
        CPRLIQ = self.objCP.Property('CP', P=PBUB, X=0)  # j/kg K

        #	DETERMINE CONDITIONS ENTERING THE CONDENSER
        HDROP = self.dt.CONDVP[self.NCYC] / self.MREF / self.dt.DUTYC

        self.P[14] = self.P[2]
        HINCND = self.H[2] - HDROP

        self.H[14] = HINCND
        self.T[14] = self.objCP.Property('T', H=self.H[14]
                                            , P=self.P[2])  # K

        #........Conderser Class..................
        if self.ICOND == 0:
            dicRest = self.objCond.cond_balance(T14=self.T[14],
                                        H14=self.H[14],
                                        T3=self.T[3],
                                        H3=self.H[3],
                                        T5=self.T[5],
                                        T8=self.T[8],
                                        T9=self.T[9],
                                        T12=self.T[12],
                                        TBUB=TBUB,
                                        HBUB=HBUB,
                                        MREF=self.MREF,
                                        CPRLIQ=CPRLIQ)

        elif self.ICOND == 1:
            dicRest = self.objCond.cond_balance(T14=self.T[14],
                                        H14=self.H[14],
                                        T3=self.T[3],
                                        H3=self.H[3],
                                        TBUB=TBUB,
                                        HBUB=HBUB,
                                        MREF=MREF,
                                        CPRLIQ=CPRLIQ,
                                        PBUB=PBUB,
                                        P4=self.P[4]
                                        )

        elif self.ICOND == 2:
            dicRest = self.objCond.cond_balance(T14=self.T[14],
                                        H14=self.H[14],
                                        T3=self.T[3],
                                        H3=self.H[3],
                                        TBUB=TBUB,
                                        HBUB=HBUB,
                                        MREF=self.MREF,
                                        CPRLIQ=CPRLIQ,
                                        PBUB=PBUB,
                                        P4=self.P[4]
                                        )

        #== Output of condenser class --------
        QDSC = dicRest['QDSC']
        QTPC = dicRest['QTPC']
        QSCC = dicRest['QSCC']
        QTOTC = dicRest['QTOTC']

        FSUP = dicRest['FSUP']
        FSUB = dicRest['FSUB']
        
        # [Q_CND_FF, Q_CND_FZ, Q_HXS_FF
            # ,Q_HXS_FZ, UACOND, UDSC, USCC, UTPC] = \
                # self.objCond.getExtarOutputs()

        # Condenser Heat Fresh Food,  Q_CND_FF
        # Condenser Heat Freezer,     Q_CND_FZ
        # Heat Exchanger Fresh Food   Q_HXS_FF
        # Heat Exchanger Freezer      Q_HXS_FZ
        # Condenser UA                UACOND
        # Desuperheating Heat Transfer Conductance, kj/hr/m2/C UDSC
        # Subcooling Heat Transfer Conductance, kj/hr/m2/C     USCC
        # Two-Phase Heat Transfer Conductance,   kj/hr/m2/C    UTPC

        #- cond method
        lstRest = self.objCond.cond(T4=self.T[4]
                       , H4=self.H[4]
                       , H14=self.H[4]
                       , TC=self.TC
                       , JC=self.JC
                       , QCONDS=QDSC
                       , QCONDC=QTPC
                       , QSCC=QSCC
                       , MROLD=self.MROLD # to check Dr Omar
                       , MREF=self.MREF
                       )

        # output of cond method
        self.TS2 = lstRest[0]
        self.TC = lstRest[1]
        self.JC = lstRest[2]
        self.ICONC = lstRest[3]

        #........End Conderser Class .............

        #	ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
        self.H[16] = self.H[4] \
                    - self.dt.CONDHT[self.NCYC] / self.MREF / self.DUTYR
        self.P[16] = self.P[4]
        self.T[16] = self.objCP.Property('T', T=self.H[16]
                                            , P=self.P[16])  # K

        #	CALCULATE THE AVERAGE EFFECTIVENESS OF THE HEAT EXCHANGER
        #	CALCULATE THE HEAT TRANSFER if THE REFRIGERANT LEFT AT Cycle.obj_parameter.TS1
        #
        #	DETERMINE THE SPECIFIC VOLUME OF THE LIQUID
        #
        if (self.TS1 < self.T[4]):
            VS1 = self.objCP.Property('V', T=self.TS1
                                         , P=self.P[4])  # m3/kg
        else:
            VS1 = V[4]

        HS1 = self.objCP.Property('T', T=self.TS1, V=VS1)  # j/kg
        QRMAX = self.MREF * (self.H[14] - HS1)

        #	CALCULATE THE HEAT TRANSFER if THE AIR LEFT AT T[14]

        QAMAX = self.CFMC * (self.T[14] - self.TS1)
        QMAXC = QAMAX

        if (QRMAX < QAMAX):
            QMAXC = QRMAX

        self.ETAC = QTOTC / QMAXC
        # -----------------------------------------------------------
        #	END OF Conderser ITERATION loop
        # -----------------------------------------------------------

    # Evaporator iteration
    def evapIteration (self):

        # prepare for inner loop
        self.JE = 1
        LECON = True
        ITMAXE =  40

        # Step 03 - Evap itaration loop
        #	enter iteration for evaporator outlet temperature
        IE = 1
        while ( (IE <= ITMAXE) and LECON):
            # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            # this block is common for all solvers (1,2, and 3)
            I_ERROR_INTER = 0
            # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=End of common block

            self.TE[self.JE] = self.T[15] + self.DTSUPE

            # [XL_Temp, X, P[15], VL[15], V[15], LCRIT]
            #   = self.bublt(T[15],XL_Temp, X, False)

            self.P[15] = self.objCP.Property('P', X=1, T=self.T[15]) # pas
            self.P[7] = self.P[15]

            # this block is common for all solvers (1,2, and 3)
            #	determine the bubble point at the evap exit pressure
            TBUB15 = self.objCP.Property('T', X=0, P=self.P[15])  # K
            VBUB15 = self.objCP.Property('V', X=0, P=self.P[15])  # m3/kg

            #	determine the bubble and dew point enthalpies
            self.H[15] = self.objCP.Property('H', X=1, T=self.T[15]) # j/kg

            HBUB15 = self.objCP.Property('H', X=0, P=self.P[15])  # j/kg
            # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=End of common block

            #--- Reptead block ----
            self.calc_lowevap()
            # end of repeated block

            # Calculate fresh food section heat exchange
            PDEWE = self.P[5] - (1.0 - self.FSUPE) * self.DPE

            if (PDEWE > self.P[5]):
                PDEWE = self.P[5]

            # [XREF,X,TDEW,VLDEW,VDEW,LCRIT] = self.bublp(PDEWE,XREF,X,False)
            TDEW = self.objCP.Property('T', P=PDEWE, X=1)  # K

            # Python POLDE is not used !!!
            POLDE = PDEWE
            if (TDEW >= self.TS3):
                TDEW = self.TS3 - 1.0

            if (self.T[5] >= self.TS3):
                self.T[5] = self.TS3 - 1.0

            #[HDEW,CVRVAP,CPRVAP,VS] = self.hcvcps(3,TDEW,VDEW,X)
            HDEW = self.objCP.Property('H', X=1, P=PDEWE)  # j/kg
            CPRVAP = self.objCP.Property('CP', X=1, P=PDEWE)  # j/kg K

            # STATE 12 IS THE POINT AT WHICH THE DEW POINT IS REACHED IN
            # THE EVAPORATOR
            self.P[12] = PDEWE
            self.T[12] = TDEW
            #self.V[12] = VDEW
            self.H[12] = HDEW
            #XQ[12] = 1.0

            if self.IC !=1: # skip first trail to calc. some values later
                [QFF, QFZ, DUTYR] = \
                    self.dutfnd(self.dt, self.dt.ICAB,
                                      self.dt.IRFTYP,
                                      self.dt.ICYCL,
                                      self.NCYC,
                                      self.QFRSH, # from Evaporator class
                                      self.QFREZ,
                                      self.dt.FROSTF,
                                      self.dt.FROSTZ,
                                      self.TS3,
                                      self.dt.TS5,
                                      self.T,
                                      self.dt.IDFRST)

            #	CALCULATE FRESH FOOD EVAPORATOR HEAT TRANSFER.
            #	TEST FOR STANDARD DESIGN.
            
            # Dr Omar to check the following
            if (self.dt.IRFTYP <= 3):
            
                if (self.dt.ICYCL == 1 
                    and self.dt.ICAB != 0
                    and self.IFRSH != 0):

                    if (self.dt.IC == 1):
                        TIN = 0.15 * TFF + 0.85 * TFZ
                        self.FF_FRACT = 0.0  # in Python only

                    else:
                        self.dt.CAPE = self.QFRSH / 1.0548 - 3.413 * \
                            self.dt.FANE - 3.413 * (self.dt.DFSTCYC + self.dt.FZCYC)
                            
                        CFMA = self.CFME / (1.08 * 1.8961)
                        QFM = QFF + 3.413 * self.dt.DUTYC * self.dt.FFCYC

                        [TIN, self.FF_FRACT] = self.mixair(
                            self.dt.CAPE, QFM, QFZ, TFF, TFZ, CFMA)

                    self.TS3 = (TIN + 459.6) / 1.8

            #........Evaporator Class..................
            # add evaporator class here

            if self.IFRSH == 0:
                dicRest = self.objEvap.evap_balance (MREF=self.MREF
                                       ,T5=self.T[5], H5=self.H[5], T7=self.T[7]
                                       ,TDEW=TDEW
                                       ,CPRVAP=CPRVAP
                                        )

            elif self.IFRSH == 1:
                dicRest = self.objEvap.evap_balance (MREF=self.MREF
                                       ,T5=self.T[5], H5=self.H[5]
                                       ,TDEW=TDEW
                                       ,CPRVAP=CPRVAP
                                       ,P5=self.P[5], P7=self.P[7]
                                        )

            elif self.IFRSH == 2:
                dicRest = self.objEvap.evap_balance (MREF=self.MREF
                                       ,T5=self.T[5], H5=self.H[5]
                                       ,TDEW=TDEW
                                       ,CPRVAP=CPRVAP
                                       ,P5=self.P[5], P7=self.P[7]
                                        )

            # ... Output ....
            self.QFRSH = dicRest['QFRSH']
            UAFF = dicRest['UAFF']
            FSUPE = dicRest['FSUPE']
            #..........................................

            # Superheating fraction
            # FSHOLD = FSUPE # useless code
            # FSUPE = (FSHOLD + FSUPE) / 2.0

            # if (FSUPE > 1.05 * FSHOLD):
                # FSUPE = 1.05 * FSHOLD

            # if (FSUPE < 0.95 * FSHOLD):
                # FSUPE = 0.95 * FSHOLD
            # # --- end of useless code block

            # fresh food section evaporator
            dicRest = self.objEvap.frsh(T5=self.T[5]
                            , H5=self.H[5]
                            , H7=self.H[7]
                            , TS3=self.TS3
                            , TE=self.TE, JE=self.JE
                            , QFRSH=self.QFRSH
                            , MREF=self.MREF)

            self.TE = dicRest['TE']
            self.JE = dicRest['JE']
            TS4 = dicRest['TS4']
            ICONE = dicRest['ICONE']

            # ---------------------------ADDED NEW CODE (12/29/90)---------
            self.T[15] = self.T[15] + self.TE[2] - self.T[7]

            HS3 = self.objCP.Property('H', X=0, T=self.TS3)  # j/kg

            QRMAX = self.MREF * (HS3 - self.H[5])

            # Calculate the heat transfer if the air left at T[5]
            QAMAX = self.CFME * (self.TS3 - self.T[5])
            QMAXE = QAMAX

            if (QRMAX < QAMAX):
                QMAXE = QRMAX

            ETAE = self.QFRSH / QMAXE

            # if (ICONE == 1):
                # LECON = False # Exit loop

            # if (self.TE[JE] <= self.TEMIN):
                # LECON = False # Exit loop

            if (ICONE == 1) or (self.TE[self.JE] <= self.TEMIN):
                LECON = False # Exit loop

            IE = IE + 1
        # -----------------------------------------------------------
        #	END OF EVAPORATOR ITERATION
        # -----------------------------------------------------------

        # Python add this statment which came after loop
        self.T[7] = self.TE[self.JE]
        self.TE[1] = self.TE[self.JE]


    # repeated block
    def  calc_lowevap (self):
        #--- Reptead block ----
        #	determine the enthalpy at [7]
        self.H[7] = self.objCP.Property('H', P=self.P[7]
                                           , T=self.T[7])  # j/kg
        self.T[7] = self.TE[self.JE]

        # this block is common for all solvers (1,and 3 only)
        self.dt.QINT = self.inter1(self.objCP
                                    , self.T[4], self.H[4]
                                    , self.T[7], self.H[7]
                                    , self.ETHX1)

        self.H[13] = self.H[7] + self.dt.QINT
        # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=End of common block 1,3

        # this block is common for all solvers (1,2.3)
        self.H[6] = self.H[16] + self.dt.QINT
        self.P[6] = self.P[4]
        self.P[13] = self.P[7]
        self.T[6] = self.objCP.Property('T', P=self.P[6]
                                           , H=self.H[6])  # K
        # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=End of common

        # this block is common for all solvers (1,2, and 3)
        self.T[13] = self.objCP.Property('T', P=self.P[13]
                                            , H=self.H[13])  # K
        # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=End of common block

        # this block is common for all solvers (1,2.3)
        self.P[5] = self.P[13] + self.DPE
        #PAGE 13 LINE 851
        [self.H, self.P, self.T, self.TS6, self.QFREZ] =\
                self.lowevp(self.dt, self.objCP, self.MREF, self.dt.ICYCL
                            , 0 # ICNTRL no value set in Fortran
                            , self.H, self.P, self.T
                            # , VL
                            # ,HL not clear its use
                            , self.TS3, self.dt.TS5, self.dt.DPF
                            , self.ETHX2)
        # end of repeated block

    def getSolution(self):
        objSolution = QData()

        objSolution.T = self.T
        objSolution.P = self.P
        objSolution.H = self.H
        objSolution.S = self.S

        return objSolution





