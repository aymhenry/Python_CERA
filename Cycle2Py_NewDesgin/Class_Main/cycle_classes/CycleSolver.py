# Python import


# User import
from common_classes.QData import QData

from cycle_classes.CycleUtils import CycleUtils
# from cycle_classes.CoolPrp import *

from cycle_classes.Compressor import *
from cycle_classes.Evaporator import *
from cycle_classes.Condenser import *
from cycle_classes.CoolPrpUtil import *
from cycle_classes.Trace import *


# from cycle_classes.ErrorException import ErrorException
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job            : Create Class object cycle parameters
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class CycleSolver(CycleUtils):
    K_C_DEG = 273.11

    #  setup basic objects
    def setupEvap(self, IFRSH, ISPEC):
        self.IFRSH = IFRSH
        self.ISPEC = ISPEC

        objEvaporator = Evaporator()
        self.objEvap = objEvaporator.getObject(objCP=self.objCP, IFRSH=IFRSH)

        # ------- Setup paramters
        self.objEvap.setParamters(ATOTE=self.dt.ATOTEI[self.lng_item],
                                  CFME=self.dt.CFME,  # watt/K
                                  TS3=self.TS3,
                                  N_EVAP=self.dt.N_EVAP,
                                  USUPE=self.USUPE,  # watt/m2-K
                                  UTPE=self.UTPE,  # watt/m2-K
                                  TROOM=self.dt.TROOM,
                                  FZTEMP=self.dt.FZTEMP,
                                  UA_FF=self.dt.UA_FF,
                                  Q_HXS_FF=self.dt.Q_HXS_FF,  # defalut =0 in Fortran
                                  IWALL_FF=self.dt.IWALL_FF,
                                  NUM_ZONE=self.dt.N_EVAP,
                                  IRFTYP=self.dt.IRFTYP
                                  )

    def setupCond(self, ICOND):
        self.ICOND = ICOND
        objCondenser = Condenser()
        self.objCond = objCondenser.getObject(objCP=self.objCP, ICOND=ICOND)

        self.objCond.setParamters(ATOTC=self.dt.ATOTCI[self.lng_item],
                                  UA_FF_CND=self.dt.UA_FF_CND,
                                  UA_FZ_CND=self.dt.UA_FZ_CND,
                                  UA_FF_HXS=self.dt.UA_FF_HXS,
                                  UA_FZ_HXS=self.dt.UA_FZ_HXS,
                                  CFMC=self.dt.CFMC,  # watt/K
                                  DTSUBC=self.DTSUBC,
                                  N_COND=self.dt.N_COND,
                                  TS1=self.TS1,
                                  TS3=self.TS3,
                                  TS5=self.TS5,
                                  USCC=self.USCC,  # W/m2-K
                                  UTPC=self.UTPC,  # W/m2-K
                                  UDSC=self.UDSC  # W/m2-K
                                  )

    def setupComp(self, ICOMP, TAMB, FRACT_SPEED, strFileName):
        self.ICOMP = ICOMP
        self.objComp = Compressor(objCP=self.objCP,
                                  TAMB=TAMB,  # K
                                  ICOMP=ICOMP,  # none
                                  FRACT_SPEED=FRACT_SPEED,  # none
                                  strFileName=strFileName  # none
                                  )

    def paraCycle(self):
        # TS1 - HEAT TRANSFER FLUID (HTF) TEMPERATURE ENTERING CONDENSER
        # TS3 - HTF TEMPERATURE ENTERING FRESH FOOD EVAPORATOR
        # TS5 - HTF TEMPERATURE ENTERING FREEZER EVAPORATOR
        self.TS1 = self.dt.TS1[self.lng_item]
        self.TS3 = self.dt.TS3[self.lng_item]
        self.TS5 = self.dt.TS5

        # Evaporator Total Heat Transfer Surface Area m2
        self.ATOTE = self.dt.ATOTEI[self.lng_item]

        # Condenser Total Heat Transfer Surface Area
        self.ATOTC = self.dt.ATOTCI[self.lng_item]

        self.IFRSH = self.dt.IFRSHI[self.lng_item]
        self.ICOND = self.dt.ICONDI[self.lng_item]

        # Refrigerant Exit Superheat (C)
        self.DTSUPE = self.dt.DTSPEI[self.lng_item]

        # Quality (0-1))
        self.XEXITE = self.dt.QUALITY[self.lng_item]

        # 'Interchanger exit superheat C
        self.DTSUPI = self.dt.SUPIHX[self.lng_item]

        # Liquid-Line Anti-Sweat Heat watt
        self.DTSUBC = self.dt.CONDHT[self.lng_item]

        # Condenser Fan Power (watt)
        self.FANCL = self.dt.FNPWRC[self.lng_item]

        # Evaporator Fan Power (watt)
        self.FANES = self.dt.FNPWRE[self.lng_item]

        # Desuperheating Heat Transfer Conductance W/m2-c
        self.UDSC = self.dt.UDSCI[self.lng_item]

        # Two-Phase Heat Transfer Conductance W/m2-c
        self.UTPC = self.dt.UTPCI[self.lng_item]

        # Subcooling Heat Transfer Conductance W/m2-c
        self.USCC = self.dt.USCCI[self.lng_item]

        # Initial Guess For Refrigerant Mas Flow Rate kg/hr
        self.MREF = self.dt.MREFI[self.lng_item]  # kg/hr

        # self.dt.DTSBCI[self.lng_item]
        # self.dt.ELOSS[self.lng_item]
        # self.dt.CONDVP[self.lng_item]

        # Two-Phase Heat Transfer Conductance W/m2-c
        self.UTPE = self.dt.UTPEI[self.lng_item]

        # Superheat Region Conductance W/m2-c
        self.USUPE = self.dt.USUPEI[self.lng_item]

        # Nominal Speed rpm
        self.SPEED = self.dt.SPEEDI[self.lng_item]

        # self.CFME Air Flow Rate Across Coil
        # self.CFMC Air Flow Rate Across Coil (L/S)

        # ===================        Air mass flow rate -----
        self.trace.dr_omar("Approved")  # Dr Omar
        # RHOCPF   = 316.8/TS5
        # CFMF     = 1.8961*(RHOCPF*CFMF)/0.4720

        # CFMCI, CFMEI L/sec --> CFMC,CFME kg/sec
        # Roh air kg/m3 = Temp_C_Deg/417.25 + 1.2934

        # RHOCPC = 316.8 / self.dt.TS1[lng_item]
        # RHOCPE = 316.8 / self.dt.TS3[lng_item]

        # https://www.gribble.org/cycling/air_density.html
        # self.CFMC = 1.8961 * (RHOCPC * self.dt.CFMCI[lng_item]) / 0.4720
        # CFMCI L/sec = 1000 cm3/sec = 1000/100^3 m3/sec= 1/1000 m3/sec

        # equation change to account for correct pressure 101.325 kPa,
        #       and interpolate between rho at 0 C and rho at 10 C
        # [m3/sec] * [kg/m3] * [j/kg/K] =j/sec/K = watt/K
        self.dt.CFMC = self.dt.CFMCI[self.lng_item] / 1000 * \
                       self.getAirDencity(self.TS1) * \
                       self.getAirCp(self.TS1)

        # ------------------------------------
        # self.CFME = 1.8961 * (RHOCPE * self.dt.CFMEI[lng_item]) / 0.4720
        # [m3/sec] * [kg/m3] * [J/kg K]*1000 =j/sec K = watt/K
        self.dt.CFME = self.dt.CFMEI[self.lng_item] / 1000 * \
                       self.getAirDencity(self.dt.TS3[self.lng_item]) * \
                       self.getAirCp(self.TS3)

        # ------------------------------------
        # [m3/sec] * [kg/m3] * [J/kg K]*1000 =j/sec K = watt/K
        self.dt.CFMF = self.dt.CFMF / 1000 * \
                       self.getAirDencity(self.dt.TS5) * \
                       self.getAirCp(self.TS5)
        # =================================

        # Temp. At Comp., Inlet or -1 If Unspecified
        # converted before from C to K
        self.TSPEC = self.dt.TSPECI[self.lng_item]

        # DPC - PRESSURE DROP THROUGH CONDENSER
        # DPE - PRESSURE DROP THROUGH FRESH FOOD EVAPORATOR
        self.DPC = self.dt.DPC[self.lng_item]
        self.DPE = self.dt.DPE[self.lng_item]

        # ETHX1 - EFFECTIVENESS OF HIGH TEMP INTERCHANGER
        # ETHX2 - EFFECTIVENESS OF LOW  TEMP INTERCHANGER
        self.ETHX1 = self.dt.ETHX[self.lng_item]  # both same value
        self.ETHX2 = self.dt.ETHX[self.lng_item]

        #   setup basic variables
        #  ----------------------------------------------
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

        self.T = [0.0] * (16 + 1)  # Temp in K
        self.P = [0.0] * (16 + 1)  # Pressure in pascal
        self.V = [0.0] * (16 + 1)  # Volume in m3/kg

        self.H = [0.0] * (16 + 1)  # Enthalpy in j/kg
        self.S = [0.0] * (16 + 1)  # Entorpy in j/kg K

        self.TE = [0, 0, 0]
        self.TC = [0, 0, 0]

        # self.objCP = objCP

    def __init__(self, objCP, dt, lng_item, NCYC=1):
        # --------------------------------------------------
        # main init method
        # --------------------------------------------------
        self.objCP = objCP
        self.dt = dt
        self.lng_item = lng_item

        # Trace Data
        self.trace = Trace(self.dt, self)
        self.coolutil = CoolPrpUtil(objCP)

        # lng_item group number of data.
        # NCYC number of calls to cycle (1=Single or 2= Dual cycle)
        self.NCYC = NCYC

        self.paraCycle()

        # Create Basic objects
        self.setupCond(self.dt.ICONDI[self.lng_item])  # ICOND

        self.setupEvap(self.dt.IFRSHI[self.lng_item],  # IFRSH
                       self.dt.ISPECI[self.lng_item]  # ISPEC
                       )

        self.setupComp(ICOMP=self.dt.ICOMP,
                       TAMB=self.TS1,
                       FRACT_SPEED=1,  # not used in IMAP=0
                       strFileName=self.dt.FILE_NAME
                       )

    def getAirCp(self, temp_K):
        # https://www.ohio.edu/mechanical/thermo/property_tables
        # /air/air_Cp_Cv.html#:~
        # :text=The%20nominal%20values%20used%20for,v%20%3D%200.718%20kJ%2Fkg
        
        # kJ/kg K as mnstioned in the above site
        # J/kg K
        return (0.0003 * (temp_K ** 2) - 0.129 * temp_K + 1016.55)

    def getAirDencity(self, temp_K):
        # m3/kg
        return 1.2873 + (temp_K - CycleSolver.K_C_DEG) / 10 * (1.2418 - 1.2873)
        
        
    #  -- Soving actions
    def solveCycle(self):
        print("\n\n== Starting processing ===")
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
        # INITIAL GUESSES FOR TC AND TE
        # ASSUME TEMP RISE OF COND IS 0.5 F PER LBM

        # MREF kg/hr = MREF/2.20462 LBM /hr
        # N.B (5/9) *(1/2) = (1/3.6)
        # self.TC[1] = self.TS1 + self.dt.MREF / 3.6
        # Approved - self.trace.dr_omar("MREF kg/hr = MREF/2.20462 LBM /hr")
        self.TC[1] = self.TS1 + 0.5 * 5 / 9 * (self.MREF / 2.20462)

        # -----------------------
        # stepts was simplifed, all vars equal to self.MREF
        # not used self.FLOW=self.FLOW2=self.FLWREF=self.MREFSV = self.MREF
        self.FLOW = self.MREFSV = self.MREF

        # set outer loop data       
        self.JC = 1
        self.LCCON = True
        self.LQUIT = False

        # SET UP TEMPERATURES AND CABINET LOADS FOR INLET TEMPERATURE
        # CALCULATION FOR EVAPORATOR OF A STANDARD DESIGN (TYPE 1)
        self.TFF = self.dt.FFTEMP  # Fresh Food Temperature
        self.TFZ = self.dt.FZTEMP  # Freezer Temperature

        self.ICONC = 0  # Condenser Flag, =1 if iteration result is good
        self.IC = 1  # Condenser iteration counter
        self.ITMAXC = 100  # Condenser max. number of iterations

        self.DUTYR = 0.5   # duty ratio

        # fraction superheat area
        self.FSUPC = 0.1  # unit (%) in python only
        self.FSUPE = 0  # unit (%) in python only
        
        # fraction subcool area
        self.FSUBC = 0  # unit (%) in python only
        # -----------------------

        self.__solveCycle()

    # basic Solver
    def __solveCycle(self):
        # GUESS A DEW POINT TEMPERATURE AT THE EVAPORATOR EXIT
        # -----------------------------
        if self.ISPEC == 1:  # Evap superheat:
            self.T[15] = self.TS3 - (self.DTSUPE + 2.0)
            self.P[15] = self.objCP.Property('P', X=1, T=self.T[15])  # Pa
            self.V[15] = self.objCP.Property('V', X=1, T=self.T[15])  # m3/kg

            # self.TE[1] = self.T[15] + self.DTSUPE
            self.TE[1] = self.TS3 - 2
            
            self.T[7] = self.TE[1]  # 7 - OUTLET FROM FRESH FOOD EVAPORATOR
            self.P[7] = self.P[15]

        elif self.ISPEC == 2:  # Interchanger superheat specified
            self.T[15] = self.TS3 - 2.0
            self.V[15] = self.objCP.Property('V', X=1, T=self.T[15])  # m3/kg

            self.T[13] = self.T[15] + self.DTSUPI  # Interchanger DT

            if self.T[13] > self.TC[1]:
                self.T[13] = self.TC[1] - 5.0
                self.T[15] = self.T[13] - self.DTSUPI  # Interchanger DT

            self.P[15] = self.objCP.Property('P', X=1, T=self.T[15])  # Pa

            self.TE[1] = self.T[15]
            self.T[7] = self.TE[1]
            self.P[7] = self.P[15]

        elif self.ISPEC == 3:  # Evap exit quality
            self.T[15] = self.TS3 - 2.0
            self.V[15] = self.objCP.Property('V', X=self.XEXITE
                                             , T=self.T[15])  # m3/kg
            self.P[15] = self.objCP.Property('P', X=self.XEXITE
                                             , T=self.T[15])  # Pa

            self.trace.dr_omar("Ayman to chk source code")
            # Dr omar
            # not logic, TBUB15 will be the same as T[15]
            # TBUB15 = self.objCP.Property('T', X=0, P=self.P[15])  # K
            #  ==> see correction above
            # XEXITE is quality !!!
            # self.TE[1] = self.T[15] - \
            # (self.T[15]- TBUB15 )*(1.0 - self.XEXITE)
            self.TE[1] = self.T[15]  # add by Ayman same as prev. statment

            self.T[7] = self.TE[1]
            self.P[7] = self.P[15]
        # -----------------------------

        # Condenser itaration loop
        while self.IC <= self.ITMAXC and self.LCCON:
            self.trace.cond_ic()  # self.IC

            # ICAB - flag to represent presence of cabinet loads in input

            self.trace.dr_omar("adjlod is not tested nor used")  # Dr. Omar
            #  ICAB Flag to represent presence of cabinet loads in input, 0 =No
            if self.dt.ICAB == 1:
                [self.TS3, self.TS5] = self.adjlod(self.dt,    # self.MREF / 3600,
                                                   # self.dt.ATOTEI[self.lng_item],
                                                   self.dt.ICYCL,
                                                   self.IC,
                                                   self.TS3,
                                                   self.TS5,
                                                   self.dt.FROSTF,
                                                   self.dt.FROSTF,
                                                   self.dt.IDFRST
                                                   )

            self.T[4] = self.TC[self.JC]  # 4 - CONDENSER OUTLET

            # find condenser pressure for current guess of TC
            TBUB4 = self.TC[self.JC] + self.DTSUBC
            self.P[4] = self.objCP.Property('P', X=0, T=TBUB4)  # Pa

            # determine the specific volume of the liquid
            if self.DTSUBC > 0:
                # Dr. Omar to approve
                # Ayman modification, in case DTSUPI = 0
                # the given point came to wet area.
                # check if in wet area, return sat. liquid or sat. vap.
                # self.V[4] = self.objCP.Property('V', T=self.TC[self.JC]
                # , P=self.P[4])  # m3/kg

                self.V[4] = self.coolutil.getProp(prp='V', T=self.TC[self.JC],
                                                  P=self.P[4], X=0)  # m3/kg

            else:
                self.V[4] = self.objCP.Property('V', X=0, T=TBUB4)  # m3/kg

            # condenser dew point
            self.P[3] = self.P[4] + (1 - self.FSUPC) * self.DPC  # DPC in Pa

            # check if CRITICAL TEMPERATURE EXCEEDED IN CONDENSER
            # and display error in this case

            # ENTHALPY AT STATE 4 (CONDENSER OUTLET)
            # [H[4], CV, CP,VSND] = self.hcvcps(1, TC[JC],V[4], X)
            self.H[4] = self.objCP.Property('H', V=self.V[4]
                                            , T=self.TC[self.JC])  # j/kg

            # ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
            # CONDHT watt /self.MREF kg/hr /3600  = j/kg, DUTYR unit less
            # self.H[16] = self.H[4] \
            #    - self.dt.CONDHT[self.NCYC] / self.MREF / self.DUTYR

            self.H[16] = self.H[4] -\
                         self.dt.CONDHT[self.NCYC] / (self.MREF / 3600) / self.DUTYR

            self.P[16] = self.P[4]

            # 16 - LIQUID LINE STATE AFTER HEAT LOSS TO CABINET AND MULLION
            self.T[16] = self.objCP.Property('T', P=self.P[16],
                                             H=self.H[16])  # K
            # if (VL[16] == 0.0):
            #     VL[16] = V[4]
            # V[16] = VL[16]

            self.V[16] = self.objCP.Property('V', P=self.P[16]
                                             , H=self.H[16])  # m3/kg
            # if (self.V[16] == 0.0):
            #    # self.V[16] = self.V[4]

            # Evaporator iteration
            self.evapIteration()
            # print("Number of evapIteration iterations self.IE",self.IE)
            
            self.evap_out7()   # 7 - OUTLET FROM FRESH FOOD EVAPORATOR

            #  ---------
            self.T[6] = self.objCP.Property('T', P=self.P[6]
                                            , H=self.H[6])  # K
            if self.ISPEC != 2:
                self.T[13] = self.objCP.Property('T', P=self.P[13]
                                                 , H=self.H[13])  # K
                self.V[13] = self.objCP.Property('V', P=self.P[13]
                                                 , H=self.H[13])  # m3/kg
            # TE[1]=TE[JE] DONE ABOVE

            # --------------------------END OF NEW CODE (12/29/90)-------------
            #
            # FIND ENTROPY AT COMPRESSOR INLET AND COMPUTE CONDITIONS AT
            # COMPRESSOR OUTLET (TSPEC IS DEFINED INLET TEMP TO THE
            # COMPRESSOR (-1 MEANS NO TEMPERATURE CHANGE)

            self.P[1] = self.P[13]

            if self.TSPEC > 0.0:
                self.T[1] = self.TSPEC
                # Approve concept self.trace.dr_omar("Wet region issue")
                # Ayman modification, in case DTSUPI = 0
                # the given point came to wet area.
                # check if in wet area, return sat. liquid or sat. vap.
                # self.V[1] = self.objCP.Property('V', T=self.T[1]
                #                                    , P=self.P[1])  # m3/kg
                # self.H[1] = self.objCP.Property('H', T=self.T[1]
                #                                    , P=self.P[1])  # m3/kg

                self.V[1] = self.coolutil.getProp(prp='V', P=self.P[1],
                                                  T=self.T[1])  # m3/kg

                self.H[1] = self.coolutil.getProp(P=self.P[1],
                                                  T=self.T[1])  # j/kg
            else:
                self.V[1] = self.V[13]
                self.T[1] = self.T[13]
                self.H[1] = self.H[13]

            self.S[1] = self.objCP.Property('S', X=1
                                            , T=self.T[1])  # j/kg K

            self.P[2] = self.P[3] + self.FSUPC * self.DPC

            # ........Compreesor Class..................
            self.trace.comp_ins()  # compress_inputs

            # only one type
            self.dicRest = self.objComp.comp_balance(PSUCT=self.P[1],   # pa
                                                     PDISC=self.P[2],   # pa
                                                     TSUCT=self.T[1],   # K
                                                     MREF=self.MREF,    # kg/hr
                                                     VSUCT=self.V[1]    # m3/kg
                                                     )

            self.trace.comp_out()

            self.trace.dr_omar("Is it Iso. T for exit")  # Dr Omar
            # self.T[1] = dicRest['TSP']

            # Dischare Temp K
            self.T[2] = self.dicRest['TDISC']   # K

            # Dischare Enthalpy    j/kg
            self.H[2] = self.HOUT = self.dicRest['HOUT']   # j/kg

            # compressor shell loss normalized to power input 
            self.QCAN = self.dicRest['QCAN']   # unitless

            # Suction sp.volume m3/kg
            VSUC = self.dicRest['VSUC']

            # Dischare sp.volume m3/kg
            VV2 = self.dicRest['VV2']

            # Cp/Cv value
            GAMA = self.dicRest['GAMA']

            # Compressor Efficiency   %
            ETAS = self.dicRest['ETAC']

            # add by Ayman 
            self.trace.dr_omar("Saved MRLOD to be used later")
            self.MROLD = self.MREF

            # Refrigerant Mass Flow Rate  kg/hr
            self.MREF = self.dicRest['MREF']
            # ........End Compreesor Class

            # not used self.FLOW2 = self.FLWREF * self.MREF / self.MREFSV

            self.trace.dr_omar("Python why calculate T[2]")  # Dr Omar
            # CONDITIONS OF GAS LEAVING COMPRESSOR SHELL

            self.H[2] = self.objCP.Property('H', P=self.P[2], V=VV2)  # j/kg
            self.T[2] = self.objCP.Property('T', P=self.P[2], V=VV2)  # K
            self.V[2] = self.objCP.Property('V', P=self.P[2]
                                            , H=self.H[2])  # m3/kg

            # ENTROPY OF GAS LEAVING COMPRESSOR  - j/kg K
            self.S[2] = self.objCP.Property('S', T=self.T[2], V=self.V[2])  

            if self.ICONC == 1:  # flag in Condenser class if iteration is good
                self.LCCON = False  # exit loop
            else:
                self.condenser_calc()

            self.IC += 1  # do another trail

            if self.dt.INCTRL in [0, 3]:
                if self.IC <= 4:
                    self.LCCON = True
                    self.ICONC = 0

            if self.dt.INCTRL in [1, 2, 4]:
                if self.IC <= 10:
                    self.LCCON = True
                    self.ICONC = 0

        self.trace.cycle_out()

        return

    # condenser calculations
    def condenser_calc(self):
        # CALCULATE CONDENSER HEAT EXCHANGE
        # DETERMINE THE DEW POINT CONDITIONS
        self.T[3] = self.objCP.Property('T', P=self.P[3], X=1)  # K
        self.H[3] = self.objCP.Property('H', P=self.P[3], X=1)  # j/kg

        # DETERMINE BUBBLE POINT CONDITIONS
        # ASSUME A LINEAR PRESSURE DROP THROUGHOUT THE CONDENSER

        self.P[11] = PBUB = self.P[4] + self.DPC * self.FSUBC

        self.T[11] = TBUB = self.objCP.Property('T', P=PBUB, X=0)  # K
        self.H[11] = HBUB = self.objCP.Property('H', P=PBUB, X=0)  # j/kg
        CPRLIQ = self.objCP.Property('CP', P=PBUB, X=0)  # j/kg K

        # DETERMINE CONDITIONS ENTERING THE CONDENSER
        self.trace.dr_omar("unit for MREF")  # Dr Omar
        # HDROP = self.dt.CONDVP[self.NCYC] / self.MREF / self.dt.DUTYC

        HDROP = self.dt.CONDVP[self.NCYC] / (self.MREF / 3600) / self.dt.DUTYC

        self.P[14] = self.P[2]
        HINCND = self.H[2] - HDROP  # j/kg

        self.H[14] = HINCND  # j/kg
        self.T[14] = self.objCP.Property('T', H=self.H[14]
                                         , P=self.P[2])  # K

        # ........Conderser Class..................
        if self.ICOND == 0:
            dicRest = self.objCond.cond_balance(T14=self.T[14],   # K
                                                H14=self.H[14],   # pa
                                                T3=self.T[3],     # K
                                                H3=self.H[3],     # j/kg
                                                T5=self.T[5],     # K
                                                T8=self.T[8],     # K
                                                T9=self.T[9],     # K
                                                T12=self.T[12],   # K
                                                TBUB=TBUB,        # K
                                                HBUB=HBUB,        # j/kg
                                                MREF=self.MREF,   # kg/hr
                                                CPRLIQ=CPRLIQ     # j/kg K
                                                )

        elif self.ICOND == 1:
            dicRest = self.objCond.cond_balance(T14=self.T[14],   # K
                                                H14=self.H[14],   # j/kg
                                                T3=self.T[3],     # K
                                                H3=self.H[3],     # j/kg
                                                TBUB=TBUB,        # K
                                                HBUB=HBUB,        # j/kg
                                                MREF=self.MREF,   # kg/hr
                                                CPRLIQ=CPRLIQ,    # j/kg K
                                                PBUB=PBUB,        # pa
                                                P4=self.P[4]      # pa
                                                )

        else:  # self.ICOND == 2:
            dicRest = self.objCond.cond_balance(T14=self.T[14],  # K
                                                H14=self.H[14],  # j/kg
                                                T3=self.T[3],    # K
                                                H3=self.H[3],    # j/kg
                                                TBUB=TBUB,       # K
                                                HBUB=HBUB,       # j/kg
                                                MREF=self.MREF,  # kg/hr
                                                CPRLIQ=CPRLIQ,   # j/kg K
                                                PBUB=PBUB,       # pa
                                                P4=self.P[4]     # pa
                                                )

        # == Output of condenser class --------
        QDSC = dicRest['QDSC']       # watt
        QTPC = dicRest['QTPC']       # watt
        QSCC = dicRest['QSCC']       # watt
        QTOTC = dicRest['QTOTC']     # watt

        FSUP = dicRest['FSUP']   # unitless
        FSUB = dicRest['FSUB']   # unitless

        [Q_CND_FF,         # watt
         Q_CND_FZ,         # watt
         Q_HXS_FF,         # watt
         Q_HXS_FZ,         # watt
         self.UACOND,      # watt/K
         UDSC,             # watt/m2 K
         USCC,             # watt/m2 K
         UTPC              # watt/m2 K
         ] = self.objCond.getExtarOutputs()

        # Condenser Heat Fresh Food,                 Q_CND_FF
        # Condenser Heat Freezer,                    Q_CND_FZ
        # Heat Exchanger Fresh Food                  Q_HXS_FF
        # Heat Exchanger Freezer                     Q_HXS_FZ
        # Condenser                                  UACOND
        # Desuperheating Heat Transfer Conductance,  UDSC
        # Subcooling Heat Transfer Conductance,      USCC
        # Two-Phase Heat Transfer Conductance,       UTPC

        # - cond method
        self.trace.dr_omar("Use of MROLD")
        lstRest = self.objCond.cond(T4=self.T[4],       # K
                                    H4=self.H[4],       # j/kg
                                    H14=self.H[4],      # j/kg
                                    TC=self.TC,         # K
                                    JC=self.JC,         # number unit less
                                    QCONDS=QDSC,        # watt
                                    QCONDC=QTPC,        # watt
                                    QSCC=QSCC,          # watt
                                    MROLD=self.MROLD,   # kg/hr
                                    MREF=self.MREF,     # kg/hr
                                    UACOND=self.UACOND  # watt/K
                                    )

        # output of cond method
        self.TS2 = lstRest[0]    # K
        self.TC = lstRest[1]     # K
        self.JC = lstRest[2]     # unitless
        self.ICONC = lstRest[3]  # unitless

        # ........End Conderser Class .............

        # ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
        # self.H[16] = self.H[4] \
        #            - self.dt.CONDHT[self.NCYC] / self.MREF / self.DUTYR

        self.H[16] = self.H[4] -\
                     self.dt.CONDHT[self.NCYC] / (self.MREF / 3600) / self.DUTYR

        self.P[16] = self.P[4]
        self.T[16] = self.objCP.Property('T', T=self.H[16],
                                         P=self.P[16])  # K

        # calculate the average effectiveness of the heat exchanger
        # calculate the heat transfer if the refrigerant left aT TS1
        #
        # determine the specific volume of the liquid
        #
        if self.TS1 < self.T[4]:
            # Approve concept self.trace.dr_omar("Wet region issue")
            # Ayman modification, in case DTSUPI = 0
            # the given point came to wet area.
            # check if in wet area, return sat. liquid or sat. vap.
            # VS1 = self.objCP.Property('V', T=self.TS1
            #                             # , P=self.P[4])  # m3/kg
            VS1 = self.coolutil.getProp(prp='V', P=self.P[4],
                                        T=self.TS1, X=0)  # m3/kg
        else:
            VS1 = self.V[4]

        HS1 = self.objCP.Property('T', T=self.TS1, V=VS1)  # j/kg
        QRMAX = self.MREF * (self.H[14] - HS1) / 3600  # watt

        # CALCULATE THE HEAT TRANSFER if THE AIR LEFT AT T[14]

        QAMAX = self.dt.CFMC * (self.T[14] - self.TS1)  # watt/K * K = watt
        QMAXC = QAMAX

        if QRMAX < QAMAX:
            QMAXC = QRMAX

        self.ETAC = QTOTC / QMAXC   # watt/watt
        # -----------------------------------------------------------
        #  END OF Conderser ITERATION loop
        # -----------------------------------------------------------

    # Evaporator iteration
    def evapIteration(self):
        # prepare for inner loop
        self.JE = 1
        self.LECON = True
        ITMAXE = 40

        TEMIN = 210.0  # K

        #  Evap itaration loop
        #  enter iteration for evaporator outlet temperature
        self.IE = 1
        while (self.IE <= ITMAXE) and self.LECON:
            self.I_ERROR_INTER = 0

            # -----------------------------
            if self.ISPEC == 1:  # Evap superheat:
                self.TE[self.JE] = self.T[15] + self.DTSUPE

                self.P[15] = self.objCP.Property('P', X=1, T=self.T[15])  # pa

                self.P[7] = self.P[15]

            elif self.ISPEC == 2:  # Interchanger superheat specified
                self.P[15] = self.objCP.Property('P', X=1, T=self.T[15])  # pa

                self.P[13] = self.P[15]
                self.T[13] = self.T[15] + self.DTSUPI
                self.trace.dr_omar("Wet region issue")  # Dr. Omar to approve
                # Ayman modification, in case DTSUPI = 0
                # the given point came to wet area.
                # check if in wet area, return sat. liquid or sat. vap.

                # self.V[13] = self.objCP.Property('V', P=self.P[13]
                #                                    # , T=self.T[13])  # m3/kg
                # self.H[13] = self.objCP.Property('H', P=self.P[13]
                #                                    # , T=self.T[13])  # j/kg

                self.V[13] = self.coolutil.getProp(prp='V', P=self.P[13],
                                                   T=self.T[13])  # m3/kg

                self.H[13] = self.coolutil.getProp(P=self.P[13],
                                                   T=self.T[13])  # j/kg

                self.P[7] = self.P[15]
                self.TE[self.JE] = self.T[7]

                if self.T[13] >= self.T[16]:
                    self.LECON = False
                    self.I_ERROR_INTER = 1
                    continue

            elif self.ISPEC == 3:  # Evap exit quality
                self.P[15] = self.objCP.Property('P', X=1, T=self.T[15])  # pa

                self.P[7] = self.P[15]
            # -----------------------------

            # TBUB15 = self.objCP.Property('T', X=0, P=self.P[15])  # K
            # VBUB15 = self.objCP.Property('V', X=0, P=self.P[15])  # m3/kg

            # determine the bubble and dew point enthalpies
            # self.H[15] = self.objCP.Property('H', P=self.P[15]
            #                                    # , V=self.V[15]) # j/kg

            self.H[15] = self.objCP.Property('H', P=self.P[15],
                                             X=1)  # j/kg
            self.HBUB15 = self.objCP.Property('H', X=0, P=self.P[15])  # j/kg

            self.evap_out7()   # 7 - OUTLET FROM FRESH FOOD EVAPORATOR
            self.calc_lowevap()

            # Calculate fresh food section heat exchange
            self.evap_dew12()   # 12 - FRESH FOOD EVAPORATOR DEW POINT

            QFF = 0  # prevent error if IC=1
            QFZ = 0

            if self.IC != 1:  # skip first trail to calc. some values later
                [QFF, QFZ, DUTYR] = \
                    self.dutfnd(self.dt, self.dt.ICAB,
                                self.dt.IRFTYP,
                                self.dt.ICYCL,
                                self.NCYC,
                                self.QFRSH,  # from Evaporator class
                                self.QFREZ,
                                self.dt.FROSTF,
                                self.dt.FROSTZ,
                                self.TS3,
                                self.TS5,
                                self.T,
                                self.dt.IDFRST
                                )

            # CALCULATE FRESH FOOD EVAPORATOR HEAT TRANSFER.
            # TEST FOR STANDARD DESIGN.

            self.trace.dr_omar("General review for this point")
            if self.dt.IRFTYP <= 3:

                if (self.dt.ICYCL == 1 and
                        self.dt.ICAB != 0 and
                        self.IFRSH != 0):

                    if self.dt.IC == 1:
                        TIN = 0.15 * self.TFF + 0.85 * self.TFZ
                        self.FF_FRACT = 0.0  # in Python only

                    else:
                        # self.dt.FANE, self.dt.DFSTCYC, FZCYC all in watt
                        # self.dt.CAPE = self.QFRSH / 1.0548 - 3.413 * \
                        #    # self.dt.FANE
                        #    # - 3.413 * (self.dt.DFSTCYC + self.dt.FZCYC)

                        self.dt.CAPE = self.QFRSH - self.dt.FANE \
                                       - (self.dt.DFSTCYC + self.dt.FZCYC)

                        # Ayamn
                        # CFMA = self.CFME / (1.08 * 1.8961)
                        CFMA = self.dt.CFME  # watt/K
                        # QFM = QFF + 3.413 * self.dt.DUTYC * self.dt.FFCYC
                        QFM = QFF + self.dt.DUTYC * self.dt.FFCYC

                        [TIN, self.FF_FRACT] = self.mixair(
                            self.dt.CAPE, QFM, QFZ, self.TFF, self.TFZ, CFMA)

                    self.trace.dr_omar("Cancelled convert to F")  # Dr Omar to check
                    # self.TS3 = (TIN + 459.6) / 1.8
                    self.TS3 = TIN

            #  ........Evaporator Class..................
            # add evaporator class here

            # CPRVAP  j/kg K
            if self.IFRSH == 0:
                dicRest = self.objEvap.evap_balance(MREF=self.MREF,   # kg/hr
                                                    T5=self.T[5],     # K
                                                    H5=self.H[5],     # j/kg
                                                    T7=self.T[7],     # K
                                                    TDEW=self.TDEW,   # K
                                                    CPRVAP=self.CPRVAP   # j/kg K
                                                    )

            elif self.IFRSH == 1:
                dicRest = self.objEvap.evap_balance(MREF=self.MREF,   # kg/hr
                                                    T5=self.T[5],     # K
                                                    H5=self.H[5],     # K
                                                    TDEW=self.TDEW,   # K
                                                    CPRVAP=self.CPRVAP,   # j/kg K
                                                    P5=self.P[5],      # pa
                                                    P7=self.P[7]       # pa
                                                    )

            else:   #  self.IFRSH == 2:
                dicRest = self.objEvap.evap_balance(MREF=self.MREF,   # kg/hr
                                                    T5=self.T[5],     # K
                                                    H5=self.H[5],     # j/kg
                                                    TDEW=self.TDEW,   # K
                                                    CPRVAP=self.CPRVAP,   # j/kg K
                                                    P5=self.P[5],     # pa
                                                    P7=self.P[7]      # pa
                                                    )

            # ... Output ....
            self.QFRSH = dicRest['QFRSH']       # watt
            self.UAFF = dicRest['UAFF']         # watt/K
            self.FSUPE = dicRest['FSUPE']       # unitless
            # ..........................................

            # Superheating fraction
            # FSHOLD = FSUPE # useless code
            # FSUPE = (FSHOLD + FSUPE) / 2.0

            # if (FSUPE > 1.05 * FSHOLD):
            #    # FSUPE = 1.05 * FSHOLD

            # if (FSUPE < 0.95 * FSHOLD):
            #    # FSUPE = 0.95 * FSHOLD
            # # --- end of useless code block

            # fresh food section evaporator
            self.trace.frsh_ins()

            dicRest = self.objEvap.frsh(T5=self.T[5],               # K
                                        H5=self.H[5],               # j/kg
                                        H7=self.H[7],               # j/kg
                                        TS3=self.TS3,               # K
                                        TE=self.TE,                 # K
                                        JE=self.JE,                 # none
                                        QFRSH=self.QFRSH,           # watt
                                        MREF=self.MREF,             # kg/kg
                                        UAFF=self.UAFF              # watt/m2 K
                                        )

            self.TE = dicRest['TE']          # K
            self.JE = dicRest['JE']
            self.TS4 = dicRest['TS4']        # K
            ICONE = dicRest['ICONE']

            self.trace.frsh_out(ICONE)
            # ---------------------------ADDED NEW CODE (12/29/90)---------
            self.T[15] = self.T[15] + self.TE[2] - self.T[7]

            # calculate the average effectiveness of the ff evaporator
            # calculate the heat transfer if the refrigerant left at TS3

            # Approve concept self.trace.dr_omar("Wet region issue")
            # Ayman modification, in case DTSUPI = 0
            # the given point came to wet area.
            # check if in wet area, return sat. liquid or sat. vap.

            # HS3 = self.objCP.Property('H', P=self.P[7], T=self.TS3)  # j/kg            

            HS3 = self.coolutil.getProp(P=self.P[7],
                                        T=self.TS3)  # m3/kg

            QRMAX = self.MREF * (HS3 - self.H[5]) / 3600  # kg/hr/3600 j/kg = watt

            # Calculate the heat transfer if the air left at T[5]
            # CFME watt/K see common in CycleType
            self.trace.dr_omar("Approved")  # Dr Omar to check
            # CFME watt/K -->

            QAMAX = self.dt.CFME * (self.TS3 - self.T[5])  # watt
            QMAXE = QAMAX  # watt

            if QRMAX < QAMAX:
                QMAXE = QRMAX

            ETAE = self.QFRSH / QMAXE  # QMAXE= watt

            # if (ICONE == 1):
            #    # LECON = False # Exit loop

            # if (self.TE[JE] <= self.TEMIN):
            #    # LECON = False # Exit loop

            self.trace.evap_ie(ICONE)

            if (ICONE == 1) or (self.TE[self.JE] <= TEMIN):
                self.LECON = False  # Exit loop

            self.IE += 1
        #  -----------------------------------------------------------
        #  END OF EVAPORATOR ITERATION
        #  -----------------------------------------------------------

        # Python add this statment which came after loop
        self.T[7] = self.TE[self.JE]
        self.TE[1] = self.TE[self.JE]

    def evap_dew12(self):   # 12 - FRESH FOOD EVAPORATOR DEW POINT
        # Calculate fresh food section heat exchange
        PDEWE = self.P[5] - (1.0 - self.FSUPE) * self.DPE

        if PDEWE > self.P[5]:
            PDEWE = self.P[5]

        self.TDEW = self.objCP.Property('T', P=PDEWE, X=1)  # K

        # Python POLDE is not used !!!
        POLDE = PDEWE
        if self.TDEW >= self.TS3:
            self.TDEW = self.TS3 - 1.0

        if self.T[5] >= self.TS3:
            self.T[5] = self.TS3 - 1.0

        HDEW = self.objCP.Property('H', X=1, P=PDEWE)  # j/kg
        self.CPRVAP = self.objCP.Property('CP', X=1, P=PDEWE)  # j/kg K

        # STATE 12 IS THE POINT AT WHICH THE DEW POINT IS REACHED IN
        # THE EVAPORATOR
        self.P[12] = PDEWE
        self.T[12] = self.TDEW
        #  self.V[12] = VDEW
        self.H[12] = HDEW

    def evap_out7(self):   # 7 - OUTLET FROM FRESH FOOD EVAPORATOR
        # determine the enthalpy at [7]
        if self.ISPEC == 1:  # Evap superheat:
            # Approve concept self.trace.dr_omar("Wet region issue")
            # Ayman modification, in case DTSUPI = 0
            # the given point came to wet area.
            # check if in wet area, return sat. liquid or sat. vap.
            # self.H[7] = self.objCP.Property('H', P=self.P[7]
            # , T=self.T[7])  # j/kg 

            self.H[7] = self.coolutil.getProp(P=self.P[7],
                                              T=self.T[7])  # j/kg

            #  VV[7] = V[7]
            self.T[7] = self.TE[self.JE]

        elif self.ISPEC == 2:  # Interchanger superheat specified
            [self.T[7], self.H[7], self.QINT] = \
                self.inter2(self.objCP,
                            self.P[16], self.T[16], self.H[16],
                            self.V[16], self.P[13], self.H[13],
                            self.T[15], self.H[15], self.V[15],
                            self.ETHX1
                            )

            print(".......? >>>>>check why re-calcuklate T[7]")
            self.T[7] = self.objCP.Property('T', H=self.H[7],
                                            P=self.P[7])  # K

            # V[7] = (1.0-XQ[7])*VL[7] + XQ[7]*VV[7]
            self.TE[self.JE] = self.T[7]

        elif self.ISPEC == 3:  # Evap exit quality
            # XQ[7] = Data.XEXITE
            # CALL ENTHAL(HBUB15,H[15],XQ[7],X,P[7],H[7])

            self.H[7] = self.enthal(self.objCP, self.HBUB15, self.H[15],
                                    self.XEXITE, self.P[7])

            self.T[7] = self.objCP.Property('T', H=self.H[7]
                                            , P=self.P[7])  # K

            # V[7] = (1.0-XQ[7])*VL[7] + XQ[7]*VV[7]
            self.T[7] = self.TE[self.JE]

        if self.ISPEC != 2:
            # by Dr. Omar
            # self.QINT = self.inter1(self.objCP, self.T[16],
            # self.T[7], self.P[7], self.ETHX1 )

            #  interchanger for subcooling condenser liquid
            #  used when the inlet states of both streams specified
            # CP7 = self.objCP.Property('CP', self.T=T[7], P=self.P[7]) j/kg K
            CP7 = self.coolutil.getProp(prp='CP', T=self.T[7],
                                        P=self.P[7])  # j/kg K

            self.QINT = self.ETHX1 * (self.T[16] - self.T[7]) * CP7
            
            self.H[13] = self.H[7] + self.QINT

    def calc_lowevap(self):
        # by Dr. Omar
        # self.H[6] = self.H[16] + self.QINT
        self.H[6] = self.H[16] - self.QINT
        
        self.P[6] = self.P[4]

        self.P[13] = self.P[7]

        self.T[6] = self.objCP.Property('T', P=self.P[6]
                                        , H=self.H[6])  # K

        # -----------------------------
        if self.ISPEC != 2:
            self.T[13] = self.objCP.Property('T', P=self.P[13]
                                             , H=self.H[13])  # K

            self.V[13] = self.objCP.Property('V', P=self.P[13]
                                             , H=self.H[13])  # m3/kg
        # -----------------------------

        # find conditions at evaporator inlet assuming isenthalpic expansion
        self.P[5] = self.P[13] + self.DPE   # pa

        self.trace.dr_omar("chk lowevp") 
        [self.H, self.P, self.T, self.TS6, self.QFREZ] = \
            self.lowevp(self.dt, self,
                        self.objCP, self.MREF, self.dt.ICYCL,
                        0,  # ICNTRL no value set in Fortran
                        self.H, self.P, self.T,
                        # , VL
                        # ,HL not clear its use
                        self.TS3, self.dt.TS5, self.dt.DPF,
                        self.ETHX2)
                        
        # self.H[5] = self.H[4] +self.H[7] - self.H[13] # by Ayman
        
    # -----------------------------
    def getSolution(self):
        objSolution = QData()

        self.calc_output()

        objCycleSlover = self
        return objCycleSlover

    def calc_output(self):
        # COMPUTE WORK, CAPACITY, COP, ETC.
        #
        # HOUT = HOUT / WMAVG
        # VSUC = VSUC / WMAVG

        def cyclos(I_VALVE, T_CYCLE):
            # I_VALVE  1 IF VALVE USED, 0 IF NOT
            # T_CYCLE  NUMBER OF CYCLES PER HOUR

            # TCYCLE = 1.0 / T_CYCLE  # Cycle time
            CORRECTION = 0.0

            if I_VALVE == 0:
                # CORRECTION = 1.0 - 0.010 / TCYCLE
                CORRECTION = 1.0 - 0.010 * T_CYCLE

            if I_VALVE == 1:  # Use microvalve
                # CORRECTION = 1.0 + 0.015 / TCYCLE
                CORRECTION = 1.0 + 0.015 * T_CYCLE

            return CORRECTION

        self.W = (self.HOUT - self.H[1]) / (1.0 - self.QCAN)

        self.QE = self.H[7] - self.H[5]
        self.QC = self.H[14] - self.H[4]
        self.QZ = self.H[9] - self.H[8]

        self.COPR = (self.QE + self.QZ) / self.W

        TH = self.TS1
        TL1 = self.TS3
        TL2 = self.TS5

        DENOM = TH * (self.QE * (1. / TL1 - 1. / TH) +
                      self.QZ * (1. / TL2 - 1. / TH))

        self.COPI = (self.QE + self.QZ) / DENOM

        self.PR = self.P[2] / self.P[1]
        self.TSUPC = self.T[2] - self.T[3]

        # begin with heat transfer fluid temperatures
        self.TS = [0.0] * (16 + 1)  # Temp in C

        #  self.T[4] = self.TC[self.JC] - CycleSolver.K_C_DEG  # deg-c
        # TGC = self.T[3] - self.T[4]

        self.TS[1] = 0.0
        self.TS[4] = self.TS1  # deg-c
        self.TS[5] = self.TS4  # deg-c
        self.TS[6] = 0.0
        self.TS[7] = self.TS3  # deg-c
        self.TS[8] = self.TS6  # deg-c
        self.TS[9] = self.TS5  # deg-c
        self.TS[10] = 0.0
        self.TS[14] = self.TS2  # deg-c
        self.TS[16] = 0.0

        # ==================================
        # SPECIFIC VOLUMES
        # self.V[5] = min(
        #    # 1.0, 1.0 - self.XQ[5]) * self.VL[5] + max(
        #    # 0.0, self.XQ[5]) * self.VV[5]
        # self.V[8] = min(
        #    # 1.0, 1.0 - self.XQ[8]) * self.VL[8] + max(
        #    # 0.0, self.XQ[8]) * self.VV[8]
        # self.V[9] = min(
        #    # 1.0, 1.0 - self.XQ[9]) * self.VL[9] + max(
        #    # 0.0, self.XQ[9]) * self.VV[9]
        # self.V[10] = min(
        #    # 1.0, 1.0 - self.XQ[10]) * self.VL[10] + max(
        #    # 0.0, self.XQ[10]) * self.VV[10]

        # ==================================
        # ENTROPIES AROUND THE CYCLE
        # for J in range(3, 16 + 1):
        rec_quality = [0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0]
        for J in range(1, 16 + 1):
            # if (J != 5):
            #    # self.S[J] = self.entrop(self.T[J], self.V[J], self.X)
            X = rec_quality[J]
            quality = self.coolutil.get_coolQuality(self.H[J],
                                                    self.P[J],
                                                    self.T[J])
            if quality == -1:
                                                  
                self.S[J] = self.coolutil.getProp(prp='S', T=self.T[J],
                                                  P=self.P[J], X=X)  # j/kg K
                self.V[J] = self.coolutil.getProp(prp='V', T=self.T[J],
                                                  P=self.P[J], X=X)  # m3/kg

            else:
                # j/kg K
                self.S[J] = self.objCP.Property('S', T=self.T[J], X=quality)
                # m3/kg
                self.V[J] = self.objCP.Property('V', T=self.T[J], X=quality)

        # SL5 = self.entrop(
        # self.T[5],
        # self.VL[5],
        # self.getArr2dCol(
        # self.XL,
        # 5))  # XL[1][5]

        # SV5 = self.entrop(
        # self.T[5],
        # self.VV[5],
        # self.getArr2dCol(
        # self.XV,
        # 5))  # XV[1][5]

        # self.S[5] = min(1.0, 1.0 - self.XQ[5]) * SL5 \
        # + max(0.0, self.XQ[5]) * SV5

        # SL8 = self.entrop(
        # self.T[8],
        # self.VL[8],
        # self.getArr2dCol(
        # self.XL,
        # 8))  # XL[1][8]

        # SV8 = self.entrop(
        # self.T[8],
        # self.VV[8],
        # self.getArr2dCol(
        # self.XV,
        # 8))  # XV[1][8]

        # self.S[8] = min(1.0, 1.0 - self.XQ[8]) * SL8 \
        # + max(0.0, self.XQ[8]) * SV8

        # SL9 = self.entrop(
        # self.T[9],
        # self.VL[9],
        # self.getArr2dCol(
        # self.XL,
        # 9))  # XL[1][9]

        # SV9 = self.entrop(
        # self.T[9],
        # self.VV[9],
        # self.getArr2dCol(
        # self.XV,
        # 9))  # XV[1][9]

        # self.S[9] = min(1.0, 1.0 - self.XQ[9]) * SL9 \
        # + max(0.0,Cycle.obj_parameter.XQ[9]) * SV9

        # correct cop dur to cycling losses
        #
        if self.dt.I_CYCLE == 0:
            self.CORR_COP = 1.0
        else:
            self.TENV = self.dt.TROOM
            self.TMID_COND = (self.T[3] + self.T[11]) / 2.0
            self.TMID_EVAP = (self.T[8] + self.T[9]) / 2.0

            self.CORR_COP = cyclos(self.dt.I_VALVE, self.dt.T_CYCLE)
