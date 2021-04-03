# Python import


# User import
# from CycleUtils import CycleUtils
from CoolPrp import *

from Compressor import *
from Evaporator import *
from Condenser import *

from Adjlod import *

from ErrorException import ErrorException
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Create Class object cycle paramters
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class CycleSolver:
    def __init__(self, objCP):
        self.objCP = objCP
    
    #-- setup basic objects
    def setupEvap(self, IFRSH):
        self.IFRSH = IFRSH
        objEvaporator = Evaporator()
        self.objEvap = objEvaporator.getObject(objCP=self.objCP, IFRSH=IFRSH)
    
    def setupCond(self, ICOND):
        self.ICOND = ICOND
        objCondenser = Condenser()
        self.objCond = objCondenser.getObject(objCP=self.objCP, ICOND=ICOND)
    
    def setupComp(self, ICOMP, TAMB, FRACT_SPEED, strFileName):
        self.ICOMP = ICOMP
        self.objComp = Compressor(objCP=self.objCP,
                                TAMB=TAMB,
                                ICOMP=ICOMP, 
                                FRACT_SPEED=FRACT_SPEED,
                                strFileName=strFileName)
       
    #-- send basic configration
    def paraEvap(self, ATOTE, CFME, TS3, N_EVAP,
                      USUPE, UTPE, TROOM, FZTEMP, 
                      UA_FF, Q_HXS_FF, IWALL_FF, NUM_ZONE, IRFTYP):
                      
        self.objEvap.setParamters(ATOTE = ATOTE,
                            CFME = CFME,
                            TS3=TS3,
                            N_EVAP=N_EVAP,
                            USUPE=USUPE,
                            UTPE=UTPE,
                            TROOM=TROOM,
                            FZTEMP=FZTEMP,
                            UA_FF=UA_FF,
                            Q_HXS_FF=Q_HXS_FF,
                            IWALL_FF=IWALL_FF,
                            NUM_ZONE=NUM_ZONE,
                            IRFTYP=IRFTYP
                            )

    def paraCond(self, ATOTC, CFMC, TS1, TS3, TS5, DTSUBC, N_COND,
                      USCC, UTPC, UDSC,
                      UA_FF_CND, UA_FZ_CND ,UA_FF_HXS  ,UA_FZ_HXS):
        
        self.DTSUBC = DTSUBC # used in loop, in python only
 
        self.objCond.setParamters(ATOTC = ATOTC,
                    UA_FF_CND = UA_FF_CND,
                    UA_FZ_CND = UA_FZ_CND,
                    UA_FF_HXS = UA_FF_HXS,
                    UA_FZ_HXS = UA_FZ_HXS,
                    CFMC = CFMC,
                    DTSUBC = DTSUBC,
                    N_COND = N_COND,
                    TS1=TS1,
                    TS3=TS3,
                    TS5=TS5,
                    USCC=USCC,
                    UTPC=UTPC,
                    UDSC=UDSC
                    )

    def paraComp(self): # NA
        pass
    
    def paraCycle(self
                  , TS1, TS3, TS5
                  , DPC, DPE, DPF
                  , ETHX1, ETHX2
                  , FROSTF, FROSTZ
                  , QHILO, QCAN
                  , ICAB, ICYCL, ICYCLS, IR, NC, NCYC
                  , IRFYPE, IDFRST
                  , MEFF, DISPLC
                  , MREF, DTSUPE, DTSUPI
                  ):

        # TS1 - HEAT TRANSFER FLUID (HTF) TEMPERATURE ENTERING CONDENSER
        # TS3 - HTF TEMPERATURE ENTERING FRESH FOOD EVAPORATOR
        # TS5 - HTF TEMPERATURE ENTERING FREEZER EVAPORATOR
        self.TS1=TS1
        self.TS3 = TS3
        self.TS5 = TS5

        # DPC - PRESSURE DROP THROUGH CONDENSER
        # DPE - PRESSURE DROP THROUGH FRESH FOOD EVAPORATOR
        # DPF - PRESSURE DROP THROUGH FREEZER EVAPORATOR
        self.DPC = DPC
        self.DPE = DPE
        self.DPF = DPF

        # ETHX1 - EFFECTIVENESS OF HIGH TEMP INTERCHANGER
        # ETHX2 - EFFECTIVENESS OF LOW  TEMP INTERCHANGER
        self.ETHX1 = ETHX1
        self.ETHX2 = ETHX2
        
        # FROSTF later
        # FROSTZ later
        self.FROSTF = FROSTF
        self.FROSTZ = FROSTZ

        # QHILO - NORMALIZED HEAT LOSS FROM DISCHANGE LINE INSIDE
        #						THE COMPRESSOR SHELL TO SUCTION GAS
        # QCAN - COMPRESSOR SHELL LOSS NORMALIZED TO POWER INPUT
        self.QHILO = QHILO
        self.QCAN = QCAN
        
        # ICAB - FLAG TO REPRESENT PRESENCE OF CABINET LOADS IN INPUT
        # ICYCL - CYCLE TYPE (1=STANDARD, 2=LORENZ, 3=DUAL LOOP, 4=DUAL EVAP)
        # ICYCLS later
        self.ICAB = ICAB
        self.ICYCL = ICYCL
        self.ICYCLS = ICYCLS
        
        # IR later
        # NC later
        # NCYC - NUMBER OF CALL TO CYCLE (1 OR 2 FOR DUAL LOOP)
        self.IR = IR
        self.NC = NC
        self.NCYC = NCYC
        
        # IRFYPE later
        # IDFRST later
        self.IRFYPE = IRFYPE
        self.IDFRST = IDFRST

        # MEFF - MECHANICAL EFFICIENCY
        # DISPLC - COMPRESSOR DISPLACEMENT (CU-IN)        
        self.MEFF = MEFF
        self.DISPLC = DISPLC
        
        # MREF kg/hr Refrigerant Mas Flow Rate
        self.MREF = MREF
        
        self.DTSUPE = DTSUPE
        self.DTSUPI = DTSUPI
        
    #-- Soving actions
    def solveCycle(self):
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

        # === del this block later ===============
        # not Requiired outside comp
        # if self.ICOMP==1:
            # [EFFC, CE] = self.objCompType.map(Data.obj_cdata.ICOMP,
                            # Data.obj_cdata.ICOOL,
                            # Data.obj_cdata.EER,
                            # Data.obj_cdata.SIZE,
                            # Data.obj_cdata.DISPL,
                            # Data.obj_cdata.SPEED)
        # -----------------------------------------------
        
        #	INITIAL GUESSES FOR TC AND TE
        #	ASSUME TEMP RISE OF COND IS 0.5 F PER LBM
        TE = [0, 0, 0]
        TC = [0, 0, 0]
        TC[1] = self.TS1 + self.MREF / 3.6
        
        # ----
        # GUESS A DEW POINT TEMPERATURE AT THE EVAPORATOR EXIT
        if self.ISPEC==1: # Exit superheater
            T15 = self.TS3 - (self.DTSUPE + 2.0)

            # [XL_Temp,
             # X,
             # P[15],
             # VL[15],
             # V[15],
             # LCRIT] = self.bublt(T[15],
                                 # XL_Temp,
                                 # X,
                                 # False)

            P15 = self.objCP.Property('P', X=1, T=T15)  # pas
            
            TE[1] = T15 + self.DTSUPE
            T7 = TE[1] # 7 - OUTLET FROM FRESH FOOD EVAPORATOR
            P7 = P15
        
        elif self.ISPEC==2: # IHX superheater
            T15 = self.TS3 - 2.0
            T13 = T15 + self.DTSUPI

            if (T13 > TC[1]):
                T13 = TC[1] - 5.0
                T15 = T13 - self.DTSUPI

                # [XL_Temp,
                 # X,
                 # P[15],
                 # VL[15],
                 # V[15],
                 # LCRIT] = self.bublt(T[15],
                                     # XL_Temp,
                                     # X,
                                     # False)
                                     
                P15 = self.objCP.Property('P', X=1, T=T15)  # pas
                TE[1] = T15
                T7 = T15 # TE[1]
                P7 = P15
        
        else # self.ISPEC==3: # Evap quality
            T15 = self.TS3 - 2.0

            # [XL_Temp,
             # X,
             # P[15],
             # VL[15],
             # V[15],
             # LCRIT] = self.bublt(T[15],
                                 # XL_Temp,
                                 # X,
                                 # False)
            P15 = self.objCP.Property('P', X=1, T=T15)  # pas
            
            # [X, XV15, TBUB15, VBUB15, VV15, LCRIT] = self.bublp(
                # P[15], X, XV15, True)
                
            #  note by aym in cycletype self.obj_data.XEXITE = self.obj_data.QUALTY[1]
            
            TBUB15 = self.objCP.Property('T', X=0, P=P15)  # K
            TE[1] = T15 - (T15 - TBUB15) * (1.0 - self.XEXITE) # fix later
            
            T7 = TE[1]
            P7 = P15

        # ----
        # steps for change MREF from kg/hr to kmole/hr was ignored
        # stepts was simplifed, all vars equal to self.MREF
        FLOW = FLOW2 = FLWREF = MREFSV = self.MREF
        
        # set outer loop data
        JC = 1
        LCCON = True
        LQUIT = False

        #	SET UP TEMPERATURES AND CABINET LOADS FOR INLET TEMPERATURE
        #	CALCULATION FOR EVAPORATOR OF A STANDARD DESIGN (TYPE 1)
        TFF = self.FFTEMP
        TFZ = self.FZTEMP
        
        ICONC = 0
        IC = 1
        ITMAX = 100
        
        #-- Outler loop
        while (IC <= ITMAXC and LCCON):
            if (self.ICAB == 1):
                [self.TS3, self.TS5] =
                    self.adjlod(self.ICYCL,
                                IC,
                                self.TS3,
                                self.TS5,
                                self.FROSTF,
                                self.FROSTF,
                                self.IDFRST)
                                
            T4 = TC[JC] # 4 - CONDENSER OUTLET
            
            #	find condenser pressure for current guess of TC
            TBUB4 = TC[JC] + self.DTSUBC

            # [X,
             # XV_Temp,
             # P[4],
             # VBUB4,
             # VV[4],
             # LCRIT] = self.bublt(TBUB4,
                                 # X,
                                 # XV_Temp,
                                 # True)
            
            P4 = self.objCP.Property('P', X=1, T=TBUB4)  # pas
            
            # determine the specific volume of the liquid
            V4 = self.objCP.Property('V', X=1, T=TBUB4)  # m3/kg
            
            # condenser dew point
            # check if CRITICAL TEMPERATURE EXCEEDED IN CONDENSER
            # and display error in this case
            
            # ENTHALPY AT STATE 4 (CONDENSER OUTLET)
            # [H[4], CV, CP,VSND] 
                # = self.hcvcps(1,
                         # TC[JC],
                         # V[4],
                         # X)
                         
            H4 = self.objCP.Property('H', P=P4, T=TC[JC])  # j/kg
            
            # ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
            H16 = H4 - CONDHT[NCYC] / self.MREF / DUTYR
            P16 = P4
            
            # [T[16],
             # XQ[16],
             # XL_Temp,
             # XV_Temp,
             # VL[16],
             # VV[16],
             # HL16,
             # HV16] = self.hpin(H[16],
                               # P[16],
                               # X)
            
            # 16 - LIQUID LINE STATE AFTER HEAT LOSS TO CABINET AND MULLION
            T16 = self.objCP.Property('T', P=P16, H=H16)  # K

            # if (VL[16] == 0.0):
                # VL[16] = V[4]

            # V[16] = VL[16]
            
            V16 = self.objCP.Property('V', P=P16, H=H16)  # K
            if (V16 == 0.0):
                V16 = V4
                
            # prepare for inner loop
            JE = 1
            LECON = True
            
            #	enter iteration for evaporator outlet temperature
            IE = 1
            while ( (IE <= ITMAXE) and LECON):
                I_ERROR_INTER = 0

                if self.ISPEC==1: # Exit superheater
                    TE[JE] = T15 + self.DTSUPE

                    # [XL_Temp,
                     # X,
                     # P[15],
                     # VL[15],
                     # V[15],
                     # LCRIT] = self.bublt(T[15],XL_Temp, X,   False)
                    
                    P15 = self.objCP.Property('P', X=0, T=T15)  # pas
                    P7 = P15

                elif self.ISPEC==2: # IHX superheater
                    P15 = self.objCP.Property('P', X=0, T=T15)  # pas
                    P13 = P15
                    T13 = T15 + self.DTSUPI

                    # [H[13], CV, CP, VS] = self.hcvcps(1, T[13], V[13], X)
                    H13 = self.objCP.Property('H', P=P13, T=T13)  # j/kg
                    
                    P7 = P15
                    TE[JE] = T7
                    
                    # 13 - SUPERHEATED GAS LEAVING THE HIGH TEMP INTERCHANGER
                    # 16 - LIQUID LINE STATE AFTER HEAT LOSS TO CABINET AND MULLION
                    if (T13 >= T16): 
                        LECON = False # exit loop
                        continue
                
                else # self.ISPEC==3: # Evap quality
                    P15 = self.objCP.Property('P', X=0, T=T15)  # pas
                    P7 = P15
            
                # to be checked === error here where is V15
                #	determine the bubble point at the evap exit pressure
                TBUB15 = self.objCP.Property('T', V=V15, P=P15)  # K
                
                #	determine the bubble and dew point enthalpies
                H15 = self.objCP.Property('H', V=V15, P=P15)  # j/kg
                HBUB15 = self.objCP.Property('H', X=1, T=TBUB15)  # j/kg
                
                #	determine the enthalpy at [7]
                if self.ISPEC==1: # Exit superheater
                    H7 = self.objCP.Property('H', P=P7, T=T7)  # j/kg
                    T7 = TE[JE]
                
                elif self.ISPEC==2: # IHX superheater
                    self.objData.QINT] = self.inter2(self.objData.X,
                                                      self.objData.P[16],
                                                      self.objData.T[16],
                                                      self.objData.H[16],
                                                      self.objData.V[16],
                                                      self.objData.P[13],
                                                      self.objData.H[13],
                                                      self.objData.T[15],
                                                      self.objData.H[15],
                                                      self.objData.V[15],
                                                      self.objData.ETHX1)                    
                    TE[JE] = T7
                
                else # self.ISPEC==3: # Evap quality
                    self.objData.H[7] = self.enthal(
                        self.objData.HBUB15,
                        self.objData.H[15],
                        self.objData.XQ[7],
                        self.objData.X,
                        self.objData.P[7])          
                    
                    V7 = self.objCP.Property('V', H=H7, P=P7)  # m3/kg
                    T7 = TE[JE]
                    
                if self.ISPEC!=2: # not IHX superheater
                    self.objData.QINT = self.inter1(
                        self.objData.X,
                        self.objData.P[16],
                        self.objData.T[16],
                        self.objData.H[16],
                        self.objData.V[16],
                        self.objData.P[7],
                        self.objData.T[7],
                        self.objData.H[7],
                        self.objData.V[7],
                        self.objData.ETHX1)
                        
                    self.objData.H[13] = self.objData.H[7] + self.objData.QINT 
                    
                H6 = H16 + QINT
                
                #	determine the remainder of the properties at [6] and [13]
                P6 = P4
                P13 = P7
                
                T6 = self.objCP.Property('T', H=H6, P=P6)  # K
                
                if self.ISPEC!=2: # not IHX superheater
                    T13 = self.objCP.Property('T', H=H13, P=P13)  # K
                
                P5 = P13 + self.DPE
                
                [H, P, X, T, XQ, XL, XV, VL, VV, HL, HV, TS6, QFREZ] 
                    = self.lowevp(ICYCL, 0,
                           H, P, X, T,  XQ,
                                                          XL,
                                                          XV,
                                                          VL,
                                                          VV,
                                                          HL,
                                                          TS3,
                                                          TS5,
                                                          DPF,
                                                          ETHX2)                
                
                
                
                
            