# Python import
import math
import sys
from abc import ABC, abstractmethod

# User import
# from .Data import Data
from CompMap import CompMap
from ErrorException import ErrorException

from FileAccess import FileAccess
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Create Compressor object based on IMAP
#          input: IMAP = 0  Map
#               : IMAP = 1  EER
#               : IMAP = 2  Efficiency Model
#               : objCP = CoolProp object
#               : FILMAP1 compressor map file
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Compressor:
    def getCompObject (self, IMAP, objCP):
        if (IMAP == 0):  # Map
            objCompType = Comp_Map(IMAP, objCP)

        elif (IMAP == 1):  # EER
            objCompType = Comp_EER(IMAP, objCP)

        elif (IMAP == 2):  # Efficiency Model
            objCompType = Comp_EMOD(IMAP, objCP)

        else:
            objCompType = None
            raise ErrorException('IMAP value error', 'Comp1000')
        return objCompType


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Evaprator configration
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Comp_Abstract (ABC):
    DATA_BASIC = 0  # =1 if method setBasicSetting is called
    DATA_EER = 0  # =1 if method setEERSetting is called
    DATA_H_T = 0  # =1 if method setH1_T12Setting is called
    
    def __init__(self, IMAP, objCP):
        self.IMAP = IMAP
        self.objCP = objCP

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=

    # Abstract methods
    # -----------------------------------------------------------
    # Job 			: Compressor balance
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
    @abstractmethod
    def comp_balance(self, PSUCT, PDISC, TSUCT, VSUCT, MREF):
        pass

    # Get compressor Iso. Efficiency and  clearance volume
    # this value is calulated only in case of IMAP = 1
    def comp_isoEta_ce(self):
        if self.IMAP == 1:
            if Comp_Abstract.DATA_BASIC == 0:
                raise ErrorException('Basic Data is not set', 'Comp2051')
            return [self.ETAC, self.CE]
  
        else:
            return [None, None]
        
    def setBasicSetting (self
                        ,EFFC, DISPLC, CE, EER, SEFF
                        ,TAMB, MEFF, ICOOL, ICOMP
                        ,SPEED, strFileName, FRACT_SPEED, SIZE
                         ):
        Comp_Abstract.DATA_BASIC = 1 # data was set
        
        # DISPLC: compressor displacement cu-cm
        # CE    : clearance volume (fraction)  if IMAP = 1
        # EFFC  : Compressor Effc. if IMAP = 1
        # EER   : Rated EER used only if IMAP = 1
        # SEFF  : isentropic efficiency

        # ICOOL : Fan cooling method 0 Static, 1 Fan-Forced
        # ICOMP : Compressor Type 1   Reciprocating, 2   Rotary

        # TAMB : Ambient Temp K
        # MEFF used only in case of IMAP = 0 (MAP) or 2 (Efficiency Model)

        # SPEED : Nominal Speed (rpm =3450)
        # FRACT_SPEED : FRACTIONAL SPEED
        # ICOMP compressor type  1-Reciprocating compressor, 2-Rotary
        # strFileName = "somname.cmp"  # File name
        # SIZE = capacity (btuh) at rating conditions (kcal/h = 3.97 btu/h)

        self.TAMB = TAMB
        self.DISPLC = DISPLC
        self.EFFC = EFFC
        self.CE = CE
        self.EER = EER
        self.SEFF = SEFF
        self.ICOOL = ICOOL
        self.ICOMP = ICOMP

        self.MEFF = MEFF
        self.ICOOL = ICOOL

        self.SPEED = SPEED
        self.strFileName = strFileName
        self.FRACT_SPEED = FRACT_SPEED

        self.SIZE = SIZE

        if self.IMAP == 1:
            [self.ETAC, self.CE] = self.map( self.ICOMP,
                                        self.ICOOL,
                                        self.EER,
                                        self.SIZE,
                                        self.DISPLC,
                                        self.SPEED)

    def setEERSetting (self, QHILO, QCAN):  # only for IMAP=1  EER
        # QHILO - normalized heat loss from dischange line inside
        # QCAN - compressor shell loss normalized to power input
        
        Comp_Abstract.DATA_EER = 1 # data was set
        self.QHILO = QHILO
        self.QCAN = QCAN

    def setH1_T12Setting (self, H1, T12):  # only for IMAP=1&2
        Comp_Abstract.DATA_H_T = 1 # data was set
        self.H1 = H1
        self.T12 = T12

    def comp(self, H1, P1, P2, T1, T12, MEFF, QHILO, QCAN,
             V1, TAMB, EER, SEFF, SPEED, MREF, IMAP, EFFC, CE, ICOOL, ICOMP):

        #  compressor model

        R = 8.314
        TOLS = 0.1
        ETA_ISEN = None

        # conversion functions
        def F_TO_K(T):
            return (T + 459.7) / 1.8

        # set up initial guess for suction port conditions
        TSUC = T1
        VSUC = V1

        HSUC = self.objCP.Property('H', T=TSUC, V=VSUC)  # j/kg
        TDEW = self.objCP.Property('T', P=P2, X=0)  # pressur in Pa
        VDEW = self.objCP.Property('V', P=P2, X=0)  # Volume in m3/kg

        # calculate isentropic conditions based on shell inlet
        SSUC = self.objCP.Property('S', T=TSUC, V=VSUC)  # S in j/kg/K
        T2 = self.objCP.Property('T', S=SSUC, P=P2)  # K

        H2 = self.objCP.Property('H', T=T2, S=SSUC) # j/kg
        VV2 = self.objCP.Property('V', T=T2, S=SSUC) # j/kg
        # HISEN = H2

        # select model
        if(IMAP == 1):  # EER model
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

            H_SUC = self.objCP.Property('H', T=TSUC, P=P1)  # j/kg
            CV = self.objCP.Property('CV', T=TSUC, P=P1)  # j/kg/K
            CP = self.objCP.Property('CP', T=TSUC, P=P1)  # j/kg/K

            SSUC = self.objCP.Property('s', T=TSUC, P=P1)  # J/kg K
            H2S = self.objCP.Property('H', P=P2, S=SSUC)  # J/kg

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

            HDISC = self.objCP.Property('H', T=TDISC, P=P2)  # j/kg

            ETA_ISEN = (H2S - H_SUC) / (HDISC - H_SUC)

            if(ICOOL == 0):
                RATIO = 0.68 - 0.05 * EER
            else:
                RATIO = 0.90 - 0.07 * EER

            T2 = TDISC - RATIO * (TDISC - TAMB)

            H2 = self.objCP.Property('H', T=T2, P=P2)  # j/kg

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

            HSUC = self.objCP.Property('H', T=TSUC, P=P1)  # j/kg

            while (ERROR > TOLS and ITER < 10):
                ITER = ITER + 1

                # use VSUC not P1, to prevent in wet area
                SSUC = self.objCP.Property('S', T=TSUC, V=VSUC)  # S in j/kg/K
                T2 = self.objCP.Property('T', S=SSUC, P=P2)  # K

                H2 = self.objCP.Property('H', T=T2, S=SSUC)  # j/kg
                CV = self.objCP.Property('CV', T=T2, S=SSUC)  # j/kg/K
                CP = self.objCP.Property('CP', T=T2, S=SSUC)  # j/kg/K

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
                T1P = self.objCP.Property('T', H=H1P, P=P1)  # K

                if self.objCP.isError():
                    print ("Error: " + self.objCP.err_description())
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
            TDISC = self.objCP.Property('T', H=HDISC, P=P2)  # K

            H2 = H1 + ((H2 - H1)) / (1.0 + QHILO / (1.0 - QCAN))
            T2 = self.objCP.Property('T', H=H2, P=P2)  # K
            VV2 = self.objCP.Property('V', H=H2, P=P2)  # m3/kg

        # calculate mass flow rate
        if(ICOMP == 1):
            ETAV = 0.92 * (1.0 - CE * (PR**RINV - 1.0))
        else:
            ETAV = 1.00 * (1.0 - CE * (PR**RINV - 1.0))

        DISP = MREF * VSUC / (60.0 * SPEED * ETAV)  # SPEED in rpm MREF kg/hr

        HOUT = H2
        ETAS = ETA_ISEN

        return [T2, HOUT, QHILO, QCAN, VSUC, VV2,
               TSUC, TDISC, GAMA, RN, ETAS, DISP]


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Compressor Type : Map
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Comp_Map(Comp_Abstract):  # Data.obj_cdata.IMAP== 0
    IUNITS = None
    TEDATA = None
    TCDATA = None
    CAPAC = None
    POWER = None

    def comp_balance(self, PSUCT, PDISC, TSUCT, VSUCT, MREF):
        if Comp_Abstract.DATA_BASIC == 0:
            raise ErrorException('Basic Data is not set', 'Comp2001')
            
        OLDMAS = MREF

        lstRes = self.compcall(
            PSUCT=PSUCT, PDISC=PDISC, TSUCT=TSUCT, VSUCT=VSUCT, MREF=MREF,
            TAMB=self.TAMB,
            FRACT_SPEED=self.FRACT_SPEED,
            ICOMP=self.ICOMP
            )

        MREF = (lstRes[10] + 2.0 * OLDMAS) / 3.0 # to review

        if (MREF > 1.05 * OLDMAS):
            MREF = 1.05 * OLDMAS
        if (MREF < 0.95 * OLDMAS):
            MREF = 0.95 * OLDMAS

        dicRes = {'TSUC':lstRes[5]
                 ,'TDISC':lstRes[6]
                 ,'HOUT':lstRes[0]
                 ,'QHILO':lstRes[1]
                 ,'QCAN':lstRes[2]
                 ,'VSUC':lstRes[3]
                 ,'VV2':lstRes[4]
                 ,'GAMA':lstRes[7]
                 ,'ETAC':lstRes[9]
                 ,'MREF':MREF  # get input from compcall
                 ,'T2':None  # not used in this model
                 ,'DISP':None  # not used in this model
                 ,'RN':lstRes[8]  # useless
                 }

        return dicRes

    def compcall( self, PSUCT, PDISC, TSUCT, VSUCT,
                  TAMB, MREF, FRACT_SPEED, ICOMP):
        # ************************************************************
        # true compressor map routine.  applies to refrigerant        *
        # subroutine compcall calculates isentropic compressor        *
        # performance and calls subroutine compmap                    *
        # *************************************************************

        # TSUCT = T1
        # PSUCT = P1
        # VSUCT = V1
        H1 = self.objCP.Property('H', T=TSUCT, V=VSUCT)  # j/kg

        CP = self.objCP.Property('CP', T=TSUCT, V=VSUCT)  # j/kg/K
        CV = self.objCP.Property('CV', T=TSUCT, V=VSUCT)  # j/kg/K

        GAMA = CP / CV

        SSUCT = self.objCP.Property('S', T=TSUCT, V=VSUCT)  # S in j/kg/K
        H2S = self.objCP.Property('H', S=SSUCT, P=PDISC)  # j/kg

        # calculate isentropic power requirement
        # moved by Dr-Omar WDOTS = MREF * (H2S - H1)/1000  # kj/hr = kg/hr * (j/kg)/1000

        # determine actual compressor performance [TSP, WDOT, MDOT, QSHELL]
        [TSUC, WDOT, MREF, QSHELL] =\
        self.compmap(PSUCT=PSUCT, PDISC=PDISC, TSUCT=TSUCT,
                    VSUCT=VSUCT,
                    GAMA=GAMA, TAMB=TAMB,
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
        TDISC = self.objCP.Property('T', H=HOUT, P=PDISC)  # K
        VV2 = self.objCP.Property('V', H=HOUT, P=PDISC)  # m3/kg

        # calculate isentropic efficiency
        ETAS = WDOTS / WDOT  # none = (kj/hr) / (kj/hr)

        # use call statement arguments to avoid compilier warning

        # MEFF = MEFF useless
        VSUC = VSUCT
        QHILO = 0.0 # use less feedback
        RN = 0.97 * GAMA

        return [ HOUT, QHILO, QCAN, VSUC,
                VV2, TSUC, TDISC, GAMA, RN, ETAS, MREF]

    def compmap(self, PSUCT, PDISC, ICOMP,
                TSUCT, VSUCT, TAMB, GAMA, FRACT_SPEED,
                strFolder=None):

        # interpolation job
        def interpolation (x_value, y_value, x_series, y_series, data):

            def find_nerest_index (flt_value, lst):
                items_x = [itm for itm in lst if flt_value >= itm]
                return len(items_x)

            def interplate (x_value, x1, x2, y1, y2):
                if x1 == x2:
                    return y2
                else:
                    return y1 + (x_value - x1) * (y2 - y1) / (x2 - x1)

            if len(x_series) != len(data[0]) or len(y_series) != len(data):
                raise ErrorException('Reading value out of range', 'Comp1001')
                return None

            if x_value > max(x_series) or y_value > max(y_series)\
                    or x_value < min(x_series) or y_value < min(y_series):
                raise ErrorException('Reading value out of range', 'Comp1002')
                return None

            x_pos = find_nerest_index(x_value, x_series) - 1
            y_pos = find_nerest_index(y_value, y_series) - 1

            if x_pos + 1 == len(x_series):
                x_pos_next = x_pos
            else:
                x_pos_next = x_pos + 1

            if y_pos + 1 == len(y_series):
                y_pos_next = y_pos
            else:
                y_pos_next = y_pos + 1

            value1 = interplate (x_value=x_value,
                                 x1=x_series [x_pos],
                                 x2=x_series [x_pos_next],
                                 y1=data [y_pos][x_pos],
                                 y2=data [y_pos][x_pos_next]
                                 )

            value2 = interplate (x_value=x_value,
                                 x1=x_series [x_pos],
                                 x2=x_series [x_pos_next],
                                 y1=data [y_pos_next][x_pos],
                                 y2=data [y_pos_next][x_pos_next]
                                 )

            value = interplate (x_value=y_value,
                                 x1=y_series [y_pos],
                                 x2=y_series [y_pos_next],
                                 y1=value1,
                                 y2=value2
                                 )
            return value

        def read_comp_file(strFile_name, strFolder=None):
            # ============Python commnet : data description and sample data
            '''
                SAMPLE CALORIMETER-BASED MAP
                default data:
                4.5 mass flow at standard rating point (lb/hr or kg/hr)
                3	number of data points along evaporating temperature axis
                3 	number of data points along condensing temperature axis
                1 	compressor type (1 - reciprocating; 2 - rotary)
                1	units for capacity, temperature data,
                    and mass flow  (1 - btu/hr, deg f, lb/hr;
                                    2 - kcal/hr, deg c, kg/hr)
                                    power data must be in watts

                CAPACITY DATA, BTU/HR
                COND TEMP (F) 	EVAPORATING TEMPERATURE (F)
                        -20 	-10 	0
                110		113.6	127.7 	142.6
                120		115.8 	132.4 	148.9
                130		118.3 	135.9 	154.4

                SMOOTHED MAP CREATED BY REMAP
                default data
                12.2	mass flow at standard rating point (lb/hr or kg/hr)
                6	number of data points along evaporating temperature axis
                7	number of data points along condensing temperature axis
                1	compressor type (1 - reciprocating; 2 - rotary)
                1	units for capacity, temperature data,
                    and mass flow (1 - btu/hr, deg f, lb/hr;
                                   2 - kcal/hr, deg c, kg/hr)
                                   power data must be in watts

                CAPACITY DATA, BTU/HR

                COND TEMP (F) 	EVAPORATING TEMPERATURE(F)
                        -40		-30		-20 	10		0		10
                70		370.4	500.5	659.3	851.1	1080.8	1353.5
                80		358.1	488.2	647.0	838.8	1068.5	1341.2
                90		344.3	474.4	633.1	824.9	1054.6	1327.2
                100		328.9	458.9	617.7	809.4	1039.0	1311.6
                110		311.8	441.8	600.4	792.1	1021.6	1294.1
                120		292.9	422.8	581.4	772.9	1002.3	1274.6
                130		272.1	401.9	560.3	751.8	980.9	1253.0


                COMPRESSOR POWER (WATTS)
                COND TEMP(F) 	EVAPORATING TEMPERATURE (F)
                        -40 	-30		-20		-10		0 		10
                70		65.5	75.6	84.8	92.3	97.7	100.1
                80		69.1	80.9	91.9	101.7	109.5	114.7
                90		72.2	85.7	98.5	110.4	120.6	128.6
                100		74.6	89.8	104.6	118.5	131.1	141.8
                110		76.4	93.3	110.0	126.0	141.0	154.2
                120		77.3	96.1	114.7	132.9	150.2	166.0
                130		77.3	98.1	118.7	139.0	158.6	177.0
            '''
            # ==============================================================

            # set the default folder is folder is none
            if strFolder is None:
                strFolder = sys.path[0] + "\\" + "compmap"

            obj_comp_map = CompMap(strFile_name, strFolder)

            if obj_comp_map.isError():
                print(obj_comp_map.err_description())
                sys.exit('6000')

            obj_comp_map.readMapData()
            if obj_comp_map.isError():
                print(obj_comp_map.err_description())
                sys.exit('6001')

            # Python comment:
            # NEVAP : Integer 3 digits, number of data points
            #           along evaporating temperature axis.
            # NCOND : Integer 3 digits, number of data points
            #           along condensing temperature axis.

            NEVAP = obj_comp_map.getX_count()
            NCOND = obj_comp_map.getY_count()

            # this input is cancelled in Python, compressor type is defined
            #  in basic entry data.
            # changing type in middle of process is not acceptable
            # user can re-enter basic entry data to use another type.
            # ICOMP : integer compressor type (1 - reciprocating; 2 - rotary)

            # IUNITS : Integer 1 digits units for capacity
            #       , temperature data, and mass flow
            # (1 - btu/hr, deg f, lb/hr; 2 - kcal/hr, deg c, kg/hr) power data
            # power must be in watts

            IUNITS = obj_comp_map.getUnit()

            # Python commnet : read EVAPORATING TEMPERATURE - x axis
            # TEDATA list has all x values for evaporator temperatures
            #             for both capacity and power.
            # TCDATA list has all y values for conderser temperatures
            #             for both capacity and power.
            # CAPAC list has the capacity data.
            # POWER list has the power data.

            TEDATA = obj_comp_map.getX_values()
            TCDATA = obj_comp_map.getY1_values()

            # READ COMPRESSOR CAPACITY DATA
            CAPAC = obj_comp_map.getCapacity()

            # READ COMPRESSOR POWER DATA
            POWER = obj_comp_map.getPower()

            del(obj_comp_map) 	# close file

            return [IUNITS, TEDATA, TCDATA, CAPAC, POWER]

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
             ] = read_comp_file(self.strFileName, strFolder)

        # Determine the saturation temperatures corresponding to PSUCT, PDISC
        TEVAPK = self.objCP.Property('T', P=PSUCT, X=0) # K
        TCONDK = self.objCP.Property('T', P=PDISC, X=0) # K

        # Determine the enthalpies at each pressure for the following:
        # 	VAPOR - 90F or 32.2222C or 305.372K
        # 	LIQUID - 90F or 32.2222C or 305.372K

        T90F_in_K = 305.372  # K
        HIN = self.objCP.Property('H', T=T90F_in_K, P=PSUCT)  # j/kg

        # liquid leaving condenser
        HOUT = self.objCP.Property('H', T=T90F_in_K, P=PDISC)  # j/kg

        # determine isentropic compression enthalpy (HS)
        SSUC = self.objCP.Property('S', T=T90F_in_K, P=PSUCT)  # j/kg/K
        TS = self.objCP.Property('T', S=SSUC, P=PDISC)  # K
        HS = self.objCP.Property('H', T=TS, P=PDISC)  # j/kg
        # or HS = self.objCP.Property('H', S=SSUC, P=PDISC)  # j/kg

        # convert the saturation temperatures to corresspond to map data units
        if (Comp_Map.IUNITS == 2): # i.e temp in F
            TEVAP = TEVAPK * 1.8 - 459.67  # convert from Deg K to F
            TCOND = TCONDK * 1.8 - 459.67
        else:
            TEVAP = TEVAPK - 273.16  # convert from Deg K to C
            TCOND = TCONDK - 273.16

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
        if self.objCP.is_two_phase(
                self.objCP.phase_byPressTemp (PSUCT, T90F_in_K)):
            VVAP = self.objCP.Property('V', T=T90F_in_K, X=1)  # m3/kg
        else:
            VVAP = self.objCP.Property('V', T=T90F_in_K, P=PSUCT)  # m3/kg
        # -- -------------End of code by Python

        MDOT = MDOT90 * VVAP / VSUCT  # kg/hr

        # estimate effect on compressor power as ratio of
        #  suction plenum temperatures
        # none =(kg/hr) * (j/kg)     /(kj/hr) /1000
        EFFS = MDOT90 * (HS - HIN) / WDOT90 /1000

        HSUCT = self.objCP.Property('H', T=TSUCT, P=PSUCT)  # j/kg
        SSUC = self.objCP.Property('S', T=TSUCT, P=PSUCT)  # j/kg/K
        TS = self.objCP.Property('T', S=SSUC, P=PDISC)  # K
        HS = self.objCP.Property('H', T=TS, P=PDISC)  # j/kg

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


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Compressor Type : EER
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Comp_EER (Comp_Abstract):  # Data.obj_cdata.IMAP== 1

    def comp_balance(self, PSUCT, PDISC, TSUCT, VSUCT, MREF):
        if Comp_Abstract.DATA_BASIC == 0:
            raise ErrorException('Basic Data is not set', 'Comp2002')

        if Comp_Abstract.DATA_EER == 0:
            raise ErrorException('Basic EER data is not set', 'Comp2012')

        if Comp_Abstract.DATA_H_T == 0:
            raise ErrorException('Basic T & H Data is not set', 'Comp2022')
            
        # call theoretically based model

        OLDMAS = MREF
        lstRes = self.comp (P1=PSUCT, P2=PDISC, T1=TSUCT, V1=VSUCT
                        ,MREF=MREF
                        ,H1=self.H1, T12=self.T12
                        ,MEFF=self.MEFF
                        ,QHILO=self.QHILO, QCAN=self.QCAN, TAMB=self.TAMB
                        ,EER=self.EER, SEFF=self.SEFF
                        ,SPEED=self.SPEED, IMAP=self.IMAP
                        ,EFFC=self.EFFC, CE=self.CE
                        ,ICOOL=self.ICOOL, ICOMP=self.ICOMP)

        #	update the mass flow to correspond to the displacement

        # DISPI = Data.obj_cdata.DISP / 1.6387E-05
        DISPI = lstRes[11] # m3
        # Python change DISPLC to m3
        MREF = (MREF * (self.DISPLC / DISPI) + 2 * OLDMAS) / 3

        if (MREF > 1.05 * OLDMAS):
            MREF = 1.04 * OLDMAS

        if (MREF < 0.95 * OLDMAS):
            MREF = 0.96 * OLDMAS

        dicRes = {'TSUC':lstRes[6]
                 ,'TDISC':lstRes[7]
                 ,'HOUT':lstRes[1]
                 ,'QHILO':lstRes[2]
                 ,'QCAN':lstRes[3]
                 ,'VSUC':lstRes[4]
                 ,'VV2':lstRes[5]
                 ,'GAMA':lstRes[8]
                 ,'ETAC':lstRes[10]
                 ,'MREF':MREF
                 ,'T2':lstRes[0]
                 ,'DISP': lstRes[11]
                 ,'RN':lstRes[9]  # useless
                 }
        return dicRes

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def map(self, ICOMP, ICOOL, EER, SIZE, DISPL, SPEEDN):
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

        #    conversion functions
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
        ITAB = 0

        if(ITAB == 0):
            EXISTS = False  # Python comment allways  False

        MOTOR = 3.413 * SIZE / EER 		# Motor power in Btu/hr
        CAP = [[1.0548 * SIZE] * (3 + 1) for i in range(3 + 1)]  # kJ/hr
        PWR = [[1.0548 * MOTOR] * (3 + 1) for i in range(3 + 1)]  # kJ/hr

        # calculate the mass flows assuming 90 f liquid and vapor temps
        for I in range(1, 3 + 1):  # DO I = 1, 3
            T_COND = 100 + 10 * I
            T_COND = F_TO_K(T_COND)

            P_COND = self.objCP.Property('P', X=1, T=T_COND)  # Pa
            H_LIQ = self.objCP.Property('H', T=T90, P=P_COND)  # j/kg

            for J in range(1, 3 + 1):  # DO J = 1, 3
                T_EVAP = -30 + 10 * J
                T_EVAP = F_TO_K(T_EVAP)

                P_EVAP = self.objCP.Property('P', X=1, T=T_EVAP)  # Pa
                H_VAP = self.objCP.Property('H', T=T90, P=P_EVAP)  # j/kg

                # kg/hr    = (kj/hr)/ (kj/kg)
                MASS[I][J] = CAP[I][J] / (H_VAP - H_LIQ) * 1000

                #    ESTIMATE THE SUCTION GAS TEMPERATURE AND THE EFFICIENCIES
                if(ICOOL == 0):
                    TSUC = 479.59 - 64.815 * EER
                else:
                    TSUC = 427.84 - 57.895 * EER

                TSUC = TSUC - 2.0 * (3 - I) - 2.0 * (J - 2)

                if(ICOMP == 2):
                    TSUC = 120.0  # Rotary

                TSUC = F_TO_K(TSUC)  # K
                VSUC = self.objCP.Property('V', T=TSUC, P=P_EVAP)  # m3/kg
                H_SUC = self.objCP.Property('H', T=T90, P=P_EVAP)  # j/kg
                CP = self.objCP.Property('CP', T=T90, P=P_EVAP)  # j/kg/K
                CV = self.objCP.Property('CV', T=T90, P=P_EVAP)  # j/kg/K
                SSUC = self.objCP.Property('S', T=TSUC, P=P_EVAP)  # j/kg/K
                T2S = self.objCP.Property('T', S=SSUC, P=P_COND)  # K
                H2S = self.objCP.Property('H', T=T2S, P=P_COND)  # j/kg

                #  None          kg/hr          * (kj/kg)             / (kJ/hr)
                ETAS[I][J] = MASS[I][J] * (H2S - H_SUC) /1000/ PWR[I][J] #

                # not the ETAV in common
                #      kg/hr  * m3/kg  /rps / m3
                ETAV[I][J] = MASS[I][J] * VSUC /(60.0 * SPEEDN) / (DISPL / 61023.6)

                # modification by Dr Omar
                # Fractional Speed (-) input value - changed from 1 (bad value) to 3450
                # ETAV[I][J] = MASS[I][J] * VSUC / (60.0 * 3450) / (DISPL / 61023.6)

                K = ETAP * CP / CV
                PR = P_COND / P_EVAP

                if(ICOMP == 1):
                    CEIJ[I][J] = ((0.92 - ETAV[I][J]) / 0.92) / \
                                (PR**(1.0 / K) - 1.0)
                else:
                    CEIJ[I][J] = ((1.00 - ETAV[I][J]) / 1.00) / \
                                (PR**(1.0 / K) - 1.0)

                # estimate cyclinder temperature and can outlet temperature
                TCYL = TSUC * (P_COND / P_EVAP) ** (1.0 - 1.0 / K)
                COP = CAP[I][J] / PWR[I][J]  # None =  (kJ/hr)  / (kJ/hr)

                if(ICOOL == 0):
                    RATIO = 0.68 - 0.05 * 3.413 * COP
                else:
                    RATIO = 0.90 - 0.07 * 3.413 * COP

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


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Compressor Type : Efficiency Model
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Comp_EMOD (Comp_Abstract):  # Data.obj_cdata.IMAP== 2
    def comp_balance(self, PSUCT, PDISC, TSUCT, VSUCT, MREF):
        if Comp_Abstract.DATA_BASIC == 0:
            raise ErrorException('Basic Data is not set', 'Comp2003')

        if Comp_Abstract.DATA_H_T == 0:
            raise ErrorException('Basic T & H Data is not set', 'Comp2023')
            
        # call theoretically based model
        OLDMAS = MREF
        lstRes = self.comp (P1=PSUCT, P2=PDISC, T1=TSUCT, V1=VSUCT
                        ,MREF=MREF
                        ,H1=self.H1, T12=self.T12
                        ,MEFF=self.MEFF
                        ,QHILO=self.QHILO, QCAN=self.QCAN, TAMB=self.TAMB
                        ,EER=self.EER, SEFF=self.SEFF
                        ,SPEED=self.SPEED, IMAP=self.IMAP
                        ,EFFC=self.EFFC, CE=self.CE
                        ,ICOOL=self.ICOOL, ICOMP=self.ICOMP)

        #	update the mass flow to correspond to the displacement

        # DISPI = Data.obj_cdata.DISP / 1.6387E-05
        DISPI = lstRes[11] # m3

        # Python change DISPLC to m3
        MREF = (MREF * (self.DISPLC / DISPI) + 2 * OLDMAS) / 3

        if (MREF > 1.05 * OLDMAS):
            MREF = 1.04 * OLDMAS

        if (MREF < 0.95 * OLDMAS):
            MREF = 0.96 * OLDMAS

        dicRes = {'TSUC':lstRes[6]
                 ,'TDISC':lstRes[7]
                 ,'HOUT':lstRes[1]
                 ,'QHILO':lstRes[2]
                 ,'QCAN':lstRes[3]
                 ,'VSUC':lstRes[4]
                 ,'VV2':lstRes[5]
                 ,'GAMA':lstRes[8]
                 ,'ETAC':lstRes[10]
                 ,'MREF':MREF
                 ,'T2':lstRes[0]
                 ,'DISP': lstRes[11]
                 ,'RN':lstRes[9]  # useless
                 }
        return dicRes


