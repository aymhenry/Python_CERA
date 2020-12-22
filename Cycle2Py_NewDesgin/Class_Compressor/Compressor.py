# Python import
import math
import sys

# User import
# from .Data import Data
from CompMap import CompMap
from ErrorException import ErrorException

from FileAccess import FileAccess
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Create Compressor object
#               : objCP = CoolProp object
#                 TAMB : Ambient Temp K
#                 ICOMP : Compressor Type 1-Reciprocating, 2-Rotary 
#                 FRACT_SPEED : FRACTIONAL SPEED
#                 strFileName = "SomName.cmp"  # File name
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Compressor:
    IUNITS = None
    TEDATA = None
    TCDATA = None
    CAPAC = None
    POWER = None
    
    def __init__(self, objCP, TAMB, ICOMP, FRACT_SPEED, strFileName):
        self.objCP = objCP

        self.TAMB = TAMB
        self.ICOMP = ICOMP
        
        self.FRACT_SPEED = FRACT_SPEED
        self.strFileName = strFileName

    def comp_balance(self, PSUCT, PDISC, TSUCT, VSUCT, MREF):
            
        OLDMAS = MREF

        lstRes = self.compcall(
            PSUCT=PSUCT, PDISC=PDISC, TSUCT=TSUCT, VSUCT=VSUCT,
            TAMB=self.TAMB,
            FRACT_SPEED=self.FRACT_SPEED,
            ICOMP=self.ICOMP
            )

        MREF = (lstRes[9] + 2.0 * OLDMAS) / 3.0 # to review

        if (MREF > 1.05 * OLDMAS):
            MREF = 1.05 * OLDMAS
        if (MREF < 0.95 * OLDMAS):
            MREF = 0.95 * OLDMAS
        
        # output list
        # return [HOUT, QCAN, VSUC, VV2, TSP, TDISC, GAMA, RN, ETAS, MREF]
        dicRes = {'TDISC':lstRes[5]  # Discharge Temp (K)
                 ,'HOUT':lstRes[0]  # Discharge Enthaply (j/kg)
                 ,'QCAN':lstRes[1]  # comp. shell loss normalized to power j/kg
                 ,'VSUC':lstRes[2]  # Suction sp.volume (m3/kg)
                 ,'VV2':lstRes[3]  # Discharge sp.volume (m3/kg)
                 ,'GAMA':lstRes[6]  # Cp/Cv ration
                 ,'ETAC':lstRes[8]  # Compressor Effe.
                 ,'MREF':MREF  # get input from compcall
                 ,'TSP':lstRes[4]  # Compressor Exit (K)
                 }

        return dicRes

    def compcall( self, PSUCT, PDISC, TSUCT, VSUCT,
                  TAMB, FRACT_SPEED, ICOMP):
        # ************************************************************
        # true compressor map routine.  applies to refrigerant        *
        # subroutine compcall calculates isentropic compressor        *
        # performance and calls subroutine compmap                    *
        # *************************************************************

        H1 = self.objCP.Property('H', T=TSUCT, V=VSUCT)  # j/kg

        CP = self.objCP.Property('CP', T=TSUCT, V=VSUCT)  # j/kg/K
        CV = self.objCP.Property('CV', T=TSUCT, V=VSUCT)  # j/kg/K

        GAMA = CP / CV

        SSUCT = self.objCP.Property('S', T=TSUCT, V=VSUCT)  # S in j/kg/K
        H2S = self.objCP.Property('H', S=SSUCT, P=PDISC)  # j/kg

        # calculate isentropic power requirement
        # moved by Dr-Omar WDOTS = MREF * (H2S - H1)/1000  # kj/hr = kg/hr * (j/kg)/1000

        # determine actual compressor performance [TSP, WDOT, MDOT, QSHELL]
        [TSP, WDOT, MREF, QSHELL] =\
        self.compmap(PSUCT=PSUCT, PDISC=PDISC, TSUCT=TSUCT,
                    VSUCT=VSUCT,
                    GAMA=GAMA, TAMB=TAMB,
                    FRACT_SPEED=FRACT_SPEED, ICOMP=ICOMP)

        # calculate isentropic power requirement
        WDOTS = MREF * (H2S - H1)/1000  # kj/hr = kg/hr * (j/kg)/1000

        # calculate refrigerant exit enthalpy and temperature
        # j/kg = (j/kg) + (kj/hr) /(kg/hr) *1000
        HOUT = H1  + (WDOT - QSHELL) / MREF * 1000  # j/kg

        # HOUT = H2  # send to output
        QCAN = QSHELL / WDOT  # no unit
        TDISC = self.objCP.Property('T', H=HOUT, P=PDISC)  # K
        VV2 = self.objCP.Property('V', H=HOUT, P=PDISC)  # m3/kg

        # calculate isentropic efficiency
        ETAS = WDOTS / WDOT  # none = (kj/hr) / (kj/hr)

        # use call statement arguments to avoid compilier warning
        VSUC = VSUCT
        RN = 0.97 * GAMA

        return [HOUT, QCAN, VSUC, VV2, TSP, TDISC, GAMA, RN, ETAS, MREF]

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

            obj_Compressor = CompMap(strFile_name, strFolder)

            if obj_Compressor.isError():
                print(obj_Compressor.err_description())
                sys.exit('6000')

            obj_Compressor.readMapData()
            if obj_Compressor.isError():
                print(obj_Compressor.err_description())
                sys.exit('6001')

            # Python comment:
            # NEVAP : Integer 3 digits, number of data points
            #           along evaporating temperature axis.
            # NCOND : Integer 3 digits, number of data points
            #           along condensing temperature axis.

            NEVAP = obj_Compressor.getX_count()
            NCOND = obj_Compressor.getY_count()

            # this input is cancelled in Python, compressor type is defined
            #  in basic entry data.
            # changing type in middle of process is not acceptable
            # user can re-enter basic entry data to use another type.
            # ICOMP : integer compressor type (1 - reciprocating; 2 - rotary)

            # IUNITS : Integer 1 digits units for capacity
            #       , temperature data, and mass flow
            # (1 - btu/hr, deg f, lb/hr; 2 - kcal/hr, deg c, kg/hr) power data
            # power must be in watts

            Compressor.IUNITS = obj_Compressor.getUnit()

            # Python commnet : read EVAPORATING TEMPERATURE - x axis
            # TEDATA list has all x values for evaporator temperatures
            #             for both capacity and power.
            # TCDATA list has all y values for conderser temperatures
            #             for both capacity and power.
            # CAPAC list has the capacity data.
            # POWER list has the power data.

            TEDATA = obj_Compressor.getX_values()
            TCDATA = obj_Compressor.getY1_values()

            # READ COMPRESSOR CAPACITY DATA
            CAPAC = obj_Compressor.getCapacity()

            # READ COMPRESSOR POWER DATA
            POWER = obj_Compressor.getPower()

            del(obj_Compressor) 	# close file

            return [Compressor.IUNITS, TEDATA, TCDATA, CAPAC, POWER]

        # ******************************************************************
        # calculates compressor performance based on tabular map
        # data and corrects for suction temperature other than 90f
        # ******************************************************************

        if (Compressor.IUNITS is None):  # if None then data was not fetched
            [Compressor.IUNITS,
             Compressor.TEDATA,
             Compressor.TCDATA,
             Compressor.CAPAC,
             Compressor.POWER
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
        if (Compressor.IUNITS == 2): # i.e temp in F
            TEVAP = TEVAPK * 1.8 - 459.67  # convert from Deg K to F
            TCOND = TCONDK * 1.8 - 459.67
        else:
            TEVAP = TEVAPK - 273.16  # convert from Deg K to C
            TCOND = TCONDK - 273.16

        CAP = interpolation (x_value=TEVAP, y_value=TCOND,
                             x_series=Compressor.TEDATA,
                             y_series=Compressor.TCDATA,
                             data=Compressor.CAPAC)

        POW = interpolation (x_value=TEVAP, y_value=TCOND,
                             x_series=Compressor.TEDATA,
                             y_series=Compressor.TCDATA,
                             data=Compressor.POWER)

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
        if (Compressor.IUNITS == 2): # i.e temp in F
            CAP = CAP * 1.0548  # from but/hr to kj/hr
        else:
            CAP = CAP * 4.184   # from kcal/hr to kj/hr

        if (Compressor.IUNITS != 1 and Compressor.IUNITS != 2):
            print("###CHECK COMPRESSOR MAP UNITS###")

        # convert to kj/hr
        WDOT90 = POW / 1000.0 * 3600.0  # from watt to kj/hr

        # calculate the mass flow rate in kg/hr
        MDOT90 = 1000 * CAP / (HIN - HOUT) # kg/hr=(kj/hr)/(j/kg) *1000

        # correct mass flow rate for suction temperature other than 90 f
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

        TSP90 = (90.0 + DELTIN + 459.67) / 1.8

        AAA = (PDISC / PSUCT)**((GAMA - 1.0) / GAMA)
        TMAX90 = TSP90 * AAA
        TMAX = TSP * AAA

        T90K = 305.372  # K (90.0 F + 459.67) / 1.8
        QLOSS = 0.80
        QSHELL = WDOT * QLOSS * (TMAX - TAMB) / (TMAX90 - T90K)  # kj/hr

        return [TSP, WDOT, MDOT, QSHELL]
