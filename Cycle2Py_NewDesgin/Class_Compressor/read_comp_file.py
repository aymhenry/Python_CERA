# Python import
import sys
# Python import
import sys

# User import
from CoolPrp import *
from decorators import *
from CompMap import *

# =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=


@show_input_output("IN")
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
