# Python Import ==================
import sys

# User Import ======================

from cab_classes.Start import Start as CabStart
from cycle_classes.Start import Start as CycStart

strCurrent_path = sys.path[0] 

FILE_CAB_INPUT = "cab_dat.csv"  # input file for cabinit module
FILE_CAB_OUTPUT = "cab_out.csv"  # output file for cabinit module
FILE_CAB2CYC = "cab2cyc_out.csv"  # output file for cabinit module

FILE_CYC_INPUT = "cycle_dat.csv"  # input file for cycle module
FILE_CYC_OUTPUT = "cycle_out.csv"  # output file for cycle module

FLDR_CAB_IN = strCurrent_path + "\\" + "data"
FLDR_CAB_OUT = strCurrent_path + "\\" + "data"
DEBUG = True

obj_cab_start = CabStart(FILE_CAB_INPUT
                         , FILE_CAB_OUTPUT
                         , FILE_CAB2CYC
                         , FLDR_CAB_IN
                         , FLDR_CAB_OUT
                         )

is_solution = obj_cab_start.main(DEBUG)      # DEBUG ON

obj_cab_start.print_scr_rep(is_solution)

if is_solution:
    obj_cyc_start = CycStart(FILE_CYC_INPUT, 
                      FILE_CYC_OUTPUT, 
                      FILE_CAB2CYC, 
                      FLDR_CAB_OUT,    # output for cab is input for cyc
                      FLDR_CAB_OUT)

    is_sol_cyc = obj_cyc_start.main(DEBUG)      # DEBUG ON
