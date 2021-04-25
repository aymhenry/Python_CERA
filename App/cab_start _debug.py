# Python Import ==================
import sys

# User Import ======================
from cab_classes.Start import Start

strCurrent_path = sys.path[0] 

FILE_CAB_INPUT = "cab_dat.csv"  # input file for cabinit module
FILE_CAB_OUTPUT = "cab_out.csv"  # output file for cabinit module
FILE_CAB2CYC = "cab2cyc_out.csv"  # output file for cabinit module

FLDR_CAB_IN = strCurrent_path + "\\" + "data"
FLDR_CAB_OUT = strCurrent_path + "\\" + "data"
    
obj_start = Start(FILE_CAB_INPUT
                  , FILE_CAB_OUTPUT
                  , FILE_CAB2CYC
                  , FLDR_CAB_IN
                  , FLDR_CAB_OUT
                  )

is_solution = obj_start.main(True)      # DEBUG ON
# is_solution = obj_start.main(False)      # DEBUG OFF

obj_start.print_scr_rep(is_solution)
