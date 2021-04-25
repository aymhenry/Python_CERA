# Python Import ==================
import sys

# User Import ======================
from cycle_classes.Start import Start

strCurrent_path = sys.path[0] 

FILE_CYC_INPUT = "cycle_dat.csv"  # input file for cycle module
FILE_CYC_OUTPUT = "cycle_out.csv"  # output file for cycle module
FILE_CAB2CYC = "cab2cyc_out.csv"  # output file for cabinit module

FOLDER_INPUT = strCurrent_path + "\\" + "data"
FOLDER_OUTPUT = strCurrent_path + "\\" + "data"
    
obj_start = Start(FILE_CYC_INPUT, 
                  FILE_CYC_OUTPUT, 
                  FILE_CAB2CYC, 
                  FOLDER_INPUT, 
                  FOLDER_OUTPUT)

# is_solution = obj_start.main(True)      # DEBUG ON
is_solution = obj_start.main(False)      # DEBUG OFF

obj_start.print_scr_rep(is_solution)
