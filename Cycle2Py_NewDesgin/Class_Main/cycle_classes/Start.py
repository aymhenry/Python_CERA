# Python Import ==================


# User Import ======================

from .View import *
from .CycleType import *
from .CycleDataModelBuiler import CycleDataModelBuiler
from cycle_classes.Trace import *

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
# Job             : Start Cycle app
#
# Editor        : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class Start:
    # Class static vars
    FILE_CYC_INPUT = "Cycle_dat.csv"  # input file for cabinit module
    FILE_CYC_OUTPUT = "Cycle_out.csv"  # output file for cabinit module

    def __init__(self):        
        self.dt = None  # object to save data        
        self.obj_control = None  # object to control data

        self.str_FILE_CYC_INPUT = Start.FILE_CYC_INPUT
        self.str_FILE_CYCLE_OUTPUT = Start.FILE_CYC_OUTPUT

        self.str_path_cyc_in = ""
        self.str_path_cyc_out = ""

    def set_filenames(self,
                      str_file_cyc_in="",
                      str_file_cyc_out="",
                      str_path_cyc_in="",
                      str_path_cyc_out=""
                      ):
                      
        self.str_path_cyc_in = str_path_cyc_in
        self.str_path_cyc_out = str_path_cyc_out

        if str_file_cyc_in != "":
            self.str_FILE_CYC_INPUT = str_file_cyc_in

        if str_file_cyc_out != "":
            self.str_FILE_CYCLE_OUTPUT = str_file_cyc_out

    # -----------------------------------------------------------
    # Job             : Main app start up, driver for all others
    # Input         :
    #
    # Output        :
    # -----------------------------------------------------------
    def main(self, DEBUG=None):
        # Add debug flag
        if DEBUG is None:
            DEBUG = True
            
        self.data_prepare(DEBUG)  # assign value to dt
        
        # add defalut feedback, OK, there is solution
        self.dt.IS_SOLTION = True
        
        # it will return objSolution object (has cycle solution)
        objSolution = self.obj_control.calculte()  # calculate heat rate
        
        # if no slotion do not print anything
        if not self.dt.IS_SOLTION:
            return False
            
        obj_view = View(
            self.dt,
            objSolution,    # will be named ds for short
            self.str_FILE_CYCLE_OUTPUT,
            self.str_path_cyc_out
            )
            
        obj_view.show_overall()
        obj_view.show_rep()        
        
        return True

    # -----------------------------------------------------------
    # Job             : Preprae the main data object & control object
    # Input         :
    #
    # Output        :
    # -----------------------------------------------------------
    def data_prepare(self, DEBUG):
        # Set main data file name
        obj_datamodel = CycleDataModelBuiler(self.str_FILE_CYC_INPUT,
                                             self.str_path_cyc_in)

        # check if error, if so exit application
        if obj_datamodel.isError():
            print("Error Opening file")
            print(obj_datamodel.err_description())  # print error description
            obj_datamodel = ""     # clean object and close file
            sys.exit('3000')  # terminat application
            # --------------

        # read data list from file, and put values in variables
        obj_datamodel.build_var_list()

        # Is data is good, or exit application
        if obj_datamodel.isError():
            print(obj_datamodel.err_description())  # print error description
            sys.exit('3001')                            # terminate
        
        # Create related data object as the given configration
        self.dt = obj_datamodel.get_data_object()
        self.dt.DEBUG = DEBUG
        
        # show row data input
        trace = Trace(dt=self.dt)
        trace.app_ins()     # show list of app inputs
        trace.cyc_pid()     # show cycle graph
        trace.pnt_lst()     # show cycle graph
        
        # Create related object as the given configration
        self.obj_control = ""

        # 1: Standard
        # self.dt.ICYCL == 1:
        self.obj_control = CycleType(self.dt)

        # INCTRL: 0 = none
        #         1 = adjust evaporator areas,
        #         2 = adjust fresh food section tempeature
        #         3 = adjust freezer    section tempeature
        #         4 = switching valve(only one section is cooled at a time)
        #         5 = solenoid valve 
        #               or fan control provides evaporator capacity 
        #               to only one cabinet during part of the cycle
  