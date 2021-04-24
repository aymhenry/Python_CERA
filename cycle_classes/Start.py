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
    
    def __init__(self,
                 cycle_in=None,
                 cycle_out=None,
                 cab2cyc_in=None,
                 path_in=None,
                 path_out=None
                 ):
    
        self.dt = None  # object to save data        
        self.cab = None  # object to cab to cycle data
        self.obj_control = None  # object to control data

        # set member variables
        self.str_FILE_CYC_INPUT = None
        self.str_FILE_CYCLE_OUTPUT = None
        self.str_FILE_CAB2CYC_IN = None

        self.str_path_cyc_in = None
        self.str_path_cyc_out = None 
        
        self.set_filenames(cycle_in, cycle_out, cab2cyc_in, path_in, path_out)

    def set_filenames(self,
                  cycle_in=None,
                  cycle_out=None,
                  cab2cyc_in=None,
                  path_in=None,
                  path_out=None
                  ):
 
        self.str_FILE_CYC_INPUT = cycle_in
        self.str_FILE_CYCLE_OUTPUT = cycle_out
        self.str_FILE_CAB2CYC_IN = cab2cyc_in

        self.str_path_cyc_in = path_in
        self.str_path_cyc_out = path_out   
            

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
            self.cab,
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
        # ---------------------------------------
        # main cycle data reading
        # ---------------------------------------
        # Set main data file name, datagroup 1
        obj_datamodel = CycleDataModelBuiler(self.str_FILE_CYC_INPUT,
                                             self.str_path_cyc_in)
        
        # check if error, if so exit application
        if obj_datamodel.isError():
            print("Error Opening file")
            print(obj_datamodel.err_description())  # print error description
            sys.exit('3000')  # terminat application
            # --------------
        # obj_datamodel.setSor(1)
        # read data list from file, and put values in variables
        obj_datamodel.build_var_list(1)

        # Is data is good, or exit application
        if obj_datamodel.isError():
            print(obj_datamodel.err_description())  # print error description
            sys.exit('3001')                            # terminate

        # ---------------------------------------
        # Data from Cab app to cycle data reading
        # ---------------------------------------
        # Set main data file name, datagroup 1
        obj_cab = CycleDataModelBuiler(self.str_FILE_CAB2CYC_IN,
                                       self.str_path_cyc_in)
        # obj_cab.setSor(2)
        # check if error, if so exit application
        if obj_cab.isError():
            print("Error Opening file")
            print(obj_cab.err_description())  # print error description
            sys.exit('3000')  # terminat application
            # --------------

        # read data list from file, and put values in variables
        obj_cab.build_var_list(2)  # configration number

        # Is data is good, or exit application
        if obj_cab.isError():
            print(obj_cab.err_description())  # print error description
            sys.exit('3005 cab to cycle file error')         # terminate

        # ----- dt data handling ----------------------------------------
        # Create related data object as the given configration
        self.dt = obj_datamodel.get_data_object()
        self.cab = obj_cab.get_data_object()
        self.dt.DEBUG = DEBUG
        
        # show row data input
        trace = Trace(self.dt, None, self.cab)
        trace.app_ins()     # show list of app inputs
        trace.cyc_pid()     # show cycle graph
        trace.pnt_lst()     # show cycle graph
        
        # Create related object as the given configration
        self.obj_control = ""

        # 1: Standard
        # self.dt.ICYCL == 1:
        self.obj_control = CycleType(self.dt, self.cab)

        # INCTRL: 0 = none
        #         1 = adjust evaporator areas,
        #         2 = adjust fresh food section tempeature
        #         3 = adjust freezer    section tempeature
        #         4 = switching valve(only one section is cooled at a time)
        #         5 = solenoid valve 
        #               or fan control provides evaporator capacity 
        #               to only one cabinet during part of the cycle

    def print_scr_rep(self, is_solution):
        def print_fixed_width(str_data, rept=-1):
            lng_width = 50
            if rept != -1:
                str_data *= rept

            str_extenstion = ''
            if len(str_data) < lng_width:
                str_extenstion = ' ' * (lng_width - len(str_data))

            print('|' + str_data + str_extenstion + '|')

        print('\n')
        print_fixed_width('=', 50)

        if is_solution:
            print_fixed_width('     Cycle. App Done Succufully')
            print_fixed_width('     was create on current directory ')

            print_fixed_width(' ')
            print_fixed_width(' ')

            print_fixed_width('     Input File: ' + self.str_FILE_CYC_INPUT)
            print_fixed_width('     Output File: ' + self.str_FILE_CYCLE_OUTPUT)
            print_fixed_width(' ')

        else:
            print_fixed_width('     Cycle. App is faied')
            print_fixed_width('     No slotion is found')
            print_fixed_width('     Check your inputs')

            print_fixed_width(' ')
            print_fixed_width(' ')

            print_fixed_width('     Input File: ' + self.str_FILE_CYC_INPUT)
            print_fixed_width(' ')

        print_fixed_width('     Configration: ' + str(self.cab.IRFTYP))
        print_fixed_width('=', 50)
