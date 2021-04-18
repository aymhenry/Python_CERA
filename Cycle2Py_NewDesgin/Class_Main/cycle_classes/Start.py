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
    def main(self):
        self.data_prepare()  # assign value to dt
        
        # it will return objSolution object (has cycle solution)
        objSolution = self.calculte()  # calculate heat rate
        
        '''
        try:
            obj_param = self.calculte ()        # calculate heat rate

        except ValueError as err_description: # OSError
            print ("Fatal program error ... system terminated")
            print (str(err_description) + "\n\n")
            print ("=======================================")
            print ("Expected reasone, none propoer input data")
            print ("=======================================\n\n")
            print ("               sys.exit('3100')  # terminat application")
            print ("=======================================\n\n")
        '''
        obj_view = View(
            self.dt,
            objSolution,    # will be named ds for short
            self.str_FILE_CYCLE_OUTPUT,
            self.str_path_cyc_out
            )
            
        obj_view.show_overall()
        obj_view.show_rep()        

    # -----------------------------------------------------------
    # Job             : Calaculte heat balance, the main app target.
    # Input         :
    #
    # Output        :
    # -----------------------------------------------------------
    def calculte(self):
        # run object in CycleType.py according to cycle type given
        # it will return objSolution object (has cycle solution)
        return self.obj_control.calculte()

    # -----------------------------------------------------------
    # Job             : Preprae the main data object & control object
    # Input         :
    #
    # Output        :
    # -----------------------------------------------------------
    def data_prepare(self):
        # Set main data file name
        obj_datamodel = CycleDataModelBuiler(self.str_FILE_CYC_INPUT,
                                             self.str_path_cyc_in)

        # check if error, if so exit application
        if obj_datamodel.isError():
            print("Error Opening file")
            print(obj_datamodel.err_description())  # print error description
            self.obj_datamodel = ""     # clean object and close file
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

        # show row data input
        trace = Trace(dt=self.dt)
        trace.app_ins()     # show list of app inputs
        trace.cyc_pid()     # show cycle graph
        trace.pnt_lst()     # show cycle graph
        
        # Create related object as the given configration
        self.obj_control = ""

        # 1: Standard
        if self.dt.ICYCL == 1:
            self.obj_control = Type_1Standard(self.dt)

        # 2: Lorenz
        elif self.dt.ICYCL == 2:
            print("Type 2 is not supported.")  # print error description
            sys.exit('9001')                            # terminate

            # INCTRL: 0 = none
            #         1 = adjust evaporator areas,
            #         2 = adjust fresh food section tempeature
            #         3 = adjust freezer    section tempeature
            #         4 = switching valve(only one section is cooled at a time)
            #         5 = solenoid valve 
            #               or fan control provides evaporator capacity 
            #               to only one cabinet during part of the cycle
            
            # if self.dt.INCTRL == 4:
            #    self.obj_control = Type_2Lorenz_4swtchVLV(self.dt)

            # elif self.dt.INCTRL == 5:
            #    self.obj_control = Type_2Lorenz_5solindVLV(self.dt)

            # else:
            #    self.obj_control = Type_2Lorenz_ctrlOthers(self.dt)

        # 3: Dual Loop
        elif self.dt.ICYCL == 3:
            self.obj_control = Type_3DualLoop(self.dt)

        # 4: Dual Evap
        elif self.dt.ICYCL == 4:
            self.obj_control = Type_4DualEvap(self.dt)

        # adjust default vars, according to basic input values
        self.obj_control.adjust_input()

        # Convert to units
        self.obj_control.adjust_units()

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
