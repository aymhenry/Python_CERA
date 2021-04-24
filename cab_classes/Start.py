# Python Import ==================
import math
import sys
import datetime

# User Import ======================
from cab_classes.QCtrl import *
from cab_classes.CabData import CabData
# from common_classes.QData import QData

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
# Job            : Start Cab app
#
# Editor       : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class Start:

    def __init__(self,
                 cab_in=None,
                 cab_out=None,
                 cab2cyc=None,
                 path_cab_in=None,
                 path_cab_out=None
                 ):
                 
        self.obj_data = None    # object to save data
        self.obj_control = None    # object to point to object control on data

        # set member variables
        self.str_file_cab_input = None
        self.str_file_cab_output = None
        self.str_file_cycle_output = None

        self.str_path_cab = None
        self.str_path_cycle = None
        
        self.set_filenames(cab_in, cab_out, cab2cyc, path_cab_in, path_cab_in)
        
    def set_filenames(self,
                      cab_in=None,
                      cab_out=None,
                      cab2cyc=None,
                      path_cab_in=None,
                      path_cab_out=None
                      ):
                  
        self.str_file_cab_input = cab_in
        self.str_file_cab_output = cab_out
        self.str_file_cycle_output = cab2cyc

        self.str_path_cab = path_cab_in
        self.str_path_cycle = path_cab_out
        
    # ----------------------------------------------------------
    # Job            : Main app start up, driver for all others
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------

    def main(self, DEBUG):
        is_solution = True
        self.data_prepare()

        if DEBUG:
                self.calculte()        # calculate heat rate in Qxx classes
                self.calculte_cycle()    # claclulate cycle data

        else:
            try:
                self.calculte()        # calculate heat rate in Qxx classes
                self.calculte_cycle()    # claclulate cycle data
                            
            except():   # ValueError as err_description:   # OSError
                is_solution = False
                # sys.exit('CAB-3100')    # terminat application

        if is_solution:
            self.view()            # output results
            return True
            
        else:
            return False
    # ----------------------------------------------------------
    # Job            : output results a reported form
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def view(self):
        self.obj_control.view(self.str_file_cab_output
                              , self.str_file_cycle_output
                              , self.str_path_cab
                              , self.str_path_cycle
                              )

    # ----------------------------------------------------------
    # Job            : Calaculte heat balance, the main app target.
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def calculte(self):
        self.obj_control.calculte()

    # ----------------------------------------------------------
    # Job            : Calculte cycle data
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def calculte_cycle(self):
        self.obj_control.calculte_cycle()
    # ----------------------------------------------------------
    # Job            : Preprae the main data object & control object
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------

    def data_prepare(self):
        # Set main data file name
        obj_datamodel = CabData(self.str_file_cab_input,
                                        self.str_path_cab)
                                        
        # obj_datamodel.set_init_data(self.str_file_cab_input, self.str_path_cab)     # Input data file name

        # check if error, if so exit application
        if obj_datamodel.isError():
            print("Error Opening file")
            print(obj_datamodel.err_description())    # print error description
            sys.exit('CAB-3000')    # terminat application
            # -------------

        # read data list from file, and put values in variables
        obj_datamodel.build_var_list()

        # Is data is good, or exit application
        if obj_datamodel.isError():
            print(obj_datamodel.err_description())    # print error description
            sys.exit('CAB-3001')                            # terminate

        # Create related data object as the given configration
        self.obj_data = obj_datamodel.get_data_object()

        # Create related object as the given configration
        self.obj_control = ""
        
        # 1: Two door, top - mount refrigerator / freezer
        if self.obj_data.IRFTYP == 1:   # Mode 3    Top - mount refrigerator / freezer
            self.obj_control = QCtrl_Ql13(self.obj_data)

        # 2: Two door, bottom - mount refrigerator / freezer
        elif self.obj_data.IRFTYP == 2:   # Mode 8    Bottom - mount refrigerator / freezer
            self.obj_control = QCtrl_Ql8(self.obj_data)

        # 3: Side by side refrigerator / freezer
        elif self.obj_data.IRFTYP == 3:   # Mode 2    Side - by - side refrigerator / freezer
            self.obj_control = QCtrl_Ql2(self.obj_data)

        # 4: Chest freezer
        elif self.obj_data.IRFTYP == 4:   # Mode 5    Chest freezer
            self.obj_control = QCtrl_Ql5(self.obj_data)

        # 5: Upright freezer
        elif self.obj_data.IRFTYP == 5:   # Mode 7    Upright freezer
            self.obj_control = QCtrl_Ql467(self.obj_data)

        # 6: One door refrigerator
        elif self.obj_data.IRFTYP == 6:   # Mode 4    Single - door refrigerator
            self.obj_control = QCtrl_Ql467(self.obj_data)

        # 7: Two door refrigerator / freezer
        elif self.obj_data.IRFTYP == 7:   # Mode 3    Top - mount refrigerator / freezer
            self.obj_control = QCtrl_Ql13(self.obj_data)

        # Creates Vars & convert units
        self.obj_control.set_vars_common()        # Create commom in abstract class
        self.obj_control.setup_vars_extra()        # Create extra vars, indvidual for every class

        self.obj_control.setMode()                # set mode in commom in abstract class
        self.obj_control.set_ncctype()            # set ncctype in commom in abstract class

        # Calculate Volume
        self.obj_control.volume()

        self.obj_control.adj_unit_common()        # unit adjust in commom in abstract class

        self.obj_control.adjust_units()
        
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
            print_fixed_width('     Cab. App Done Succufully')
            print_fixed_width('     was create on current directory ')

            print_fixed_width(' ')
            print_fixed_width(' ')

            print_fixed_width('    Input File: ' + self.str_file_cab_input)
            print_fixed_width('    Output File: ' + self.str_file_cab_output)
            
            print_fixed_width("    Configration: " + str(self.obj_data.IRFTYP))
            print_fixed_width("    Mode        : " + str(self.obj_data.NMOD))
            
            print_fixed_width(' ')

        else:
            print_fixed_width('    Cab App is faied')
            print_fixed_width("      Fatal program error")
            
            print_fixed_width("      Expected resone, none propoer input data")
            print_fixed_width('      Check your inputs')

            print_fixed_width(' ')
            print_fixed_width(' ')

            print_fixed_width('     Input File: ' + self.str_file_cab_input)
            print_fixed_width(' ')

        print_fixed_width("    Configration: " + str(self.obj_data.IRFTYP))
        print_fixed_width("    Mode        : " + str(self.obj_data.NMOD))
        print_fixed_width('=', 50)
