# Python Import ====================
import math
from abc import ABC, abstractmethod

# User Import ======================
from common_classes.QData import QData
from common_classes.Unit import Unit
from cycle_classes.CycleSolver import *
from cycle_classes.CoolPrp import *

from cycle_classes.ShowInput import *
from cycle_classes.ErrorException import ErrorException

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Control class
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class CycleType_Abstract (ABC):

    def __init__(self, objdata):
        self.dt = objdata

        self.setup_vars_for_all_types()
        self.setup_vars_extra()

        #======= Create basic object for coolProp
        self.objCP = CoolPrp()
        # set be will be made later self.objCP.setup('R12')

        self.obj_show = ShowInput(objdata)

    # Abstract methods
    # -----------------------------------------------------------
    # Job 			: Change units, from the give SI units. (individual for every sub-class)
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
    @abstractmethod
    def adjust_units(self):
        pass

    # -----------------------------------------------------------
    # Job 			: inialize extra varibale in dt object with value (individual for every sub-class)
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------

    @abstractmethod
    def setup_vars_extra(self):
        pass

    # -----------------------------------------------------------
    # Job 			: Calculate cycle data, (individual for every sub-class)
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------

    @abstractmethod
    def calculte(self):
        pass

    # -----------------------------------------------------------
    # Job 			: Adjsut input data
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
    @abstractmethod
    def adjust_input(self):
        pass
    # -----------------------------------------------------------
    # Job 			: Adjsut input data for all types
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------

    def adjust_input_for_all_types(self):
        QZ_NET = 0.0

        # set Temperatue 17.11 C
        self.dt.TS5 = 256.0

        # initialize error code for liquid line anti-sweat heat
        self.dt.I_LIQUID_LINE = 0

        # get compressor file name
        # self.dt.FILMAP1 = self.getCompressorFileName(
            # self.dt.FILMAP1_CODE) + ".cmp"

        # Binary interaction parameter (BIP)
        # for int_ref in range(1, 2 + 1):
            # self.dt.F[2][1][int_ref] = self.dt.F[1][2][int_ref]
            # self.dt.F[3][1][int_ref] = self.dt.F[1][3][int_ref]
            # self.dt.F[3][2][int_ref] = self.dt.F[2][3][int_ref]

        # Delat Temp Refrigerant Exit Superheat (C) Or Quality (0-1)
        self.dt.QUALTY = self.dt.DTSPEI[:]

        # if IDFRST(Manual Defrost) =1  (i.e Autoamtic ) DFSTCYC :Closed-Door
        # Automatic Defrost (W)
        if(self.dt.IDFRST == 1):
            self.dt.DFSTCYC = 0.0

        #  zero condenser heat loads to cabinet and evaporators
        #
        self.dt.Q_CND_FF = 0.0
        self.dt.Q_CND_FZ = 0.0
        self.dt.Q_HXS_FF = 0.0
        self.dt.Q_HXS_FZ = 0.0
        self.dt.CONDF_IN_WALL = 0.0
        self.dt.CONDZ_IN_WALL = 0.0
        self.dt.FF_AIR = 0

        self.dt.DUTYC = 0.5
        self.dt.IRFTPL = self.dt.IRFTYP
        self.dt.IFAN = 0

        # Python comment: save original ICYCL value
        self.dt.ICYCLS = self.dt.ICYCL

        # Python comment: if 4- chest freezer, change it to 2- two-door
        # bottom-mount befrigerator/freezer
        if(self.dt.ICYCL == 4):
            self.dt.ICYCL = 2

    # -----------------------------------------------------------
    # Job 			: Adjsut unit for SI to Britch
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
    def adjust_units_for_all_types(self):
        # Python - Temperatue input (C) and is converted to K
        self.dt.FILE_NAME = self.dt.FILE_NAME + '.cmp'
        # TOL_FRSH 
        # TOL_FRZ 
        # TOL_COND
        self.dt.TS1= [temp_c + 273.11 for temp_c in self.dt.TS1]
        self.dt.TS3= [temp_c + 273.11 for temp_c in self.dt.TS3]
        self.dt.DTSBCI= [temp_c + 273.11 for temp_c in self.dt.DTSBCI]
        self.dt.SUPIHX= [temp_c + 273.11 for temp_c in self.dt.SUPIHX]
        
        for item in range(0, len(self.dt.TSPECI)):
            if(self.dt.TSPECI[item] > 0.0):
                self.dt.TSPECI[item] = self.dt.TSPECI[item] + 273.11
            
        self.dt.TS5 = self.dt.TS5 + 273.11
        self.dt.TROOM = self.dt.TROOM + 273.11
        self.dt.FFTEMP = self.dt.FFTEMP + 273.11
        self.dt.FZTEMP = self.dt.FZTEMP + 273.11
        
        # precent
        self.dt.CEI = [percet / 100.0 for percet in self.dt.CEI]
        self.dt.SEFFI = [percet / 100.0 for percet in self.dt.SEFFI]
        self.dt.MEFF = [percet / 100.0 for percet in self.dt.MEFF]
        self.dt.QCAN = [percet / 100.0 for percet in self.dt.QCAN]
        self.dt.QHILO = [percet / 100.0 for percet in self.dt.QHILO]
        
        # kpa to pa
        self.dt.DPC = [kpa * 1000 for kpa in self.dt.DPC]
        self.dt.DPE = [kpa * 1000 for kpa in self.dt.DPE]
        
        # others
        self.dt.FZQON = self.dt.FZQOFF
        self.dt.FZQ = self.dt.FZQOFF
        self.dt.FROSTFS = self.dt.FROSTF
        self.dt.FROSTZS = self.dt.FROSTZ

        # error ===CONVERT UNITS need adjust may be move to other class
        print ("=== to be checked in Cycle.type =======")
        self.dt.CONDF = self.dt.FFQ - self.dt.FFSEN - self.dt.FFLAT \
            - self.dt.FFHTQ - self.dt.FROSTF - self.dt.FFREFQ - self.dt.FFPENA

        self.dt.CONDZ = self.dt.FZQ - self.dt.FZSEN - self.dt.FZLAT - \
            self.dt.FZHTQ - self.dt.FROSTZ - self.dt.FZREFQ - self.dt.FZPENA

        RHOCPF = 316.8 / self.dt.TS5
        self.dt.CFMF = 1.8961 * (RHOCPF * self.dt.CFMF) / 0.4720
        
    # -----------------------------------------------------------
    # Job 			: add extra var for all types
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
    def setup_vars_for_all_types(self):
        self.dt.ETAF = 0.0
        self.dt.ETAV = 0.0
        self.dt.MROLD = 0.0

        self.dt.DUTYZ = 0.0
        self.dt.CAPZ = 0.0

    # -----------------------------------------------------------
    # Job 			: add extra var for all types except type 2
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
    def setup_vars(self):
        
        self.dt.INCTRL = 0  # input is given only in Type 2
        self.dt.IFREZI = [1, 1, 1]

        self.dt.TS5 = 300  # set non zero value, prevent calculation error
        self.dt.CFMF = 300  # set non zero value, prevent calculation error
        self.dt.AREAFZ = 300  # set non zero value, prevent calculation error
        self.dt.UAF = 300  # set non zero value, prevent calculation error
        self.dt.DPF = 3  # set non zero value, prevent calculation error

    # -----------------------------------------------------------
    # Job 			: prepare calling data to Cycle analysis
    # Input 		: Item in array to use
    #
    # Output		:
    # -----------------------------------------------------------
    def prepare_Data4Cycle(self, lng_item):

        # # convert to other units =========

        # self.dt.UDSC = self.dt.UDSCI[lng_item] # * 3.600
        # self.dt.UTPC = self.dt.UTPCI[lng_item] # * 3.600
        # self.dt.USCC = self.dt.USCCI[lng_item] # * 3.600
        # self.dt.UTPE = self.dt.UTPEI[lng_item] # * 3.600
        # self.dt.USUPE = self.dt.USUPEI[lng_item] # * 3.600

        # self.dt.SIZE = self.dt.SIZEN[lng_item] # / 0.252
        # self.dt.MREF = self.dt.MREFI[lng_item] # * 2.20462
        
        # # from cm3 to cu-inch compressor displacement
        # self.dt.DISPLC[lng_item] = self.dt.DISPLC[lng_item] # / 16.3871

        print ("===============to be checked ============cycle type====")
        RHOCPC = 316.8 / self.dt.TS1[lng_item]
        RHOCPE = 316.8 / self.dt.TS3[lng_item]

        self.dt.CFMC = 1.8961 * \
            (RHOCPC * self.dt.CFMCI[lng_item]) / 0.4720
            
        self.dt.CFME = 1.8961 * \
            (RHOCPE * self.dt.CFMEI[lng_item]) / 0.4720

        # if(self.dt.IMAP == 1):
            # self.dt.SPEED = self.dt.SPEEDN * self.dt.SPEEDI[lng_item]

        if(self.dt.IFREZI[lng_item] != 0):
            self.dt.UAF = 3.600 * self.dt.UAF

        # ============= end of data to be checked ==========
        
        # Python change from L/s to cm3/s to be checked
        self.dt.CFMEI[lng_item] = self.dt.CFMEI[lng_item] * 1000


    # -----------------------------------------------------------
    # Job 			: prepare calling parameter for fror Cycle analysis
    # Input 		: lng_item cycle number, N.B cycle type 3 only which has two cycles
    #
    # Output		:
    # -----------------------------------------------------------
    def call_cycle(self, lng_item):
        if self.getRefName(self.dt.IR[1][1])=="":
            raise ErrorException('Error refrigerant code: ', 'cyt1000')
            return
        
        print ("Using Ref. ", self.getRefName(self.dt.IR[1][1]) )
        
        self.objCP.setup(self.getRefName(self.dt.IR[1][1]))  # 'R12'
        
        objCycleSolver = CycleSolver(objCP=self.objCP
                            , objData=self.dt
                            , lng_item=lng_item
                            , NCYC=1
                            )

        #=== solve
        objCycleSolver.solveCycle()
        objFeedback = objCycleSolver.getSolution()
        
        return objFeedback # obj_cycle.cycle()


    def getRefName(self, lng_Code):
        lstRefList = [
            'R11',
            'R12',
            'R13',
            'n-C5',
            'R14',
            'R22',
            'R23',
            'R113',
            'R114',
            'R142b',
            'R152a',
            'R216a',
            'R125',
            'R143a',
            'R134a',
            'R123',
            'Rc318',
            'R134',
            'RC270',
            'R141b',
            'i-C5',
            'R290',
            'R600',
            'R600a',
            'R32',
            'R1270',
            'R124',
            'R115',
            'CE-216',
            'E-125',
            'R123a',
            'R143',
            'R218',
            'E-134']
            
        if lng_Code < 1 or lng_Code > len(lstRefList):
            return ""

        else:
            return lstRefList[lng_Code - 1]
            
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 1 - Standard
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_1Standard (CycleType_Abstract):
    '''
    def view (self, str_file_cycle, str_path_cycle = ""):
            # SET UP PARAMETERS FOR SUMMARY OUTPUT
            #
            if(self.dt.IFAN != 2)  :
                    self.dt.DUTYN[N] = self.dt.DUTYC
                    self.dt.WCOMP[N] = self.dt.W / self.dt.CORR_COP / 1.0548

            #if(ICYCL != 2)  :
            self.dt.QE_NET = self.dt.QE - self.dt.Q_HXS_FF
            self.dt.QEL[N] = self.dt.QE_NET
            self.dt.FLOWN[N]  = self.dt.FLOW
            self.dt.COPRN[N]  = self.dt.COPR
            self.dt.COPCYC[N] = self.dt.CORR_COP

            obj_view = ViewCycle(self.dt, str_file_cycle, str_path_cycle)
            obj_view.show_rep()
    '''

    def calculte(self):
        self.dt.ITYPE = 1
        self.dt.IEVAP = 0
        
        self.dt.TS5 = -300.0 # 256
        self.dt.DPF = 0.0
        
        # self.dt.XEXITE = self.dt.QUALTY[1]
        
        # not used var self.dt.DTSUPI = self.dt.SUPIHX[1]
        # not used var - self.dt.ISPEC = self.dt.ISPECI[1]
        
        # show all data from input file
        self.obj_show.show(self.dt.ITYPE)

        return self.call_cycle(self.dt.ITYPE)

    def setup_vars_extra(self):
        self.setup_vars()

    def adjust_units(self):
        self.adjust_units_for_all_types()

    def adjust_input(self):
        self.adjust_input_for_all_types()
        self.prepare_Data4Cycle(1)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 2 - Lorenz
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_2Lorenz (CycleType_Abstract):

    def calculte(self):
        pass

    def setup_vars_extra(self):
        pass

    def adjust_units(self):
        self.adjust_units_for_all_types()

    def adjust_input(self):
        self.adjust_input_for_all_types()
        self.prepare_Data4Cycle(1)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 2 - Lorenz
#				Control Method 4 -switching valve
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_2Lorenz_4swtchVLV (Type_2Lorenz):
    def adjust_input(self):
        super.adjust_input()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 2 - Lorenz
#				Control Method 5 -solenoid valve
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_2Lorenz_5solindVLV (Type_2Lorenz):
    def adjust_input(self):
        #	if INCTRL == 5 i.e solenoid valve or fan control
        #	set IDFRST =1 Manual Defrost to Yes
        self.dt.IDFRST = 1
        super.adjust_input()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 2 - Lorenz
#				Control Method any other ctrl method, but not 4 & 5
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_2Lorenz_ctrlOthers (Type_2Lorenz):
    def adjust_input(self):
        super.adjust_input()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 3 - Dual Loop
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_3DualLoop (CycleType_Abstract):

    def calculte(self):
        pass

    def setup_vars_extra(self):
        self.setup_vars()

    def adjust_units(self):
        self.adjust_units_for_all_types()

    def adjust_input(self):
        self.adjust_input_for_all_types()

        # get compressor file name
        # self.dt.FILMAP2 = self.getCompressorFileName(
            # self.dt.FILMAP2_CODE)

        self.prepare_Data4Cycle(1)
        #self.prepare_Data4Cycle (2)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 4 - Dual Evap
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_4DualEvap (CycleType_Abstract):

    def calculte(self):
        pass

    def setup_vars_extra(self):
        self.setup_vars()

    def adjust_units(self):
        self.adjust_units_for_all_types()

    def adjust_input(self):
        self.adjust_input_for_all_types()
        self.prepare_Data4Cycle(1)
