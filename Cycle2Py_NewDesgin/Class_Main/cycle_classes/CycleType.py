# Python Import ====================

# User Import ======================
from cycle_classes.CycleSolver import *
from cycle_classes.CoolPrp import *

from cycle_classes.ErrorException import ErrorException

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Control class
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class CycleType:

    def __init__(self, objdata):
        self.objCP = None
        self.dt = objdata
        
        # --------------------------------------------------
        # Setup basic vars
        # --------------------------------------------------
        self.dt.ETAF = 0.0
        self.dt.ETAV = 0.0
        self.dt.MROLD = 0.0

        self.dt.DUTYZ = 0.0
        self.dt.CAPZ = 0.0
        
        self.dt.INCTRL = 0  # input is given only in Type 2
        # self.dt.IFREZI = [1, 1, 1]

        self.dt.TS5 = 300  # set non zero value, prevent calculation error
        self.dt.AREAFZ = 300  # set non zero value, prevent calculation error
        self.dt.UAF = 300  # set non zero value, prevent calculation error
        self.dt.DPF = 3  # set non zero value, prevent calculation error

        # --------------------------------------------------        
        # adjust default vars, according to basic input values
        # --------------------------------------------------
        self.dt.TS5 = 256.0  # set Temperatue 17.11 C

        # initialize error code for liquid line anti-sweat heat
        self.dt.I_LIQUID_LINE = 0

        if self.dt.IDFRST == 1:
            self.dt.DFSTCYC = 0.0

        #  zero condenser heat loads to cabinet and evaporators
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
        self.dt.ICYCLS = 1  # self.dt.ICYCL

        # --------------------------------------------------
        # Convert units
        # --------------------------------------------------
        self.dt.FILE_NAME += '.cmp'
                
        # Python - Temperatue input (C) and is converted to K
        self.dt.TS1 = [temp_c + 273.11 for temp_c in self.dt.TS1]
        self.dt.TS3 = [temp_c + 273.11 for temp_c in self.dt.TS3]

        for item in range(0, len(self.dt.TSPECI)):
            if self.dt.TSPECI[item] > 0.0:
                self.dt.TSPECI[item] += 273.11

        self.dt.TS5 += 273.11
        self.dt.TROOM += 273.11
        self.dt.FFTEMP += 273.11
        self.dt.FZTEMP += 273.11
                
        # kpa to pa
        self.dt.DPC = [kpa * 1000 for kpa in self.dt.DPC]
        self.dt.DPE = [kpa * 1000 for kpa in self.dt.DPE]
        
        # others
        self.dt.FZQON = self.dt.FZQOFF
        self.dt.FZQ = self.dt.FZQOFF
        
        self.dt.CONDF = self.dt.FFQ - self.dt.FFSEN - self.dt.FFLAT \
            - self.dt.FFHTQ - self.dt.FROSTF - self.dt.FFREFQ - self.dt.FFPENA

        self.dt.CONDZ = self.dt.FZQ - self.dt.FZSEN - self.dt.FZLAT - \
            self.dt.FZHTQ - self.dt.FROSTZ - self.dt.FZREFQ - self.dt.FZPENA

    def calculte(self):
        self.dt.TS5 = -300.0   # 256
        self.dt.DPF = 0.0
        self.dt.CFMF = 0  # by pass required value.
        
        if self.getRefName(self.dt.REF) == "":
            raise ErrorException('Error refrigerant code: ', 'cyt1000')

        print("Using Ref. ", self.getRefName(self.dt.REF))
        if self.dt.DEBUG:
            print("\t>>>>WORKING IN DEBUG mode")
            
        self.objCP = CoolPrp(self.dt.DEBUG)
        self.objCP.setup(self.getRefName(self.dt.REF))  # 'R12'
        
        objCycleSolver = CycleSolver(objCP=self.objCP,
                                     dt=self.dt,
                                     lng_item=1
                                     )

        # === solve
        objCycleSolver.solveCycle()
        if self.dt.IS_SOLTION:
            return objCycleSolver.getSolution()
        
        return None   # obj_cycle.cycle()

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
            