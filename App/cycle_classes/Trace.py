# Python Import ====================
import inspect


# User Import ======================
from cycle_classes.ShowInput import *

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job 			: Show data in some selected points
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class Trace:
    # if False no tracing at all,
    # must be true to print anythins
    DATA_ALL = True 
    
    DR_OMAR = False
    
    COMP_INS = False   # for comp_ins
    COMP_OUT = False   # for comp_out
    COND_IC = False    # for cond_ic
    EVAP_IE = False    # for evap_ie
    FRSH_INS = False   # for frsh_ins
    FRSH_OUT = False   # for frsh_out
    CYCLE_OUT = False  # for cycle_out
    APP_INS = True     # for app ins
    CYC_PID = True     # for cycle pid
    PNT_LST = False     # for list of points name
    
    RANDAM = False      # for randam
    
    def __init__(self, dt=None, obj_cycletype=None, cab=None):
        self.dt = dt     # app data
        self.cab = cab     # app data
        self.ds = obj_cycletype     # CycleSolver object

    @staticmethod
    def file_line():
        #  Returns the current file name & line number in our program
        str_file = inspect.getfile(inspect.currentframe().f_back.f_back)
        lng_line = inspect.currentframe().f_back.f_back.f_lineno
        return "\tFile: " + str_file + "\n\tLine: " + str(lng_line)
        
    @staticmethod
    def is_canc_print(b_Flag):
        if not (b_Flag and Trace.DATA_ALL):
            return True
        else:
            return False

    def dr_omar(self, str_msg=''):    # inspection to be done by Dr Omar.
        if self.is_canc_print(Trace.DR_OMAR):
            return
            
        print("Description: Dr Omar Inspection Required" + " - " + str_msg)
        
        print(self.file_line(), "\n")
        # print("\n")
        
    def pnt_lst(self):
        if self.is_canc_print(Trace.PNT_LST):
            return
            
        print('\t1- Compressor inlet (saturated vapor)')
        print('\t2- Compressor discharge')
        print('\t3- Condenser dew point')
        print('\t4- Condenser outlet')
        print('\t5- Inlet to fresh food evaporator')
        print('\t6- Iiquid line outlet from high temp interchanger')
        print('\t7- Outlet from fresh food evaporator')
        print('\t8- Inlet to freezer evaporator')
        print('\t9- Outlet from freezer evaporator')
        print('\t10- Liquid line outlet from low temp interchanger')
        print('\t11- Condenser bubble point')
        print('\t12- Fresh food evaporator dew point')
        print('\t13- Superheated gas leaving the high temp interchanger')
        print('\t14- Condenser inlet')
        print('\t15- Internal variable (not shown) for evap dew point')
        print('\t16- Liquid line state after heat loss to cabinet and mullion')
        print('\n\n')

    def cyc_pid(self):
        if self.is_canc_print(Trace.CYC_PID):
            return
            
        obj_show = ShowInput(None, None)
        obj_show.graph()
        
    def app_ins(self):
        if self.is_canc_print(Trace.APP_INS):
            return
            
        obj_show = ShowInput(self.dt, self.cab)
        obj_show.show()
    
    def randam(self, *args, **kargs):
        # how to call
        # self.trace.randam ("Some Data", T=5)
        
        if self.is_canc_print(Trace.RANDAM):
            return
            
        int_count = 1
        for arg in args:
            print("        >>> " + str(int_count) + " = ", arg)
            int_count += 1

        for key, value in kargs.items():
            print("        " + key + " = ", value)
            int_count += 1
                    
    def cycle_out(self):
        if self.is_canc_print(Trace.CYCLE_OUT):
            return
        print("\n----------------------------")
        # n = 0
        print("#\t\tT C\tP kPa\tH kj/kg")
        for n in range(1, 17):
            print(n, "\t%9.2f\t%9.2f\t%9.2f"
                     % (self.ds.T[n] - 273.11,
                        self.ds.P[n]/1000, self.ds.H[n]/1000))

        print("----------------------\n\n\n")

    def frsh_out(self, ICONE):
        if self.is_canc_print(Trace.FRSH_OUT):
            return
            
        print("    Output from frsh - main iteration function ...")
        print("        self.TE=", self.ds.TE)
        print("      not used  TS4=", self.ds.TS4)
        print("              ICONE=", ICONE)   
        
    def frsh_ins(self):
        if self.is_canc_print(Trace.FRSH_INS):
            return
            
        print("    Input to frsh - main iteration function ...")
        print("        self.H[5]=", self.ds.H[5])
        print("        self.H[7]=", self.ds.H[7])
        print("        self.TE=", self.ds.TE)
        print("        self.TS3=", self.ds.TS3)
        print("        self.QFRSH=", self.ds.QFRSH)
        print("        self.MREF=", self.ds.MREF)
        
    def evap_ie(self, ICONE):
        if self.is_canc_print(Trace.EVAP_IE):
            return
            
        print("\n  Cond iter.=", self.ds.IC,
              " Evaporator Iter. Number --> IE=", self.ds.IE)
        print("    ===========================================")
        print("    ICONE if 1, iteration done > ", ICONE)

    def cond_ic(self):
        if self.is_canc_print(Trace.COND_IC):
            return
            
        print("\n-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
        print("Condenser Iteration Number self.IC = ", self.ds.IC)
        print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
            
    def comp_ins(self):
        if self.is_canc_print(Trace.COMP_INS):
            return
            
        print("\n\nInput to compressor -------------------")
        print("\tPSUCT = self.P[1] = ", self.ds.P[1])
        print("\tTSUCT = self.T[1] = ", self.ds.T[1])
        print("\tVSUCT = self.V[1] = ", self.ds.V[1])

        print("\tPDISC = self.P[2] = ", self.ds.P[2])
        print("\tMREF = self.MREF = ", self.ds.MREF)
        print("\n\n")
    
    def comp_out(self):
        if self.is_canc_print(Trace.COMP_OUT):
            return
        
        print("Compressor output")
        print('\tCompressor exit Temp K      TSP = ', self.ds.dicRest['TSP'])
        print('\tDischare Temp K           TDISC = ', self.ds.dicRest['TDISC'])
        print('\tDischare Enthalpy    j/kg  HOUT = ', self.ds.dicRest['HOUT'])
        print("""\tcompressor shell loss
              normalized to power input j/kg QCAN  = """,
              self.ds.dicRest['QCAN'])

        print('\tSuction sp.volume m3/kg    VSUC = ', self.ds.dicRest['VSUC'])
        print('\tDischare sp.volume m3/kg    VV2 = ', self.ds.dicRest['VV2'])
        print('\tCp/Cv value                GAMA = ', self.ds.dicRest['GAMA'])
        print('\tCompressor Efficiency   %  ETAC = ', self.ds.dicRest['ETAC'])
        print('\tRefrigerant Mas Flow Rate  kg/hr  MREF = ',
              self.ds.dicRest['MREF'])
