# Python import
import sys

# User import
from CoolPrp import *

# =====================

class CycleOmar():
    # Cycle Standard inputs
    
    # NC = 1
    # not used see strGas IR = [0.0, 2, 1, 1, 0.0, 0.0]
    # XM = [0.0, 1, 0, 0, 0.0, 0.0]
    # F = [[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0, 0, 0.0, 0.0, 0.0], [0.0, 0, 0.0, 0, 0.0, 0.0, 0.0], [0.0, 0, 0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]]
    TS1 = 308.11 # K or 35.0 C
    TS3 = 261.776  # K or -11.334 C
    # TS5 = -300.0  # K or  -573.11
    
    # MEFF = 0.0
    # QHILO = 0.0
    # QCAN = 0.0
    # DPC = 4.72
    # DPE = 7.2
    # DPF = 0.0
    # ETHX1 = 0.8
    # ETHX2 = 0.8
    # DISPLC = 0.4009251179281264
    # NCYC = 1
    # FROSTF = 0.0
    # FROSTZ = 0.0
    
    # ICAB = 1
    # IRFTYPE = 1
    # ICYCL = 1
    # ICYCLS = 1
    # IDFRST = 0

    # New Input
    strGas = 'R12' # replacment of IR
    Qcab = 80 # Cab load in Watts
    DTsub = 5 # delat_T subcooling
    X5 = 0.2
    Eta = 0.9
    
    # 1 - COMPRESSOR INLET (SATURATED VAPOR)
    # 2 - COMPRESSOR DISCHARGE
    # 3 - CONDENSER DEW POINT
    # 4 - CONDENSER OUTLET
    # 5 - INLET TO FRESH FOOD EVAPORATOR
    # 6 - LIQUID LINE OUTLET FROM HIGH TEMP INTERCHANGER
    # 7 - OUTLET FROM FRESH FOOD EVAPORATOR
    # 8 - INLET TO FREEZER EVAPORATOR
    # 9 - OUTLET FROM FREEZER EVAPORATOR
    # 10 - LIQUID LINE OUTLET FROM LOW TEMP INTERCHANGER
    # 11 - CONDENSER BUBBLE POINT
    # 12 - FRESH FOOD EVAPORATOR DEW POINT
    # 13 - SUPERHEATED GAS LEAVING THE HIGH TEMP INTERCHANGER
    # 14 - CONDENSER INLET
    # 15 - INTERNAL VARIABLE (NOT SHOWN) FOR EVAP DEW POINT
    # 16 - LIQUID LINE STATE AFTER HEAT LOSS TO CABINET AND MULLION
    
    def __init__(self):
        self.objCP = CoolPrp()
        self.objCP.setup(CycleOmar.strGas)
        if self.objCP.isError():
            sys.exit('CycleOmar-1000: ' + self.objCP.err_description())
        
    def startCal(self):
        T12 = CycleOmar.TS3 - 5 # Point 12: FRESH FOOD EVAPORATOR DEW POINT
        T11 = CycleOmar.TS1 + 5 # Point 11: CONDENSER BUBBLE POINT
        T4 = T11 - CycleOmar.DTsub
        
        # Sat. Pressure @ point 4 - CONDENSER OUTLET
        P4 = self.objCP.Property('P', T=T4, X=0)

        # Sat. Pressure @ point 12 - FRESH FOOD EVAPORATOR DEW POINT
        P5 = P7 = P12 = self.objCP.Property('P', T=T12, X=0)
        
        # get H liquild @ T12
        H7 = self.objCP.Property('H', T=T12, X=0)
        
        # get H gas @ T12
        H12g = self.objCP.Property('H', T=T12, X=1)
        
        H5 = H7 + CycleOmar.X5 * (H12g - H7)
        m_ref = (2 * CycleOmar.Qcab) / (H7 - H5)
        
        # Solve Qlas
        # ----------------------------------------
        P11 = self.objCP.Property('P', T=T11, X=1)
        H4 = self.objCP.Property('H', P=P11, T=T4)
        
        Qlas = 40 # what ???
        H16 = H4 - Qlas / m_ref
        
        P16 = P4
        T16 = self.objCP.Property('T', P=P16, H=H16)

        # Solve Evap
        # ----------------------------------------
        T7 = self.objCP.Property('T', P=P7, X=1)
        H7 = self.objCP.Property('H', P=P7, T=T7)

        # Solve Interchanger
        # ----------------------------------------
        T13 = T7 + CycleOmar.Eta *  (T16 - T7)
        P13 = 101325 # what ???
       
        H13 = self.objCP.Property('H', P=P13, T=T13)
        V13 = self.objCP.Property('V', P=P13, T=T13)
        S13 = self.objCP.Property('S', P=P13, T=T13)
        
        # Iteration
        # ----------------------------------------
        P13_sat = self.objCP.Property('P', T=T13, X=1)
        P11_sat = self.objCP.Property('P', T=T11, X=1)
        P2 = P11_sat - P13_sat
        
        # H2 = self.objCP.Property('H', P=P2, T=T2)
        # T2 = self.objCP.Property('T', P=P2, T=T2)
        # S2 = self.objCP.Property('S', P=P2, T=T2)
        
        flt_Err = 10000 # random big error
        int_try = 0 # trial count
        
        boo_status = False
        
        MAX_TRY = 20 # max. number of traials
        ACCPTED_ERR = 0.01
        
        while int_try < MAX_TRY:
            int_try = int_try + 1
            flt_Err = flt_Err / 2 # set some equation
            
            if flt_Err <= ACCPTED_ERR:
                boo_status = True
                break
            print ("Iteration Count: ", int_try)
        
        print ("----Final Results--------------")
        if not boo_status:
            print ("\t Iteration Falied, Error =", flt_Err)
        else:
            print ("\t Iteration Succed, Error =", flt_Err)        
            
# =====================
def main():
    obj_cyc = CycleOmar()
    obj_cyc.startCal()
    
# =====================
if __name__ == '__main__':
    main()
