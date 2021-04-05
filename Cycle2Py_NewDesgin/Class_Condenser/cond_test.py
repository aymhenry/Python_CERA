# Python Import ==================

# User Import ======================
from Condenser import *

#========================================================
ICOND = 0
objCondenser = Condenser()
objCond = objCondenser.getObject(objCP=None, ICOND=ICOND)

# -------------------------------------

R12_TABLE_READING = 200.0 - 27.10795349661135

'''
>>>>         QCONDS, QCONDC, QSCC= 251.95112653472043 209.1199773325663 0.0
     MREF =  0.047917168814271854
   watt CFMC = 175.44549885936618
        ICOND = 0
        TOL_COND = 0.075
        TOL_MASS = 0.01
        TS1 = 308.11
        TC,JC = [0.0, 311.6618877777778, 316.6618877777778, 0.0] 2
        T4 = 316.6618877777778
        H14 = 28942.719831096856
        H4 = 8428.584292676984
==================================
     UACOND =  30.782357693866953



>>>>         QCONDS, QCONDC, QSCC= 210.21853538661313 75.15606379210476 0.0
    kg/hr MREF =  0.049913717514866514
   watt CFMC = 175.44549885936618
        ICOND = 0
        TOL_COND = 0.075
        TOL_MASS = 0.01
        TS1 = 308.11
        TC,JC = [0.0, 311.6618877777778, 0.0, 0.0] 1
        T4 = 311.6618877777778
        H14 = 27704.574145826966
        H4 = 7811.8171566003
==================================
     UACOND =  28.556372797340597
     '''
# -------------------------------------

MROLD = 6  # kg/hr old Initial Guess For Refrigerant Mas Flow Rate
MREF = 5.8  # kg/hr Initial Guess For Refrigerant Mas Flow Rate
N_COND = 1  # Refrigeration Type 1,2,3,4,5,6 or 7

TS1 = 308.11  #  heat transfer fluid (htf) temperature entering condenser
TC = [0.0, 311.66, 316.66, 0.0]  #  Condenser Temp Traials
JC = 1  # Current trailar 1 the first time 2 others

T4 = 316.66  # Temp K condenser outlet 
H4 = 1000*(8428.58  /120.91 + R12_TABLE_READING) # Enthalpy condenser outlet J/kg
H14 = 1000*(28942.71  /120.91 + R12_TABLE_READING) # Enthalpy condenser inlet J/kg

ICOND = 1  # 0 Nat. cooling, 1 Cross, OR 2 Counter
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

CFMC = 175.44    # watt/K

UACOND = 28.556 # check the units unknow, need to be in SI

QCONDS = 210.218  # check the units unknow, need to be in watt
QCONDC = 75.156  # check the units unknow, need to be in watt
QSCC = 0    # condenser subcooling heat transfer watt

print("==================input============")
print(" old Initial Guess For Refrigerant Mas Flow Rate kg/hr MROLD = ", MROLD)
print(" Initial Guess For Refrigerant Mas Flow Rate      kg/hr MREF = ", MREF)

print("   watt CFMC =", CFMC)
print("        ICOND =", ICOND)

print(" heat transfer fluid (htf) temperature entering condenser  TS1 =", TS1)
print(" Condenser Temp                 TC =", TC)
print(" Iteration count 1 or others    JC =", JC)

print(" condenser outlet temperature   T4 =", T4)
print(" condenser inlet Enthalpy      H14 =", H14)
print(" condenser outlet Enthalpy      H4 =", H4)

print("     UACOND = ", UACOND)
print("       watt QCONDS =", QCONDS)
print("       watt QCONDC =", QCONDC)
print("       watt QSCC =", QSCC)

lstRest = objCond.cond(T4=T4, H4=H4, H14=H14, TS1=TS1,
               TC=TC, JC=JC,
               QCONDS=QCONDS, QCONDC=QCONDC, QSCC=QSCC,
               MROLD=MROLD, MREF=MREF,
               UACOND=UACOND, CFMC=CFMC,
               ICOND=ICOND)

print("\n\n==================Output============")
print("           K TS2 =", lstRest[0])
print("            K TC =", lstRest[1])
print("              JC =", lstRest[2])
print("           ICONC =", lstRest[3])


