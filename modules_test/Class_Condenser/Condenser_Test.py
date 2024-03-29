# Python Import ==================

# User Import ======================
from CoolPrp import *
from Condenser import *

# Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')

# UACOND
#========================================================
CFMC = 175.44    # to check the units - random value unknow
DTSUBC = 0    # Refrigerant Exit Subcooling, Deg C

ATOTC = 0.849  # m2 Total Heat Transfer Surface Area
MREF = 5.8  # kg/hr Refrigerant Mas Flow Rate
MROLD = 5.7  # kg/hr Refrigerant Mas Flow Rate old value
UA_FF_CND = 0  # watt/K , Cond: A/R In Fresh Food Section (Or Cabinet Walls)
UA_FZ_CND = 0  # watt/K , Cond: A/R In Freezer Section Walls (If Separate Section)
UA_FF_HXS = 0  # watt/K , Both: A/R In Fresh Food Section (Or Cabinet Walls)
UA_FZ_HXS = 0  # watt/K , Both: A/R In Freezer Section Walls (If Separate Section)
N_COND = 1  # Refrigeration Type 1,2,3,4,5,6 or 7

UDSC  = 15.167* 3600    # Desuperheating Heat Transfer Conductance,# W/m2-K
USCC  = 15.434 * 3600   # Subcooling Heat Transfer Conductance, # W/m2-K
UTPC  = 19.227 * 3600   # Two-Phase Heat Transfer Conductance, # W/m2-K
#=========================================================
R12_TABLE_READING = 200.0 - 27.10795349661135

TS1 = 308.11    # heat transfer fluid (htf) temperature entering condenser
TS3 = 261.776   # htf temperature entering fresh food evaporator
TS5 = -300.0    # htf temperature entering freezer evaporator

T14 = 360.44    # Temperature k CONDENSER INLET
H14 = 1000*(27705 /120.91 + R12_TABLE_READING) # Enthalpy j/kg CONDENSER INLET
P2 = 928.18 * 1000  # Discharge pressure Pa
P4 = 926.00 * 1000   # Condenser Out pressure Pa
PBUB = 828 * 1000   # Condenser DEW pressure Pa

T3 = 311.7728   # Temperature CONDENSER DEW POINT
H3 = 1000*(23492.94 /120.91 + R12_TABLE_READING) # Enthalpy j/kg CONDENSER DEW POINT

T5 = 249.51 # 5 - INLET TO FRESH FOOD EVAPORATOR
T8 = 249.51 # 8 - INLET TO FREEZER EVAPORATOR
T9 = 249.51 # 9 - OUTLET FROM FREEZER EVAPORATOR
T12 = 249.51 # 12 - FRESH FOOD EVAPORATOR DEW POINT

TBUB = 311.59 # 38.5C Condenser bubble point Temp
HBUB = 1000*(7802.18 /120.91 + R12_TABLE_READING) # J/kg Condenser bubble point Enthalpy
CPRLIQ = 1000* 122.98 /120.91 # Condenser bubble point CP j/kg/K

#========================================================
ICOND = 0
objCondenser = Condenser()
objCond = objCondenser.getObject(objCP=objCP, ICOND=ICOND)

objCond.setParamters(ATOTC=ATOTC,
                     UA_FF_CND=UA_FF_CND,   # watt/K
                     UA_FZ_CND=UA_FZ_CND,   # watt/K
                     UA_FF_HXS=UA_FF_HXS,   # watt/K
                     UA_FZ_HXS=UA_FZ_HXS,   # watt/K
                     CFMC=CFMC,             # watt/K
                     DTSUBC=DTSUBC,         # K
                     N_COND=N_COND,
                     TS1=TS1,
                     TS3=TS3,
                     TS5=TS5,
                     USCC=USCC,         # W/m2-K
                     UTPC=UTPC,         # W/m2-K
                     UDSC=UDSC          # W/m2-K
                          )

dicRest = objCond.cond_balance(T14=T14,
                            H14=H14,
                            T3=T3,
                            H3=H3,
                            T5=T5,
                            T8=T8,
                            T9=T9,
                            T12=T12,
                            TBUB=TBUB,
                            HBUB=HBUB,
                            MREF=MREF,
                            CPRLIQ=CPRLIQ)

print ("\n\n Teat 1 ICOND = 0 ---------------------------------------------- ")
print ("\n=== Parameters ======================")
print ('Total Heat Transfer Surface Area               m2 ATOTC=', ATOTC)
print ("Units to be checked  CFMC")
# sec.Kel/kj = 1.8961 sec.F/Btu(th)
# RHOCPC   = 316.8/TS1(N) deg-C
# CFMC     = 1.8961 watt/K

print ('      watt/K      CFMC=', CFMC)
print ("\n\n  Unit sec-F/Btu(th) ")
print ('Desuperheating Heat Transfer Conductance, kj/hr/m2/C USCC=', USCC)
print ('Subcooling Heat Transfer Conductance,     kj/hr/m2/C UTPC=', UTPC)
print ('Two-Phase Heat Transfer Conductance,      kj/hr/m2/C UDSC=', UDSC)

print ('A/R In Fresh Food Sec.(Or Cabinet Walls) UA_FF_CND=', UA_FF_CND)
print ('Cond: A/R In Freezer Section Walls       UA_FZ_CND=', UA_FZ_CND)
print ('                (If Separate Section) ')
print ('Both: A/R In Fresh Food Section          UA_FF_HXS=', UA_FF_HXS)
print ('                (Or Cabinet Walls)')
print ('Both: A/R In Freezer Section Walls       UA_FZ_HXS=', UA_FZ_HXS)
print ('                (If Separate Section) ')

print ('Refrigerant Exit Subcooling, Deg C             DTSUBC=', DTSUBC)
print ('     used only for 1-CCOUNT and 2-ccross, (NOT useD IN 0-cnat)')

print (" N_COND 1	Two Door Topmount Refrigerator/Freezer.")
print ("        2	Two Door Bottommount Befrigerator/Freezer.")
print ("        3	Side Byside Refrigerator/Freezer.")
print ("        4	Chest Freezer.")
print ("        5	Upright Freezer.")
print ("        6	Onedoor Refrigerator.")
print ("        7	Onedoor Refrigerator/Freezer")
print ('heat transfer fluid                            K TS1=', TS1)
print ('  (htf) temperature entering condenser')
print ('htf temperature entering fresh food evaporator K TS3=', TS3)
print ('htf temperature entering freezer evaporator    K TS5=', TS5)

print ('Refrigeration Type                           N_COND=', N_COND)
print ('     used only for 1-CCOUNT and 2-ccross, (NOT useD IN 0-cnat)')

print ("=== Inputs ======================")
print ('Conderser cooling type        Nat ICOND=', ICOND)
print ('Temperature INLET TO FRESH FOOD EVAPORATOR     K T5=', T5)
print ('Temperature INLET TO FREEZER EVAPORATOR        K T8=', T8)
print ('Temperature OUTLET FROM FREEZER EVAPORATOR     K T9=', T9)
print ('Temperature FRESH FOOD EVAPORATOR DEW POINT   K T12=', T12)

print ('Temperature CONDENSER INLET                    K T14=', T14)
print ('Enthalpy CONDENSER INLET                    j/kg H14=', H14)

print ('Temperature condenser dew point                 K T3=', T3)
print ('Enthalpy condenser dew point                 j/kg H3=', H3)

print ('Condenser bubble point Temp                   K TBUB=', TBUB)
print ('Condenser bubble point Enthalpy            j/kg HBUB=', HBUB )
print ('Condenser bubble point CP              j/kg/K CPRLIQ=', CPRLIQ )

print ("\n=== Output ==Not sure about unit====================")
print ('Q desuperheating       watt    QDSC = ',dicRest['QDSC'])
print ('Q two phase            watt    QTPC = ',dicRest['QTPC'])
print ('Q subcooling           watt    QSCC = ',dicRest['QSCC'])
print ('Q total condenser       watt  QTOTC = ',dicRest['QTOTC'])

print ('Fraction desuperheating   ratio% FSUP = ',dicRest['FSUP'])
print ('Fraction subcooling       ratio% FSUB = ',dicRest['FSUB'])


print ("\n\n Teat 2 ICOND = 1 ---------------------------------------------- ")
ICOND = 1
print ('Input --')
print ('Conderser cooling type        Cross-Flow ICOND=', ICOND)
print ("Condenser Out pressure Pa   P4=",P4)
print ("Condenser DEW pressure Pa PBUB=",PBUB)

objCondenser = Condenser()
objCond = objCondenser.getObject(objCP=objCP, ICOND=ICOND)
objCond.setParamters(ATOTC=ATOTC,
                     UA_FF_CND=UA_FF_CND,   # watt/K
                     UA_FZ_CND=UA_FZ_CND,   # watt/K
                     UA_FF_HXS=UA_FF_HXS,   # watt/K
                     UA_FZ_HXS=UA_FZ_HXS,   # watt/K
                     CFMC=CFMC,             # watt/K
                     DTSUBC=DTSUBC,         # K
                     N_COND=N_COND,
                     TS1=TS1,
                     TS3=TS3,
                     TS5=TS5,
                     USCC=USCC,         # W/m2-K
                     UTPC=UTPC,         # W/m2-K
                     UDSC=UDSC          # W/m2-K
                          )

dicRest = objCond.cond_balance(T14=T14,
                            H14=H14,
                            T3=T3,
                            H3=H3,
                            TBUB=TBUB,
                            HBUB=HBUB,
                            MREF=MREF,
                            CPRLIQ=CPRLIQ,
                            PBUB=PBUB,
                            P4=P4
                            )

print ("\n=== Output ==Not sure about unit====================")
print ('Q desuperheating       watt    QDSC = ',dicRest['QDSC'])
print ('Q two phase            watt    QTPC = ',dicRest['QTPC'])
print ('Q subcooling           watt    QSCC = ',dicRest['QSCC'])
print ('Q total condenser       watt  QTOTC = ',dicRest['QTOTC'])

print ('Fraction desuperheating   ration%  FSUP = ',dicRest['FSUP'])
print ('Fraction subcooling       ration% FSUB = ',dicRest['FSUB'])
#========================================================
print ("\n\n Teat 3 ICOND = 2---------------------------------------------- ")
ICOND = 2
print ('Input --')
print ('Conderser cooling type        Counter-Flow ICOND=', ICOND)
print ("Condenser Out pressure Pa   P4=",P4)
print ("Condenser DEW pressure Pa PBUB=",PBUB)

objCondenser = Condenser()
objCond = objCondenser.getObject(objCP=objCP, ICOND=ICOND)
objCond.setParamters(ATOTC=ATOTC,
                     UA_FF_CND=UA_FF_CND,   # watt/K
                     UA_FZ_CND=UA_FZ_CND,   # watt/K
                     UA_FF_HXS=UA_FF_HXS,   # watt/K
                     UA_FZ_HXS=UA_FZ_HXS,   # watt/K
                     CFMC=CFMC,             # watt/K
                     DTSUBC=DTSUBC,         # K
                     N_COND=N_COND,
                     TS1=TS1,
                     TS3=TS3,
                     TS5=TS5,
                     USCC=USCC,         # W/m2-K
                     UTPC=UTPC,         # W/m2-K
                     UDSC=UDSC          # W/m2-K
                          )


dicRest = objCond.cond_balance(T14=T14,
                            H14=H14,
                            T3=T3,
                            H3=H3,
                            TBUB=TBUB,
                            HBUB=HBUB,
                            MREF=MREF,
                            CPRLIQ=CPRLIQ,
                            PBUB=PBUB,
                            P4=P4
                            )

print ("\n=== Output ==Not sure about unit====================")
print ('Q desuperheating       watt    QDSC = ',dicRest['QDSC'])
print ('Q two phase            watt    QTPC = ',dicRest['QTPC'])
print ('Q subcooling           watt    QSCC = ',dicRest['QSCC'])
print ('Q total condenser       watt  QTOTC = ',dicRest['QTOTC'])

print ('Fraction desuperheating   % FSUP = ',dicRest['FSUP'])
print ('Fraction subcooling       % FSUB = ',dicRest['FSUB'])
#========================================================

print ("\n\n\n Testing cond method =======================")
R12_TABLE_READING = 200.0 - 27.10795349661135

TC = [0.0, 311.66, 316.66, 0.0]  #  Condenser Temp Traials
JC = 1  # Current trailar 1 the first time 2 others

T4 = 316.66  # Temp K condenser outlet
H4 = 1000*(8428.58  /120.91 + R12_TABLE_READING) # Enthalpy condenser outlet J/kg
H14 = 1000*(28942.71  /120.91 + R12_TABLE_READING) # Enthalpy condenser inlet J/kg

ICOND = 1  # 0 Nat. cooling, 1 Cross, OR 2 Counter
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

QCONDS = 210.218  # watt
QCONDC = 75.156  # watt
QSCC = 0    # condenser subcooling heat transfer watt
UACOND = 14.8 # watt/K

print("==================input============")
print(" old Initial Guess For Refrigerant Mas Flow Rate kg/hr MROLD = ", MROLD)
print(" Initial Guess For Refrigerant Mas Flow Rate      kg/hr MREF = ", MREF)

print("        ICOND =", ICOND)

print(" heat transfer fluid (htf) temperature entering condenser  TS1 =", TS1)
print(" Condenser Temp                 TC =", TC)
print(" Iteration count 1 or others    JC =", JC)

print(" condenser outlet temperature   T4 =", T4)
print(" condenser inlet Enthalpy      H14 =", H14)
print(" condenser outlet Enthalpy      H4 =", H4)

print("       watt QCONDS =", QCONDS)
print("       watt QCONDC =", QCONDC)
print("       watt QSCC =", QSCC)
print("   watt/K UACOND =", UACOND)

lstRest = objCond.cond(T4=T4,             # K
                       H4=H4,             # j/kg
                       H14=H14,           # j/kg
                       TC=TC,             # K
                       JC=JC,             # number unit less
                       QCONDS=QCONDS,       # watt
                       QCONDC=QCONDC,       # watt
                       QSCC=QSCC,         # watt
                       MROLD=MROLD,       # kg/hr
                       MREF=MREF,         # kg/hr
                       UACOND=UACOND      # watt/K
                    )

print("\n\n==================Output============")
print("           K TS2 =", lstRest[0])
print("            K TC =", lstRest[1])
print("              JC =", lstRest[2])
print("           ICONC =", lstRest[3])

print ("\n=== Others outputs ======================")

[Q_CND_FF,         # watt
 Q_CND_FZ,         # watt
 Q_HXS_FF,         # watt
 Q_HXS_FZ,         # watt
 UACOND,           # watt/K
 UDSC,             # watt/m2 K
 USCC,             # watt/m2 K
 UTPC              # watt/m2 K
 ] = objCond.getExtarOutputs()


print ("if value is None, value is not calculated")
print ("USCC, UTPC, UDSC are app inputs, and is recaluted here ")
print (' Condenser Heat Fresh Food, watt Q_CND_FF=', Q_CND_FF )
print (' Condenser Heat Freezer,    watt Q_CND_FZ=', Q_CND_FZ )
print (' Heat Exchanger Fresh Food  watt Q_HXS_FF=', Q_HXS_FF )
print (' Heat Exchanger Freezer     watt Q_HXS_FZ=', Q_HXS_FZ )
print (' Condenser UA               watt/K UACOND=', UACOND )

print (' Desuperheating Heat Transfer Conductance, watt/m2 K UDSC=', UDSC)
print (' Subcooling Heat Transfer Conductance,    watt/m2 K  USCC=', USCC)
print (' Two-Phase Heat Transfer Conductance,      watt/m2 K UTPC=',UTPC)