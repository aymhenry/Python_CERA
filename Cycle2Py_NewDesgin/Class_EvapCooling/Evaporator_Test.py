# Python Import ==================

# User Import ======================
from CoolPrp import *
from Evaporator import *

# Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')

#=========================================================
R12_TABLE_READING = 200.0 - 27.10795349661135
#========================================================
T5= 257.86  # K

#Omar Abdelaziz Commented
# HDEW = 1000*(20824.4 /120.91 + R12_TABLE_READING) # j/kg
TDEW= 256.77 # K
CPRVAP= 0.91 # Dr Omar Cp of Ref. Vap
PIN= 180.1 *1000 # Pa
POUT= 172.87 *1000 # Pa

TS3= 260.86 # K
N_EVAP = 5
USUPE = 5.633  # W/m2-c Superheat Region Conductance, W/m2-C
UTPE = 13.787  # W/m2-c Two-Phase Heat Transfer Conductance, W/m2-C
ATOTE = 2.338  # m2 Total Heat Transfer Surface Area, m2

CFME = 14.5   # watt/K

FZTEMP = -15 + 273.15 # K  Freezer Temperature (K)
UA_FF = 0  # watt	UA_FF	Evap: A/R In Fresh Food Section (Or Cabinet Walls)

TROOM = 32.22 + 273.15 #  K Room Temperature (K)
Q_HXS_FF = 0 # watt


IWALL_FF = 0 # FF (OR CABINET) EVAPORATOR BEHIND LINER (0: NO, 1: YES)
NUM_ZONE= 5 #OA change from 1 to 5
IRFTYP = 1  # Refrigeration Type (1 to 7)

# ===========================================
T5 = 249.51 # 5 - INLET TO FRESH FOOD EVAPORATOR
MREF = 5.8 # kg/hr

T8 = 249.51 # 8 - INLET TO FREEZER EVAPORATOR
T9 = 249.51+5 # 9 - OUTLET FROM FREEZER EVAPORATOR
T12 = 249.51 # 12 - FRESH FOOD EVAPORATOR DEW POINT

H5 = 211 * 1000 #1000*(23492.94 /120.91 + R12_TABLE_READING) # j/kg
T7 = 249.51 + 5 # K

P5 = 130500  # Pa INLET TO FRESH FOOD EVAPORATOR  Pa
P7 = 130500  # Pa OUTLET FROM FRESH FOOD EVAPORATOR Pa
#========================================================
IFRSH = 0
objEvaporator = Evaporator()
objEvap = objEvaporator.getObject(objCP=objCP, IFRSH=IFRSH)
                      

objEvap.setParamters(ATOTE=ATOTE,
                          CFME=CFME,  # watt/K
                          TS3=TS3,
                          N_EVAP=N_EVAP,
                          USUPE=USUPE,  # watt/m2-K
                          UTPE=UTPE,  # watt/m2-K
                          TROOM=TROOM,
                          FZTEMP=FZTEMP,
                          UA_FF=UA_FF,
                          Q_HXS_FF=Q_HXS_FF,  # defalut =0 in Fortran
                          IWALL_FF=IWALL_FF,
                          NUM_ZONE=N_EVAP,
                          IRFTYP=IRFTYP
                          )

dicRest = objEvap.evap_balance(MREF=MREF,      # kg/hr
                               T5=T5,          # K
                               H5=H5,          # j/kg
                               T7=T7,          # K
                               TDEW=TDEW,      # K
                               CPRVAP=CPRVAP   # j/kg K
                               )
print ("\n\n\n Test 1 IFRSH = 0 ---------------------------------------------- ")
print ("\n=== Parameters ======================")
print ('Total Heat Transfer Surface Area               m2 ATOTE=', ATOTE)
print ("Units to be checked  CFME")
print ('L/s Air Flow Rate Across Coil (L/S)             CFME=', CFME)

print ('  (htf) temperature entering Evaporator')
print ('htf temperature entering fresh food evaporator K TS3=', TS3)
print ('Number of Zones on Evaporator                 N_EVAP=', N_EVAP)
print ('Superheat Region Conductance, W/M2-C           USUPE=', USUPE)
print ('Two-Phase Heat Transfer Conductance            UTPE=', UTPE)
print ('room temp K                                   TROOM=', TROOM)
print ('Freezer Temperature (K)                      FZTEMP=', FZTEMP)
print ('A/R In Fresh Food Section (Or Cabinet Walls)  UA_FF=', UA_FF)
print ('Heat Fresh Food Section (Or Cabinet Walls) Q_HXS_FF=', Q_HXS_FF)
print ('FF (OR CABINET) EVAPORATOR BEHIND LINER (0: NO, 1: YES) IWALL_FF=',
        IWALL_FF)
        
print ('count no. of zones                         NUM_ZONE=', NUM_ZONE)
print ('Refrigeration Type (1 to 7)                  IRFTYP=', IRFTYP)

print ("\n=== Inputs for IFRSH = 0 ======================")
print ('Evaporator cooling type        Nat IFRSH=', IFRSH)
print ('Temperature INLET TO FRESH FOOD EVAPORATOR      K T5=', T5)
print ('Temperature OUTLET FROM FRESH FOOD EVAPORATOR   K T7=', T7)
print ('Temperature htf entering fresh food evaporator K TS3=', TS3)
print ('Temperature Evaporator Dew point              K TDEW=', TDEW)

# print ('Enthalpy Evaporator Dew point              j/kg HDEW=', HDEW )
print ('Enthalpy INLET TO FRESH FOOD EVAPORATOR Enthalpy j/kg H5=', H5 )

print ('Evaporator  CP  Cp of Ref. Vap              j/kg/K CPRVAP=', CPRVAP )

# ------------------------------------------
print ("\n\n\n=== Output for IFRSH = 0   ====================")
print ('                      W QFRSH = ',dicRest['QFRSH']) #[OA changed units]
print ('                        W/C UAFF = ',dicRest['UAFF'])
print ('Fraction subcooling,area Ration ASUPE / AEVAP FSUPE = ',dicRest['FSUPE'])
UAFF = dicRest['UAFF']

#========================================================
print ("\n\n Test 2 IFRSH = 1 ---------------------------------------------- ")
IFRSH = 1
objEvaporator = Evaporator()
objEvap = objEvaporator.getObject(objCP=objCP, IFRSH=IFRSH)

print ("=== input for IFRSH = 1   ====================")
print ('Evaporator cooling type        Nat IFRSH=', IFRSH)
print ('Temperature INLET TO FRESH FOOD EVAPORATOR      K T5=', T5)

print ('Temperature htf entering fresh food evaporator K TS3=', TS3)
print ('Temperature Evaporator Dew point           K TDEW=', TDEW)

# print ('Enthalpy Evaporator Dew point               j/kg HDEW=', HDEW )
print ('Enthalpy INLET TO FRESH FOOD EVAPORATOR  j/kg H5=', H5 )

print ('Evaporator  CP                             j/kg/K CPRVAP=', CPRVAP )

print ('Pressure INLET TO FRESH FOOD EVAPORATOR    Pa P5=', P5)
print ('Pressure OUTLET FROM FRESH FOOD EVAPORATOR Pa P7=', P7)

objEvaporator = Evaporator()
objEvap = objEvaporator.getObject(objCP=objCP, IFRSH=IFRSH)
objEvap.setParamters(ATOTE=ATOTE,
                          CFME=CFME,  # watt/K
                          TS3=TS3,
                          N_EVAP=N_EVAP,
                          USUPE=USUPE,  # watt/m2-K
                          UTPE=UTPE,  # watt/m2-K
                          TROOM=TROOM,
                          FZTEMP=FZTEMP,
                          UA_FF=UA_FF,
                          Q_HXS_FF=Q_HXS_FF,  # defalut =0 in Fortran
                          IWALL_FF=IWALL_FF,
                          NUM_ZONE=N_EVAP,
                          IRFTYP=IRFTYP
                          )

dicRest = objEvap.evap_balance(MREF=MREF,   # kg/hr
                                    T5=T5,     # K
                                    H5=H5,     # K
                                    TDEW=TDEW,   # K
                                    CPRVAP=CPRVAP,   # j/kg K
                                    P5=P5,      # pa
                                    P7=P7       # pa
                                    )

# ------------------------------------------
print ("\n\n\n=== Output for IFRSH = 1   ====================")
print ('                      W QFRSH = ',dicRest['QFRSH'])#[OA changed units]
print ('                        W/C UAFF = ',dicRest['UAFF'])
print ('Fraction subcooling,area Ration ASUPE / AEVAP FSUPE = ',dicRest['FSUPE'])
UAFF = dicRest['UAFF']
#========================================================
print ("\n\n Test 3 IFRSH = 2---------------------------------------------- ")
IFRSH = 2
objEvaporator = Evaporator()
objEvap = objEvaporator.getObject(objCP=objCP, IFRSH=IFRSH)

print ("=== input for IFRSH = 2   ====================")
print ('Evaporator cooling type        Nat IFRSH=', IFRSH)
print ('Temperature INLET TO FRESH FOOD EVAPORATOR      K T5=', T5)

print ('Temperature htf entering fresh food evaporator K TS3=', TS3)
print ('Temperature Evaporator Dew point           K TDEW=', TDEW)

# print ('Enthalpy Evaporator Dew point               j/kg HDEW=', HDEW )
print ('Enthalpy INLET TO FRESH FOOD EVAPORATOR Enthalpy j/kg H5=', H5 )

print ('Evaporator  CP                             j/kg/K CPRVAP=', CPRVAP )

print ('Pressure INLET TO FRESH FOOD EVAPORATOR    Pa P5=', P5)
print ('Pressure OUTLET FROM FRESH FOOD EVAPORATOR Pa P7=', P7)


objEvaporator = Evaporator()
objEvap = objEvaporator.getObject(objCP=objCP, IFRSH=IFRSH)

objEvap.setParamters(ATOTE=ATOTE,
                          CFME=CFME,  # watt/K
                          TS3=TS3,
                          N_EVAP=N_EVAP,
                          USUPE=USUPE,  # watt/m2-K
                          UTPE=UTPE,  # watt/m2-K
                          TROOM=TROOM,
                          FZTEMP=FZTEMP,
                          UA_FF=UA_FF,
                          Q_HXS_FF=Q_HXS_FF,  # defalut =0 in Fortran
                          IWALL_FF=IWALL_FF,
                          NUM_ZONE=N_EVAP,
                          IRFTYP=IRFTYP
                          )

dicRest = objEvap.evap_balance(MREF=MREF,   # kg/hr
                               T5=T5,     # K
                               H5=H5,     # j/kg
                               TDEW=TDEW,   # K
                               CPRVAP=CPRVAP,   # j/kg K
                               P5=P5,     # pa
                               P7=P7      # pa
                               )

# -------------------------------------------------
print ("\n=== Output for IFRSH = 2   ====================")
print ('Q                     W QFRSH = ',dicRest['QFRSH'])#[OA changed units]
print ('                     W/C UAFF = ',dicRest['UAFF'])
print ('Fraction subcooling,area Ration ASUPE / AEVAP FSUPE = ',dicRest['FSUPE'])

UAFF = dicRest['UAFF']
#========================================================

print ("\n\n\n Testing frsh method  ======================")
R12_TABLE_READING = 200.0 - 27.10795349661135
TS3 = 261.776   # htf temperature entering fresh food evaporator

TE = [0.0, 273.15 - 13.38, 00.0]  #  Evaporator Temp Trials
JE = 1  # Current trial 1 the first time 2 others

T5 = 273.15 - 13.38  # Temp K INLET TO FRESH FOOD EVAPORATOR # J/kg
# Enthalpy INLET TO FRESH FOOD EVAPORATOR J/kg
H5 = 1000 * (4484.17 / 120.91 + R12_TABLE_READING) # J/kg  

# Enthalpy LIQUID LINE OUTLET FROM HIGH TEMP INTERCHANGER J/kg
H7 = 1000 * (21034.06 / 120.91 + R12_TABLE_READING) # J/kg

# ALPHA = QFRSH/(MREF*(H(7) - H(5)))
# QFRSH = MREF * Delat H = (kg/s) * (j/kg)= j/s = W. #[OA changed units]

IFRSH = 0  # 0 Nat. cooling, 1 Cross, OR 2 Counter
QFRSH = 450 #126354.802 #MREF * 10 * 1000    # W #[OA changed units]
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

print ("\n\n\n Test 4 Input for frsh method  IFRSH = 0 =====================")

print("        IFRSH =", IFRSH)
print(" Ref.          Watt          QFRSH = ", QFRSH)

print(" Evaporator Temp                 TE =", TE)
print(" Iteration count 1 or others     JE =", JE)

print(" Temperature INLET TO FRESH FOOD EVAPORATOR  K  T5 =", T5)
print(" Entry Value to be checked Enthalpy INLET TO FRESH FOOD EVAPORATOR   j/kg H5 =", H5)
print(" Entry Value to be checked Enthalpy LIQUID LINE OUTLET FROM HIGH TEMP INTERCHANGER j/kg H7 =", H7)
print(" Initial Guess For Refrigerant Mas Flow Rate   kg/s MREF = ", MREF) #[OA changed units]


dicRest = objEvap.frsh(T5=T5,               # K
                       H5=H5,               # j/kg
                       H7=H7,               # j/kg
                       TS3=TS3,               # K
                       TE=TE,                 # K
                       JE=JE,                 # none
                       QFRSH=QFRSH,           # watt
                       MREF=MREF,             # kg/kg
                       UAFF=UAFF              # watt/m2 K
                       )
print ("\n output for frsh method  IFRSH = 0 =====================")
print("           K TE =", dicRest['TE'])
print("             JE =", dicRest['JE'])
print("          K TS4 =", dicRest['TS4'])
print(" 1=Free Error, 0=Error Found         ICONE =", dicRest['ICONE']) # 0=Free Error, 1=Error Found

#------------------------------------
print ("\n\n\n Test 5 input for frsh method  IFRSH = 1 =====================")
IFRSH = 1  # 0 Nat. cooling, 1 Cross, OR 2 Counter

print("        IFRSH =", IFRSH)
print(" Ref.          Watt          QFRSH = ", QFRSH)

dicRest = objEvap.frsh(T5=T5,               # K
                       H5=H5,               # j/kg
                       H7=H7,               # j/kg
                       TS3=TS3,               # K
                       TE=TE,                 # K
                       JE=JE,                 # none
                       QFRSH=QFRSH,           # watt
                       MREF=MREF,             # kg/kg
                       UAFF=UAFF              # watt/m2 K
                       )

print ("\n output for frsh method  IFRSH = 1 =====================")
print("           K TE =", dicRest['TE'])
print("             JE =", dicRest['JE'])
print("          K TS4 =", dicRest['TS4'])
print(" 1=Free Error, 0=Error Found         ICONE =", dicRest['ICONE']) # 0=Free Error, 1=Error Found

#------------------------------------
print ("\n\n\n Test 6 b input for frsh method  IFRSH = 2 =====================")
IFRSH = 2  # 0 Nat. cooling, 1 Cross, OR 2 Counter

print("        IFRSH =", IFRSH)
print(" Ref.          Watt          QFRSH = ", QFRSH)

dicRest = objEvap.frsh(T5=T5,               # K
                       H5=H5,               # j/kg
                       H7=H7,               # j/kg
                       TS3=TS3,               # K
                       TE=TE,                 # K
                       JE=JE,                 # none
                       QFRSH=QFRSH,           # watt
                       MREF=MREF,             # kg/kg
                       UAFF=UAFF              # watt/m2 K
                       )

print ("\n output for frsh method  IFRSH = 2 =====================")
print("           K TE =", dicRest['TE'])
print("             JE =", dicRest['JE'])
print("          K TS4 =", dicRest['TS4'])
print(" 1=Free Error, 0=Error Found         ICONE =", dicRest['ICONE']) # 0=Free Error, 1=Error Found


