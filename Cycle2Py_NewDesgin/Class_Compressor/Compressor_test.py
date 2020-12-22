# Python Import ==================

# User Import ======================
from CoolPrp import *
from Compressor import *

# Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')


#==Happ/120.91 + R12_TABLE_READING=========================================
T1 = 300 #296     # K Suction temp. compressor inlet
T12 = 280 #249.5  # K fresh food evap. dew point

P1 = 123.05 * 1000  # Suction pressure Pa
P2 = 928.18 * 1000  # Discharge pressire Pa

R12_TABLE_READING = 200.0 - 27.10795349661135 # 26.2257538946007 	# kj/kg   200-app result at 0C need to be 200, valid only for RF12
H1 = 1000*(23726 /120.91 + R12_TABLE_READING) # J/kg
V1 = 19.490 / 120.91 # m3/kg
SIZE = 218 / 3.97  # capacity (btuh) at rating conditions (kcal/h = 3.97 btu/h)

QHILO = 0.0 # QHILO - NORMALIZED HEAT LOSS FROM DISCHANGE LINE INSIDE
QCAN = 0.0  # QCAN - COMPRESSOR SHELL LOSS NORMALIZED TO POWER INPUT

MREF = 5.8 # kg/hr ===>0.04796956413861551 kmol/hr 
# Initial Guess For Refrigerant Mas Flow Rate (kg/hr) * 2.20462 lbs/hr

# used only if IMAP =1 useless otherwise
EFFC =  0.6058868684272488 # used only if IMAP =1 useless otherwise

# used only if IMAP =1 useless otherwise
CE =  0.022338000722326633 # Clearance volume (fraction) output of map

#-=-=-=-=-=-=-=-==
IMAP = 1 # Compressor Analysis method values 0 MAP, 1 ERR, 2 Efficiency mode
TSUCT = 296     # K suction temp.
PSUCT = 123.05 * 1000  # Suction pressure Pa
PDISC = 928.18 * 1000  # Discharge pressire Pa
V1 = 19.490 / 120.91 # m3/kg

EER = 5.28  # Rated EER used only if IMAP = 1
SEFF =  0.9 # isentropic efficiency

ICOOL =  1  # Fan cooling method 0 Static, 1 Fan-Forced
ICOMP =  1  # Compressor Type 1   Reciprocating, 2   Rotary
TAMB = 308.11 # ambient temp K
# MEFF used only in case of IMAP = 0 (MAP) or 2 (Efficiency Model)
MEFF = 0.8  # MEFF - mechanical efficiency
SPEED =  3450 # Nominal Speed (rpm =3450)
FRACT_SPEED = 1  # FRACTIONAL SPEED
ICOMP = 1 # compressor type  1-Reciprocating compressor, 2-Rotary
strFileName = "DG73C12RAU6.cmp"  # File name

DISPLC = 6.57	# cu-cm	DISPLC[1] compressor displacement

H1 = 1000*(23726 /120.91 + R12_TABLE_READING) # J/kg

#-=-=-=-=-=-=-=-==

#========================================================

print ("Common Data===================")

objCompType = Compressor(objCP=objCP,
                        TAMB=TAMB,
                        ICOMP=ICOMP, 
                        FRACT_SPEED=FRACT_SPEED,
                        strFileName=strFileName)
                        
print ("== Inputs parameters===============")
print (' ambitem Temp                               TAMB=', TAMB)
print (' Compressor Type 1-Reciprocating, 2-Rotary  ICOMP=', ICOMP)
print (' Compressor file                            strFileName=', strFileName)
print ('                                            FRACT_SPEED=', FRACT_SPEED)

print ("== Inputs===============")
print ('    PSUCT Pa    = ', PSUCT)
print ('    PDISC Pa    = ', PDISC)
print ('    TSUCT K     = ', TSUCT)
print ('    VSUCT kg/m3 = ', V1)
print ('    MREF kg/hr  = ', MREF)

print ("\n== Output===============")
dicRest = objCompType.comp_balance(PSUCT=PSUCT,
                                    PDISC=PDISC, TSUCT=TSUCT,
                                    VSUCT=V1, MREF=MREF)

print ('Compressor exit Temp K        TSP = ',dicRest['TSP'])
print ('Dischare Temp K             TDISC = ',dicRest['TDISC'])
print ('Dischare Enthalpy    j/kg    HOUT = ',dicRest['HOUT'])
print ('compressor shell loss normalized to power input j/kg QCAN  = ',dicRest['QCAN'])
print ('Suction sp.volume m3/kg      VSUC = ',dicRest['VSUC'])
print ('Dischare sp.volume m3/kg      VV2 = ',dicRest['VV2'])
print ('Cp/Cv value                  GAMA = ',dicRest['GAMA'])
print ('Compressor Efficiency   %    ETAC = ',dicRest['ETAC'])
print ('Refrigerant Mas Flow Rate  kg/hr  MREF = ',dicRest['MREF'])


