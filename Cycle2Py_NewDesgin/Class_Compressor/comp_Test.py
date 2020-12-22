# Python Import ==================

# User Import ======================
from comp import *

#================== Test Driver ========


T1 = 300 #296     # K Suction temp. compressor inlet
T12 = 280 #249.5  # K fresh food evap. dew point

P1 = 123.05 * 1000  # Suction pressure Pa
P2 = 928.18 * 1000  # Discharge pressire Pa

R12_TABLE_READING = 200.0 - 27.10795349661135 # 26.2257538946007 	# kj/kg   200-app result at 0C need to be 200, valid only for RF12
H1 = 1000*(23726 /120.91 + R12_TABLE_READING )# J/kg
V1 = 19.490 / 120.91 # m3/kg

TAMB = 308.11 # K
# MEFF used only in case of IMAP = 0 (MAP) or 2 (Efficiency Model)
MEFF = 0.8  # MEFF - MECHANICAL EFFICIENCY

QHILO = 0.0 # QHILO - NORMALIZED HEAT LOSS FROM DISCHANGE LINE INSIDE
QCAN = 0.0  # QCAN - COMPRESSOR SHELL LOSS NORMALIZED TO POWER INPUT
IMAP = 1 # Compressor Analysis method values 0 MAP, 1 ERR, 2 Efficiency mode

EER = 5.28  # Rated EER used only if IMAP = 1
SEFF =  0.9 # isentropic efficiency

ICOOL =  1  # Fan cooling method 0 Static, 1 Fan-Forced
ICOMP =  1  # Compressor Type 1   Reciprocating, 2   Rotary

SPEED =  3450 # Nominal Speed (rpm =3450)
MREF = 5.8 # kg/hr ===>0.04796956413861551 kmol/hr # Initial Guess For Refrigerant Mas Flow Rate (kg/hr) * 2.20462 lbs/hr

# used only if IMAP =1 useless otherwise
EFFC =  0.6058868684272488 # used only if IMAP =1 useless otherwise

# used only if IMAP =1 useless otherwise
CE =  0.022338000722326633 # CLEARANCE VOLUME (FRACTION) output of map

# Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')

print ("\n\n==Test 1 =========================")
lstRes = comp (objCP, H1=H1, P1=P1, P2=P2, T1=T1, T12=T12   
               ,MEFF=MEFF, QHILO=QHILO, QCAN=QCAN, V1=V1, TAMB=TAMB
               ,EER=EER, SEFF=SEFF, SPEED=SPEED, MREF=MREF, IMAP=IMAP
               ,EFFC=EFFC, CE=CE, ICOOL=ICOOL, ICOMP=ICOMP)

print ("  ................ K")
print ("  T2    ", lstRes[0])
print ("  TSUC  ", lstRes[6])
print ("  TDISC ", lstRes[7])

print ("  ................j/kg ")
print ("  HOUT  ", lstRes[1])
print ("  QHILO ", lstRes[2])
print ("  QCAN  ", lstRes[3])

print ("  ................m3/kg ")
print ("  VV2   ", lstRes[5])
print ("  VSUC  ", lstRes[4])

print ("  ................Other ")
print ("  GAMA  ", lstRes[8])
print ("  RN    ", lstRes[9])
print ("  ETAS  ", lstRes[10])
print ("  DISP m3 ", lstRes[11])

print ("\n\n==Test 2 =========================")
IMAP = 2

lstRes = comp (objCP, H1=H1, P1=P1, P2=P2, T1=T1, T12=T12   
               ,MEFF=MEFF, QHILO=QHILO, QCAN=QCAN, V1=V1, TAMB=TAMB
               ,EER=EER, SEFF=SEFF, SPEED=SPEED, MREF=MREF, IMAP=IMAP
               ,EFFC=EFFC, CE=CE, ICOOL=ICOOL, ICOMP=ICOMP)

print ("  ................ K")
print ("  T2    ", lstRes[0])
print ("  TSUC  ", lstRes[6])
print ("  TDISC ", lstRes[7])

print ("  ................j/kg ")
print ("  HOUT  ", lstRes[1])
print ("  QHILO ", lstRes[2])
print ("  QCAN  ", lstRes[3])

print ("  ................m3/kg ")
print ("  VV2   ", lstRes[5])
print ("  VSUC  ", lstRes[4])

print ("  ................Other ")
print ("  GAMA  ", lstRes[8])
print ("  RN    ", lstRes[9])
print (" not calculated ETAS  ", lstRes[10])
print ("  DISP  ", lstRes[11])

print ("\n\n==Test 3 =========================")
IMAP = 0
print ("Not Applicable for IMPA=0")