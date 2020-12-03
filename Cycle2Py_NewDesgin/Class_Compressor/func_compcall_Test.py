# Python Import ==================

# User Import ======================
from CompressorClass_methods import *

# Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')

objComp_Map = Comp_Map(objCP)

TAMB = 308.11 # K
TSUCT = 296     # K suction temp.
PSUCT = 123.05 * 1000  # Suction pressure Pa
PDISC = 928.18 * 1000  # Discharge pressire Pa

V1 = 19.490 / 120.91 # m3/kg

MREF = 5.8 # kg/hr ===>0.04796956413861551 kmol/hr # Initial Guess For Refrigerant Mas Flow Rate (kg/hr) * 2.20462 lbs/hr

# FRACTIONAL SPEED = 1
FRACT_SPEED = 1  # FRACTIONAL SPEED
ICOMP = 1 # compressor type  1-Reciprocating compressor, 2-Rotary
strFileName = "DG73C12RAU6.cmp"  # File name


print ("\n\n==Test compcall only IMAP=0 ................")
lstRes = objComp_Map.compcall (objCP, PSUCT=PSUCT, PDISC=PDISC, 
                TSUCT=TSUCT, VSUCT=V1,
                TAMB=TAMB, MREF=MREF, FRACT_SPEED=FRACT_SPEED,
                strFileName=strFileName, ICOMP=ICOMP)

print ("  ................. K")
print ("  TSUC    ", lstRes[5])
print ("  TDISC   ", lstRes[6])

print ("  HOUT  j/kg  ", lstRes[0])
print ("  QHILO =0    ", lstRes[1])

print ("  ............. m3/kg")
print ("  VSUC    ", lstRes[3])
print ("  VV2     ", lstRes[4])


print ("  QCAN (#)    ", lstRes[2])
print ("  GAMA (%) ", lstRes[7])
print ("  RN   (%) ", lstRes[8])
print ("  ETAS (%) less than 1 ", lstRes[9])
