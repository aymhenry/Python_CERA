# Python Import ==================

# User Import ======================
from CompressorClass_methods import *

# Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')

objComp_Map = Comp_Map(objCP)

# FRACTIONAL SPEED = 1
FRACT_SPEED = 1  # FRACTIONAL SPEED

TAMB = 308.11  # K
TSUCT = 296     # K suction temp.
PSUCT = 123.05 * 1000  # Suction pressure Pa
VSUCT = 19.49 / 120.9 # 1/6 m3/kg

PDISC = 928.18 * 1000  # Discharge pressire Pa
GAMA = 1.1449

ICOMP = 1 # compressor type  1-Reciprocating compressor, 2-Rotary
 
strFileName = "DG73C12RAU6.cmp"  # File name

print("\n\n==Test compcall only IMAP=0 ................")             

lstRes = objComp_Map.compmap(PSUCT=PSUCT, PDISC=PDISC, TSUCT=TSUCT,
                            VSUCT=VSUCT, strFileName=strFileName,
                            GAMA=GAMA, TAMB=TAMB, 
                            FRACT_SPEED=FRACT_SPEED, ICOMP=1)


print("  TSP  : Temperature K ", lstRes[0])
print("  WDOT : Capacity  kj/hr", lstRes[1])
print("  MDOT : mass flow rate  kg/hr ", lstRes[2])
print("  QSHELL : Shell Heat kj/hr", lstRes[3])
