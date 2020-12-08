# Python Import ==================

# User Import ======================
from map import *

#================== Test Driver ========
#-----------------------------
# Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')

ICOOL =  1  # Fan cooling method 0 Static, 1 Fan-Forced
ICOMP =  1  # Compressor Type 1   Reciprocating, 2   Rotary
EER = 5.28  # Rated EER used only if IMAP = 1
SPEED =  3450 # Nominal Speed (rpm =3450)
DISPL = 5.7212885425902e-06 # cu-m3
SIZE = 1

lstRes = map(objCP=objCP, ICOMP=ICOMP, ICOOL=ICOOL, EER=EER, SIZE=SIZE, DISPL=DISPL, SPEEDN=SPEED)

print ("  ................. K")
print ("  ETAC    ", lstRes[0])
print ("  CE   ", lstRes[0])