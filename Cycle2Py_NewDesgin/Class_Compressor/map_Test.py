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
DISPL = 6.57 # compressor displacement (cu-in)
SIZE = 218 / 3.97  # capacity (btuh) at rating conditions (kcal/h = 3.97 btu/h)

lstRes = map(objCP=objCP, ICOMP=ICOMP, ICOOL=ICOOL, EER=EER, SIZE=SIZE, DISPL=DISPL, SPEEDN=SPEED)

print ("  Compressor isentropic efficiency including mechanical and motor losses")
print ("  ETAC:     ", lstRes[0])
print ("  CE : clearance volume (fraction)  ", lstRes[1])