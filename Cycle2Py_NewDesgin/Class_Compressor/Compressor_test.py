# Python Import ==================

# User Import ======================
from CoolPrp import *
from Compressor import *

# Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')

R12_TABLE_READING = 200.0 - 27.10795349661135 

#-=-=-=-=-=-=-=-==
TAMB = 308.11 # ambient temp K
FRACT_SPEED = 1  # FRACTIONAL SPEED
ICOMP = 1 # compressor type  1-Reciprocating compressor, 2-Rotary
strFileName = "DG73C12RAU6.cmp"  # File name

H1 = 1000*(23726 /120.91 + R12_TABLE_READING) # J/kg
TSUCT = 296     # K suction temp.
PSUCT = 123.05 * 1000  # Suction pressure Pa
PDISC = 928.18 * 1000  # Discharge pressire Pa
MREF = 5.8 # kg/hr
V1 = 19.490 / 120.91 # m3/kg

#-=-=-=-=-=-=-=-==
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
print ('    MREF kg/hr  = ', MREF)
print ('    VSUCT kg/m3 = ', V1)

print ("\n== Output===============")
dicRest = objCompType.comp_balance(PSUCT=PSUCT,
                                   PDISC=PDISC,
                                   TSUCT=TSUCT,
                                   MREF=MREF,
                                   VSUCT=V1)

print ('Compressor exit Temp K        TSP = ',dicRest['TSP'])
print ('Dischare Temp K             TDISC = ',dicRest['TDISC'])
print ('Dischare Enthalpy    j/kg    HOUT = ',dicRest['HOUT'])
print ('compressor shell loss normalized to power input j/kg QCAN  = ',dicRest['QCAN'])
print ('Suction sp.volume m3/kg      VSUC = ',dicRest['VSUC'])
print ('Dischare sp.volume m3/kg      VV2 = ',dicRest['VV2'])
print ('Cp/Cv value                  GAMA = ',dicRest['GAMA'])
print ('Compressor Efficiency   %    ETAC = ',dicRest['ETAC'])
print ('Refrigerant Mas Flow Rate  kg/hr  MREF = ',dicRest['MREF'])


