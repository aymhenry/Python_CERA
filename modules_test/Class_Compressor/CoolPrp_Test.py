# Python Import ==================

# User Import ======================
from CoolPrp import *

#================== Test Driver ========

objCP = CoolPrp()
objCP.setup("R12") # used Fuild RF12

if objCP.isError():
    print(objCP.err_description())
    exit

TK_C = 273.15  # K
T1 = TK_C + 72  # K
P1 = 200000  # Pa
# H = 400061.6198132278	# J/kg
S = 1741.3033288796132  # J/kg/K
# D = 8.632966162031755  #kg/m3
V = 0.11583504223589493  # m3/kg

print("Fuild: ", objCP.getFuild())
print("----------------------------------------")
print("Critical temperature: ", objCP.getCrtiticalTemp(), "K")
print("Critical Pressure:    ", objCP.getCrtiticalPress(), "Pa")
print("Critical Volume:      ", objCP.getCrtiticalVolume(), "kg/m3")

print("----------------------------------------")
print("---Saturated condition")
T1 = TK_C + 18  # K
P1 = 535130  # Pa
print("Given P1=", P1, "  T1=", T1)
print("----------------------------------------")
print("V-liq  by T ", objCP.Property('V', T=T1, X=0), 'kg/m3')
print("V-gas  by T ", objCP.Property('V', T=T1, X=1), 'm3/kg')
print("D-liq  by T ", objCP.Property('D', T=T1, X=0), 'kg/m3')
print("D-gas  by T ", objCP.Property('D', T=T1, X=1), 'm3/kg')

print("----------------------------------------")
print("V-liq  by P ", objCP.Property('V', P=P1, X=0), 'm3/kg')
print("D-liq  by P ", objCP.Property('D', P=P1, X=0), 'kg/m3')
print("V-liq  by P ", objCP.Property('V', P=P1, X=1), 'm3/kg')
print("D-liq  by P ", objCP.Property('D', P=P1, X=1), 'kg/m3')

print("----------------------------------------")
Tsat = 315.14934314624594
print("Sat-Temp for P = 1006800 Pa by P X=0",
      objCP.Property('T', P=1006800, X=0), 'K')
print("Sat-Temp for P = 1006800 Pa by P X=1",
      objCP.Property('T', P=1006800, X=1), 'K')

print("Sat-Pressure for T = 315.15 Pa by P X=0",
      objCP.Property('P', T=Tsat, X=0), 'Pa')
print("Sat-Pressure for T = 315.15 Pa by P X=1",
      objCP.Property('P', T=Tsat, X=1), 'Pa')

print("Phase at T=373 K, P = 1013250 Pa: (confirmed)",
      objCP.phase_byPressTemp(1013250, TK_C + 100))

print("========================================")
print("---others=-=-=-=-=")
print("H (liquid) at T=273 K, P = 308150 Pa: by P, T",
      objCP.Property('h', P=308150, T=TK_C), 'J/kg/K')

print("----------------------------------------")
print(
    "Cp  at T=273 K, P = 308150 Pa: by P,T",
    objCP.Property(
        'cp',
        P=308150,
        T=TK_C),
    'J/kg/K')
print(
    "Cp  at T=273 K, P = 308150 Pa: by P,T",
    objCP.Property(
        'cv',
        P=308150,
        T=TK_C),
    'J/kg/K')

print("Cp  by P, V", objCP.Property('cp', P=P1, V=V), 'J/kg/K')

print("Cp  by P, V", objCP.Property('cv', P=P1, V=V), 'J/kg/K')

print("----------------------------------------")
print("Given P1=", P1, "  T1=", T1, "V=", V)
print("S   by P, T ", objCP.Property("S", P=P1, T=T1), 'J/kg/K')

print("S   by T, V ", objCP.Property("S", T=T1, V=V), 'J/kg/K')

print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
print("V       Press,Temp ", objCP.Property("V", P=P1, T=T1), 'J/kg/K')
print("Den     Press,Temp ", objCP.Property("D", P=P1, T=T1), 'kg/m3')
print("T          by S, P ", objCP.Property("T", S=S, P=P1), 'K')

print("----------------------------------------")
print("H (gas) by P,T ", objCP.Property("h", P=P1, T=T1), 'J/kg')
print("H       by Press, Vol  ", objCP.Property("h", P=P1, V=V), 'J/kg')
print("H       by T, V  ", objCP.Property("h", T=T1, V=V), 'J/kg')

print("========================================")
print ("T1 = ", T1)
print("H       by T, X  ", objCP.Property("h", T=T1, X=0), 'J/kg')
if objCP.isError():
    print ("Error: " + objCP.err_description())

# print("================Create an Error======================")
# print ("T1 = ", T1)
# print("H       by T, P  ", objCP.Property("h", T=Tsat, X=2), 'J/kg')
# if objCP.isError():
    # print ("Error: " + objCP.err_description())
    
print("========================================")
print ("Output ->",objCP.phase_byPressTemp (1013250, 273+100))
print ("Is Gas ->",objCP.is_gas_phase(objCP.phase_byPressTemp (1013250, 273+100)))

print("========================================")
print ("Ouput  ->",objCP.phase_byPressTemp (1013250, 273+00 ))
print ("Is liquid ->", objCP.is_liquid_phase(objCP.phase_byPressTemp (1013250, 273+00)))
    