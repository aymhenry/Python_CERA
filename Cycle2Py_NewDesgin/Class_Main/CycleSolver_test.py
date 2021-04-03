# Python Import ==================

# User Import ======================
from cycle_classes.CycleSolver import *

#--- Condenser Data
ICOND = 0
        #    input: ICOND = 0  Natural Convection
        #         : ICOND = 1  Cross-Flow
        #         : ICOND = 2  Counter-Flow
        #         : objCP = CoolProp object

CFMC = 175.44    # to check the units - random value unknow
DTSUBC = 0    # Refrigerant Exit Subcooling, Deg C

ATOTC = 0.849  # m2 Total Heat Transfer Surface Area
MREF = 5.8  # kg/hr Refrigerant Mas Flow Rate
MROLD = 5.7  # kg/hr Refrigerant Mas Flow Rate old value
UA_FF_CND = 0  # sec-F/Btu(th) , Cond: A/R In Fresh Food Section (Or Cabinet Walls)
UA_FZ_CND = 0  # sec-F/Btu(th) , Cond: A/R In Freezer Section Walls (If Separate Section)
UA_FF_HXS = 0  # sec-F/Btu(th) , Both: A/R In Fresh Food Section (Or Cabinet Walls)
UA_FZ_HXS = 0  # sec-F/Btu(th) , Both: A/R In Freezer Section Walls (If Separate Section)
N_COND = 1  # Refrigeration Type 1,2,3,4,5,6 or 7

UDSC  = 15.167* 3600    # Desuperheating Heat Transfer Conductance, kj/hr/m2/C
USCC  = 15.434 * 3600   # Subcooling Heat Transfer Conductance, kj/hr/m2/C  
UTPC  = 19.227 * 3600   # Two-Phase Heat Transfer Conductance, kj/hr/m2/C  
#=========================================================
        
#--- Evaporator Data
IFRSH = 0 
        #    input: IFRSH = 0  Natural Convection
        #         : IFRSH = 1  Cross-Flow
        #         : IFRSH = 2  Counter-Flow
R12_TABLE_READING = 200.0 - 27.10795349661135
#--------------
T5= 257.86  # K

HDEW = 1000*(20824.4 /120.91 + R12_TABLE_READING) # j/kg
TDEW= 256.77 # K
CPRVAP= 0.91 # Dr Omar Cp of Ref. Vap
PIN= 180.1 *1000 # Pa
POUT= 172.87 *1000 # Pa

TS3= 260.86 # K
N_EVAP = 5
USUPE = 5.633  # W/m2-c Superheat Region Conductance, W/M2-C
UTPE = 13.787  # W/m2-c Two-Phase Heat Transfer Conductance, W/M2-C
ATOTE = 2.338  # m2 Total Heat Transfer Surface Area, M2

CFME = 23.6   # L/s Air Flow Rate Across Coil (L/S)
# RHOCPE   = 316.8/TS3
# CFME     = 1.8961*(RHOCPE * CFME)/0.4720

FZTEMP = -15 + 273.15 # K  Freezer Temperature (K)
UA_FF = 0  # watt	UA_FF	Evap: A/R In Fresh Food Section (Or Cabinet Walls)

TROOM = 32.22 + 273.15 #  K Room Temperature (K)
Q_HXS_FF = 0 # Q_HXS_FF = 1.8 * Data.UA_FF_HXS*(TCND - TFF)*1.0548
             # where 
             # UA_FF_HXS=0 watt, Both: A/R 
             # In Fresh Food Section (Or Cabinet Walls)

IWALL_FF = 0 # FF (OR CABINET) EVAPORATOR BEHIND LINER (0: NO, 1: YES)
NUM_ZONE= 1
IRFTYP = 1  # Refrigeration Type (1 to 7)
#========================================================

#-- Compressor Data
ICOMP = 1 # compressor type  1-Reciprocating compressor, 2-Rotary

TAMB = 308.11 # ambient temp K
FRACT_SPEED = 1  # FRACTIONAL SPEED
strFileName = "DG73C12RAU6.cmp"  # File name

#-- CycleSolver Data

XM = [0.0, 1, 0, 0, 0.0, 0.0]
F = [[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0, 0, 0.0, 0.0, 0.0], [0.0, 0, 0.0, 0, 0.0, 0.0, 0.0], [0.0, 0, 0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]]

TS1 = 308.11 # K = 35.0 C
TS3 = 261.776 # K = -11.334000000000003 C
TS5 = -300.0  # K = -573.11C

DPC = 4.72
DPE = 7.2
DPF = 0.0

ETHX1 = 0.8
ETHX2 = 0.8

FROSTF = 0.0
FROSTZ = 0.0

QHILO = 0.0
QCAN = 0.0

ICAB = 1
ICYCL = 1
ICYCLS = 1

IR = [0.0, 2, 1, 1, 0.0, 0.0]
NC = 1
NCYC = 1

#IRFTYPE = 1 # not used
IRFYPE = 1 # to be checked
IDFRST = 0

MEFF = 0.0
DISPLC = 0.4009251179281264

DTSUPE = 0.0 # to be checked
DTSUPI = 0.0 # to be checked

FFTEMP = 3.33 + 273.3 # K
FZTEMP = -15 + 273.3 # K

#======= Create basic object for coolProp
objCP = CoolPrp()
objCP.setup('R12')

objCycleSolver = CycleSolver(objCP, None, 1)

objCycleSolver.setupCond(ICOND)
objCycleSolver.setupEvap(IFRSH)
objCycleSolver.setupComp(ICOMP, TAMB, FRACT_SPEED, strFileName)

#------- Setup paramters 
objCycleSolver.paraEvap(ATOTE = ATOTE,
                    CFME=CFME,
                    TS3=TS3,
                    N_EVAP=N_EVAP,
                    USUPE=USUPE,
                    UTPE=UTPE,
                    TROOM=TROOM,
                    FZTEMP=FZTEMP,
                    UA_FF=UA_FF,
                    Q_HXS_FF=Q_HXS_FF,
                    IWALL_FF=IWALL_FF,
                    NUM_ZONE=NUM_ZONE,
                    IRFTYP=IRFTYP
                    )
                    
objCycleSolver.paraCond(ATOTC = ATOTC,
                    UA_FF_CND = UA_FF_CND,
                    UA_FZ_CND = UA_FZ_CND,
                    UA_FF_HXS = UA_FF_HXS,
                    UA_FZ_HXS = UA_FZ_HXS,
                    CFMC = CFMC,
                    DTSUBC = DTSUBC,
                    N_COND = N_COND,
                    TS1=TS1,
                    TS3=TS3,
                    TS5=TS5,
                    USCC=USCC,
                    UTPC=UTPC,
                    UDSC=UDSC
                    )
                    
objCycleSolver.paraCycle(ISPEC=1
            ,TS1=TS1, TS3=TS3, TS5=TS5
            , DPC=DPC, DPE=DPE, DPF=DPF
            , ETHX1=ETHX1, ETHX2=ETHX2
            , FROSTF=FROSTF, FROSTZ=FROSTZ
            , QHILO=QHILO, QCAN=QCAN
            , ICAB=ICAB, ICYCL=ICYCL, ICYCLS=ICYCLS, IR=IR, NC=NC, NCYC=NCYC
            , IRFYPE=IRFYPE, IDFRST=IDFRST
            , MEFF=MEFF, DISPLC=DISPLC
            , MREF=MREF, DTSUPE=DTSUPE, DTSUPI=DTSUPI
            , FFTEMP = FFTEMP, FZTEMP = FZTEMP
            )
            
#=== solve
objCycleSolver.solveCycle()
            
                  


