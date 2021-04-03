# =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
def cond(T4, H4, H14, TS1, TC, JC, QCONDS, QCONDC, QSCC,
         MROLD, MREF, UACOND, CFMC, ICOND):
    # SUBROUTINE COND(T,H,TBUB,  HBUB,TS1,TS2,  TC,CPRLIQ,QCONDS,  QCONDC,
    #        QSCC,JC,ICONC)
    # TBUB,HBUB, CPRLIQ(P3, P4, P8) is not used
    # input  T,H, TS1, TC, QCONDS,QCONDC, QSCC, JC
    # output  TS2, TC, JC, ICONC
    #
    #     *****************************************************************
    #     *    CALCULATE CONDENSER EXIT TEMPERATURE                       *
    #     *****************************************************************
    #
    # REAL MREF,MROLD
    # DIMENSION H(16),T(16),TC(3)
    # COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
    # COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
    # COMMON/SPECS/DTSUPE,DTSUBC
    # COMMON/CONDEN/UDSC,UTPC,USCC,ATOTC,UACOND
    # COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX
    # COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
    #                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
    #                  CONDF_IN_WALL, CONDZ_IN_WALL
    #
    #         INITIALIZE
    #

    TOL_COND = 0.075  # Tolerance for condenser temperature
    TOL_MASS = 0.01  # Tolerance for condenser mass flow
    
    # useless ICNT = 10  # any value but not 0,1,2, Python only to be checked

    ICONC = 0
    if(JC == 1):
        # ICNT = 0
        MROLD = 0.0
        
    # Estimate new value for exit temperature

    QREF = MREF * (H14 - H4)
    QCOND = QCONDS + QCONDC + QSCC
    EPS = QREF - QCOND
    DELT = EPS / UACOND

    if(DELT > 5.0):
        DELT = 5.0
        
    if(DELT < -5.0):
        DELT = -5.0

    TCOUT = TC[JC] + DELT
    TS2 = TS1 + (QCONDS + QCONDC + QSCC) / CFMC

    if(ICOND == 0):
        TS2 = 0.9 * T4 + 0.1 * TS1
        
    if(TCOUT < TS1):
        TCOUT = (TS1 + TC[JC]) / 2.0

    # modification by Ayman if(ICNT <= 2) :
    if(JC < 2):
        TCNEW = TCOUT
    else:
        if((TCOUT > TC[1] and TC[1] > TC[2])
                or (TCOUT < TC[1] and TC[1] < TC[2])):
            TCNEW = 0.5 * (TC[1] + TC[2])
        else:
            TCNEW = TCOUT

        if(TCNEW < TS1):
            TCNEW = (TS1 + TC[JC]) / 2.0

        TC[1] = TC[2]

    # Check convergence
    ERRORT = abs(TCNEW - TC[JC])
    ERRORM = abs(MREF - MROLD) / MREF

    # ==============this block modified by Ayman
    # if(ERRORT < TOL_COND and ERRORM <= TOL_MASS):
        # ICONC = 1    
    if(ERRORT < TOL_COND and ERRORM <= TOL_MASS):
        ICONC = 0
    else:
        ICONC = 1
    # ======================End of Ayman Modification

    JC = 2
    ICNT = ICNT + 1  # useless
    TC[JC] = TCNEW
    # modification by Ayman - MROLD = MREF #useless
    # MROLD = MREF

    return [TS2, TC, JC, ICONC, MREF]

# -------------------------------------

R12_TABLE_READING = 200.0 - 27.10795349661135

'''
>>>>         QCONDS, QCONDC, QSCC= 251.95112653472043 209.1199773325663 0.0
     MREF =  0.047917168814271854
   watt CFMC = 175.44549885936618
        ICOND = 0
        TOL_COND = 0.075
        TOL_MASS = 0.01
        TS1 = 308.11
        TC,JC = [0.0, 311.6618877777778, 316.6618877777778, 0.0] 2
        T4 = 316.6618877777778
        H14 = 28942.719831096856
        H4 = 8428.584292676984
==================================
     UACOND =  30.782357693866953



>>>>         QCONDS, QCONDC, QSCC= 210.21853538661313 75.15606379210476 0.0
    kg/hr MREF =  0.049913717514866514
   watt CFMC = 175.44549885936618
        ICOND = 0
        TOL_COND = 0.075
        TOL_MASS = 0.01
        TS1 = 308.11
        TC,JC = [0.0, 311.6618877777778, 0.0, 0.0] 1
        T4 = 311.6618877777778
        H14 = 27704.574145826966
        H4 = 7811.8171566003
==================================
     UACOND =  28.556372797340597
     '''
# -------------------------------------

MROLD = 6  # kg/hr old Initial Guess For Refrigerant Mas Flow Rate
MREF = 5.8  # kg/hr Initial Guess For Refrigerant Mas Flow Rate

TS1 = 308.11  #  heat transfer fluid (htf) temperature entering condenser
TC = [0.0, 311.66, 316.66, 0.0]  #  Condenser Temp Traials
JC = 1  # Current trailar 1 the first time 2 others

T4 = 316.66  # Temp K condenser outlet 
H4 = 1000*(8428.58  /120.91 + R12_TABLE_READING) # Enthalpy condenser outlet J/kg
H14 = 1000*(28942.71  /120.91 + R12_TABLE_READING) # Enthalpy condenser inlet J/kg

ICOND = 1  # 0 Nat. cooling, 1 Cross, OR 2 Counter
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 42.476	L/s	CFMCI[1]	Air Flow Rate Across Coil(L/S)
# RHOCPC   = 316.8/TS1(N)
# CFMC     = 1.8961*(RHOCPC*CFMCI(N))/0.4720   # unknow
# CFMC     = 1.8961*(16.8/TS1(N)*CFMCI(N))/0.4720   # unknow
# QSCC = UAIR*ANET*DELTAT + ANET*Q_IN_WALL

CFMC = 175.44    # to check the units - random value unknow

UACOND = 28.556 # check the units unknow, need to be in SI

QCONDS = 210.218  # check the units unknow, need to be in watt
QCONDC = 75.156  # check the units unknow, need to be in watt
QSCC = 0    # condenser subcooling heat transfer watt

print("==================input============")
print(" old Initial Guess For Refrigerant Mas Flow Rate kg/hr MROLD = ", MROLD)
print(" Initial Guess For Refrigerant Mas Flow Rate      kg/hr MREF = ", MREF)

print("   watt CFMC =", CFMC)
print("        ICOND =", ICOND)

print(" heat transfer fluid (htf) temperature entering condenser  TS1 =", TS1)
print(" Condenser Temp                 TC =", TC)
print(" Iteration count 1 or others    JC =", JC)

print(" condenser outlet temperature   T4 =", T4)
print(" condenser inlet Enthalpy      H14 =", H14)
print(" condenser outlet Enthalpy      H4 =", H4)

print("     UACOND = ", UACOND)
print("       watt QCONDS =", QCONDS)
print("       watt QCONDC =", QCONDC)
print("       watt QSCC =", QSCC)

lstRest = cond(T4=T4, H4=H4, H14=H14, TS1=TS1,
               TC=TC, JC=JC,
               QCONDS=QCONDS, QCONDC=QCONDC, QSCC=QSCC,
               MROLD=MROLD, MREF=MREF, UACOND=UACOND, CFMC=CFMC,
               ICOND=ICOND)

print("\n\n==================Output============")
print("           K TS2 =", lstRest[0])
print("            K TC =", lstRest[1])
print("              JC =", lstRest[2])
print("           ICONC =", lstRest[3])

print("     kg/hr MROLD =", lstRest[4])

print("\n\n==========many trails test Output============")
[TS2, TC, JC, ICONC, MROLD] = lstRest

for i in range(50):
    JC = 2
    [TS2, TC, JC, ICONC, MROLD] = cond(T4=T4, H4=H4, H14=H14, TS1=TS1,
                   TC=TC, JC=JC,
                   QCONDS=QCONDS, QCONDC=QCONDC, QSCC=QSCC,
                   MROLD=MROLD, MREF=MREF, UACOND=UACOND, CFMC=CFMC,
                   ICOND=ICOND)
                   

    print ('Err if 1 ICONC=', ICONC, " Trail Count:", i)             
    if ICONC == 0:
        exit
        
    print ("    TS2, TC, MROLD", TS2, TC, MROLD)
