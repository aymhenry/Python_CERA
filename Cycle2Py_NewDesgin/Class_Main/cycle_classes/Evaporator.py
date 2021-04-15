# Python import

# User import
from cycle_classes.exf4Cond_Evap import exf4Cond_Evap
from cycle_classes.ErrorException import ErrorException
from cycle_classes.Trace import *

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Create Condenser object based on ICOND
#    input: IFRSH = 0  Natural Convection
#         : IFRSH = 1  Cross-Flow
#         : IFRSH = 2  Counter-Flow
#         : objCP = CoolProp object
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class Evaporator:
    def getObject(self, IFRSH, objCP):
        if (IFRSH == 0):  # Natural Convection
            objEvapCool = EvapCool_FFNat(IFRSH, objCP)

        elif (IFRSH == 1):  # Cross-Flow
            objEvapCool = EvapCool_FFCross(IFRSH, objCP)

        elif (IFRSH == 2):  # Counter-Flow
            objEvapCool = EvapCool_FFCount(IFRSH, objCP)

        else:
            objEvapCool = None
            raise ErrorException('IFRSH value error', 'Evap1000')
        return objEvapCool


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Evaprator cooling
#                   method (Natural, Cross, Counter-flow
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class EvapCool_Abstract (exf4Cond_Evap):
    def __init__(self, IFRSH, objCP):
        self.IFRSH = IFRSH
        self.objCP = objCP
        self.trace = Trace()

    def setParamters(self, ATOTE, CFME, TS3, N_EVAP,
                     USUPE, UTPE, TROOM, FZTEMP,
                     UA_FF, Q_HXS_FF, IWALL_FF, NUM_ZONE, IRFTYP):
        # ATOTE m2 Total Heat Transfer Surface Area
        # CFME Air Flow Rate Across Coil (L/s) [OA]
        # TS3 htf temperature entering fresh food evaporator
        # N_EVAP Number of Zones on Evaporator

        # USUPE Subcooling Heat Transfer Conductance, W/m2-C [OA]
        # UTPE Two-Phase Heat Transfer Conductance, W/m2-C [OA]
        # TROOM room temp K
        # FZTEMP Freezer Temperature (K)

        # UA_FF Evap: A/R In Fresh Food Section (Or Cabinet Walls)

        # IWALL_FF  FF (Or Cabinet) Evaporator Behind Liner
        # Q_HXS_FF W/K or j/sec-K, Both: A/R In Fresh Food Section 
        #    (Or Cabinet Walls) old unit sec-F/Btu(th)

        # NUM_ZONE   count no. of zones
        # IRFTYP Refrigeration Type (1 to 7)

        # by Dr.Omar
        # rho air = 1.354 kg/m3, CPair = 1.0058 kj/kg.K
        #    j/sec  = L/sec .kg/m3 kj/kg.K
        self.CFME = CFME   # watt/k see comment in CycleSolver

        self.UTPE = UTPE   # W/m2-c
        self.USUPE = USUPE  # W/m2-c
        self.UA_FF = UA_FF  # watt/K

        self.ATOTE = ATOTE   # m2
        self.TS3 = TS3      # K

        self.N_EVAP = N_EVAP

        self.TROOM = TROOM      # K
        self.FZTEMP = FZTEMP      # K

        self.Q_HXS_FF = Q_HXS_FF

        self.IWALL_FF = IWALL_FF
        self.NUM_ZONE = NUM_ZONE
        self.IRFTYP = IRFTYP

    def getExtarOutputs(self):
        pass

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def frsh(self, H5, H7, T5, TS3, TE, JE, QFRSH, MREF, UAFF=0):

        # calculate fresh food section exit temperature
        #   initialize
        MREF_kg_s = MREF / 3600
        TOL_FRSH = 0.1   # Tolerance

        FTE = [0.0, 0.0, 0.0]  # in Python only, to review no old data for this

        ICONE = 0

        # Python: MREF:Initial Guess For Refrigerant Mass Flow Rate (kg/s) [OA changed /hr to /s]
        # = (W)/(kg/s * j/kg) = %Ration [OA changed /hr to /s]
        ALPHA = QFRSH / (MREF_kg_s * (H7 - H5))

        if (QFRSH == 0.0):
            ALPHA = 0.01

        # estimate new value for exit temperature
        DELT = (TS3 - T5) * (1.0 - 1.0 / ALPHA)
        if (-DELT > TE[JE]):
            DELT = -0.5 * TE[JE]

        TEOUT = TE[JE] + DELT

        TS4 = TS3 - QFRSH / self.CFME   # K - [j/kg] [j/kg/K]
        if (TEOUT > TS3):
            TEOUT = TS3

        FTE[JE] = TEOUT - TE[JE]   # allways equall DELT !!!

        if (JE == 1):
            TENEW = TEOUT
        else:
            TENEW = TEOUT
            if (FTE[2] != FTE[1]):
                TENEW = TE[2] - FTE[2] * (TE[2] - TE[1]) / (FTE[2] - FTE[1])

            TE[1] = TE[2]
            FTE[1] = FTE[2]

        TENEW = (TENEW + TE[JE]) / 2.0

        if (TENEW > 1.05 * TE[JE]):
            TENEW = 1.05 * TE[JE]

        if (TENEW < 0.95 * TE[JE]):
            TENEW = 0.95 * TE[JE]

        if (TENEW > TS3):
            TENEW = TS3

        ERROR = abs(TENEW - TE[JE])

        # ==============this block modified by Ayman
        # Iteration is finised when ICONE = 1, i.e LECON = False
        # See source code Cycle.for Line 0801
        if (ERROR < TOL_FRSH):
            ICONE = 1  # ok no error
        else:
            ICONE = 0   # out of limit error

        # if (ERROR < TOL_FRSH):
            # ICONE = 0 # ok no error
        # else:
            # ICONE = 1 # out of limit error
        # ======================End of Ayman Modification

        JEOLD = JE  # useless not used
        TE[2] = TE[1]   # [OA added this line to store the previous value!]
        # J E = 1 #[OA changed from 2 to 1; we need to compare the new value to the old value]

        # TE[JE] = TENEW
        TE[1] = TENEW

        #  adjust exit air temp to 90% approach if natural convection
        self.trace.dr_omar("Modification in Unit by Ayman for imprical Equ.")
        JE = 2   # by Ayman to chk Dr omar
        if (self.IFRSH == 0):
            TS4 = 0.9 * TE[JE] + 0.1 * TS3

        dicRes = {'TS4': TS4,
                  'TE': TE,
                  'JE': JE,
                  'ICONE': ICONE    # 0=Free Error, 1=Error Found
                  }
        return dicRes

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Evaporator with Natural cooling
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


class EvapCool_FFNat(EvapCool_Abstract):  # IFRSH== 0
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.

    def evap_balance(self, MREF,
                     T5, H5, T7,
                     TDEW,
                     CPRVAP
                     ):

        HDEW = self.objCP.Property('H', T=TDEW, X=1)  # j/kg

        lstRes = self.ffnat(T5=T5, H5=H5, T7=T7,
                            TDEW=TDEW, HDEW=HDEW,
                            TS3=self.TS3,
                            CPRVAP=CPRVAP, IRFTYP=self.IRFTYP,
                            MREF=MREF
                            )

        dicRes = {'QFRSH': lstRes[0],  # Q
                  'FSUPE': lstRes[1],   # Fraction subcooling
                  'UAFF': lstRes[2]
                  }
        return dicRes

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def ffnat(self, MREF, T5, H5, T7, TDEW, HDEW, TS3, CPRVAP, IRFTYP):

        # subroutine  ffnat - calculates the fresh food evaporator
        # heat  transfer for a natural convection  evaporator

        # Calculate the radiation heat transfer heat transfer
        #  coefficient using small delta t approximation (black body)
        #  use the refrigerant dew point to evaluate h radiation
        MREF_kg_s = MREF / 3600

        SIGMA = 2.04326E-7
        EPS = 0.8   # emissivity of heat exchanger

        self.trace.dr_omar("Modification in Unit by Ayman.")
        # TENV = (self.TROOM + 459.6) / 1.8 # R
        # by Dr.Omar
        TENV = self.TROOM  # K

        TAVE = (T5 + T7) / 2.0   # K

        TAIR = TS3   # K
        # [OA] commented next line - not used elsewhere
        # FZTMPK = (self.FZTEMP + 459.6) / 1.8 # R

        # in Fortran HRAD is converted from kW/m2 K using eq:-
        # kW/m2 K = 0.04892 Btu/(s ft2 F)
        # so HRAD in kW/m2 K

        HRAD = SIGMA * (TAVE + TAIR) * (TAVE**2 + TAIR**2) * EPS    # kW/m2 K

        # get the net evaporator area
        AEVAP = self.ATOTE   # m2
        if (IRFTYP == 6):
            AEVAP = self.ATOTE + self.ATOTE   # m2

        # calculate the natural convection heat transfer coefficient
        DELTAT = TAIR - TAVE      # K
        if(DELTAT <= 0.0):
            DELTAT = 0.0001

        # by Dr.Omar Cancel next line
        # DELTA = DELTAT * 1.8 # DTemp F.

        TBAR = 0.67 * TAVE + 0.33 * TAIR
        A_NAT = 0.239 + 3.34E-04 * (273.0 - TBAR)

        # by Dr.Omar
        # HNAT = A_NAT * DELTA**0.33 * 20.44

        self.trace.dr_omar("Modification in Unit by Ayman for imprical Equ.")
        # HNAT = A_NAT * DELTAT**0.33 * 20.44
        HNAT = A_NAT * (DELTAT*1.8)**0.33 * 20.44   # kW/m2 K

        # Calculate combined air-side heat transfer coefficient
        UAIR = HRAD + HNAT  # kW/m2 K feedback from condenser class

        if (self.IWALL_FF == 1):
            UAIR = 1.0 / (1.0 / UAIR + 0.1389 / 20.44)   # kW/m2 K
            # UAIR by ayman units is power/Temp/sq-lenght
            # Btu/hr/Feh/(length * Length)

        # by Ayman ( not in Fortran)
        # info is feedback from condener
        UAIR = UAIR * 1000   # convert to W/m2 K

        self.trace.dr_omar("this is not SI units")
        # UA_FF is  W/K,  BTU = 1.0548 J
        # Q_IN_WALL = 1.0548 * 1.8 * self.UA_FF * \
        # (TENV - TAVE) / AEVAP + self.Q_HXS_FF / AEVAP

        # UA_FF W/C,  W/C = 1.8961 Btu/(h.Feh);
        Q_IN_WALL = self.UA_FF * (TENV - TAVE) / AEVAP + self.Q_HXS_FF / AEVAP   # watt/m2

        # Calculate the heat transfer assuming that the air side
        #  resistance dominates
        #
        # Calculate the are necessary to evaporate the refrigerant
        QTPNEC = MREF_kg_s * (HDEW - H5)   # j/s [OA changed /hr to /s]
        # QTPNEC[watt] /( UAIR[watt/m2 K] * [K])
        ATPNEC = QTPNEC / (UAIR * DELTAT + Q_IN_WALL)   # m2

        # Calculate the superheating area fraction
        if (ATPNEC < AEVAP):
            QTPE = QTPNEC   # j/s [OA changed to /hr to /s]
            ASUPE = AEVAP - ATPNEC  # m2
            QSUPMX = MREF_kg_s * CPRVAP * (TAIR - TDEW)   # j/s [OA changed /hr to /s]
            # J/s [OA changed /hr to /s]
            QSUPE = UAIR * ASUPE * DELTAT + ASUPE * Q_IN_WALL   # watt

            if (QSUPE > QSUPMX):
                QSUPE = QSUPMX
            QTOTE = QTPE + QSUPE   # j/s [OA changed /hr to /s]
            FSUPE = ASUPE / AEVAP   # unitless
        else:
            # UAIR[watt/m2 K] * [m2] [K],   Q_IN_WALL[watt/m2]
            QTOTE = UAIR * AEVAP * DELTAT + AEVAP * Q_IN_WALL  # watt
            FSUPE = 0    # unitless

        UAFF = UAIR * AEVAP   # watt/K

        # watt, -, watt/K
        return [QTOTE, FSUPE, UAFF]


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Evaporator with Cross cooling
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class EvapCool_FFCross (EvapCool_Abstract):  # IFRSH== 1
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def evap_balance(self, MREF,
                     T5, H5,
                     TDEW,
                     CPRVAP,
                     P5, P7
                     ):

        HDEW = self.objCP.Property('H', T=TDEW, X=1)  # j/kg

        lstRes = self.ffcross(T5_S=T5, H5_S=H5,
                              TDEW_S=TDEW, HDEW_S=HDEW,
                              TS3=self.TS3,
                              CPR=CPRVAP,
                              PIN=P5, POUT=P7,
                              MREF=MREF, NUM_ZONE=self.NUM_ZONE
                              )

        dicRes = {'QFRSH': lstRes[0],  # watt
                  'FSUPE': lstRes[1],  # Fraction subcooling
                  'UAFF': lstRes[2]    # watt/K
                  }

        return dicRes

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def ffcross(self, MREF,
                H5_S, T5_S, HDEW_S, TDEW_S,
                TS3,
                CPR,
                PIN, POUT,
                NUM_ZONE
                ):

        # solves for the fresh food evaporator
        #   heat transfer for cross heat exchanger

        AREA_TOL = 0.001

        # initialize
        T5 = T5_S   # K
        H5 = H5_S   # K
        HDEW = HDEW_S   # j/kg
        TDEW = TDEW_S   # j/kg

        DELP = (PIN - POUT) / (NUM_ZONE)    # pa
        DELH = (HDEW - H5) / (NUM_ZONE)    # pa
        P5 = POUT   #  pa

        QSUP = 0.0
        QTPC = 0.0

        ASUP = 0
        ATPC = 0

        TAIR = TS3     # K

        self.trace.dr_omar("to check Ayman Modification")  # by Dr.Omar
        # rho air = 1.354 kg/m3, CPair = 1.0058 kj/kg.K
        #    j/sec  = L/sec .kg/m3 kj/kg.K
        # CAIR = self.CFME * 1.354 * 1.0058   # [OA]
        CAIR = self.CFME   # watt/K see comment in CycleSolver
        HAVE_NOT_USED_FULL_AREA = True

        # begin  with two-phase area
        ALEFT = self.ATOTE      # m2

        for N in range(1, NUM_ZONE):
            PDEW = P5 + DELP    # pa
            HDEW = H5 + DELH    # pa

            # [TDEW, XQ, XL, XV, VL, VV, HL, HV] = self.hpin(HDEW, PDEW, X)
            TDEW = self.objCP.Property('T', H=HDEW, P=PDEW)  # K

            if(HAVE_NOT_USED_FULL_AREA):
                CPRTP = (HDEW - H5) / abs(TDEW - T5 + 0.0001) # j/kg K
                CRTP = MREF_kg_s * CPRTP   # watt/K

                # determine CMIN and CMAX in the two-phase region

                CAIR = (ALEFT / self.ATOTE) * self.CFME    # watt/K

                # if (CAIR <= CRTP):
                #    CMINTP = CAIR
                #    CMAXTP = CRTP
                # else:
                #    CMINTP = CRTP
                #    CMAXTP = CAIR

                CMINTP = min (CAIR, CRTP)   # watt/K
                CMAXTP = max (CAIR, CRTP)   # watt/K

                # is area big enough for condensation

                QMAX = CMINTP * (TAIR - T5)     # watt
                QDUM = MREF_kg_s * (HDEW - H5)     # watt

                EFF_TPC = QDUM / QMAX        # unitless

                # [unitless]  [1/m2]
                [EFFTPC, DEXDAR] = self.exf(2, ALEFT, self.UTPE,
                                            CMINTP, CMAXTP)

                if(EFFTPC <= EFF_TPC):  # Need more area
                    ATPC = ATPC + ALEFT     # m2
                    HAVE_NOT_USED_FULL_AREA = False

                    # begin iteration process to determine solution for the
                    #   two phase region
                    #
                    # initialize variables

                else:
                    ADUM = 0.9 * ALEFT      # m2
                    LOOKING_FOR_AREA = True

                    ICOUNT = 0
                    QTOL = 1.0

                    while (LOOKING_FOR_AREA):
                        ICOUNT = ICOUNT + 1
                        if(ICOUNT > 100):
                            LOOKING_FOR_AREA = False
                            continue

                        CAIR = (ADUM / self.ATOTE) * \
                            self.CFME
                        # if(CAIR <= CRTP):
                        #    # CMINTP = CAIR
                        #    # CMAXTP = CRTP
                        # else:
                        #    # CMINTP = CRTP
                        #    # CMAXTP = CAIR

                        CMINTP = min (CAIR, CRTP)   # watt/K
                        CMAXTP = max (CAIR, CRTP)   # watt/K

                        QMAX = CMINTP * (TAIR - T5)    # watt
                        EFF_TPC = QDUM / QMAX          # unitless

                        # [unitless]  [1/m2]
                        [EFFTPC, DEXDAR] = self.exf(2, ADUM, self.UTPE,
                                                   CMINTP, CMAXTP)

                        ERROR = abs(QTOL)
                        if(ERROR <= AREA_TOL):
                            LOOKING_FOR_AREA = False
                            # Continue [OA commented this line - no continue in python!]
                            continue  # [Ayman commented fixed to lower case ok]

                        QRAT = EFFTPC * QMAX / QDUM           # unitless
                        QTOL = 1.0 - QRAT          # unitless

                        DAREA = ADUM * (1.0 - QRAT)   # m2

                        DAREA_MIN = -0.75 * ADUM   # m2
                        DAREA_MAX = 0.50 * (ALEFT - ADUM)   # m2

                        if(DAREA < DAREA_MIN):
                            DAREA = DAREA_MIN      # m2
                        if(DAREA > DAREA_MAX):
                            DAREA = DAREA_MAX   # m2

                        ADUM = ADUM + DAREA   # m2

                    ATPC = ATPC + ADUM   # m2

                QTPC = QTPC + EFFTPC * CMINTP * (TAIR - T5)  # watt

                ALEFT = self.ATOTE - ATPC  # m2

                H5 = H5 + DELH      # j/kg
                T5 = TDEW           # K
                P5 = P5 + DELP      # pa
                N = N + 1

        if(ALEFT <= 0.0):
            HAVE_NOT_USED_FULL_AREA = False

        # continue with desuperheating area

        HDEW = HDEW_S      # j/kg
        TDEW = TDEW_S      # K

        if(HAVE_NOT_USED_FULL_AREA):
            # MREF_kg_s[kg/sec] * CPR[[j/kg K]] *
            CR = MREF_kg_s * CPR    # watt/K

            # determine cmin and cmax in the two-phase region
            CAIR = (ALEFT / self.ATOTE) * self.CFME      # watt/K

            # if(CAIR  <=  CR) :
            # CMINDS = CAIR
            # CMAXDS = CR
            # else:
            # CMINDS = CR
            # CMAXDS = CAIR

            CMINDS = min(CR, CAIR)
            CMAXDS = max(CR, CAIR)

            # determine the net heat transfer
            # [unitless]  [1/m2]
            [EFFDSC, DEXDAR] = self.exf(2, ALEFT, self.USUPE,
                                        CMINDS, CMAXDS)

            QSUP = CMINDS * EFFDSC * (TS3 - TDEW)   # watt

            ASUP = ALEFT

        #  calculate the fractional subcooling and superheating regions

        FSUPE = ASUP / self.ATOTE   # unitless
        QTOTE = QSUP + QTPC      # watt

        # USUPE, UTPE both [watt/m2-c], 
        UAFF = self.ATOTE * FSUPE * self.USUPE \
            + self.ATOTE * (1.0 - FSUPE) * self.UTPE     # watt/K

        # watt, -, watt/K
        return [QTOTE, FSUPE, UAFF]


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Evaporator with Counter-flow cooling
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class EvapCool_FFCount (EvapCool_Abstract):  # IFRSH== 2
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def evap_balance(self, MREF,
                     T5, H5,
                     TDEW,
                     CPRVAP,
                     P5, P7
                     ):

        HDEW = self.objCP.Property('H', T=TDEW, X=1)  # j/kg

        lstRes = self.ffcount(T5_S=T5, H5_S=H5,
                              TDEW_S=TDEW, HDEW_S=HDEW,
                              TS3=self.TS3,
                              CPR=CPRVAP,
                              PIN=P5, POUT=P7,
                              MREF=MREF, NUM_ZONE=self.NUM_ZONE
                              )

        dicRes = {'QFRSH': lstRes[0],  # watt
                  'FSUPE': lstRes[1],  # Fraction subcooling
                  'UAFF': lstRes[2]    # watt/K
                  }

        return dicRes

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.
    def ffcount(self, MREF,
                H5_S, T5_S,
                HDEW_S, TDEW_S,
                TS3,
                CPR,
                PIN, POUT,
                NUM_ZONE
                ):

        # subroutine ffcount - solves for the fresh food evaporator
        #    heat transfer for counterflow heat exchanger
        
        MREF_kg_s = MREF / 3600
        AREA_TOL = 0.001  # Area Tolerance
        NCALL = 0

        # initialize
        ICOUNT = 0

        T5 = T5_S       # K
        H5 = H5_S       # K
        HDEW = HDEW_S       # j/kg
        TDEW = TDEW_S       # j/kg

        DELP = (POUT - PIN) / (NUM_ZONE)        # pa
        DELH = (HDEW - H5) / (NUM_ZONE)         # pa
        P5 = PIN           # pa

        QSUP = 0.0
        QTPC = 0.0
        QTOTE_LAST = 0.0

        ASUP = 0.0
        ATPC = 0.0

        if(NCALL == 0):
            TAIR = TS3 - 2             # K
            TAIR_GUESS = TAIR          # K
            NCALL = 1
        else:
            # by Ayman
            # TAIR = TAIR_GUESS
            TAIR_GUESS = TS3

        CAIR = self.CFME    # watt/K
        HAVE_NOT_USED_FULL_AREA = True

        # begin with two-phase area
        CONVERGED = False
        while (not CONVERGED):
            ICOUNT = ICOUNT + 1
            ALEFT = self.ATOTE  # m2

            for N in range(1, NUM_ZONE + 1):
                PDEW = P5 + DELP    # pa
                HDEW = H5 + DELH    # pa

                if(HAVE_NOT_USED_FULL_AREA):
                    # [TDEW, XQ, XL, XV, VL, VV, HL,
                    #  HV] = self.hpin(HDEW, PDEW, X)
                    TDEW = self.objCP.Property('T', H=HDEW, P=PDEW)  # K

                    CPRTP = (HDEW - H5) / abs(TDEW - T5 + 0.0001)  # j/kg K
                    CRTP = MREF_kg_s * CPRTP   # watt

                    # determine cmin and cmax in the two-phase region
                    # if(CAIR <= CRTP):
                    #    # CMINTP = CAIR
                    #    # CMAXTP = CRTP
                    # else:
                    #    # CMINTP = CRTP
                    #    # CMAXTP = CAIR

                    CMINTP = min (CAIR, CRTP)   # watt/K
                    CMAXTP = max (CAIR, CRTP)   # watt/K
            
                    #  is area big enough for evaporation

                    QDUM = MREF_kg_s * (HDEW - H5)   # watt
                    TAIR_END = TAIR + QDUM / CAIR    # K
                    # # watt/K * K
                    QMAX = CMINTP * (TAIR_END - T5)     # watt

                    EFF_TPC = QDUM / QMAX   # unitless
                    
                    # [unitless]  [1/m2]
                    [EFFTPC, DEXDAR] = self.exf(1, ALEFT, self.UTPE,
                                                CMINTP, CMAXTP
                                                )

                    if(EFFTPC <= EFF_TPC):
                        ATPC = ATPC + ALEFT
                        HAVE_NOT_USED_FULL_AREA = False

                        # begin iteration process to determine solution for the
                        # two phase region
                        #
                        #  initialize variables

                    else:
                        ADUM = 0.9 * ALEFT      # m2
                        LOOKING_FOR_AREA = True

                        ILOOK = 0
                        while (LOOKING_FOR_AREA):
                            ILOOK = ILOOK + 1
                            
                            # [unitless]  [1/m2]
                            [EFFTPC, DEXDAR] = self.exf(1, ADUM, self.UTPE,
                                                        CMINTP, CMAXTP
                                                        )

                            ERROR = abs(EFFTPC - EFF_TPC)
                            if(ERROR <= AREA_TOL or ILOOK >= 10):
                                LOOKING_FOR_AREA = False
                                continue

                            # DEXDAR [1/m2]
                            DAREA = - (EFFTPC - EFF_TPC) / DEXDAR   # m2
                            DAREA_MIN = -0.75 * ADUM     # m2
                            DAREA_MAX = 0.50 * (ALEFT - ADUM)   # m2

                            if(DAREA < DAREA_MIN):
                                DAREA = DAREA_MIN     # m2
                            if(DAREA > DAREA_MAX):
                                DAREA = DAREA_MAX     # m2

                            if(abs(DAREA) <= 0.001 * self.ATOTE):
                                LOOKING_FOR_AREA = False
                                continue

                            ADUM = ADUM + DAREA    # m2
                        ATPC = ATPC + ADUM     # m2

                    QTPC = QTPC + EFFTPC * QMAX       # watt
                    TAIR = TAIR + EFFTPC * QMAX / CAIR   # K

                ALEFT = self.ATOTE - ATPC    # m2
                H5 = H5 + DELH  # j/kg
                T5 = TDEW       # K
                P5 = P5 + DELP  # pa

            if(ALEFT <= 0.0):
                HAVE_NOT_USED_FULL_AREA = False

            #   continue with desuperheating area
            HDEW = HDEW_S    # j/kg
            TDEW = TDEW_S    # K

            if(HAVE_NOT_USED_FULL_AREA):
                CR = MREF_kg_s * CPR  # watt/K

                #  determine cmin and cmax in the two-phase region
                # if(CAIR <= CR):
                #    # CMINDS = CAIR
                #    # CMAXDS = CR
                # else:
                #    # CMINDS = CR
                #    # CMAXDS = CAIR

                CMINDS = min (CAIR, CR)   # watt/K
                CMAXDS = max (CAIR, CR)   # watt/K
                
                # determine the net heat transfer
                # [unitless]  [1/m2]
                [EFFDSC, DEXDAR] = self.exf(1, ALEFT, self.USUPE, CMINDS, CMAXDS)

                # CMINDS[watt/K] *[K]
                QSUP = CMINDS * EFFDSC * (TS3 - TDEW)   # watt
                TAIR = TAIR + QSUP / CAIR    # K

                ASUP = ALEFT    # m2

            self.trace.dr_omar("Strang Equation !!!")
            QTOTE = QSUP + QTPC   # watt
            ERROR_Q = abs(1 - QTOTE_LAST / QTOTE)   # equall zero all times
            QTOTE_LAST = QTOTE

            ERROR = abs(TAIR - TS3)
            if(ERROR < 0.05 or ERROR_Q <= 0.01 or ICOUNT >= 10):
                CONVERGED = True
                continue
            else:
                DEL_AIR = TAIR - TS3    # K

                TAIR_NEW = TAIR_GUESS - 0.5 * DEL_AIR    # K
                if(TAIR_NEW <= T5_S):
                    TAIR_NEW = 0.9 * T5_S + 0.1 * TAIR_GUESS    # K
                TAIR = TAIR_NEW    # K
                TAIR_GUESS = TAIR    # K

                T5 = T5_S    # K
                H5 = H5_S    # K
                HDEW = HDEW_S    # j/kg K
                TDEW = TDEW_S    # j/kg K
                P5 = PIN            # pa

                QSUP = 0.0
                QTPC = 0.0
                ASUP = 0
                ATPC = 0

                HAVE_NOT_USED_FULL_AREA = True

        # calculate the fractional subcooling and superheating regions
        FSUPE = ASUP / self.ATOTE       # unitless
        
        # USUPE, UTPE both [watt/m2-c], 
        UAFF = self.ATOTE * FSUPE * self.USUPE + \
            self.ATOTE * (1.0 - FSUPE) * self.UTPE
        # watt, -, watt/K
        return [QTOTE, FSUPE, UAFF]
