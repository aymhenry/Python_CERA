# Python Import
import datetime

# User Import
from common_classes.FileAccess import FileAccess

# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = .
class View:
    def __init__(self, dt, objCycleSlover, str_file_cycle, str_path_cycle):
        self.str_file_cycle = str_file_cycle
        self.str_path_cycle = str_path_cycle

        self.dt = dt        # input data object
        # will add some other data to the given ds object in this class
        self.ds = objCycleSlover     # soultion in objCycleSlover

        #    OPEN OUTPUT FILE
        self.prn = FileAccess(self.str_file_cycle, "write")  # IO_Cycle Tag
        # self.prn.write_or_terminate(" ")

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def showError(self, strMsg, fltValue=0.0, isOnlyTest=False):
        print("\n\n==========================Error detected =================")
        if isOnlyTest:
            print("    Error : ", strMsg)
        else:
            #print('{},,{}'.format(strMsg, fltValue))
            print("    Error : ", strMsg + ",, %10.3f" % (fltValue))
        print("==========================================================\n\n")

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def print_width(self, arr_data):
        col_width = 10
        print("".join(str(word)[:8].ljust(col_width) for word in arr_data))


    def setBasicData(self):
        self.ds.AIRTMP = [0.0] * (16 + 1)
        self.ds.AIRTMP = [True,  # add new item to start list at 1    \
                                     False, False, True, False, False,    \
                                     True, False, False, False, False,    \
                                     False, True, False, True, False]

        # in python add extra item for item 0
        self.dt.HSTATE = [
            '',
            'COMP IN',
            'COMP DIS',
            'COND IN',
            'COND DEW',
            'COND BUB',
            'COND OUT',
            'LIQ LINE',
            'SUBCOOL1',
            'SUBCOOL2',
            'FREZ IN ',
            'FREZ OUT',
            'FRSH IN ',
            'FRSH DEW',
            'FRSH OUT',
            'HX1 OUT ']

        # in python add extra item for item 0
        self.ds.MSTATE = [
            '',
            'COMP IN',
            'COMP DIS',
            'COND IN',
            'COND DEW',
            'COND BUB',
            'COND OUT',
            'LIQ LINE',
            'SUBCOOL ',
            'EVAP IN ',
            'EVAP DEW',
            'EVAP OUT',
            'HX OUT  ']

        # in python add extra item for item 0
        self.ds.LPNT = [0, 1, 2, 14, 3, 11, 4, 16, 6, 10, 8, 9, 5, 12, 7, 13]

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def show_rep(self):
        self.setBasicData()

        #    SET UP LOGICAL VECTOR ON AIR TEMPERATURES
        if (self.dt.ICYCL == 2):
            self.ds.AIRTMP[10] = True
            self.ds.AIRTMP[11] = True
        else:
            self.ds.AIRTMP[10] = False
            self.ds.AIRTMP[11] = False

        # #    OPEN OUTPUT FILE
        # objCycOut = FileAccess(self.str_file_cycle, "write")  # IO_Cycle Tag
        self.prn.write_or_terminate(" ")

        now = datetime.datetime.now()
        if (self.ds.NCYC != 2):
            self.prn.write_or_terminate(
                (now.strftime("%H %M %S %d %b %Y")) +
                " - Python Output aymhenry@gmail")

        #    OUTPUT INFORMATION ON TYPE OF CYCLE
        if (self.dt.ICYCL == 1):
            if (self.dt.ICYCLS == 1):
                self.prn.write_or_terminate('STANDARD ONE EVAPORATOR CYCLE')
            else:
                if (self.dt.ITYPE == 1 or self.dt.ITYPE == 4):
                    self.prn.write_or_terminate(
                        'DUAL EVAP CYCLE: FRESH FOOD LOOP')
                else:
                    self.prn.write_or_terminate(
                        'DUAL EVAP CYCLE: FREEZER LOOP')

        if (self.dt.ICYCL == 2):
            if (self.dt.ICYCLS == 2):
                self.prn.write_or_terminate('LORENZ CYCLE')
            else:
                self.prn.write_or_terminate('DUAL EVAP CYCLE')

        if (self.dt.ICYCL == 3):
            if (self.ds.NCYC == 1):
                self.prn.write_or_terminate('DUAL LOOP CYCLE - FREEZER')
            else:
                self.prn.write_or_terminate(" ")
                self.prn.write_or_terminate('DUAL LOOP CYCLE - FRESH FOOD')

        self.prn.write_or_terminate(" ")
        #    OUTPUT REFRIGERATION MIXTURE INFORMATION

        ''' later ==============
    X2 = 100.0 * self.ds.XM[1]
    self.prn.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(X2, self.dt.HREF[ self.dt.IR[1] ])  )

    if (self.dt.NC >  1) :
        for I in range (2, self.dt.NC+1) :
        X2 = 100.0 * self.ds.XM[I]
        self.prn.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(X2, self.dt.HREF[ self.dt.IR[1] ])  ) # fixed in python
    self.prn.write_or_terminate(" ")
    '''

        self.prn.write_or_terminate("OUTPUT RESULTS")

        # print Error message if occured in Cycle, to be improved later
        # str_err need to be created in Cycle Solver
        # if self.ds.str_err != "":
            # self.prn.write_or_terminate(self.ds.str_err)

        # -----------------------------
        #
        #    OUTPUT WARNING IF CONVERGENCE FORCED BY HOT KEY <F10>.
        #
        if (self.ds.LQUIT):
            self.prn.write_or_terminate(
                "Convergence forced after, iterations, %3.0f," %(self.dt.IC))

        #
        #    PRINT ERROR MESSAGES if NON-CONVERGENCE
        #
        if (self.ds.LECON or self.ds.LCCON or self.ds.I_ERROR_INTER > 0):
            # in Fortran, no use in Python
            # LWIND = 5
            # if (self.ds.LECON and self.ds.LCCON):
                # LWIND = 7

            if (self.ds.LECON):
                self.prn.write_or_terminate(
                    'EVAPORATOR ITERATION DID NOT CONVERGE,  %9.2f, %9.2f ' %
                    (self.ds.TE[1] - 273.11, self.ds.TE[2] - 273.11))

                # self.showError("EVAPORATOR ITERATION DID NOT CONVERGE", "")

            if (self.ds.LCCON):
                self.prn.write_or_terminate(
                    'CONDENSER ITERATION DID NOT CONVERGE,  %9.2f, %9.2f ' %
                    (self.ds.TC[1] - 273.11, self.ds.TC[2] - 273.11))
                self.showError(
                    'CONDENSER ITERATION DID NOT CONVERGE ,  %9.2f, %9.2f ' %
                    (self.ds.TC[1] - 273.11, self.ds.TC[2] - 273.11))

            if (self.ds.I_ERROR_INTER > 0):
                self.prn.write_or_terminate(
                    'INTERCHANGER SUPERHEAT NOT POSSIBLE')
                self.showError("INTERCHANGER SUPERHEAT NOT POSSIBLE")

            input("Press Enter to continue...")

        #
        #    OUTPUT RESULTS.  BEGIN BY CONVERTING TO ENGLISH UNITS.
        #

        #TENV = (self.dt.TROOM + 459.6) / 1.8
        TENV = self.dt.TROOM

        if (self.ds.T[16] < TENV):
            self.dt.I_LIQUID_LINE = 1

        self.prn.write_or_terminate(""", STATE
                                      , T(C), T(C)
                                      , P, H, V, S
                                      , XL, XV, XQ""")

        self.prn.write_or_terminate(""", , AIR,   REF
                                      ,  kPa,  kj/kg ,  m3/kg ,  kj/kg-C
                                      , (MASS FRAC),(MASS FRAC),(MASS FRAC)
                                      """)


        self.print_width(['#', 'Point', 'STATE'
                             , 'T(C)', 'T(C)'
                             , 'P', 'H', 'V', 'S'
                             #, 'XL', 'XV', 'XQ'
                             ])

        self.print_width(['', '', '', 'AIR', 'REF'
                          , 'kPa', 'kj/kg', 'm3/kg', 'kj/kg-C'
                          # , 'MASS-FRAC', 'MASS-FRAC', 'MASS-FRAC'
                          ])

        K = 1
        if (self.dt.ICYCL == 2):
            while (K <= 15):
                J = self.ds.LPNT[K]
                # self.ds.TS[J] = self.ds.TS[J] - 273.11
                # self.ds.T[J] = self.ds.T[J] - 273.11
                # self.ds.V[J] = self.ds.V[J] / 10.0

                # if (self.ds.XQ[J] > 1.0):
                    # self.ds.XQ[J] = 1.0
                # if (self.ds.XQ[J] < 0.0):
                    # self.ds.XQ[J] = 0.0

                if (self.ds.AIRTMP[K]):
                    self.prn.write_or_terminate(
                        "%d, %s , %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f" %
                            (K,
                             self.dt.HSTATE[K],
                             self.ds.TS[J]  - 273.11,
                             self.ds.T[J]  - 273.11,
                             self.ds.P[J] / 1000,
                             self.ds.H[J] / 1000,
                             self.ds.V[J],
                             self.ds.S[J] / 1000
                             # self.ds.XL[1][J],
                             # self.ds.XV[1][J],
                             # self.ds.XQ[J]
                             )
                         )

                    # print ( "%d,%d, %s , %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %s, %s, %s"    \
                    #    %(K,J,self.dt.HSTATE[K],self.ds.TS[J],self.ds.T[J],self.ds.P[J],self.ds.H[J],self.ds.V[J],  self.ds.S[J],self.ds.XL[1][J],self.ds.XV[1][J],self.ds.XQ[J]) )

                    self.print_width([K,
                                      J,
                                      self.dt.HSTATE[K],
                                      self.ds.TS[J] - 273.11,
                                      self.dt.T[J] - 273.11,
                                      self.ds.P[J] / 1000,
                                      self.ds.H[J] / 1000,
                                      self.ds.V[J] / 1000,
                                      self.ds.S[J] / 1000
                                      # self.ds.XL[1][J],
                                      # self.ds.XV[1][J],
                                      # self.ds.XQ[J]
                                      ])
                else:

                    self.prn.write_or_terminate(
                        " %d, %s,  N/A, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f" %
                        (K,
                         self.dt.HSTATE[K],
                         self.ds.T[J]  - 273.11,
                         self.ds.P[J] / 1000,
                         self.ds.H[J] / 1000,
                         self.ds.V[J],
                         self.ds.S[J] / 1000
                         # self.ds.XL[1][J],
                         # self.ds.XV[1][J],
                         # self.ds.XQ[J]
                         ))
                    # print ( "%d,%d, %s , N/A, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %s, %s, %s"    \
                    #    %(K,J,self.dt.HSTATE[K], self.ds.T[J],self.ds.P[J],self.ds.H[J],self.ds.V[J],  self.ds.S[J],self.ds.XL[1][J],self.ds.XV[1][J],self.ds.XQ[J]) )

                    self.print_width([K,
                                      J,
                                      self.dt.HSTATE[K],
                                      'N/A',
                                      self.ds.T[J] - 273.11,
                                      self.ds.P[J] / 1000,
                                      self.ds.H[J] / 1000,
                                      self.ds.V[J],
                                      self.ds.S[J] / 1000
                                      # self.ds.XL[1][J],
                                      # self.ds.XV[1][J],
                                      # self.ds.XQ[J]
                                      ])

                K = K + 1

        else:
            M = 0
            K = 0
            while (M <= 14):
                M = M + 1
                J = self.ds.LPNT[M]

                # self.ds.TS[J] = self.ds.TS[J] - 273.11
                # self.ds.T[J] = self.ds.T[J] - 273.11
                # self.ds.V[J] = self.ds.V[J] #/ 10.0
                # if (self.ds.XQ[J] > 1.0):
                    # self.ds.XQ[J] = 1.0
                # if (self.ds.XQ[J] < 0.0):
                    # self.ds.XQ[J] = 0.0

                if 9 <= M <= 11:
                    continue
                K = K + 1
                if (self.ds.AIRTMP[M]):

                    self.prn.write_or_terminate(
                        "%d, %s, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f" %
                        (K,
                         self.ds.MSTATE[K],
                         self.ds.TS[J] - 273.11,
                         self.ds.T[J]  - 273.11,
                         self.ds.P[J] / 1000,
                         self.ds.H[J] / 1000,
                         self.ds.V[J],
                         self.ds.S[J] / 1000
                         # self.ds.XL[1][J],
                         # self.ds.XV[1][J],
                         # self.ds.XQ[J]
                         ))

                    # print ("%d,%d, %s , %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %s, %s, %s"    \
                    #    %(K,J, self.ds.MSTATE[K], self.ds.TS[J],self.ds.T[J],self.ds.P[J],self.ds.H[J],self.ds.V[J],  self.ds.S[J],self.ds.XL[1][J],self.ds.XV[1][J],self.ds.XQ[J]) )
                    self.print_width([K, J ,
                                      self.ds.MSTATE[K],
                                      self.ds.TS[J]  - 273.11,
                                      self.ds.T[J]  - 273.11,
                                      self.ds.P[J] / 1000,
                                      self.ds.H[J] / 1000,
                                      self.ds.V[J],
                                      self.ds.S[J] / 1000
                                      # self.ds.XL[1][J],
                                      # self.ds.XV[1][J],
                                      # self.ds.XQ[J]
                                      ])
                else:

                    self.prn.write_or_terminate(
                        "%d, %s , N/A, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f" %
                        (K,
                             self.ds.MSTATE[K],
                             self.ds.T[J] - 273.11,
                             self.ds.P[J] / 1000,
                             self.ds.H[J] / 1000,
                             self.ds.V[J],
                             self.ds.S[J] / 1000
                            )
                         )

                    # print ("%d,%d, %s , N/A, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %s, %s, %s"    \
                    #    %(K,J, self.ds.MSTATE[K], self.ds.T[J],self.ds.P[J],self.ds.H[J],self.ds.V[J],  self.ds.S[J],self.ds.XL[1][J],self.ds.XV[1][J],self.ds.XQ[J]) )

                    self.print_width([K,
                                      J,
                                      self.ds.MSTATE[K],
                                      'N/A',
                                      self.ds.T[J]  - 273.11,
                                      self.ds.P[J] / 1000,
                                      self.ds.H[J] / 1000,
                                      self.ds.V[J],
                                      self.ds.S[J] / 1000
                                      # self.ds.XL[1][J],
                                      # self.ds.XV[1][J],
                                      # self.ds.XQ[J]]
                                      ])

        #================================
        #    NORMALIZE BY THE MASS FLOW

        FLOW = self.ds.FLOW * self.ds.MREF / self.ds.MREFSV

        # self.dt.DISP = self.dt.DISP     #/ 1.6387E-05
        # self.ds.W = 0.4302 * self.ds.W * self.ds.FLOW * 1.0548
        # self.dt.QZ = 0.4302 * self.dt.QZ * self.ds.FLOW * 1.0548
        # self.dt.QE = 0.4302 * self.dt.QE * self.ds.FLOW * 1.0548
        # self.ds.QC = 0.4302 * self.ds.QC * self.ds.FLOW * 1.0548

        self.ds.W =  self.ds.W * FLOW
        self.ds.QZ = self.ds.QZ * FLOW
        self.ds.QE = self.ds.QE * FLOW
        self.ds.QC = self.ds.QC * FLOW
        
        self.ds.COPR = (self.ds.QE + self.ds.QZ)/self.ds.W
        #
        #    REST OF THE CONVERSIONS
        # done on printing time

        #
        #    OUTPUT SUMMARY TABLE OF RESULTS
        #
        self.prn.write_or_terminate('CYCLE PERFORMANCE SUMMARY')
        if (self.dt.ITYPE == 1):
            self.prn.write_or_terminate(
                'EVAPORATOR CAPACITY,  %9.2f, KJ/HR,  %9.2f, W' %
                (self.ds.QE, self.ds.QE))
        else:
            self.prn.write_or_terminate(
                'FRESH FOOD EVAPORATOR CAPACITY,  %9.2f, KJ/HR,  %9.2f, W' %
                (self.ds.QE, self.ds.QE))

            self.prn.write_or_terminate(
                'FREEZER EVAPORATOR CAPACITY,     %9.2f, KJ/HR,  %9.2f, W' %
                (self.ds.QZ, self.ds.QZ))

        self.prn.write_or_terminate(
            'CONDENSER HEAT REJECTION RATE,  %9.2f, KJ/HR,  %9.2f, W' %
            (self.ds.QC, self.ds.QC))

        self.prn.write_or_terminate(
            'COMPRESSOR POWER REQUIREMENT,     %9.2f, KJ/HR,  %9.2f, W' %
            (self.ds.W, self.ds.W ))

        self.prn.write_or_terminate('''COEFFICIENT OF PERFORMANCE,
            %9.2f''' %(self.ds.COPR))

        if (self.dt.IRFTYP <= 3 and self.dt.ICYCL ==
                1 and self.dt.ICAB != 0 and self.ds.IFRSH != 0):
            self.prn.write_or_terminate(
                'FRACTION AIR TO FRESH FOOD,     %9.2f, (SINGLE EVAPORATOR CYCLE)' %
                (self.dt.FF_FRACT))

        self.prn.write_or_terminate(" ")

        if (self.dt.IMAP == 1):
            self.prn.write_or_terminate('''ESTIMATED COMPRESSION EFFICIENCY,
                %9.2f, (COMPRESSOR EER MODEL)''' %(self.ds.ETAS))

            self.prn.write_or_terminate('''ESTIMATED MOTOR-PUMP EFFICIENCY,
                %9.2f, (COMPRESSOR EER MODEL)''' 
                %(self.ds.EFFC / self.ds.ETAS))

            self.prn.write_or_terminate('''ESTIMATED CLEARANCE VOLUME,
                %9.2f, (COMPRESSOR EER MODEL)''' %(self.dt.CE))

            self.prn.write_or_terminate('''ESTIMATED SHELL LOSS,
                %9.2f, (COMPRESSOR EER MODEL)''' %(self.ds.QCAN))

            self.prn.write_or_terminate(''''ESTIMATED DISC TUBE HEAT LOSS,
                %9.2f, (COMPRESSOR EER MODEL)''' %(self.ds.QHILO))

        self.prn.write_or_terminate("HEAT EXCHANGER PERFORMANCE SUMMARY")
        self.prn.write_or_terminate('''EXCHANGER, EFFECTIVENESS,
            SUBCOOLED FRACTION, SUPERHEATED FRACTION''')

        if (self.dt.ITYPE == 1):
            if (self.ds.IFRSH != 0):
                self.prn.write_or_terminate(
                    'EVAPORATOR,     %9.2f, N/A, ,%9.2f' %
                    (self.ds.ETAE * 100, self.ds.FSUPE * 100))
            else:
                self.prn.write_or_terminate(
                    'EVAPORATOR,     , N/A, N/A, %9.2f ' %
                    (self.ds.FSUPE * 100))
        else:
            if (self.ds.IFRSH != 0):
                self.prn.write_or_terminate(
                    'FRESH FOOD EVAP.,     %9.2f, N/A, %9.2f ' %
                    (self.dt.ETAE * 100, self.ds.FSUPE * 100))
            else:
                self.prn.write_or_terminate(
                    'FRESH FOOD EVAP.,  , N/A, N/A, %9.2f ' %
                    (self.ds.FSUPE * 100))

            if (self.dt.IFREZ != 0):
                self.prn.write_or_terminate(
                    'FREEZER EVAP.,   %9.2f, N/A,  ---- ' %
                    (self.dt.ETAF * 100))
            else:
                self.prn.write_or_terminate(' ')

        if (self.ds.ICOND != 0):
            self.prn.write_or_terminate(
                'CONDENSER,     %9.2f,%9.2f, %9.2f ' %
                (self.ds.ETAC, self.ds.FSUPC, self.ds.FSUPC))
        else:
            self.prn.write_or_terminate(
                'CONDENSER,     , N/A, %9.2f, %9.2f ' %
                (self.ds.FSUPC * 100, self.ds.FSUPC * 100))

        self.prn.write_or_terminate("COMPRESSOR PERFORMANCE SUMMARY")
        self.prn.write_or_terminate(
            'REFRIGERANT MASS FLOW RATE.,   %9.2f, KG/HR' %
            (self.ds.FLOW ))

        if (self.dt.IMAP != 0):
            self.prn.write_or_terminate(
                'VOLUMETRIC EFFICIENCY,   %9.2f' %
                (self.dt.ETAV ))

        self.prn.write_or_terminate(
            'PRESSURE RATIO,   %9.2f' %
            (self.ds.P[2] / self.ds.P[1]))

        if (self.dt.IMAP != 0):
            self.prn.write_or_terminate(
                'SUCTION PORT TEMPERATURE (C),   %9.2f' %
                (self.dt.TSUC - 273.11))
            self.prn.write_or_terminate(
                'DISCHARGE PORT TEMPERATURE (C),   %9.2f' %
                (self.dt.TDISC - 273.11))
            self.prn.write_or_terminate(
                'DISCHARGE SUPERHEAT (C),   %9.2f' %
                (self.dt.TSUPC - 273.11))

        self.prn.write_or_terminate(" ")
        #
        #    OUTPUT A FIGURE OF THE RESULTS
        #

        print ("\n\t\tLeaving cycle with IC:" + str(self.ds.IC) )
        print ("\t\tLeaving cycle with IE:" + str(self.ds.IE) )

    def show_overall(self):
        # HOUT = HOUT/WMAVG
        # VSUC = VSUC/WMAVG

        W = self.ds.W
        QE =self.ds.QE
        QC = self.ds.QC
        QZ = self.ds.QZ
        # COPR = (QE + QZ)/W

        TH = self.ds.TS1
        TL1 = self.ds.TS3
        TL2 = self.ds.TS5
        
        DENOM = TH * (QE * (1 / TL1 - 1 / TH) + QZ * (1 / TL2 - 1 / TH))
        COPI = (QE + QZ) / DENOM

        PR = self.ds.P[2]/self.ds.P[1]
        TSUPC = self.ds.T[2] - self.ds.T[3]

        self.prn.write_or_terminate("OUTPUT RESULTS")
        #    OUTPUT SUMMARY TABLE OF RESULTS

        self.prn.write_or_terminate  ('CYCLE PERFORMANCE SUMMARY')
        if (self.dt.ITYPE  ==  1) :
            self.prn.write_or_terminate  ('''EVAPORATOR CAPACITY,
            %9.3f, watt, %9.3f, W''' %(QE, QE) )

        else:
            self.prn.write_or_terminate  ('''FRESH FOOD EVAPORATOR CAPACITY,
                %9.3f, KJ/HR,  %9.3f, W''' %(QE, QE))

            self.prn.write_or_terminate  ('''FREEZER EVAPORATOR CAPACITY,
                %9.3f, KJ/HR,  %9.3f, W''' %(QZ, QZ))

        self.prn.write_or_terminate  ('''CONDENSER HEAT REJECTION RATE,
                %9.3f, KJ/HR,  %9.3f, W''' %( QC, QC) )

        self.prn.write_or_terminate  ('''COMPRESSOR POWER REQUIREMENT,
                %9.3f, KJ/HR,  %9.3f, W''' %(W, W))

        self.prn.write_or_terminate  ('''COEFFICIENT OF PERFORMANCE,
                %9.3f''' %(self.ds.COPR) )

        if (self.dt.IRFTYP <= 3 and self.dt.ICYCL == 1 and self.dt.ICAB != 0
                    and  self.ds.IFRSH != 0):
            self.prn.write_or_terminate ('''FRACTION AIR TO FRESH FOOD,
                %9.3f, (SINGLE EVAPORATOR CYCLE)''' %(self.dt.FF_FRACT))

        self.prn.write_or_terminate (" ")

        if (self.dt.IMAP  ==  1) :
            self.prn.write_or_terminate  ('''ESTIMATED COMPRESSION EFFICIENCY,
                %9.3f, (COMPRESSOR EER MODEL)''' %( self.dt.ETAS) )
            self.prn.write_or_terminate  ('''ESTIMATED MOTOR-PUMP EFFICIENCY,
                %9.3f, (COMPRESSOR EER MODEL)''' %( self.dt.EFFC/ self.dt.ETAS))
            self.prn.write_or_terminate  ('''ESTIMATED CLEARANCE VOLUME,
                %9.3f, (COMPRESSOR EER MODEL)''' %( self.dt.CE) )
            self.prn.write_or_terminate  ('''ESTIMATED SHELL LOSS,
                %9.3f, (COMPRESSOR EER MODEL)''' %( self.ds.QCAN) )
            self.prn.write_or_terminate  ('''ESTIMATED DISC TUBE HEAT LOSS,
                %9.3f, (COMPRESSOR EER MODEL)''' %( self.dt.QHILO))

        self.prn.write_or_terminate ("HEAT EXCHANGER PERFORMANCE SUMMARY")
        self.prn.write_or_terminate ('''EXCHANGER, EFFECTIVENESS,
            SUBCOOLED FRACTION, SUPERHEATED FRACTION''')

        if (self.dt.ITYPE  ==  1):
            if (self.ds.IFRSH  !=  0):
                self.prn.write_or_terminate('''EVAPORATOR,
                    %9.3f, N/A, ,%9.3f''' %(self.dt.ETAE, self.ds.self.ds.FSUPE ) )

            else:
                self.prn.write_or_terminate('''EVAPORATOR,     , N/A, N/A,
                    %9.3f''' %(self.ds.FSUPE ) )

        else:
            if (self.ds.IFRSH  !=  0) :
                self.prn.write_or_terminate('''FRESH FOOD EVAP.,
                    %9.3f, N/A, %9.3f ''' %(self.dt.ETAE,self.ds.FSUPE ) )

            else:
                self.prn.write_or_terminate('''FRESH FOOD EVAP.,  , N/A, N/A,
                    %9.3f''' %(self.ds.FSUPE ) )

            if (self.dt.IFREZ  !=  0) :
                self.prn.write_or_terminate('''FREEZER EVAP.,
                    %9.3f, N/A,  ---- ''' %( self.dt.ETAF ) )

            else:
                self.prn.write_or_terminate  (' ' )

        if (self.ds.ICOND  !=  0) :
            self.prn.write_or_terminate  ('''CONDENSER,
                %9.3f,%9.3f, %9.3f ''' %(self.dt.ETAC
                                       , self.ds.FSUBC
                                       , self.ds.FSUPC ) )
        else:
            self.prn.write_or_terminate  ('''CONDENSER,     , N/A,
                %9.3f,%9.3f ''' %(self.ds.FSUBC, self.ds.FSUPC ) )

        self.prn.write_or_terminate ("COMPRESSOR PERFORMANCE SUMMARY")
        self.prn.write_or_terminate ('''REFRIGERANT MASS FLOW RATE.,
            %9.3f, kg/hr''' %( self.ds.FLOW ) )

        if (self.dt.IMAP != 0):
            self.prn.write_or_terminate ('''VOLUMETRIC EFFICIENCY,
                %9.3f''' %(  self.dt.ETAV * 0.45359 ) )

        self.prn.write_or_terminate ('PRESSURE RATIO,   %9.3f' %( PR ) )

        if (self.dt.IMAP != 0) :
            self.prn.write_or_terminate ('''SUCTION PORT TEMPERATURE (C),
                %9.3f''' %( self.ds.TSUC ) )

            self.prn.write_or_terminate ('''DISCHARGE PORT TEMPERATURE (C),
                %9.3f''' %( self.ds.TDISC ) )
            self.prn.write_or_terminate ('''DISCHARGE SUPERHEAT (C),
                %9.3f''' %( TSUPC ) )

        self.prn.write_or_terminate (" ")

        return

