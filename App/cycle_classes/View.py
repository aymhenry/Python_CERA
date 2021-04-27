# Python Import
import datetime

# User Import
from common_classes.FileAccess import FileAccess

# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = .


class View:
    def __init__(self, dt, cab, objCycleSlover, str_file_cycle, str_path_cycle):
        self.str_file_cycle = str_file_cycle
        self.str_path_cycle = str_path_cycle

        self.dt = dt        # input data object
        self.cab = cab       
        # will add some other data to the given ds object in this class
        self.ds = objCycleSlover     # soultion in objCycleSlover

        #    OPEN OUTPUT FILE
        self.prn = FileAccess(self.str_file_cycle, "write", self.str_path_cycle)
        # self.prn.write_or_terminate(" ")

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    @staticmethod
    def showError(strMsg, fltValue=0.0, isOnlyTest=False):
        print("\n\n==========================Error detected =================")
        if isOnlyTest:
            print("    Error: ", strMsg)
        else:
            #  print('{},,{}'.format(strMsg, fltValue))
            print("    Error: ", strMsg + ",, %10.3f" % fltValue)
        print("==========================================================\n\n")

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    @staticmethod
    def print_width(arr_data):
        col_width = 10
        print("".join(str(word)[:8].ljust(col_width) for word in arr_data))

    def setBasicData(self):
        self.ds.AIRTMP = [0.0] * (16 + 1)
        self.ds.AIRTMP = [True,  # add new item to start list at 1    \
                          False, False, True, False, False,
                          True, False, False, False, False,
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

        self.ds.AIRTMP[10] = False
        self.ds.AIRTMP[11] = False

        # #    OPEN OUTPUT FILE
        # objCycOut = FileAccess(self.str_file_cycle, "write")  # IO_Cycle Tag
        self.prn.write_or_terminate(" ")

        now = datetime.datetime.now()
        if self.ds.NCYC != 2:
            self.prn.write_or_terminate(
                (now.strftime("%H %M %S %d %b %Y")) +
                " - Python Output aymhenry@gmail")

        #    OUTPUT INFORMATION ON TYPE OF CYCLE
        if self.dt.ICYCLS == 1:
            self.prn.write_or_terminate('STANDARD ONE EVAPORATOR CYCLE')
        else:
            if self.dt.ITYPE == 1 or self.dt.ITYPE == 4:
                self.prn.write_or_terminate(
                    'DUAL EVAP CYCLE: FRESH FOOD LOOP')
            else:
                self.prn.write_or_terminate(
                    'DUAL EVAP CYCLE: FREEZER LOOP')

        self.prn.write_or_terminate(" ")
        #    OUTPUT REFRIGERATION MIXTURE INFORMATION
        self.prn.write_or_terminate("OUTPUT RESULTS")

        # print Error message if occured in Cycle, to be improved later
        # str_err need to be created in Cycle Solver
        # if self.ds.str_err != "":
        #     # self.prn.write_or_terminate(self.ds.str_err)

        # -----------------------------
        #
        #    OUTPUT WARNING IF CONVERGENCE FORCED BY HOT KEY <F10>.
        #
        if self.ds.LQUIT:
            self.prn.write_or_terminate(
                "Convergence forced after, iterations, %3.0f," % self.dt.IC)

        #
        #    PRINT ERROR MESSAGES if NON-CONVERGENCE
        #
        if self.ds.LECON or self.ds.LCCON or self.ds.I_ERROR_INTER > 0:
            # in Fortran, no use in Python
            # LWIND = 5
            # if (self.ds.LECON and self.ds.LCCON): LWIND = 7

            if self.ds.LECON:
                self.prn.write_or_terminate(
                    'EVAPORATOR ITERATION DID NOT CONVERGE,  %9.2f, %9.2f ' %
                    (self.ds.TE[1] - 273.11, self.ds.TE[2] - 273.11))

                # self.showError("EVAPORATOR ITERATION DID NOT CONVERGE", "")

            if self.ds.LCCON:
                self.prn.write_or_terminate(
                    'CONDENSER ITERATION DID NOT CONVERGE,  %9.2f, %9.2f ' %
                    (self.ds.TC[1] - 273.11, self.ds.TC[2] - 273.11))
                self.showError(
                    'CONDENSER ITERATION DID NOT CONVERGE ,  %9.2f, %9.2f ' %
                    (self.ds.TC[1] - 273.11, self.ds.TC[2] - 273.11))

            if self.ds.I_ERROR_INTER > 0:
                self.prn.write_or_terminate(
                    'INTERCHANGER SUPERHEAT NOT POSSIBLE')
                self.showError("INTERCHANGER SUPERHEAT NOT POSSIBLE")

            input("Press Enter to continue...")

        #    OUTPUT RESULTS.
        TENV = self.cab.TROOM

        if self.ds.T[16] < TENV:
            self.dt.I_LIQUID_LINE = 1

        self.prn.write_or_terminate('  , State, T(C), T(C), P, H, V, S')
        self.prn.write_or_terminate(' , , AIR,  REF, kPa, kj/kg, m3/kg, kj/kg-C')

        self.print_width(['#', 'Point', 'State',
                          'T(C)', 'T(C)',
                          'P', 'H', 'V', 'S'
                          ])

        self.print_width(['', '', '', 'AIR', 'REF',
                          'kPa', 'kj/kg', 'm3/kg', 'kj/kg-C'
                          ])

        K = 1
        M = 0
        K = 0
        while M <= 14:
            M += 1
            J = self.ds.LPNT[M]

            if 9 <= M <= 11:
                continue
            K += 1
            if self.ds.AIRTMP[M]:

                self.prn.write_or_terminate(
                    "%d, %s, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f"
                    % (K,
                       self.ds.MSTATE[K],
                       self.ds.TS[J] - 273.11,
                       self.ds.T[J] - 273.11,
                       self.ds.P[J] / 1000,
                       self.ds.H[J] / 1000,
                       self.ds.V[J],
                       self.ds.S[J] / 1000
                       ))

                self.print_width([K, J,
                                  self.ds.MSTATE[K],
                                  self.ds.TS[J] - 273.11,
                                  self.ds.T[J] - 273.11,
                                  self.ds.P[J] / 1000,
                                  self.ds.H[J] / 1000,
                                  self.ds.V[J],
                                  self.ds.S[J] / 1000
                                  ])
            else:

                self.prn.write_or_terminate(
                     "%d, %s , N/A, %9.2f, %9.2f, %9.2f, %9.2f, %9.2f"
                     % (K,
                        self.ds.MSTATE[K],
                        self.ds.T[J] - 273.11,
                        self.ds.P[J] / 1000,
                        self.ds.H[J] / 1000,
                        self.ds.V[J],
                        self.ds.S[J] / 1000
                        )
                                            )

                self.print_width([K,
                                  J,
                                  self.ds.MSTATE[K],
                                  'N/A',
                                  self.ds.T[J] - 273.11,
                                  self.ds.P[J] / 1000,
                                  self.ds.H[J] / 1000,
                                  self.ds.V[J],
                                  self.ds.S[J] / 1000
                                  ])

        # ================================
        #    NORMALIZE BY THE MASS FLOW

        self.ds.W = self.ds.W * self.ds.MREF / 3600     # j/kg . kg/hr/3600 =watt
        self.ds.QZ = self.ds.QZ * self.ds.MREF / 3600   # watt
        self.ds.QE = self.ds.QE * self.ds.MREF / 3600   # watt
        self.ds.QC = self.ds.QC * self.ds.MREF / 3600   # watt
        
        self.ds.COPR = (self.ds.QE + self.ds.QZ)/self.ds.W
        #
        #    REST OF THE CONVERSIONS
        # --------------------------------------------------------
        #    OUTPUT SUMMARY TABLE OF RESULTS
        
        self.prn.write_or_terminate(' ')
        self.prn.write_or_terminate('Cycle Performance Summary')

        self.prn.write_or_terminate(
            'Evaporator capacity,  %9.2f, watt'
            % self.ds.QE)

        self.prn.write_or_terminate(
            'Condenser heat rejection rate,  %9.2f, watt' 
            % self.ds.QC)

        self.prn.write_or_terminate(
            'Compressor power requirement, %9.2f, watt' 
            % self.ds.W)

        self.prn.write_or_terminate(
            'Coefficient of performance, %9.2f'
            % self.ds.COPR)

        if self.cab.IRFTYP <= 3 and self.dt.ICAB != 0 and self.ds.IFRSH != 0:
            self.prn.write_or_terminate(
                'Fraction air to fresh food, %9.2f, (single evaporator cycle)' 
                % self.dt.FF_FRACT)

        # ----------------------------------------------
        self.prn.write_or_terminate(" ")
        self.prn.write_or_terminate("Heat Exchanger Performance Summary")
        self.prn.write_or_terminate(
            'Exchanger, Effectiveness, Subcooled fraction, Superheated fraction')

        if self.ds.IFRSH != 0:
            self.prn.write_or_terminate(
                'Evaporator, %9.2f, N/A, ,%9.2f' 
                % (self.ds.ETAE * 100, self.ds.FSUPE * 100))
        else:
            self.prn.write_or_terminate(
                'Evaporator,     , N/A, N/A, %9.2f ' 
                % (self.ds.FSUPE * 100))

        if self.ds.ICOND != 0:
            self.prn.write_or_terminate(
                'Condenser, %9.2f,%9.2f, %9.2f' 
                % (self.ds.ETAC, self.ds.FSUPC, self.ds.FSUPC))
        else:
            self.prn.write_or_terminate(
                'Condenser,     , N/A, %9.2f, %9.2f '
                % (self.ds.FSUPC * 100, self.ds.FSUPC * 100))

        # ----------------------------------------------------------
        self.prn.write_or_terminate(" ")
        self.prn.write_or_terminate("Compressor Performance Summary")
        self.prn.write_or_terminate(
            'Refrigerant mass flow rate, %9.2f, kg/hr' 
            % self.ds.FLOW)

        self.prn.write_or_terminate(
            'Pressure ratio, %9.2f' 
            % (self.ds.P[2] / self.ds.P[1]))

        self.prn.write_or_terminate(" ")
        #
        #    OUTPUT A FIGURE OF THE RESULTS
        #

        print("\n\t\tLeaving cycle with IC:" + str(self.ds.IC))
        print("\t\tLeaving cycle with IE:" + str(self.ds.IE))

    def show_overall(self):
        W = self.ds.W
        QE = self.ds.QE
        QC = self.ds.QC
        QZ = self.ds.QZ

        TH = self.ds.TS1
        TL1 = self.ds.TS3
        TL2 = self.ds.TS5

        PR = self.ds.P[2]/self.ds.P[1]
        TSUPC = self.ds.T[2] - self.ds.T[3]

        # ----------------------------------------------------------
        self.prn.write_or_terminate("OUTPUT RESULTS")
        #    OUTPUT SUMMARY TABLE OF RESULTS

        self.prn.write_or_terminate(' ')
        self.prn.write_or_terminate('Cycle Performance Summary')

        self.prn.write_or_terminate(
            'Evaporator capacity,%9.3f, watt'
            % QE)

        self.prn.write_or_terminate(
            'Condenser heat rejection rate, %9.3f, watt'
            % QC)

        self.prn.write_or_terminate(
            'Compressor power requirement, %9.3f,watt'
            % W)

        self.prn.write_or_terminate(
            'Coefficient of performance, %9.3f' 
            % self.ds.COPR)

        if (self.cab.IRFTYP <= 3 and
                self.dt.ICAB != 0 and
                self.ds.IFRSH != 0):
                
            self.prn.write_or_terminate(
                'Fraction air to fresh food, %9.3f, (single evaporator cycle)'
                % self.dt.FF_FRACT)

        self.prn.write_or_terminate(" ")

        self.prn.write_or_terminate("Heat Exchanger Performance Summary")
        self.prn.write_or_terminate(
            'Exchanger, Effectiveness, Subcooled fraction, Superheated fraction')

        # ----------------------------------------------------------

        if self.ds.IFRSH != 0:
            self.prn.write_or_terminate(
                'Evaporator, %9.3f, N/A, ,%9.3f'
                % (self.dt.ETAE, self.ds.self.ds.FSUPE))

        else:
            self.prn.write_or_terminate(
                'Evaporator, , N/A, N/A, %9.3f'
                % self.ds.FSUPE)

        if self.ds.ICOND != 0:
            self.prn.write_or_terminate(
                'Condenser, %9.3f, %9.3f, %9.3f '
                % (self.ds.ETAC, self.ds.FSUBC, self.ds.FSUPC))
        else:
            self.prn.write_or_terminate(
                'Condenser,  , N/A, %9.3f,%9.3f' 
                % (self.ds.FSUBC, self.ds.FSUPC))

        # ----------------------------------------------------------
        self.prn.write_or_terminate("Compressor Performance Summary")
        
        self.prn.write_or_terminate(
            'Refrigerant mass flow rate, %9.3f, kg/hr' 
            % self.ds.FLOW)

        self.prn.write_or_terminate('Pressure ratio, %9.3f' % PR)
        return

