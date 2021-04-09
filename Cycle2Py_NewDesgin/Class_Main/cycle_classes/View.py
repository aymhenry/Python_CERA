# Python Import
import datetime

# User Import
from common_classes.FileAccess import FileAccess


# -----------------------------------------------------------
# Job 			: Creates Data Model for Q files
#
# Editor		: aymhenry@gmail.com
# -----------------------------------------------------------

class View:
    def __init__(self, obj_comm, str_file_cycle, str_path_cycle):
        self.str_file_cycle = str_file_cycle
        self.str_path_cycle = str_path_cycle

        self.Data = obj_comm

    def show_rep(self):
        print("Show all results in View Class ... later ...")

# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = .


class ViewCycle:
    def __init__(self, obj_comm, obj_para, str_file_cycle, str_path_cycle):
        self.str_file_cycle = str_file_cycle
        self.str_path_cycle = str_path_cycle

        self.Data = obj_comm
        self.obj_parameter = obj_para
        self.setBasicData()

    def setBasicData(self):
        self.obj_parameter.AIRTMP = [0.0] * (16 + 1)
        self.obj_parameter.AIRTMP = [True,  # add new item to start list at 1	\
                                     False, False, True, False, False,	\
                                     True, False, False, False, False,	\
                                     False, True, False, True, False]

        # in python add extra item for item 0
        self.obj_parameter.HSTATE = [
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
        self.obj_parameter.MSTATE = [
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
        self.obj_parameter.LPNT = [0, 1, 2, 14, 3,
                                   11, 4, 16, 6, 10, 8, 9, 5, 12, 7, 13]

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def showError(self, strMsg, fltValue=0.0, isOnlyTest=False):
        print("\n\n==========================Error detected ============================")
        if isOnlyTest:
            print("	Error : ", strMsg)
        else:
            #print('{},,{}'.format(strMsg, fltValue))
            print("	Error : ", strMsg + ",, %10.3f" % (fltValue))
        print("=====================================================================\n\n")

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def print_width(self, arr_data):
        col_width = 10
        print("".join(str(word)[:8].ljust(col_width) for word in arr_data))

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def show_rep(self):
        print("Show cycle result in View Class ... later ...")

        #	SET UP LOGICAL VECTOR ON AIR TEMPERATURES
        if (self.obj_parameter.ICYCL == 2):
            self.obj_parameter.AIRTMP[10] = True
            self.obj_parameter.AIRTMP[11] = True
        else:
            self.obj_parameter.AIRTMP[10] = False
            self.obj_parameter.AIRTMP[11] = False

        #	OPEN OUTPUT FILE
        objCycOut = FileAccess(self.str_file_cycle, "write")  # IO_Cycle Tag
        objCycOut.write_or_terminate(" ")

        now = datetime.datetime.now()
        if (self.obj_parameter.NCYC != 2):
            objCycOut.write_or_terminate(
                (now.strftime("%H %M %S %d %b %Y")) +
                " - Python Output aymhenry@gmail")

        #	OUTPUT INFORMATION ON TYPE OF CYCLE
        if (self.obj_parameter.ICYCL == 1):
            if (self.obj_parameter.ICYCLS == 1):
                objCycOut.write_or_terminate('STANDARD ONE EVAPORATOR CYCLE')
            else:
                if (self.Data.ITYPE == 1 or self.Data.ITYPE == 4):
                    objCycOut.write_or_terminate(
                        'DUAL EVAP CYCLE: FRESH FOOD LOOP')
                else:
                    objCycOut.write_or_terminate(
                        'DUAL EVAP CYCLE: FREEZER LOOP')

        if (self.obj_parameter.ICYCL == 2):
            if (self.obj_parameter.ICYCLS == 2):
                objCycOut.write_or_terminate('LORENZ CYCLE')
            else:
                objCycOut.write_or_terminate('DUAL EVAP CYCLE')

        if (self.obj_parameter.ICYCL == 3):
            if (self.obj_parameter.NCYC == 1):
                objCycOut.write_or_terminate('DUAL LOOP CYCLE - FREEZER')
            else:
                objCycOut.write_or_terminate(" ")
                objCycOut.write_or_terminate('DUAL LOOP CYCLE - FRESH FOOD')

        objCycOut.write_or_terminate(" ")
        #	OUTPUT REFRIGERATION MIXTURE INFORMATION

        ''' later ==============
		X2 = 100.0 * self.obj_parameter.XM[1]
		objCycOut.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(X2, self.Data.HREF[ self.obj_parameter.IR[1] ])  )

		if (self.obj_parameter.NC >  1) :
			for I in range (2, self.obj_parameter.NC+1) :
				X2 = 100.0 * self.obj_parameter.XM[I]
				objCycOut.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(X2, self.Data.HREF[ self.obj_parameter.IR[1] ])  ) # fixed in python
		objCycOut.write_or_terminate(" ")
		'''

        objCycOut.write_or_terminate("OUTPUT RESULTS")

        if self.obj_parameter.str_err != "":
            objCycOut.write_or_terminate(self.obj_parameter.str_err)

        # -----------------------------
        #
        #	OUTPUT WARNING IF CONVERGENCE FORCED BY HOT KEY <F10>.
        #
        if (self.obj_parameter.LQUIT):
            objCycOut.write_or_terminate(
                "Convergence forced after, iterations ", self.Data.IC)

        #
        #	PRINT ERROR MESSAGES if NON-CONVERGENCE
        #
        if (self.obj_parameter.LECON or self.obj_parameter.LCCON or self.obj_parameter.I_ERROR_INTER > 0):
            LWIND = 5
            if (self.obj_parameter.LECON and self.obj_parameter.LCCON):
                LWIND = 7

            if (self.obj_parameter.LECON):

                objCycOut.write_or_terminate(
                    'EVAPORATOR ITERATION DID NOT CONVERGE,  %9.3f, %9.3f ' %
                    (self.obj_parameter.TE[1] - 273.11, self.obj_parameter.TE[2] - 273.11))
                self.showError("EVAPORATOR ITERATION DID NOT CONVERGE", "")

            if (self.obj_parameter.LCCON):
                objCycOut.write_or_terminate(
                    'CONDENSER ITERATION DID NOT CONVERGE,  %9.3f, %9.3f ' %
                    (self.obj_parameter.TC[1] - 273.11, self.obj_parameter.TC[2] - 273.11))
                self.showError(
                    'CONDENSER ITERATION DID NOT CONVERGE ,  %9.3f, %9.3f ' %
                    (self.obj_parameter.TC[1] - 273.11, self.obj_parameter.TC[2] - 273.11))

            if (self.obj_parameter.I_ERROR_INTER > 0):
                objCycOut.write_or_terminate(
                    'INTERCHANGER SUPERHEAT NOT POSSIBLE')
                self.showError("INTERCHANGER SUPERHEAT NOT POSSIBLE")

            input("Press Enter to continue...")

        #
        #	OUTPUT RESULTS.  BEGIN BY CONVERTING TO ENGLISH UNITS.
        #

        TENV = (self.Data.TROOM + 459.6) / 1.8

        if (self.obj_parameter.T[16] < TENV):
            self.Data.I_LIQUID_LINE = 1

        objCycOut.write_or_terminate(
            ",STATE, T(C), T(C), P, H, V, S, XL, XV, XQ")
        objCycOut.write_or_terminate(
            ",,AIR,   REF, (KPa), (KJ/KG), (M3/KG), (KJ/KG-C), (MASS FRAC),(MASS FRAC),(MASS FRAC)")

        #print (",STATE, T(C), T(C), P, H, V, S, XL, XV, XQ")
        self.print_width(['#', 'Point', 'STATE', 'T(C)', 'T(C)',
                          'P', 'H', 'V', 'S', 'XL', 'XV', 'XQ'])
        #print (",,AIR,   REF, (KPA), (KJ/KG), (M3/KG), (KJ/KG-C), (MASS FRAC),(MASS FRAC),(MASS FRAC)")
        self.print_width(['',
                          '',
                          '',
                          'AIR',
                          'REF',
                          'KPA',
                          'KJ/KG',
                          'M3/KG',
                          'kJ/kg-C',
                          'MASS-FRAC',
                          'MASS-FRAC',
                          'MASS-FRAC'])

        K = 1
        if (self.obj_parameter.ICYCL == 2):
            while (K <= 15):
                J = self.obj_parameter.LPNT[K]
                self.obj_parameter.TS[J] = self.obj_parameter.TS[J] - 273.11
                self.obj_parameter.T[J] = self.obj_parameter.T[J] - 273.11
                self.obj_parameter.V[J] = self.obj_parameter.V[J] / 10.0

                if (self.obj_parameter.XQ[J] > 1.0):
                    self.obj_parameter.XQ[J] = 1.0
                if (self.obj_parameter.XQ[J] < 0.0):
                    self.obj_parameter.XQ[J] = 0.0

                if (self.obj_parameter.AIRTMP[K]):

                    objCycOut.write_or_terminate(
                        "%d, %s , %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s" %
                        (K,
                         self.obj_parameter.HSTATE[K],
                         self.obj_parameter.TS[J],
                         self.obj_parameter.T[J],
                         self.obj_parameter.P[J],
                         self.obj_parameter.H[J],
                         self.obj_parameter.V[J],
                         self.obj_parameter.S[J],
                         self.obj_parameter.XL[1][J],
                         self.obj_parameter.XV[1][J],
                         self.obj_parameter.XQ[J]))

                    # print ( "%d,%d, %s , %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
                    #	%(K,J,self.obj_parameter.HSTATE[K],self.obj_parameter.TS[J],self.obj_parameter.T[J],self.obj_parameter.P[J],self.obj_parameter.H[J],self.obj_parameter.V[J],  self.obj_parameter.S[J],self.obj_parameter.XL[1][J],self.obj_parameter.XV[1][J],self.obj_parameter.XQ[J]) )

                    self.print_width([K,
                                      J,
                                      self.obj_parameter.HSTATE[K],
                                      self.obj_parameter.TS[J],
                                      self.obj_parameter.T[J],
                                      self.obj_parameter.P[J],
                                      self.obj_parameter.H[J],
                                      self.obj_parameter.V[J],
                                      self.obj_parameter.S[J],
                                      self.obj_parameter.XL[1][J],
                                      self.obj_parameter.XV[1][J],
                                      self.obj_parameter.XQ[J]])
                else:

                    objCycOut.write_or_terminate(
                        " %d, %s,  N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s" %
                        (K,
                         self.obj_parameter.HSTATE[K],
                         self.obj_parameter.T[J],
                         self.obj_parameter.P[J],
                         self.obj_parameter.H[J],
                         self.obj_parameter.V[J],
                         self.obj_parameter.S[J],
                         self.obj_parameter.XL[1][J],
                         self.obj_parameter.XV[1][J],
                         self.obj_parameter.XQ[J]))
                    # print ( "%d,%d, %s , N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
                    #	%(K,J,self.obj_parameter.HSTATE[K], self.obj_parameter.T[J],self.obj_parameter.P[J],self.obj_parameter.H[J],self.obj_parameter.V[J],  self.obj_parameter.S[J],self.obj_parameter.XL[1][J],self.obj_parameter.XV[1][J],self.obj_parameter.XQ[J]) )

                    self.print_width([K,
                                      J,
                                      self.obj_parameter.HSTATE[K],
                                      'N/A',
                                      self.obj_parameter.T[J],
                                      self.obj_parameter.P[J],
                                      self.obj_parameter.H[J],
                                      self.obj_parameter.V[J],
                                      self.obj_parameter.S[J],
                                      self.obj_parameter.XL[1][J],
                                      self.obj_parameter.XV[1][J],
                                      self.obj_parameter.XQ[J]])

                K = K + 1

        else:
            M = 0
            K = 0
            while (M <= 14):
                M = M + 1
                J = self.obj_parameter.LPNT[M]

                self.obj_parameter.TS[J] = self.obj_parameter.TS[J] - 273.11
                self.obj_parameter.T[J] = self.obj_parameter.T[J] - 273.11
                self.obj_parameter.V[J] = self.obj_parameter.V[J] / 10.0
                if (self.obj_parameter.XQ[J] > 1.0):
                    self.obj_parameter.XQ[J] = 1.0
                if (self.obj_parameter.XQ[J] < 0.0):
                    self.obj_parameter.XQ[J] = 0.0

                if (M >= 9 and M <= 11):
                    continue
                K = K + 1
                if (self.obj_parameter.AIRTMP[M]):

                    objCycOut.write_or_terminate(
                        "%d, %s, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s" %
                        (K,
                         self.obj_parameter.MSTATE[K],
                         self.obj_parameter.TS[J],
                         self.obj_parameter.T[J],
                         self.obj_parameter.P[J],
                         self.obj_parameter.H[J],
                         self.obj_parameter.V[J],
                         self.obj_parameter.S[J],
                         self.obj_parameter.XL[1][J],
                         self.obj_parameter.XV[1][J],
                         self.obj_parameter.XQ[J]))

                    # print ("%d,%d, %s , %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
                    #	%(K,J, self.obj_parameter.MSTATE[K], self.obj_parameter.TS[J],self.obj_parameter.T[J],self.obj_parameter.P[J],self.obj_parameter.H[J],self.obj_parameter.V[J],  self.obj_parameter.S[J],self.obj_parameter.XL[1][J],self.obj_parameter.XV[1][J],self.obj_parameter.XQ[J]) )
                    self.print_width([K,
                                      J,
                                      self.obj_parameter.MSTATE[K],
                                      self.obj_parameter.TS[J],
                                      self.obj_parameter.T[J],
                                      self.obj_parameter.P[J],
                                      self.obj_parameter.H[J],
                                      self.obj_parameter.V[J],
                                      self.obj_parameter.S[J],
                                      self.obj_parameter.XL[1][J],
                                      self.obj_parameter.XV[1][J],
                                      self.obj_parameter.XQ[J]])
                else:

                    objCycOut.write_or_terminate(
                        "%d, %s , N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s" %
                        (K,
                         self.obj_parameter.MSTATE[K],
                         self.obj_parameter.T[J],
                         self.obj_parameter.P[J],
                         self.obj_parameter.H[J],
                         self.obj_parameter.V[J],
                         self.obj_parameter.S[J],
                         self.obj_parameter.XL[1][J],
                         self.obj_parameter.XV[1][J],
                         self.obj_parameter.XQ[J]))
                    # print ("%d,%d, %s , N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
                    #	%(K,J, self.obj_parameter.MSTATE[K], self.obj_parameter.T[J],self.obj_parameter.P[J],self.obj_parameter.H[J],self.obj_parameter.V[J],  self.obj_parameter.S[J],self.obj_parameter.XL[1][J],self.obj_parameter.XV[1][J],self.obj_parameter.XQ[J]) )

                    self.print_width([K,
                                      J,
                                      self.obj_parameter.MSTATE[K],
                                      'N/A',
                                      self.obj_parameter.T[J],
                                      self.obj_parameter.P[J],
                                      self.obj_parameter.H[J],
                                      self.obj_parameter.V[J],
                                      self.obj_parameter.S[J],
                                      self.obj_parameter.XL[1][J],
                                      self.obj_parameter.XV[1][J],
                                      self.obj_parameter.XQ[J]])

        #
        #	NORMALIZE BY THE MASS FLOW
        #

        self.Data.FLOW = self.Data.FLOW * self.Data.MREF / self.obj_parameter.MREFSV
        self.Data.DISP = self.Data.DISP / 1.6387E-05
        self.Data.W = 0.4302 * self.Data.W * self.Data.FLOW * 1.0548
        self.Data.QZ = 0.4302 * self.Data.QZ * self.Data.FLOW * 1.0548
        self.Data.QE = 0.4302 * self.Data.QE * self.Data.FLOW * 1.0548
        self.obj_parameter.QC = 0.4302 * self.obj_parameter.QC * self.Data.FLOW * 1.0548
        #
        #	REST OF THE CONVERSIONS
        #
        self.obj_parameter.TSUC = self.obj_parameter.TSUC - 273.11
        self.obj_parameter.TDISC = self.obj_parameter.TDISC - 273.11
        self.Data.ETAE = 100. * self.Data.ETAE
        self.obj_parameter.FSUPE = 100. * self.obj_parameter.FSUPE

        self.Data.ETAF = 100. * self.Data.ETAF
        self.Data.ETAC = 100. * self.Data.ETAC

        self.obj_parameter.FSUBC = 100. * self.obj_parameter.FSUBC
        self.obj_parameter.FSUPC = 100. * self.obj_parameter.FSUPC
        #
        #	OUTPUT SUMMARY TABLE OF RESULTS
        #

        objCycOut.write_or_terminate('CYCLE PERFORMANCE SUMMARY')
        if (self.Data.ITYPE == 1):
            objCycOut.write_or_terminate(
                'EVAPORATOR CAPACITY,  %9.3f, KJ/HR,  %9.3f, W' %
                (self.Data.QE, self.Data.QE / 3.6))
        else:
            objCycOut.write_or_terminate(
                'FRESH FOOD EVAPORATOR CAPACITY,  %9.3f, KJ/HR,  %9.3f, W' %
                (self.Data.QE, self.Data.QE / 3.6))
            objCycOut.write_or_terminate(
                'FREEZER EVAPORATOR CAPACITY,     %9.3f, KJ/HR,  %9.3f, W' %
                (self.Data.QZ, self.Data.QZ / 3.6))

        objCycOut.write_or_terminate(
            'CONDENSER HEAT REJECTION RATE,  %9.3f, KJ/HR,  %9.3f, W' %
            (self.obj_parameter.QC, self.obj_parameter.QC / 3.6))
        objCycOut.write_or_terminate(
            'COMPRESSOR POWER REQUIREMENT,     %9.3f, KJ/HR,  %9.3f, W' %
            (self.Data.W, self.Data.W / 3.6))
        objCycOut.write_or_terminate(
            'COEFFICIENT OF PERFORMANCE,     %9.3f' %
            (self.Data.COPR))

        if (self.obj_parameter.IRFTYP <= 3 and self.obj_parameter.ICYCL ==
                1 and self.obj_parameter.ICAB != 0 and self.Data.IFRSH != 0):
            objCycOut.write_or_terminate(
                'FRACTION AIR TO FRESH FOOD,     %9.3f, (SINGLE EVAPORATOR CYCLE)' %
                (self.obj_parameter.FF_FRACT))

        objCycOut.write_or_terminate(" ")

        if (self.Data.IMAP == 1):
            objCycOut.write_or_terminate(
                'ESTIMATED COMPRESSION EFFICIENCY, %9.3f, (COMPRESSOR EER MODEL)' %
                (self.obj_parameter.ETAS))
            objCycOut.write_or_terminate(
                'ESTIMATED MOTOR-PUMP EFFICIENCY,  %9.3f, (COMPRESSOR EER MODEL)' %
                (self.Data.EFFC / self.obj_parameter.ETAS))
            objCycOut.write_or_terminate(
                'ESTIMATED CLEARANCE VOLUME,       %9.3f, (COMPRESSOR EER MODEL)' %
                (self.Data.CE))
            objCycOut.write_or_terminate(
                'ESTIMATED SHELL LOSS,       %9.3f, (COMPRESSOR EER MODEL)' %
                (self.obj_parameter.QCAN))
            objCycOut.write_or_terminate(
                'ESTIMATED DISC TUBE HEAT LOSS,       %9.3f, (COMPRESSOR EER MODEL)' %
                (self.obj_parameter.QHILO))

        objCycOut.write_or_terminate("HEAT EXCHANGER PERFORMANCE SUMMARY")
        objCycOut.write_or_terminate(
            "EXCHANGER, EFFECTIVENESS, SUBCOOLED FRACTION, SUPERHEATED FRACTION")

        if (self.Data.ITYPE == 1):
            if (self.Data.IFRSH != 0):
                objCycOut.write_or_terminate(
                    'EVAPORATOR,     %9.3f, N/A, ,%9.3f' %
                    (self.Data.ETAE, self.obj_parameter.FSUPE))
            else:
                objCycOut.write_or_terminate(
                    'EVAPORATOR,     , N/A, N/A, %9.3f ' %
                    (self.obj_parameter.FSUPE))
        else:
            if (self.Data.IFRSH != 0):
                objCycOut.write_or_terminate(
                    'FRESH FOOD EVAP.,     %9.3f, N/A, %9.3f ' %
                    (self.Data.ETAE, self.obj_parameter.FSUPE))
            else:
                objCycOut.write_or_terminate(
                    'FRESH FOOD EVAP.,  , N/A, N/A, %9.3f ' %
                    (self.obj_parameter.FSUPE))

            if (self.Data.IFREZ != 0):
                objCycOut.write_or_terminate(
                    'FREEZER EVAP.,   %9.3f, N/A,  ---- ' %
                    (self.Data.ETAF))
            else:
                objCycOut.write_or_terminate(' ')

        if (self.Data.ICOND != 0):
            objCycOut.write_or_terminate(
                'CONDENSER,     %9.3f,%9.3f, %9.3f ' %
                (self.Data.ETAC, self.obj_parameter.FSUBC, self.obj_parameter.FSUPC))
        else:
            objCycOut.write_or_terminate(
                'CONDENSER,     , N/A, %9.3f,%9.3f ' %
                (self.obj_parameter.FSUBC, self.obj_parameter.FSUPC))

        objCycOut.write_or_terminate("COMPRESSOR PERFORMANCE SUMMARY")
        objCycOut.write_or_terminate(
            'REFRIGERANT MASS FLOW RATE.,   %9.3f, KG/HR' %
            (self.Data.FLOW * 0.45359))

        if (self.Data.IMAP != 0):
            objCycOut.write_or_terminate(
                'VOLUMETRIC EFFICIENCY,   %9.3f' %
                (self.Data.ETAV * 0.45359))

        objCycOut.write_or_terminate(
            'PRESSURE RATIO,   %9.3f' %
            (self.obj_parameter.PR))

        if (self.Data.IMAP != 0):
            objCycOut.write_or_terminate(
                'SUCTION PORT TEMPERATURE (C),   %9.3f' %
                (self.obj_parameter.TSUC))
            objCycOut.write_or_terminate(
                'DISCHARGE PORT TEMPERATURE (C),   %9.3f' %
                (self.obj_parameter.TDISC))
            objCycOut.write_or_terminate(
                'DISCHARGE SUPERHEAT (C),   %9.3f' %
                (self.obj_parameter.TSUPC))

        objCycOut.write_or_terminate(" ")
        #
        #	OUTPUT A FIGURE OF THE RESULTS
        #
        #self.showError (" Check cycle fig number ...")
        print("LEAVING CYCLE WITH IC:" + str(self.Data.IC) + str(self.Data.IE))
