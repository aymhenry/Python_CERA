# Python Import ====================
import math
from abc import ABC,abstractmethod

# User Import ======================

from common_classes.QData import QData
from common_classes.Unit import Unit
from cycle_classes.Cycle import *
from cycle_classes.CycleUtil import *

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Control class
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class CycleType_Abstract (ABC, CycleUtil):

	def __init__ (self, objdata):
		self.obj_data = objdata
		
		self.setup_vars_for_all_types()
		self.setup_vars_extra ()
		
	# Abstract methods
	#-----------------------------------------------------------
	# Job 			: Change units, from the give SI units. (individual for every sub-class)
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def adjust_units (self):
		pass
		

	#-----------------------------------------------------------
	# Job 			: inialize extra varibale in obj_data object with value (individual for every sub-class)
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def setup_vars_extra (self):
		pass
		

	#-----------------------------------------------------------
	# Job 			: Calculate cycle data, (individual for every sub-class)
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def calculte (self):
		pass
	
	#-----------------------------------------------------------
	# Job 			: Adjsut input data 
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def adjust_input (self):
		pass
	#-----------------------------------------------------------
	# Job 			: Adjsut input data for all types
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def adjust_input_for_all_types (self):
		QZ_NET = 0.0
		
		# set Temperatue 17.11 C
		self.obj_data.TS5 = 256.0
		
		# initialize error code for liquid line anti-sweat heat
		self.obj_data.I_LIQUID_LINE = 0
		
		# get compressor file name
		self.obj_data.FILMAP1 = self.getCompressorFileName (self.obj_data.FILMAP1_CODE)
		
		# Binary interaction parameter (BIP) 
		for int_ref in range (1, 2+1):
			self.obj_data.F[2][1][int_ref] = self.obj_data.F[1][2][int_ref]
			self.obj_data.F[3][1][int_ref] = self.obj_data.F[1][3][int_ref]
			self.obj_data.F[3][2][int_ref] = self.obj_data.F[2][3][int_ref]
			
		# Delat Temp Refrigerant Exit Superheat (C) Or Quality (0-1)
		self.obj_data.QUALTY = self.obj_data.DTSPEI [:]
		
		#	if IDFRST(Manual Defrost) =1  (i.e Autoamtic ) DFSTCYC :Closed-Door Automatic Defrost (W)
		if(self.obj_data.IDFRST == 1): self.obj_data.DFSTCYC = 0.0
		
		#  zero condenser heat loads to cabinet and evaporators
		#
		self.obj_data.Q_CND_FF = 0.0
		self.obj_data.Q_CND_FZ = 0.0
		self.obj_data.Q_HXS_FF = 0.0
		self.obj_data.Q_HXS_FZ = 0.0
		self.obj_data.CONDF_IN_WALL = 0.0
		self.obj_data.CONDZ_IN_WALL = 0.0
		self.obj_data.FF_AIR = 0

		self.obj_data.DUTYC = 0.5
		self.obj_data.IRFTPL = self.obj_data.IRFTYP
		self.obj_data.IFAN = 0
		
		self.obj_data.ICYCLS = self.obj_data.ICYCL	# Python comment: save original ICYCL value
		
		#Python comment: if 4- chest freezer, change it to 2- two-door bottom-mount befrigerator/freezer
		if(self.obj_data.ICYCL == 4): self.obj_data.ICYCL = 2

	#-----------------------------------------------------------
	# Job 			: Adjsut unit for SI to Britch
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def adjust_units_for_all_types (self):
		self.obj_data.UA_FF = self.obj_data.UA_FF * 1.8961
		self.obj_data.UA_FZ = self.obj_data.UA_FZ * 1.8961
		self.obj_data.UA_ML = self.obj_data.UA_ML * 1.8961

		self.obj_data.UA_FF_CND = self.obj_data.UA_FF_CND * 1.8961
		self.obj_data.UA_FZ_CND = self.obj_data.UA_FZ_CND * 1.8961

		self.obj_data.UA_FF_HXS = self.obj_data.UA_FF_HXS * 1.8961
		self.obj_data.UA_FZ_HXS = self.obj_data.UA_FZ_HXS * 1.8961

		# change units on refrigeration load data
		self.obj_data.TROOM  = 1.8 * self.obj_data.TROOM  + 32.0
		self.obj_data.FFTEMP = 1.8 * self.obj_data.FFTEMP + 32.0
		self.obj_data.FZTEMP = 1.8 * self.obj_data.FZTEMP + 32.0

		self.obj_data.FFQ = self.obj_data.FFQ * 3.413
		self.obj_data.FZQOFF = self.obj_data.FZQOFF * 3.413
		self.obj_data.FZQON = self.obj_data.FZQOFF
		self.obj_data.FZQ   = self.obj_data.FZQOFF

		self.obj_data.FFSEN = self.obj_data.FFSEN * 3.413
		self.obj_data.FFLAT = self.obj_data.FFLAT * 3.413
		self.obj_data.FROSTF = self.obj_data.FROSTF * 3.413
		self.obj_data.FROSTFS = self.obj_data.FROSTF

		self.obj_data.FZSEN = self.obj_data.FZSEN * 3.413
		self.obj_data.FZLAT = self.obj_data.FZLAT * 3.413
		self.obj_data.FROSTZ = self.obj_data.FROSTZ * 3.413
		self.obj_data.FROSTZS = self.obj_data.FROSTZ

		self.obj_data.FFHTQ = self.obj_data.FFHTQ * 3.413
		self.obj_data.FZHTQ = self.obj_data.FZHTQ * 3.413

		self.obj_data.FFPENA = self.obj_data.FFPENA * 3.413
		self.obj_data.FZPENA = self.obj_data.FZPENA * 3.413

		self.obj_data.QMUL = self.obj_data.QMUL * 3.413

		self.obj_data.FFREFQ = self.obj_data.FFREFQ * 3.413
		self.obj_data.FZREFQ = self.obj_data.FZREFQ * 3.413

		self.obj_data.CONDF = self.obj_data.FFQ - self.obj_data.FFSEN - self.obj_data.FFLAT \
			- self.obj_data.FFHTQ - self.obj_data.FROSTF - self.obj_data.FFREFQ - self.obj_data.FFPENA
		
		self.obj_data.CONDZ = self.obj_data.FZQ - self.obj_data.FZSEN - self.obj_data.FZLAT \
			- self.obj_data.FZHTQ - self.obj_data.FROSTZ - self.obj_data.FZREFQ - self.obj_data.FZPENA		
		
		self.obj_data.CONDHT[1] = 3.6 * self.obj_data.CONDHT[1] # fixed not the same FORTAN
		self.obj_data.CONDHT[2] = 3.6 * self.obj_data.CONDHT[2] # fixed not the same FORTAN
		
		self.obj_data.CONDVP[1] = 3.6 * self.obj_data.CONDVP[1] # fixed not the same FORTAN
		self.obj_data.CONDVP[2] = 3.6 * self.obj_data.CONDVP[2] # fixed not the same FORTAN
		
		# error ===CONVERT UNITS need adjust may be move to other class
		self.obj_data.TS5 = self.obj_data.TS5  + 273.11
		RHOCPF   = 316.8 / self.obj_data.TS5
		self.obj_data.CFMF     = 1.8961 * (RHOCPF * self.obj_data.CFMF) / 0.4720

	#-----------------------------------------------------------
	# Job 			: add extra var for all types
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def setup_vars_for_all_types (self):
		self.obj_data.ETAF = 0.0
		self.obj_data.ETAV = 0.0
		self.obj_data.MROLD = 0.0
		
		self.obj_data.DUTYZ = 0.0
		self.obj_data.CAPZ = 0.0
		
	#-----------------------------------------------------------
	# Job 			: add extra var for all types except type 2
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def setup_vars (self):
		self.obj_data.INCTRL  = 0 # input is given only in Type 2
		self.obj_data.IFREZI = [1, 1, 1]
		
		self.obj_data.TS5 = 300 # set non zero value, prevent calculation error
		self.obj_data.CFMF = 300 # set non zero value, prevent calculation error
		self.obj_data.AREAFZ = 300 # set non zero value, prevent calculation error
		self.obj_data.UAF = 300 # set non zero value, prevent calculation error
		self.obj_data.DPF = 3 # set non zero value, prevent calculation error
		
	
	#-----------------------------------------------------------
	# Job 			: Convert file code to file name according to the list
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def getCompressorFileName (self, lng_fileCode):
		lstCompMap = ['ABB_EMX70HSC','BADBAD','DG57C84TAU6','DG73C12','DG73C12RAU6','DGH66C94','EGX90HLC','EGZ100HLP','EMBRACO MODEL','EMBRACO_NT6215Z','EMU30HSC','EMX70HSC','EMY60HER','FILES.TXT','GVT44AD','GVY44AD','SF51C97','SF51NEW','SMOOTHED','SP51C97','TESTMAP','TSA1374YAS','TTE46FK']
		if lng_fileCode < 1 or lng_fileCode > len(lstCompMap):
			return ""
		
		else:
			return lstCompMap[lng_fileCode -1]

	#-----------------------------------------------------------
	# Job 			: prepare calling data to Cycle analysis
	# Input 		: Item in array to use
	#			
	# Output		:
	#-----------------------------------------------------------
	def prepare_Data4Cycle (self, lng_item):
		self.obj_data.TS1[lng_item]   = self.obj_data.TS1[lng_item] + 273.11
		self.obj_data.TS3[lng_item]   = self.obj_data.TS3[lng_item] + 273.11
		
		self.obj_data.UTPE     = self.obj_data.UTPEI[lng_item] * 3.600
		self.obj_data.USUPE    = self.obj_data.USUPEI[lng_item] * 3.600
		self.obj_data.ATOTE    = self.obj_data.ATOTEI[lng_item]

		self.obj_data.DTSUPE   = self.obj_data.DTSPEI[lng_item]

		self.obj_data.UDSC     = self.obj_data.UDSCI[lng_item] * 3.600
		self.obj_data.UTPC     = self.obj_data.UTPCI[lng_item] * 3.600
		self.obj_data.USCC     = self.obj_data.USCCI[lng_item] * 3.600
		self.obj_data.ATOTC    = self.obj_data.ATOTCI[lng_item]

		self.obj_data.DTSUBC = self.obj_data.DTSBCI[lng_item]

		self.obj_data.TSPEC = self.obj_data.TSPECI[lng_item]
		if(self.obj_data.TSPECI[lng_item]  >  0.0): self.obj_data.TSPEC = self.obj_data.TSPECI[lng_item] + 273.11

		self.obj_data.CEI[lng_item]   = self.obj_data.CEI[lng_item] / 100.0
		self.obj_data.SEFFI[lng_item] = self.obj_data.SEFFI[lng_item] / 100.0
		self.obj_data.MEFF[lng_item]  = self.obj_data.MEFF[lng_item] / 100.0
		self.obj_data.QCAN[lng_item]  = self.obj_data.QCAN[lng_item] / 100.0
		self.obj_data.QHILO[lng_item] = self.obj_data.QHILO[lng_item] / 100.0

		self.obj_data.SIZE = self.obj_data.SIZEN[lng_item] / 0.252
		self.obj_data.EER = self.obj_data.EERN[lng_item]
		self.obj_data.ICOOL = self.obj_data.ICOOLN[lng_item]
		#
		# FILL THE COMMON BLOCK FOR REFRIGERATION LOOP N
		#
		self.obj_data.ICOND = self.obj_data.ICONDI[lng_item]
		self.obj_data.IFRSH = self.obj_data.IFRSHI[lng_item]
		
		self.obj_data.MREF  = self.obj_data.MREFI[lng_item] * 2.20462
		self.obj_data.SPEEDN = self.obj_data.SPDNOM[lng_item]
		self.obj_data.SPEED  = self.obj_data.SPEEDI[lng_item]

		if(self.obj_data.IMAP == 1): self.obj_data.SPEED = self.obj_data.SPEEDN * self.obj_data.SPEEDI[lng_item]
		self.obj_data.DISPLC[lng_item] = self.obj_data.DISPLC[lng_item] / 16.3871 # from cm3 to cu-inch compressor displacement
	
		self.obj_data.CE = self.obj_data.CEI[lng_item]
		
		self.obj_data.SEFF  = self.obj_data.SEFFI[lng_item]
		self.obj_data.FANE  = self.obj_data.FNPWRE[lng_item]
		self.obj_data.FANC  = self.obj_data.FNPWRC[lng_item]
		
		RHOCPC   = 316.8 / self.obj_data.TS1[lng_item]
		RHOCPE   = 316.8 / self.obj_data.TS3[lng_item]
		self.obj_data.CFMC     = 1.8961 * (RHOCPC * self.obj_data.CFMCI[lng_item]) / 0.4720
		self.obj_data.CFME     = 1.8961 * (RHOCPE * self.obj_data.CFMEI[lng_item]) / 0.4720

		if(self.obj_data.IFREZI[lng_item] != 0)  :
			self.obj_data.UAF = 3.600 * self.obj_data.UAF				
		
		self.obj_data.IFREZ = self.obj_data.IFREZI[lng_item]

	#-----------------------------------------------------------
	# Job 			: prepare calling parameter for fror Cycle analysis
	# Input 		: lng_item cycle number, N.B cycle type 3 only which has two cycles
	#			
	# Output		:
	#-----------------------------------------------------------
	def call_cycle (self, lng_item):

		obj_parameter_data = QData ()
		
		#obj_parameter_data.IR = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
		#for nCnt in range (len(self.obj_data.IR)):
		#	obj_parameter_data.IR[nCnt]= self.obj_data.IR[nCnt]
		obj_parameter_data.IR = self.getArr2dCol(self.obj_data.IR, lng_item)
		
		#obj_parameter_data.XM = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
		#for nCnt in range (len(self.obj_data.X)):
		#	obj_parameter_data.XM[nCnt]= self.obj_data.X[nCnt]
		obj_parameter_data.XM = self.getArr2dCol(self.obj_data.X, lng_item)
		
		#obj_parameter_data.F = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
		#for nCnt in range (len(self.obj_data.F)):
		#	obj_parameter_data.F[nCnt]= self.obj_data.F[nCnt]
		obj_parameter_data.F = self.getArr3dLevel(self.obj_data.F, lng_item)
		
		obj_parameter_data.NC = self.obj_data.NC[lng_item]
		obj_parameter_data.TS1 = self.obj_data.TS1[lng_item]
		obj_parameter_data.TS3 = self.obj_data.TS3[lng_item]
		obj_parameter_data.TS5 = self.obj_data.TS5
		obj_parameter_data.MEFF = self.obj_data.MEFF[lng_item]
		obj_parameter_data.QHILO = self.obj_data.QHILO[lng_item]
		obj_parameter_data.QCAN = self.obj_data.QCAN[lng_item]
		
		obj_parameter_data.DPC = self.obj_data.DPC[lng_item]
		obj_parameter_data.DPE = self.obj_data.DPE[lng_item]
		obj_parameter_data.DPF = self.obj_data.DPF
		
		obj_parameter_data.ETHX1 = self.obj_data.ETHX[lng_item] # both the same value
		obj_parameter_data.ETHX2 = self.obj_data.ETHX[lng_item]
		
		obj_parameter_data.DISPLC = self.obj_data.DISPLC[lng_item]
				
		obj_parameter_data.FROSTF = self.obj_data.FROSTF
		obj_parameter_data.FROSTZ = self.obj_data.FROSTZ
		
		obj_parameter_data.ICAB  = 1#self.obj_data.ICAB
		
		obj_parameter_data.IRFTYPE = self.obj_data.IRFTPL
		obj_parameter_data.ICYCL = self.obj_data.ICYCL
		obj_parameter_data.ICYCLS = self.obj_data.ICYCLS
		obj_parameter_data.IDFRST = self.obj_data.IDFRST
		
		obj_cycle = Cycle (obj_parameter_data, self.obj_data)
		
		obj_parameter_data.NCYC = 1 #number of call to cycle (1=Single or 2= Dual cycle)
		return obj_cycle.cycle()
		
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 1 - Standard 
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_1Standard (CycleType_Abstract):
	'''
	def view (self, str_file_cycle, str_path_cycle = ""):	
		# SET UP PARAMETERS FOR SUMMARY OUTPUT
		#
		if(self.obj_data.IFAN != 2)  :
			self.obj_data.DUTYN[N] = self.obj_data.DUTYC
			self.obj_data.WCOMP[N] = self.obj_data.W / self.obj_data.CORR_COP / 1.0548
		
		#if(ICYCL != 2)  :
		self.obj_data.QE_NET = self.obj_data.QE - self.obj_data.Q_HXS_FF
		self.obj_data.QEL[N] = self.obj_data.QE_NET
		self.obj_data.FLOWN[N]  = self.obj_data.FLOW
		self.obj_data.COPRN[N]  = self.obj_data.COPR
		self.obj_data.COPCYC[N] = self.obj_data.CORR_COP
					
		obj_view = ViewCycle(self.obj_data, str_file_cycle, str_path_cycle)
		obj_view.show_rep()
	'''
	def calculte (self):
		self.obj_data.ITYPE = 1
		self.obj_data.TS5 = -300.0
		self.obj_data.DPF = 0.0
		
		self.obj_data.IEVAP  = 0
		self.obj_data.ISPEC  = self.obj_data.ISPECI[1]
		self.obj_data.XEXITE = self.obj_data.QUALTY[1]
		self.obj_data.DTSUPI = self.obj_data.SUPIHX[1]
		
		return self.call_cycle (1)
				
	def setup_vars_extra (self):
		self.setup_vars()
		
	def adjust_units (self):
		self.adjust_units_for_all_types()
	
	def adjust_input (self):
		self.adjust_input_for_all_types()
		self.prepare_Data4Cycle (1)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 2 - Lorenz  
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_2Lorenz (CycleType_Abstract):

	def calculte (self):
		pass
		
	def setup_vars_extra (self):
		pass
		
	def adjust_units (self):
		self.adjust_units_for_all_types()

	def adjust_input (self):
		self.adjust_input_for_all_types()
		self.prepare_Data4Cycle (1)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 2 - Lorenz
#				Control Method 4 -switching valve 
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_2Lorenz_4swtchVLV (Type_2Lorenz):
	def adjust_input (self):
		super.adjust_input()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 2 - Lorenz
#				Control Method 5 -solenoid valve
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_2Lorenz_5solindVLV (Type_2Lorenz):
	def adjust_input (self):
		#	if INCTRL == 5 i.e solenoid valve or fan control
		#	set IDFRST =1 Manual Defrost to Yes
		self.obj_data.IDFRST = 1	
		super.adjust_input()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 2 - Lorenz
#				Control Method any other ctrl method, but not 4 & 5
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_2Lorenz_ctrlOthers (Type_2Lorenz):
	def adjust_input (self):
		super.adjust_input()
	
		
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 3 - Dual Loop 
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_3DualLoop (CycleType_Abstract):
		
	def calculte (self):
		pass
		
	def setup_vars_extra (self):
		self.setup_vars()
		
	def adjust_units (self):
		self.adjust_units_for_all_types()

	def adjust_input (self):
		self.adjust_input_for_all_types()
		
		# get compressor file name
		self.obj_data.FILMAP2 = self.getCompressorFileName (self.obj_data.FILMAP2_CODE)
		
		self.prepare_Data4Cycle (1)
		#self.prepare_Data4Cycle (2)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Analisis Cycle Type 4 - Dual Evap 
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Type_4DualEvap (CycleType_Abstract):
		
	def calculte (self):
		pass

	def setup_vars_extra (self):
		self.setup_vars()
	
	def adjust_units (self):
		self.adjust_units_for_all_types()
	
	def adjust_input (self):
		self.adjust_input_for_all_types()
		self.prepare_Data4Cycle (1)
