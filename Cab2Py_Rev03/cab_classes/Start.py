# Python Import ==================
import math, sys, datetime

# User Import ======================


#..................................
#TestONLY =====================
#from .Test import Test
#TestONLY =====================

from .QCtrl import *

from .DataModelBuiler import DataModelBuiler
from .QData import QData
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
# Job 			: Start Cab app
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -

class Start:
	#Class static vars
	FILE_CAB_INPUT  = "Cabinet_dat.csv"	# input file for cabinit module
	FILE_CAB_OUTPUT = "Cabinet_out.csv"	# output file for cabinit module
	FILE_CYC_OUTPUT = "Cycle_out.csv"	# output file for cabinit module
		
	def __init__ (self):
		self.obj_data = None	# object to save data
		self.obj_control = None	# object to point to object control on data
	#-----------------------------------------------------------
	# Job 			: Main app start up, driver for all others
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def main (self):
		self.data_prepare ()
		#TestONLY =====================
				#obj_test = Test(self.obj_data)
				#obj_test.config1_input()
		#TestONLY =====================	
		try:
			self.calculte ()		# calculate heat rate in Qxx classes
			self.calculte_cycle ()	# claclulate cycle data
			
		except ValueError as err_description: # OSError
			print ("Fatal program error ... system terminated")
			print (str(err_description) + "\n\n")
			print ("=======================================")
			print ("Expected resone, none propoer input data")
			print ("=======================================\n\n")
			sys.exit('3100')	# terminat application
		
		self.view ()			# output results
	#-----------------------------------------------------------
	# Job 			: output results a reported form
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def view (self):
		self.obj_control.view( Start.FILE_CAB_OUTPUT, Start.FILE_CYC_OUTPUT)
		
	#-----------------------------------------------------------
	# Job 			: Calaculte heat balance, the main app target.
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def calculte (self):
		self.obj_control.calculte()
			
	#-----------------------------------------------------------
	# Job 			: Calculte cycle data
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def calculte_cycle (self):
		self.obj_control.calculte_cycle()
	#-----------------------------------------------------------
	# Job 			: Preprae the main data object & control object
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def data_prepare (self):
		# Set main data file name
		obj_datamodel = DataModelBuiler().getInstance()		# get object to create data inside obj_data
		obj_datamodel.set_init_data (Start.FILE_CAB_INPUT) 	# Input data file name
		
		# check if error, if so exit application
		if obj_datamodel.isError():
			print ("Error Opening file")
			print (obj_datamodel.err_description() )	# print error description
			obj_datamodel ="" 	# clean object and close file
			sys.exit('3000')	# terminat application
			#--------------
		
		# read data list from file, and put values in variables
		obj_datamodel.build_var_list()

		# Is data is good, or exit application
		if obj_datamodel.isError():
			print (obj_datamodel.err_description() )	# print error description
			sys.exit('3001')							# terminate

		# Create related data object as the given configration
		self.obj_data = obj_datamodel.get_data_object()

		# Create related object as the given configration
		self.obj_control = ""
		
		# 1: Two door, top - mount refrigerator / freezer
		if self.obj_data.IRFTYP == 1: # Mode 3	Top - mount refrigerator / freezer
			self.obj_control = QCtrl_Ql13 (self.obj_data)
		
		# 2: Two door, bottom - mount refrigerator / freezer
		elif self.obj_data.IRFTYP == 2: # Mode 8	Bottom - mount refrigerator / freezer
			self.obj_control = QCtrl_Ql8 (self.obj_data)
		
		# 3: Side by side refrigerator / freezer
		elif self.obj_data.IRFTYP == 3: # Mode 2	Side - by - side refrigerator / freezer
			self.obj_control = QCtrl_Ql2 (self.obj_data)
		
		# 4: Chest freezer
		elif self.obj_data.IRFTYP == 4: # Mode 5	Chest freezer
			self.obj_control = QCtrl_Ql5 (self.obj_data)
		
		# 5: Upright freezer
		elif self.obj_data.IRFTYP == 5: # Mode 7	Upright freezer
			self.obj_control = QCtrl_Ql467 (self.obj_data)
		
		# 6: One door refrigerator
		elif self.obj_data.IRFTYP == 6: # Mode 4	Single - door refrigerator
			self.obj_control = QCtrl_Ql467 (self.obj_data)
		
		# 7: Two door refrigerator / freezer
		elif self.obj_data.IRFTYP == 7: # Mode 3	Top - mount refrigerator / freezer	
			self.obj_control = QCtrl_Ql13 (self.obj_data)
		
		# Creates Vars & convert units
		self.obj_control.set_vars_common()		# Create commom in abstract class
		self.obj_control.setup_vars_extra()		# Create extra vars, indvidual for every class
		
		self.obj_control.setMode()				# set mode in commom in abstract class
		self.obj_control.set_ncctype()			# set ncctype in commom in abstract class

		# Calculate Volume
		self.obj_control.volume()
		
		self.obj_control.adj_unit_common()		# unit adjust in commom in abstract class
		
		self.obj_control.adjust_units()
		
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
