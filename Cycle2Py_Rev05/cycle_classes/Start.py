# Python Import ==================
import math, sys, datetime

# User Import ======================

from .CycleType import *

from .CycleDataModelBuiler import CycleDataModelBuiler
#from ..common_class.QData import QData

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
# Job 			: Start Cycle app
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Start:
	#Class static vars
	FILE_CYC_INPUT  = "Cycle_dat.csv"	# input file for cabinit module
	FILE_CYC_OUTPUT = "Cycle_out.csv"	# output file for cabinit module

	def __init__ (self):
		self.obj_data = None	# object to save data
		self.obj_control = None	# object to point to object control on data

		self.str_FILE_CYC_INPUT = Start.FILE_CYC_INPUT		
		self.str_FILE_CYCLE_OUTPUT = Start.FILE_CYC_OUTPUT

		self.str_path_cyc_in   = ""
		self.str_path_cyc_out = ""

	def set_filenames  (self, str_file_cyc_in = "", str_file_cyc_out="", str_path_cyc_in = "", str_path_cyc_out =""):
		self.str_path_cyc_in   = str_path_cyc_in
		self.str_path_cyc_out = str_path_cyc_out

		if str_file_cyc_in != "":
			self.str_FILE_CYC_INPUT = str_file_cyc_in

		if str_file_cyc_out != "":
			self.str_FILE_CYCLE_OUTPUT = str_file_cyc_out

	#-----------------------------------------------------------
	# Job 			: Main app start up, driver for all others
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def main (self):
		self.data_prepare ()

		try:
			#print ("aym @ 51 self.calculte")
			self.calculte ()		# calculate heat rate 
			#self.calculte_cycle ()	# claclulate cycle data
						
		except ValueError as err_description: # OSError
			print ("Fatal program error ... system terminated")
			print (str(err_description) + "\n\n")
			print ("=======================================")
			print ("Expected resone, none propoer input data")
			print ("=======================================\n\n")
			sys.exit('3100')	# terminat application

		print ("aym @ 51 self.view")
		#self.view ()			# output results
		
	#-----------------------------------------------------------
	# Job 			: output results a reported form
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def view (self):
		self.obj_control.view( self.str_FILE_CYCLE_OUTPUT, str_path_cyc_out)

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
		obj_datamodel = CycleDataModelBuiler()
		
		obj_datamodel.set_init_data (self.str_FILE_CYC_INPUT, self.str_path_cyc_in ) 	# Input data file name

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
			
		# 1: Standard
		if self.obj_data.ICYCL == 1: 
			self.obj_control = Type_1Standard (self.obj_data)
						
		# 2: Lorenz
		elif self.obj_data.ICYCL == 2:
			# INCTRL: 	0 = none, 
			#			1 = adjust evaporator areas,
			#			2 = adjust fresh food section tempeature,
			#			3 = adjust freezer    section tempeature,
			# 			4 = switching valve (only one section is cooled  at a time)
			#			5 = solenoid valve or fan control provides evaporator capacity to only one cabinet
			#				during part of the cycle		
			if self.obj_data.INCTRL == 4:
				self.obj_control = Type_2Lorenz_4swtchVLV (self.obj_data)
				
			elif self.obj_data.INCTRL == 5:
				self.obj_control = Type_2Lorenz_5solindVLV (self.obj_data)
			
			else:
				self.obj_control = Type_2Lorenz_ctrlOthers (self.obj_data)

		# 3: Dual Loop
		elif self.obj_data.ICYCL == 3: 
			self.obj_control = Type_3DualLoop (self.obj_data)

		# 4: Dual Evap
		elif self.obj_data.ICYCL == 4: 
			self.obj_control = Type_4DualEvap (self.obj_data)

		# add extra vars to obj_data
		# moved to class: self.obj_control.setup_vars_extra()
		
		# adjust default vars, according to basic input values
		self.obj_control.adjust_input()
		
		# Convert to units
		self.obj_control.adjust_units()
		
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
