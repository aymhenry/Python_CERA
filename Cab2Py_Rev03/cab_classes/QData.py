# Python Import


# User Import
#-----------------------------------------------------------
# Job 			: Creates Data Model for Q files
# 		
# Editor		: aymhenry@gmail.com
#-----------------------------------------------------------

class QData:
	
	def setup_vars( self, arg, lst_var_names, int_confg_row=0, int_para_count=-1, b_set_data_as_string = False):
		int_count = int_para_count
		
		# if call int_para_count = -1 then use all list items
		if int_count == -1:
			int_count = len(lst_var_names) -1
		
		# simulate override function in Python
		if type(arg) is list:
			self.__setup_vars_list ( arg, lst_var_names, int_confg_row, int_count, b_set_data_as_string)
			
		if type(arg) is float:
			self.__setup_vars_single ( arg, lst_var_names, int_confg_row, int_count, b_set_data_as_string)

			
	def __setup_vars_single (self, flt_val, lst_var_names, int_confg_row, int_para_count, b_set_data_as_string = False ):
		for int_var_numer in range (int_confg_row, int_para_count +1):
		
			# check if saving the data is string , the add "'"
			if b_set_data_as_string :
				flt_val = "'" + str(flt_val) + "'"
				
			# save values to data object	
			exec ("self." + lst_var_names[int_var_numer - int_confg_row] + "=" + str(flt_val)  )
		
	def __setup_vars_list (self, lst_valuse, lst_var_names, int_confg_row, int_para_count, b_set_data_as_string = False):
		'''
		Creates a list of vars, its names in lst_var_names, and values in lst_valuse
		Input
			lst_valuse 		: list of values
			lst_var_names	: List of Vars names
			int_confg_row	: Start read values from  lst_valuse from this item 
			int_para_count	: Count of vars
		Output
			Vars creaed iside this object
		'''
		
		for int_var_numer in range (int_confg_row, int_para_count +1):
			feedback = lst_valuse [int_var_numer]
			
			# check if saving the data is string , the add "'"
			if b_set_data_as_string :
				feedback = "'" + str(feedback) + "'"
			
			# save values to data object
			exec ("self." + lst_var_names[int_var_numer - int_confg_row] + "="  + str(feedback) )

		#
		# ERMxxx is the emissivity of the Room 
		#		(FRT = Front, BCK = Back, LFT = Left, RGT = Right, TOP = Top, BOT = Bottom)
		# For the top mount freezer (1 & 3) 
		#        FTT = Front/Top,
		#        FTB = Front/Bottom
		#        BKT = Back/Top
		#        BKB = Back/Bottom
		#        LTT = Left/Top
		#        LTB = Left/Bottom
		#        RTT = Right/Top
		#        RTB = Right/Bottom
		# EFRxxx is the emissivity of the refrigerator walls
		# TRADxx or TRAxxx is the Radiative temperature of the Room walls (F)
		# TFSccc is the CALCULATED Freezer outside wall surface temperature (F)
		# TRSccc is the CALCULATED Fresh Food outside wall surface temperature (F)
		# QRDxxx is the Radiation Heat Transfer from the room walls to the outside wall of the refrigerator (BTU/hr)
		# QFCxxx is the Forced Convection Heat Transfer from the air to the outside wall of the refrigerator (BTU/hr)
		# HIxxx  is the Forced Convection Heat Transfer from the air to the outside wall of the refrigerator (BTU/hr)
		# HOxxx  is the Forced Convection Heat Transfer from the air to the inside wall of the refrigerator (BTU/hr)
		# TxxSID is the air temperature on the outside of the :-
		#        F (Freezer) 
		#        R (Fresh Food) cabinet on the L (Left) or R (Right) side
		# TxBACK is the air temperature on the outside of the F (Freezer) or
		#        R (Fresh Food) cabinet on the BACK
		# TxFRNT is the air temperature on the outside of the F (Freezer) or
		#        R (Fresh Food) cabinet on the front


		# COMPRESSOR COMPARTMENT DIMENSIONS
		# CWIDE - width of compressor compartment from outside wall to inner( freezer side ) wall (will be converted to INCHES )
		# CHGT - height of compressor compartment (will be converted to INCHES )
		
		# LINER DATA
		# DOL - thickness of outer liner (will convert to INCHES )
		# DIL - thickness of inner liner (will convert to INCHES )
		# COL - conductivity of outer liner (will convert to BTU / HR-FT-F )
		# CIL - conductivity of inner liner (will convert to BTU / HR-FT-F )

		# MULLION DATA
		# TOPMUL - distance from outer top of unit to top of mullion (will convert to INCHES )
		# THMUL - total thickness of the mullion section (will convert to INCHES )
		# WALL - the distance from the outside cab.wall of the fresh food compartment to the mullion
		
		# FREEZER INSULATION THICKNESS
		# TIFT - thickness of insulation on top of freezer (will convert to INCHES )
		# TIFLS - thickness of insulation on left side of freezer (will convert to INCHES )
		# TIFRS - thickness of insulation on right side of freezer (will convert to INCHES )
		# TIFF - thickness of insulation on front of freezer (will convert to INCHES )
		# TIFB - thickness of insulation on back of freezer (will convert to INCHES )
		
		# REFRIGERATOR SECTION INSULATION (all will convert to INCHES )
		# TIRT - thickness of insulation on top of fresh food compartment.
		# TIRS - thickness of insulation on sides of fresh food compartment.
		# TIRF - thickness of insulation on front of fresh food compartment 
		# TIRB - thickness of insulation on back of fresh food compartment.

		# INSULATION AROUND SIDE OF THE COMPRESSOR COMPARTMENT (all will convert to INCHES )
		# BINSUL - maximum thickness of cab.bottom insulation fresh food.
		# CINSUL - thickness of insulation over compressor.
		# BINFRZ - maximum thickness of cab.bottom insulation freezer.
		# SCIN - thickness of insulation on side of compressor compartment.
		# STIN - thickness of insulation on top of compressor compartment.
		
		# CABINET SECTION INTERNAL VOLUMES (all will convert Cu.FT )
		# HXVUZ - freezer volume used for heat exchangers.
		# VOLAZ - adjusted freezer volume.
		# HXVUR - fresh food volume used for heat exchangers.
		# VOLAR - adjusted general refrigerated volume.
	
		# TEMPERATURES AND THERMAL VALUES (all will convert DEG F )
		# TROOM - room temperature 
		# TFRZ - freezer temperature
		# TFF - fresh food compartment temperature 
		# TBTM - underside air temperature.
		
		# READ THE THERMAL CHARACTERISITICS
		# DKIN - thermal conductivity of the door (will be converted to BTU/HR-FT -DEG F )
		# HIWP - wall panel gap film coefficient (will be converted to  BTU/HR-FT2-DEG F )
		# HO - outside film coefficient (will be converted to  BTU/HR-FT2-DEG F )
		# HI - Inside film coefficient (will be converted to  BTU/HR-FT2-DEG F )
		# HLRG - gasket heat leak for fresh food compartment for UNITS 1,2,3 OR 4 (will be converted to BTU/HR IN DEG F)
		# HLGZN - fan on gasket heat leak for freezer compartment for UNITS 1,2 OR 3 OR total unit for UNITS 5,6 OR 7 will be converted to( BTU/HR IN DEG F )
		# HLGZF - fan off gasket heat leak for freezer compartment for UNITS 1,2 OR 3 OR total unit for units 5,6 OR 7, will be converted to (BTU/HR IN DEG F )
		# WKIN - thermal conductivity of fz cab.wedge insulation (will be converted to  BTU/HR FT DEG F )
		# WKINR - thermal conductivity of ff cab.wedge insulation (will be converted to BTU/HR FT DEG F )
		# RKIN - thermal conductivity of refrigerator insulation (will be converted to  BTU/HR FT DEG F )
		# TKIN - thermal conductivity of insulation cab.in top of chest freezeR (will be converted to BTU/HR FT DEG F )
		# CKMUL - thermal conductivity of the mullion insulation (will be converted to BTU/HR FT DEG F )
		
		# the Door Gasket Heater Watts and calculate the BTU's of heat into the cabinet, the ANTI - SWEAT heater watss and the
		# BTU's into the cabinet and finally the AUXILIARY energy and the BTU's into the cabinet.
		# FFxxx and FZxxx for Frish food and Frizer compartment.
		
