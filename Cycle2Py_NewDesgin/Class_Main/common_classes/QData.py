# Python Import


# User Import
# -----------------------------------------------------------
# Job 			: Creates Data Model for Q files
# 		
# Editor		: aymhenry@gmail.com
# -----------------------------------------------------------

class QData:
	
	def setup_vars(self, arg, lst_var_names, int_confg_row=0, int_var_names_count=-1, b_set_data_as_string=False):
		int_para_count = int_var_names_count
		
		# if int_var_names_count = -1 then use all list items
		if int_para_count == -1:
			int_para_count = len(lst_var_names) - 1
		
		# simulate override function in Python
		if type(arg) is list:
			self.__setup_vars_list(arg, lst_var_names, int_confg_row, int_para_count, b_set_data_as_string)
			
		if type(arg) is float:
			self.__setup_vars_single(arg, lst_var_names, int_confg_row, int_para_count, b_set_data_as_string)

	def __setup_vars_single(self, flt_val, lst_var_names, int_confg_row, int_para_count, b_set_data_as_string=False):
		for int_var_numer in range(int_confg_row, int_para_count + 1):
		
			# check if saving the data is string , the add "'"
			if b_set_data_as_string:
				flt_val = flt_val.replace("'", '"')
				flt_val = flt_val.replace("\n", '')
				flt_val = "'" + str(flt_val) + "'"
				
			# save values to data object	
			exec("self." + lst_var_names[int_var_numer - int_confg_row] + "=" + str(flt_val))
		
	def __setup_vars_list(self, lst_valuse, lst_var_names, int_confg_row, int_para_count, b_set_data_as_string=False):
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
		
		for int_var_numer in range(int_confg_row, int_para_count + 1):
			feedback = lst_valuse[int_var_numer]
			
			# check if saving the data is string , the add "'"
			if b_set_data_as_string:
				feedback = feedback.replace("'", '"')
				feedback = feedback.replace("\n", '')

				feedback = "'" + str(feedback) + "'"

			#  save values to data object
			# print("self." + lst_var_names[int_var_numer - int_confg_row] + "=" + str(feedback))
			exec("self." + lst_var_names[int_var_numer - int_confg_row] + "=" + str(feedback))

		# =================================================
		# Variable list for CYCLE module
		# =================================================
		#            NC - NUMBER OF COMPONENTS
		#            IR[I] - CODE NUMBERS FOR THE I'TH COMPONENT OF THE
		#            			REFRIGERANT MIXTURE (REFER TO PROPERTIES
		#            			DOCUMENTATION)
		#            F(I1,I2) - MIXTURE INTERACTION PARAMETER BETWEEN COMPONENT
		#            				I1 AND I2
		#            XM[I] - COMPOSITION OF CIRCULATING REFRIGERANT (MASS
		#            			FRACTION OF IR[I])
		#            TS1 - HEAT TRANSFER FLUID (HTF) TEMPERATURE ENTERING CONDENSER
		#            TS3 - HTF TEMPERATURE ENTERING FRESH FOOD EVAPORATOR
		#            TS5 - HTF TEMPERATURE ENTERING FREEZER EVAPORATOR
		#            MEFF - MECHANICAL EFFICIENCY
		#            QHILO - NORMALIZED HEAT LOSS FROM DISCHANGE LINE INSIDE
		#            			THE COMPRESSOR SHELL TO SUCTION GAS
		#            QCAN - COMPRESSOR SHELL LOSS NORMALIZED TO POWER INPUT
		#            Cycle.obj_data.DPC - PRESSURE DROP THROUGH CONDENSER
		#            DPE - PRESSURE DROP THROUGH FRESH FOOD EVAPORATOR
		#            DPF - PRESSURE DROP THROUGH FREEZER EVAPORATOR
		#            ETHX1 - EFFECTIVENESS OF HIGH TEMP INTERCHANGER
		#            ETHX2 - EFFECTIVENESS OF LOW  TEMP INTERCHANGER
		#            DISPLC - COMPRESSOR DISPLACEMENT (CU-IN)
		#            NCYC - NUMBER OF CALL TO CYCLE (1 OR 2 FOR DUAL LOOP)
		#            ICAB - FLAG TO REPRESENT PRESENCE OF CABINET LOADS IN INPUT
		#            ICYCL - CYCLE TYPE (1=STANDARD, 2=LORENZ, 3=DUAL LOOP, 4=DUAL EVAP)
		#            ICNTRL - CONTROL METHOD FOR EVAPORATOR LOAD
		#            			0 = NONE
		#            			1 = FRESH FOOD FAN OFF
		#            			2 = FRESH FOOD EVAPORATOR SUPERHEAT CONTROL
		#            			3 = FREEZER FAN OFF
		#            			4 = FREEZER AIR DAMPER CONTROL
		#
		# PROPERTY ROUTINES REFERENCED:
		#            BCONST - INITIALIZES ARRAYS OF PROPERTY COEFFICIENTS
		#            BUBLP - SATURATION PROPERTIES AT GIVEN PRESSURE
		#            BUBLT - SATURATION PROPERTIES AT GIVEN TEMPERATURE
		#            ENTROP - MOLAR ENTROPY
		#            ESPAR - SET UP COEFFICIENTS FOR EQUATION OF STATE
		#            HCVCPS - MOLAR ENTHALPY AND HEAT CAPACITY
		#            HPIN - TEMPERATURE, QUALITY, ETC. AS A FUNCTION OF ENTHALPY
		#            		AND PRESSURE
		#            SPIN - TEMPERATURE, QUALITY, ETC. AS A FUNCTION OF ENTROPY
		#            		AND TEMPERATURE
		#            VIT - CALCULATE SPECIFIC VOLUME
		#
		# NOTE:  THE ABOVE ROUTINES REFERENCE ADDITIONAL PROPERTY ROUTINES
		#            	THE ENTIRE SET SHOULD BE INCLUDED IN THE EXECUTABLE ELEMENT
		#
		# ADDITIONAL SUBROUTINES REFERENCED:
		#            CCROSS - CROSS FLOW CONDENSER
		#            CCOUNT - COUNTER FLOW CONDENSER
		#            COMP - COMPRESSOR MODEL
		#            COND - CONDENSER ALGORITHMS
		#            FFCROSS - CROSS FLOW EVAPORATOR
		#            FFCOUNTER - COUNTER FLOW EVAPORATOR
		#            FRSH - FRESH FOOD EVAPORATOR ALGORITHMS
		#            LOWEVP - FREEZER EVAPORATOR ALGORITHMS
		#            PROGRS - DISPLAY CURRENT VALUES ON SCREEN
		#            SHWFIG - DISPLAY A DRAWING OF THE CYCLE ON THE SCREEN
		#            SHWOUT - PRINT OUT CALCULATED VALUES OT THE SCREEN
		#
		# NOMENCLATURE FOR FIRST LETTER(S) OF VARIABLE NAMES:
		#            DT - TEMPERATURE DIFFERENCE
		#            FT - CONVERGENCE VARIABLE, LOOP HAS CONVERGED WHEN FT=0
		#            H - ENTHALPY
		#            HREF - CHARACTER VARIABLE FOR REFRIGERANT NAMES
		#            HSTATE - CHARACTER VARIABLE FOR CYCLE STATE POINTS:
		#            			(INLET AND OUTLET REFER TO REFRIGERANT FLOW)
		#            	1 - COMPRESSOR INLET (SATURATED VAPOR)
		#            	2 - COMPRESSOR DISCHARGE
		#            	3 - CONDENSER DEW POINT
		#            	4 - CONDENSER OUTLET
		#            	5 - INLET TO FRESH FOOD EVAPORATOR
		#            	6 - LIQUID LINE OUTLET FROM HIGH TEMP INTERCHANGER
		#            	7 - OUTLET FROM FRESH FOOD EVAPORATOR
		#            	8 - INLET TO FREEZER EVAPORATOR
		#            	9 - OUTLET FROM FREEZER EVAPORATOR
		#            	10 - LIQUID LINE OUTLET FROM LOW TEMP INTERCHANGER
		#            	11 - CONDENSER BUBBLE POINT
		#            	12 - FRESH FOOD EVAPORATOR DEW POINT
		#            	13 - SUPERHEATED GAS LEAVING THE HIGH TEMP INTERCHANGER
		#            	14 - CONDENSER INLET
		#            	15 - INTERNAL VARIABLE (NOT SHOWN) FOR EVAP DEW POINT
		#            	16 - LIQUID LINE STATE AFTER HEAT LOSS TO CABINET AND MULLION
		#
		#            L - LOGICAL VARIABLE (ERROR FLAG, ETC.)
		#            P - PRESSURE
		#            T - TEMPERATURE
		#            TC - REFRIGERANT AT CONDENSER OUTLET
		#            TE - REFRIGERANT AT EVAPORATOR OUTLET
		#            TOL - CONVERGENCE TOLERANCE
		#            TS - TEMPERATURE OF HEAT TRANSFER FLUID
		#            V - VOLUME
		#            X - COMPOSITION
		#            XL(I,J) - LIQUID PHASE COMPOSITION AT STATE J
		#            XV(I,J) - VAPOR PHASE COMPOSITION AT STATE J
		#            XQ - QUALITY
		#

		# =================================================
		# Variable list for CAB module
		# =================================================
		# ERMxxx is the emissivity of the Room 
		# 	(FRT = Front, BCK = Back, LFT = Left, RGT = Right, TOP = Top, BOT = Bottom)
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
		# CWIDE - width of compressor compartment
		# 	from outside wall to inner( freezer side ) wall (will be converted to INCHES )
		# CHGT - height of compressor compartment (will be converted to INCHES )
		
		# LINER DATA
		# DOL - thickness of outer liner (will convert to INCHES )
		# DIL - thickness of inner liner (will convert to INCHES )
		# COL - conductivity of outer liner (will convert to BTU / HR-FT-F )
		# CIL - conductivity of inner liner (will convert to BTU / HR-FT-F )

		# MULLION DATA
		# TOPMUL - distance from outer top of unit to top of mullion (will convert to INCHES )
		# THMUL - total thickness of the mullion section (will convert to INCHES )
		# WALL - the distance from the outside wall of the fresh food compartment to the mullion
		
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
		# BINSUL - maximum thickness of bottom insulation fresh food.
		# CINSUL - thickness of insulation over compressor.
		# BINFRZ - maximum thickness of bottom insulation freezer.
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
		# HLGZN - fan on gasket heat leak for freezer compartment
		# 			for UNITS 1,2 OR 3 OR total unit for UNITS 5,6 OR 7 will be converted to( BTU/HR IN DEG F )
		# HLGZF - fan off gasket heat leak for freezer compartment for UNITS 1,2 OR 3 OR total unit
		# 			for units 5,6 OR 7, will be converted to (BTU/HR IN DEG F )
		# WKIN - thermal conductivity of fz wedge insulation (will be converted to  BTU/HR FT DEG F )
		# WKINR - thermal conductivity of ff wedge insulation (will be converted to BTU/HR FT DEG F )
		# RKIN - thermal conductivity of refrigerator insulation (will be converted to  BTU/HR FT DEG F )
		# TKIN - thermal conductivity of insulation in top of chest freezeR (will be converted to BTU/HR FT DEG F )
		# CKMUL - thermal conductivity of the mullion insulation (will be converted to BTU/HR FT DEG F )
		
		# the Door Gasket Heater Watts and calculate the BTU's of heat into the cabinet
		# 			, the ANTI - SWEAT heater watss and the
		# BTU's into the cabinet and finally the AUXILIARY energy and the BTU's into the cabinet.
		# FFxxx and FZxxx for Frish food and Frizer compartment.

