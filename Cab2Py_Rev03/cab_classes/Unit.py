# Python import


# User Import
#-----------------------------------------------------------
# Job 			: Unit Conversion
# 		
# Editor		: aymhenry@gmail.com
#-
class Unit:
	# Factor used to change other unit.
	# Example DIST_Unit1_Unit2, factor to change from unit1 to unuit 2 : unit1 = DIST_Unit1_Unit2 * Unit2
	# DIST : Distance
	# TEMP : Temperature  F1(T) = (T - 32.0)/1.8                 !F to C
	# ENRG : Energy
	# COND : Conductivity (BTU/HR-FT-F)
	
	DIST_INCH_CM = 2.54
	DIST_INCH_MM = 25.4
	DIST_FEET_INCH = 12
	
	TEMP_F_COF1 = 32.0
	TEMP_F_COF2 = 1.8
	
	#1 watt = 3.41442595 Btu (th)/hour
	POWR_W_BTUH = 3.41442595 #3.413
	
	VOLU_FT_LITER = 28.317 
	
	# 1 watt/m K = 0.048174 Btu(th) /hr.inch Deg-F
	COND_WattM_BtuThHrInchF = 0.048174 
	
	# 1 cm watt/square meter/K = 0.06938111789 Btu (th) inch/hour/square foot/DEG_F
	COND_WATTCM_BTU_INCH = 0.069380
	
	#1 Btu (th) foot/hour/square foot/DEG_F = 1.729577206 watt/meter/K	Conductivity
	#1 watt/meter/K = 1/1.729577206 Btu (th) foot/hour/square foot/DEG_F
	COND_WATTM_BTU_FOOT = 1/1.729577206 #1/1.7307

	#-----------------------------------------------
	@staticmethod	
	def WattM_BtuThHInchF (Watt):
		return Watt * Unit.COND_WattM_BtuThHrInchF

	@staticmethod	
	def BtuThHInchF_WattM (BtuH):
		return BtuH / Unit.COND_WattM_BtuThHrInchF	
	
	#-----------------------------------------------
	@staticmethod	
	def Watt_BtuH (Watt):
		return Watt * Unit.POWR_W_BTUH

	@staticmethod	
	def BtuH_Watt (BtuH):
		return BtuH / Unit.POWR_W_BTUH	
	
	#-----------------------------------------------
	@staticmethod	
	def f_c (temp_F):
		return (temp_F - Unit.TEMP_F_COF1 ) / Unit.TEMP_F_COF2

	@staticmethod	
	def c_f (temp_C):
		return Unit.TEMP_F_COF2 * temp_C + Unit.TEMP_F_COF1	
	#-----------------------------------------------
	@staticmethod	
	def ft3_liter (ft3):
		return  ft3 * Unit.VOLU_FT_LITER 
	
	@staticmethod	
	def liter_ft3 (liter):
		return liter /Unit.VOLU_FT_LITER 
	#-----------------------------------------------
	
	#.......Distrance 
	@staticmethod
	def feet_mm (dist_feet):
		return dist_feet * Unit.DIST_INCH_MM * Unit.DIST_FEET_INCH
		
	@staticmethod
	def mm_feet (dist_mm):
		return dist_mm / Unit.DIST_INCH_MM / Unit.DIST_FEET_INCH

	#-----------------------------------------------	
	@staticmethod
	def inch_mm (dist_inch):
		return dist_inch * Unit.DIST_INCH_MM
		
	@staticmethod
	def mm_inch (dist_mm):
		return dist_mm / Unit.DIST_INCH_MM

	#-----------------------------------------------
	@staticmethod
	def inch_feet (dist_inch):
		return dist_inch / Unit.DIST_FEET_INCH
		
	@staticmethod
	def feet_inch (dist_feet):
		return dist_feet * Unit.DIST_FEET_INCH

	#-----------------------------------------------
	@staticmethod
	def inch_cm (dist_inch):
		return dist_inch * Unit.DIST_INCH_CM
		
	@staticmethod
	def cm_inch (dist_cm):
		return dist_cm / Unit.DIST_INCH_CM
	#-----------------------------------------------
	@staticmethod
	def cm_feet (dist_cm):
		return dist_cm / Unit.DIST_INCH_CM / Unit.DIST_FEET_INCH

	@staticmethod
	def feet_cm (dist_feet):
		return dist_feet * Unit.DIST_INCH_CM * Unit.DIST_FEET_INCH
	
	#.......Cond
		# 1 watt/meter/K = 6.938111789 Btu (th) inch/hour/square foot/DEG_F
		# 1 cm watt/square meter/K = 0.06938111789 Btu (th) inch/hour/square foot/DEG_F	
	
	#-----------------------------------------------
	@staticmethod	
	def CmWattF2K_BtuInchHrF2F (cond_watt):
		'''
		input : cm watt/square meter/K 		Btu_inch_hr_f2_F__cm_watt_f2_K
		output: Btu (th) inch/hour/square foot/DEG_F or (BTU-INCH/HR-FT2-DEG F)
		'''
		return  cond_watt * Unit.COND_WATTCM_BTU_INCH
		
	@staticmethod
	def BtuInchHrF2F_CmWattF2K (cond_btu):
		'''
		input : Btu (th) inch/hour/square foot/DEG_F or (BTU-INCH/HR-FT2-DEG F)
		output: cm watt/square meter/K
		'''	
		return  cond_btu / Unit.COND_WATTCM_BTU_INCH
	#-----------------------------------------------
	@staticmethod
	def CmWattF2K_BtuHrFtF (cond_watt):
		'''
		input : cm watt/square meter/K or (CM.W/m2-DEG_C) CmWattF2K
		output: (BTU/HR-FT-DEG F)/12   BtuHrFtF 
		Btu (th) inch/hour/square foot/DEG_F /12 = Btu (th) /hour/ foot/DEG_F/12 or
		conductivity to resistivity example:  resistivity = 1/ watt_btuInch(cond_watt)
							resistivity unit = hr foot DEG_F/ Btu (th).
		Unit.COND_WATTCM_BTU_INCH / Unit.DIST_FEET_INCH  =  0.069380 / 12
		'''
		return  cond_watt * Unit.COND_WATTCM_BTU_INCH / Unit.DIST_FEET_INCH 
	@staticmethod
	def BtuHrFtF_CmWattF2K (cond_btu):
		'''
		input: Btu (th) inch/hour/square foot/DEG_F /12 = Btu (th) /hour/ foot/DEG_F/12 or (BTU/HR-FT-DEG F)/12		
		output : cm watt/square meter/K or (CM.W/m2-DEG_C)
		Unit.DIST_FEET_INCH / Unit.COND_WATTCM_BTU_INCH  = 12/0.069380
		'''
		return  cond_btu * Unit.DIST_FEET_INCH / Unit.COND_WATTCM_BTU_INCH 
	#-----------------------------------------------
	@staticmethod
	def WattMK_BtuHrFtF (cond_watt):	
		'''
		input : watt/meter/K or (W/m-DEG_C) WattMK
		output: Btu (th) /hour/foot/DEG_F or (BTU/HR-FT-DEG F) BtuHrFtF
		'''
		return  cond_watt * Unit.COND_WATTM_BTU_FOOT
	
	@staticmethod
	def BtuHrFtF_WattMK (cond_btu):
		'''
		input: Btu (th) /hour/foot/DEG_F or (BTU/HR-FT-DEG F)
		output : watt/meter/K or (W/m-DEG_C)
		'''
		return  cond_btu / Unit.COND_WATTM_BTU_FOOT
	#-----------------------------------------------
	@staticmethod
	def WattMK_BtuHrInchF (cond_watt):	
		'''
		input : watt/meter/K or (W/m-DEG_C) WattMK
		output: Btu (th) /hour/foot/DEG_F or (BTU/HR-IN-DEG F) BtuHrInchF
		Unit.COND_WATTM_BTU_FOOT/Unit.DIST_FEET_INCH = 0.04818133185627408
		'''
		return  cond_watt * Unit.COND_WATTM_BTU_FOOT/Unit.DIST_FEET_INCH

	@staticmethod
	def BtuHrInchF_WattMK (cond_btu):	
		'''
		input: Btu (th) /hour/foot/DEG_F or (BTU/HR-IN-DEG F) 
		output : watt/meter/K or (W/m-DEG_C)
		Unit.DIST_FEET_INCH  / Unit.COND_WATTM_BTU_FOOT = 20.75492
		'''
		return  cond_btu * Unit.DIST_FEET_INCH  / Unit.COND_WATTM_BTU_FOOT 
	#-----------------------------------------------




	
