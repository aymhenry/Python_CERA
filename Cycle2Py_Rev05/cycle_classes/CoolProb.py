# Python import
from CoolProp.CoolProp import PhaseSI, PropsSI, get_global_param_string

# User import


class CoolProb:
	# Class Startic vars
	# Error internal code
	ERR_NOT_FOUND = 0	# no error code
	ERR_FUILD_NOT_FOUND = 10   	# some error found
		
	def setup (self, strFluid ):
		# member varbiable
		self.m_error =  CoolProb.ERR_FUILD_NOT_FOUND
		self.m_error_desc =  ""
		self.m_fluid = None
		
		lstFluid = get_global_param_string("FluidsList").split(',')
		
		#CP.PropsSI('D','T',300,'P',101325,'HEOS::R32[0.697615]&R125[0.302385]')
		if strFluid in lstFluid:
			self.m_fluid = strFluid
			self.m_error = CoolProb.ERR_NOT_FOUND
		else:
			self.m_error_desc = strFluid
	
	#-----------------------------------------------------------
	# Job 			: Check if there are an error
	# 		
	# Input 		: 
	# Output		: True if error, else false
	#-----------------------------------------------------------
	def isError (self):
		# return true if error, else return false
		return self.m_error != CoolProb.ERR_NOT_FOUND 

	#-----------------------------------------------------------
	# Job 			: descript the error occured if any
	# Input 		:
	#
	# Output		: text of error number and error description
	#-----------------------------------------------------------
	def err_description (self):
		if self.m_error == CoolProb.ERR_FUILD_NOT_FOUND :
			return "Err " + str(CoolProb.ERR_FUILD_NOT_FOUND) + " Fuild is not supported: " + str(self.m_error_desc)
		
		elif self.m_error == CoolProb.ERR_NOT_FOUND :
			return "No error."
		else:
			return "No error description, info. number CoolProb:" + str(self.m_error)

	#-----------------------------------------------------------
	# Job 			: Get Used Fuild
	# Input 		:
	#
	# Output		: Name of used fuild
	#-----------------------------------------------------------
	def getFuild (self):
		return self.m_fluid
		
	#-----------------------------------------------------------
	# Job 			: Get Critical Temp
	# Input 		:
	#
	# Output		: Critical Temp in K
	#-----------------------------------------------------------
	def getCrtiticalTemp (self):
		if self.isError() : return None
		return PropsSI(self.m_fluid, "Tcrit")

	#-----------------------------------------------------------
	# Job 			: Get Critical Press
	# Input 		: 
	#
	# Output		: Critical Pressure in Pa
	#-----------------------------------------------------------
	def getCrtiticalPress (self):
		if self.isError() : return None
		return PropsSI(self.m_fluid, "Pcrit")

	#-----------------------------------------------------------
	# Job 			: Get Critical Press
	# Input 		: 
	#
	# Output		: Critical Volume in kg/m3
	#-----------------------------------------------------------
	def getCrtiticalVolume (self):
		if self.isError() : return None
		return PropsSI(self.m_fluid, "rhocrit")
		
	#-----------------------------------------------------------
	# Job 			: Get Saturated temperature
	# Input 		: fltPressure pressure in Pascal
	# 
	# Output		: Saturated Temp in K
	#-----------------------------------------------------------
	def getSatTemp_byPress (self, flt_P_Pascal ):
		if self.isError() : return None
		return PropsSI("T", "P", flt_P_Pascal, "Q", 0, self.m_fluid)

	#-----------------------------------------------------------
	# Job 			: Get Saturated pressure
	# Input 		: flt_Temp_K temperature in K
	# 
	# Output		: Saturated pressure in Pa
	#-----------------------------------------------------------
	def getSatPress_byTemp (self, flt_Temp_K ):
		if self.isError() : return None
		return PropsSI("P", "T", flt_Temp_K, "Q", 0, self.m_fluid)


	#-----------------------------------------------------------
	# Job 			: Check if liquied or gas phase
	# Input 		: fltPressure pressure in Pascal, temperature in K
	# 
	# Output		: True if liquid
	#-----------------------------------------------------------
	def isLiquidPhase_byPressTemp (self, flt_P_Pascal, flt_Temp_K ):
		if self.isError() : return None

		str_phase  =  PhaseSI("P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid) 
		#str_phase =  PropsSI("P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid)
		return True if str_phase =='liquid' else False
		
	#-----------------------------------------------------------
	# Job 			: get Cp
	# Input 		: fltPressure pressure in Pascal, temperature in K
	# 
	# Output		: Cp in J/kg/K
	#-----------------------------------------------------------
	def getCp_byPressTemp (self, flt_P_Pascal, flt_Temp_K ):
		if self.isError() : return None

		return PropsSI("CPMASS", "P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid)

	#-----------------------------------------------------------
	# Job 			: get Cv
	# Input 		: fltPressure pressure in Pascal, temperature in K
	# 
	# Output		: Cv in J/kg/K
	#-----------------------------------------------------------
	def getCv_byPressTemp (self, flt_P_Pascal, flt_Temp_K ):
		if self.isError() : return None

		return PropsSI("CVMASS", "P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid)
		
	#-----------------------------------------------------------
	# Job 			: get Enthalpy
	# Input 		: fltPressure pressure in Pascal, temperature in K
	# 
	# Output		: Enthalpy in J/kg
	#-----------------------------------------------------------
	def getH_byPressTemp (self, flt_P_Pascal, flt_Temp_K ):
		if self.isError() : return None

		return PropsSI("H", "P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid)

	#-----------------------------------------------------------
	# Job 			: get Enthalpy
	# Input 		: fltPressure pressure in Pascal, flt_Vol_m3kg in m3/kg
	# 
	# Output		: Enthalpy in J/kg
	#-----------------------------------------------------------
	def getH_byPressVol (self, flt_P_Pascal, flt_Vol_m3kg ):
		if self.isError() : return None
		if flt_Vol_m3kg ==0 : return None

		return PropsSI("H", "P", flt_P_Pascal, "D", 1/flt_Vol_m3kg, self.m_fluid)

	#-----------------------------------------------------------
	# Job 			: get Cp
	# Input 		: fltPressure pressure in Pascal, flt_Vol_m3kg in m3/kg
	# 
	# Output		: Cp in J/kg K
	#-----------------------------------------------------------
	def getCp_byPressVol (self, flt_P_Pascal, flt_Vol_m3kg ):
		if self.isError() : return None
		if flt_Vol_m3kg ==0 : return None

		return PropsSI("CPMASS", "P", flt_P_Pascal, "D", 1/flt_Vol_m3kg, self.m_fluid)
		
	#-----------------------------------------------------------
	# Job 			: get Cv
	# Input 		: fltPressure pressure in Pascal, flt_Vol_m3kg in m3/kg
	# 
	# Output		: Cv in J/kg K
	#-----------------------------------------------------------
	def getCv_byPressVol (self, flt_P_Pascal, flt_Vol_m3kg ):
		if self.isError() : return None
		if flt_Vol_m3kg ==0 : return None

		return PropsSI("CVMASS", "P", flt_P_Pascal, "D", 1/flt_Vol_m3kg, self.m_fluid)
	#-----------------------------------------------------------
	# Job 			: get Enthalpy
	# Input 		: flt_Temp_K temperature in K, flt_Vol_m3kg in m3/kg
	# 
	# Output		: Enthalpy in J/kg
	#-----------------------------------------------------------
	def getH_byTempVol (self, flt_Temp_K, flt_Vol_m3kg ):
		if self.isError() : return None
		if flt_Vol_m3kg ==0 : return None

		return PropsSI("H", "T", flt_Temp_K, "D", 1/flt_Vol_m3kg, self.m_fluid)



	#-----------------------------------------------------------
	# Job 			: get Entory
	# Input 		: fltPressure pressure in Pascal, temperature in K
	# 
	# Output		: Enthalpy in J/kg/K
	#-----------------------------------------------------------
	def getS_byPressTemp (self, flt_P_Pascal, flt_Temp_K ):
		if self.isError() : return None

		return PropsSI("S", "P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid)

	#-----------------------------------------------------------
	# Job 			: get Entory
	# Input 		: flt_Temp_K Temperature in K, volume in m3/kg
	# 
	# Output		: Enthalpy in J/kg/K
	#-----------------------------------------------------------
	def getS_byTempVol (self, flt_Temp_K, flt_Vol_m3kg ):
		if self.isError() : return None
		if flt_Vol_m3kg ==0 : return None
				
		return PropsSI("S", "D", 1/flt_Vol_m3kg, "T", flt_Temp_K, self.m_fluid)

	#-----------------------------------------------------------
	# Job 			: get Temperature
	# Input 		: Entory flt_S_jkg_K in j/kg.K, pressure flt_P_Pascal in Pa
	# 
	# Output		: Temperature in K
	#-----------------------------------------------------------
	def getT_byEntrpPress (self, flt_S_jkg_K, flt_P_Pascal ):
		if self.isError() : return None
				
		return PropsSI("T", "S", flt_S_jkg_K, "P", flt_P_Pascal, self.m_fluid)
		
	#-----------------------------------------------------------
	# Job 			: get Volume 
	# Input 		: fltPressure pressure in Pascal, temperature in K
	# 
	# Output		: Volume in ,m3/kg
	#-----------------------------------------------------------
	def getV_byPressTemp (self, flt_P_Pascal, flt_Temp_K ):
		if self.isError() : return None

		return 1/self.getD_byPressTemp(flt_P_Pascal, flt_Temp_K)

	#-----------------------------------------------------------
	# Job 			: get Densisty 
	# Input 		: fltPressure pressure in Pascal, temperature in K
	# 
	# Output		: Densisty in ,kg/m3
	#-----------------------------------------------------------
	def getD_byPressTemp (self, flt_P_Pascal, flt_Temp_K ):
		if self.isError() : return None

		return PropsSI("D", "P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid)

	#-----------------------------------------------------------
	# Job 			: get Saturated Volume 
	# Input 		: Temperature in K
	# 
	# Output		: Volume in ,m3/kg
	#-----------------------------------------------------------
	def getSatV_byTemp (self, flt_Temp_K, isGas = None ):
		if self.isError() : return None
		
		return 1/self.getSatD_byTemp ( flt_Temp_K, isGas )
	#-----------------------------------------------------------
	# Job 			: get Saturated Densisty 
	# Input 		: Temperature in K
	# 
	# Output		: Densisty in ,kg/m3
	#-----------------------------------------------------------
	def getSatD_byTemp (self, flt_Temp_K, isGas = None ):
		if self.isError() : return None
		inGasPhase = 1 if isGas else 0

		return PropsSI("D", "T", flt_Temp_K, "Q", inGasPhase,  self.m_fluid)

	#-----------------------------------------------------------
	# Job 			: get Saturated Volume 
	# Input 		: fltPressure pressure in Pascal
	# 
	# Output		: Volume in ,m3/kg
	#-----------------------------------------------------------
	def getSatV_byPress (self, fltPressure, isGas = None ):
		if self.isError() : return None
		
		return 1/self.getSatD_byPress ( fltPressure, isGas )
	#-----------------------------------------------------------
	# Job 			: get Saturated Densisty 
	# Input 		: fltPressure pressure in Pascal
	# 
	# Output		: Densisty in ,kg/m3
	#-----------------------------------------------------------
	def getSatD_byPress (self, fltPressure, isGas = None ):
		if self.isError() : return None
		inGasPhase = 1 if isGas else 0

		return PropsSI("D", "P", fltPressure, "Q", inGasPhase,  self.m_fluid)
#=====================
def main ():
	objCP = CoolProb()
	objCP.setup ("R12")
	
	if objCP.isError ():
		print (objCP.err_description() )
		return
	
	TK_C= 273.15 # K
	T1= TK_C + 72  # K
	P1= 200000 # Pa
	# H = 400061.6198132278	# J/kg
	S = 1741.3033288796132 #J/kg/K
	# D = 8.632966162031755  #kg/m3
	V = 0.11583504223589493 #m3/kg	

	
	print ("Fuild: " , objCP.getFuild() )
	print ("----------------------------------------")
	print ("Critical temperature: ", objCP.getCrtiticalTemp(), "K" )
	print ("Critical Pressure: ", objCP.getCrtiticalPress(), "Pa" )
	print ("Critical Volume: ", objCP.getCrtiticalVolume(), "kg/m3" )
	
	print ("----------------------------------------")
	print ("Saturated temperature for P = 1006800 Pa: (confirmed)", objCP.getSatTemp_byPress(1006800), "K" )
	print ("Saturated Pressue for T =  315.15 Pa: (confirmed)", objCP.getSatPress_byTemp( 315.14934314624594), "Pa" )
	print ("Phase at T=373 K, P = 1013250 Pa: (confirmed)", objCP.isLiquidPhase_byPressTemp(1013250, TK_C+100) )

	print ("H (liquid) at T=273 K, P = 308150 Pa: (confirmed)", objCP.getH_byPressTemp(308150, TK_C), 'J/kg/K' )

	print ("----------------------------------------")	
	print ("Cp  at T=273 K, P = 308150 Pa: (check)", objCP.getCp_byPressTemp(308150, TK_C), 'J/kg/K' )
	print ("Cv  at T=273 K, P = 308150 Pa: (check)", objCP.getCv_byPressTemp(308150, TK_C), 'J/kg/K' )

	print ("Cp  (check)", objCP.getCp_byPressVol(P1, V), 'J/kg/K' )
	print ("Cv  (check)", objCP.getCv_byPressVol(P1, V), 'J/kg/K' )
	
	print ("========================================")
	
	
	print ("Given P1=", P1, "  T1=",T1, "V=", V)
	print ("S   getS_byPressTemp ", objCP.getS_byPressTemp(P1, T1), 'J/kg/K' )
	print ("S   getS_byTempVol   ", objCP.getS_byTempVol(T1, V), 'J/kg/K' )
	print ("V       getV_byPressTemp ", objCP.getV_byPressTemp(P1, T1), 'm3/kg' )
	print ("Den     getD_byPressTemp ", objCP.getD_byPressTemp(P1, T1), 'kg/m3' )
	print ("T     getT_byEntrpPress ", objCP.getT_byEntrpPress(S, P1), 'K' )
	
	print ("----------------------------------------")
	print ("H (gas) getH_byPressTemp ", objCP.getH_byPressTemp(P1, T1), 'J/kg' )
	print ("H       getH_byPressVol  ", objCP.getH_byPressVol(P1, V), 'J/kg' )
	print ("H       getH_byTempVol  ", objCP.getH_byTempVol(T1, V), 'J/kg' )
		
	print ("========================================")
	print ("---Saturated condition")
	T1= TK_C + 18  # K
	P1= 535130 # Pa
	print ("Given P1=", P1, "  T1=",T1)
	print ("V   (liquid) getSatV_byTemp ", objCP.getSatV_byTemp(T1), 'm3/kg' )
	print ("Den (liquid) getSatD_byTemp ", objCP.getSatD_byTemp(T1), 'kg/m3' )
	print ("V   (gas)    getSatV_byTemp ", objCP.getSatV_byTemp(T1, True), 'm3/kg')
	print ("Den (gas)    getSatD_byTemp ", objCP.getSatD_byTemp(T1, True), 'kg/m3')	
	
	print ("----------------------------------------")
	print ("V   (liquid) getSatV_byPress ", objCP.getSatV_byPress(P1), 'm3/kg' )
	print ("Den (liquid) getSatD_byPress ", objCP.getSatD_byPress(P1), 'kg/m3' )
	print ("V   (gas)    getSatV_byPress ", objCP.getSatV_byPress(P1, True), 'm3/kg')
	print ("Den (gas)    getSatD_byPress ", objCP.getSatD_byPress(P1, True), 'kg/m3')	
#=====================
if __name__	== '__main__':
	main()
	