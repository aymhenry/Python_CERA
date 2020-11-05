# Python import
from CoolProp.CoolProp import PhaseSI, PropsSI, get_global_param_string

# User import


class CoolProb:
	# Class Startic vars
	# Error internal code
	ERR_NOT_FOUND = 0	# no error code
	ERR_FUILD_NOT_FOUND = 10   	# some error found
	ERR_PROB_NOT_FOUND =  500   # Property not supported
	ERR_PROB_ERROR =  510   # Property not supported
		
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
		
		elif self.m_error == CoolProb.ERR_PROB_ERROR :
			return "Err " + str(CoolProb.ERR_PROB_ERROR) + " Call Prop error, error in parameters " + str(self.m_error_desc)
			
		elif self.m_error == CoolProb.ERR_PROB_NOT_FOUND :
			return "Err " + str(CoolProb.ERR_PROB_NOT_FOUND) + " Property not supported: " + str(self.m_error_desc)
			
		elif self.m_error == CoolProb.ERR_NOT_FOUND :
			return "No error."
		else:
			return "No error description, info. number CoolProb:" + str(self.m_error)

	#-----------------------------------------------------------
	# Job 			: get of of the properties 'P','T','V','V','H','S','CP','CV'
	# Input 		: getProb code of required property, referance properties
	#
	# Output		: text of error number and error description
	#-----------------------------------------------------------
	def Property (self, getProb, P = None, T = None, V = None, D=None,\
					H = None, S = None, Q = None ):
		MAX_PARA = 2
		lst_prob_io = ['P','T','V','D','H','S'] # probs for input/output
		lst_prob_o = ['CP','CV']  # probs for output only
		lst_prob_i = ['Q']  # probs for input only
		
		getProb_adj = getProb.upper()
		
		if getProb_adj not in lst_prob_io + lst_prob_o:
			self.m_error = CoolProb.ERR_PROB_NOT_FOUND
			self.m_error_desc = "Property: " + getProb
			return
		
		# adjust id for Cp and Cv
		if getProb_adj in lst_prob_o:
			getProb_adj = getProb_adj + "MASS"

		# adjust V (volume) - coolprob use D for dencity
		if getProb == 'V':
			getProb_adj = "D"
			
		#get two referance parameters
		int_count = 0
		
		try:		
			str_command = "PropsSI(" + "'" + getProb_adj  + "'"
			for prob in lst_prob_io + lst_prob_i:
				if eval (prob + " !=None"):
					int_count = int_count +1
					
					if prob == 'V': # CoolProb use D for dencity
						prob_code = "D"
						prob_val = 1/eval(prob)
					else:
						prob_code = prob
						prob_val = eval(prob)
					
					str_command = str_command + ", " + "'" + prob_code + "'" + ", " + str(prob_val)
					
					if int_count >=MAX_PARA: break # only limited number of parameters
			
			str_command = str_command + ', self.m_fluid)'

			result = eval( str_command)
			if getProb == 'V':
				result = 1/result
				
			return result
		except:
			self.m_error = CoolProb.ERR_PROB_ERROR
			self.m_error_desc = str_command
			return None
		
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
	# Job 			: Check if liquied or gas phase
	# Input 		: fltPressure pressure in Pascal, temperature in K
	# 
	# Output		: True if liquid
	#-----------------------------------------------------------
	def isLiquidPhase_byPressTemp (self, flt_P_Pascal, flt_Temp_K ):
		if self.isError() : return None

		str_phase  =  PhaseSI("P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid) 
		#str_phase =  PropsSI("P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid)
		return str_phase 
		

		
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
	print ("Critical Pressure:    ", objCP.getCrtiticalPress(), "Pa" )
	print ("Critical Volume:      ", objCP.getCrtiticalVolume(), "kg/m3" )
	
	print ("----------------------------------------")
	print ("---Saturated condition")
	T1= TK_C + 18  # K
	P1= 535130 # Pa
	print ("Given P1=", P1, "  T1=",T1)
	print ("----------------------------------------")
	print ("V-liq  by T ", objCP.Property('V', T=T1, Q=0), 'kg/m3' )
	print ("V-gas  by T ", objCP.Property('V', T=T1, Q=1), 'm3/kg' )
	print ("D-liq  by T ", objCP.Property('D', T=T1, Q=0), 'kg/m3' )
	print ("D-gas  by T ", objCP.Property('D', T=T1, Q=1), 'm3/kg' )	
	
	print ("----------------------------------------")
	print ("V-liq  by P ", objCP.Property('V', P=P1, Q=0), 'm3/kg' )
	print ("D-liq  by P ", objCP.Property('D', P=P1, Q=0), 'kg/m3' )
	print ("V-liq  by P ", objCP.Property('V', P=P1, Q=1), 'm3/kg' )
	print ("D-liq  by P ", objCP.Property('D', P=P1, Q=1), 'kg/m3' )
	
	print ("----------------------------------------")
	Tsat = 315.14934314624594
	print ("Sat-Temp for P = 1006800 Pa by P Q=0", objCP.Property('T', P=1006800, Q=0), 'K' )
	print ("Sat-Temp for P = 1006800 Pa by P Q=1", objCP.Property('T', P=1006800, Q=1), 'K' )

	print ("Sat-Temp for T = 315.15 Pa by P Q=0", objCP.Property('P', T=Tsat, Q=0), 'Pa' )
	print ("Sat-Temp for T = 315.15 Pa by P Q=1", objCP.Property('P', T=Tsat, Q=1), 'Pa' )
	
	print ("Phase at T=373 K, P = 1013250 Pa: (confirmed)", objCP.isLiquidPhase_byPressTemp(1013250, TK_C+100) )
	
	print ("========================================")
	print ("---others=-=-=-=-=")
	print ("H (liquid) at T=273 K, P = 308150 Pa: by P, T", objCP.Property('h', P=308150, T=TK_C), 'J/kg/K' )

	print ("----------------------------------------")	
	print ("Cp  at T=273 K, P = 308150 Pa: by P,T",objCP.Property('cp', P=308150, T=TK_C), 'J/kg/K' )
	print ("Cp  at T=273 K, P = 308150 Pa: by P,T",objCP.Property('cv', P=308150, T=TK_C), 'J/kg/K' )
	
	print ("Cp  by P, V", objCP.Property('cp', P=P1, V=V), 'J/kg/K' )
	
	print ("Cp  by P, V", objCP.Property('cv', P=P1, V=V), 'J/kg/K' )

	
	print ("----------------------------------------")	
	print ("Given P1=", P1, "  T1=",T1, "V=", V)
	print ("S   by P, T ", objCP.Property("S", P=P1, T=T1), 'J/kg/K' )

	print ("S   by T, V ", objCP.Property("S", T=T1, V=V), 'J/kg/K' )

	
	print ("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
	print ("V       Press,Temp ", objCP.Property("V", P=P1, T=T1), 'J/kg/K' )
	print ("Den     Press,Temp ", objCP.Property("D", P=P1, T=T1), 'kg/m3' )
	print ("T          by S, P ", objCP.Property("T", S=S, P=P1), 'K' )
	
	print ("----------------------------------------")
	print ("H (gas) by P,T ", objCP.Property("h", P=P1, T=T1), 'J/kg' )
	print ("H       by Press, Vol  ", objCP.Property("h", P=P1, V=V), 'J/kg' )
	print ("H       by T, V  ", objCP.Property("h", T=T1, V=V), 'J/kg' )
		
	print ("========================================")

#=====================
if __name__	== '__main__':
	main()
	print ("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
	objCP = CoolProb()
	objCP.setup ("R12")
	
	print (objCP.Property ("V", P=101325, T=300, V=.02))
	
	