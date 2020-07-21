# Python Import
import math

# User Import

from .CabUtils import CabUtils
#-----------------------------------------------------------
# Job 			:Calculates the Latent and Sensible Loads for Door Openings 
#
# Editor		: aymhenry@gmail.com
#-----------------------------------------------------------

class DoorOpen(CabUtils):
	#------- Internal Classs builder
	class DoorpnBuilder:
		# set copy of all vars as in basic class
		def __init__ (self):
			self.par_TSETFF = None
			self.par_TSETFZ = None
			self.par_HFFC = None
			self.par_WFFC = None
			self.par_DFFC = None
			self.par_HFRZ = None
			self.par_WFRZ = None
			self.par_DFRZ = None

			self.com_NMOD = None
			self.com_VOLAZ = None
			self.com_VOLAR = None
			self.com_RELHUM = None
			self.com_TDRAIR = None
			self.com_FFCOPN = None
					
			self.com_WATERF = None
			self.com_WATERZ = None
			self.com_FRZOPN = None
			self.com_HRFFC = None
			self.com_HRFRZ = None
		#----------------------------------
		
		# builder method, transfer vars to main class and return the main object class
		def build (self):
			obj_doorpn = DoorOpen ()
			
			obj_doorpn.par_TSETFF = self.par_TSETFF
			obj_doorpn.par_TSETFZ = self.par_TSETFZ
			
			obj_doorpn.par_HFFC = self.par_HFFC
			obj_doorpn.par_WFFC = self.par_WFFC
			obj_doorpn.par_DFFC = self.par_DFFC
			
			obj_doorpn.par_HFRZ = self.par_HFRZ
			obj_doorpn.par_WFRZ = self.par_WFRZ
			obj_doorpn.par_DFRZ = self.par_DFRZ

			obj_doorpn.com_NMOD = self.com_NMOD
			obj_doorpn.com_VOLAZ = self.com_VOLAZ
			obj_doorpn.com_VOLAR = self.com_VOLAR
			
			obj_doorpn.com_RELHUM = self.com_RELHUM
			obj_doorpn.com_TDRAIR = self.com_TDRAIR
			
			obj_doorpn.com_FFCOPN = self.com_FFCOPN
			obj_doorpn.com_WATERF = self.com_WATERF
			obj_doorpn.com_WATERZ = self.com_WATERZ
			
			obj_doorpn.com_FRZOPN = self.com_FRZOPN
			obj_doorpn.com_HRFFC = self.com_HRFFC
			obj_doorpn.com_HRFRZ = self.com_HRFRZ
			return obj_doorpn
		#----------------------------------
		# for every var one method, method name start (withMy_method_name)
		def withTempFFSetpoint (self, par_TSETFF):
			'''
			Fresh Food Compartment Set Point (Deg F)
			'''
			self.par_TSETFF = par_TSETFF
			return self
			
		def withTempFZSetpoint (self, par_TSETFZ):
			'''
			Freezer Compartment Set Point (Deg F)
			'''
			self.par_TSETFZ = par_TSETFZ
			return self
			
		def withHeighFF (self, par_HFFC): 
			'''
			Internal Fresh Food Height (Inches)
			'''
			self.par_HFFC = par_HFFC
			return self
			
		def withWidthFF (self, par_WFFC):
			'''
			Internal Fresh Food Width (Inches)
			'''
			self.par_WFFC = par_WFFC
			return self

		def withDepthFF (self, par_DFFC):
			'''
			Internal Fresh Food Depth (Inches)
			'''
			self.par_DFFC = par_DFFC
			return self
			
		def withHeighFZ (self, par_HFRZ):
			'''
			Internal Freezer Height (Inches)
			'''
			self.par_HFRZ = par_HFRZ
			return self
			
		def withWidthFZ (self, par_WFRZ):
			'''
			Internal Freezer Width (Inches)
			'''
			self.par_WFRZ = par_WFRZ
			return self

		def withDepthFZ (self, par_DFRZ):
			'''
			Internal Freezer Depth (Inches)
			'''
			self.par_DFRZ = par_DFRZ
			return self
			
		def withMode (self, com_NMOD):
			'''
			internal number of the configuration to be run, Mode is 2,3,4,5,7 or 8 
			'''
			self.com_NMOD = com_NMOD
			return self
			
		def withVolumeFZ (self, com_VOLAZ):
			'''
			Adjusted freezer volume (FT3)
			'''
			self.com_VOLAZ = com_VOLAZ
			return self

		def withVolumeFF (self, com_VOLAR):
			'''
			Adjusted general refrigerated volume (FT3)
			'''
			self.com_VOLAR = com_VOLAR
			return self
		
		def withRelHumidity (self, RELHUM):
			'''
			Relative Humidity from 0 to 100
			'''
			self.com_RELHUM = RELHUM
			return self
			
		def withTempAirAmbient (self, com_TDRAIR):
			'''
			Ambient Air Temperature (Deg F)
			'''
			self.com_TDRAIR = com_TDRAIR
			return self

		def withOpenHrFF (self, com_FFCOPN):
			'''
			Number of door openings/hr for Fresh Food (this and nos below are the average per hour)
			'''
			self.com_FFCOPN = com_FFCOPN
			return self
			
		def withWaterFF (self, com_WATERF):
			self.com_WATERF = com_WATERF
			return self

		def withWaterFZ (self, com_WATERZ):
			self.com_WATERZ = com_WATERZ
			return self			
		#------------------------------------
		def withOpenHrFZ (self, com_FRZOPN):
			'''
			Freezer food compartment Openings/hr
			'''
			self.com_FRZOPN = com_FRZOPN
			return self
			
		def withHrOpenFF (self, com_HRFFC):
			'''
			Fresh food compartment Openings duration hr
			'''
			self.com_HRFFC = com_HRFFC
			return self

		def withHrOpenFZ (self, com_HRFRZ):
			'''
			Freezer compartment Openings duration hr
			'''
			self.com_HRFRZ = com_HRFRZ
			return self			
		
	#------------------------------
	# define vars in Compressor, also in builder class
	def __init__(self):
		self.par_TSETFF = None
		self.par_TSETFZ = None
		self.par_HFFC = None
		self.par_WFFC = None
		self.par_DFFC = None
		self.par_HFRZ = None
		self.par_WFRZ = None
		self.par_DFRZ = None

		self.com_NMOD = None
		self.com_VOLAZ = None
		self.com_VOLAR = None
		self.com_RELHUM = None
		self.com_TDRAIR = None
		self.com_FFCOPN = None
				
		self.com_WATERF = None
		self.com_WATERZ = None
		self.com_FRZOPN = None
		self.com_HRFFC = None
		self.com_HRFRZ = None
	#---------------------------------------
	def main(self):
		'''
		#par_TSETFF		Fresh Food Compartment Set Point (Deg F)
		#par_TSETFZ		Freezer Compartment Set Point (Deg F)
		
		#self.par_HFFC		Internal Fresh Food Height (Inches)		
		#par_WFFC		Internal Fresh Food Width (Inches)
		#par_DFFC		Internal Fresh Food Depth (Inches)
		
		#par_HFRZ		Internal Freezer Height (Inches)
		#par_WFRZ		Internal Freezer Width (Inches)
		#par_DFRZ		Internal Freezer Depth (Inches)
		
		#self.com_NMOD		Relative humidity
		#self.com_VOLAZ		Adjusted freezer volume (FT3)
		#self.com_VOLAR		Adjusted general refrigerated volume ( cu. ft. )
		#com_RELHUM		Relative humidity
		#com_TDRAIR		Ambient Air Temperature (Deg F)
		#
	
		#com_FFCOPN		Number of door openings/hr for Fresh Food (this and nos below are the average per hour)
		#com_FRZOPN		Number of door openings/hr for Freezer
		#com_HRFFC		Hours/hr the Fresh Food door is open
		#com_HRFRZ		Hours/hr the Freezer door is open
		#com_WATERF
		#com_WATERZ
		'''
		#	Output: For the Fresh Food Compartment (FFC):
		#         com_RELHUM..... Relative Humidity (Deg F)		
		
		#   Sensible Load (BTU)
		#		res1_QDFFCS		Total sensible load in fresh food section assuming the moisture remains liquid
		#		res2_QDFFFS		Total sensible load in fresh food section assuming the moisture is frozen		
		#		res5_QDFFCL		Total latent load in fresh food section assuming the moisture remains liquid   

		#       res6_QDFFFL		Total latent load in fresh food section assuming the moisture is frozen
		# 		res3_QDFZCS		No Definition was given in FORTRAN Document
		# 		res4_QDFZFS		No Definition was given
		# 		res7_QDFZCL		No Definition was given
		# 		res8_QDFZFL		No Definition was given
		
		# The following vars is listed as output in FORTRAN, all are Local Var. inside Sub and is not output
		#   	loc_QLFFC		Immediate Door Opening Latent Heat Load (BTU)
		#       loc_QSFFC		Immediate Door Opening Sensible Heat Load (BTU)
		#       loc_QNVFFC		Time Dependant (Convective) Door Opening
		
		#   Latent Load (BTU)
		#       loc_QDFFC		Time Dependant (Convective) Door Opening For the Freezer Compartment (FRZ):
		#		loc_QNVFRZ		Time Dependant (Convective) Door Opening
		
		
		# Sensible Heat Transfer Coefficients (BTU/hr-ft2-F)
		
		# loc_HTRFFC - Fresh Food Compartment
		loc_HTRFFC = 0.76
		
		# loc_HTRFRZ - Freezer Compartment		
		loc_HTRFRZ = 0.99
		
		#  Mass Transfer Coefficients (Pounds/hr)
		
		#  loc_HMFFC - Fresh Food Compartment
		loc_HMFFC = 47.14
		
		#  loc_HMFRZ - Freezer Compartment		
		loc_HMFRZ = 61.41
		
		#  Volume of Freezer & Fresh Food Compartment (Ft3)
		if self.com_NMOD != 4:					# input from common
			loc_VOLFRZ = self.com_VOLAZ
			loc_VOLFFC = self.com_VOLAR
		else:
			loc_VOLFRZ = self.com_VOLAR
			loc_VOLFFC = self.com_VOLAZ			
			
		#  Area Fresh Food Compartment (Ft2)
		loc_AFFC = 2.0 * (self.par_HFFC * self.par_WFFC 	\
						+ self.par_HFFC * self.par_DFFC 	\
						+ self.par_WFFC * self.par_DFFC)
		
		#  Area of Freezer (Ft2)
		loc_AFRZ = 2.0 * (self.par_HFRZ * self.par_WFRZ 	\
						+ self.par_HFRZ * self.par_DFRZ 	\
						+ self.par_WFRZ * self.par_DFRZ)
		
		loc_TFFCAB = self.par_TSETFF		#  Set evaporator Temperatures set point
		loc_TFZCAB = self.par_TSETFZ 		#  Set Freezer Temperatures set point

		loc_TFFC  = 32.0
		loc_TFFF  = self.par_TSETFF - 15.0
		loc_TFRZE = self.par_TSETFZ - 15.0
		
		#  Enthalpy Changes for Fresh Food & Freezer
		loc_RHAIR = self.com_RELHUM/100.0		#Ambient air
		
		# Calculates the Humidity Ratio of Air 	
		loc_WAIR = self.getAirHumidity (self.com_TDRAIR, loc_RHAIR)			
		
		#Condensation in Fresh Food
		loc_HFFC = 1.0
		loc_WFFC = self.getAirHumidity(loc_TFFC, loc_HFFC)				#Calculates the Humidity Ratio of Air 
		
		#Freezing in Fresh Food
		loc_RHFFF = 1.0
		loc_WWFFF = self.getAirHumidity(loc_TFFF, loc_RHFFF)			#Calculates the Humidity Ratio of Air 	
		
		#Condensation in Freezer
		loc_RHFZC = 1.0
		loc_WWFZC = self.getAirHumidity(loc_TFFC, loc_RHFZC)			#Calculates the Humidity Ratio of Air 	
		
		#Freezing in Freezer
		loc_RHFZF = 1.0
		loc_WWFZF = self.getAirHumidity (loc_TFRZE, loc_RHFZF)			#Calculates the Humidity Ratio of Air 	

		#====== Enthalpy Change ===================================
		# Calculates the Enthalpy Change of Moist Air
		[loc_DUMS1, loc_DUML1, self.par_HFFCS, self.par_HFFCL, loc_DUMS2, loc_DUML2] 	\
			 = self.getAirMoistEnthalpy (self.com_TDRAIR, loc_WAIR, loc_TFFC, loc_WFFC, loc_TFFCAB )
		
		[loc_DUMS1, loc_DUML1, loc_DUMS2, loc_DUML2, loc_DHFFFS, loc_DHFFFL] 			\
			= self.getAirMoistEnthalpy  (self.com_TDRAIR, loc_WAIR, loc_TFFF, loc_WWFFF, loc_TFFCAB )
		
		[loc_DUMS1, loc_DUML1, loc_DHFZCS, loc_DHFZCL, loc_DUMS2, loc_DUML2] 			\
			= self.getAirMoistEnthalpy (self.com_TDRAIR, loc_WAIR, loc_TFFC, loc_WWFZC, loc_TFZCAB )
			
		[loc_DUMS1, loc_DUML1, loc_DUMS2, loc_DUML2, loc_DHFZFS, loc_DHFZFL] 		\
			= self.getAirMoistEnthalpy (self.com_TDRAIR, loc_WAIR, loc_TFRZE, loc_WWFZF, loc_TFZCAB )

		# Immediate Fresh Food Compartment Door Opening Loads
		loc_RHODRY = 0.0807 * 492.0 /(self.com_TDRAIR + 460.0 )

		#  Transient Latent Heat Transfer
		loc_RHOH2O = self.getSatWaterDensity(self.com_TDRAIR)
		
		loc_RHOAIR = loc_RHODRY + loc_RHOH2O

		#FF Condensation
		loc_QLFFC = self.com_FFCOPN * self.par_HFFCL * loc_VOLFFC * loc_RHOAIR + self.com_WATERF * (1061.0 + 0.444 *loc_TFFCAB)
		loc_QSFFC = self.com_FFCOPN * self.par_HFFCS * loc_VOLFFC * loc_RHOAIR

		#FF Freezing
		loc_QLFFF = self.com_FFCOPN * loc_DHFFFL * loc_VOLFFC * loc_RHOAIR + self.com_WATERF*((1061.0 + 0.444 * loc_TFFCAB) + (158.9 - 0.488 * loc_TFFF))
		loc_QSFFF = self.com_FFCOPN * loc_DHFFFS * loc_VOLFFC * loc_RHOAIR

		#FZ Condensation
		loc_QLFZC = self.com_FRZOPN * loc_DHFZCL * loc_VOLFRZ * loc_RHOAIR + self.com_WATERZ*(1061.0 + 0.444*loc_TFZCAB)
		loc_QSFZC = self.com_FRZOPN * loc_DHFZCS * loc_VOLFRZ * loc_RHOAIR

		#FZ Freezing
		loc_QLFZF = self.com_FRZOPN * loc_DHFZFL * loc_VOLFRZ * loc_RHOAIR + self.com_WATERZ*((1061.0 + 0.444*loc_TFZCAB) + (158.9 - 0.488*loc_TFRZE))
		loc_QSFZF = self.com_FRZOPN * loc_DHFZFS * loc_VOLFRZ*loc_RHOAIR

		#  Transient Sensible Heat Transfer
		loc_QDFFC  = loc_HTRFFC * loc_AFFC * (self.com_TDRAIR - loc_TFFCAB) * self.com_HRFFC
		loc_QNDFFF = loc_HTRFFC * loc_AFFC * (self.com_TDRAIR - loc_TFFCAB) * self.com_HRFFC
		loc_QNDFZC = loc_HTRFRZ * loc_AFRZ * (self.com_TDRAIR - loc_TFZCAB) * self.com_HRFRZ
		loc_QNDFZF = loc_HTRFRZ * loc_AFRZ * (self.com_TDRAIR - loc_TFZCAB) * self.com_HRFRZ

		#  Set evaporator Temperatures
		loc_TFFC  = self.par_TSETFF
		loc_TFRZE = self.par_TSETFZ

		#  Transient Latent Heat Transfer
		loc_RHFFC = self.getSatWaterDensity(loc_TFFC)
		
		#  Transient Latent Heat Transfer
		loc_RHFRZ = self.getSatWaterDensity(loc_TFRZE)
		
		loc_HIG = 1220.0
		loc_QNVFFC = loc_HMFFC * loc_HIG * loc_AFFC * (loc_RHAIR * loc_RHOH2O - loc_RHFFC) * self.com_HRFFC
		
		if loc_QNVFFC < 0.0 : loc_QNVFFC = 0.0
 
		loc_QNVFFF = loc_QNVFFC
		loc_QNVFZC = loc_HMFRZ * loc_HIG * loc_AFRZ * (loc_RHAIR * loc_RHOH2O - loc_RHFRZ) * self.com_HRFRZ
		
		if loc_QNVFZC < 0.0 : loc_QNVFRZ = 0.0
		
		loc_QNVFZF = loc_QNVFZC
		
		
		# TOTAL HEAT TRANSFER RATES
		if self.com_NMOD != 5.0:                                 #Upright door
			res1_QDFFCS =  loc_QSFFC + loc_QDFFC
			res2_QDFFFS =  loc_QSFFF + loc_QNDFFF
			res3_QDFZCS =  loc_QSFZC + loc_QNDFZC
			res4_QDFZFS =  loc_QSFZF + loc_QNDFZF

			res5_QDFFCL =  loc_QLFFC + loc_QNVFFC
			res6_QDFFFL =  loc_QLFFF + loc_QNVFFF
			
			res7_QDFZCL =  loc_QLFZC + loc_QNVFZC
			res8_QDFZFL =  loc_QLFZF + loc_QNVFZF
			
		else:									#Chest freezer             
			res1_QDFFCS = 0.0 # Python Comment :not required NMOD > 5
			res2_QDFFFS = 0.0 # Python Comment :not required NMOD > 5
			res5_QDFFCL = 0.0 # Python Comment :not required NMOD > 5
			res6_QDFFFL = 0.0 # Python Comment :not required NMOD > 5
			
			res3_QDFZCS =  0.45 * loc_QNDFZC
			res4_QDFZFS =  0.45 * loc_QNDFZF

			res7_QDFZCL =  0.25 * loc_QLFZC + 0.45 * loc_QNVFZC + self.com_WATERZ*( 1061.0 + 0.444 * loc_TFZCAB)
			res8_QDFZFL =  0.25 * loc_QLFZF + 0.45 * loc_QNVFZF + self.com_WATERZ*(( 1061.0 + 0.444 * loc_TFZCAB) + (158.9 - 0.488*loc_TFRZE))
		
		return [res1_QDFFCS, res2_QDFFFS, res3_QDFZCS, res4_QDFZFS, res5_QDFFCL, res6_QDFFFL, res7_QDFZCL, res8_QDFZFL]
		
#-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==
'''
def main():
	# here is the final style for using Doorpn class
	par_TSETFF  =  37.994
	par_TSETFZ=  5.0
	par_HFFC =  3.25
	par_WFFC =  2.1876640419947506
	par_DFFC =  1.7496719160104985
	par_HFRZ =  1.3021653543307086
	par_WFRZ =  2.1043307086614176
	par_DFRZ =  1.7080052493438318
	com_VOLAZ =  4.769926192746406
	com_VOLAR =  13.941801744535086
	com_RELHUM =  50.0
	com_TDRAIR =  89.96000000000001
	com_FFCOPN =  0.0
	com_WATERF =  0.0
	com_WATERZ =  0.0
	com_FRZOPN =  0.0
	com_HRFFC =  0.0
	com_HRFRZ =  0.0
	com_NMOD = 3
	
	#alternative to 
	# call DOORPN(par_TSETFF, par_TSETFZ, par_HFFC, par_WFFC, par_DFFC, par_HFRZ, par_WFRZ, par_DFRZ)
	obj_doorpn = DoorOpen().DoorpnBuilder ()	\
		.withTempFFSetpoint (par_TSETFF)		\
		.withTempFZSetpoint (par_TSETFZ)		\
		.withHeighFF ( par_HFFC) 				\
		.withWidthFF (par_WFFC)			\
		.withDepthFF (par_DFFC)			\
		.withHeighFZ (par_HFRZ)			\
		.withWidthFZ (par_WFRZ)			\
		.withDepthFZ (par_DFRZ)			\
		.withMode (com_NMOD)			\
		.withVolumeFZ (com_VOLAZ)	\
		.withVolumeFF (com_VOLAR)	\
		.withRelHumidity (com_RELHUM)		\
		.withTempAirAmbient (com_TDRAIR)	\
		.withOpenHrFF (com_FFCOPN)		\
		.withWaterFF (com_WATERF)		\
		.withWaterFZ (com_WATERZ)		\
		.withOpenHrFZ (com_FRZOPN)	\
		.withHrOpenFF (com_HRFFC)	\
		.withHrOpenFZ (com_HRFRZ)	\
		.build()
	
	[p1,p2,p3, p4,p5,p6, p7, p8]= obj_doorpn.main()
	
	print ("p1 =",p1)
	print ("p2 =",p2)
	print ("------")
	print ("p3 =",p3)
	print ("p4 =",p4)
	print ("------")
	print ("p5 =",p5)
	print ("p6 =",p6)
	print ("------")
	print ("p7 =",p7)
	print ("p8 =",p8)
	
#Testing=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

if __name__ == "__main__":
	main()
'''	