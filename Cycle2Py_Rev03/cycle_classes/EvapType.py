# Python import
import math, sys, datetime
from abc import ABC,abstractmethod

# User import
from Data import Data
from Block2 import Block2
from HeatExch import HeatExch

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Evaprator configration
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Evap_Abstract (ABC, HeatExch):
	def __init__ (self, objdata):
		self.objData = objdata
	# Abstract methods
	#-----------------------------------------------------------
	# Job 			: Evap. output pressure point (7)
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def calc_evap_exit_p7 (self):
		pass

	#-----------------------------------------------------------
	# Job 			: Iterate Evap. output pressure point (7)
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def iterat_evap_exit_p7 (self):
		pass

	#-----------------------------------------------------------
	# Job 			: Iterate Evap. enthalpy at point (7)
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def iterat_evap_enthalpy_p7 (self):
		pass

	#-----------------------------------------------------------
	# Job 			: interchanger for subcooling  liquid
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def iterat_evap_subcool_p7 (self):
		pass

	#-----------------------------------------------------------
	# Job 			: calculate point 7 Temp
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def iterat_evap_subcool_p7 (self):
		pass
	#-----------------------------------------------------------
	# Job 			: interchanger for subcooling  liquid
	#					only for ISPEC== 1 and ISPEC== 3
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def evap_subcool_p7 (self):
		self.objData.QINT = self.inter1 ( self.objData.X,self.objData.P[16],self.objData.T[16],  self.objData.H[16],self.objData.V[16],self.objData.P[7],  self.objData.T[7],self.objData.H[7],self.objData.V[7],  self.objData.ETHX1 )
		self.objData.H[13] = self.objData.H[7] + self.objData.QINT	
		
	#-----------------------------------------------------------
	# Job 			: calculate temp for point 7
	#					only for ISPEC== 1 and ISPEC== 3
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def call_exit_t7 (self):
		[self.objData.T[13],XQ13,self.objData.XL_Temp, self.objData.XV_Temp, L13,self.objData.V[13],HL,HV] \
			= self.hpin ( self.objData.H[13],self.objData.P[13],self.objData.X )

		self.setArr2dCol (self.objData.XL, 13, self.objData.XL_Temp )
		self.setArr2dCol (self.objData.XV, 13, self.objData.XV_Temp )

	#-----------------------------------------------------------
	# Job 			: calculate pressue for point 13
	#					only for ISPEC== 1 and ISPEC== 3
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def call_exit_p13 (self):
		[self.objData.T[13], self.objData.XQ13, self.objData.XL_Temp,  \
			self.objData.XV_Temp, self.objData.VL13, self.objData.V[13], self.objData.HL, self.objData.HV] \
			= self.hpin ( self.objData.H[13],self.objData.P[13],self.objData.X )
			
		self.setArr2dCol (self.objData.XL, 13, self.objData.XL_Temp)
		self.setArr2dCol (self.objData.XV, 13, self.objData.XV_Temp )
		
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Evaporator output for Evap superheat, Evap. output pressure point (7)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class EvapSuper (Evap_Abstract): #Data.ISPEC== 1

	def iterat_call_exit_p13 (self):
		self.call_exit_p13()
		
	def iterat_evap_subcool_p7 (self):
		self.evap_subcool_p7()
		
	def calc_evap_exit_p7 (self):
		self.objData.T[15] = self.objData.TS3 - (Data.DTSUPE + 2.0)

		[ self.objData.XL_Temp, self.objData.X, self.objData.P[15], self.objData.VL[15], self.objData.V[15], LCRIT]=self.bublt( self.objData.T[15], self.objData.XL_Temp, self.objData.X,  False)
		self.setArr2dCol (self.objData.XL, 15, self.objData.XL_Temp)
		
		self.objData.TE[1] = self.objData.T[15] + Data.DTSUPE
		self.objData.T[7] = self.objData.TE[1]
		self.objData.P[7] = self.objData.P[15]

	def iterat_evap_exit_p7 (self):
		self.objData.TE[self.objData.JE] = self.objData.T[15] + Data.DTSUPE

		[ self.objData.XL_Temp, self.objData.X, self.objData.P[15], self.objData.VL[15], self.objData.V[15], LCRIT]\
			= self.bublt (self.objData.T[15], self.objData.XL_Temp, self.objData.X, False )
		self.setArr2dCol (self.objData.XL, 1, self.objData.XL_Temp)
		
		self.objData.P[7] = self.objData.P[15]
		self.objData.XQ[15] = 1.0
		self.objData.XQ[7] = self.objData.XQ[15]
		return 0
		
	def iterat_evap_enthalpy_p7 (self):
		VGUESS = self.objData.V[15]*self.objData.T[7]/self.objData.T[15]
		[A7, B7] = self.espar (0, self.objData.T[7], self.objData.X) 
		[VGUESS, LCONV] = self.vit (self.objData.T[7],self.objData.P[7],A7,B7,VGUESS,False)	
		self.objData.V[7] = VGUESS

		[self.objData.H[7],CV,CP,VS] = self.hcvcps (1, self.objData.T[7], self.objData.V[7], self.objData.X) 

		self.objData.XQ[7] = 1.0
		self.objData.VV[7] = self.objData.V[7]
		self.objData.T[7] = self.objData.TE[self.objData.JE]

		#INC = 1
		for INC in range (1, self.objData.NC + 1): #DO WHILE(INC  <=  NC)
			self.objData.XL[INC][7] = 0.0
			self.objData.XV[INC][7] = self.objData.X[INC]
	
	def iterat_evap_subcool_p7 (self):
		self.evap_subcool_p7()
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Evaporator output for IHX superheat, Evap. output pressure point (7)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class EvapIHX (Evap_Abstract): #Data.ISPEC == 2
	def calc_evap_exit_p7 (self):
		self.objData.T[15] = self.objData.TS3 - 2.0
		self.objData.T[13] = self.objData.T[15] + Data.DTSUPI

		if (self.objData.T[13] >  self.objData.TC[1]) :
			self.objData.T[13] = self.objData.TC[1] - 5.0
			self.objData.T[15] = self.objData.T[13] - Data.DTSUPI

		# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
		[ self.objData.XL_Temp,self.objData.X,self.objData.P[15],self.objData.VL[15],self.objData.V[15], LCRIT] = self.bublt(self.objData.T[15], self.objData.XL_Temp,self.objData.X, False)
		self.setArr2dCol (self.objData.XL, 15, self.objData.XL_Temp)
		
		self.objData.TE[1] = self.objData.T[15]
		self.objData.T[7] = self.objData.TE[1]
		self.objData.P[7] = self.objData.P[15]

	def iterat_evap_exit_p7 (self):
		[self.objData.XL_Temp,self.objData.X,self.objData.P[15],self.objData.VL[15],self.objData.V[15], LCRIT] = bublt (self.objData.T[15], self.objData.XL_Temp,self.objData.X,self.objData.P[15], False )
		self.setArr2dCol (self.objData.XL, 15, self.objData.XL_Temp)
		
		self.objData.P[13] = self.objData.P[15]
		self.objData.T[13] = self.objData.T[15] + Data.DTSUPI
		VGUESS = self.objData.V[15]*self.objData.T[13]/self.objData.T[15]

		[A13, B13] = self.espar (0, self.objData.T[13], self.objData.X) 
		[VGUESS, LCONV] = self.vit (self.objData.T[13],self.objData.P[13],A13,B13,VGUESS, False) 
		self.objData.V[13] = VGUESS

		[ self.objData.H[13],CV,CP,VS] = self.hcvcps (1,self.objData.T[13],self.objData.V[13],self.objData.X)
							
		self.objData.P[7] = self.objData.P[15]
		self.objData.TE[self.objData.JE] = self.objData.T[7]
		
		if (self.objData.T[13]  >=  self.objData.T[16]) :
			LECON = False
			return 1#I_ERROR_INTER = 1
			#continue	
		else:
			return 0
	
	def iterat_evap_enthalpy_p7 (self):
		[self.objData.T[7],self.objData.H[7],self.objData.QINT] = self.inter2 (self.objData.X,self.objData.P[16],self.objData.T[16],self.objData.H[16],self.objData.V[16],self.objData.P[13],self.objData.H[13], self.objData.T[15],self.objData.H[15],self.objData.V[15],self.objData.ETHX1 )
		[self.objData.T[7],self.objData.XQ[7],self.objData.XL_Temp, self.objData.XV_Temp,  self.objData.VL[7],self.objData.VV[7],HL7,HV7] = self.hpin ( self.objData.H[7],self.objData.P[7],self.objData.X)

		self.setArr2dCol (self.objData.XL, 7, self.objData.XL_Temp)
		self.setArr2dCol (self.objData.XV, 7, self.objData.XV_Temp )
		
		self.objData.V[7] = (1.0-self.objData.XQ[7])*self.objData.VL[7] + self.objData.XQ[7]*self.objData.VV[7]
		self.objData.TE[self.objData.JE] = self.objData.T[7]

	def iterat_evap_subcool_p7 (self):
		pass # nothing to do
	
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Evaporator output for Evaporator Quality,Evap. output pressure point (7)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class EvapQual (Evap_Abstract): # Data.ISPEC== 3
	def iterat_call_exit_p13 (self):
		self.call_exit_p13()
		
	def iterat_evap_subcool_p7 (self):
		self.evap_subcool_p7()
		
	def calc_evap_exit_p7 (self):
		self.objData.T[15] = self.objData.TS3 - 2.0

		[self.objData.XL_Temp, self.objData.X,self.objData.P[15],self.objData.VL[15],self.objData.V[15], LCRIT] = self.bublt (self.objData.T[15], self.objData.XL_Temp, self.objData.X , False ) 
		self.setArr2dCol (self.objData.XL, 15, self.objData.XL_Temp)
		
		[self.objData.X, self.objData.XV15, TBUB15, VBUB15, VV15, LCRIT] = self.bublp ( self.objData.P[15],self.objData.X, self.objData.XV15,   True)		
		
		self.objData.TE[1] = self.objData.T[15] - ( self.objData.T[15]- TBUB15 )*(1.0 - Data.XEXITE)
		self.objData.T[7]  = self.objData.TE[1]
		self.objData.P[7]  = self.objData.P[15]

	def iterat_evap_exit_p7 (self):
		self.objData.XQ[15] = 1.0
		[self.objData.XL_Temp, self.objData.X, self.objData.P[15], self.objData.VL[15], self.objData.V[15], LCRIT] = bublt (self.objData.T[15], self.objData.XL_Temp,self.objData.X,self.objData.P[15] , False )
		self.setArr2dCol (self.objData.XL, 15, self.objData.XL_Temp)
		
		self.objData.P[7] = self.objData.P[15]
		return 0

	def iterat_evap_enthalpy_p7 (self):
		self.objData.XQ[7] = Data.XEXITE

		[ self.objData.X,self.objData.P[7],self.objData.H[7] ] = self.enthal ( HBUB15,self.objData.H[15],self.objData.XQ[7] )	

		[self.objData.T[7],self.objData.XQ[7],self.objData.XL_Temp, self.objData.XV_Temp,  self.objData.VL[7],self.objData.VV[7],HL7,HV7] = self.hpin ( self.objData.H[7],self.objData.P[7],self.objData.X)
		self.setArr2dCol (self.objData.XL, 7, self.objData.XL_Temp)
		self.setArr2dCol (self.objData.XV, 7, self.objData.XV_Temp )
		
		self.objData.V[7] = (1.0-self.objData.XQ[7])*self.objData.VL[7] + self.objData.XQ[7]*self.objData.VV[7]
		self.objData.T[7] = self.objData.TE[self.objData.JE]
	
	def iterat_evap_subcool_p7 (self):
		evap_subcool_p7()
	