# Python import
import math, sys, datetime
from abc import ABC,abstractmethod

# User import
from .Data import Data
from .Block2 import Block2
from .HeatExch import HeatExch
from .CycleUtil import *

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Evaprator configration
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Evap_Abstract (ABC, HeatExch, CycleUtil, Data):
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
	# job iterates to determine the enthalpy.
	#      the pressure and quality are inputs 
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def enthal(self,HBUB,HDEW,XSPEC ):
		#	[P4, P5, P6] = self.enthal ( P1, P2, P3)
		#	SUBROUTINE ENTHAL( HBUB,HDEW,XSPEC,  X,P,H)
		#     ******************************************************************
		#     *    ITERATES TO DETERMINE THE ENTHALPY.                         *
		#     *    THE PRESSURE AND QUALITY ARE INPUTS                         *
		#     ******************************************************************
		#
		#	DIMENSION X(5),XL(5),XV[5]
		#
		#          MAKE INITIAL GUESS ASSUMING A LINEAR VARIATION IN ENTHALPY
		#          WITH THE EXIT QUALITY
		#
		HGUESS = HBUB + (HDEW-HBUB)*XSPEC
		DELH = 0.0

		ITERH = 0
		XTOL = 1000.0

		while (ITERH < 100 and XTOL >= 0.001):
			HGUESS = HGUESS + DELH
			if (HGUESS < HBUB): HGUESS = HBUB*1.01
			if (HGUESS > HDEW): HGUESS = HDEW*0.99

			# [P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
			[T,XCALC,XL,XV,Cycle.obj_data.VL,Cycle.obj_data.V,HL,HV] = self.hpin ( HGUESS,P,X )	#CALL HPIN(HGUESS,P,X,  T,XCALC,XL,XV,Cycle.obj_data.VL,Cycle.obj_data.V,HL,HV)

			if (XCALC < 0.0): XCALC = 0.001
			if (XCALC > 1.0): XCALC = 0.999
			#
			#        ADJUST ENTHALPY GUESS
			#
			ALPHAH = 1.0 - XCALC/XSPEC
			XTOL = abs(ALPHAH)
			DELH = (HDEW-HBUB)*ALPHAH
			ITERH = ITERH + 1


		H = HGUESS

		return [X,P, H ]

	#-----------------------------------------------------------
	# job interchanger for subcooling condenser liquid 
	#     used when the inlet states of both streams specified
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def inter1 (self, X,P4,T4 ,H4,V4,P7 ,T7,H7,V7, ETHX1):
		#  P11 = self.inter1 ( P1, ... to .. P10)
		#	  SUBROUTINE INTER1(X,P4,T4,H4,V4,P7,T7,H7,V7,ETHX1,QACT)
		#     ******************************************************************
		#     *    INTERCHANGER FOR SUBCOOLING CONDENSER LIQUID                *
		#     *    USED WHEN THE INLET STATES OF BOTH STREAMS SPECIFIED        *
		#     ******************************************************************
		#
		#     STATEPOINTS:  4 = CONDENSER OUTLET,
		#                   6 = LIQUID OUTLET FROM INTERCHANGER
		#                   7 = OUTLET FROM FRESH FOOD EVAPORATOR
		#                  13 = LOW PRESSURE SIDE OUTLET FROM INTERCHANGER
		#
		LCONV = False
		#
		#          DETERMINE STATE 6 FOR CASE OF REFRIGERANT EXIT TEMP=T[7]
		#
		P6STAR = P4
		T6STAR = T7
		VGUESS = V4

		# [P4, P5] = self.espar [P1, P2, P3]
		[A6STAR,B6STAR] = self.espar (0,T6STAR,X) #  CALL ESPAR(0,T6STAR,X,A6STAR,B6STAR)

		#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
		[VGUESS, LCONV] = self.vit (T6STAR,P6STAR,A6STAR,B6STAR,VGUESS,True) #  CALL VIT(T6STAR,P6STAR,A6STAR,B6STAR,VGUESS,True,LCONV)

		V6STAR = VGUESS

		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[H6STAR,CV,CP,VS] = self.hcvcps (1,T6STAR,V6STAR,X) #  CALL HCVCPS(1,T6STAR,V6STAR,X,  H6STAR,CV,CP,VS)
		#
		#
		#          DETERMINE STATE 13 if REFRIGERANT EXIT TEMP=T(4)
		#          FOR THE CASE OF EVAPORATOR EXIT SUPERHEAT SPECIFIED
		#
		P13STR = P7
		T13STR = T4
		VGUESS = V7*T13STR/T7

		# [P4, P5] = self.espar [P1, P2, P3]
		[A13STR,B13STR] = self.espar (0,T13STR,X)	# CALL ESPAR(0,T13STR,X,A13STR,B13STR)

		#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
		[VGUESS, LCONV] = self.vit (T13STR,P13STR,A13STR,B13STR,VGUESS, False) #CALL VIT(T13STR,P13STR,A13STR,B13STR,VGUESS,.FALSE.,LCONV)
		V13STR = VGUESS

		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[H13STR,CV,CP,VS] = self.hcvcps (1,T13STR,V13STR,X) # CALL HCVCPS(1,T13STR,V13STR,X,   H13STR,CV,CP,VS)
		#
		#          FIND THE MAXIMUM AND ACTUAL HEAT TRANSFER
		#
		DELH1 = H4 - H6STAR
		DELH2 = H13STR - H7
		QBEST = min(DELH1,DELH2)
		QACT = ETHX1*QBEST

		return QACT
	
	#-----------------------------------------------------------
	# Job  interchanger for subcooling condenser liquid
	#       used when the inlet states of both streams specified
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def inter2 (self, X,PA,TAI,HAI,VAI,PB,HBO,TDEW,HDEW,VDEW,ETA ):
		#  [P13, P14, P15] = self.inter2 ( P1, ... to .. P12)
		#	  SUBROUTINE INTER2(X,PA,TAI,HAI,VAI,PB,HBO,TDEW,HDEW,VDEW,ETA,
		#	 .                  TBI,HBI,QACT)
		#     ******************************************************************
		#     *    ITERATES TO SOLVE FOR INTERCHANGER HEAT TRANSFER KNOWING    *
		#     *    THE INLET STATE OF ONE STREAM AND OUTLET STATE OF THE       *
		#     *    OTHER FOR A COUNTERFLOW HEAT EXCHANGER.                     *
		#     *    EQUAL MASS FLOW RATES OF THE SAME FLUID                     *
		#     ******************************************************************
		#
		LCONV = False
		#DIMENSION X(5),XL(5),XV[5]
		#
		#          KNOWN: INLET STATE OF STREAM A
		#                 OUTLET STATE OF STREAM B
		#
		#          GUESS THE INLET STATE FOR STREAM B
		#
		HBI = HDEW - 5.0
		ITER = 0
		HTOL = 1000.0
		while (ITER <=100 and  HTOL > 0.001):
			# [P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
			[TBI,XQBI,XL,XV,VL,VV,HL,HV] = self.hpin ( HBI,PB,X ) # CALL HPIN(HBI,PB,X,  TBI,XQBI,XL,XV,VL,VV,HL,HV)    !State at BI

			#
			#          DETERMINE EXIT STATE OF STREAM A if AT TBI
			#
			VGUESS = VAI

			# [P4, P5] = self.espar [P1, P2, P3]
			[AA, BB] = self.espar (0,TBI,X) #  CALL ESPAR(0,TBI,X,AA,BB)

			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[VGUESS, LCONV] = self.vit (TBI,PA,AA,BB,VGUESS,True) # CALL VIT(TBI,PA,AA,BB,VGUESS,True,LCONV)

			VAOSTR = VGUESS

			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HAOSTR,CV,CP,VSND] = self.hcvcps (1,TBI,VAOSTR,X) #  CALL HCVCPS(1,TBI,VAOSTR,X,  HAOSTR,CV,CP,VSND)

			DHAMAX = HAI - HAOSTR
			#
			#          DETERMINE EXIT STATE OF STREAM B if AT TAI
			#
			VGUESS = VDEW * TAI/TDEW

			# [P4, P5] = self.espar [P1, P2, P3]
			[AA,BB] = self.espar (0,TAI,X) #  CALL ESPAR(0,TAI,X,AA,BB)

			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[VGUESS, LCONV] = self.vit (TAI,PB,AA,BB,VGUESS,True) #  CALL VIT(TAI,PB,AA,BB,VGUESS,True,LCONV)

			VBOSTR = VGUESS

			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HBOSTR,CV,CP,VSND] = self.hcvcps (1,TAI,VBOSTR,X) # CALL HCVCPS(1,TAI,VBOSTR,X,  HBOSTR,CV,CP,VSND)

			DHBMAX = HBI - HBOSTR
			#
			#          DETERMINE THE HEAT TRANSFER FOR THE GUESSED INLET STATE HBI
			#
			QMAX = min(DHAMAX,DHBMAX)
			QACT = ETA * QMAX
			#
			#          ADJUST THE STREAM B ENTHALPY GUESS
			#
			DELTA = QACT / (HBO-HBI)
			HTOL = abs(1.0-DELTA)
			HBI2 = HBO - (HBO-HBI)*DELTA

			if(HBI2 > 1.1 * HBI): HBI2 = 1.1 * HBI
			if(HBI2 < 0.9 * HBI): HBI2 = 0.9 * HBI
			HBI = HBI2

			ITER = ITER + 1
		#END DO

		return [TBI,HBI,QACT]

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
class EvapSuper (Evap_Abstract): #Data.obj_cdata.ISPEC== 1

	def iterat_call_exit_p13 (self):
		self.call_exit_p13()
		
	def iterat_evap_subcool_p7 (self):
		self.evap_subcool_p7()
		
	def calc_evap_exit_p7 (self):
		self.objData.T[15] = self.objData.TS3 - (Data.obj_cdata.DTSUPE + 2.0)

		[ self.objData.XL_Temp, self.objData.X, self.objData.P[15], self.objData.VL[15], self.objData.V[15], LCRIT]=self.bublt( self.objData.T[15], self.objData.XL_Temp, self.objData.X,  False)
		self.setArr2dCol (self.objData.XL, 15, self.objData.XL_Temp)
		
		self.objData.TE[1] = self.objData.T[15] + Data.obj_cdata.DTSUPE
		self.objData.T[7] = self.objData.TE[1]
		self.objData.P[7] = self.objData.P[15]

	def iterat_evap_exit_p7 (self):
		self.objData.TE[self.objData.JE] = self.objData.T[15] + Data.obj_cdata.DTSUPE

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
class EvapIHX (Evap_Abstract): #Data.obj_cdata.ISPEC == 2
	def calc_evap_exit_p7 (self):
		self.objData.T[15] = self.objData.TS3 - 2.0
		self.objData.T[13] = self.objData.T[15] + Data.obj_cdata.DTSUPI

		if (self.objData.T[13] >  self.objData.TC[1]) :
			self.objData.T[13] = self.objData.TC[1] - 5.0
			self.objData.T[15] = self.objData.T[13] - Data.obj_cdata.DTSUPI

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
		self.objData.T[13] = self.objData.T[15] + Data.obj_cdata.DTSUPI
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
class EvapQual (Evap_Abstract): # Data.obj_cdata.ISPEC== 3
	def iterat_call_exit_p13 (self):
		self.call_exit_p13()
		
	def iterat_evap_subcool_p7 (self):
		self.evap_subcool_p7()
		
	def calc_evap_exit_p7 (self):
		self.objData.T[15] = self.objData.TS3 - 2.0

		[self.objData.XL_Temp, self.objData.X,self.objData.P[15],self.objData.VL[15],self.objData.V[15], LCRIT] = self.bublt (self.objData.T[15], self.objData.XL_Temp, self.objData.X , False ) 
		self.setArr2dCol (self.objData.XL, 15, self.objData.XL_Temp)
		
		[self.objData.X, self.objData.XV15, TBUB15, VBUB15, VV15, LCRIT] = self.bublp ( self.objData.P[15],self.objData.X, self.objData.XV15,   True)		
		
		self.objData.TE[1] = self.objData.T[15] - ( self.objData.T[15]- TBUB15 )*(1.0 - Data.obj_cdata.XEXITE)
		self.objData.T[7]  = self.objData.TE[1]
		self.objData.P[7]  = self.objData.P[15]

	def iterat_evap_exit_p7 (self):
		self.objData.XQ[15] = 1.0
		[self.objData.XL_Temp, self.objData.X, self.objData.P[15], self.objData.VL[15], self.objData.V[15], LCRIT] = bublt (self.objData.T[15], self.objData.XL_Temp,self.objData.X,self.objData.P[15] , False )
		self.setArr2dCol (self.objData.XL, 15, self.objData.XL_Temp)
		
		self.objData.P[7] = self.objData.P[15]
		return 0

	def iterat_evap_enthalpy_p7 (self):
		self.objData.XQ[7] = Data.obj_cdata.XEXITE

		[ self.objData.X,self.objData.P[7],self.objData.H[7] ] = self.enthal ( HBUB15,self.objData.H[15],self.objData.XQ[7] )	

		[self.objData.T[7],self.objData.XQ[7],self.objData.XL_Temp, self.objData.XV_Temp,  self.objData.VL[7],self.objData.VV[7],HL7,HV7] = self.hpin ( self.objData.H[7],self.objData.P[7],self.objData.X)
		self.setArr2dCol (self.objData.XL, 7, self.objData.XL_Temp)
		self.setArr2dCol (self.objData.XV, 7, self.objData.XV_Temp )
		
		self.objData.V[7] = (1.0-self.objData.XQ[7])*self.objData.VL[7] + self.objData.XQ[7]*self.objData.VV[7]
		self.objData.T[7] = self.objData.TE[self.objData.JE]
	
	def iterat_evap_subcool_p7 (self):
		evap_subcool_p7()
	