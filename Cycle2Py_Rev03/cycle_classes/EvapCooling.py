# Python import
import math, sys, datetime
from abc import ABC,abstractmethod

# User import
from Data import Data
from Block2 import Block2
from HeatExch import HeatExch

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Evaprator cooling method (Natural, Cross, Counter-flow
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class EvapCool_Abstract (ABC, HeatExch):
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
	def calc_heat_temp (self):
		pass

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def frsh(self, H,T,TS3, TE,JE,QFRSH):
		# [P4, P5,P6  P8] = self.frsh (P1,P2,P3   P5,P6,P7)
		# [TS4, TE,JE, ICONE] = self.frsh ( H,T,TS3, TE,JE,  QFRSH)
		#	  SUBROUTINE FRSH(H,T,TS3,  TS4,TE,JE,  QFRSH,ICONE)
		#     *****************************************************************
		#     *    CALCULATE FRESH FOOD SECTION EXIT TEMPERATURE              *
		#     *****************************************************************
		#         INITIALIZE
		
		FTE = [0.0, 0.0, 0.0] # in Python only
		
		ICONE = 0
		
		ALPHA = QFRSH/(Data.MREF*( H[7] - H[5]) ) # Python: MREF:Initial Guess For Refrigerant Mas Flow Rate (kg/hr)
		
		if (QFRSH  ==  0.0): ALPHA = 0.01
		#
		#          ESTIMATE NEW VALUE FOR EXIT TEMPERATURE
		#
		DELT = (TS3 - T[5])*(1.0 - 1.0/ALPHA)
		if (-DELT  >  TE[JE]): DELT = -0.5*TE[JE]

		TEOUT = TE[JE] + DELT
		
		TS4 = TS3 - QFRSH/Data.CFME
		if (TEOUT  >  TS3): TEOUT = TS3

		FTE[JE] = TEOUT - TE[JE]
		
		if (JE  ==  1) :
			TENEW = TEOUT
		else:
			TENEW = TEOUT
			if (FTE[2]  != FTE[1]) :
				TENEW = TE[2] - FTE[2]*(TE[2] - TE[1])/(FTE[2] - FTE[1])

			TE[1] = TE[2]
			FTE[1] = FTE[2]

		TENEW = (TENEW + TE[JE] )/2.0
		
		if (TENEW  >  1.05*TE[JE] ): TENEW = 1.05*TE[JE]
		if (TENEW  <  0.95*TE[JE] ): TENEW = 0.95*TE[JE]
		if (TENEW  >  TS3): TENEW = TS3

		ERROR = abs(TENEW - TE[JE] )
		if (ERROR  <  Data.TOL_FRSH): ICONE = 1

		JEOLD = JE #useless not used
		JE = 2
		
		TE[JE] = TENEW
		
		#          ADJUST EXIT AIR TEMP TO 90% APPROACH if NATURAL CONVECTION

		if (Data.IFRSH == 0) :
			TS4 = 0.9 * TE [JE] + 0.1 * TS3
		
		return [TS4, TE, JE, ICONE]

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Evaporator with Natural cooling
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class EvapCool_FFNat (EvapCool_Abstract): #Data.IFRSH== 0 
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def calc_heat_temp (self):
		return self.ffnat (self.objData.T5,self.objData.H5,self.objData.T7,
			self.objData.TDEW,self.objData.HDEW,self.objData.TS3,	\
			self.objData.CPRVAP, self.objData.IRFTYP)
			
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def ffnat (self, T5,H5,T7,  TDEW,HDEW,TS3,  CPRVAP, IRFTYP):
		#     ******************************************************************
		#     *    SUBROUTINE  FFNAT - CALCULATES THE FRESH FOOD EVAPORATOR    *
		#     *    HEAT  TRANSFER FOR A NATURAL CONVECTION  EVAPORATOR         *
		#     ******************************************************************
		# Calculate the radiation heat transfer heat transfer
		#  coefficient using small delta t approximation (black body)
		#  use the refrigerant dew point to evaluate h radiation
		SIGMA = 2.04326E-7
		EPS   = 0.8
		
		TENV = (TROOM + 459.6)/1.8
		TAVE = (T5+T7)/2.0
		
		TAIR = TS3
		FZTMPK = (FZTEMP + 459.6)/1.8
		
		HRAD = SIGMA*(TAVE + TAIR)*(TAVE**2 + TAIR**2)*EPS

		# get the net evaporator area
		AEVAP = ATOTE
		if (IRFTYP == 6): AEVAP = ATOTE + ATOTE
		
		# calculate the natural convection heat transfer coefficient
		DELTAT = TAIR - TAVE
		if(DELTAT <= 0.0): DELTAT = 0.0001
		
		DELTA = DELTAT*1.8
		 
		TBAR = 0.67*TAVE + 0.33*TAIR
		A_NAT = 0.239 + 3.34E-04*(273.0 - TBAR)
		
		HNAT = A_NAT*DELTA**0.33*20.44
		
		# Calculate combined air-side heat transfer coefficient
		UAIR = HRAD + HNAT
		
		if (obj_data.IWALL_FF == 1):
			UAIR = 1.0/(1.0/UAIR + 0.1389/20.44)
		
		Q_IN_WALL = 1.0548 * 1.8 * UA_FF * (TENV - TAVE)/AEVAP + Q_HXS_FF/AEVAP
		
		# Calculate the heat transfer assuming that the air side
		#  resistance dominates
		#
		# Calculate the are necessary to evaporate the refrigerant
		QTPNEC = Data.MREF * (HDEW-H5)
		ATPNEC = QTPNEC/(UAIR*DELTAT + Q_IN_WALL)
		
		# Calculate the superheating area fraction
		if (ATPNEC  <  AEVAP):
			QTPE = QTPNEC
			ASUPE = AEVAP - ATPNEC
			QSUPMX = Data.MREF*CPRVAP*(TAIR-TDEW)
			QSUPE = UAIR*ASUPE*DELTAT + ASUPE*Q_IN_WALL
			if (QSUPE > QSUPMX): QSUPE = QSUPMX
			QTOTE = QTPE + QSUPE
			FSUPE = ASUPE/AEVAP
		else:
			QTOTE = UAIR*AEVAP*DELTAT + AEVAP*Q_IN_WALL
			FSUPE = 0
		 
		Data.UAFF = UAIR * AEVAP     # extra output to common
		 
		return [QTOTE, FSUPE]

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Evaporator with Cross cooling
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class EvapCool_FFCross (EvapCool_Abstract): #Data.IFRSH== 1
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def calc_heat_temp (self):
		return self.ffcross (self.objData.H[5], self.objData.T[5], 
			self.objData.HDEW, self.objData.TDEW, self.objData.TS3, \
			self.objData.CPRVAP, self.objData.P[5], self.objData.P[7], self.objData.X, Data.N_EVAP)
		
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def ffcross(self, H5_S,T5_S,  HDEW_S,TDEW_S,TS3,  CPR,PIN,POUT, X,NUM_ZONE):
		#   ******************************************************************
		#   solves for the fresh food evaporator   
		#   heat transfer for cross heat exchanger       
		#   ******************************************************************
		
		AREA_TOL = 0.001
		#
		#          INITIALIZE
		#
		T5 = T5_S
		H5 = H5_S
		HDEW = HDEW_S
		TDEW = TDEW_S

		DELP = (PIN  - POUT)/(NUM_ZONE)
		DELH = (HDEW - H5)/(NUM_ZONE)
		P5 = POUT

		QSUP = 0.0
		QTPC = 0.0

		ASUP = 0
		ATPC = 0

		TAIR = TS3
		CAIR = Data.CFME
		HAVE_NOT_USED_FULL_AREA = True
		#
		#          BEGIN  WITH TWO-PHASE AREA
		#
		ALEFT = Data.ATOTE

		#	N = 1
		for N in range (1, NUM_ZONE): #	DO WHILE (N  <=  NUM_ZONE)
			PDEW = P5 + DELP
			HDEW = H5 + DELH

			#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
			[TDEW,XQ,XL,XV,VL,VV,HL,HV] = self.hpin ( HDEW,PDEW,X )
			#CALL HPIN(HDEW,PDEW,X,TDEW,XQ,XL,XV,VL,VV,HL,HV)

			if(HAVE_NOT_USED_FULL_AREA) :
				CPRTP = (HDEW-H5)/abs(TDEW-T5+0.0001)
				CRTP  = Data.MREF*CPRTP
				#
				#          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
				#
				CAIR = (ALEFT/ Data.ATOTE) * Data.CFME
				if (CAIR  <=  CRTP) :
					CMINTP = CAIR
					CMAXTP = CRTP
				else:
					 CMINTP = CRTP
					 CMAXTP = CAIR
				#END if
				#
				#          IS AREA BIG ENOUGH FOR CONDENSATION
				#
				QMAX = CMINTP*(TAIR - T5)
				QDUM = Data.MREF * (HDEW - H5)

				EFF_TPC = QDUM/QMAX
				[EFFTPC,DEXDAR] = self.exf (2,ALEFT, Data.UTPE,CMINTP,CMAXTP)
				#CALL EXF(2,ALEFT,UTPE,CMINTP,CMAXTP, EFFTPC,DEXDAR)

				if(EFFTPC  <=  EFF_TPC):                   #Need more area
					ATPC = ATPC + ALEFT
					HAVE_NOT_USED_FULL_AREA = False
					#
					#          BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
					#          TWO PHASE REGION
					#
					#          INITIALIZE VARIABLES
					#
				else:
					ADUM = 0.9*ALEFT
					LOOKING_FOR_AREA = True

					ICOUNT = 0
					QTOL = 1.0

					while (LOOKING_FOR_AREA):
						ICOUNT = ICOUNT + 1
						if(ICOUNT >  100):
							LOOKING_FOR_AREA = False
							continue
						#END if

						CAIR = (ADUM/ Data.ATOTE) * Data.CFME
						if(CAIR  <=  CRTP):
							CMINTP = CAIR
							CMAXTP = CRTP
						else:
							CMINTP = CRTP
							CMAXTP = CAIR
						#END if

						QMAX = CMINTP*(TAIR - T5)
						EFF_TPC = QDUM/QMAX

						#[P6, P7] = self.EXF (P1 ... P5)
						[ EFFTPC,DEXDAR] = self.exf (2,ADUM, Data.UTPE,CMINTP,CMAXTP)
						#CALL EXF(2,ADUM,UTPE,CMINTP,CMAXTP, EFFTPC,DEXDAR)

						ERROR = abs(QTOL)
						if(ERROR  <=  AREA_TOL):
							LOOKING_FOR_AREA = False
							Cointinue
						#END if

						QRAT  = EFFTPC*QMAX/QDUM
						QTOL = 1.0 - QRAT

						DAREA = ADUM*(1.0 - QRAT)

						DAREA_MIN = -0.75*ADUM
						DAREA_MAX = 0.50*(ALEFT - ADUM)

						if(DAREA  <  DAREA_MIN): DAREA = DAREA_MIN
						if(DAREA >  DAREA_MAX): DAREA = DAREA_MAX

						ADUM  = ADUM + DAREA
					#END DO
					ATPC = ATPC + ADUM

				#END if

				QTPC = QTPC + EFFTPC*CMINTP*(TAIR - T5)
			#END if

				ALEFT = Data.ATOTE - ATPC
				H5 = H5 + DELH
				T5 = TDEW
				P5 = P5 + DELP
				N = N + 1
			#END DO

		if(ALEFT  <=  0.0): HAVE_NOT_USED_FULL_AREA = False
		#
		#          CONTINUE WITH DESUPERHEATING AREA
		#

		HDEW = HDEW_S
		TDEW = TDEW_S

		if(HAVE_NOT_USED_FULL_AREA):
			CR  = Data.MREF * CPR
			#
			#          DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
			#
			CAIR = (ALEFT/ Data.ATOTE) * Data.CFME
			
			#if(CAIR  <=  CR) :
			#	CMINDS = CAIR
			#	CMAXDS = CR
			#else:
			#	CMINDS = CR
			#	CMAXDS = CAIR
			
			CMINDS = min(CR, CAIR)
			CMAXDS = max(CR, CAIR)
			
			#
			#          DETERMINE THE NET HEAT TRANSFER
			#
			#[P6, P7] = self.exf (P1 ... P5)
			[EFFDSC, DEXDAR] = self.exf (2,ALEFT,Data.USUPE,CMINDS,CMAXDS)
			#CALL EXF(2,ALEFT,USUPE,CMINDS,CMAXDS,EFFDSC,DEXDAR)
			
			QSUP = CMINDS*EFFDSC*(TS3 - TDEW)

			ASUP = ALEFT
		#END if

		#
		#        CALCULATE THE FRACTIONAL SUBCOOLING AND SUPERHEATING REGIONS
		#

		FSUPE = ASUP/ Data.ATOTE
		QTOTE = QSUP + QTPC
		Data.UAFF = Data.ATOTE * FSUPE * Data.USUPE + Data.ATOTE * (1.0 - FSUPE) * Data.UTPE

		return [QTOTE, FSUPE]

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Evaporator with Counter-flow cooling
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class EvapCool_FFCount (EvapCool_Abstract): #Data.IFRSH== 2
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def calc_heat_temp (self):
		return self.ffcount (self.objData.H[5], self.objData.T[5], 
			self.objData.HDEW, self.objData.TDEW, self.objData.TS3, \
			self.objData.CPRVAP, self.objData.P[5], self.objData.P[7], self.objData.X, Data.N_EVAP)
			
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def ffcount (self, H5_S,T5_S, HDEW_S,TDEW_S,TS3,  CPR,PIN,POUT, X,NUM_ZONE):
		#   ******************************************************************
		#   *    SUBROUTINE FFCOUNT - SOLVES FOR THE FRESH FOOD EVAPORATOR   *
		#   *    HEAT TRANSFER FOR COUNTERFLOW HEAT EXCHANGER                *
		#   ******************************************************************

		AREA_TOL  = 0.001
		NCALL = 0
		#
		#        INITIALIZE
		#
		ICOUNT = 0

		T5 = T5_S
		H5 = H5_S
		HDEW = HDEW_S
		TDEW = TDEW_S

		DELP = (POUT - PIN)/(NUM_ZONE)
		DELH = (HDEW - H5)/(NUM_ZONE)
		P5 = PIN

		QSUP = 0.0
		QTPC = 0.0
		QTOTE_LAST = 0.0

		ASUP = 0.0
		ATPC = 0.0

		if(NCALL  ==  0):
			TAIR = TS3 - 2
			TAIR_GUESS = TAIR
			NCALL = 1
		else:
			TAIR = TAIR_GUESS
		#End if

		CAIR = Data.CFME
		HAVE_NOT_USED_FULL_AREA = True
		#
		#        BEGIN WITH TWO-PHASE AREA
		#
		CONVERGED = False
		while ( not  CONVERGED):
			ICOUNT = ICOUNT + 1
			ALEFT = Data.ATOTE

			#N = 1
			for N in range (1, NUM_ZONE + 1) : #DO WHILE (N  <=  NUM_ZONE)
				PDEW = P5 + DELP
				HDEW = H5 + DELH

				if(HAVE_NOT_USED_FULL_AREA) :
					[TDEW,XQ,XL,XV,VL,VV,HL,HV] = self.hpin ( HDEW,PDEW,X)
					#CALL HPIN(HDEW,PDEW,X,  TDEW,XQ,XL,XV,VL,VV,HL,HV)
					CPRTP = (HDEW-H5)/abs(TDEW-T5+0.0001)
					CRTP  = Data.MREF*CPRTP
					#
					#        DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
					#
					if(CAIR  <=  CRTP) :
						CMINTP = CAIR
						CMAXTP = CRTP
					else:
						CMINTP = CRTP
						CMAXTP = CAIR
					#End if
					#
					#        IS AREA BIG ENOUGH FOR EVAPORATION
					#

					QDUM = Data.MREF*(HDEW - H5)
					TAIR_END = TAIR + QDUM/CAIR
					QMAX = CMINTP*(TAIR_END - T5)

					EFF_TPC = QDUM/QMAX

					[EFFTPC,DEXDAR] = self.exf (1,ALEFT,Data.UTPE,CMINTP,CMAXTP)
					#CALL EXF(1,ALEFT,Data.UTPE,CMINTP,CMAXTP, EFFTPC,DEXDAR)
					if(EFFTPC  <=  EFF_TPC) :
						ATPC = ATPC + ALEFT
						HAVE_NOT_USED_FULL_AREA = False
						#
						#        BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
						#        TWO PHASE REGION
						#
						#        INITIALIZE VARIABLES
						#
					else:
						ADUM = 0.9*ALEFT
						LOOKING_FOR_AREA = True

						ILOOK = 0
						while (LOOKING_FOR_AREA):
							ILOOK = ILOOK + 1
							[EFFTPC,DEXDAR] = self.ext (1,ADUM,Data.UTPE,CMINTP,CMAXTP)
							#CALL EXF(1,ADUM,Data.UTPE,CMINTP,CMAXTP,  EFFTPC,DEXDAR)

							ERROR = abs(EFFTPC - EFF_TPC)
							if(ERROR  <=  AREA_TOL  or  ILOOK  >=  10) :
								LOOKING_FOR_AREA = False
								continue
							#End if

							DAREA = - (EFFTPC - EFF_TPC)/DEXDAR
							DAREA_MIN = -0.75*ADUM
							DAREA_MAX = 0.50*(ALEFT - ADUM)

							if(DAREA  <  DAREA_MIN): DAREA = DAREA_MIN
							if(DAREA  >  DAREA_MAX): DAREA = DAREA_MAX

							if(abs(DAREA)  <=  0.001*Data.ATOTE) :
								LOOKING_FOR_AREA = False
								continue
							#End if

							ADUM  = ADUM + DAREA
						###End Do
						ATPC = ATPC + ADUM
					#End if

					QTPC = QTPC + EFFTPC*QMAX
					TAIR = TAIR + EFFTPC*QMAX/CAIR
				#End if

				ALEFT = Data.ATOTE - ATPC
				H5 = H5 + DELH
				T5 = TDEW
				P5 = P5 + DELP
				#N = N + 1
			###End Do
			if(ALEFT  <=  0.0): HAVE_NOT_USED_FULL_AREA = False
			#
			#        CONTINUE WITH DESUPERHEATING AREA
			#
			HDEW = HDEW_S
			TDEW = TDEW_S

			if(HAVE_NOT_USED_FULL_AREA) :
				CR  = Data.MREF*CPR
				#
				#        DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
				#
				if(CAIR  <=  CR) :
					CMINDS = CAIR
					CMAXDS = CR
				else:
					CMINDS = CR
					CMAXDS = CAIR
				#End if
				#
				#        DETERMINE THE NET HEAT TRANSFER
				#
				[EFFDSC,DEXDAR] = self.exf (1,ALEFT,Data.USUPE,CMINDS,CMAXDS)
				#CALL EXF(1,ALEFT,Data.USUPE,CMINDS,CMAXDS, EFFDSC,DEXDAR)
				QSUP = CMINDS*EFFDSC*(TS3 - TDEW)
				TAIR = TAIR + QSUP/CAIR

				ASUP = ALEFT
			#End if

			QTOTE = QSUP + QTPC
			ERROR_Q = abs(1 - QTOTE_LAST/QTOTE)
			QTOTE_LAST = QTOTE

			ERROR = abs(TAIR - TS3)
			if(ERROR  <  0.05  or  ERROR_Q  <=  0.01 or  ICOUNT  >=  10) :
				CONVERGED = True
				continue
			else:
				DEL_AIR = TAIR - TS3

				TAIR_NEW = TAIR_GUESS - 0.5*DEL_AIR
				if(TAIR_NEW  <=  T5_S): TAIR_NEW = 0.9*T5_S + 0.1*TAIR_GUESS
				TAIR = TAIR_NEW
				TAIR_GUESS = TAIR

				T5 = T5_S
				H5 = H5_S
				HDEW = HDEW_S
				TDEW = TDEW_S
				P5 = PIN

				QSUP = 0.0
				QTPC = 0.0
				ASUP = 0
				ATPC = 0

				HAVE_NOT_USED_FULL_AREA = True
			#End if
		#End Do
		#
		#      CALCULATE THE FRACTIONAL SUBCOOLING AND SUPERHEATING REGIONS
		#
		FSUPE = ASUP/Data.ATOTE

		Data.UAFF = Data.ATOTE * FSUPE * Data.USUPE + Data.ATOTE * (1.0 - FSUPE) * Data.UTPE

		return [QTOTE, FSUPE] 



