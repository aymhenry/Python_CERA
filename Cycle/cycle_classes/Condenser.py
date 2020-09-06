# Python import
import math, sys, datetime

# User import
from Data import Data
from Block2 import Block2

class Condenser (Block2):
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def ccount (self, TS1,T2,H2,  TDEW_S,HDEW_S,TBUB_S,  HBUB_S,CPRLIQ,  PIN,POUT,X, NUM_ZONE):
		#	  SUBROUTINE CCOUNT(TS1,T2,H2,  TDEW_S,HDEW_S,TBUB_S,  HBUB_S,CPRLIQ,
		#	 .     CPRVAP,    PIN,POUT,X, NUM_ZONE,       QDSC,QTPC,QSCC,QTOTC,FSUP,FSUB)
		#
		# Output  CPRVAP, QDSC, QTPC, QSCC, QTOTC, FSUP,FSUB
		# [P9, P14 to P19 ]= self.ccount (P1 to P8, P10,P11,P12,  P13   )
		#   ******************************************************************
		#   *    SUBROUTINE CCOUNT - CALCULATES THE CONDENSER HEAT EXCHANGE  *
		#   *    FOR COUNTERFLOW HEAT EXCHANGER                              *
		#   ******************************************************************
		#
		#	  LOGICAL LOOKING_FOR_AREA, HAVE_NOT_USED_FULL_AREA, ENTERS_WET
		#
		#	  REAL MDOTR use MREF
		#
		#	  DIMENSION X(5),XL(5),XV(5)
		#
		#	  COMMON/PARMS/Data.ICOND,Data.IFRSH,Data.IFREZ,Data.DISP,Data.SPEED,Data.CE,Data.CREF,MDOTR use MREF,Data.ETAV,Data.SEFF
		#	  COMMON/HTEXS/Data.CFMC,Data.CFME,CMFF,Data.UAF,Data.ETAC,Data.ETAE,Data.ETAF
		#	  COMMON/CONDEN/Data.UDSC,Data.UTPC,Data.USCC,Data.ATOTC,Data.UACOND
		#	  COMMON/SPECS/Data.DTSUPE,Data.DTSUBC
		#	  COMMON / DIAG / Data.IM, Data.IC,Data.IE
		#
		#	  COMMON/TLRNCE/Data.TOL_COND, Data.TOL_MASS, Data.TOL_FRSH, Data.TOL_FRZ, Data.TOL_HX,
		#	 .              Data.N_EVAP, Data.N_COND
		#
		AREA_TOL = 0.001
		#
		#        INITIALIZE
		#
		TBUB = TBUB_S
		HBUB = HBUB_S

		if(H2  <  HDEW_S) :
			HDEW = H2
			TDEW = T2
			ENTERS_WET = True
		else:
			HDEW = HDEW_S
			TDEW = TDEW_S
			ENTERS_WET = False
		#End if

		HDEW_START = HDEW
		TDEW_START = TDEW

		DELP = (PIN  - POUT)/float(NUM_ZONE)
		DELH = (HDEW - HBUB)/float(NUM_ZONE)
		PBUB = POUT

		QDSC = 0.0
		QTPC = 0.0
		QSCC = 0.0

		ASCC = 0
		ATPC = 0
		ADSC = 0

		TAIR = TS1
		CAIR = Data.CFMC
		HAVE_NOT_USED_FULL_AREA = True
		#
		#        START OFF WITH THE SUBCOOLING AREA.
		#
		if(Data.DTSUBC >  0.0) :
			TCSUB = TBUB - Data.DTSUBC
			CRSC = Data.MREF*CPRLIQ

			if(CAIR  <  CRSC) :
				CMINSC = CAIR
				CMAXSC = CRSC
			else:
				CMINSC = CRSC
				CMAXSC = CAIR
			#End if
			#
			#        IS AREA BIG ENOUGH FOR SUBCOOLING
			#
			QMAX = CMINSC*(TBUB - TAIR)
			QSUB = CRSC  *(TBUB - TCSUB)

			EFF_SUB = QSUB/QMAX
			[EFFSCC,DEXDAR] = self.ext (1,Data.ATOTC,Data.USCC,CMINSC,CMAXSC)
			#CALL EXF(1,Data.ATOTC,Data.USCC,CMINSC,CMAXSC,  EFFSCC,DEXDAR)

			if(EFFSCC  <  EFF_SUB) :                      #Need more area
				ASCC = Data.ATOTC
				HAVE_NOT_USED_FULL_AREA = False
				#
				#        BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
				#        SUBCOOLED REGION
				#
				#        INITIALIZE VARIABLES
				#
			else:
				ASCC = Data.ATOTC/10.0
				LOOKING_FOR_AREA = True

				while (LOOKING_FOR_AREA):
					[EFFSCC,DEXDAR] = self.ext (1,ASCC,Data.USCC,CMINSC,CMAXSC)
					#CALL EXF(1,ASCC,Data.USCC,CMINSC,CMAXSC, EFFSCC,DEXDAR)
					ERROR = abs(EFFSCC - EFF_SUB)

					if(ERROR  <  AREA_TOL) :
						LOOKING_FOR_AREA = False
						continue
					#End if

					DAREA = - (EFFSCC - EFF_SUB)/DEXDAR
					DAREA_MIN = -0.50 * ASCC
					DAREA_MAX =  0.50 * (Data.ATOTC - ASCC)

					if(DAREA  <  DAREA_MIN): DAREA = DAREA_MIN
					if(DAREA  >  DAREA_MAX): DAREA = DAREA_MAX

					ASCC  = ASCC + DAREA
				###End Do
			#End if

			QSCC = EFFSCC*QMAX
			TAIR = TAIR + QSCC/CAIR
		#End if

		#
		#        CONTINUE WITH TWO-PHASE AREA
		#
		ALEFT = Data.ATOTC - ASCC

		#N = 1
		for N in range (1, NUM_ZONE): # DO WHILE (N  <  NUM_ZONE)
			PDEW = PBUB + DELP
			HDEW = HBUB + DELH
			[TDEW,XQ,XL,XV,VL,VV,HL,HV] = self.hpin ( HDEW,PDEW,X )
			#CALL HPIN(HDEW,PDEW,X, TDEW,XQ,XL,XV,VL,VV,HL,HV)

			if(HAVE_NOT_USED_FULL_AREA) :
				CPRTP = (HDEW-HBUB)/ abs(TDEW-TBUB+0.0001)
				CRTP  = Data.MREF * CPRTP
				#
				#        DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
				#
				if(CAIR  <  CRTP) :
					CMINTP = CAIR
					CMAXTP = CRTP
				else:
					CMINTP = CRTP
					CMAXTP = CAIR
				#End if
				#
				#        IS AREA BIG ENOUGH FOR CONDENSATION
				#
				QMAX = CMINTP*(TDEW - TAIR)
				QDUM = Data.MREF*(HDEW - HBUB)

				EFF_TPC = QDUM/QMAX
				[EFFTPC,DEXDAR] = self.ext (1,ALEFT,Data.UTPC,CMINTP,CMAXTP)
				#CALL EXF(1,ALEFT,Data.UTPC,CMINTP,CMAXTP, EFFTPC,DEXDAR)


				if(EFFTPC  <  EFF_TPC or  (ENTERS_WET .AND. N  ==  NUM_ZONE)) :
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
						[EFFTPC,DEXDAR] = self.ext (1,ADUM,Data.UTPC,CMINTP,CMAXTP)
						#CALL EXF(1,ADUM,Data.UTPC,CMINTP,CMAXTP, EFFTPC,DEXDAR)
						ERROR = abs(EFFTPC - EFF_TPC)

						if(ERROR  <  AREA_TOL  or  ILOOK  >=  10) :
							OOKING_FOR_AREA = False
							continue
						#End if
						DAREA = - (EFFTPC - EFF_TPC)/DEXDAR
						DAREA_MIN = -0.75*ADUM
						DAREA_MAX = 0.50*(ALEFT - ADUM)

						if(DAREA  <  DAREA_MIN): DAREA = DAREA_MIN
						if(DAREA  >  DAREA_MAX): DAREA = DAREA_MAX

						ADUM  = ADUM + DAREA
					###End Do
					ATPC = ATPC + ADUM
				#End if

				QTPC = QTPC + EFFTPC*QMAX
				TAIR = TAIR + EFFTPC*QMAX/CAIR
			#End if

			ALEFT = Data.ATOTC - ASCC - ATPC
			HBUB = HBUB + DELH
			TBUB = TDEW
			PBUB = PBUB + DELP

			#N = N + 1
		###End Do

		if(ALEFT  <  0.0): HAVE_NOT_USED_FULL_AREA = False
		#
		#        CONTINUE WITH DESUPERHEATING AREA
		#
		HDEW = HDEW_START
		TDEW = TDEW_START

		if(HAVE_NOT_USED_FULL_AREA) :
			CPRVAP = (H2 - HDEW)/(T2 - TDEW)
			CRDS  = Data.MREF*CPRVAP
			#
			#        DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
			#
			if(CAIR  <  CRDS) :
				CMINDS = CAIR
				CMAXDS = CRDS
			else:
				CMINDS = CRDS
				CMAXDS = CAIR
			#End if
			#
			#        DETERMINE THE NET HEAT TRANSFER
			#
			[EFFDSC,DEXDAR] = self.exf (1,ALEFT,Data.UDSC,CMINDS,CMAXDS)
			#CALL EXF(1,ALEFT,Data.UDSC,CMINDS,CMAXDS  ,EFFDSC,DEXDAR)
			QDSC = CMINDS*EFFDSC*(T2 - TAIR)

			ADSC = ALEFT
		#End if
		#
		#      CALCULATE THE FRACTIONAL SUBCOOLING AND SUPERHEATING REGIONS
		#
		FSUP = ADSC/Data.ATOTC
		FSUB = ASCC/Data.ATOTC

		QTOTC = QSCC + QTPC + QDSC
		Data.UACOND = Data.ATOTC*(FSUP*Data.UDSC + FSUB*Data.USCC + (1.0 - FSUP - FSUB)*Data.UTPC)

		return [CPRVAP, QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def ccross (self, TS1,T2,H2,  TDEW_S,HDEW_S,TBUB_S,  HBUB_S,CPRLIQ,  PIN,POUT,X, NUM_ZONE):
		#	  SUBROUTINE CCROSS(TS1,T2,H2,TDEW_S,HDEW_S,TBUB_S,HBUB_S,CPRLIQ,
		#	 .     CPRVAP,    PIN,POUT,X, NUM_ZONE,     QDSC,QTPC,QSCC,QTOTC,FSUP,FSUB)
		# output	CPRVAP, QDSC, QTPC, QSCC, QTOTC, FSUP,FSUB
		#
		# [P9, P14 ... P19 ]= self.ccross (P1 to P8, P10,P11,P12,P13   )
		#   ******************************************************************
		#   *    SUBROUTINE CCOUNT - CALCULATES THE CONDENSER HEAT EXCHANGE  *
		#   *    FOR COUNTERFLOW HEAT EXCHANGER                              *
		#   ******************************************************************
		#
		#	  LOGICAL LOOKING_FOR_AREA, HAVE_NOT_USED_FULL_AREA, ENTERS_WET
		#
		#	  REAL MDOTR use MREF use MREF
		#
		#	  DIMENSION X(5),XL(5),XV(5)
		#
		#	  COMMON/PARMS/Data.ICOND,Data.IFRSH,Data.IFREZ,Data.DISP,Data.SPEED,Data.CE,Data.CREF,MDOTR use MREF use MREF,Data.ETAV,Data.SEFF
		#	  COMMON/HTEXS/Data.CFMC,Data.CFME,CMFF,Data.UAF,Data.ETAC,Data.ETAE,Data.ETAF
		#	  COMMON/CONDEN/Data.UDSC,Data.UTPC,Data.USCC,Data.ATOTC,Data.UACOND
		#	  COMMON/SPECS/Data.DTSUPE,Data.DTSUBC
		#	  COMMON / DIAG / Data.IM, Data.IC,Data.IE
		#
		#	  COMMON/TLRNCE/Data.TOL_COND, Data.TOL_MASS, Data.TOL_FRSH, Data.TOL_FRZ, Data.TOL_HX,
		#	 .              Data.N_EVAP, Data.N_COND
		#
		CPRVAP = 0.0 # in Python only

		AREA_TOL = 0.001
		#
		#        INITIALIZE
		#
		TBUB = TBUB_S
		HBUB = HBUB_S

		if(H2  <=  HDEW_S) :
			HDEW = H2
			TDEW = T2
			ENTERS_WET = True
		else:
			HDEW = HDEW_S
			TDEW = TDEW_S
			ENTERS_WET = False
		#End if

		HDEW_START = HDEW
		TDEW_START = TDEW

		DELP = (PIN  - POUT)/float(NUM_ZONE)
		DELH = (HDEW - HBUB)/float(NUM_ZONE)
		PBUB = POUT

		QDSC = 0.0
		QTPC = 0.0
		QSCC = 0.0

		ASCC = 0
		ATPC = 0
		ADSC = 0

		TAIR = TS1
		CAIR = Data.CFMC
		HAVE_NOT_USED_FULL_AREA = True

		# Start off with the subcooling area.
		if(Data.DTSUBC  >  0.0) :
			TCSUB = TBUB - Data.DTSUBC
			CRSC = Data.MREF*CPRLIQ

			if(CAIR  <=  CRSC) :
				CMINSC = CAIR
				CMAXSC = CRSC
			else:
				CMINSC = CRSC
				CMAXSC = CAIR
			#End if

			# is area big enough for subcooling
			QMAX = CMINSC * (TBUB - TAIR)
			QSUB = CRSC   * (TBUB - TCSUB)

			EFF_SUB = QSUB/QMAX

			[ EFFSCC,DEXDAR] = self.ext (2,Data.ATOTC,Data.USCC,CMINSC,CMAXSC)
			#CALL EXF(2,Data.ATOTC,Data.USCC,CMINSC,CMAXSC,  EFFSCC,DEXDAR)

			if(EFFSCC  <=  EFF_SUB) :                      #Need more area
				ASCC = Data.ATOTC
				HAVE_NOT_USED_FULL_AREA = False
				#
				#        BEGIN ITERATION PROCESS TO DETERMINE SOLUTION FOR THE
				#        SUBCOOLED REGION
				#
				#        INITIALIZE VARIABLES
				#
			else:
				ASCC = Data.ATOTC / 10.0
				LOOKING_FOR_AREA = True

				ICOUNT = 0
				QTOL = 1.0

				while (LOOKING_FOR_AREA):
					ICOUNT = ICOUNT + 1
					if(ICOUNT  >  100) :
						LOOKING_FOR_AREA = False
						continue
					#End if

					CAIR = (ASCC/Data.ATOTC)*Data.CFMC
					if(CAIR  <=  CRSC) :
						CMINSC = CAIR
						CMAXSC = CRSC
					else:
						CMINSC = CRSC
						CMAXSC = CAIR
					#End if

					QMAX = CMINSC*(TBUB - TAIR)
					EFF_SUB = QSUB/QMAX

					[ EFFSCC,DEXDAR] = self.ext (2,ASCC,Data.USCC,CMINSC,CMAXSC)
					#CALL EXF(2,ASCC,Data.USCC,CMINSC,CMAXSC,  EFFSCC,DEXDAR)

					ERROR = abs(QTOL)
					if(ERROR  <=  AREA_TOL) :
						LOOKING_FOR_AREA = False
						continue
					#End if

					QRAT = EFFSCC*QMAX/QSUB
					QTOL = 1.0 - QRAT

					DAREA = ASCC*(1.0 - QRAT)

					DAREA_MIN = -0.50*ASCC
					DAREA_MAX =  0.50*(Data.ATOTC - ASCC)

					if(DAREA  <  DAREA_MIN): DAREA = DAREA_MIN
					if(DAREA  >  DAREA_MAX): DAREA = DAREA_MAX

					ASCC  = ASCC + DAREA
				###End Do

			#End if

			QSCC = EFFSCC*CMINSC*(TBUB - TAIR)
		#End if
		#
		#        CONTINUE WITH TWO-PHASE AREA
		#
		ALEFT = Data.ATOTC - ASCC

		#N = 1
		for N in range (1, NUM_ZONE+1): #DO WHILE (N  <=  NUM_ZONE)
			PDEW = PBUB + DELP
			HDEW = HBUB + DELH
			[TDEW,XQ,XL,XV,VL,VV,HL,HV] = self.hpin ( HDEW,PDEW,X )
			#CALL HPIN(HDEW,PDEW,X  ,TDEW,XQ,XL,XV,VL,VV,HL,HV)

			if(HAVE_NOT_USED_FULL_AREA) :
				CPRTP = (HDEW-HBUB)/abs(TDEW-TBUB+0.0001)
				CRTP  = Data.MREF*CPRTP
				#
				#        DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
				#
				CAIR = (ALEFT/Data.ATOTC)*Data.CFMC
				if(CAIR  <=  CRTP) :
					CMINTP = CAIR
					CMAXTP = CRTP
				else:
					CMINTP = CRTP
					CMAXTP = CAIR
				#End if
				#
				#        IS AREA BIG ENOUGH FOR CONDENSATION
				#
				QMAX = CMINTP*(TDEW - TAIR)
				QDUM = Data.MREF*(HDEW - HBUB)

				EFF_TPC = QDUM/QMAX
				[EFFTPC,DEXDAR] = self.exf (2,ALEFT,Data.UTPC,CMINTP,CMAXTP)
				#CALL EXF(2,ALEFT,Data.UTPC,CMINTP,CMAXTP  ,EFFTPC,DEXDAR)

				if(EFFTPC  <=  EFF_TPC  or  ENTERS_WET) :      #Need more area
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

					ICOUNT = 0
					QTOL = 1.0
					while (LOOKING_FOR_AREA):
						ICOUNT = ICOUNT + 1
						if(ICOUNT  >  100) :
							LOOKING_FOR_AREA = False
							continue
						#End if

						CAIR = (ADUM/Data.ATOTC)*Data.CFMC
						if(CAIR  <=  CRTP) :
							CMINTP = CAIR
							CMAXTP = CRTP
						else:
							CMINTP = CRTP
							CMAXTP = CAIR
						#End if

						QMAX = CMINTP*(TDEW - TAIR)
						EFF_TPC = QDUM/QMAX

						[EFFTPC,DEXDAR] = self.exf (2,ADUM,Data.UTPC,CMINTP,CMAXTP)
						#CALL EXF(2,ADUM,Data.UTPC,CMINTP,CMAXTP,  EFFTPC,DEXDAR)

						ERROR = abs(QTOL)
						if(ERROR  <=  AREA_TOL) :
							LOOKING_FOR_AREA = False
							continue
						#End if

						QRAT  = EFFTPC*QMAX/QDUM
						QTOL = 1.0 - QRAT

						DAREA = ADUM*(1.0 - QRAT)

						DAREA_MIN = -0.75*ADUM
						DAREA_MAX = 0.50*(ALEFT - ADUM)

						if(DAREA  <  DAREA_MIN): DAREA = DAREA_MIN
						if(DAREA  >  DAREA_MAX): DAREA = DAREA_MAX

						ADUM  = ADUM + DAREA
					###End Do
					ATPC = ATPC + ADUM
				#End if

				QTPC = QTPC + EFFTPC*CMINTP*(TDEW - TAIR)
			#End if

			ALEFT = Data.ATOTC - ASCC - ATPC
			HBUB = HBUB + DELH
			TBUB = TDEW
			PBUB = PBUB + DELP
			#N = N + 1
		###End Do

		#
		#        CONTINUE WITH DESUPERHEATING AREA
		#
		ALEFT = Data.ATOTC - ASCC - ATPC
		if(ALEFT  <=  0.0): HAVE_NOT_USED_FULL_AREA = False

		HDEW = HDEW_START
		TDEW = TDEW_START

		if(HAVE_NOT_USED_FULL_AREA) :
			CPRVAP = (H2 - HDEW)/(T2 - TDEW)
			CRDS  = Data.MREF*CPRVAP
			#
			#        DETERMINE CMIN AND CMAX IN THE TWO-PHASE REGION
			#
			CAIR = (ALEFT/Data.ATOTC)*Data.CFMC

			if(CAIR  <=  CRDS) :
				CMINDS = CAIR
				CMAXDS = CRDS
			else:
				CMINDS = CRDS
				CMAXDS = CAIR
			#End if
			#
			#        DETERMINE THE NET HEAT TRANSFER
			#
			[EFFDSC,DEXDAR] = self.exf (2,ALEFT,Data.UDSC,CMINDS,CMAXDS)
			#CALL EXF(2,ALEFT,Data.UDSC,CMINDS,CMAXDS,  EFFDSC,DEXDAR)
			QDSC = CMINDS*EFFDSC*(T2 - TAIR)

			ADSC = ALEFT
		#End if
		#
		#      CALCULATE THE FRACTIONAL SUBCOOLING AND SUPERHEATING REGIONS
		#
		FSUP = ADSC/Data.ATOTC
		FSUB = ASCC/Data.ATOTC

		QTOTC = QSCC + QTPC + QDSC
		Data.UACOND = Data.ATOTC*(FSUP*Data.UDSC + FSUB*Data.USCC + (1.0 - FSUP - FSUB)*Data.UTPC)
		return  [  CPRVAP, QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def cnat (self, TS1,TS3,TS5,  T14,H14,T3, H3,T,TBUB, HBUB,CPRLIQ):
		# [ P12,P13,P14,P15, P16, P17 ] = self.cnat (P1 to P11 )
		#	input TS1, TS3, TS5, T14,H14,T3, H3,T,TBUB, HBUB,CPRLIQ
		# 	output QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB
		#
		#	SUBROUTINE CNAT(TS1,TS3,TS5,  T14,H14,T3, H3,T,TBUB, HBUB,CPRLIQ,  QDSC,
		#	 .                QTPC,QSCC,QTOTC,FSUP,FSUB)
		#    ******************************************************************
		#    *     SUBROUTINE CNAT - CALCULATES THE CONDENSER HEAT            *
		#    *     TRANSFER FOR A NATURAL CONVECTION CONDENSER                *
		#    ******************************************************************
		#
		#	REAL MDOTR use MREF
		#	DIMENSION T(1)
		#	COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MDOTR use MREF,ETAV,SEFF
		#	COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
		#	COMMON/CONDEN/UDSC,UTPC,USCC,ATOTC,UACOND
		#	COMMON/SPECS/DTSUPE,DTSUBC
		#	COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
		#	 .                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
		#	 .                  CONDF_IN_WALL, CONDZ_IN_WALL


		SIGMA = 2.04326E-7
		#
		# Segregate the desuperheating region for the heat transfer coefficient
		#  calculation since the temperature dif ference is high
		#
		# Set up the hdew and tdew parameters.  account for a wet gas entering the condenser

		if (H3 < H14) :
			TDEW = T3
			HDEW = H3
		else:
			TDEW = T14
			HDEW = H14

		# calculate the radiation heat transfer, heat transfer coefficient
		#  using small delta t approximation (black body)
		#  use the arithmetic average of temperature to evaluate h
		#  radiation in the desuperheating region
		#
		TAVE = (T14+TDEW)/2.0
		T1 = TAVE*1.8 - 459.6
		HRAD = 4.*SIGMA*TAVE**3
		HRAD = SIGMA*(TAVE + TS1)*(TAVE**2 + TS1**2)

		# calculate the natural convection heat transfer coefficient
		#
		DELTAT = TAVE - TS1
		if (DELTAT < 0.0): DELTAT = 0.0001

		DELTA = DELTAT*1.8
		HNAT = 0.19*DELTA**0.33*20.44
		#
		#         CALCULATE COMBINED AIR-SIDE HEAT TRANSFER COEFFICIENT
		#
		UAIR = HRAD + HNAT
		U1 = UAIR*0.04892
		#
		#         CALCULATE THE HEAT TRANSFER ASSUMING THAT THE AIR SIDE
		#         RESISTANCE DOMINATES
		#
		#         CALCULATE THE AREA NECESSARY TO DESUPERHEAT THE REFRIGERANT
		#
		TCND = 0.2*T14 + 0.4*TDEW + 0.4*TBUB
		Data.Q_CND_FF = 1.8 * Data.UA_FF_CND*(TCND - TS3)*1.0548
		Data.Q_CND_FZ = 1.8 * Data.UA_FZ_CND*(TCND - TS5)*1.0548

		TFZ = 0.5*(T(8) + T(9))
		TFF = 0.5*(T(5) + T(12))
		Data.Q_HXS_FF = 1.8 * Data.UA_FF_HXS*(TCND - TFF)*1.0548
		Data.Q_HXS_FZ = 1.8 * Data.UA_FZ_HXS*(TCND - TFZ)*1.0548

		if (TS5 < -290.0) : Data.Q_CND_FZ = 0
		if (TS5 < -290.0) : Data.Q_HXS_FZ = 0

		Q_IN_WALL = (Q_CND_FF + Data.Q_CND_FZ)/Data.ATOTC + (Data.Q_HXS_FF + Data.Q_HXS_FZ)/Data.ATOTC

		QDSNEC = Data.MREF*(H14-HDEW)
		ADSNEC = QDSNEC/(UAIR*DELTAT + Q_IN_WALL)

		if (ADSNEC > Data.ATOTC): ADSNEC = Data.ATOTC

		QDSC = UAIR * ADSNEC * DELTAT + ADSNEC * Q_IN_WALL
		FSUP = ADSNEC/Data.ATOTC
		QTPC = 0.0
		QSCC = 0.0

		QTOTC = QDSC + QTPC + QSCC

		FSUB = 0.0
		Data.UDSC = UAIR
		Data.UACOND = Data.ATOTC * Data.UDSC

		if (FSUP == 1.0): return [QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]

		# calculate the heat transfer coefficients for the two-phase and subcooling regions
		TAVE = (TDEW+TBUB)/2.0
		T2 = TAVE*1.8 - 459.6
		HRAD = 4.*SIGMA*TAVE**3
		HRAD = SIGMA*(TAVE + TS1)*(TAVE**2 + TS1**2)

		# calculate the natural convection heat transfer coefficient
		DELTAT = TAVE - TS1

		if (DELTAT < 0.0): DELTAT = 0.0001

		DELTA = DELTAT*1.8
		HNAT = 0.19*DELTA**0.33*20.44

		# calculate combined air-side heat transfer coefficient
		UAIR = HRAD + HNAT
		U2 = UAIR*0.04892

		# calculate the heat transfer necessary to condense the refrigerant
		QTPNEC = Data.MREF*(HDEW-HBUB)

		# calculate the remaining surface area
		ANET = Data.ATOTC - ADSNEC

		# calculate the actual heat transfer in the two-phase region

		QTPC = UAIR*ANET*DELTAT + ANET*Q_IN_WALL

		if (QTPC > QTPNEC): QTPC = QTPNEC

		# calculate the area in the two-phase region
		ATPC = QTPC/(UAIR*DELTAT + Q_IN_WALL)
		QSCC = 0.0
		QTOTC = QDSC + QTPC + QSCC
		FSUB = 0.0
		Data.UTPC = UAIR
		Data.UACOND = Data.ATOTC*(FSUP * Data.UDSC + (1.0 - FSUP) * Data.UTPC)
		if (QTPC < QTPNEC): return [QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]

		# calculate the remaining surface area
		ANET = Data.ATOTC - (ADSNEC+ATPC)

		# calculate the heat transfer if the refrigerant exits at ts1
		QSCMAX = Data.MREF*CPRLIQ*(TBUB-TS1)

		# calculate the subcooling heat transfer
		QSCC = UAIR*ANET*DELTAT + ANET*Q_IN_WALL

		if (QSCC .GT. QSCMAX): QSCC = QSCMAX

		QTOTC = QDSC + QTPC + QSCC
		FSUB = ANET/Data.ATOTC
		Data.USCC = UAIR
		Data.UACOND = Data.ATOTC*(FSUP * Data.UDSC + FSUB * Data.USCC + (1.0 - FSUP - FSUB)*Data.UTPC)
		return [QDSC, QTPC, QSCC, QTOTC, FSUP, FSUB]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def cond (self,  T,H,TS1, TC,  QCONDS,QCONDC,QSCC,  JC,  ICONC ):
		#	SUBROUTINE COND(T,H,TBUB,  HBUB,TS1,TS2,  TC,CPRLIQ,QCONDS,  QCONDC,
		#	 .     QSCC,JC,ICONC)
		#	TBUB,HBUB, CPRLIQ (P3, P4, P8) is not used
		# input  T,H, TS1, TC, QCONDS,QCONDC, QSCC, JC
		# output  TS2, TC, JC, ICONC
		#
		# [ P6, P7, P12, P13] = self.cond (P1,P2, P5, P7, P9 to P13 )
		#     *****************************************************************
		#     *    CALCULATE CONDENSER EXIT TEMPERATURE                       *
		#     *****************************************************************
		#
		#REAL MREF,MROLD
		#DIMENSION H(16),T(16),TC(3)
		#COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
		#COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
		#COMMON/SPECS/DTSUPE,DTSUBC
		#COMMON/CONDEN/UDSC,UTPC,USCC,ATOTC,UACOND
		#COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX
		#COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
		#	 .                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
		#	 .                  CONDF_IN_WALL, CONDZ_IN_WALL
		#
		#         INITIALIZE
		#

		ICNT = 0 # Python only to be checked
		#print (" in Python only, to check later, ICNT set in cond method ", ICNT)
		MROLD = 0# Python only

		ICONC = 0
		if(JC == 1) :
			ICNT = 0
			MROLD = 0

		# Python: MREF:Initial Guess For Refrigerant Mas Flow Rate (kg/hr)
		# Python: QSCC: condenser SUBCOOLING HEAT TRANSFER
		# Estimate new value for exit temperature
		QREF  = Data.MREF * ( H[14] - H[4]) # Python Commnet : MREF-Initial Guess For Refrigerant Mas Flow Rate (kg/hr)
		QCOND = QCONDS + QCONDC + QSCC
		EPS = QREF - QCOND
		DELT = EPS/Data.UACOND

		if(DELT > 5.0)  : DELT = 5.0
		if(DELT < -5.0) : DELT = -5.0

		TCOUT = TC[JC] + DELT
		TS2 = TS1 + (QCONDS + QCONDC +QSCC)/ Data.CFMC

		if(Data.ICOND == 0): TS2 = 0.9*T[4] + 0.1 * TS1
		if(TCOUT < TS1): TCOUT = (TS1 + TC[JC] )/2.0

		if(ICNT <= 2) :
			TCNEW = TCOUT
		else:
			if ((TCOUT > TC[1] and TC[1] > TC[2]) or (TCOUT < TC[1] and TC[1] < TC[2] )) :
				TCNEW = 0.5 * (TC[1] + TC[2])
			else:
				TCNEW = TCOUT

			if(TCNEW < TS1): TCNEW = (TS1 + TC[JC] )/2.0

			TC[1] = TC[2]

		# Check convergence
		ERRORT = abs( TCNEW - TC[JC] )
		ERRORM = abs( Data.MREF - MROLD)/Data.MREF # useless allways 1, MROLD is not given and =0

		if(ERRORT < Data.TOL_COND and ERRORM <= Data.TOL_MASS): ICONC = 1

		JC = 2
		ICNT = ICNT + 1 # useless
		TC[JC] = TCNEW
		MROLD = Data.MREF #useless

		#print ("aym ====.................. DELT=", DELT, " TC=", TC)
		return [ TS2, TC, JC, ICONC]

