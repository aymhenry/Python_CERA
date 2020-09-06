# Python import
import math, sys, datetime
from abc import ABC,abstractmethod

# User import
from Data import Data
from Block2 import Block2

from FileAccess import FileAccess
from Compressor import *
from Adjlod import Adjlod

from EvapType import *
from EvapCooling import *
from CondCooling import *

#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
class Cycle (Adjlod, HeatExch):
	FILE_ERROR_OUT = 'ERROR.OUT'
	FILE_CYCLE_OUT = "CYCLE.OUT"

	obj_data = None
	
	def __init__ (self, obj_data):
		Cycle.obj_data = obj_data
		
		Cycle.obj_data.FSUBC = 0.0 # in Python only, it has no value in Fortant
		Cycle.obj_data.FSUPE = 0.0 # in python only
		Cycle.obj_data.FSUPC = 0.1
		Cycle.obj_data.TEMIN = 210.0
		
		
		Cycle.obj_data.JE = 0
		Cycle.obj_data.JC = 0
		Cycle.obj_data.ITMAXC = 100
		Cycle.obj_data.ITMAXE = 40
		
		Cycle.obj_data.TS = [0.0] *(16+1)
		Cycle.obj_data.WMAVGL = [0.0] *(16+1)
		Cycle.obj_data.WMAVGV = [0.0] *(16+1)
		Cycle.obj_data.AIRTMP = [0.0] *(16+1)		

		Cycle.obj_data.XREF= [0.0] *(5+1)

		Cycle.obj_data.XV = [[-9999999.9] * (16+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		Cycle.obj_data.XL = [[-9999999.9] * (16+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
			
		Cycle.obj_data.XL_Temp = [0.0] * len(Cycle.obj_data.XL) # in python only
		Cycle.obj_data.XV_Temp = [0.0] * len(Cycle.obj_data.XV) # in python only
		Cycle.obj_data.XV15 = [0.0] * (5+1) # in Python only
		
		Cycle.obj_data.T = [-9999999.9] * (16+1)
		Cycle.obj_data.S = [-9999999.9] * (16+1)
		Cycle.obj_data.H = [-9999999.9] * (16+1)
		Cycle.obj_data.XQ= [-9999999.9] * (16+1)

		Cycle.obj_data.P = [-9999999.9] * (16+1)
		Cycle.obj_data.V = [-9999999.9] * (16+1)
		Cycle.obj_data.X = [0.0] * (5+1)
		Cycle.obj_data.VL= [-9999999.9] * (16+1)
		Cycle.obj_data.VV= [-9999999.9] * (16+1)

		Cycle.obj_data.TE= [0.0] * (3+1)
		Cycle.obj_data.TC= [0.0] * (3+1)
		
		Cycle.obj_data.AIRTMP = [True,  #add new item to start list at 1	\
			False, False, True,  False, False,	\
			True,  False, False, False, False,	\
			False, True,  False, True,  False]
			
		# in python add extra item for item 0
		Cycle.obj_data.HSTATE = ['','COMP IN','COMP DIS','COND IN','COND DEW',	\
					'COND BUB','COND OUT','LIQ LINE',	\
					'SUBCOOL1','SUBCOOL2','FREZ IN ','FREZ OUT','FRSH IN ',	\
					'FRSH DEW','FRSH OUT','HX1 OUT ']

		# in python add extra item for item 0
		Cycle.obj_data.MSTATE = ['','COMP IN','COMP DIS','COND IN','COND DEW',	\
					'COND BUB','COND OUT','LIQ LINE',			\
					'SUBCOOL ','EVAP IN ', 'EVAP DEW','EVAP OUT','HX OUT  ']		

		# in python add extra item for item 0
		Cycle.obj_data.LPNT  = [0,1,2,14,3,11,4,16,6,10,8,9,5,12,7,13]
		
		#create basic objects according to the specifications of cycle
		self.create_basic_obj()
		
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def create_basic_obj (self):
		self.objEvapType = None  # evaporator type
		self.objEvapCool = None  # evaporator cooling method
		self.objCondType = None  # Condenser cooling method
		self.objCompType = None  # Condenser cooling method
		
		if (Data.IMAP ==  0) : #Map
			self.objCompType = Comp_Map(Cycle.obj_data)
			
		elif (Data.IMAP  ==  1) : # ERR
			self.objCompType = Comp_ERR(Cycle.obj_data)
			
		else: # Efficiency Model
			self.objCompType = Comp_EMOD(Cycle.obj_data)
			
		#----------------------------
		if (Data.ICOND ==  0) :
			self.objCondType = CondCool_CNat(Cycle.obj_data)
			
		elif (Data.ICOND  ==  1) :
			self.objCondType = CondCool_CCross(Cycle.obj_data)
			
		else : #(Data.ICOND  ==  2) :
			self.objCondType = CondCool_CCount(Cycle.obj_data)
			
		#----------------------------
		if (Data.IFRSH  ==  0) :
			self.objEvapCool = EvapCool_FFNat(Cycle.obj_data)
			
		elif (Data.IFRSH  ==  1) :
			self.objEvapCool = EvapCool_FFCross(Cycle.obj_data)
			
		else : #(Data.IFRSH  ==  2) :
			self.objEvapCool = EvapCool_FFCount(Cycle.obj_data)
			
		#----------------------------
		if (Data.ISPEC  ==  1) :
			self.objEvapType = EvapSuper(Cycle.obj_data)
			
		elif (Data.ISPEC  ==  2) :
			self.objEvapType = EvapIHX(Cycle.obj_data)
			
		else :	#(Data.ISPEC  ==  3) :
			self.objEvapType = EvapQual(Cycle.obj_data)
	
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	#def cycle (self, NC,IR,XM,F, TS1,TS3,TS5, MEFF,QHILO,QCAN,	\
	#			DPC,DPE,DPF,ETHX1,ETHX2,DISPLC,NCYC,FROSTF,FROSTZ,ICAB,	\
	#			IRFTYPE,ICYCL,ICYCLS,IDFRST):
	
	def cycle (self):
		print ("aym Cycle.obj_data.NC,",Cycle.obj_data.NC)
		print ("aym Cycle.obj_data.IR,",Cycle.obj_data.IR)
		print ("aym Cycle.obj_data.TS1,",Cycle.obj_data.TS1, Cycle.obj_data.TS1-273.11)
		print ("aym Cycle.obj_data.TS3,",Cycle.obj_data.TS3, Cycle.obj_data.TS3-273.11)
		print ("aym Cycle.obj_data.TS5,",Cycle.obj_data.TS5)
		print ("aym Cycle.obj_data.MEFF,",Cycle.obj_data.MEFF)
		print ("aym Cycle.obj_data.IRFTYPE,",Cycle.obj_data.IRFTYPE)
		print ("aym Cycle.obj_data.ICYCL,",Cycle.obj_data.ICYCL)
		print ("aym Cycle.obj_data.ICYCLS,",Cycle.obj_data.ICYCLS)
		print ("aym Cycle.obj_data.NCYC,",Cycle.obj_data.NCYC)
		print ("aym Cycle.obj_data.ICAB,",Cycle.obj_data.ICAB)
		print ("aym Cycle.obj_data.DPC,",Cycle.obj_data.DPC)
		print ("aym Cycle.obj_data.DPE,",Cycle.obj_data.DPE)
		print ("aym Cycle.obj_data.DPF,",Cycle.obj_data.DPF)
		print ("aym Cycle.obj_data.DISPLC,",Cycle.obj_data.DISPLC)
		print ("aym =====\n\n")
		
		#	******************************************************************
		#	*	CYCLE ANALYSIS FOR ONE OR TWO EVAPORATOR SYSTEM. BASED	*
		#	*	ON CYCLE7 PROGRAM DEVELOPED BY NIST.  MODIFIED BY ADL		*
		#	*	TO REPRESENT LORENZ CYCLE AND TO INCORPORATE HEAT			*
		#	*	EXCHANGER ALGORITHMS AND A COMPRESSOR MODEL.					*
		#	******************************************************************
		#
		#	UNITS:
		#			SI UNITS ARE USED THROUGHOUT;  INPUTS AND OUTPUTS ARE
		#			ON A MASS BASIS WHILE MOST INTERNAL CALCULATIONS ARE ON A
		#			MOLAR BASIS
		#
		#			PRESSURE - KPA
		#			TEMPERATURE - K
		#			ENTHALPY - KJ/KG
		#			ENTROPY - KJ/KG K
		#			VOLUME - M**3/KG
		#
		#	INPUTS:
		#			NC - NUMBER OF COMPONENTS
		#			IR[I] - CODE NUMBERS FOR THE I'TH COMPONENT OF THE
		#						REFRIGERANT MIXTURE (REFER TO PROPERTIES
		#						DOCUMENTATION)
		#			F(I1,I2) - MIXTURE INTERACTION PARAMETER BETWEEN COMPONENT
		#							I1 AND I2
		#			XM[I] - COMPOSITION OF CIRCULATING REFRIGERANT (MASS
		#						FRACTION OF IR[I])
		#			TS1 - HEAT TRANSFER FLUID (HTF) TEMPERATURE ENTERING CONDENSER
		#			TS3 - HTF TEMPERATURE ENTERING FRESH FOOD EVAPORATOR
		#			TS5 - HTF TEMPERATURE ENTERING FREEZER EVAPORATOR
		#			MEFF - MECHANICAL EFFICIENCY
		#			QHILO - NORMALIZED HEAT LOSS FROM DISCHANGE LINE INSIDE
		#						THE COMPRESSOR SHELL TO SUCTION GAS
		#			QCAN - COMPRESSOR SHELL LOSS NORMALIZED TO POWER INPUT
		#			Cycle.obj_data.DPC - PRESSURE DROP THROUGH CONDENSER
		#			DPE - PRESSURE DROP THROUGH FRESH FOOD EVAPORATOR
		#			DPF - PRESSURE DROP THROUGH FREEZER EVAPORATOR
		#			ETHX1 - EFFECTIVENESS OF HIGH TEMP INTERCHANGER
		#			ETHX2 - EFFECTIVENESS OF LOW  TEMP INTERCHANGER
		#			DISPLC - COMPRESSOR DISPLACEMENT (CU-IN)
		#			NCYC - NUMBER OF CALL TO CYCLE (1 OR 2 FOR DUAL LOOP)
		#			ICAB - FLAG TO REPRESENT PRESENCE OF CABINET LOADS IN INPUT
		#			ICYCL - CYCLE TYPE (1=STANDARD, 2=LORENZ, 3=DUAL LOOP, 4=DUAL EVAP)
		#			ICNTRL - CONTROL METHOD FOR EVAPORATOR LOAD
		#						0 = NONE
		#						1 = FRESH FOOD FAN OFF
		#						2 = FRESH FOOD EVAPORATOR SUPERHEAT CONTROL
		#						3 = FREEZER FAN OFF
		#						4 = FREEZER AIR DAMPER CONTROL
		#
		#	PROPERTY ROUTINES REFERENCED:
		#			BCONST - INITIALIZES ARRAYS OF PROPERTY COEFFICIENTS
		#			BUBLP - SATURATION PROPERTIES AT GIVEN PRESSURE
		#			BUBLT - SATURATION PROPERTIES AT GIVEN TEMPERATURE
		#			ENTROP - MOLAR ENTROPY
		#			ESPAR - SET UP COEFFICIENTS FOR EQUATION OF STATE
		#			HCVCPS - MOLAR ENTHALPY AND HEAT CAPACITY
		#			HPIN - TEMPERATURE, QUALITY, ETC. AS A FUNCTION OF ENTHALPY
		#					AND PRESSURE
		#			SPIN - TEMPERATURE, QUALITY, ETC. AS A FUNCTION OF ENTROPY
		#					AND TEMPERATURE
		#			VIT - CALCULATE SPECIFIC VOLUME
		#
		#	NOTE:  THE ABOVE ROUTINES REFERENCE ADDITIONAL PROPERTY ROUTINES
		#				THE ENTIRE SET SHOULD BE INCLUDED IN THE EXECUTABLE ELEMENT
		#
		#	ADDITIONAL SUBROUTINES REFERENCED:
		#			CCROSS - CROSS FLOW CONDENSER
		#			CCOUNT - COUNTER FLOW CONDENSER
		#			COMP - COMPRESSOR MODEL
		#			COND - CONDENSER ALGORITHMS
		#			FFCROSS - CROSS FLOW EVAPORATOR
		#			FFCOUNTER - COUNTER FLOW EVAPORATOR
		#			FRSH - FRESH FOOD EVAPORATOR ALGORITHMS
		#			LOWEVP - FREEZER EVAPORATOR ALGORITHMS
		#			PROGRS - DISPLAY CURRENT VALUES ON SCREEN
		#			SHWFIG - DISPLAY A DRAWING OF THE CYCLE ON THE SCREEN
		#			SHWOUT - PRINT OUT CALCULATED VALUES OT THE SCREEN
		#
		#	NOMENCLATURE FOR FIRST LETTER(S) OF VARIABLE NAMES:
		#			DT - TEMPERATURE DIFFERENCE
		#			FT - CONVERGENCE VARIABLE, LOOP HAS CONVERGED WHEN FT=0
		#			H - ENTHALPY
		#			HREF - CHARACTER VARIABLE FOR REFRIGERANT NAMES
		#			HSTATE - CHARACTER VARIABLE FOR CYCLE STATE POINTS:
		#						(INLET AND OUTLET REFER TO REFRIGERANT FLOW)
		#				1 - COMPRESSOR INLET (SATURATED VAPOR)
		#				2 - COMPRESSOR DISCHARGE
		#				3 - CONDENSER DEW POINT
		#				4 - CONDENSER OUTLET
		#				5 - INLET TO FRESH FOOD EVAPORATOR
		#				6 - LIQUID LINE OUTLET FROM HIGH TEMP INTERCHANGER
		#				7 - OUTLET FROM FRESH FOOD EVAPORATOR
		#				8 - INLET TO FREEZER EVAPORATOR
		#				9 - OUTLET FROM FREEZER EVAPORATOR
		#				10 - LIQUID LINE OUTLET FROM LOW TEMP INTERCHANGER
		#				11 - CONDENSER BUBBLE POINT
		#				12 - FRESH FOOD EVAPORATOR DEW POINT
		#				13 - SUPERHEATED GAS LEAVING THE HIGH TEMP INTERCHANGER
		#				14 - CONDENSER INLET
		#				15 - INTERNAL VARIABLE (NOT SHOWN) FOR EVAP DEW POINT
		#				16 - LIQUID LINE STATE AFTER HEAT LOSS TO CABINET AND MULLION
		#
		#			L - LOGICAL VARIABLE (ERROR FLAG, ETC.)
		#			P - PRESSURE
		#			T - TEMPERATURE
		#			TC - REFRIGERANT AT CONDENSER OUTLET
		#			TE - REFRIGERANT AT EVAPORATOR OUTLET
		#			TOL - CONVERGENCE TOLERANCE
		#			TS - TEMPERATURE OF HEAT TRANSFER FLUID
		#			V - VOLUME
		#			X - COMPOSITION
		#			XL(I,J) - LIQUID PHASE COMPOSITION AT STATE J
		#			XV(I,J) - VAPOR PHASE COMPOSITION AT STATE J
		#			XQ - QUALITY
		#

		IRET = 6
		#	!! Don't allow adjustment to freezer evap if not one-door refrigerator
		if (Cycle.obj_data.IRFTYPE  !=  6): IRET = 7

		IRFTYP = Cycle.obj_data.IRFTYPE
		if (Cycle.obj_data.IRFTYPE  ==  7): IRFTYP = 1

		#	SET UP LOGICAL VECTOR ON AIR TEMPERATURES
		if (Cycle.obj_data.ICYCL  ==  2) :
			Cycle.obj_data.AIRTMP[10] = True
			Cycle.obj_data.AIRTMP[11] = True
		else:
			Cycle.obj_data.AIRTMP[10] = False
			Cycle.obj_data.AIRTMP[11] = False

		#	OPEN OUTPUT FILE
		objCycOut = FileAccess (Cycle.FILE_CYCLE_OUT,  "write")  # IO_Cycle Tag
		objError = FileAccess (Cycle.FILE_ERROR_OUT, "write" ) # IM tag

		objError.write_or_terminate ('ENTERING SUBROUTINE CYCLE')
		objCycOut.write_or_terminate (" ") 

		now = datetime.datetime.now( )

		if (Cycle.obj_data.NCYC  !=  2) :
			objCycOut.write_or_terminate( ( now.strftime( "%H %M %S %d %b %Y" ) ) + " - Python Output aymhenry@gmail")

		#	OUTPUT INFORMATION ON TYPE OF CYCLE
		if (Cycle.obj_data.ICYCL  ==  1) :
			if (Cycle.obj_data.ICYCLS  ==  1) :
				objCycOut.write_or_terminate('STANDARD ONE EVAPORATOR CYCLE')
			else:
				if (Data.ITYPE  ==  1  or  Data.ITYPE  ==  4) :
					objCycOut.write_or_terminate('DUAL EVAP CYCLE: FRESH FOOD LOOP')
				else:
					objCycOut.write_or_terminate('DUAL EVAP CYCLE: FREEZER LOOP')

		if (Cycle.obj_data.ICYCL  ==  2) :
			if (Cycle.obj_data.ICYCLS  ==  2) :
				objCycOut.write_or_terminate('LORENZ CYCLE')
			else:
				objCycOut.write_or_terminate('DUAL EVAP CYCLE')

		if (Cycle.obj_data.ICYCL  ==  3) :
			if (Cycle.obj_data.NCYC  ==  1) :
				objCycOut.write_or_terminate('DUAL LOOP CYCLE - FREEZER')
			else:
				objCycOut.write_or_terminate(" ")
				objCycOut.write_or_terminate('DUAL LOOP CYCLE - FRESH FOOD')

		objCycOut.write_or_terminate(" ")

		if (Data.ITYPE  ==  3): Data.ITYPE = 1

		#	OUTPUT REFRIGERATION MIXTURE INFORMATION
		X2 = 100.0 * Cycle.obj_data.XM[1]
		objCycOut.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(X2, Data.HREF[ Cycle.obj_data.IR[1] ])  )

		if (Cycle.obj_data.NC >  1) :
			for I in range (2, Cycle.obj_data.NC+1) : 
				X2 = 100.0 * Cycle.obj_data.XM[I]
				objCycOut.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(X2, Data.HREF[ Cycle.obj_data.IR[1] ])  ) # fixed in python
				

		objCycOut.write_or_terminate(" ")
		objCycOut.write_or_terminate("OUTPUT RESULTS")

		#	INITIALIZE COMPRESSOR MAP ANALYSIS
		DUTYR = 0.5
		Data.IREAD = 0
		Data.DISPL = Cycle.obj_data.DISPLC
		
		[Data.EFFC, Data.CE ] = self.objCompType.map(Data.ICOMP, Data.ICOOL, Data.EER, Data.SIZE, Data.DISPL, Data.SPEEDN)
		#if (Data.IMAP  ==  1) :
		#	[Data.EFFC, Data.CE ] =self.map(Data.ICOMP, Data.ICOOL, Data.EER, Data.SIZE, Data.DISPL, Data.SPEEDN)

		#	INITIALIZE THERMODYNAMIC DATA
		self.bconst (Cycle.obj_data.NC,Cycle.obj_data.IR,Cycle.obj_data.F)

		#	CONVERT TO MOLAR COMPOSITION FOR THE CALCULATION OF ALL PROPERTIES
		WMSUM = 0.0
		WMAVG = 0.0

		for I in range (1, Cycle.obj_data.NC+1) : 
			WMSUM = WMSUM+Cycle.obj_data.XM[I]/Data.WM[I]

		for I in range (1, Cycle.obj_data.NC+1) : 
			Cycle.obj_data.X[I] = Cycle.obj_data.XM[I]/Data.WM[I] / WMSUM
			WMAVG = WMAVG+Cycle.obj_data.X[I] * Data.WM[I]

		#	INITIAL GUESSES FOR TC AND TE
		#	ASSUME TEMP RISE OF COND IS 0.5 F PER LBM
		Cycle.obj_data.TC[1] = Cycle.obj_data.TS1 + Data.MREF/3.6

		#	GUESS A DEW POINT TEMPERATURE AT THE EVAPORATOR EXIT
		self.objEvapType.calc_evap_exit_p7()

		Cycle.obj_data.JC = 1
		LCCON = True
		LQUIT = False

		#	SET UP TEMPERATURES AND CABINET LOADS FOR INLET TEMPERATURE
		#	CALCULATION FOR EVAPORATOR OF A STANDARD DESIGN (TYPE 1)
		TFF = Data.FFTEMP
		TFZ = Data.FZTEMP

		#	INITIAL GUESS FOR REFRIGERANT MASS FLOW (ASSUME 10 LB/HR)
		Data.FLOW = Data.MREF
		FLWREF = Data.FLOW
		FLOW2 = Data.FLOW
		Data.MREF = Data.MREF/(2.20462*WMAVG)
		MREFSV= Data.MREF

		#	BEGIN ITERATION FOR CONDENSER OUTLET TEMPERATURE
		self.shwfig(Cycle.obj_data.ICYCL)
		ICONC = 0

		#	BEGIN MAIN ITERATION LOOP ON THE CONDENSER TEMPERATURE
		Data.IC = 1
		Cycle.obj_data.ITMAXC = 3 # 3#100
	
		self.showError ("Python, check later, ICNT set in cond method, limit Cycle.obj_data.ITMAXC to ", Cycle.obj_data.ITMAXC )
		
		while (Data.IC  <=  Cycle.obj_data.ITMAXC  and  LCCON): 
			
			if (Cycle.obj_data.ICAB  ==  1) :
				[Cycle.obj_data.TS3,Cycle.obj_data.TS5] = self.adjlod(Cycle.obj_data.ICYCL,Data.IC,  Cycle.obj_data.TS3,Cycle.obj_data.TS5,  Cycle.obj_data.FROSTF,Cycle.obj_data.FROSTF,Cycle.obj_data.IDFRST)

			Cycle.obj_data.T[4] = Cycle.obj_data.TC[Cycle.obj_data.JC]

			#	find condenser pressure for current guess of TC
			#	if bublt routine exceeds the critical point write a warning message and return
			self.showMsg ("Iteration number for CONDENSER TEMPERATURE ", Data.IC)

			TBUB4 = Cycle.obj_data.TC[Cycle.obj_data.JC] + Data.DTSUBC

			[Cycle.obj_data.X, Cycle.obj_data.XV_Temp, Cycle.obj_data.P[4], VBUB4, Cycle.obj_data.VV[4], LCRIT] = self.bublt (TBUB4,Cycle.obj_data.X , Cycle.obj_data.XV_Temp, True) 	
			self.setArr2dCol (Cycle.obj_data.XV, 4, Cycle.obj_data.XV_Temp )
			
			#	determine the specific volume of the liquid
			if (Data.DTSUBC >  0.0) :
				VGUESS=VBUB4
				[A4, B4] = self.espar (0, Cycle.obj_data.TC[Cycle.obj_data.JC], Cycle.obj_data.X) #	CALL ESPAR(0,Cycle.obj_data.TC[JC],Cycle.obj_data.X, A4,B4)

				[VGUESS, LCONV] = self.vit (Cycle.obj_data.TC[Cycle.obj_data.JC],Cycle.obj_data.P[4],A4,B4, VGUESS,True) 
				Cycle.obj_data.V[4]=VGUESS
			else:
				Cycle.obj_data.V[4]=VBUB4

			#	condenser dew point
			Cycle.obj_data.P[3] = Cycle.obj_data.P[4] + (1.0 - Cycle.obj_data.FSUPC) * Cycle.obj_data.DPC
			self.showMsg ("Conderser Temp TC (C) ", Cycle.obj_data.TC[Cycle.obj_data.JC] - 273.11 )

			if (LCRIT) :
				objCycOut.write_or_terminate ('CRITICAL TEMPERATURE EXCEEDED IN CONDENSER')
				objCycOut = "" # close file
				return

			#	ENTHALPY AT STATE 4 (CONDENSER OUTLET)
			[Cycle.obj_data.H[4],CV,CP,VSND] = self.hcvcps (1,Cycle.obj_data.TC[Cycle.obj_data.JC],Cycle.obj_data.V[4],Cycle.obj_data.X)
			Cycle.obj_data.JE=1
			LECON=True

			#	ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
			Cycle.obj_data.H[16] = Cycle.obj_data.H[4] - Data.CONDHT[Cycle.obj_data.NCYC]/ Data.MREF/DUTYR
			Cycle.obj_data.P[16] = Cycle.obj_data.P[4]
			
			[Cycle.obj_data.T[16], Cycle.obj_data.XQ[16],Cycle.obj_data.XL_Temp ,Cycle.obj_data.XV_Temp,  Cycle.obj_data.VL[16],Cycle.obj_data.VV[16],  HL16,HV16] = self.hpin (Cycle.obj_data.H[16],Cycle.obj_data.P[16],Cycle.obj_data.X)
			
			self.setArr2dCol (Cycle.obj_data.XL, 16, Cycle.obj_data.XL_Temp )
			self.setArr2dCol (Cycle.obj_data.XV, 16, Cycle.obj_data.XV_Temp )
						
			if (Cycle.obj_data.VL[16]  ==  0.0): Cycle.obj_data.VL[16] = Cycle.obj_data.V[4]

			Cycle.obj_data.V[16] = Cycle.obj_data.VL[16]

			#	enter iteration for evaporator outlet temperature
			Data.IE = 1
			while ( (Data.IE  <=  Cycle.obj_data.ITMAXE)  and  LECON ): #DO WHILE (Data.IE  <=  ITMAXE  and  LECON)
				self.showMsg ("Iteration Count for (OR EVAPORATOR OUTLET TEMPERATURE) ",Data.IE)

				I_ERROR_INTER = 0
				# Python comment, only if case of IHX, system may return 1
				if self.objEvapType.iterat_evap_exit_p7()==1:
					LECON = False
					I_ERROR_INTER = 1
					continue

				#	determine the bubble point at the evap exit pressure
				[Cycle.obj_data.X, Cycle.obj_data.XV15, TBUB15, VBUB15, VV15, LCRIT] = self.bublp ( Cycle.obj_data.P[15],Cycle.obj_data.X, Cycle.obj_data.XV15, True) 

				#	determine the bubble and dew point enthalpies
				[Cycle.obj_data.H[15] , CV, CP, VS] = self.hcvcps (1, Cycle.obj_data.T[15], Cycle.obj_data.V[15], Cycle.obj_data.X)	
				[HBUB15, CV, CP, VS] = self.hcvcps (1, TBUB15, VBUB15, Cycle.obj_data.X)	
				
				for INC in range (1, Cycle.obj_data.NC + 1): 
					Cycle.obj_data.XL[INC][13] = Cycle.obj_data.XL[INC][15] 

				Cycle.obj_data.XQ[13] = 1.0

				#	determine the enthalpy at [7]
				self.objEvapType.iterat_evap_enthalpy_p7()
				self.showMsg ("Outlet from fresh food evaporator (C) - point 7 ", Cycle.obj_data.T[7] - 273.11)

				#	interchanger for subcooling condenser liquid
				self.objEvapType.iterat_evap_subcool_p7()
				Cycle.obj_data.H[6] = Cycle.obj_data.H[16] - Cycle.obj_data.QINT
				
				#	determine the remainder of the properties at [6] and [13]
				Cycle.obj_data.P[6]  = Cycle.obj_data.P[4]
				Cycle.obj_data.P[13] = Cycle.obj_data.P[7]
				
				[ Cycle.obj_data.T[6],XQ6, Cycle.obj_data.XL_Temp, Cycle.obj_data.XV_Temp, Cycle.obj_data.V[6],  VV6,HL,HV] \
					= self.hpin ( Cycle.obj_data.H[6],Cycle.obj_data.P[6],Cycle.obj_data.X )
				self.setArr2dCol (Cycle.obj_data.XL, 6, Cycle.obj_data.XL_Temp)
				self.setArr2dCol (Cycle.obj_data.XV, 6, Cycle.obj_data.XV_Temp)
				
				Cycle.obj_data.VL[6] = Cycle.obj_data.V[6] 
				Cycle.obj_data.XQ[6] = 0.0
				
				self.objEvapType.iterat_call_exit_p13()
				
				'''
				if (Data.ISPEC  !=  2):
					[Cycle.obj_data.T[13], Cycle.obj_data.XQ13, Cycle.obj_data.XL_Temp,  \
						Cycle.obj_data.XV_Temp, Cycle.obj_data.VL13, Cycle.obj_data.V[13], Cycle.obj_data.HL, Cycle.obj_data.HV] \
						= self.hpin ( Cycle.obj_data.H[13],Cycle.obj_data.P[13],Cycle.obj_data.X )
						
					self.setArr2dCol (Cycle.obj_data.XL, 13, Cycle.obj_data.XL_Temp)
					self.setArr2dCol (Cycle.obj_data.XV, 13, Cycle.obj_data.XV_Temp )
				'''

				Cycle.obj_data.XQ[13] = 1.0


				self.showMsg ("Liquid line state after heat loss to cabinet & mullion (C) - point 16 ", Cycle.obj_data.T[16] - 273.11)
				self.showMsg ("Superheated gas leaving hight temp interchanger (C) - point 13 ", Cycle.obj_data.T[13] - 273.11)

				#	FIND CONDITIONS AT EVAPORATOR INLET ASSUMING ISENTHALPIC EXPANSION
				Cycle.obj_data.P[5] = Cycle.obj_data.P[13] + Cycle.obj_data.DPE
				self.showMsg ("liquid line outlet from high temp interchanger (C) -point 6 ", Cycle.obj_data.T[6] - 273.11)
				
				#	CALL ANALYSIS OF FREEZER SECTION AND THE LOWER
				#	INTERCHANGER (LOOKS LIKE A NON-ADIABATIC EXPANSION VALVE TO REST OF SYSTEM
				[Cycle.obj_data.H, Cycle.obj_data.P , Cycle.obj_data.X , Cycle.obj_data.T, \
					Cycle.obj_data.XQ, Cycle.obj_data.XL, Cycle.obj_data.XV,  Cycle.obj_data.VL,   Cycle.obj_data.VV,\
					Cycle.obj_data.HL, Cycle.obj_data.HV, Cycle.obj_data.TS6, Cycle.obj_data.QFREZ ] \
					= self.lowevp ( Cycle.obj_data.ICYCL,0,Cycle.obj_data.H,  \
									Cycle.obj_data.P,  Cycle.obj_data.X,  Cycle.obj_data.T,  \
									Cycle.obj_data.XQ, Cycle.obj_data.XL, Cycle.obj_data.XV,  \
									Cycle.obj_data.VL, Cycle.obj_data.VV, Cycle.obj_data.HL, Cycle.obj_data.TS3, \
									Cycle.obj_data.TS5,Cycle.obj_data.DPF,Cycle.obj_data.ETHX2)
				
				#	CALCULATE FRESH FOOD SECTION HEAT EXCHANGE
				Cycle.obj_data.PDEWE = Cycle.obj_data.P[5] - (1.0-Cycle.obj_data.FSUPE)*Cycle.obj_data.DPE
				if (Cycle.obj_data.PDEWE >  Cycle.obj_data.P[5]): Cycle.obj_data.PDEWE = Cycle.obj_data.P[5]
				
				[Cycle.obj_data.XREF, Cycle.obj_data.X, Cycle.obj_data.TDEW, Cycle.obj_data.VLDEW,
					Cycle.obj_data.VDEW, Cycle.obj_data.LCRIT] \
					= self.bublp (Cycle.obj_data.PDEWE, Cycle.obj_data.XREF, Cycle.obj_data.X, False) 

				# Python POLDE is not used !!!
				POLDE = Cycle.obj_data.PDEWE
				if (Cycle.obj_data.TDEW  >=  Cycle.obj_data.TS3): Cycle.obj_data.TDEW = Cycle.obj_data.TS3 - 1.0
				if (Cycle.obj_data.T[5]  >=  Cycle.obj_data.TS3): Cycle.obj_data.T[5] = Cycle.obj_data.TS3 - 1.0

				[Cycle.obj_data.HDEW, Cycle.obj_data.CVRVAP, Cycle.obj_data.CPRVAP, Cycle.obj_data.VS] \
					= self.hcvcps (3, Cycle.obj_data.TDEW, Cycle.obj_data.VDEW, Cycle.obj_data.X) 

				#	STATE 12 IS THE POINT AT WHICH THE DEW POINT IS REACHED IN THE EVAPORATOR
				Cycle.obj_data.P[12] = Cycle.obj_data.PDEWE
				Cycle.obj_data.T[12] = Cycle.obj_data.TDEW
				Cycle.obj_data.V[12] = Cycle.obj_data.VDEW
				Cycle.obj_data.H[12] = Cycle.obj_data.HDEW
				Cycle.obj_data.XQ[12] = 1.0
		
				#	FIND DUTY CYCLE, NET CAPACITY AND AVERAGE FREEZER LOAD if ICYCL = 1
				if (Data.IC  !=  1) :
					[QFF, QFZ, DUTYR] \
						= self.dutfnd (Cycle.obj_data.ICAB, IRFTYP,Cycle.obj_data.ICYCL, Cycle.obj_data.NCYC, \
						Cycle.obj_data.QFRSH, Cycle.obj_data.QFREZ, Cycle.obj_data.FROSTF,\
						Cycle.obj_data.FROSTF,Cycle.obj_data.TS3,   Cycle.obj_data.TS5,  \
						Cycle.obj_data.T,     Cycle.obj_data.IDFRST )

				#	CALCULATE FRESH FOOD EVAPORATOR HEAT TRANSFER.
				#	TEST FOR STANDARD DESIGN.
				if (IRFTYP  <=  3) :
					if (Cycle.obj_data.ICYCL  ==  1  and  Cycle.obj_data.ICAB	!=  0  and  Data.IFRSH  !=  0) :
						if (Data.IC  ==  1) :
							TIN = 0.15*TFF + 0.85*TFZ
							FF_FRACT = 0.0 # in Python only
						else:
							Data.CAPE = Cycle.obj_data.QFRSH/1.0548 - 3.413 * Data.FANE - 3.413*(Data.DFSTCYC + Data.FZCYC)
							CFMA = Data.CFME/(1.08*1.8961)
							QFM = QFF + 3.413 * Data.DUTYC * Data.FFCYC
							[TIN,FF_FRACT] = self.mixair (Data.CAPE, QFM, QFZ, TFF, TFZ, CFMA)

						Cycle.obj_data.TS3 = (TIN + 459.6)/1.8

				[Cycle.obj_data.QFRSH, Cycle.obj_data.FSUPE] = self.objEvapCool.calc_heat_temp()
								
				#	superheating fraction
				FSHOLD = Cycle.obj_data.FSUPE
				Cycle.obj_data.FSUPE = (FSHOLD + Cycle.obj_data.FSUPE)/2.0

				if (Cycle.obj_data.FSUPE >  1.05*FSHOLD): Cycle.obj_data.FSUPE = 1.05*FSHOLD
				if (Cycle.obj_data.FSUPE <  0.95*FSHOLD): Cycle.obj_data.FSUPE = 0.95*FSHOLD

				#	fresh food section evaporator
				[Cycle.obj_data.TS4, Cycle.obj_data.TE,Cycle.obj_data.JE, ICONE] = \
					self.objEvapCool.frsh (Cycle.obj_data.H,Cycle.obj_data.T,Cycle.obj_data.TS3,  
						Cycle.obj_data.TE,Cycle.obj_data.JE, Cycle.obj_data.QFRSH) 
				
				#---------------------------ADDED NEW CODE (12/29/90)-------------------
				Cycle.obj_data.T[15] = Cycle.obj_data.T[15] + Cycle.obj_data.TE[2] - Cycle.obj_data.T[7]
				#-----------------------------END OF NEW CODE---------------------------

				#	calculate the average effectiveness of the ff evaporator
				#	calculate the heat transfer if the refrigerant left at TS3
				VGUESS = Cycle.obj_data.VDEW * Cycle.obj_data.TS3 / Cycle.obj_data.TDEW
				[AS3, BS3] = self.espar (0, Cycle.obj_data.TS3, Cycle.obj_data.X) 

				[VGUESS, LCONV] = self.vit (Cycle.obj_data.TDEW,Cycle.obj_data.P[7],AS3,BS3,VGUESS,False)
				VS3 = VGUESS

				[HS3, CV, CP, VS] = self.hcvcps (1, Cycle.obj_data.TS3, VS3, Cycle.obj_data.X) 

				Cycle.obj_data.QRMAX = Data.MREF*(HS3-Cycle.obj_data.H[5])

				#	calculate the heat transfer if the air left at T[5]
				Cycle.obj_data.QAMAX = Data.CFME*(Cycle.obj_data.TS3-Cycle.obj_data.T[5])
				Cycle.obj_data.QMAXE = Cycle.obj_data.QAMAX

				if (Cycle.obj_data.QRMAX < Cycle.obj_data.QAMAX) : Cycle.obj_data.QMAXE = Cycle.obj_data.QRMAX

				Data.ETAE = Cycle.obj_data.QFRSH/Cycle.obj_data.QMAXE
				
				if (ICONE  ==  1): LECON = False
				if (Cycle.obj_data.TE[Cycle.obj_data.JE]  <=  Cycle.obj_data.TEMIN):  LECON = False
					
				Data.IE = Data.IE + 1


			#----------------------------------------------------------------------------
			#	END OF EVAPORATOR ITERATION
			#----------------------------------------------------------------------------
			
			Cycle.obj_data.T[7] = Cycle.obj_data.TE[Cycle.obj_data.JE]
			self.showMsg ("outlet from fresh food evaporator (C) - point 7 ", Cycle.obj_data.T[7] - 273.11)
			#
			#	INTERCHANGER FOR SUBCOOLING CONDENSER LIQUID
			#-----------------NEW CODE ADDED FOR INCOMPLETE EVAPORATION-------------

			#	Python : repeated code, determine the enthalpy at [7]
			self.objEvapType.iterat_evap_enthalpy_p7()

			#	interchanger for subcooling condenser liquid
			self.objEvapType.iterat_evap_subcool_p7()
				
			Cycle.obj_data.H[6] = Cycle.obj_data.H[16] -  Cycle.obj_data.QINT
			[Cycle.obj_data.T[6],XQ6,Cycle.obj_data.XL_Temp ,Cycle.obj_data.XV_Temp ,Cycle.obj_data.V[6],   VV6,HL,HV] \
				= self.hpin ( Cycle.obj_data.H[6],Cycle.obj_data.P[6],Cycle.obj_data.X )

			self.setArr2dCol (Cycle.obj_data.XL, 6, Cycle.obj_data.XL_Temp)
			self.setArr2dCol (Cycle.obj_data.XV, 6, Cycle.obj_data.XV_Temp )
			
			Cycle.obj_data.VL[6] = Cycle.obj_data.V[6]
			Cycle.obj_data.XQ[6] = 0.0

			# calc. point 13 Temp
			self.objEvapType.iterat_call_exit_p13()
			'''
			if (Data.ISPEC  !=  2) :
				[Cycle.obj_data.T[13],XQ13,Cycle.obj_data.XL_Temp, Cycle.obj_data.XV_Temp, L13,Cycle.obj_data.V[13],HL,HV] = self.hpin ( Cycle.obj_data.H[13],Cycle.obj_data.P[13],Cycle.obj_data.X )

				self.setArr2dCol (Cycle.obj_data.XL, 13, Cycle.obj_data.XL_Temp )
				self.setArr2dCol (Cycle.obj_data.XV, 13, Cycle.obj_data.XV_Temp )
			'''

			Cycle.obj_data.XQ[13] = 1.0

			self.showMsg ("superheated gas leaving the high temp interchanger (C) - point 13", Cycle.obj_data.T[13] - 273.11)

			Cycle.obj_data.TE[1] = Cycle.obj_data.TE[Cycle.obj_data.JE]

			#	FIND ENTROPY AT COMPRESSOR INLET AND COMPUTE CONDITIONS AT
			#	COMPRESSOR OUTLET (TSPEC IS DEFINED INLET TEMP TO THE
			#	COMPRESSOR (-1 MEANS NO TEMPERATURE CHANGE)
			#
			Cycle.obj_data.P[1] = Cycle.obj_data.P[13]
			if (Data.TSPEC >  0.0) :
				Cycle.obj_data.T[1] = Data.TSPEC
				VGUESS = Cycle.obj_data.V[7]*Cycle.obj_data.T[1]/Cycle.obj_data.T[7]
				
				[A1, B1] = self.espar (0, Cycle.obj_data.T[1], Cycle.obj_data.X) 
				
				
				[VGUESS, LCONV] = self.vit ( Cycle.obj_data.T[1],Cycle.obj_data.P[1],A1,B1,VGUESS,False)
				Cycle.obj_data.V[1] = VGUESS
				
				[Cycle.obj_data.H[1],CV,CP,VS] = self.hcvcps (1,Cycle.obj_data.T[1],Cycle.obj_data.V[1],Cycle.obj_data.X) 
			else:
				
				Cycle.obj_data.T[1] = Cycle.obj_data.T[13]
				Cycle.obj_data.H[1] = Cycle.obj_data.H[13]
				Cycle.obj_data.V[1] = Cycle.obj_data.V[13]
				Cycle.obj_data.XQ[1] = Cycle.obj_data.XQ[13]
			# End if
			
			Cycle.obj_data.S[1] = self.entrop(Cycle.obj_data.T[1],Cycle.obj_data.V[1],Cycle.obj_data.X)
			Cycle.obj_data.P[2] = Cycle.obj_data.P[3] + Cycle.obj_data.FSUPC*Cycle.obj_data.DPC
			
			self.showMsg ("compressor inlet (saturated vapor) (C) - point 1", Cycle.obj_data.T[1] - 273.11)
			
			#
			# CALL COMPRESSOR MODEL
			[Cycle.obj_data.T, HOUT, Cycle.obj_data.QHILO, Cycle.obj_data.QCAN,VSUC,  VV2, TSUC, TDISC, GAMA, RN, ETAS] \
				= self.objCompType.comp_balance()
				
			'''
			if (Data.IMAP == 0) :
				#
				# CALL TABULAR MAP BASED MODEL
				#
				OLDMAS = Data.MREF

				[Cycle.obj_data.T, HOUT, Cycle.obj_data.QHILO, Cycle.obj_data.QCAN, VSUC, VV2, TSUC, TDISC, GAMA, RN, ETAS] \
					= self.compcall (Cycle.obj_data.H, Cycle.obj_data.P, Cycle.obj_data.X, Cycle.obj_data.T, Cycle.obj_data.V,Cycle.obj_data.TS1)

				Data.MREF = (Data.MREF + 2.0*OLDMAS)/ 3.0
				if (Data.MREF >  1.05*OLDMAS): Data.MREF = 1.05*OLDMAS
				if (Data.MREF <  0.95*OLDMAS): Data.MREF = 0.95*OLDMAS

				#FLOW2 = FLWREF * Data.MREF/MREFSV
			# End if

			if (Data.IMAP == 1 or Data.IMAP == 2) :
				
				#
				# CALL THEORETICALLY BASED MODEL

				[Cycle.obj_data.T, HOUT, Cycle.obj_data.QHILO, Cycle.obj_data.QCAN,VSUC,  VV2, TSUC, TDISC, GAMA, RN, ETAS] = 	\
					self.comp (Cycle.obj_data.H, Cycle.obj_data.P, Cycle.obj_data.X, Cycle.obj_data.T, Cycle.obj_data.MEFF, Cycle.obj_data.QHILO, Cycle.obj_data.QCAN, Cycle.obj_data.V, Cycle.obj_data.TS1 )

				#
				#	UPDATE THE MASS FLOW TO CORRESPOND TO THE DISPLACEMENT
				#
				DISPI = Data.DISP/1.6387E-05
				OLDMAS = Data.MREF

				Data.MREF = (Data.MREF * (Cycle.obj_data.DISPLC/DISPI) + 2.0*OLDMAS)/3.0
				if (Data.MREF >  1.05*OLDMAS): Data.MREF = 1.04*OLDMAS
				if (Data.MREF <  0.95*OLDMAS): Data.MREF = 0.96*OLDMAS
				#FLOW2 = FLWREF * Data.MREF/MREFSV
			'''	
				
			
			# repeted code, get it out of IF
			FLOW2 = FLWREF * Data.MREF/MREFSV
			
			#	SHOW THE PROGRESS IN THE SOLUTION
			self.progrs (Cycle.obj_data.ICYCL,Cycle.obj_data.H,HOUT,WMAVG,FLOW2,Cycle.obj_data.QCAN)
			#
			#	CONDITIONS OF GAS LEAVING COMPRESSOR SHELL
			#

			[Cycle.obj_data.H[2],CV,CP,VS] = self.hcvcps (1, Cycle.obj_data.T[2], VV2, Cycle.obj_data.X) 

			[Cycle.obj_data.T[2],Cycle.obj_data.XQ[2],Cycle.obj_data.XL_Temp, Cycle.obj_data.XV_Temp ,VL2,VV2,HL2,HV2] = self.hpin ( Cycle.obj_data.H[2],Cycle.obj_data.P[2],Cycle.obj_data.X )
			self.setArr2dCol (Cycle.obj_data.XL, 2, Cycle.obj_data.XL_Temp)
			self.setArr2dCol (Cycle.obj_data.XV, 2, Cycle.obj_data.XV_Temp )
			
			Cycle.obj_data.V[2] = VV2
			#
			#	ENTROPY OF GAS LEAVING COMPRESSOR
			#
			if (Cycle.obj_data.XQ[2] <  1.0) :
				SL2 = self.entrop(Cycle.obj_data.T[2],VL2,self.getArr2dCol (Cycle.obj_data.XL,2) ) 
				SV2 = self.entrop(Cycle.obj_data.T[2],VV2,self.getArr2dCol (Cycle.obj_data.XV,2) ) 
				Cycle.obj_data.S[2] = Cycle.obj_data.XQ[2]*SV2 + (1.0-Cycle.obj_data.XQ[2])*SL2
			else:
				Cycle.obj_data.S[2] = self.entrop(Cycle.obj_data.T[2],VV2,Cycle.obj_data.X)

			self.showMsg ("compressor discharge (C) - point 2", Cycle.obj_data.T[2] - 273.11)

			if (ICONC  !=  1) :
				#
				#	CALCULATE CONDENSER HEAT EXCHANGE
				#	DETERMINE THE DEW POINT CONDITIONS

				# [P2, P3, P4, P5, P6, P8] = bublp ( P1, P2, P3,    P7)
				[Cycle.obj_data.XL_Temp ,Cycle.obj_data.X,TDEW,VL_notused,VDEW, LCRIT] = self.bublp (Cycle.obj_data.P[3], Cycle.obj_data.XL_Temp ,Cycle.obj_data.X,False) 
				self.setArr2dCol (Cycle.obj_data.XL, 3, Cycle.obj_data.XL_Temp)
				
				Cycle.obj_data.T[3] = TDEW
				Cycle.obj_data.V[3] = VDEW

				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[HDEW,CVRVAP,CPRVAP,VS] = self.hcvcps (3,TDEW,VDEW,Cycle.obj_data.X) 

				Cycle.obj_data.H[3] = HDEW
				#
				#	DETERMINE BUBBLE POINT CONDITIONS
				#	ASSUME A LINEAR PRESSURE DROP THROUGHOUT THE CONDENSER
				#
				PBUB = Cycle.obj_data.P[4] + Cycle.obj_data.DPC * Cycle.obj_data.FSUBC

				[Cycle.obj_data.X, Cycle.obj_data.XV_Temp ,TBUB,VBUB,V4, LCRIT] = self.bublp ( PBUB,Cycle.obj_data.X, Cycle.obj_data.XV_Temp, True) 
				self.setArr2dCol (Cycle.obj_data.XV, 4, Cycle.obj_data.XV_Temp )
				
				[HBUB,CVRLIQ,CPRLIQ,VS] = self.hcvcps (3,TBUB,VBUB,Cycle.obj_data.X) 

				#
				#	STATE 11 IS THE POINT AT WHICH THE BUBBLE POINT
				#	IS REACHED IN THE CONDENSER
				#
				Cycle.obj_data.P[11] = PBUB
				Cycle.obj_data.T[11] = TBUB
				Cycle.obj_data.V[11] = VBUB
				Cycle.obj_data.H[11] = HBUB
				#
				#	DETERMINE CONDITIONS ENTERING THE CONDENSER
				#
				HDROP = Data.CONDVP[Cycle.obj_data.NCYC]/Data.MREF/Data.DUTYC

				Cycle.obj_data.P[14] = Cycle.obj_data.P[2]
				HINCND = Cycle.obj_data.H[2] - HDROP

				Cycle.obj_data.H[14] = HINCND
				
				[Cycle.obj_data.T[14], Cycle.obj_data.XQ[14], Cycle.obj_data.XL_Temp, Cycle.obj_data.XV_Temp,Cycle.obj_data.VL[14],Cycle.obj_data.VV[14],HL14,HV14] = self.hpin ( Cycle.obj_data.H[14],Cycle.obj_data.P[2],Cycle.obj_data.X )

				self.setArr2dCol (Cycle.obj_data.XL, 14, Cycle.obj_data.XL_Temp )
				self.setArr2dCol (Cycle.obj_data.XV, 14, Cycle.obj_data.XV_Temp )
				
				Cycle.obj_data.V[14] = (1.0-Cycle.obj_data.XQ[14])*Cycle.obj_data.VL[14] + Cycle.obj_data.XQ[14]*Cycle.obj_data.VV[14]

				[_, QDSC,QTPC,QSCC,QTOTC, Cycle.obj_data.FSUPC, Cycle.obj_data.FSUBC ] = self.objCondType.cond_balance(CPRLIQ)
				
				
				#[ Cycle.obj_data.TS2,Cycle.obj_data.TC, Cycle.obj_data.JC, ICONC] = self.cond (Cycle.obj_data.T,Cycle.obj_data.H,   Cycle.obj_data.TS1,Cycle.obj_data.TC,QDSC,   QTPC,QSCC,Cycle.obj_data.JC, ICONC )
				[ Cycle.obj_data.TS2,Cycle.obj_data.TC, Cycle.obj_data.JC, ICONC] \
					= self.objCondType.cond (Cycle.obj_data.T,Cycle.obj_data.H,	\
						Cycle.obj_data.TS1,Cycle.obj_data.TC,QDSC,   QTPC,QSCC,Cycle.obj_data.JC, ICONC )

				#
				#	ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
				#
				Cycle.obj_data.H[16] = Cycle.obj_data.H[4] - Data.CONDHT[Cycle.obj_data.NCYC]/Data.MREF/DUTYR
				Cycle.obj_data.P[16] = Cycle.obj_data.P[4]
				[Cycle.obj_data.T[16], Cycle.obj_data.XQ[16],Cycle.obj_data.XL_Temp ,Cycle.obj_data.XV_Temp,  Cycle.obj_data.VL[16],Cycle.obj_data.VV[16],HL16,HV16] = self.hpin ( Cycle.obj_data.H[16],Cycle.obj_data.P[16],Cycle.obj_data.X )

				self.setArr2dCol (Cycle.obj_data.XL, 16, Cycle.obj_data.XL_Temp )
				self.setArr2dCol (Cycle.obj_data.XV, 16, Cycle.obj_data.XV_Temp )
				
				if (Cycle.obj_data.VL[16]  ==  0.0): Cycle.obj_data.VL[16] = Cycle.obj_data.V[4]
				Cycle.obj_data.V[16] = Cycle.obj_data.VL[16]

				#
				#	CALCULATE THE AVERAGE EFFECTIVENESS OF THE HEAT EXCHANGER
				#	CALCULATE THE HEAT TRANSFER if THE REFRIGERANT LEFT AT Cycle.obj_data.TS1
				#
				#	DETERMINE THE SPECIFIC VOLUME OF THE LIQUID
				#
				if (Cycle.obj_data.TS1 <  Cycle.obj_data.T[4]) :
					VGUESS = Cycle.obj_data.V[4]
					# [P4, P5] = self.espar (P1, P2, P3)
					[AS1, BS1] = self.espar (0, Cycle.obj_data.TS1, Cycle.obj_data.X) #  CALL ESPAR(0,Cycle.obj_data.TS1,Cycle.obj_data.X,AS1,BS1)

					#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
					[VGUESS, LCONV] = self.vit (Cycle.obj_data.TS1,Cycle.obj_data.P[4],AS1,BS1,VGUESS,True) # CALL VIT(Cycle.obj_data.TS1,Cycle.obj_data.P[4],AS1,BS1,VGUESS,True,LCONV)
					VS1 = VGUESS
				else:
					VS1 = Cycle.obj_data.V[4]

				[HS1, CV, CP, VS] = self.hcvcps (1, Cycle.obj_data.TS1, VS1, Cycle.obj_data.X) # 	CALL HCVCPS(1,Cycle.obj_data.TS1,VS1,Cycle.obj_data.X,  HS1,CV,CP,VS)

				QRMAX = Data.MREF*(Cycle.obj_data.H[14]-HS1)
				#
				#	CALCULATE THE HEAT TRANSFER if THE AIR LEFT AT T[14]
				#
				QAMAX = Data.CFMC*(Cycle.obj_data.T[14]-Cycle.obj_data.TS1)
				QMAXC = QAMAX
				if (QRMAX <  QAMAX): QMAXC = QRMAX

				Data.ETAC = QTOTC/QMAXC
			else:
				LCCON = False

			Data.IC = Data.IC + 1
			
			if Data.INCTRL in [0,3]:
				if (Data.IC  <=  4) :
					LCCON = True
					ICONC = 0

			if Data.INCTRL in [1,2,4]:
				if (Data.IC  <=  10) :
					LCCON = True
					ICONC = 0
					
			#if (not LECON): LCCON = False #only in Puthon
			#if (LQUIT): LCCON = False #if key pressed in lowevp ,ethod then exit, cancel in Python
			
			
		#END DO IC loop
		#
		#END OF CONDENSER ITERATION
		#
		#
		#	ONCE CONDENSER AND EVAPORATOR HAVE CONVERGED COMPUTE
		#	REMAINING PROPERTIES AT EACH STATE POINT IN THE CYCLE
		#	AND PRINT RESULTS
		#
		#	BEGIN WITH HEAT TRANSFER FLUID TEMPERATURES
		#

		Cycle.obj_data.T[4] = Cycle.obj_data.TC[Cycle.obj_data.JC]
		Cycle.obj_data.TS[1] = 0.0
		Cycle.obj_data.TS[4] = Cycle.obj_data.TS1
		Cycle.obj_data.TS[5] = Cycle.obj_data.TS4
		Cycle.obj_data.TS[6] =0.0
		Cycle.obj_data.TS[7] = Cycle.obj_data.TS3
		Cycle.obj_data.TS[8] = Cycle.obj_data.TS6
		Cycle.obj_data.TS[9] = Cycle.obj_data.TS5
		Cycle.obj_data.TS[10] =0.0
		Cycle.obj_data.TS[14] =Cycle.obj_data.TS2
		Cycle.obj_data.TS[16] = 0.0
		TGC = Cycle.obj_data.T[3] - Cycle.obj_data.T[4]
		#
		#	SPECIFIC VOLUMES
		#
		Cycle.obj_data.V[5] = min(1.0, 1.0 - Cycle.obj_data.XQ[5] ) * Cycle.obj_data.VL[5] + max(0.0, Cycle.obj_data.XQ[5]  ) * Cycle.obj_data.VV[5]
		Cycle.obj_data.V[8] = min(1.0, 1.0 - Cycle.obj_data.XQ[8] ) * Cycle.obj_data.VL[8] + max(0.0, Cycle.obj_data.XQ[8]  ) * Cycle.obj_data.VV[8]
		Cycle.obj_data.V[9] = min(1.0, 1.0 - Cycle.obj_data.XQ[9] ) * Cycle.obj_data.VL[9] + max(0.0, Cycle.obj_data.XQ[9]  ) * Cycle.obj_data.VV[9]
		Cycle.obj_data.V[10]= min(1.0, 1.0 - Cycle.obj_data.XQ[10]) * Cycle.obj_data.VL[10]+ max(0.0, Cycle.obj_data.XQ[10] ) * Cycle.obj_data.VV[10]
		#
		#	QUALITIES
		#
		Cycle.obj_data.XQ[1] = 1.0
		Cycle.obj_data.XQ[3] = 1.0
		Cycle.obj_data.XQ[4] = 0.0
		Cycle.obj_data.XQ[6] = 0.0
		Cycle.obj_data.XQ[10] = 0.0
		#
		#	LIQUID AND VAPOR COMPOSITIONS AROUND THE CYCLE LOOP
		#
		#I = 1
		for I in range (1, Cycle.obj_data.NC + 1): #	DO WHILE (I  <=  NC)
			Cycle.obj_data.XL[I][1] = 0.0
			Cycle.obj_data.XV[I][1] = Cycle.obj_data.X[I]
			
			Cycle.obj_data.XL[I][2] = 0.0
			Cycle.obj_data.XV[I][2] = Cycle.obj_data.X[I]
			
			Cycle.obj_data.XL[I][3] = 0.0
			Cycle.obj_data.XV[I][3] = Cycle.obj_data.X[I]
			
			Cycle.obj_data.XL[I][4] = Cycle.obj_data.X[I]
			Cycle.obj_data.XV[I][4] = 0.0
			
			Cycle.obj_data.XL[I][6] = Cycle.obj_data.X[I]
			Cycle.obj_data.XV[I][6] = 0.0
			
			Cycle.obj_data.XL[I][7] = 0.0
			Cycle.obj_data.XV[I][7] = Cycle.obj_data.X[I]
			
			Cycle.obj_data.XL[I][10] = Cycle.obj_data.X[I]
			Cycle.obj_data.XV[I][10] = 0.0
			
			Cycle.obj_data.XL[I][11] = Cycle.obj_data.X[I]
			Cycle.obj_data.XV[I][11] = 0.0
			
			Cycle.obj_data.XL[I][12] = 0.0
			Cycle.obj_data.XV[I][12] = Cycle.obj_data.X[I]
			
			Cycle.obj_data.XL[I][13] = 0.0
			Cycle.obj_data.XV[I][13] = Cycle.obj_data.X[I]
			
			Cycle.obj_data.XL[I][14] = 0.0
			Cycle.obj_data.XV[I][14] = Cycle.obj_data.X[I]
			
			Cycle.obj_data.XL[I][16] = Cycle.obj_data.X[I]
			Cycle.obj_data.XV[I][16] = 0.0

		#
		#	ENTROPIES AROUND THE CYCLE
		#
		#J = 3
		for J in range(3, 16+1): #DO WHILE (J  <=  16)
			if (J  !=  5): Cycle.obj_data.S[J] = self.entrop( Cycle.obj_data.T[J], Cycle.obj_data.V[J] ,Cycle.obj_data.X)
			#J = J + 1
		#END DO
		
		SL5 = self.entrop(Cycle.obj_data.T[5],Cycle.obj_data.VL[5], self.getArr2dCol (Cycle.obj_data.XL,5) ) # XL[1][5]
		SV5 = self.entrop(Cycle.obj_data.T[5],Cycle.obj_data.VV[5], self.getArr2dCol (Cycle.obj_data.XV,5) ) # XV[1][5]
		Cycle.obj_data.S[5] = min(1.0,1.0-Cycle.obj_data.XQ[5]) * SL5 + max(0.0,Cycle.obj_data.XQ[5]) * SV5
		
		SL8 = self.entrop(Cycle.obj_data.T[8],Cycle.obj_data.VL[8],  self.getArr2dCol (Cycle.obj_data.XL,8) ) # XL[1][8]
		SV8 = self.entrop(Cycle.obj_data.T[8],Cycle.obj_data.VV[8],  self.getArr2dCol (Cycle.obj_data.XV,8) ) # XV[1][8]
		Cycle.obj_data.S[8] = min(1.0,1.0-Cycle.obj_data.XQ[8]) * SL8 + max(0.0,Cycle.obj_data.XQ[8]) * SV8
		
		SL9 = self.entrop(Cycle.obj_data.T[9],Cycle.obj_data.VL[9], self.getArr2dCol (Cycle.obj_data.XL,9) ) # XL[1][9]
		SV9 = self.entrop(Cycle.obj_data.T[9],Cycle.obj_data.VV[9], self.getArr2dCol (Cycle.obj_data.XV,9) ) # XV[1][9]
		Cycle.obj_data.S[9] = min(1.0,1.0-Cycle.obj_data.XQ[9]) * SL9 + max(0.0,Cycle.obj_data.XQ[9]) * SV9
		#
		#	CONVERT FROM MOLAR QUANTITIES TO MASS BASIS
		#
		#J = 1
		
		for J in range (1, 16 + 1): #DO WHILE (J  <=  16)
			Cycle.obj_data.WMAVGL[J] = 0.0
			Cycle.obj_data.WMAVGV[J] = 0.0

			for I in range (1, Cycle.obj_data.NC + 1): #DO WHILE (I  <=  NC)
				Cycle.obj_data.WMAVGL[J] = Cycle.obj_data.WMAVGL[J] + (Cycle.obj_data.XL[I][J] ) * (Data.WM[I])
				Cycle.obj_data.WMAVGV[J] = Cycle.obj_data.WMAVGV[J] + (Cycle.obj_data.XV[I][J] ) * (Data.WM[I])
			
			for I in range (1, Cycle.obj_data.NC + 1): #DO WHILE (I  <=  NC)
				if (Cycle.obj_data.XL[I][J] >  0.0): Cycle.obj_data.XL[I][J] = Cycle.obj_data.XL[I][J] * Data.WM[I]/Cycle.obj_data.WMAVGL[J]
				if (Cycle.obj_data.XV[I][J] >  0.0): Cycle.obj_data.XV[I][J] = Cycle.obj_data.XV[I][J] * Data.WM[I]/Cycle.obj_data.WMAVGV[J]
			
			Cycle.obj_data.V[J] = Cycle.obj_data.V[J]/WMAVG
			Cycle.obj_data.H[J] = Cycle.obj_data.H[J]/WMAVG
			Cycle.obj_data.S[J] = Cycle.obj_data.S[J]/WMAVG

		
		#
		#	COMPUTE WORK, CAPACITY, COP, ETC.
		#
		HOUT = HOUT/WMAVG
		VSUC = VSUC/WMAVG
		Data.W = (HOUT - Cycle.obj_data.H[1])/(1.0 - Cycle.obj_data.QCAN)
		Data.QE = Cycle.obj_data.H[7] - Cycle.obj_data.H[5]
		QC = Cycle.obj_data.H[14] - Cycle.obj_data.H[4]
		Data.QZ = Cycle.obj_data.H[9] - Cycle.obj_data.H[8]
		Data.COPR = (Data.QE + Data.QZ)/Data.W
		
		TH = Cycle.obj_data.TS1
		TL1 = Cycle.obj_data.TS3
		TL2 = Cycle.obj_data.TS5
		DENOM = TH * (Data.QE * (1./TL1-1./TH)+ Data.QZ * (1./TL2-1./TH))
		COPI = (Data.QE+ Data.QZ)/DENOM
		PR = Cycle.obj_data.P[2]/Cycle.obj_data.P[1]
		TSUPC = Cycle.obj_data.T[2] - Cycle.obj_data.T[3]
		
		#
		#	CORRECT COP DUR TO CYCLING LOSSES
		#
		if (Data.I_CYCLE  ==  0) :
			Data.CORR_COP = 1.0
		else:
			TENV = (Data.TROOM + 459.6)/1.8
			TMID_COND = (Cycle.obj_data.T[3] + Cycle.obj_data.T[11])/2.0
			TMID_EVAP = (Cycle.obj_data.T[8] + Cycle.obj_data.T[9])/2.0
			Data.CORR_COP = self.cyclos (Data.I_VALVE, Data.T_CYCLE)

		#
		#	OUTPUT WARNING IF CONVERGENCE FORCED BY HOT KEY <F10>.
		#
		if (LQUIT):
			objCycOut.write_or_terminate ("Convergence forced after, iterations ", Data.IC)
			
			#WRITE(IO_Cycle,'('' CONVERGENCE FORCED AFTER '',I2,
			#	'' ITERATIONS''/)') Data.IC
		#
		#	PRINT ERROR MESSAGES if NON-CONVERGENCE
		#
		if (LECON  or  LCCON  or  I_ERROR_INTER  > 0) :
			LWIND = 5
			if (LECON  and  LCCON): LWIND = 7
			#CALL WINDOW(8,8+LWIND,20,60,32,1)
			if (LECON) :
				Cycle.obj_data.TE[1] = Cycle.obj_data.TE[1] - 273.11
				Cycle.obj_data.TE[2] = Cycle.obj_data.TE[2] - 273.11

				# WRITE (IO_Cycle,2200) Cycle.obj_data.TE[1],Cycle.obj_data.TE[2]'
				objCycOut.write_or_terminate  ('EVAPORATOR ITERATION DID NOT CONVERGE,  %9.3f, %9.3f ' %( Cycle.obj_data.TE[1],Cycle.obj_data.TE[2]) )

				#CALL GOTOXY(22,11)
				#CALL PRINT('EVAPORATOR ITERATION DID NOT CONVERGE$',37,-2)
				self.showError ("EVAPORATOR ITERATION DID NOT CONVERGE")

			if (LCCON) :
				Cycle.obj_data.TC[1] = Cycle.obj_data.TC[1] - 273.11
				Cycle.obj_data.TC[2] = Cycle.obj_data.TC[2] - 273.11

				#'WRITE (IO_Cycle,2202) Cycle.obj_data.TC[1],Cycle.obj_data.TC[2]'
				objCycOut.write_or_terminate  ('CONDENSER ITERATION DID NOT CONVERGE,  %9.3f, %9.3f ' %( Cycle.obj_data.TE[1],Cycle.obj_data.TE[2]) )
			
				self.showError ("CONDENSER ITERATION DID NOT CONVERGE")


			if (I_ERROR_INTER >  0) :
				#"WRITE (IO_Cycle,2203)"
				objCycOut.write_or_terminate  ('INTERCHANGER SUPERHEAT NOT POSSIBLE')
			
				self.showError ("INTERCHANGER SUPERHEAT NOT POSSIBLE")

			# say beep CALL WARBLE
			# say beep CALL WARBLE
			# say beep CALL WARBLE

			#READ( * , * )
			input("Press Enter to continue...")


		#
		#	OUTPUT RESULTS.  BEGIN BY CONVERTING TO ENGLISH UNITS.
		#

		TENV = (Data.TROOM + 459.6)/1.8

		if (Cycle.obj_data.T[16] <  TENV):  Data.I_LIQUID_LINE = 1

		#WRITE (IO_Cycle,1010)
		objCycOut.write_or_terminate (",STATE, Cycle.obj_data.T(C), Cycle.obj_data.T(C), Cycle.obj_data.P, Cycle.obj_data.H, Cycle.obj_data.V, Cycle.obj_data.S, Cycle.obj_data.XL, Cycle.obj_data.XV, Cycle.obj_data.XQ")
		objCycOut.write_or_terminate (",,AIR,   REF, (KPa), (KJ/KG), (M3/KG), (KJ/KG-C), (MASS FRAC),(MASS FRAC),(MASS FRAC)")
		print (",STATE, Cycle.obj_data.T(C),Cycle.obj_data.T(C), Cycle.obj_data.P, Cycle.obj_data.H, Cycle.obj_data.V, Cycle.obj_data.S, Cycle.obj_data.XL, Cycle.obj_data.XV, Cycle.obj_data.XQ")
		print (",,AIR,   REF, (KPA), (KJ/KG), (M3/KG), (KJ/KG-C), (MASS FRAC),(MASS FRAC),(MASS FRAC)")

		K = 1
		if (Cycle.obj_data.ICYCL  ==  2) :
			while (K <= 15): #DO WHILE (K  <=  15)
				J = Cycle.obj_data.LPNT[K]
				Cycle.obj_data.TS[J] = Cycle.obj_data.TS[J] - 273.11
				Cycle.obj_data.T[J] = Cycle.obj_data.T[J] - 273.11
				Cycle.obj_data.V[J] = Cycle.obj_data.V[J]/10.0

				if (Cycle.obj_data.XQ[J] >  1.0): Cycle.obj_data.XQ[J] = 1.0
				if (Cycle.obj_data.XQ[J] <  0.0): Cycle.obj_data.XQ[J] = 0.0
				
				if (Cycle.obj_data.AIRTMP[K]) :
					#WRITE (8,1020) K,HSTATE(K),TS[J],Cycle.obj_data.T[J],P[J],H[J],Cycle.obj_data.V[J],'
					objCycOut.write_or_terminate ( "%d, %s , %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,Cycle.obj_data.HSTATE[K],Cycle.obj_data.TS[J],Cycle.obj_data.T[J],Cycle.obj_data.P[J],Cycle.obj_data.H[J],Cycle.obj_data.V[J],  Cycle.obj_data.S[J],Cycle.obj_data.XL[1][J],Cycle.obj_data.XV[1][J],Cycle.obj_data.XQ[J]) )
					print ( "%d, %d, %s , %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,J,Cycle.obj_data.HSTATE[K],Cycle.obj_data.TS[J],Cycle.obj_data.T[J],Cycle.obj_data.P[J],Cycle.obj_data.H[J],Cycle.obj_data.V[J],  Cycle.obj_data.S[J],Cycle.obj_data.XL[1][J],Cycle.obj_data.XV[1][J],Cycle.obj_data.XQ[J]) )
				else:
					#'WRITE (8,1021) K,HSTATE(K),		Cycle.obj_data.T[J],Cycle.obj_data.P[J],H[J],Cycle.obj_data.V[J],') print ('S[J],Cycle.obj_data.XL(1,J),Cycle.obj_data.XV(1,J),Cycle.obj_data.XQ[J]')
					objCycOut.write_or_terminate ( " %d, %s,  N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,Cycle.obj_data.HSTATE[K], Cycle.obj_data.T[J],Cycle.obj_data.P[J],Cycle.obj_data.H[J],Cycle.obj_data.V[J],  Cycle.obj_data.S[J],Cycle.obj_data.XL[1][J],Cycle.obj_data.XV[1][J],Cycle.obj_data.XQ[J]) )
					print ( "%d,%d, %s , N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,J,Cycle.obj_data.HSTATE[K], Cycle.obj_data.T[J],Cycle.obj_data.P[J],Cycle.obj_data.H[J],Cycle.obj_data.V[J],  Cycle.obj_data.S[J],Cycle.obj_data.XL[1][J],Cycle.obj_data.XV[1][J],Cycle.obj_data.XQ[J]) )
					
				# End if
				K = K + 1
			#END DO
		else:
			M = 0
			K = 0
			while (M <= 14): #DO WHILE (M  <=  14)
				M = M + 1
				J = Cycle.obj_data.LPNT[M]

				Cycle.obj_data.TS[J] = Cycle.obj_data.TS[J] - 273.11
				Cycle.obj_data.T[J] = Cycle.obj_data.T[J] - 273.11
				Cycle.obj_data.V[J] = Cycle.obj_data.V[J]/10.0
				if (Cycle.obj_data.XQ[J] >  1.0): Cycle.obj_data.XQ[J] = 1.0
				if (Cycle.obj_data.XQ[J] <  0.0): Cycle.obj_data.XQ[J] = 0.0

				if (M  >=  9  and  M  <=  11): continue
				K = K + 1
				if ( Cycle.obj_data.AIRTMP[M] ) :
					#WRITE (8,1020) K,MSTATE(K),TS[J],Cycle.obj_data.T[J],Cycle.obj_data.P[J],H[J],Cycle.obj_data.V[J],'  'Cycle.obj_data.S[J],Cycle.obj_data.XL(1,J),Cycle.obj_data.XV(1,J),XQ[J]
					objCycOut.write_or_terminate ( "%d, %s, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,Cycle.obj_data.MSTATE[K], Cycle.obj_data.TS[J],Cycle.obj_data.T[J],Cycle.obj_data.P[J],Cycle.obj_data.H[J],Cycle.obj_data.V[J],  Cycle.obj_data.S[J],Cycle.obj_data.XL[1][J],Cycle.obj_data.XV[1][J],Cycle.obj_data.XQ[J]) )
						
					print ("%d,%d, %s , %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,J, Cycle.obj_data.MSTATE[K], Cycle.obj_data.TS[J],Cycle.obj_data.T[J],Cycle.obj_data.P[J],Cycle.obj_data.H[J],Cycle.obj_data.V[J],  Cycle.obj_data.S[J],Cycle.obj_data.XL[1][J],Cycle.obj_data.XV[1][J],Cycle.obj_data.XQ[J]) )
				else:
					
					#WRITE (8,1021) K,MSTATE(K),		Cycle.obj_data.T[J],P[J],H[J],Cycle.obj_data.V[J],'\	'S[J],Cycle.obj_data.XL(1,J),Cycle.obj_data.XV(1,J),XQ[J]
					objCycOut.write_or_terminate ( "%d, %s , N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K, Cycle.obj_data.MSTATE[K],  Cycle.obj_data.T[J],Cycle.obj_data.P[J],Cycle.obj_data.H[J],Cycle.obj_data.V[J],  Cycle.obj_data.S[J],Cycle.obj_data.XL[1][J],Cycle.obj_data.XV[1][J],Cycle.obj_data.XQ[J]) )
					print ("%d,%d, %s , N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,J, Cycle.obj_data.MSTATE[K], Cycle.obj_data.T[J],Cycle.obj_data.P[J],Cycle.obj_data.H[J],Cycle.obj_data.V[J],  Cycle.obj_data.S[J],Cycle.obj_data.XL[1][J],Cycle.obj_data.XV[1][J],Cycle.obj_data.XQ[J]) )						
						
				# End if
			#END DO

		# End if

		#
		#	NORMALIZE BY THE MASS FLOW
		#

		Data.FLOW = Data.FLOW * Data.MREF/MREFSV
		Data.DISP = Data.DISP/1.6387E-05
		Data.W = 0.4302 * Data.W * Data.FLOW * 1.0548
		Data.QZ = 0.4302 * Data.QZ * Data.FLOW * 1.0548
		Data.QE = 0.4302 * Data.QE * Data.FLOW * 1.0548
		QC = 0.4302 * QC * Data.FLOW * 1.0548
		#
		#	REST OF THE CONVERSIONS
		#
		TSUC = TSUC - 273.11
		TDISC = TDISC - 273.11
		Data.ETAE = 100. * Data.ETAE
		Cycle.obj_data.FSUPE = 100. * Cycle.obj_data.FSUPE
		Data.ETAF = 100. * Data.ETAF
		Data.ETAC = 100. * Data.ETAC
		Cycle.obj_data.FSUBC = 100. * Cycle.obj_data.FSUBC
		Cycle.obj_data.FSUPC = 100. * Cycle.obj_data.FSUPC
		#
		#	OUTPUT SUMMARY TABLE OF RESULTS
		#
		#WRITE(8,1025)
		objCycOut.write_or_terminate  ('CYCLE PERFORMANCE SUMMARY')
		if (Data.ITYPE  ==  1) :
			#WRITE(8,1105) Data.QE, Data.QE/3.6'
			objCycOut.write_or_terminate  ('EVAPORATOR CAPACITY,  %9.3f, KJ/HR,  %9.3f, W' %( Data.QE, Data.QE/3.6) )
		else:
			#WRITE(8,1030) Data.QE, Data.QE/3.6'
			#WRITE(8,1035) Data.QZ, Data.QZ/3.6')
			objCycOut.write_or_terminate  ('FRESH FOOD EVAPORATOR CAPACITY,  %9.3f, KJ/HR,  %9.3f, W' %( Data.QE, Data.QE/3.6) )
			objCycOut.write_or_terminate  ('FREEZER EVAPORATOR CAPACITY,     %9.3f, KJ/HR,  %9.3f, W' %( Data.QZ, Data.QZ/3.6) )
			
		# End if

		#WRITE(8,1040) QC, QC/3.6
		#WRITE(8,1045) Data.W, Data.W/3.6')
		#WRITE(8,1050) Data.COPR
		
		objCycOut.write_or_terminate  ('CONDENSER HEAT REJECTION RATE,  %9.3f, KJ/HR,  %9.3f, W' %( QC, QC/3.6) )
		objCycOut.write_or_terminate  ('COMPRESSOR POWER REQUIREMENT,     %9.3f, KJ/HR,  %9.3f, W' %(Data.W, Data.W/3.6 ) )
		objCycOut.write_or_terminate  ('COEFFICIENT OF PERFORMANCE,     %9.3f' %( Data.COPR) )

		if (IRFTYP  <=  3  and  Cycle.obj_data.ICYCL  ==  1  and  Cycle.obj_data.ICAB  !=  0
								and  Data.IFRSH  !=  0):
			#WRITE(8,1055) FF_FRACT
			objCycOut.write_or_terminate  ('FRACTION AIR TO FRESH FOOD,     %9.3f, (SINGLE EVAPORATOR CYCLE)' %( FF_FRACT) )

		#write(8, '( )')
		objCycOut.write_or_terminate (" ")
		
		if (Data.IMAP  ==  1) :
			objCycOut.write_or_terminate  ('ESTIMATED COMPRESSION EFFICIENCY, %9.3f, (COMPRESSOR EER MODEL)' %( ETAS) )
			objCycOut.write_or_terminate  ('ESTIMATED MOTOR-PUMP EFFICIENCY,  %9.3f, (COMPRESSOR EER MODEL)' %( Data.EFFC/ETAS) )
			objCycOut.write_or_terminate  ('ESTIMATED CLEARANCE VOLUME,       %9.3f, (COMPRESSOR EER MODEL)' %( Data.CE) )
			objCycOut.write_or_terminate  ('ESTIMATED SHELL LOSS,       %9.3f, (COMPRESSOR EER MODEL)' %( Cycle.obj_data.QCAN) )
			objCycOut.write_or_terminate  ('ESTIMATED DISC TUBE HEAT LOSS,       %9.3f, (COMPRESSOR EER MODEL)' %( Cycle.obj_data.QHILO) )
			
			#WRITE(8,2215) ETAS
			#WRITE(8,2211) Data.EFFC/ETAS
			#WRITE(8,2212) Data.CE
			#WRITE(8,2213) Cycle.obj_data.QCAN
			#WRITE(8,2214) Cycle.obj_data.QHILO
		# End if

		objCycOut.write_or_terminate ("HEAT EXCHANGER PERFORMANCE SUMMARY")
		objCycOut.write_or_terminate ("EXCHANGER, EFFECTIVENESS, SUBCOOLED FRACTION, SUPERHEATED FRACTION")
		#WRITE(8,1060)
		#WRITE(8,1065)

		if (Data.ITYPE  ==  1) :
			if (Data.IFRSH  !=  0) :
				#WRITE(8,1110) Data.ETAE,FSUPE
				objCycOut.write_or_terminate  ('EVAPORATOR,     %9.3f, N/A, ,%9.3f' %(Data.ETAE,Cycle.obj_data.FSUPE ) )
			else:
				#WRITE(8,1111) FSUPE
				objCycOut.write_or_terminate  ('EVAPORATOR,     , N/A, N/A, %9.3f ' %(Cycle.obj_data.FSUPE ) )
		else:
			if (Data.IFRSH  !=  0) :
				#WRITE(8,1070) Data.ETAE,FSUPE
				objCycOut.write_or_terminate  ('FRESH FOOD EVAP.,     %9.3f, N/A, %9.3f ' %(Data.ETAE,Cycle.obj_data.FSUPE ) )
			else:
				#WRITE(8,1071) FSUPE
				objCycOut.write_or_terminate  ('FRESH FOOD EVAP.,  , N/A, N/A, %9.3f ' %(Cycle.obj_data.FSUPE ) )
				
			if (Data.IFREZ  !=  0) :
				#WRITE(8,1075) Data.ETAF
				objCycOut.write_or_terminate  ('FREEZER EVAP.,   %9.3f, N/A,  ---- ' %( Data.ETAF ) )
			else:
				#WRITE(8,1076)
				objCycOut.write_or_terminate  (' ' )
			# End if
		# End if

		if (Data.ICOND  !=  0) :
			#WRITE(8,1080) Data.ETAC,FSUBC,FSUPC
			objCycOut.write_or_terminate  ('CONDENSER,     %9.3f,%9.3f, %9.3f ' %(Data.ETAC,Cycle.obj_data.FSUBC,Cycle.obj_data.FSUPC ) )
		else:
			#WRITE(8,1081) FSUBC,FSUPC
			objCycOut.write_or_terminate  ('CONDENSER,     , N/A, %9.3f,%9.3f ' %(Cycle.obj_data.FSUBC,Cycle.obj_data.FSUPC ) )
		# End if

		#WRITE(8,1085)
		#WRITE(8,1090): Data.FLOW * 0.45359
		
		objCycOut.write_or_terminate ("COMPRESSOR PERFORMANCE SUMMARY")
		objCycOut.write_or_terminate ('REFRIGERANT MASS FLOW RATE.,   %9.3f, KG/HR' %( Data.FLOW * 0.45359 ) )
		
		if (Data.IMAP != 0): 
			#WRITE(8,1091) Data.ETAV
			objCycOut.write_or_terminate ('VOLUMETRIC EFFICIENCY,   %9.3f' %(  Data.ETAV * 0.45359 ) )

		#WRITE(8,1092) PR
		objCycOut.write_or_terminate ('PRESSURE RATIO,   %9.3f' %( PR ) )
		
		if (Data.IMAP != 0) :
			#WRITE(8,1093) TSUC
			#WRITE(8,1094) TDISC
			#WRITE(8,1095) TSUPC
			objCycOut.write_or_terminate ('SUCTION PORT TEMPERATURE (C),   %9.3f' %( TSUC ) )
			objCycOut.write_or_terminate ('DISCHARGE PORT TEMPERATURE (C),   %9.3f' %( TDISC ) )
			objCycOut.write_or_terminate ('DISCHARGE SUPERHEAT (C),   %9.3f' %( TSUPC ) )
		# End if

		#WRITE(8,'(A1)') CHAR[12]
		objCycOut.write_or_terminate (" ")
		#
		#	OUTPUT A FIGURE OF THE RESULTS
		#
		#CALL OUTPUT(ICYCL,Cycle.obj_data.T,Data.W,QC,Data.QE,Data.QZ)
		self.showError (" Check cycle fig number ...")
		#
		#	return TO CALLER
		#
		#WRITE(Data.IM_Err,'(''LEAVING CYCLE WITH IC: '',I5,''  Data.IE: '',I5)') Data.IC, Data.IE")
		self.showMsg("LEAVING CYCLE WITH IC:" + str(Data.IC) + str(Data.IE) )
		
		#CLOSE(IO_Cycle)
		#CLOSE(IM_Err)
		
		return

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def cyclos ( self, I_VALVE, T_CYCLE):
		# P8 = self.cyclos (P1, P2)
		#      SUBROUTINE CYCLOS(I_VALVE, T_CYCLE, T_SET, T_ENV, T_EVAP,    T_COND, DUTY, CORRECTION)
		# I_VALVE                        1 IF VALVE USED, 0 IF NOT
		# T_CYCLE                        NUMBER OF CYCLES PER HOUR

		TCYCLE = 1.0/T_CYCLE	#Cycle time
		CORRECTION = 0.0
		if I_VALVE == 0 :
			CORRECTION = 1.0 - 0.010/TCYCLE

		if I_VALVE == 1 : #Use microvalve
			CORRECTION = 1.0 + 0.015/TCYCLE
		return CORRECTION

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def dutfnd (self, ICAB,IRFTYP,ICYCL,N,  QFRSH,QFREZ,FROSTF,FROSTZ,  TS3,TS5,  T,IDFRST):
		# [P9, P10, P15] = self.dutfnd (P1 ... P8, P11 to P15 )
		#	SUBROUTINE DUTFND(ICAB,IRFTYP,ICYCL,N,   QFRSH,QFREZ,FROSTF,FROSTZ,
		#	QFF,QFZ,Cycle.obj_data.TS3,TS5,   T,IDFRST, DUTYR)
		# output    QFF,QFZ,DUTYR
		#	*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
		#	* 	CALCULATE DUTY CYCLE AND THE AVERAGE CABINET LOADS			*
		#	*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *

		if (ICAB  ==  0): return
		#
		#	CALCULATE IN-WALL HEAT LOADS
		#
		TENV = (Data.TROOM + 459.6)/1.8
		TCND = 0.2 * T[14] + 0.4 * T[3] + 0.4 * T[11]

		if (TS5 >  -300.0) :									#Freezer evaporator
			TRFZ = (T[8] + T[9])/2.0
			Data.Q_FZ_IN_WALL = 1.8 * Data.UA_FZ * (TENV - TS5)
			Data.Q_ML_IN_WALL = 1.8 * Data.UA_ML * (Cycle.obj_data.TS3  - TS5)

			Data.CAPZ_IN_WALL = 1.8 * Data.UA_FZ * (TENV - TRFZ)
			Data.CAPM_IN_WALL = 1.8 * Data.UA_ML * (Cycle.obj_data.TS3  - TRFZ)

			Data.Q_FZ_FF = 1.8 * Data.UA_ML * (TS5 - TRFZ)
		else:
			Data.Q_FZ_IN_WALL = 0
			Data.Q_ML_IN_WALL = 0

			Data.CAPZ_IN_WALL = 0
			Data.CAPM_IN_WALL = 0

			Data.Q_FZ_FF = 0
		# End if

		TRFF = (T[5] + T[7])/2.0
		Data.Q_FF_IN_WALL = 1.8 * Data.UA_FF * (TENV - Cycle.obj_data.TS3)
		Data.CAPE_IN_WALL = 1.8 * Data.UA_FF * (TENV - TRFF)
		Data.CONDF_IN_WALL = 1.8 * Data.UA_FF_CND * (TCND - TENV)
		Data.CONDZ_IN_WALL = 1.8 * Data.UA_FZ_CND * (TCND - TENV)
		#
		#	BRANCH ACCORDING TO THE TYPE OF REFRIGERATOR
		#
		QFF = Data.FFQ
		QFZ = Data.FZQOFF
		#

		if IRFTYP in [1, 3]:
			if (ICYCL  ==  1) :
				if (Cycle.obj_data.IDFRST  ==  0) :
					QFF = QFF + Cycle.obj_data.FROSTF
					QFZ = QFZ + Cycle.obj_data.FROSTF
				# End if

				Data.CAPE = QFRSH/1.0548 - 3.413 * Data.FANE - 3.413 * Data.DFSTCYC	\
					- 3.413 * Data.FFCYC	- 3.413 * Data.FZCYC	\
					- Data.CONDF_IN_WALL - Data.CONDZ_IN_WALL

				Data.DUTYC = (QFF + QFZ)/Data.CAPE
				if (Data.DUTYC >  1.0): Data.DUTYC = 1.0
				DUTYR = Data.DUTYC
			# End if

			if (ICYCL  ==  2) :
				QFF = QFF - Cycle.obj_data.FROSTF
				if (Cycle.obj_data.IDFRST  ==  0): QFZ = QFZ + Cycle.obj_data.FROSTF

				Data.CAPZ = QFREZ/1.0548 - 3.413 * Data.FANZ - 3.413 * Data.DFSTCYC	\
					+ Data.Q_FZ_IN_WALL + Data.Q_ML_IN_WALL	\
					- Data.CAPZ_IN_WALL - Data.CAPM_IN_WALL	\
					- Data.CONDZ_IN_WALL - 3.413 * Data.FZCYC	\
					- Data.Q_HXS_FZ/1.0548

				if (Data.CAPZ  <=  0.0) :
					#CALL window (10, 15, 15, 65, 32, 1)
					#CALL gotoxy (22,12)
					#CALL print ('Incorrect SolutION -- ',22,-2)
					#CALL print ('Check Mass Flow',15,-2)
					self.showError ("Incorrect Solution, Check Mass Flow")

					#CALL gotoxy (22,13)
					#CALL print ('SolutION being Terminated',25,-2)
					#CALL setatr[1]
					#CALL gotoxy (0, 24)
					#CALL print (' ', 1, -2)
					self.showError ("Solution being Terminated")

					# say beep CALL WARBLE
					# say beep CALL WARBLE
					# say beep CALL WARBLE

					sys.exit(100)	#STOP ' '
				# End if

				Data.DUTYZ = QFZ/Data.CAPZ

				Data.CAPE = QFRSH/1.0548 - 3.413 * Data.FANE - 3.413 * Data.FFCYC	\
					+ Data.Q_FF_IN_WALL - Data.CAPE_IN_WALL	\
					- Data.CONDF_IN_WALL + Data.Q_FZ_FF	\
					- Data.Q_HXS_FF/1.0548

				Data.DUTYE = QFF/Data.CAPE

				Data.DUTYC = min(Data.DUTYE,Data.DUTYZ)

				if (Data.DUTYC >  1.0): Data.DUTYC = 1.0
				DUTYR = max(Data.DUTYE, Data.DUTYZ)

				if (DUTYR >  1.0): DUTYR = 1.0
			# End if

			if (ICYCL  ==  3) :
				if (N  ==  1) :
					if (Cycle.obj_data.IDFRST  ==  0): QFZ = QFZ + Cycle.obj_data.FROSTF

					Data.CAPZ = QFRSH/1.0548 - 3.413 * Data.FANE	- 3.413 * (Data.DFSTCYC + Data.FZCYC)
					Data.DUTYZ = QFZ/Data.CAPZ

					Data.DUTYC = min(Data.DUTYZ,1.0)
					Data.DUTYZ = Data.DUTYC

				else:
					Data.CAPE = QFRSH/1.0548 - 3.413 * (Data.FANE + Data.FFCYC) + Data.Q_FF_IN_WALL - Data.CAPE_IN_WALL
					QFF = QFF - Cycle.obj_data.FROSTF
					Data.DUTYE = QFF/Data.CAPE
					Data.DUTYC = min(Data.DUTYE,1.0)
					Data.DUTYE = Data.DUTYC
				# End if

				DUTYR = Data.DUTYC
			# End if
			else :
				#CASE DEFAULT					!One door type units
				if (Cycle.obj_data.IDFRST  ==  0): QFZ = QFZ + Cycle.obj_data.FROSTF
				Data.CAPE = QFRSH/1.0548 - 3.413 * (Data.FANE + Data.DFSTCYC + Data.FZCYC)	\
					+ Data.Q_FF_IN_WALL - Data.CAPE_IN_WALL						\
					- Data.CONDF_IN_WALL - Data.Q_HXS_FF/1.0548

				Data.DUTYE = QFZ/Data.CAPE
				Data.DUTYC = min(Data.DUTYE,1.0)
				DUTYR = Data.DUTYC

		#END SELECT
		return [QFF,QFZ,DUTYR]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def shwfig(self, ICYCL):
		if ICYCL == 1:
			self.showMsg ("Show Single cycle PIC")
			if Data.IEVAP == 0:
				self.showMsg ("STANDARD SINGLE EVAPORATOR CYCLE")
				
			if Data.IEVAP == 1:
				self.showMsg ("FRESH FOOD SECTION")

			if Data.IEVAP == 2:
				self.showMsg ("REEZER SECTION ")
		else:
			self.showMsg ("Show Lorenze cycle PIC")
			if IEVAP == 1:
				self.showMsg ("LORENZ CYCLE")
			if IEVAP == 2:
				self.showMsg ("UAL EVAP CYCLE")


		if ICYCL == 3:
			self.showMsg('DUAL LOOP CYCLE: FRESH FOOD LOOP')
		else:
			self.showMsg('DUAL LOOP CYCLE: FREEZER LOOP')

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def showError (self, strMsg, fltValue=0.0, isOnlyTest = False):
		print ("\n\n==========================Error detected ============================")
		if isOnlyTest :
			print ("	Error : ", strMsg)
		else:
			#print('{}\t\t{}'.format(strMsg, fltValue))
			print ("	Error : ", strMsg + "\t\t %10.3f"  %(fltValue))	
		print ("=====================================================================\n\n")
	
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def progrs(self, ICYCL,H,HOUT,WM,FLOW,QCAN):
		#	  SUBROUTINE PROGRS(ICYCL,H,HOUT,WM,FLOW,QCAN)
		#     ******************************************************************
		#     *    DISPLAY THE INTERMEDIATE SOLUTIONS ON THE SCREEN            *
		#     ******************************************************************
		#
		#
		#          ESTABLISH THE OUTPUT VALUES
		#
		QE = ((H[7] - H[5])/WM)*(0.431*FLOW)
		QZ = ((H[9] - H[8])/WM)*(0.431*FLOW)
		W  = ((HOUT - H[1])/WM)*(0.431*FLOW)/(1.0 - QCAN)
		COPR = (QE + QZ)/W
		
		#
		#          OUTPUT RESULTS
		#
		self.showMsg ("====================",0, True)
		if (ICYCL == 2) :
			self.showMsg ('Mass Flow:', FLOW/2.2046)	# ,5,1
			self.showMsg ('Freezer:', int(QZ/3.413)  )
			self.showMsg ('Fresh Food:', int(QE/3.413) )
			self.showMsg ('Compressor:',int(W/3.413)   )
			self.showMsg ('COP:', COPR)
			
		else:
			self.showMsg ('Mass Flow:',  FLOW/2.2046  ) # ,5,1
			self.showMsg ('Evaporator:', int(QE/3.413)  )
			self.showMsg ('Compressor:', int(W/3.413)   )
			self.showMsg ('COP:',COPR)
		self.showMsg ("====================", 0, True)
		return		
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def showMsg (self, strMsg, fltValue=0.0, isOnlyTest = False):
		#return
		if isOnlyTest :
			print ("\t ", strMsg)
		else:
			print ("\t ", strMsg + "\t\t %10.3f"  %(fltValue))	
			