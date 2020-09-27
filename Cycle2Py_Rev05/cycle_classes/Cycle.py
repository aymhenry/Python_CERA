# Python import
import math, sys
from abc import ABC,abstractmethod

# User import
from .Data import Data
from .CycleUtil import *
from .Block2 import Block2

from .Compressor import *
from .Adjlod import Adjlod
from .EvapType import *
from .EvapCooling import *
from .CondCooling import *

#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
class Cycle (Adjlod, HeatExch, CycleUtil, Block2, Data):

	obj_parameter = None
		
	def __init__ (self, obj_parameter, obj_cdata):
		Block2.setup() # setup data in Boock2
		
		Cycle.obj_parameter = obj_parameter
		Data.obj_cdata = obj_cdata
		
		Cycle.obj_parameter.FSUBC = 0.0 # in Python only, it has no value in Fortant
		Cycle.obj_parameter.FSUPE = 0.0 # in python only
		Cycle.obj_parameter.FSUPC = 0.1
		Cycle.obj_parameter.TEMIN = 210.0
		
		
		Cycle.obj_parameter.JE = 0
		Cycle.obj_parameter.JC = 0
		Cycle.obj_parameter.ITMAXC = 100
		Cycle.obj_parameter.ITMAXE = 40
		
		Cycle.obj_parameter.TS = [0.0] *(16+1)
		Cycle.obj_parameter.WMAVGL = [0.0] *(16+1)
		Cycle.obj_parameter.WMAVGV = [0.0] *(16+1)
		Cycle.obj_parameter.AIRTMP = [0.0] *(16+1)		

		Cycle.obj_parameter.XREF= [0.0] *(5+1)

		Cycle.obj_parameter.XV = [[-9999999.9] * (16+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		Cycle.obj_parameter.XL = [[-9999999.9] * (16+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
			
		Cycle.obj_parameter.XL_Temp = [0.0] * len(Cycle.obj_parameter.XL) # in python only
		Cycle.obj_parameter.XV_Temp = [0.0] * len(Cycle.obj_parameter.XV) # in python only
		Cycle.obj_parameter.XV15 = [0.0] * (5+1) # in Python only
		
		Cycle.obj_parameter.T = [-9999999.9] * (16+1)
		Cycle.obj_parameter.S = [-9999999.9] * (16+1)
		Cycle.obj_parameter.H = [-9999999.9] * (16+1)
		Cycle.obj_parameter.XQ= [-9999999.9] * (16+1)

		Cycle.obj_parameter.P = [-9999999.9] * (16+1)
		Cycle.obj_parameter.V = [-9999999.9] * (16+1)
		Cycle.obj_parameter.X = [0.0] * (5+1)
		Cycle.obj_parameter.VL= [-9999999.9] * (16+1)
		Cycle.obj_parameter.VV= [-9999999.9] * (16+1)

		Cycle.obj_parameter.TE= [0.0] * (3+1)
		Cycle.obj_parameter.TC= [0.0] * (3+1)
		
		'''
		Cycle.obj_parameter.AIRTMP = [True,  #add new item to start list at 1	\
			False, False, True,  False, False,	\
			True,  False, False, False, False,	\
			False, True,  False, True,  False]
				
		# in python add extra item for item 0
		Cycle.obj_parameter.HSTATE = ['','COMP IN','COMP DIS','COND IN','COND DEW',	\
					'COND BUB','COND OUT','LIQ LINE',	\
					'SUBCOOL1','SUBCOOL2','FREZ IN ','FREZ OUT','FRSH IN ',	\
					'FRSH DEW','FRSH OUT','HX1 OUT ']

		# in python add extra item for item 0
		Cycle.obj_parameter.MSTATE = ['','COMP IN','COMP DIS','COND IN','COND DEW',	\
					'COND BUB','COND OUT','LIQ LINE',			\
					'SUBCOOL ','EVAP IN ', 'EVAP DEW','EVAP OUT','HX OUT  ']		

		# in python add extra item for item 0
		#Cycle.obj_parameter.LPNT  = [0,1,2,14,3,11,4,16,6,10,8,9,5,12,7,13]
		'''
		
		#create basic objects according to the specifications of cycle
		self.create_basic_obj()
		
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def create_basic_obj (self):
		self.objEvapType = None  # evaporator type
		self.objEvapCool = None  # evaporator cooling method
		self.objCondType = None  # Condenser cooling method
		self.objCompType = None  # Condenser cooling method
		
		print ("aym  Data.obj_cdata.IMAP =", Data.obj_cdata.IMAP)
		print ("aym  Data.obj_cdata.ICOND=", Data.obj_cdata.ICOND)
		print ("aym  Data.obj_cdata.IFRSH=", Data.obj_cdata.IFRSH)
		print ("aym  Data.obj_cdata.ISPEC=", Data.obj_cdata.ISPEC)
		
		if (Data.obj_cdata.IMAP ==  0) : #Map
			self.objCompType = Comp_Map(Cycle.obj_parameter)
			
		elif (Data.obj_cdata.IMAP  ==  1) : # ERR
			self.objCompType = Comp_ERR(Cycle.obj_parameter)
			
		else: # Efficiency Model
			self.objCompType = Comp_EMOD(Cycle.obj_parameter)
			
		#----------------------------
		if (Data.obj_cdata.ICOND ==  0) :
			self.objCondType = CondCool_CNat(Cycle.obj_parameter)
			
		elif (Data.obj_cdata.ICOND  ==  1) :
			self.objCondType = CondCool_CCross(Cycle.obj_parameter)
			
		else : #(Data.obj_cdata.ICOND  ==  2) :
			self.objCondType = CondCool_CCount(Cycle.obj_parameter)
			
		#----------------------------
		if (Data.obj_cdata.IFRSH  ==  0) :
			self.objEvapCool = EvapCool_FFNat(Cycle.obj_parameter)
			
		elif (Data.obj_cdata.IFRSH  ==  1) :
			self.objEvapCool = EvapCool_FFCross(Cycle.obj_parameter)
			
		else : #(Data.obj_cdata.IFRSH  ==  2) :
			self.objEvapCool = EvapCool_FFCount(Cycle.obj_parameter)
			
		#----------------------------
		if (Data.obj_cdata.ISPEC  ==  1) :
			self.objEvapType = EvapSuper(Cycle.obj_parameter)
			
		elif (Data.obj_cdata.ISPEC  ==  2) :
			self.objEvapType = EvapIHX(Cycle.obj_parameter)
			
		else :	#(Data.obj_cdata.ISPEC  ==  3) :
			self.objEvapType = EvapQual(Cycle.obj_parameter)
	
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def mixair ( self, CAP,QFF,QFZ,  TFF,TFZ,CFME) :
		#[P7, P8] = self.mixair (P1 to P6)
		#	  SUBROUTINE MIXAIR(CAP,QFF,QFZ,TFF,TFZ,CFME   ,TIN,X)
		#     ******************************************************************
		#     *     CALCULATE INLET TEMPERATURE TO THE EVAPORATOR              *
		#     ******************************************************************

		#          SET UP THE QUADRATIC EQUATION COEFFICIENTS
		#
		#	  COMMON /FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
		
		A = 1.08  *CFME * (TFF - TFZ)/CAP
		B = - (A + 1.0)
		C = QFF/(QFF+QFZ)
		
		# Solve the quadratic equation
		X = - B/(2.0*A) - math.sqrt(B**2 - 4.0*A*C)/(2.0*A)
		TIN = X*TFF + (1.0 - X)*TFZ
		Data.obj_cdata.FF_AIR = X
		return [TIN,X]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	#def cycle (self, NC,IR,XM,F, TS1,TS3,TS5, MEFF,QHILO,QCAN,	\
	#			DPC,DPE,DPF,ETHX1,ETHX2,DISPLC,NCYC,FROSTF,FROSTZ,ICAB,	\
	#			IRFTYPE,ICYCL,ICYCLS,IDFRST):
	
	def cycle (self):
		print ("aym Rev05")
		print ("aym Cycle.obj_parameter.NC,",Cycle.obj_parameter.NC)
		print ("aym Cycle.obj_parameter.IR,",Cycle.obj_parameter.IR)
		print ("aym Cycle.obj_parameter.XM,",Cycle.obj_parameter.XM)
		print ("aym Cycle.obj_parameter.F,",Cycle.obj_parameter.F)
		
		print ("aym Cycle.obj_parameter.TS1,",Cycle.obj_parameter.TS1, Cycle.obj_parameter.TS1-273.11)
		print ("aym Cycle.obj_parameter.TS3,",Cycle.obj_parameter.TS3, Cycle.obj_parameter.TS3-273.11)
		print ("aym Cycle.obj_parameter.TS5,",Cycle.obj_parameter.TS5, Cycle.obj_parameter.TS5-273.11)
		
		print ("aym Cycle.obj_parameter.MEFF,",Cycle.obj_parameter.MEFF)
		print ("aym Cycle.obj_parameter.QHILO,",Cycle.obj_parameter.QHILO)
		print ("aym Cycle.obj_parameter.QCAN,",Cycle.obj_parameter.QCAN)
		print ("aym Cycle.obj_parameter.DPC,",Cycle.obj_parameter.DPC)
		print ("aym Cycle.obj_parameter.DPE,",Cycle.obj_parameter.DPE)
		print ("aym Cycle.obj_parameter.DPF,",Cycle.obj_parameter.DPF)
		print ("aym Cycle.obj_parameter.ETHX1,",Cycle.obj_parameter.ETHX1)
		print ("aym Cycle.obj_parameter.ETHX2,",Cycle.obj_parameter.ETHX2)
		
		print ("aym Cycle.obj_parameter.DISPLC,",Cycle.obj_parameter.DISPLC)
		print ("aym Cycle.obj_parameter.NCYC,",Cycle.obj_parameter.NCYC)
		
		print ("aym Cycle.obj_parameter.FROSTF,",Cycle.obj_parameter.FROSTF)
		print ("aym Cycle.obj_parameter.FROSTZ,",Cycle.obj_parameter.FROSTZ)
		print ("aym Cycle.obj_parameter.ICAB,",Cycle.obj_parameter.ICAB)
		print ("aym Cycle.obj_parameter.IRFTYPE,",Cycle.obj_parameter.IRFTYPE)
		print ("aym Cycle.obj_parameter.ICYCL,",Cycle.obj_parameter.ICYCL)
		print ("aym Cycle.obj_parameter.ICYCLS,",Cycle.obj_parameter.ICYCLS)
		print ("aym Cycle.obj_parameter.IDFRST,",Cycle.obj_parameter.IDFRST)
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
		#			Cycle.obj_parameter.DPC - PRESSURE DROP THROUGH CONDENSER
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
		Cycle.obj_parameter.str_err = "" # errors description only in Python
		
		IRET = 6
		#	!! Don't allow adjustment to freezer evap if not one-door refrigerator
		if (Cycle.obj_parameter.IRFTYPE  !=  6): IRET = 7

		Cycle.obj_parameter.IRFTYP = Cycle.obj_parameter.IRFTYPE
		if (Cycle.obj_parameter.IRFTYPE  ==  7): Cycle.obj_parameter.IRFTYP = 1

		#	SET UP LOGICAL VECTOR ON AIR TEMPERATURES
		if (Cycle.obj_parameter.ICYCL  ==  2) :
			Cycle.obj_parameter.AIRTMP[10] = True
			Cycle.obj_parameter.AIRTMP[11] = True
		else:
			Cycle.obj_parameter.AIRTMP[10] = False
			Cycle.obj_parameter.AIRTMP[11] = False

		'''
		#	OPEN OUTPUT FILE
		objCycOut = FileAccess (Cycle.FILE_CYCLE_OUT,  "write")  # IO_Cycle Tag
		objCycOut.write_or_terminate (" ") 

		now = datetime.datetime.now( )

		if (Cycle.obj_parameter.NCYC  !=  2) :
			objCycOut.write_or_terminate( ( now.strftime( "%H %M %S %d %b %Y" ) ) + " - Python Output aymhenry@gmail")

		
		#	OUTPUT INFORMATION ON TYPE OF CYCLE
		if (Cycle.obj_parameter.ICYCL  ==  1) :
			if (Cycle.obj_parameter.ICYCLS  ==  1) :
				objCycOut.write_or_terminate('STANDARD ONE EVAPORATOR CYCLE')
			else:
				if (Data.obj_cdata.ITYPE  ==  1  or  Data.obj_cdata.ITYPE  ==  4) :
					objCycOut.write_or_terminate('DUAL EVAP CYCLE: FRESH FOOD LOOP')
				else:
					objCycOut.write_or_terminate('DUAL EVAP CYCLE: FREEZER LOOP')

		if (Cycle.obj_parameter.ICYCL  ==  2) :
			if (Cycle.obj_parameter.ICYCLS  ==  2) :
				objCycOut.write_or_terminate('LORENZ CYCLE')
			else:
				objCycOut.write_or_terminate('DUAL EVAP CYCLE')

		if (Cycle.obj_parameter.ICYCL  ==  3) :
			if (Cycle.obj_parameter.NCYC  ==  1) :
				objCycOut.write_or_terminate('DUAL LOOP CYCLE - FREEZER')
			else:
				objCycOut.write_or_terminate(" ")
				objCycOut.write_or_terminate('DUAL LOOP CYCLE - FRESH FOOD')

		objCycOut.write_or_terminate(" ")
		'''
		
		if (Data.obj_cdata.ITYPE  ==  3): Data.obj_cdata.ITYPE = 1

		'''
		#	OUTPUT REFRIGERATION MIXTURE INFORMATION
		
		#X2 = 100.0 * Cycle.obj_parameter.XM[1]

		objCycOut.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(100.0 * Cycle.obj_parameter.XM[1], Data.HREF[ Cycle.obj_parameter.IR[1] ])  )
				
		if (Cycle.obj_parameter.NC >  1) :
			for I in range (2, Cycle.obj_parameter.NC+1) : 
				X2 = 100.0 * Cycle.obj_parameter.XM[I]
				objCycOut.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(X2, Data.HREF[ Cycle.obj_parameter.IR[1] ])  ) # fixed in python

				

		objCycOut.write_or_terminate(" ")
		objCycOut.write_or_terminate("OUTPUT RESULTS")
		'''
		
		#	INITIALIZE COMPRESSOR MAP ANALYSIS
		DUTYR = 0.5
		Data.obj_cdata.IREAD = 0
		Data.obj_cdata.DISPL = Cycle.obj_parameter.DISPLC
		
		[Data.obj_cdata.EFFC, Data.obj_cdata.CE ] = self.objCompType.map(Data.obj_cdata.ICOMP, Data.obj_cdata.ICOOL, Data.obj_cdata.EER, Data.obj_cdata.SIZE, Data.obj_cdata.DISPL, Data.obj_cdata.SPEEDN)
		
		
		#if (Data.obj_cdata.IMAP  ==  1) :
		#	[Data.obj_cdata.EFFC, Data.obj_cdata.CE ] =self.map(Data.obj_cdata.ICOMP, Data.obj_cdata.ICOOL, Data.obj_cdata.EER, Data.obj_cdata.SIZE, Data.obj_cdata.DISPL, Data.obj_cdata.SPEEDN)

		#	INITIALIZE THERMODYNAMIC DATA
		self.bconst (Cycle.obj_parameter.NC,Cycle.obj_parameter.IR,Cycle.obj_parameter.F)

		#	CONVERT TO MOLAR COMPOSITION FOR THE CALCULATION OF ALL PROPERTIES
		WMSUM = 0.0
		WMAVG = 0.0

		for I in range (1, Cycle.obj_parameter.NC+1) : 
			WMSUM = WMSUM+Cycle.obj_parameter.XM[I] / Data.WM[I]

		for I in range (1, Cycle.obj_parameter.NC+1) : 
			Cycle.obj_parameter.X[I] = Cycle.obj_parameter.XM[I] / Data.WM[I] / WMSUM
			WMAVG = WMAVG+Cycle.obj_parameter.X[I] * Data.WM[I]

		#	INITIAL GUESSES FOR TC AND TE
		#	ASSUME TEMP RISE OF COND IS 0.5 F PER LBM
		Cycle.obj_parameter.TC[1] = Cycle.obj_parameter.TS1 + Data.obj_cdata.MREF/3.6

		#	GUESS A DEW POINT TEMPERATURE AT THE EVAPORATOR EXIT
		self.objEvapType.calc_evap_exit_p7()

		Cycle.obj_parameter.JC = 1
		Cycle.obj_parameter.LCCON = True
		Cycle.obj_parameter.LQUIT = False

		#	SET UP TEMPERATURES AND CABINET LOADS FOR INLET TEMPERATURE
		#	CALCULATION FOR EVAPORATOR OF A STANDARD DESIGN (TYPE 1)
		TFF = Data.obj_cdata.FFTEMP
		TFZ = Data.obj_cdata.FZTEMP

		#	INITIAL GUESS FOR REFRIGERANT MASS FLOW (ASSUME 10 LB/HR)
		Data.obj_cdata.FLOW = Data.obj_cdata.MREF
		FLWREF = Data.obj_cdata.FLOW
		FLOW2 = Data.obj_cdata.FLOW
		Data.obj_cdata.MREF = Data.obj_cdata.MREF/(2.20462*WMAVG)
		Cycle.obj_parameter.MREFSV= Data.obj_cdata.MREF

		#	BEGIN ITERATION FOR CONDENSER OUTLET TEMPERATURE
		self.shwfig(Cycle.obj_parameter.ICYCL)
		ICONC = 0

		#	BEGIN MAIN ITERATION LOOP ON THE CONDENSER TEMPERATURE
		Data.obj_cdata.IC = 1
		Cycle.obj_parameter.ITMAXC = 20 # 3#100
	
		self.showError ("Python, check later, ICNT set in cond method, limit Cycle.obj_parameter.ITMAXC to ", Cycle.obj_parameter.ITMAXC )
		
		while (Data.obj_cdata.IC  <=  Cycle.obj_parameter.ITMAXC  and  Cycle.obj_parameter.LCCON): 
			
			if (Cycle.obj_parameter.ICAB  ==  1) :
				[Cycle.obj_parameter.TS3,Cycle.obj_parameter.TS5] = self.adjlod(Cycle.obj_parameter.ICYCL,Data.obj_cdata.IC,  Cycle.obj_parameter.TS3,Cycle.obj_parameter.TS5,  Cycle.obj_parameter.FROSTF,Cycle.obj_parameter.FROSTF,Cycle.obj_parameter.IDFRST)

			Cycle.obj_parameter.T[4] = Cycle.obj_parameter.TC[Cycle.obj_parameter.JC]

			#	find condenser pressure for current guess of TC
			#	if bublt routine exceeds the critical point write a warning message and return
			self.showMsg ("Iteration number for CONDENSER TEMPERATURE ", Data.obj_cdata.IC)

			TBUB4 = Cycle.obj_parameter.TC[Cycle.obj_parameter.JC] + Data.obj_cdata.DTSUBC

			[Cycle.obj_parameter.X, Cycle.obj_parameter.XV_Temp, Cycle.obj_parameter.P[4], VBUB4, Cycle.obj_parameter.VV[4], LCRIT] = self.bublt (TBUB4,Cycle.obj_parameter.X , Cycle.obj_parameter.XV_Temp, True) 	
			self.setArr2dCol (Cycle.obj_parameter.XV, 4, Cycle.obj_parameter.XV_Temp )
			
			#	determine the specific volume of the liquid
			if (Data.obj_cdata.DTSUBC >  0.0) :
				VGUESS=VBUB4
				[A4, B4] = self.espar (0, Cycle.obj_parameter.TC[Cycle.obj_parameter.JC], Cycle.obj_parameter.X) #	CALL ESPAR(0,Cycle.obj_parameter.TC[JC],Cycle.obj_parameter.X, A4,B4)

				[VGUESS, LCONV] = self.vit (Cycle.obj_parameter.TC[Cycle.obj_parameter.JC],Cycle.obj_parameter.P[4],A4,B4, VGUESS,True) 
				Cycle.obj_parameter.V[4]=VGUESS
			else:
				Cycle.obj_parameter.V[4]=VBUB4

			#	condenser dew point
			Cycle.obj_parameter.P[3] = Cycle.obj_parameter.P[4] + (1.0 - Cycle.obj_parameter.FSUPC) * Cycle.obj_parameter.DPC
			self.showMsg ("Conderser Temp TC (C) ", Cycle.obj_parameter.TC[Cycle.obj_parameter.JC] - 273.11 )

			if (LCRIT) :
				#objCycOut.write_or_terminate ('CRITICAL TEMPERATURE EXCEEDED IN CONDENSER')
				#objCycOut = "" # close file
				Cycle.obj_paramete.str_err = "CRITICAL TEMPERATURE EXCEEDED IN CONDENSER"
				return  Cycle.obj_parameter

			#	ENTHALPY AT STATE 4 (CONDENSER OUTLET)
			[Cycle.obj_parameter.H[4],CV,CP,VSND] = self.hcvcps (1,Cycle.obj_parameter.TC[Cycle.obj_parameter.JC],Cycle.obj_parameter.V[4],Cycle.obj_parameter.X)
			Cycle.obj_parameter.JE=1
			Cycle.obj_parameter.LECON=True
			
			#	ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
			Cycle.obj_parameter.H[16] = Cycle.obj_parameter.H[4] - Data.obj_cdata.CONDHT[Cycle.obj_parameter.NCYC]/ Data.obj_cdata.MREF/DUTYR
			Cycle.obj_parameter.P[16] = Cycle.obj_parameter.P[4]
			
								
			[Cycle.obj_parameter.T[16], Cycle.obj_parameter.XQ[16],Cycle.obj_parameter.XL_Temp ,Cycle.obj_parameter.XV_Temp,  Cycle.obj_parameter.VL[16],Cycle.obj_parameter.VV[16],  HL16,HV16] = self.hpin (Cycle.obj_parameter.H[16],Cycle.obj_parameter.P[16],Cycle.obj_parameter.X)
			
			self.setArr2dCol (Cycle.obj_parameter.XL, 16, Cycle.obj_parameter.XL_Temp )
			self.setArr2dCol (Cycle.obj_parameter.XV, 16, Cycle.obj_parameter.XV_Temp )
						
			if (Cycle.obj_parameter.VL[16]  ==  0.0): Cycle.obj_parameter.VL[16] = Cycle.obj_parameter.V[4]

			Cycle.obj_parameter.V[16] = Cycle.obj_parameter.VL[16]

			#	enter iteration for evaporator outlet temperature
			Data.obj_cdata.IE = 1
			print ("aym Cycle.obj_parameter.ITMAXE=",Cycle.obj_parameter.ITMAXE)
			while ( (Data.obj_cdata.IE  <=  Cycle.obj_parameter.ITMAXE)  and  Cycle.obj_parameter.LECON ): #DO WHILE (Data.obj_cdata.IE  <=  ITMAXE  and  Cycle.obj_parameter.LECON)
				print ("\naym Data.obj_cdata.IE=", Data.obj_cdata.IE)
				self.showMsg ("Iteration Count for (OR EVAPORATOR OUTLET TEMPERATURE) ",Data.obj_cdata.IE)

				Cycle.obj_parameter.I_ERROR_INTER = 0
				# Python comment, only if case of IHX, system may return 1
				if self.objEvapType.iterat_evap_exit_p7()==1:
					Cycle.obj_parameter.LECON = False
					Cycle.obj_parameter.I_ERROR_INTER = 1
					continue

				#	determine the bubble point at the evap exit pressure
				[Cycle.obj_parameter.X, Cycle.obj_parameter.XV15, TBUB15, VBUB15, VV15, LCRIT] = self.bublp ( Cycle.obj_parameter.P[15],Cycle.obj_parameter.X, Cycle.obj_parameter.XV15, True) 

				#	determine the bubble and dew point enthalpies
				[Cycle.obj_parameter.H[15] , CV, CP, VS] = self.hcvcps (1, Cycle.obj_parameter.T[15], Cycle.obj_parameter.V[15], Cycle.obj_parameter.X)	
				[HBUB15, CV, CP, VS] = self.hcvcps (1, TBUB15, VBUB15, Cycle.obj_parameter.X)	
				
				for INC in range (1, Cycle.obj_parameter.NC + 1): 
					Cycle.obj_parameter.XL[INC][13] = Cycle.obj_parameter.XL[INC][15] 

				Cycle.obj_parameter.XQ[13] = 1.0

				#	determine the enthalpy at [7]
				self.objEvapType.iterat_evap_enthalpy_p7()
				self.showMsg ("Outlet from fresh food evaporator (C) - point 7 ", Cycle.obj_parameter.T[7] - 273.11)


				#	interchanger for subcooling condenser liquid
				self.objEvapType.iterat_evap_subcool_p7()
				Cycle.obj_parameter.H[6] = Cycle.obj_parameter.H[16] - Cycle.obj_parameter.QINT
				

				#	determine the remainder of the properties at [6] and [13]
				Cycle.obj_parameter.P[6]  = Cycle.obj_parameter.P[4]
				Cycle.obj_parameter.P[13] = Cycle.obj_parameter.P[7]
				
				[ Cycle.obj_parameter.T[6],XQ6, Cycle.obj_parameter.XL_Temp, Cycle.obj_parameter.XV_Temp, Cycle.obj_parameter.V[6],  VV6,HL,HV] \
					= self.hpin ( Cycle.obj_parameter.H[6],Cycle.obj_parameter.P[6],Cycle.obj_parameter.X )
				self.setArr2dCol (Cycle.obj_parameter.XL, 6, Cycle.obj_parameter.XL_Temp)
				self.setArr2dCol (Cycle.obj_parameter.XV, 6, Cycle.obj_parameter.XV_Temp)
				
				Cycle.obj_parameter.VL[6] = Cycle.obj_parameter.V[6] 
				Cycle.obj_parameter.XQ[6] = 0.0
				
				self.objEvapType.iterat_call_exit_p13()
				
				'''
				if (Data.obj_cdata.ISPEC  !=  2):
					[Cycle.obj_parameter.T[13], Cycle.obj_parameter.XQ13, Cycle.obj_parameter.XL_Temp,  \
						Cycle.obj_parameter.XV_Temp, Cycle.obj_parameter.VL13, Cycle.obj_parameter.V[13], Cycle.obj_parameter.HL, Cycle.obj_parameter.HV] \
						= self.hpin ( Cycle.obj_parameter.H[13],Cycle.obj_parameter.P[13],Cycle.obj_parameter.X )
						
					self.setArr2dCol (Cycle.obj_parameter.XL, 13, Cycle.obj_parameter.XL_Temp)
					self.setArr2dCol (Cycle.obj_parameter.XV, 13, Cycle.obj_parameter.XV_Temp )
				'''

				Cycle.obj_parameter.XQ[13] = 1.0


				self.showMsg ("Liquid line state after heat loss to cabinet & mullion (C) - point 16 ", Cycle.obj_parameter.T[16] - 273.11)
				self.showMsg ("Superheated gas leaving hight temp interchanger (C) - point 13 ", Cycle.obj_parameter.T[13] - 273.11)

				#	FIND CONDITIONS AT EVAPORATOR INLET ASSUMING ISENTHALPIC EXPANSION
				Cycle.obj_parameter.P[5] = Cycle.obj_parameter.P[13] + Cycle.obj_parameter.DPE
				self.showMsg ("liquid line outlet from high temp interchanger (C) -point 6 ", Cycle.obj_parameter.T[6] - 273.11)
				
				#	CALL ANALYSIS OF FREEZER SECTION AND THE LOWER
				#	INTERCHANGER (LOOKS LIKE A NON-ADIABATIC EXPANSION VALVE TO REST OF SYSTEM
				[Cycle.obj_parameter.H, Cycle.obj_parameter.P , Cycle.obj_parameter.X , Cycle.obj_parameter.T, \
					Cycle.obj_parameter.XQ, Cycle.obj_parameter.XL, Cycle.obj_parameter.XV,  Cycle.obj_parameter.VL,   Cycle.obj_parameter.VV,\
					Cycle.obj_parameter.HL, Cycle.obj_parameter.HV, Cycle.obj_parameter.TS6, Cycle.obj_parameter.QFREZ ] \
					= self.lowevp ( Cycle.obj_parameter.ICYCL,0,Cycle.obj_parameter.H,  \
									Cycle.obj_parameter.P,  Cycle.obj_parameter.X,  Cycle.obj_parameter.T,  \
									Cycle.obj_parameter.XQ, Cycle.obj_parameter.XL, Cycle.obj_parameter.XV,  \
									Cycle.obj_parameter.VL, Cycle.obj_parameter.VV, Cycle.obj_parameter.HL, Cycle.obj_parameter.TS3, \
									Cycle.obj_parameter.TS5,Cycle.obj_parameter.DPF,Cycle.obj_parameter.ETHX2)
				
				#	CALCULATE FRESH FOOD SECTION HEAT EXCHANGE
				Cycle.obj_parameter.PDEWE = Cycle.obj_parameter.P[5] - (1.0-Cycle.obj_parameter.FSUPE)*Cycle.obj_parameter.DPE
				if (Cycle.obj_parameter.PDEWE >  Cycle.obj_parameter.P[5]): Cycle.obj_parameter.PDEWE = Cycle.obj_parameter.P[5]
				
				[Cycle.obj_parameter.XREF, Cycle.obj_parameter.X, Cycle.obj_parameter.TDEW, Cycle.obj_parameter.VLDEW,
					Cycle.obj_parameter.VDEW, Cycle.obj_parameter.LCRIT] \
					= self.bublp (Cycle.obj_parameter.PDEWE, Cycle.obj_parameter.XREF, Cycle.obj_parameter.X, False) 

				# Python POLDE is not used !!!
				POLDE = Cycle.obj_parameter.PDEWE
				if (Cycle.obj_parameter.TDEW  >=  Cycle.obj_parameter.TS3): Cycle.obj_parameter.TDEW = Cycle.obj_parameter.TS3 - 1.0
				if (Cycle.obj_parameter.T[5]  >=  Cycle.obj_parameter.TS3): Cycle.obj_parameter.T[5] = Cycle.obj_parameter.TS3 - 1.0

				[Cycle.obj_parameter.HDEW, Cycle.obj_parameter.CVRVAP, Cycle.obj_parameter.CPRVAP, Cycle.obj_parameter.VS] \
					= self.hcvcps (3, Cycle.obj_parameter.TDEW, Cycle.obj_parameter.VDEW, Cycle.obj_parameter.X) 

				#	STATE 12 IS THE POINT AT WHICH THE DEW POINT IS REACHED IN THE EVAPORATOR
				Cycle.obj_parameter.P[12] = Cycle.obj_parameter.PDEWE
				Cycle.obj_parameter.T[12] = Cycle.obj_parameter.TDEW
				Cycle.obj_parameter.V[12] = Cycle.obj_parameter.VDEW
				Cycle.obj_parameter.H[12] = Cycle.obj_parameter.HDEW
				Cycle.obj_parameter.XQ[12] = 1.0
		
				#	FIND DUTY CYCLE, NET CAPACITY AND AVERAGE FREEZER LOAD if ICYCL = 1
				if (Data.obj_cdata.IC  !=  1) :
					[QFF, QFZ, DUTYR] \
						= self.dutfnd (Cycle.obj_parameter.ICAB, Cycle.obj_parameter.IRFTYP,Cycle.obj_parameter.ICYCL, Cycle.obj_parameter.NCYC, \
						Cycle.obj_parameter.QFRSH, Cycle.obj_parameter.QFREZ, Cycle.obj_parameter.FROSTF,\
						Cycle.obj_parameter.FROSTF,Cycle.obj_parameter.TS3,   Cycle.obj_parameter.TS5,  \
						Cycle.obj_parameter.T,     Cycle.obj_parameter.IDFRST )

				#	CALCULATE FRESH FOOD EVAPORATOR HEAT TRANSFER.
				#	TEST FOR STANDARD DESIGN.
				if (Cycle.obj_parameter.IRFTYP  <=  3) :
					if (Cycle.obj_parameter.ICYCL  ==  1  and  Cycle.obj_parameter.ICAB	!=  0  and  Data.obj_cdata.IFRSH  !=  0) :
						if (Data.obj_cdata.IC  ==  1) :
							TIN = 0.15*TFF + 0.85*TFZ
							Cycle.obj_parameter.FF_FRACT = 0.0 # in Python only
						else:
							Data.obj_cdata.CAPE = Cycle.obj_parameter.QFRSH/1.0548 - 3.413 * Data.obj_cdata.FANE - 3.413*(Data.obj_cdata.DFSTCYC + Data.obj_cdata.FZCYC)
							CFMA = Data.obj_cdata.CFME/(1.08*1.8961)
							QFM = QFF + 3.413 * Data.obj_cdata.DUTYC * Data.obj_cdata.FFCYC
							[TIN, Cycle.obj_parameter.FF_FRACT] = self.mixair (Data.obj_cdata.CAPE, QFM, QFZ, TFF, TFZ, CFMA)

						Cycle.obj_parameter.TS3 = (TIN + 459.6)/1.8

				[Cycle.obj_parameter.QFRSH, Cycle.obj_parameter.FSUPE] = self.objEvapCool.calc_heat_temp()
								
				#	superheating fraction
				FSHOLD = Cycle.obj_parameter.FSUPE
				Cycle.obj_parameter.FSUPE = (FSHOLD + Cycle.obj_parameter.FSUPE)/2.0

				if (Cycle.obj_parameter.FSUPE >  1.05*FSHOLD): Cycle.obj_parameter.FSUPE = 1.05*FSHOLD
				if (Cycle.obj_parameter.FSUPE <  0.95*FSHOLD): Cycle.obj_parameter.FSUPE = 0.95*FSHOLD

				
				#	fresh food section evaporator
				[Cycle.obj_parameter.TS4, Cycle.obj_parameter.TE,Cycle.obj_parameter.JE, ICONE] = \
					self.objEvapCool.frsh (Cycle.obj_parameter.H,Cycle.obj_parameter.T,Cycle.obj_parameter.TS3,  
						Cycle.obj_parameter.TE,Cycle.obj_parameter.JE, Cycle.obj_parameter.QFRSH) 
				
				#---------------------------ADDED NEW CODE (12/29/90)-------------------
				Cycle.obj_parameter.T[15] = Cycle.obj_parameter.T[15] + Cycle.obj_parameter.TE[2] - Cycle.obj_parameter.T[7]
				#-----------------------------END OF NEW CODE---------------------------

				#	calculate the average effectiveness of the ff evaporator
				#	calculate the heat transfer if the refrigerant left at TS3
				VGUESS = Cycle.obj_parameter.VDEW * Cycle.obj_parameter.TS3 / Cycle.obj_parameter.TDEW
				[AS3, BS3] = self.espar (0, Cycle.obj_parameter.TS3, Cycle.obj_parameter.X) 

				[VGUESS, LCONV] = self.vit (Cycle.obj_parameter.TDEW,Cycle.obj_parameter.P[7],AS3,BS3,VGUESS,False)
				VS3 = VGUESS

				[HS3, CV, CP, VS] = self.hcvcps (1, Cycle.obj_parameter.TS3, VS3, Cycle.obj_parameter.X) 

				Cycle.obj_parameter.QRMAX = Data.obj_cdata.MREF*(HS3-Cycle.obj_parameter.H[5])

				#	calculate the heat transfer if the air left at T[5]
				Cycle.obj_parameter.QAMAX = Data.obj_cdata.CFME*(Cycle.obj_parameter.TS3-Cycle.obj_parameter.T[5])
				Cycle.obj_parameter.QMAXE = Cycle.obj_parameter.QAMAX

				if (Cycle.obj_parameter.QRMAX < Cycle.obj_parameter.QAMAX) : Cycle.obj_parameter.QMAXE = Cycle.obj_parameter.QRMAX

				Data.obj_cdata.ETAE = Cycle.obj_parameter.QFRSH/Cycle.obj_parameter.QMAXE
				
				if (ICONE  ==  1): Cycle.obj_parameter.LECON = False
				if (Cycle.obj_parameter.TE[Cycle.obj_parameter.JE]  <=  Cycle.obj_parameter.TEMIN):  Cycle.obj_parameter.LECON = False
					
				Data.obj_cdata.IE = Data.obj_cdata.IE + 1


			#----------------------------------------------------------------------------
			#	END OF EVAPORATOR ITERATION
			#----------------------------------------------------------------------------
			
			Cycle.obj_parameter.T[7] = Cycle.obj_parameter.TE[Cycle.obj_parameter.JE]
			self.showMsg ("outlet from fresh food evaporator (C) - point 7 ", Cycle.obj_parameter.T[7] - 273.11)
			#
			#	INTERCHANGER FOR SUBCOOLING CONDENSER LIQUID
			#-----------------NEW CODE ADDED FOR INCOMPLETE EVAPORATION-------------

			#	Python : repeated code, determine the enthalpy at [7]
			self.objEvapType.iterat_evap_enthalpy_p7()

			#	interchanger for subcooling condenser liquid
			self.objEvapType.iterat_evap_subcool_p7()
			
			Cycle.obj_parameter.H[6] = Cycle.obj_parameter.H[16] -  Cycle.obj_parameter.QINT
			[Cycle.obj_parameter.T[6],XQ6,Cycle.obj_parameter.XL_Temp ,Cycle.obj_parameter.XV_Temp ,Cycle.obj_parameter.V[6],   VV6,HL,HV] \
				= self.hpin ( Cycle.obj_parameter.H[6],Cycle.obj_parameter.P[6],Cycle.obj_parameter.X )

			self.setArr2dCol (Cycle.obj_parameter.XL, 6, Cycle.obj_parameter.XL_Temp)
			self.setArr2dCol (Cycle.obj_parameter.XV, 6, Cycle.obj_parameter.XV_Temp )
			
			Cycle.obj_parameter.VL[6] = Cycle.obj_parameter.V[6]
			Cycle.obj_parameter.XQ[6] = 0.0

			# calc. point 13 Temp
			self.objEvapType.iterat_call_exit_p13()
			'''
			if (Data.obj_cdata.ISPEC  !=  2) :
				[Cycle.obj_parameter.T[13],XQ13,Cycle.obj_parameter.XL_Temp, Cycle.obj_parameter.XV_Temp, L13,Cycle.obj_parameter.V[13],HL,HV] = self.hpin ( Cycle.obj_parameter.H[13],Cycle.obj_parameter.P[13],Cycle.obj_parameter.X )

				self.setArr2dCol (Cycle.obj_parameter.XL, 13, Cycle.obj_parameter.XL_Temp )
				self.setArr2dCol (Cycle.obj_parameter.XV, 13, Cycle.obj_parameter.XV_Temp )
			'''

			Cycle.obj_parameter.XQ[13] = 1.0

			self.showMsg ("superheated gas leaving the high temp interchanger (C) - point 13", Cycle.obj_parameter.T[13] - 273.11)

			Cycle.obj_parameter.TE[1] = Cycle.obj_parameter.TE[Cycle.obj_parameter.JE]

			#	FIND ENTROPY AT COMPRESSOR INLET AND COMPUTE CONDITIONS AT
			#	COMPRESSOR OUTLET (TSPEC IS DEFINED INLET TEMP TO THE
			#	COMPRESSOR (-1 MEANS NO TEMPERATURE CHANGE)
			#
			Cycle.obj_parameter.P[1] = Cycle.obj_parameter.P[13]
			if (Data.obj_cdata.TSPEC >  0.0) :
				Cycle.obj_parameter.T[1] = Data.obj_cdata.TSPEC
				VGUESS = Cycle.obj_parameter.V[7]*Cycle.obj_parameter.T[1]/Cycle.obj_parameter.T[7]
				
				[A1, B1] = self.espar (0, Cycle.obj_parameter.T[1], Cycle.obj_parameter.X) 
				
				
				[VGUESS, LCONV] = self.vit ( Cycle.obj_parameter.T[1],Cycle.obj_parameter.P[1],A1,B1,VGUESS,False)
				Cycle.obj_parameter.V[1] = VGUESS
				
				[Cycle.obj_parameter.H[1],CV,CP,VS] = self.hcvcps (1,Cycle.obj_parameter.T[1],Cycle.obj_parameter.V[1],Cycle.obj_parameter.X) 
			else:
				
				Cycle.obj_parameter.T[1] = Cycle.obj_parameter.T[13]
				Cycle.obj_parameter.H[1] = Cycle.obj_parameter.H[13]
				Cycle.obj_parameter.V[1] = Cycle.obj_parameter.V[13]
				Cycle.obj_parameter.XQ[1] = Cycle.obj_parameter.XQ[13]
			# End if
			
			Cycle.obj_parameter.S[1] = self.entrop(Cycle.obj_parameter.T[1],Cycle.obj_parameter.V[1],Cycle.obj_parameter.X)
			Cycle.obj_parameter.P[2] = Cycle.obj_parameter.P[3] + Cycle.obj_parameter.FSUPC*Cycle.obj_parameter.DPC
			
			self.showMsg ("compressor inlet (saturated vapor) (C) - point 1", Cycle.obj_parameter.T[1] - 273.11)
			
			print ("aym =====3================ self.obj_cdata.CE =", self.obj_cdata.CE)
			# CALL COMPRESSOR MODEL
			[Cycle.obj_parameter.T, HOUT, Cycle.obj_parameter.QHILO, Cycle.obj_parameter.QCAN,VSUC,  VV2, Cycle.obj_parameter.TSUC, Cycle.obj_parameter.TDISC, GAMA, RN, Cycle.obj_parameter.ETAS] \
				= self.objCompType.comp_balance()
			
			# repeted code, get it out of IF
			FLOW2 = FLWREF * Data.obj_cdata.MREF/ Cycle.obj_parameter.MREFSV
			
			#	SHOW THE PROGRESS IN THE SOLUTION
			self.progrs (Cycle.obj_parameter.ICYCL,Cycle.obj_parameter.H,HOUT,WMAVG,FLOW2,Cycle.obj_parameter.QCAN)
			#
			#	CONDITIONS OF GAS LEAVING COMPRESSOR SHELL
			#

			[Cycle.obj_parameter.H[2],CV,CP,VS] = self.hcvcps (1, Cycle.obj_parameter.T[2], VV2, Cycle.obj_parameter.X) 

			[Cycle.obj_parameter.T[2],Cycle.obj_parameter.XQ[2],Cycle.obj_parameter.XL_Temp, Cycle.obj_parameter.XV_Temp ,VL2,VV2,HL2,HV2] = self.hpin ( Cycle.obj_parameter.H[2],Cycle.obj_parameter.P[2],Cycle.obj_parameter.X )
			self.setArr2dCol (Cycle.obj_parameter.XL, 2, Cycle.obj_parameter.XL_Temp)
			self.setArr2dCol (Cycle.obj_parameter.XV, 2, Cycle.obj_parameter.XV_Temp )
			
			Cycle.obj_parameter.V[2] = VV2
			#
			#	ENTROPY OF GAS LEAVING COMPRESSOR
			#
			if (Cycle.obj_parameter.XQ[2] <  1.0) :
				SL2 = self.entrop(Cycle.obj_parameter.T[2],VL2,self.getArr2dCol (Cycle.obj_parameter.XL,2) ) 
				SV2 = self.entrop(Cycle.obj_parameter.T[2],VV2,self.getArr2dCol (Cycle.obj_parameter.XV,2) ) 
				Cycle.obj_parameter.S[2] = Cycle.obj_parameter.XQ[2]*SV2 + (1.0-Cycle.obj_parameter.XQ[2])*SL2
			else:
				Cycle.obj_parameter.S[2] = self.entrop(Cycle.obj_parameter.T[2],VV2,Cycle.obj_parameter.X)

			self.showMsg ("compressor discharge (C) - point 2", Cycle.obj_parameter.T[2] - 273.11)

			if (ICONC  !=  1) :
				#
				#	CALCULATE CONDENSER HEAT EXCHANGE
				#	DETERMINE THE DEW POINT CONDITIONS

				# [P2, P3, P4, P5, P6, P8] = bublp ( P1, P2, P3,    P7)
				[Cycle.obj_parameter.XL_Temp ,Cycle.obj_parameter.X,TDEW,VL_notused,VDEW, LCRIT] = self.bublp (Cycle.obj_parameter.P[3], Cycle.obj_parameter.XL_Temp ,Cycle.obj_parameter.X,False) 
				self.setArr2dCol (Cycle.obj_parameter.XL, 3, Cycle.obj_parameter.XL_Temp)
				
				Cycle.obj_parameter.T[3] = TDEW
				Cycle.obj_parameter.V[3] = VDEW

				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[HDEW,CVRVAP,CPRVAP,VS] = self.hcvcps (3,TDEW,VDEW,Cycle.obj_parameter.X) 

				Cycle.obj_parameter.H[3] = HDEW
				#
				#	DETERMINE BUBBLE POINT CONDITIONS
				#	ASSUME A LINEAR PRESSURE DROP THROUGHOUT THE CONDENSER
				#
				PBUB = Cycle.obj_parameter.P[4] + Cycle.obj_parameter.DPC * Cycle.obj_parameter.FSUBC

				[Cycle.obj_parameter.X, Cycle.obj_parameter.XV_Temp ,TBUB,VBUB,V4, LCRIT] = self.bublp ( PBUB,Cycle.obj_parameter.X, Cycle.obj_parameter.XV_Temp, True) 
				self.setArr2dCol (Cycle.obj_parameter.XV, 4, Cycle.obj_parameter.XV_Temp )
				
				[HBUB,CVRLIQ,CPRLIQ,VS] = self.hcvcps (3,TBUB,VBUB,Cycle.obj_parameter.X) 

				#
				#	STATE 11 IS THE POINT AT WHICH THE BUBBLE POINT
				#	IS REACHED IN THE CONDENSER
				#
				Cycle.obj_parameter.P[11] = PBUB
				Cycle.obj_parameter.T[11] = TBUB
				Cycle.obj_parameter.V[11] = VBUB
				Cycle.obj_parameter.H[11] = HBUB
				#
				#	DETERMINE CONDITIONS ENTERING THE CONDENSER
				#
				HDROP = Data.obj_cdata.CONDVP[Cycle.obj_parameter.NCYC]/Data.obj_cdata.MREF/Data.obj_cdata.DUTYC

				Cycle.obj_parameter.P[14] = Cycle.obj_parameter.P[2]
				HINCND = Cycle.obj_parameter.H[2] - HDROP

				Cycle.obj_parameter.H[14] = HINCND
				
				[Cycle.obj_parameter.T[14], Cycle.obj_parameter.XQ[14], Cycle.obj_parameter.XL_Temp, Cycle.obj_parameter.XV_Temp,Cycle.obj_parameter.VL[14],Cycle.obj_parameter.VV[14],HL14,HV14] = self.hpin ( Cycle.obj_parameter.H[14],Cycle.obj_parameter.P[2],Cycle.obj_parameter.X )

				self.setArr2dCol (Cycle.obj_parameter.XL, 14, Cycle.obj_parameter.XL_Temp )
				self.setArr2dCol (Cycle.obj_parameter.XV, 14, Cycle.obj_parameter.XV_Temp )
				
				Cycle.obj_parameter.V[14] = (1.0-Cycle.obj_parameter.XQ[14])*Cycle.obj_parameter.VL[14] + Cycle.obj_parameter.XQ[14]*Cycle.obj_parameter.VV[14]

				[_, QDSC,QTPC,QSCC,QTOTC, Cycle.obj_parameter.FSUPC, Cycle.obj_parameter.FSUBC ] = self.objCondType.cond_balance(CPRLIQ)
				
				
				#[ Cycle.obj_parameter.TS2,Cycle.obj_parameter.TC, Cycle.obj_parameter.JC, ICONC] = self.cond (Cycle.obj_parameter.T,Cycle.obj_parameter.H,   Cycle.obj_parameter.TS1,Cycle.obj_parameter.TC,QDSC,   QTPC,QSCC,Cycle.obj_parameter.JC, ICONC )
				[ Cycle.obj_parameter.TS2,Cycle.obj_parameter.TC, Cycle.obj_parameter.JC, ICONC] \
					= self.objCondType.cond (Cycle.obj_parameter.T,Cycle.obj_parameter.H,	\
						Cycle.obj_parameter.TS1,Cycle.obj_parameter.TC,QDSC,   QTPC,QSCC,Cycle.obj_parameter.JC )

				#
				#	ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
				#
				Cycle.obj_parameter.H[16] = Cycle.obj_parameter.H[4] - Data.obj_cdata.CONDHT[Cycle.obj_parameter.NCYC]/Data.obj_cdata.MREF/DUTYR
				Cycle.obj_parameter.P[16] = Cycle.obj_parameter.P[4]
				[Cycle.obj_parameter.T[16], Cycle.obj_parameter.XQ[16],Cycle.obj_parameter.XL_Temp ,Cycle.obj_parameter.XV_Temp,  Cycle.obj_parameter.VL[16],Cycle.obj_parameter.VV[16],HL16,HV16] = self.hpin ( Cycle.obj_parameter.H[16],Cycle.obj_parameter.P[16],Cycle.obj_parameter.X )

				self.setArr2dCol (Cycle.obj_parameter.XL, 16, Cycle.obj_parameter.XL_Temp )
				self.setArr2dCol (Cycle.obj_parameter.XV, 16, Cycle.obj_parameter.XV_Temp )
				
				if (Cycle.obj_parameter.VL[16]  ==  0.0): Cycle.obj_parameter.VL[16] = Cycle.obj_parameter.V[4]
				Cycle.obj_parameter.V[16] = Cycle.obj_parameter.VL[16]

				#
				#	CALCULATE THE AVERAGE EFFECTIVENESS OF THE HEAT EXCHANGER
				#	CALCULATE THE HEAT TRANSFER if THE REFRIGERANT LEFT AT Cycle.obj_parameter.TS1
				#
				#	DETERMINE THE SPECIFIC VOLUME OF THE LIQUID
				#
				if (Cycle.obj_parameter.TS1 <  Cycle.obj_parameter.T[4]) :
					VGUESS = Cycle.obj_parameter.V[4]
					# [P4, P5] = self.espar (P1, P2, P3)
					[AS1, BS1] = self.espar (0, Cycle.obj_parameter.TS1, Cycle.obj_parameter.X) #  CALL ESPAR(0,Cycle.obj_parameter.TS1,Cycle.obj_parameter.X,AS1,BS1)

					#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
					[VGUESS, LCONV] = self.vit (Cycle.obj_parameter.TS1,Cycle.obj_parameter.P[4],AS1,BS1,VGUESS,True) # CALL VIT(Cycle.obj_parameter.TS1,Cycle.obj_parameter.P[4],AS1,BS1,VGUESS,True,LCONV)
					VS1 = VGUESS
				else:
					VS1 = Cycle.obj_parameter.V[4]

				[HS1, CV, CP, VS] = self.hcvcps (1, Cycle.obj_parameter.TS1, VS1, Cycle.obj_parameter.X) # 	CALL HCVCPS(1,Cycle.obj_parameter.TS1,VS1,Cycle.obj_parameter.X,  HS1,CV,CP,VS)

				QRMAX = Data.obj_cdata.MREF*(Cycle.obj_parameter.H[14]-HS1)
				#
				#	CALCULATE THE HEAT TRANSFER if THE AIR LEFT AT T[14]
				#
				QAMAX = Data.obj_cdata.CFMC*(Cycle.obj_parameter.T[14]-Cycle.obj_parameter.TS1)
				QMAXC = QAMAX
				if (QRMAX <  QAMAX): QMAXC = QRMAX

				Data.obj_cdata.ETAC = QTOTC/QMAXC
			else:
				Cycle.obj_parameter.LCCON = False

			Data.obj_cdata.IC = Data.obj_cdata.IC + 1
			
			if Data.obj_cdata.INCTRL in [0,3]:
				if (Data.obj_cdata.IC  <=  4) :
					Cycle.obj_parameter.LCCON = True
					ICONC = 0

			if Data.obj_cdata.INCTRL in [1,2,4]:
				if (Data.obj_cdata.IC  <=  10) :
					Cycle.obj_parameter.LCCON = True
					ICONC = 0
					
			#if (not Cycle.obj_parameter.LECON): Cycle.obj_parameter.LCCON = False #only in Puthon
			#if (LQUIT): Cycle.obj_parameter.LCCON = False #if key pressed in lowevp ,ethod then exit, cancel in Python
			
			
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

		Cycle.obj_parameter.T[4] = Cycle.obj_parameter.TC[Cycle.obj_parameter.JC]
		Cycle.obj_parameter.TS[1] = 0.0
		Cycle.obj_parameter.TS[4] = Cycle.obj_parameter.TS1
		Cycle.obj_parameter.TS[5] = Cycle.obj_parameter.TS4
		Cycle.obj_parameter.TS[6] =0.0
		Cycle.obj_parameter.TS[7] = Cycle.obj_parameter.TS3
		Cycle.obj_parameter.TS[8] = Cycle.obj_parameter.TS6
		Cycle.obj_parameter.TS[9] = Cycle.obj_parameter.TS5
		Cycle.obj_parameter.TS[10] =0.0
		Cycle.obj_parameter.TS[14] =Cycle.obj_parameter.TS2
		Cycle.obj_parameter.TS[16] = 0.0
		TGC = Cycle.obj_parameter.T[3] - Cycle.obj_parameter.T[4]
		#
		#	SPECIFIC VOLUMES
		#
		Cycle.obj_parameter.V[5] = min(1.0, 1.0 - Cycle.obj_parameter.XQ[5] ) * Cycle.obj_parameter.VL[5] + max(0.0, Cycle.obj_parameter.XQ[5]  ) * Cycle.obj_parameter.VV[5]
		Cycle.obj_parameter.V[8] = min(1.0, 1.0 - Cycle.obj_parameter.XQ[8] ) * Cycle.obj_parameter.VL[8] + max(0.0, Cycle.obj_parameter.XQ[8]  ) * Cycle.obj_parameter.VV[8]
		Cycle.obj_parameter.V[9] = min(1.0, 1.0 - Cycle.obj_parameter.XQ[9] ) * Cycle.obj_parameter.VL[9] + max(0.0, Cycle.obj_parameter.XQ[9]  ) * Cycle.obj_parameter.VV[9]
		Cycle.obj_parameter.V[10]= min(1.0, 1.0 - Cycle.obj_parameter.XQ[10]) * Cycle.obj_parameter.VL[10]+ max(0.0, Cycle.obj_parameter.XQ[10] ) * Cycle.obj_parameter.VV[10]
		#
		#	QUALITIES
		#
		Cycle.obj_parameter.XQ[1] = 1.0
		Cycle.obj_parameter.XQ[3] = 1.0
		Cycle.obj_parameter.XQ[4] = 0.0
		Cycle.obj_parameter.XQ[6] = 0.0
		Cycle.obj_parameter.XQ[10] = 0.0
		#
		#	LIQUID AND VAPOR COMPOSITIONS AROUND THE CYCLE LOOP
		#
		#I = 1
		for I in range (1, Cycle.obj_parameter.NC + 1): #	DO WHILE (I  <=  NC)
			Cycle.obj_parameter.XL[I][1] = 0.0
			Cycle.obj_parameter.XV[I][1] = Cycle.obj_parameter.X[I]
			
			Cycle.obj_parameter.XL[I][2] = 0.0
			Cycle.obj_parameter.XV[I][2] = Cycle.obj_parameter.X[I]
			
			Cycle.obj_parameter.XL[I][3] = 0.0
			Cycle.obj_parameter.XV[I][3] = Cycle.obj_parameter.X[I]
			
			Cycle.obj_parameter.XL[I][4] = Cycle.obj_parameter.X[I]
			Cycle.obj_parameter.XV[I][4] = 0.0
			
			Cycle.obj_parameter.XL[I][6] = Cycle.obj_parameter.X[I]
			Cycle.obj_parameter.XV[I][6] = 0.0
			
			Cycle.obj_parameter.XL[I][7] = 0.0
			Cycle.obj_parameter.XV[I][7] = Cycle.obj_parameter.X[I]
			
			Cycle.obj_parameter.XL[I][10] = Cycle.obj_parameter.X[I]
			Cycle.obj_parameter.XV[I][10] = 0.0
			
			Cycle.obj_parameter.XL[I][11] = Cycle.obj_parameter.X[I]
			Cycle.obj_parameter.XV[I][11] = 0.0
			
			Cycle.obj_parameter.XL[I][12] = 0.0
			Cycle.obj_parameter.XV[I][12] = Cycle.obj_parameter.X[I]
			
			Cycle.obj_parameter.XL[I][13] = 0.0
			Cycle.obj_parameter.XV[I][13] = Cycle.obj_parameter.X[I]
			
			Cycle.obj_parameter.XL[I][14] = 0.0
			Cycle.obj_parameter.XV[I][14] = Cycle.obj_parameter.X[I]
			
			Cycle.obj_parameter.XL[I][16] = Cycle.obj_parameter.X[I]
			Cycle.obj_parameter.XV[I][16] = 0.0

		#
		#	ENTROPIES AROUND THE CYCLE
		#
		#J = 3
		for J in range(3, 16+1): #DO WHILE (J  <=  16)
			if (J  !=  5): Cycle.obj_parameter.S[J] = self.entrop( Cycle.obj_parameter.T[J], Cycle.obj_parameter.V[J] ,Cycle.obj_parameter.X)
			#J = J + 1
		#END DO
		
		SL5 = self.entrop(Cycle.obj_parameter.T[5],Cycle.obj_parameter.VL[5], self.getArr2dCol (Cycle.obj_parameter.XL,5) ) # XL[1][5]
		SV5 = self.entrop(Cycle.obj_parameter.T[5],Cycle.obj_parameter.VV[5], self.getArr2dCol (Cycle.obj_parameter.XV,5) ) # XV[1][5]
		Cycle.obj_parameter.S[5] = min(1.0,1.0-Cycle.obj_parameter.XQ[5]) * SL5 + max(0.0,Cycle.obj_parameter.XQ[5]) * SV5
		
		SL8 = self.entrop(Cycle.obj_parameter.T[8],Cycle.obj_parameter.VL[8],  self.getArr2dCol (Cycle.obj_parameter.XL,8) ) # XL[1][8]
		SV8 = self.entrop(Cycle.obj_parameter.T[8],Cycle.obj_parameter.VV[8],  self.getArr2dCol (Cycle.obj_parameter.XV,8) ) # XV[1][8]
		Cycle.obj_parameter.S[8] = min(1.0,1.0-Cycle.obj_parameter.XQ[8]) * SL8 + max(0.0,Cycle.obj_parameter.XQ[8]) * SV8
		
		SL9 = self.entrop(Cycle.obj_parameter.T[9],Cycle.obj_parameter.VL[9], self.getArr2dCol (Cycle.obj_parameter.XL,9) ) # XL[1][9]
		SV9 = self.entrop(Cycle.obj_parameter.T[9],Cycle.obj_parameter.VV[9], self.getArr2dCol (Cycle.obj_parameter.XV,9) ) # XV[1][9]
		Cycle.obj_parameter.S[9] = min(1.0,1.0-Cycle.obj_parameter.XQ[9]) * SL9 + max(0.0,Cycle.obj_parameter.XQ[9]) * SV9
		#
		#	CONVERT FROM MOLAR QUANTITIES TO MASS BASIS
		#
		#J = 1
		
		for J in range (1, 16 + 1): #DO WHILE (J  <=  16)
			Cycle.obj_parameter.WMAVGL[J] = 0.0
			Cycle.obj_parameter.WMAVGV[J] = 0.0

			for I in range (1, Cycle.obj_parameter.NC + 1): #DO WHILE (I  <=  NC)
				Cycle.obj_parameter.WMAVGL[J] = Cycle.obj_parameter.WMAVGL[J] + (Cycle.obj_parameter.XL[I][J] ) * (Data.WM[I])
				Cycle.obj_parameter.WMAVGV[J] = Cycle.obj_parameter.WMAVGV[J] + (Cycle.obj_parameter.XV[I][J] ) * (Data.WM[I])
			
			for I in range (1, Cycle.obj_parameter.NC + 1): #DO WHILE (I  <=  NC)
				if (Cycle.obj_parameter.XL[I][J] >  0.0): Cycle.obj_parameter.XL[I][J] = Cycle.obj_parameter.XL[I][J] * Data.WM[I]/Cycle.obj_parameter.WMAVGL[J]
				if (Cycle.obj_parameter.XV[I][J] >  0.0): Cycle.obj_parameter.XV[I][J] = Cycle.obj_parameter.XV[I][J] * Data.WM[I]/Cycle.obj_parameter.WMAVGV[J]
			
			Cycle.obj_parameter.V[J] = Cycle.obj_parameter.V[J]/WMAVG
			Cycle.obj_parameter.H[J] = Cycle.obj_parameter.H[J]/WMAVG
			Cycle.obj_parameter.S[J] = Cycle.obj_parameter.S[J]/WMAVG

		
		#
		#	COMPUTE WORK, CAPACITY, COP, ETC.
		#
		HOUT = HOUT/WMAVG
		VSUC = VSUC/WMAVG
		Data.obj_cdata.W = (HOUT - Cycle.obj_parameter.H[1])/(1.0 - Cycle.obj_parameter.QCAN)
		Data.obj_cdata.QE = Cycle.obj_parameter.H[7] - Cycle.obj_parameter.H[5]
		Cycle.obj_parameter.QC = Cycle.obj_parameter.H[14] - Cycle.obj_parameter.H[4]
		
		Data.obj_cdata.QZ = Cycle.obj_parameter.H[9] - Cycle.obj_parameter.H[8]
		Data.obj_cdata.COPR = (Data.obj_cdata.QE + Data.obj_cdata.QZ)/Data.obj_cdata.W
		
		TH = Cycle.obj_parameter.TS1
		TL1 = Cycle.obj_parameter.TS3
		TL2 = Cycle.obj_parameter.TS5
		DENOM = TH * (Data.obj_cdata.QE * (1./TL1-1./TH)+ Data.obj_cdata.QZ * (1./TL2-1./TH))
		COPI = (Data.obj_cdata.QE+ Data.obj_cdata.QZ)/DENOM
		Cycle.obj_parameter.PR = Cycle.obj_parameter.P[2]/Cycle.obj_parameter.P[1]
		Cycle.obj_parameter.TSUPC = Cycle.obj_parameter.T[2] - Cycle.obj_parameter.T[3]
		
		#
		#	CORRECT COP DUR TO CYCLING LOSSES
		#
		if (Data.obj_cdata.I_CYCLE  ==  0) :
			Data.obj_cdata.CORR_COP = 1.0
		else:
			TENV = (Data.obj_cdata.TROOM + 459.6)/1.8
			TMID_COND = (Cycle.obj_parameter.T[3] + Cycle.obj_parameter.T[11])/2.0
			TMID_EVAP = (Cycle.obj_parameter.T[8] + Cycle.obj_parameter.T[9])/2.0
			Data.obj_cdata.CORR_COP = self.cyclos (Data.obj_cdata.I_VALVE, Data.obj_cdata.T_CYCLE)
		
		print ("aym inside cycle ===============", Cycle.obj_parameter.T)
		dir (Cycle.obj_parameter)
		return Cycle.obj_parameter

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
		#	QFF,QFZ,Cycle.obj_parameter.TS3,TS5,   T,IDFRST, DUTYR)
		# output    QFF,QFZ,DUTYR
		#	*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
		#	* 	CALCULATE DUTY CYCLE AND THE AVERAGE CABINET LOADS			*
		#	*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *

		if (ICAB  ==  0): return
		#
		#	CALCULATE IN-WALL HEAT LOADS
		#
		TENV = (Data.obj_cdata.TROOM + 459.6)/1.8
		TCND = 0.2 * T[14] + 0.4 * T[3] + 0.4 * T[11]

		if (TS5 >  -300.0) :									#Freezer evaporator
			TRFZ = (T[8] + T[9])/2.0
			Data.obj_cdata.Q_FZ_IN_WALL = 1.8 * Data.obj_cdata.UA_FZ * (TENV - TS5)
			Data.obj_cdata.Q_ML_IN_WALL = 1.8 * Data.obj_cdata.UA_ML * (Cycle.obj_parameter.TS3  - TS5)

			Data.obj_cdata.CAPZ_IN_WALL = 1.8 * Data.obj_cdata.UA_FZ * (TENV - TRFZ)
			Data.obj_cdata.CAPM_IN_WALL = 1.8 * Data.obj_cdata.UA_ML * (Cycle.obj_parameter.TS3  - TRFZ)

			Data.obj_cdata.Q_FZ_FF = 1.8 * Data.obj_cdata.UA_ML * (TS5 - TRFZ)
		else:
			Data.obj_cdata.Q_FZ_IN_WALL = 0
			Data.obj_cdata.Q_ML_IN_WALL = 0

			Data.obj_cdata.CAPZ_IN_WALL = 0
			Data.obj_cdata.CAPM_IN_WALL = 0

			Data.obj_cdata.Q_FZ_FF = 0
		# End if

		TRFF = (T[5] + T[7])/2.0
		Data.obj_cdata.Q_FF_IN_WALL = 1.8 * Data.obj_cdata.UA_FF * (TENV - Cycle.obj_parameter.TS3)
		Data.obj_cdata.CAPE_IN_WALL = 1.8 * Data.obj_cdata.UA_FF * (TENV - TRFF)
		Data.obj_cdata.CONDF_IN_WALL = 1.8 * Data.obj_cdata.UA_FF_CND * (TCND - TENV)
		Data.obj_cdata.CONDZ_IN_WALL = 1.8 * Data.obj_cdata.UA_FZ_CND * (TCND - TENV)
		#
		#	BRANCH ACCORDING TO THE TYPE OF REFRIGERATOR
		#
		QFF = Data.obj_cdata.FFQ
		QFZ = Data.obj_cdata.FZQOFF
		#

		if IRFTYP in [1, 3]:
			if (ICYCL  ==  1) :
				if (Cycle.obj_parameter.IDFRST  ==  0) :
					QFF = QFF + Cycle.obj_parameter.FROSTF
					QFZ = QFZ + Cycle.obj_parameter.FROSTF
				# End if

				Data.obj_cdata.CAPE = QFRSH/1.0548 - 3.413 * Data.obj_cdata.FANE - 3.413 * Data.obj_cdata.DFSTCYC	\
					- 3.413 * Data.obj_cdata.FFCYC	- 3.413 * Data.obj_cdata.FZCYC	\
					- Data.obj_cdata.CONDF_IN_WALL - Data.obj_cdata.CONDZ_IN_WALL

				Data.obj_cdata.DUTYC = (QFF + QFZ)/Data.obj_cdata.CAPE
				if (Data.obj_cdata.DUTYC >  1.0): Data.obj_cdata.DUTYC = 1.0
				DUTYR = Data.obj_cdata.DUTYC
			# End if

			if (ICYCL  ==  2) :
				QFF = QFF - Cycle.obj_parameter.FROSTF
				if (Cycle.obj_parameter.IDFRST  ==  0): QFZ = QFZ + Cycle.obj_parameter.FROSTF

				Data.obj_cdata.CAPZ = QFREZ/1.0548 - 3.413 * Data.obj_cdata.FANZ - 3.413 * Data.obj_cdata.DFSTCYC	\
					+ Data.obj_cdata.Q_FZ_IN_WALL + Data.obj_cdata.Q_ML_IN_WALL	\
					- Data.obj_cdata.CAPZ_IN_WALL - Data.obj_cdata.CAPM_IN_WALL	\
					- Data.obj_cdata.CONDZ_IN_WALL - 3.413 * Data.obj_cdata.FZCYC	\
					- Data.obj_cdata.Q_HXS_FZ/1.0548

				if (Data.obj_cdata.CAPZ  <=  0.0) :
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

				Data.obj_cdata.DUTYZ = QFZ/Data.obj_cdata.CAPZ

				Data.obj_cdata.CAPE = QFRSH/1.0548 - 3.413 * Data.obj_cdata.FANE - 3.413 * Data.obj_cdata.FFCYC	\
					+ Data.obj_cdata.Q_FF_IN_WALL - Data.obj_cdata.CAPE_IN_WALL	\
					- Data.obj_cdata.CONDF_IN_WALL + Data.obj_cdata.Q_FZ_FF	\
					- Data.obj_cdata.Q_HXS_FF/1.0548

				Data.obj_cdata.DUTYE = QFF/Data.obj_cdata.CAPE

				Data.obj_cdata.DUTYC = min(Data.obj_cdata.DUTYE,Data.obj_cdata.DUTYZ)

				if (Data.obj_cdata.DUTYC >  1.0): Data.obj_cdata.DUTYC = 1.0
				DUTYR = max(Data.obj_cdata.DUTYE, Data.obj_cdata.DUTYZ)

				if (DUTYR >  1.0): DUTYR = 1.0
			# End if

			if (ICYCL  ==  3) :
				if (N  ==  1) :
					if (Cycle.obj_parameter.IDFRST  ==  0): QFZ = QFZ + Cycle.obj_parameter.FROSTF

					Data.obj_cdata.CAPZ = QFRSH/1.0548 - 3.413 * Data.obj_cdata.FANE	- 3.413 * (Data.obj_cdata.DFSTCYC + Data.obj_cdata.FZCYC)
					Data.obj_cdata.DUTYZ = QFZ/Data.obj_cdata.CAPZ

					Data.obj_cdata.DUTYC = min(Data.obj_cdata.DUTYZ,1.0)
					Data.obj_cdata.DUTYZ = Data.obj_cdata.DUTYC

				else:
					Data.obj_cdata.CAPE = QFRSH/1.0548 - 3.413 * (Data.obj_cdata.FANE + Data.obj_cdata.FFCYC) + Data.obj_cdata.Q_FF_IN_WALL - Data.obj_cdata.CAPE_IN_WALL
					QFF = QFF - Cycle.obj_parameter.FROSTF
					Data.obj_cdata.DUTYE = QFF/Data.obj_cdata.CAPE
					Data.obj_cdata.DUTYC = min(Data.obj_cdata.DUTYE,1.0)
					Data.obj_cdata.DUTYE = Data.obj_cdata.DUTYC
				# End if

				DUTYR = Data.obj_cdata.DUTYC
			# End if
			else :
				#CASE DEFAULT					!One door type units
				if (Cycle.obj_parameter.IDFRST  ==  0): QFZ = QFZ + Cycle.obj_parameter.FROSTF
				Data.obj_cdata.CAPE = QFRSH/1.0548 - 3.413 * (Data.obj_cdata.FANE + Data.obj_cdata.DFSTCYC + Data.obj_cdata.FZCYC)	\
					+ Data.obj_cdata.Q_FF_IN_WALL - Data.obj_cdata.CAPE_IN_WALL						\
					- Data.obj_cdata.CONDF_IN_WALL - Data.obj_cdata.Q_HXS_FF/1.0548

				Data.obj_cdata.DUTYE = QFZ/Data.obj_cdata.CAPE
				Data.obj_cdata.DUTYC = min(Data.obj_cdata.DUTYE,1.0)
				DUTYR = Data.obj_cdata.DUTYC

		#END SELECT
		return [QFF,QFZ,DUTYR]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def shwfig(self, ICYCL):
		if ICYCL == 1:
			self.showMsg ("Show Single cycle PIC")
			if Data.obj_cdata.IEVAP == 0:
				self.showMsg ("STANDARD SINGLE EVAPORATOR CYCLE")
				
			if Data.obj_cdata.IEVAP == 1:
				self.showMsg ("FRESH FOOD SECTION")

			if Data.obj_cdata.IEVAP == 2:
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
			