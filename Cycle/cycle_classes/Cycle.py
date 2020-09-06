# Python import
import math, sys, datetime

# User import
#from Cnat import Cnat
from Data import Data

from Condenser import Condenser
from FFEvap import FFEvap
from Compressor import Compressor

from Adjlod import Adjlod
from Evap2 import  Evap2
from Block2 import Block2

from FileAccess import FileAccess

class Cycle (Adjlod, Condenser, FFEvap, Compressor, Evap2):
	FILE_ERROR_OUT = 'ERROR.OUT'
	FILE_CYCLE_OUT = "CYCLE.OUT"

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	# in Python only, to fix Fortant issue
	def setArr2dCol(self, arry_2dim, int_col, arr_1dim ):
		for ncnt in range (len(arry_2dim)):
			arry_2dim [ncnt][int_col] = arr_1dim [ncnt]
		return

	def getArr2dCol(self, arry_2dim, int_col):
		arr_1dim = []
		for ncnt in range (len(arry_2dim)):
			arr_1dim.append ( arry_2dim [ncnt][int_col])
		return arr_1dim

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def getArr3dLevel(self, arry_3dim, int_level):
		x_direc = len(arry_3dim)
		y_direc = len(arry_3dim[0])
		arr_2dim = [[0.0] * (y_direc +1) for i in range(x_direc)]
		for x in range (x_direc):
			for y in range (y_direc):
				arr_2dim [x][y] =( arry_3dim [x][y][int_level])
		return arr_2dim

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def cycle (self, NC,IR,XM,F, TS1,TS3,TS5, MEFF,QHILO,QCAN,	\
				DPC,DPE,DPF,ETHX1,ETHX2,DISPLC,NCYC,FROSTF,FROSTZ,ICAB,	\
				IRFTYPE,ICYCL,ICYCLS,IDFRST):
		print ("aym NC,",NC)
		print ("aym IR,",IR)
		print ("aym TS1,",TS1, TS1-273.11)
		print ("aym TS3,",TS3, TS3-273.11)
		print ("aym TS5,",TS5)
		print ("aym MEFF,",MEFF)
		print ("aym IRFTYPE,",IRFTYPE)
		print ("aym ICYCL,",ICYCL)
		print ("aym ICYCLS,",ICYCLS)
		print ("aym NCYC,",NCYC)
		print ("aym ICAB,",ICAB)
		print ("aym DPC,",DPC)
		print ("aym DPE,",DPE)
		print ("aym DPF,",DPF)
		print ("aym DISPLC,",DISPLC)
		
				
		#	SUBROUTINE CYCLE (NC,IR,XM,F,TS1,TS3,TS5,MEFF,QHILO,QCAN,
		#	.	DPC,DPE,DPF,ETHX1,ETHX2,DISPLC,NCYC,   FROSTF,FROSTZ,    ICAB,
		#	.	IRFTYPE,  ICYCL,ICYCLS,IDFRST)
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
		#			DPC - PRESSURE DROP THROUGH CONDENSER
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
		#			I REFERS TO COMPONENT
		#			J REFERS TO STATE
		#
		#	LOGICAL LCRIT,LCCON,LECON,LCONV,LQUIT
		#	LOGICAL AIRTMP
		#	CHARACTER*2 IRUN(100),CHOUR,CMIN,CSEC,CDAY,CMONN,CYEAR
		#	CHARACTER*3 CMONTH[12],CMON
		#	CHARACTER*6 HREF(34),REFH(34)
		#	CHARACTER*8 HSTATE[15], MSTATE[12]
		#	REAL MEFF,MREF,MREFSV
		#
		#	DIMENSION T[16],S[16],H[16],XQ[16],P[16],V[16],XL(5,16),XV(5,16),
		#	.	TS[16],VL[16],VV[16],F(5,5),XM(5),X(5),IR(5),XREF[5]
		#	DIMENSION TC[3],TE[3],TCRIT(5),WM[5]
		#	DIMENSION WMAVGL[16],WMAVGV[16],LPNT[15]
		#	DIMENSION AIRTMP[15]
		#
		#	ok - COMMON /RDATA2/ WM,TCRIT
		#	ok - COMMON /HREF1/ HREF,REFH
		#	ok - COMMON /PARMS/ ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
		#	ok - COMMON /PARMS2/ TSPEC,I_LIQUID_LINE
		#	ok - COMMON /HTEXS/ CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
		#	ok - COMMON /SPECS/ DTSUPE,DTSUBC
		#	ok - COMMON /SPECS2/ ISPEC,XEXITE,DTSUPI
		#	ok - COMMON /EVAPS/ ITYPE, FRACT_FF, FRACT_FZ
		#	ok - COMMON /CABLOD/ FFASH,FAUXF,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
		#	ok - .					FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
		#	ok - .					FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
		#	ok - COMMON /FANS/ FANE,FANZ,FANC,DUTYC,W,COPR
		#	ok - COMMON / DIAG / IM_Err,IC,IE
		#	COMMON / TIME / CHOUR,CMIN,CSEC,CDAY,CMON,CMONN,IYEAR,CYEAR
		#	ok - COMMON / RESULT / QE, QZ, FLOW, QEN[2], FLOWN[2], COPRN[2]
		#	ok - COMMON / CYCLNG / CORR_COP, COPCYC[2], I_CYCLE, I_VALVE, T_CYCLE
		#	ok - COMMON / TLRNCE / TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
		#	ok -  .						N_EVAP, N_COND

		#	ok - COMMON / CHINA / INCTRL
		#	ok - COMMON / MAPDAT / IMAP, ICOMP, ICOOL, EER, SIZE, DISPL, EFFC,
		#	ok -  .						SPEEDN, IREAD
		#	ok - COMMON / CONDEN / UDSC,UTPC,USCC,ATOTC,UACOND
		#	ok - COMMON / LIQLIN / FFREFQ, FZREFQ, CONDHT[2], CONDVP[2]
		#	ok - COMMON / CYCLIC / DFSTCYC, FFCYC, FZCYC, OUTCYC
		#
		FSUBC = 0.0 # in Python only, it has no value in Fortant

		TS = [0.0] *(16+1)
		WMAVGL = [0.0] *(16+1)
		WMAVGV = [0.0] *(16+1)
		AIRTMP = [0.0] *(16+1)
		#LPNT   = [0.0] *(15+1) not required

		ITMAXC = 100
		ITMAXE = 40

		# in python add extra item for item 0
		HSTATE = ['','COMP IN','COMP DIS','COND IN','COND DEW',	\
					'COND BUB','COND OUT','LIQ LINE',	\
					'SUBCOOL1','SUBCOOL2','FREZ IN ','FREZ OUT','FRSH IN ',	\
					'FRSH DEW','FRSH OUT','HX1 OUT ']

		# in python add extra item for item 0
		MSTATE = ['','COMP IN','COMP DIS','COND IN','COND DEW',	\
					'COND BUB','COND OUT','LIQ LINE',			\
					'SUBCOOL ','EVAP IN ', 'EVAP DEW','EVAP OUT','HX OUT  ']

		FSUPE = 0 # in python only
		XREF= [0.0] *(5+1)

		XV = [[-9999999.9] * (16+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		XL = [[-9999999.9] * (16+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
			
		XL_Temp = [0.0] * len(XL) # in python only
		XV_Temp = [0.0] * len(XV) # in python only
		XV15 = [0.0] * (5+1) # in Python only

		T = [-9999999.9] * (16+1)
		S = [-9999999.9] * (16+1)
		H = [-9999999.9] * (16+1)
		XQ= [-9999999.9] * (16+1)

		P = [-9999999.9] * (16+1)
		V = [-9999999.9] * (16+1)
		X= [0.0] * (5+1)
		VL= [-9999999.9] * (16+1)
		VV= [-9999999.9] * (16+1)

		TE= [0.0] * (3+1)
		TC= [0.0] * (3+1)

		FSUPC = 0.1

		# in python add extra item for item 0
		LPNT  = [0,1,2,14,3,11,4,16,6,10,8,9,5,12,7,13]

		#	DATA CMONTH / 'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
			#				'SEP','OCT','NOV','DEC' /
		#	DATA IRUN/'00','01','02','03','04','05','06','07','08','09',
			#			'10','11','12','13','14','15','16','17','18','19',
			#			'20','21','22','23','24','25','26','27','28','29',
			#			'30','31','32','33','34','35','36','37','38','39',
			#			'40','41','42','43','44','45','46','47','48','49',
			#			'50','51','52','53','54','55','56','57','58','59',
			#			'60','61','62','63','64','65','66','67','68','69',
			#			'70','71','72','73','74','75','76','77','78','79',
			#			'80','81','82','83','84','85','86','87','88','89',
			#			'90','91','92','93','94','95','96','97','98','99'/

		AIRTMP = [True,  #add new item to start list at 1	\
			False, False, True,  False, False,	\
			True,  False, False, False, False,	\
			False, True,  False, True,  False]

		#DATA IO_Cycle/8/, IM_Err/9/
		TEMIN = 210.0

		#CALL GETCOL('OLD_MODEL=YES$','SETUP.DAT ',IRET) read config.sys file and pass back color indicator

		#if (IRET  ==  1) :IRET  = return error code (0=no error)
		#	IRET = 7
		#else:

		IRET = 6
		# End if

		#	!! Don't allow adjustment to freezer evap if not one-door refrigerator
		if (IRFTYPE  !=  6): IRET = 7

		#			GET DATE AND TIME
		#
		IRFTYP = IRFTYPE
		if (IRFTYPE  ==  7): IRFTYP = 1

		#if (NCYC  !=  2) :
		#	CALL GETDAT(IYEAR,IMONTH,IDAY)
		#	CALL GETTIM(IHOUR,IMIN,ISEC)
		#	IHOUR = IHOUR + 1
		#	IMIN  = IMIN + 1
		#	ISEC  = ISEC + 1
		#	IDAY  = IDAY + 1
		#	CHOUR = IRUN(IHOUR)
		#	CMIN  = IRUN(IMIN)
		#	CSEC  = IRUN(ISEC)
		#	CDAY  = IRUN(IDAY)
		#	CMON  = CMONTH(IMONTH)
		#	CMONN = IRUN(IMONTH+1)

		#	IPNT = IYEAR - 1900
		#	if (IPNT  >=  100): IPNT = IPNT - 100
		#	CYEAR = IRUN(IPNT + 1)
		# End if
		#
		#			SET UP LOGICAL VECTOR ON AIR TEMPERATURES
		#
		if (ICYCL  ==  2) :
			AIRTMP[10] = True
			AIRTMP[11] = True
		else:
			AIRTMP[10] = False
			AIRTMP[11] = False
		# End if
		#
		#			OPEN OUTPUT FILE
		#
		#objCycOut = FileAccess (Cycle.FILE_CYCLE_OUT,  "append")  # IO_Cycle Tag
		objCycOut = FileAccess (Cycle.FILE_CYCLE_OUT,  "write")  # IO_Cycle Tag
		#objError = FileAccess (Cycle.FILE_ERROR_OUT, "append" ) # IM tag
		objError = FileAccess (Cycle.FILE_ERROR_OUT, "write" ) # IM tag

		# End if

		objError.write_or_terminate ('ENTERING SUBROUTINE CYCLE')
		objCycOut.write_or_terminate (" ") #WRITE(IO_Cycle,'(A1)') CHAR[12]

		now = datetime.datetime.now( )

		if (NCYC  !=  2) :
			objCycOut.write_or_terminate( ( now.strftime( "%H %M %S %d %b %Y" ) ) + " - Python Output aymhenry@gmail")
		#
		#			OUTPUT INFORMATION ON TYPE OF CYCLE
		#
		if (ICYCL  ==  1) :
			if (ICYCLS  ==  1) :
				objCycOut.write_or_terminate('STANDARD ONE EVAPORATOR CYCLE')
			else:
				if (Data.ITYPE  ==  1  or  Data.ITYPE  ==  4) :
					objCycOut.write_or_terminate('DUAL EVAP CYCLE: FRESH FOOD LOOP')
				else:
					objCycOut.write_or_terminate('DUAL EVAP CYCLE: FREEZER LOOP')

		if (ICYCL  ==  2) :
			if (ICYCLS  ==  2) :
				objCycOut.write_or_terminate('LORENZ CYCLE')
			else:
				objCycOut.write_or_terminate('DUAL EVAP CYCLE')

		if (ICYCL  ==  3) :
			if (NCYC  ==  1) :
				objCycOut.write_or_terminate('DUAL LOOP CYCLE - FREEZER')
			else:
				objCycOut.write_or_terminate(" ")
				objCycOut.write_or_terminate('DUAL LOOP CYCLE - FRESH FOOD')

		objCycOut.write_or_terminate(" ")

		if (Data.ITYPE  ==  3): Data.ITYPE = 1

		#
		#			OUTPUT REFRIGERATION MIXTURE INFORMATION
		#

		X2 = 100.0 * XM[1]
		objCycOut.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(X2, Data.HREF[ IR[1] ])  )

		if (NC >  1) :
			for I in range (2, NC+1) : #DO WHILE (I  <=  NC)
				X2 = 100.0 * XM[I]
				# objCycOut.write_or_terminate("  , %%7.3f" %( Data.HREF[ IR[I] ])  )
				objCycOut.write_or_terminate('THE REFRIGERANT MIXTURE CONSISTS OF  %4.0f OF %s'    %(X2, Data.HREF[ IR[1] ])  ) # fixed in python
				

		objCycOut.write_or_terminate(" ")
		objCycOut.write_or_terminate("OUTPUT RESULTS")
		#
		#			INITIALIZE COMPRESSOR MAP ANALYSIS
		#
		DUTYR = 0.5
		Data.IREAD = 0
		Data.DISPL = DISPLC
		if (Data.IMAP  ==  1) :
			#[ P6 P7] = self.cnat (P1 to P5, P8 )
			[Data.EFFC, Data.CE ] =self.map(Data.ICOMP, Data.ICOOL, Data.EER, Data.SIZE, Data.DISPL, Data.SPEEDN)

		#
		#			INITIALIZE THERMODYNAMIC DATA
		#
		self.bconst (NC,IR,F)

		#
		#			CONVERT TO MOLAR COMPOSITION FOR THE CALCULATION OF
		#			ALL PROPERTIES
		#
		WMSUM = 0.0
		WMAVG = 0.0

		for I in range (1, NC+1) : #DO WHILE (I  <=  NC)
			WMSUM = WMSUM+XM[I]/Data.WM[I]

		for I in range (1, NC+1) : #DO WHILE (I  <=  NC)
			X[I] = XM[I]/Data.WM[I] / WMSUM
			WMAVG = WMAVG+X[I] * Data.WM[I]

		#
		#			INITIAL GUESSES FOR TC AND TE
		#
		#			ASSUME TEMP RISE OF COND IS 0.5 F PER LBM
		#

		TC[1] = TS1 + Data.MREF/3.6

		#----------NEW CODE FOR EVAPORATOR WITH INCOMPLETE EVAPORATION----------
		#
		#	TE[1] = TS3 - 6.0  -  OLD REPLACED CODE
		#
		#			GUESS A DEW POINT TEMPERATURE AT THE EVAPORATOR EXIT
		#
		if (Data.ISPEC  ==  1) :		#Evap superheat
			#specified
			T[15] = TS3 - (Data.DTSUPE + 2.0)
			# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
			
			[ XL_Temp, X, P[15], VL[15], V[15], LCRIT]=self.bublt( T[15], XL_Temp, X,  False)
			#  CALL BUBLT(T(15),XL(1,15),X,P(15),VL(15),V(15),.FALSE.,LCRIT)
			self.setArr2dCol (XL, 15, XL_Temp)
			
			print ("aym 		before start iteratiojn  T[15],P[15]=",T[15], P[15])
			
			TE[1] = T[15] + Data.DTSUPE
			T[7] = TE[1]
			P[7] = P[15]
			
		if (Data.ISPEC  ==  2) : 		# Interchanger super-
									# heat specified
			T[15] = TS3 - 2.0
			T[13] = T[15] + Data.DTSUPI

			if (T[13] >  TC[1]) :
				T[13] = TC[1] - 5.0
				T[15] = T[13] - Data.DTSUPI

			# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
			[ XL_Temp,X,P[15],VL[15],V[15], LCRIT] = self.bublt(T[15], XL_Temp,X, False) #  CALL BUBLT(T(15),XL(1,15),X,P(15),VL(15),V(15),.FALSE.,LCRIT)
			self.setArr2dCol (XL, 15, XL_Temp)
			
			TE[1] = T[15]
			T[7] = TE[1]
			P[7] = P[15]

		if (Data.ISPEC  ==  3) :		# Evap exit quality
				# specified

			T[15] = TS3 - 2.0

			# [P2, P3 ,P4, P5, P6, P8] = self.bublt (P1, P2, P3 , P7 )
			[XL_Temp, X,P[15],VL[15],V[15], LCRIT] = self.bublt (T[15], XL_Temp, X , False ) # CALL BUBLT(T(15),XL(1,15),X,P(15),VL(15),V(15),.FALSE.,LCRIT)
			self.setArr2dCol (XL, 15, XL_Temp)
			
			# [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
			[X, XV15, TBUB15, VBUB15, VV15, LCRIT] = self.bublp ( P[15],X, XV15,   True)		#	CALL BUBLP(P(15),X,XV15,TBUB15,VBUB15,VV15,True,LCRIT)
			
			TE[1] = T[15] - ( T[15]- TBUB15 )*(1.0 - Data.XEXITE)
			T[7]  = TE[1]
			P[7]  = P[15]
		# End if

		#--------------------------END OF NEW CODE (12/29/90)-------------------
		JC = 1
		LCCON = True
		LQUIT = False
		#
		#			SET UP TEMPERATURES AND CABINET LOADS FOR INLET TEMPERATURE
		#			CALCULATION FOR EVAPORATOR OF A STANDARD DESIGN (TYPE 1)
		#
		TFF = Data.FFTEMP
		TFZ = Data.FZTEMP
		#
		#			INITIAL GUESS FOR REFRIGERANT MASS FLOW (ASSUME 10 LB/HR)
		#
		Data.FLOW = Data.MREF
		FLWREF = Data.FLOW
		FLOW2 = Data.FLOW
		Data.MREF = Data.MREF/(2.20462*WMAVG)
		MREFSV= Data.MREF
		#
		#			BEGIN ITERATION FOR CONDENSER OUTLET TEMPERATURE
		#

		self.shwfig(ICYCL)
		ICONC = 0
		#
		#			BEGIN MAIN ITERATION LOOP ON THE CONDENSER TEMPERATURE
		#
		Data.IC = 1
		ITMAXC = 3 # 3#100
	
		self.showError ("Python, check later, ICNT set in cond method, limit ITMAXC to ", ITMAXC )
		
		while (Data.IC  <=  ITMAXC  and  LCCON): #DO WHILE (IC  <=  ITMAXC  and  LCCON)
			
			if (ICAB  ==  1) :
				#[p3, p4] = self.adjlod (all)
				[TS3,TS5] = self.adjlod(ICYCL,Data.IC,  TS3,TS5,  FROSTF,FROSTZ,IDFRST)
			# End if
			T[4] = TC[JC]
			#
			#					FIND CONDENSER PRESSURE FOR CURRENT GUESS OF TC
			#					if BUBLT ROUTINE EXCEEDS THE CRITICAL POINT WRITE
			#					A WARNING MESSAGE AND return
			#
			#CALL GOTOXY(45,9)
			#CALL PRINT(Data.IC,2,-1)
			self.showMsg ("Iteration number for CONDENSER TEMPERATURE ", Data.IC)

			TBUB4 = TC[JC] + Data.DTSUBC

			# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
			#CALL BUBLT (TBUB4,X,XV(1,4),   P(4),VBUB4,VV(4),   .TRUE.,LCRIT)
			[X, XV_Temp, P[4], VBUB4, VV[4], LCRIT] = self.bublt (TBUB4,X , XV_Temp, True) 	
			self.setArr2dCol (XV, 4, XV_Temp )
			
			#
			#					DETERMINE THE SPECIFIC VOLUME OF THE LIQUID
			#
			if (Data.DTSUBC >  0.0) :
				VGUESS=VBUB4
				# [P4, P5] = self.espar (P1, P2, P3)
				[A4, B4] = self.espar (0, TC[JC], X) #	CALL ESPAR(0,TC[JC],X, A4,B4)

				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[VGUESS, LCONV] = self.vit (TC[JC],P[4],A4,B4, VGUESS,True) # CALL VIT(TC[JC],P[4],A4,B4,VGUESS,True,LCONV)
				V[4]=VGUESS
			else:
				V[4]=VBUB4

			#
			#	CONDENSER DEW POINT
			#

			P[3] = P[4] + (1.0-FSUPC)*DPC
			
			#TSHOW = TC[JC] - 273.11
			#CALL GOTOXY(12,2)
			#CALL PRINT(TSHOW,5,1)
			self.showMsg ("Conderser Temp TC (C) ", TC[JC] - 273.11 )

			if (LCRIT) :
				objCycOut.write_or_terminate ('CRITICAL TEMPERATURE EXCEEDED IN CONDENSER')
				#WRITE (IO_Cycle,2204)
				#CLOSE(IO_Cycle)
				#CLOSE(IM_Err)
				objCycOut = "" # close file
				return
				
				
			#
			#	ENTHALPY AT STATE 4 (CONDENSER OUTLET)
			#
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[H[4],CV,CP,VSND] = self.hcvcps (1,TC[JC],V[4],X)	#CALL HCVCPS (1,TC[JC],V[4],X, H[4],CV,CP,VSND)
			JE=1
			LECON=True
			#
			#	ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
			#

			H[16] = H[4] - Data.CONDHT[NCYC]/ Data.MREF/DUTYR
			P[16] = P[4]
			
			#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
			[T[16], XQ[16],XL_Temp ,XV_Temp,  VL[16],VV[16],  HL16,HV16] = self.hpin (H[16],P[16],X)
			#CALL HPIN (H[16],P[16],X,  T[16],XQ[16],XL[1][16],XV[1][16],VL[16],VV[16],HL16,HV16)
			
			self.setArr2dCol (XL, 16, XL_Temp )
			self.setArr2dCol (XV, 16, XV_Temp )
						
			if (VL[16]  ==  0.0): VL[16] = V[4]

			V[16] = VL[16]
			#
			#	ENTER ITERATION FOR EVAPORATOR OUTLET TEMPERATURE
			#
			Data.IE = 1
			while ( (Data.IE  <=  ITMAXE)  and  LECON ): #DO WHILE (Data.IE  <=  ITMAXE  and  LECON)
				print ("aym  inner loooop=============================================Data.IC, IE", Data.IC, Data.IE)
				#if (ICYCL  ==  2) :
				#	CALL GOTOXY(45,17)
				#else:
				#	CALL GOTOXY(45,11)
				# End if
				#CALL PRINT(Data.IE,2,-1)
				self.showMsg ("Iteration Count for (OR EVAPORATOR OUTLET TEMPERATURE) ",Data.IE)

				#----------------------------OLD CODE REPLACED BELOW-------------------
				# old code removed from Python
				#--------------------NEW CODE FOR INCOMPLETE EVAPOPRATION---------------
				#
				I_ERROR_INTER = 0

				if Data.ISPEC== 1:	#!Evap superheat
					#SELECT CASE (ISPEC)

					TE[JE] = T[15] + Data.DTSUPE

					#CALL BUBLT(T[15], XL[1][15], X, P[15], VL[15], V[15], False,LCRIT)
					# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
					
					[ XL_Temp, X, P[15], VL[15], V[15], LCRIT] = self.bublt (T[15], XL_Temp, X, False )
					self.setArr2dCol (XL, 1, XL_Temp)
					
					print ("aym start-=-=-=-==-=inner loop IE,T[15], P15 with be P7 =",Data.IE, T[15], P[15])
					
					P[7] = P[15]
					XQ[15] = 1.0
					XQ[7] = XQ[15]

				if Data.ISPEC== 2:	#IHX superheat
					#CALL BUBLT(T[15],XL[1][15],X,P[15],VL[15],V[15], False,LCRIT)
					# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
					
					[XL_Temp,X,P[15],VL[15],V[15], LCRIT] = bublt (T[15], XL_Temp,X,P[15], False )
					self.setArr2dCol (XL, 15, XL_Temp)
					
					P[13] = P[15]
					T[13] = T[15] + Data.DTSUPI
					VGUESS = V[15]*T[13]/T[15]

					# [P4, P5] = self.espar (P1, P2, P3)
					[A13, B13] = self.espar (0, T[13], X) # 	CALL ESPAR(0,T[13],X, A13,B13)

					#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
					[VGUESS, LCONV] = self.vit (T[13],P[13],A13,B13,VGUESS, False) # CALL VIT(T[13],P[13],A13,B13,VGUESS, False,LCONV)

					V[13] = VGUESS
					#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
					[ H[13],CV,CP,VS] = self.hcvcps (1,T[13],V[13],X)# CALL HCVCPS(1,T[13],V[13],X,  H[13],CV,CP,VS)
					
					P[7] = P[15]
					TE[JE] = T[7]
					if (T[13]  >=  T[16]) :
						LECON = False
						I_ERROR_INTER = 1
						continue

				if Data.ISPEC== 3:	#Evap quality
					XQ[15] = 1.0
					#CALL BUBLT(T[15],XL[1][15],X,P[15],VL[15],V[15], False,LCRIT)
					# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
					[XL_Temp, X, P[15], VL[15], V[15], LCRIT] = bublt (T[15], XL_Temp,X,P[15] , False )
					self.setArr2dCol (XL, 15, XL_Temp)
					
					P[7] = P[15]

				#END SELECT
				#
				#	DETERMINE THE BUBBLE POINT AT THE EVAP EXIT PRESSURE
				#
				# [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
								
				[X, XV15, TBUB15, VBUB15, VV15, LCRIT] = self.bublp ( P[15],X, XV15, True) # CALL BUBLP(P[15],X,XV15,TBUB15,VBUB15,VV15,True,LCRIT)
				#
				#	DETERMINE THE BUBBLE AND DEW POINT ENTHALPIES
				#
				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[H[15] , CV, CP, VS] = self.hcvcps (1, T[15],   V[15], X)		# CALL HCVCPS(1,T[15],V[15],X,   H[15],CV,CP,VS)
				[HBUB15, CV, CP, VS] = self.hcvcps (1, TBUB15, VBUB15, X)	# CALL HCVCPS(1,TBUB15,VBUB15,X, HBUB15,CV,CP,VS)
				
				#
				#INC = 1
				for INC in range (1, NC + 1): #DO WHILE(INC  <=  NC)
					XL[INC][13] = XL[INC][15] #XL_Temp [INC] 
					#INC = INC + 1

				XQ[13] = 1.0
				#
				#
				#	DETERMINE THE ENTHALPY AT [7]
				#
				if Data.ISPEC== 1:		# Exit superheat
					VGUESS = V[15]*T[7]/T[15]

					# [P4, P5] = self.espar (P1, P2, P3)
					[A7, B7] = self.espar (0, T[7], X) #	CALL ESPAR(0,T[7],X, A7,B7)

					#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
					[VGUESS, LCONV] = self.vit (T[7],P[7],A7,B7,VGUESS,False)	#CALL VIT(T[7],P[7],A7,B7,VGUESS,False,LCONV)
					V[7] = VGUESS

					#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
					[H[7],CV,CP,VS] = self.hcvcps (1, T[7], V[7], X) #CALL HCVCPS(1,T[7],V[7],X, H[7],CV,CP,VS)

					XQ[7] = 1.0
					VV[7] = V[7]
					T[7] = TE[JE]

					#INC = 1
					for INC in range (1, NC + 1): #DO WHILE(INC  <=  NC)
						XL[INC][7] = 0.0
						XV[INC][7] = X[INC]

				if Data.ISPEC== 2:		# IHX superheat
					#[P13, P14, P15] = self.inter2 ( P1, ... to .. P12)
					[T[7],H[7],QINT] = self.inter2 (X,P[16],T[16],H[16],V[16],P[13],H[13], T[15],H[15],V[15],ETHX1 )
					#CALL INTER2(X,P[16],T[16],H[16],V[16],P[13],H[13],
					#		T[15],H[15],V[15],ETHX1,  T[7],H[7],QINT)

					#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
					[T[7],XQ[7],XL_Temp, XV_Temp,  VL[7],VV[7],HL7,HV7] = self.hpin ( H[7],P[7],X)
					#CALL HPIN(H[7],P[7],X,  T[7],XQ[7],XL[1][7],XV[1][7],
					#		VL[7],VV[7],HL7,HV7)
					self.setArr2dCol (XL, 7, XL_Temp)
					self.setArr2dCol (XV, 7, XV_Temp )
					
					V[7] = (1.0-XQ[7])*VL[7] + XQ[7]*VV[7]
					TE[JE] = T[7]

				if Data.ISPEC== 3: #Exit quality
					XQ[7] = Data.XEXITE
					#	[P4, P5, P6] = self.enthal ( P1, P2, P3)
					[ X,P[7],H[7] ] = self.enthal ( HBUB15,H[15],XQ[7] )	# CALL ENTHAL(HBUB15,H[15],XQ[7],X,P[7],H[7])

					#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
					[T[7],XQ[7],XL_Temp, XV_Temp,  VL[7],VV[7],HL7,HV7] = self.hpin ( H[7],P[7],X)
					#CALL HPIN(H[7],P[7],X,  T[7],XQ[7],XL[1][7],XV[1][7],
					#		VL[7],VV[7],HL7,HV7)
					self.setArr2dCol (XL, 7, XL_Temp)
					self.setArr2dCol (XV, 7, XV_Temp )
					
					V[7] = (1.0-XQ[7])*VL[7] + XQ[7]*VV[7]
					T[7] = TE[JE]
				#END SELECT

				
				#if (ICYCL  ==  2) :
				#	CALL GOTOXY(14,11)
				#else:
				#	CALL GOTOXY(14,13)
				# End if
				#TSHOW = T[7] - 273.11
				#CALL PRINT(TSHOW,5,1)
				self.showMsg ("Outlet from fresh food evaporator (C) - point 7 ", T[7] - 273.11)

				#
				#			INTERCHANGER FOR SUBCOOLING CONDENSER LIQUID
				#
				if (Data.ISPEC  !=  2) :
					#P11 = self.inter1 ( P1, ... to .. P10)
					QINT = self.inter1 ( X,P[16],T[16],  H[16],V[16],P[7],  T[7],H[7],V[7],  ETHX1 )
					#CALL INTER1(X,P[16],T[16],  H[16],V[16],P[7],  T[7],H[7],V[7],  ETHX1,QINT)
					H[13] = H[7] + QINT

				H[6] = H[16] - QINT
				#
				#			DETERMINE THE REMAINDER OF THE PROPERTIES AT [6] AND [13]
				#
				P[6]  = P[4]
				P[13] = P[7]

				print ("aym 		inside inner loop Data.IE,, T[15],P[7]=",Data.IE,T[15], P[7])
				
				#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
				[ T[6],XQ6, XL_Temp, XV_Temp, V[6],  VV6,HL,HV] = self.hpin ( H[6],P[6],X )
				#CALL HPIN (H[6],P[6],X,  T[6],XQ6,XL[1][6],XV[1][6],V[6],
				#		VV6,HL,HV)
				self.setArr2dCol (XL, 6, XL_Temp)
				self.setArr2dCol (XV, 6, XV_Temp)
				
				VL[6] = V[6]  #Tony
				XQ[6] = 0.0

				if (Data.ISPEC  !=  2):
					#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
					#	CALL HPIN (H[13],P[13],X,  T[13],XQ13,XL[1][13],
					#		XV[1][13],VL13,V[13],HL,HV)
					[T[13], XQ13, XL_Temp,  XV_Temp, VL13, V[13], HL, HV] = self.hpin ( H[13],P[13],X )
					self.setArr2dCol (XL, 13, XL_Temp)
					self.setArr2dCol (XV, 13, XV_Temp )
					
				XQ[13] = 1.0

				#if (ICYCL  ==  2) :
				#	CALL GOTOXY(2,5)
				#else:
				#	CALL GOTOXY(2,6)
				# End if
				#TSHOW = T[16] - 273.11
				#CALL PRINT(TSHOW,5,1)
				self.showMsg ("Liquid line state after heat loss to cabinet & mullion (C) - point 16 ", T[16] - 273.11)

				#if (ICYCL  ==  2) :
				#	CALL GOTOXY(14,5)
				#else:
				#	CALL GOTOXY(14,6)
				# End if
				#TSHOW = T[13] - 273.11
				#CALL PRINT(TSHOW,5,1)
				self.showMsg ("Superheated gas leaving hight temp interchanger (C) - point 13 ", T[13] - 273.11)
				#
				#	FIND CONDITIONS AT EVAPORATOR INLET ASSUMING ISENTHALPIC
				#	EXPANSION
				#
				P[5] = P[13] + DPE
				
				print ("aym inner loop 							IE,P5 from p13+DPE =",Data.IE, P[5])
				
				#TSHOW = T[6] - 273.11
				#CALL GOTOXY(2,13)
				#CALL PRINT(TSHOW,5,1)
				self.showMsg ("liquid line outlet from high temp interchanger (C) -point 6 ", T[6] - 273.11)
				
				#-----------------------END OF NEW CODE (12/29/90)----------------------
				#
				#	CALL ANALYSIS OF FREEZER SECTION AND THE LOWER
				#	INTERCHANGER (LOOKS LIKE A NON-ADIABATIC
				#	EXPANSION VALVE TO REST OF SYSTEM
				#
				#  [P3 to P12, P13 ,P16, P19] = self.lowevp (P1... P12, P14, P15, P17..P18)
				
				[H,  P,X,T,  XQ,XL,XV,  VL,VV,HL, HV, TS6, QFREZ ] = self.lowevp (ICYCL,0,H,  P,X,T,  XQ,XL,XV,  VL,VV,HL, TS3, TS5, DPF,ETHX2)
				
				#CALL LOWEVP(ICYCL,ICNTRL,H,  P,X,T,  XQ,XL,XV,  VL,VV,HL,  HV,TS3,TS5,  TS6,DPF,ETHX2,  QFREZ,LQUIT)
				#
				#	CALCULATE FRESH FOOD SECTION HEAT EXCHANGE
				#
				PDEWE = P[5] - (1.0-FSUPE)*DPE
				if (PDEWE >  P[5]): PDEWE = P[5]
				
				print ("aym 		inner loop IE,PDEWE final=",Data.IE, PDEWE)
				
				# [P2, P3, P4, P5, P6, P8] = bublp ( P1, P2, P3,    P7)
				[XREF,X,TDEW,VLDEW,VDEW, LCRIT] = self.bublp (PDEWE,XREF,X, False) # 	# CALL BUBLP(PDEWE,XREF,X,TDEW,VLDEW,VDEW,False,LCRIT)

				POLDE = PDEWE
				if (TDEW  >=  TS3): TDEW = TS3 - 1.0
				if (T[5]  >=  TS3): T[5] = TS3 - 1.0

				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[HDEW, CVRVAP, CPRVAP, VS] = self.hcvcps (3, TDEW, VDEW, X) # CALL HCVCPS(3,TDEW,VDEW,X, HDEW,CVRVAP,CPRVAP,VS)
				#
				#	STATE 12 IS THE POINT AT WHICH THE DEW POINT IS REACHED
				#	IN THE EVAPORATOR
				#
				P[12] = PDEWE
				T[12] = TDEW
				V[12] = VDEW
				H[12] = HDEW
				XQ[12] = 1.0
				
				print ("aym inner loop IE,T[12]=",Data.IE, -273.11+T[12])
				#
				#	FIND DUTY CYCLE, NET CAPACITY AND AVERAGE
				#	FREEZER LOAD if ICYCL = 1
				#
				if (Data.IC  !=  1) :
					# [P9, P10, P15] = self.dutfnd (P1 ... P8, P11 to P15 )
					#CALL DUTFND(ICAB,IRFTYP,ICYCL,NCYC,QFRSH,QFREZ,
					#			FROSTF,FROSTZ,QFF,QFZ,TS3,TS5,T,IDFRST,
					#			DUTYR)
					[QFF, QFZ, DUTYR] = self.dutfnd (ICAB,IRFTYP,ICYCL,NCYC,QFRSH,QFREZ, FROSTF,FROSTZ,TS3,TS5,T,IDFRST )

				#
				#	CALCULATE FRESH FOOD EVAPORATOR HEAT TRANSFER.
				#	TEST FOR STANDARD DESIGN.
				#
				if (IRFTYP  <=  3) :
					if (ICYCL  ==  1  and  ICAB	!=  0  and  Data.IFRSH  !=  0) :
						if (Data.IC  ==  1) :
							TIN = 0.15*TFF + 0.85*TFZ
							FF_FRACT = 0.0 # in Python only
						else:
							Data.CAPE = QFRSH/1.0548 - 3.413 * Data.FANE - 3.413*(Data.DFSTCYC + Data.FZCYC)
							CFMA = Data.CFME/(1.08*1.8961)
							QFM = QFF + 3.413 * Data.DUTYC * Data.FFCYC
							#[P7, P8] = self.mixair (P1,P2 ... P6)
							[TIN,FF_FRACT] = self.mixair (Data.CAPE,QFM,QFZ,TFF,TFZ,CFMA)
							#CALL MIXAIR(CAPE,QFM,QFZ,TFF,TFZ,CFMA,   TIN,FF_FRACT)
						# End if
						TS3 = (TIN + 459.6)/1.8

				if (Data.IFRSH  ==  0) :
					#	NATURAL CONVECTION
					#CALL FFNAT(T[5],H[5],T[7],TDEW,HDEW,TS3,CPRVAP,
					#			QFRSH,FSUPE, IRET)
					#  .cancel in Fortran		QFRSH,FSUPE, IRFTYP)
					[QFRSH,FSUPE] = self.ffnat (T[5],H[5],T[7],TDEW,HDEW,TS3,CPRVAP, IRET)

				if (Data.IFRSH  ==  1) :
					#
					#  CROSS FLOW
					#
					# [P13, P14] = self.ffcross (P1 ... P12)
					[QFRSH, FSUPE] = self.ffcross (H[5],T[5],HDEW, TDEW,TS3,CPRVAP,   P[5], P[7], X, Data.N_EVAP )
					#CALL FFCROSS(H[5],T[5],HDEW,TDEW,TS3,CPRVAP,
					#			P[5], P[7], X, Data.N_EVAP, QFRSH, FSUPE)

				if (Data.IFRSH  ==  2) :
					#
					#  COUNTERFLOW
					#
					[QFRSH, FSUPE ]= self.ffcount (H[5],T[5],HDEW,TDEW,TS3,CPRVAP,P[5], P[7], X, Data.N_EVAP)
					#CALL FFCOUNT(H[5],T[5],HDEW,TDEW,TS3,CPRVAP,
					#			P[5], P[7], X, Data.N_EVAP, QFRSH,FSUPE)
				# End if
				#
				#			SUPERHEATING FRACTION

				FSHOLD = FSUPE
				FSUPE = (FSHOLD + FSUPE)/2.0

				if (FSUPE >  1.05*FSHOLD): FSUPE = 1.05*FSHOLD
				if (FSUPE <  0.95*FSHOLD): FSUPE = 0.95*FSHOLD
				#
				#	FRESH FOOD SECTION EVAPORATOR
				#
				# [P4, P5,P6  P8] = self.frsh (P1,P2,P3   P5,P6,P7)
				[TS4, TE,JE, ICONE] = self.frsh (H,T,TS3,  TE,JE,QFRSH) # CALL FRSH(H,T,TS3, TS4,TE,JE,QFRSH,ICONE)
				
				#---------------------------ADDED NEW CODE (12/29/90)-------------------
				T[15] = T[15] + TE[2] - T[7]
				print ("aym ICONE=", ICONE)
				#-----------------------------END OF NEW CODE---------------------------
				#
				#
				#	CALCULATE THE AVERAGE EFFECTIVENESS OF THE FF EVAPORATOR
				#	CALCULATE THE HEAT TRANSFER if THE REFRIGERANT LEFT AT TS3
				#


				VGUESS = VDEW * TS3 / TDEW
				# [P4, P5] = self.espar (P1, P2, P3)
				[AS3, BS3] = self.espar (0, TS3, X) # CALL ESPAR(0,TS3,X, AS3,BS3)

				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[VGUESS, LCONV] = self.vit (TDEW,P[7],AS3,BS3,VGUESS,False) # CALL VIT(TDEW,P[7],AS3,BS3,VGUESS,False,LCONV)
				VS3 = VGUESS

				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[HS3, CV, CP, VS] = self.hcvcps (1, TS3, VS3, X) # CALL HCVCPS(1,TS3,VS3,X,  HS3,CV,CP,VS)

				QRMAX = Data.MREF*(HS3-H[5])
				#
				#	CALCULATE THE HEAT TRANSFER if THE AIR LEFT AT T[5]
				#
				QAMAX = Data.CFME*(TS3-T[5])
				QMAXE = QAMAX

				if (QRMAX < QAMAX) :QMAXE = QRMAX

				Data.ETAE = QFRSH/QMAXE
				
				if (ICONE  ==  1): LECON = False
				print ("aym ........................................ TE[JE]",TE[JE])
				if (TE[JE]  <=  TEMIN):  LECON = False
					
				Data.IE = Data.IE + 1
			#END DO

			#
			#	END OF EVAPORATOR ITERATION
			#
			T[7] = TE[JE]
			#if (ICYCL  ==  2) :
			#	CALL GOTOXY(14,11)
			#else:
			#	CALL GOTOXY(14,13)
			# End if

			#TSHOW = T[7] - 273.11
			#CALL PRINT(TSHOW,5,1)
			self.showMsg ("outlet from fresh food evaporator (C) - point 7 ", T[7] - 273.11)
			#
			#	INTERCHANGER FOR SUBCOOLING CONDENSER LIQUID
			# old code removed from Python
			#-----------------NEW CODE ADDED FOR INCOMPLETE EVAPORATION-------------

			if Data.ISPEC==1:	# Exit superheat
				#SELECT CASE (Data.ISPEC)
				#CASE [1]												!Exit superheat

				VGUESS = V[15]*T[7]/T[15]
				# [P4, P5] = self.espar (P1, P2, P3)
				[A7, B7] = self.espar (0, T[7], X) #  CALL ESPAR(0,T[7],X,A7,B7)

				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[VGUESS, LCONV] = self.vit (T[7],P[7],A7,B7,VGUESS,False) #	CALL VIT(T[7],P[7],A7,B7,VGUESS,False,LCONV)
				V[7] = VGUESS

				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[H[7],CV,CP,VS] = self.hcvcps (1, T[7], V[7], X) # CALL HCVCPS(1,T[7],V[7],X,   H[7],CV,CP,VS)

				XQ[7] = 1.0
				VV[7] = V[7]
				T[7] = TE[JE]

				#INC = 1
				for INC in range (1, NC + 1): # DO WHILE(INC  <=  NC)
					XL[INC][7] = 0.0
					XV[INC][7] = X[INC]
					#INC = INC + 1
				#END DO
			if Data.ISPEC==2:	# IHX Superheat
				#CASE [2]												!IHX Superheat
				#[P13, P14, P15] = self.inter2 ( P1, ... to .. P12)
				[T[7],H[7],QINT] = self.inter2 (X,P[16],T[16],H[16],V[16],P[13],H[13], T[15],H[15],V[15],ETHX1)
				#CALL INTER2(X,P[16],T[16],H[16],V[16],P[13],H[13],
				#	T[15],H[15],V[15],ETHX1,  T[7],H[7],QINT)

				#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
				[T[7],XQ[7],XL_Temp ,XV_Temp ,VL[7],  VV[7],HL7,HV7] = self.hpin ( H[7],P[7],X )
				#CALL HPIN(H[7],P[7],X  ,T[7],XQ[7],XL[1][7],XV[1][7],VL[7],
				#	VV[7],HL7,HV7)
				self.setArr2dCol (XL, 7, XL_Temp )
				self.setArr2dCol (XV, 7, XV_Temp )
				
				V[7] = (1.0-XQ[7])*VL[7] + XQ[7]*VV[7]
				TE[JE] = T[7]

			if Data.ISPEC==3:	# Exit quality
				#CASE [3]												!Exit quality
				XQ[7] = Data.XEXITE

				#	[P4, P5, P6] = self.enthal ( P1, P2, P3)
				[X,P[7],H[7]] = self.enthal ( HBUB15,H[15],XQ[7]) # CALL ENTHAL(HBUB15,H[15],XQ[7],X,P[7],H[7])

				#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
				[T[7],XQ[7],XL_Temp,XV_Temp ,VL[7], VV[7],HL7,HV7] = self.hpin ( H[7],P[7],X )
				#CALL HPIN(H[7],P[7],X,  T[7],XQ[7],XL[1][7],XV[1][7],VL[7],
				#	VV[7],HL7,HV7)
				self.setArr2dCol (XL, 7, XL_Temp)
				self.setArr2dCol (XV, 7, XV_Temp)
				
				V[7] = (1.0-XQ[7])*VL[7] + XQ[7]*VV[7]
				T[7] = TE[JE]

			#END SELECT

			if (Data.ISPEC  !=  2) :
				#P11 = self.inter1 ( P1, ... to .. P10)
				QINT = self.inter1 ( X,P[16],T[16],  H[16],V[16],P[7],  T[7],H[7],V[7], ETHX1 )
				#CALL INTER1(X,P[16],T[16],  H[16],V[16],P[7],  T[7],H[7],V[7], ETHX1,QINT )
				H[13] = H[7] + QINT
			# End if

			H[6] = H[16] - QINT

			#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
			[T[6],XQ6,XL_Temp ,XV_Temp ,V[6],   VV6,HL,HV] = self.hpin ( H[6],P[6],X )
			#CALL HPIN (H[6],P[6],X,   T[6],XQ6,XL[1][6],XV[1][6],V[6],
			#	VV6,HL,HV)
			self.setArr2dCol (XL, 6, XL_Temp)
			self.setArr2dCol (XV, 6, XV_Temp )
			
			VL[6] = V[6]
			XQ[6] = 0.0

			if (Data.ISPEC  !=  2) :
				
				#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
				[T[13],XQ13,XL_Temp, XV_Temp, L13,V[13],HL,HV] = self.hpin ( H[13],P[13],X )
				#CALL HPIN (H[13],P[13],X,  T[13],XQ13,XL[1][13],XV[1][13],
				#	VL13,V[13],HL,HV)
				self.setArr2dCol (XL, 13, XL_Temp )
				self.setArr2dCol (XV, 13, XV_Temp )
			# End if

			XQ[13] = 1.0

			#TSHOW = T[13] - 273.11
			#if (ICYCL  ==  2) :
			#	CALL GOTOXY(14,5)
			#else:
			#	CALL GOTOXY(14,6)
			# End if

			#CALL PRINT(TSHOW,5,1)
			self.showMsg ("superheated gas leaving the high temp interchanger (C) - point 13", T[13] - 273.11)

			TE[1] = TE[JE]
			#--------------------------END OF NEW CODE (12/29/90)-------------------
			#
			#	FIND ENTROPY AT COMPRESSOR INLET AND COMPUTE CONDITIONS AT
			#	COMPRESSOR OUTLET (TSPEC IS DEFINED INLET TEMP TO THE
			#	COMPRESSOR (-1 MEANS NO TEMPERATURE CHANGE)
			#
			P[1] = P[13]
			if (Data.TSPEC >  0.0) :
				T[1] = Data.TSPEC
				VGUESS = V[7]*T[1]/T[7]
				# [P4, P5] = self.espar (P1, P2, P3)
				[A1, B1] = self.espar (0, T[1], X) # CALL ESPAR(0,T[1],X,A1,B1)
				
				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[VGUESS, LCONV] = self.vit ( T[1],P[1],A1,B1,VGUESS,False) #	CALL VIT(T[1],P[1],A1,B1,VGUESS,False,LCONV)
				V[1] = VGUESS
				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[H[1],CV,CP,VS] = self.hcvcps (1,T[1],V[1],X) # CALL HCVCPS (1,T[1],V[1],X,  H[1],CV,CP,VS)
			else:
				
				T[1] = T[13]
				H[1] = H[13]
				V[1] = V[13]
				XQ[1] = XQ[13]
			# End if
			
			S[1] = self.entrop(T[1],V[1],X)
			P[2] = P[3] + FSUPC*DPC
			
			#TSHOW = T[1] - 273.11
			#if (ICYCL  ==  2) :
			#	CALL GOTOXY(43,5)
			#else:
			#	CALL GOTOXY(43,6)
			# End if

			#CALL PRINT(TSHOW,5,1)
			self.showMsg ("compressor inlet (saturated vapor) (C) - point 1", T[1] - 273.11)
			#
			# CALL COMPRESSOR MODEL
			#
			if (Data.IMAP == 0) :
				#
				# CALL TABULAR MAP BASED MODEL
				#
				OLDMAS = Data.MREF
				#[P1, P3to P5,P7, P9 to P11, P13,P14, P15,P17 to P19 ] = self.compcall (P1 to P6, P12, P16)
				[T, HOUT, QHILO, QCAN, VSUC, VV2, TSUC, TDISC, GAMA, RN, ETAS] = self.compcall (H, P, X, T, V,TS1)
				#CALL COMPCALL(H, P, X, T, CV, CP,     HOUT, MEFF,
				#	QHILO, QCAN, VSUC, V, VV2, TSUC, TDISC,
				#	TS1, GAMA, RN, ETAS)

				Data.MREF = (Data.MREF + 2.0*OLDMAS)/ 3.0
				if (Data.MREF >  1.05*OLDMAS): Data.MREF = 1.05*OLDMAS
				if (Data.MREF <  0.95*OLDMAS): Data.MREF = 0.95*OLDMAS

				FLOW2 = FLWREF * Data.MREF/MREFSV
			# End if

			if (Data.IMAP == 1 or Data.IMAP == 2) :
				
				#
				# CALL THEORETICALLY BASED MODEL
				#[P2, P4, P5, P6, P7 , P9 to P11,  P14, P15, P17, P18,P19 ] = self.comp (P1 to P4, P8,P9, P12, P16 )
				[T, HOUT, QHILO, QCAN,VSUC,  VV2, TSUC, TDISC, GAMA, RN, ETAS] = 	\
					self.comp (H, P, X, T, MEFF, QHILO, QCAN, V, TS1 )
				#CALL COMP(H, P, X, T,     CV, CP,    HOUT, MEFF, QHILO, QCAN,
				#	VSUC, V, VV2, TSUC, TDISC, TS1, GAMA, RN, ETAS)
				#
				#	UPDATE THE MASS FLOW TO CORRESPOND TO THE DISPLACEMENT
				#
				DISPI = Data.DISP/1.6387E-05
				OLDMAS = Data.MREF

				Data.MREF = (Data.MREF * (DISPLC/DISPI) + 2.0*OLDMAS)/3.0
				if (Data.MREF >  1.05*OLDMAS): Data.MREF = 1.04*OLDMAS
				if (Data.MREF <  0.95*OLDMAS): Data.MREF = 0.96*OLDMAS

				FLOW2 = FLWREF * Data.MREF/MREFSV
			#
			#	SHOW THE PROGRESS IN THE SOLUTION
			#
			self.progrs (ICYCL,H,HOUT,WMAVG,FLOW2,QCAN)
			#
			#	CONDITIONS OF GAS LEAVING COMPRESSOR SHELL
			#

			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[H[2],CV,CP,VS] = self.hcvcps (1, T[2], VV2, X) # CALL HCVCPS (1,T[2],VV2,X,   H[2],CV,CP,VS)

			#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
			[T[2],XQ[2],XL_Temp, XV_Temp ,VL2,VV2,HL2,HV2] = self.hpin ( H[2],P[2],X )
			#CALL HPIN (H[2],P[2],X    ,T[2],XQ[2],XL[1][2],
			#	XV[1][2],VL2,VV2,HL2,HV2)
			self.setArr2dCol (XL, 2, XL_Temp)
			self.setArr2dCol (XV, 2, XV_Temp )
			
			V[2] = VV2
			#
			#	ENTROPY OF GAS LEAVING COMPRESSOR
			#
			if (XQ[2] <  1.0) :
				SL2 = self.entrop(T[2],VL2,self.getArr2dCol (XL,2) ) #XL[1][2]
				SV2 = self.entrop(T[2],VV2,self.getArr2dCol (XV,2)) # XV[1][2]
				S[2] = XQ[2]*SV2 + (1.0-XQ[2])*SL2
			else:
				S[2] = self.entrop(T[2],VV2,X)

			#TSHOW = T[2] - 273.11
			#CALL GOTOXY(63,2)
			#CALL PRINT(TSHOW,5,1)
			self.showMsg ("compressor discharge (C) - point 2", T[2] - 273.11)

			if (ICONC  !=  1) :
				#
				#	CALCULATE CONDENSER HEAT EXCHANGE
				#	DETERMINE THE DEW POINT CONDITIONS
				#

				# [P2, P3, P4, P5, P6, P8] = bublp ( P1, P2, P3,    P7)
				[XL_Temp ,X,TDEW,VL_notused,VDEW, LCRIT] = self.bublp (P[3], XL_Temp ,X,False) # CALL BUBLP(P[3],XL(1,3),X,TDEW,VL,VDEW,False,LCRIT)
				self.setArr2dCol (XL, 3, XL_Temp)
				
				T[3] = TDEW
				V[3] = VDEW

				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[HDEW,CVRVAP,CPRVAP,VS] = self.hcvcps (3,TDEW,VDEW,X) # 	CALL HCVCPS(3,TDEW,VDEW,X,  HDEW,CVRVAP,CPRVAP,VS)

				H[3] = HDEW
				#
				#	DETERMINE BUBBLE POINT CONDITIONS
				#	ASSUME A LINEAR PRESSURE DROP THROUGHOUT THE CONDENSER
				#
				PBUB = P[4] + DPC * FSUBC

				# [P2, P3, P4, P5, P6, P8] = bublp ( P1, P2, P3,    P7)
				[X, XV_Temp ,TBUB,VBUB,V4, LCRIT] = self.bublp ( PBUB,X, XV_Temp, True) # 	CALL BUBLP(PBUB,X,XV[1][4],TBUB,VBUB,V4,True,LCRIT)
				self.setArr2dCol (XV, 4, XV_Temp )
				
				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[HBUB,CVRLIQ,CPRLIQ,VS] = self.hcvcps (3,TBUB,VBUB,X) # CALL HCVCPS(3,TBUB,VBUB,X, HBUB,CVRLIQ,CPRLIQ,VS)

				#
				#	STATE 11 IS THE POINT AT WHICH THE BUBBLE POINT
				#	IS REACHED IN THE CONDENSER
				#
				P[11] = PBUB
				T[11] = TBUB
				V[11] = VBUB
				H[11] = HBUB
				#
				#	DETERMINE CONDITIONS ENTERING THE CONDENSER
				#
				HDROP = Data.CONDVP[NCYC]/Data.MREF/Data.DUTYC
				# !				HDROP = CONDVP(NCYC)/FLOW2/DUTYC

				P[14] = P[2]
				HINCND = H[2] - HDROP

				H[14] = HINCND
				#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
				
				
				[T[14], XQ[14], XL_Temp, XV_Temp,VL[14],VV[14],HL14,HV14] = self.hpin ( H[14],P[2],X )
				#CALL HPIN(H[14],P[2],X,   T[14],XQ[14],XL[1][14],	XV[1][14],VL[14],VV[14],HL14,HV14)
				self.setArr2dCol (XL, 14, XL_Temp )
				self.setArr2dCol (XV, 14, XV_Temp )
				
				V[14] = (1.0-XQ[14])*VL[14] + XQ[14]*VV[14]

				if (Data.ICOND  ==  0) :
					#	NATURAL CONVECTION
					#[ P12,P13,P14,P15, P16, P17 ] = self.cnat (P1 to P11 )
					#CALL CNAT(TS1,TS3,TS5,T[14],H[14],TDEW,HDEW,T,
					#	TBUB,HBUB,CPRLIQ,  QDSC,QTPC,QSCC,QTOTC, FSUPC,FSUBC)
					[ QDSC,QTPC,QSCC,QTOTC, FSUPC,FSUBC ] = self.cnat (TS1,TS3,TS5,T[14],H[14],TDEW,HDEW,T, TBUB,HBUB,CPRLIQ )

				if (Data.ICOND  ==  1) :
					#	CROSS FLOW
					#[P9, P14 ... P19 ]= self.ccross (P1 to P8, P10,P11,P12,P13   )
					#CALL CCROSS(TS1, T[14], H[14], TDEW, HDEW, TBUB,
					#	HBUB, CPRLIQ, CPRVAP, PBUB, P[4], X,
					#	Data.N_COND, QDSC, QTPC, QSCC, QTOTC,
					#	FSUPC, FSUBC)
					[CPRVAP, QDSC, QTPC, QSCC, QTOTC, FSUPC, FSUBC ]=self.ccross (TS1, T[14], H[14], TDEW, HDEW, TBUB, HBUB, CPRLIQ, PBUB, P[4], X, Data.N_COND)

				if (Data.ICOND  ==  2) :
					#
					# COUNTERFLOW
					#[P9, P14 to P19 ]= self.ccount (P1 to P8, P10,P11,P12,  P13   )
					#CALL CCOUNT(TS1, T[14], H[14], TDEW, HDEW, TBUB,
					#	HBUB, CPRLIQ, CPRVAP, PBUB, P[4], X,
					#	Data.N_COND, QDSC, QTPC, QSCC, QTOTC,
					#	FSUPC, FSUBC)
					[CPRVAP, QDSC, QTPC, QSCC, QTOTC, FSUPC, FSUBC]=self.ccount (TS1, T[14], H[14], TDEW, HDEW, TBUB, HBUB, CPRLIQ, CPRVAP, P[4], X, Data.N_COND, QDSC)

				# End if
				#[ P6, P13] = self.cond (P1,P2, P5,P7,P9, P10,P11,P12 )
				#[ P6, P7, P12, P13] = self.cond (P1,P2, P5, P7, P9 to P13 )
				[ TS2,TC,JC, ICONC] = self.cond (T,H,   TS1,TC,QDSC,   QTPC,QSCC,JC, ICONC )
				#CALL COND(T,H,  TBUB,HBUB,  TS1,TS2,TC,CPRLIQ,QDSC,	QTPC,QSCC,JC,ICONC)
				#
				#	ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
				#
				H[16] = H[4] - Data.CONDHT[NCYC]/Data.MREF/DUTYR
				P[16] = P[4]
				#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
				[T[16], XQ[16],XL_Temp ,XV_Temp,  VL[16],VV[16],HL16,HV16] = self.hpin ( H[16],P[16],X )
				#CALL HPIN (H[16],P[16],X,T[16], XQ[16],XL[1][16],XV[1][16],
				#	VL[16],VV[16],HL16,HV16)
				self.setArr2dCol (XL, 16, XL_Temp )
				self.setArr2dCol (XV, 16, XV_Temp )
				
				if (VL[16]  ==  0.0): VL[16] = V[4]
				V[16] = VL[16]

				#
				#	CALCULATE THE AVERAGE EFFECTIVENESS OF THE HEAT EXCHANGER
				#	CALCULATE THE HEAT TRANSFER if THE REFRIGERANT LEFT AT TS1
				#
				#	DETERMINE THE SPECIFIC VOLUME OF THE LIQUID
				#
				if (TS1 <  T[4]) :
					VGUESS = V[4]
					# [P4, P5] = self.espar (P1, P2, P3)
					[AS1, BS1] = self.espar (0, TS1, X) #  CALL ESPAR(0,TS1,X,AS1,BS1)

					#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
					[VGUESS, LCONV] = self.vit (TS1,P[4],AS1,BS1,VGUESS,True) # CALL VIT(TS1,P[4],AS1,BS1,VGUESS,True,LCONV)
					VS1 = VGUESS
				else:
					VS1 = V[4]
				# End if

				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[HS1, CV, CP, VS] = self.hcvcps (1, TS1, VS1, X) # 	CALL HCVCPS(1,TS1,VS1,X,  HS1,CV,CP,VS)

				QRMAX = Data.MREF*(H[14]-HS1)
				#
				#	CALCULATE THE HEAT TRANSFER if THE AIR LEFT AT T[14]
				#
				QAMAX = Data.CFMC*(T[14]-TS1)
				QMAXC = QAMAX
				if (QRMAX <  QAMAX): QMAXC = QRMAX

				Data.ETAC = QTOTC/QMAXC
			else:
				LCCON = False
			# End if

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

		T[4] = TC[JC]
		TS[1] = 0.0
		TS[4] = TS1
		TS[5] = TS4
		TS[6] =0.0
		TS[7] = TS3
		TS[8] = TS6
		TS[9] = TS5
		TS[10] =0.0
		TS[14] =TS2
		TS[16] = 0.0
		TGC = T[3] - T[4]
		#
		#	SPECIFIC VOLUMES
		#
		V[5] = min(1.0, 1.0 - XQ[5] ) * VL[5] + max(0.0, XQ[5]  ) * VV[5]
		V[8] = min(1.0, 1.0 - XQ[8] ) * VL[8] + max(0.0, XQ[8]  ) * VV[8]
		V[9] = min(1.0, 1.0 - XQ[9] ) * VL[9] + max(0.0, XQ[9]  ) * VV[9]
		V[10]= min(1.0, 1.0 - XQ[10]) * VL[10]+ max(0.0, XQ[10] ) * VV[10]
		#
		#	QUALITIES
		#
		XQ[1] = 1.0
		XQ[3] = 1.0
		XQ[4] = 0.0
		XQ[6] = 0.0
		XQ[10] = 0.0
		#
		#	LIQUID AND VAPOR COMPOSITIONS AROUND THE CYCLE LOOP
		#
		#I = 1
		for I in range (1, NC + 1): #	DO WHILE (I  <=  NC)
			XL[I][1] = 0.0
			XV[I][1] = X[I]
			
			XL[I][2] = 0.0
			XV[I][2] = X[I]
			
			XL[I][3] = 0.0
			XV[I][3] = X[I]
			
			XL[I][4] = X[I]
			XV[I][4] = 0.0
			
			XL[I][6] = X[I]
			XV[I][6] = 0.0
			
			XL[I][7] = 0.0
			XV[I][7] = X[I]
			
			XL[I][10] = X[I]
			XV[I][10] = 0.0
			
			XL[I][11] = X[I]
			XV[I][11] = 0.0
			
			XL[I][12] = 0.0
			XV[I][12] = X[I]
			
			XL[I][13] = 0.0
			XV[I][13] = X[I]
			
			XL[I][14] = 0.0
			XV[I][14] = X[I]
			
			XL[I][16] = X[I]
			XV[I][16] = 0.0

		#
		#	ENTROPIES AROUND THE CYCLE
		#
		#J = 3
		for J in range(3, 16+1): #DO WHILE (J  <=  16)
			if (J  !=  5): S[J] = self.entrop( T[J], V[J] ,X)
			#J = J + 1
		#END DO
		
		SL5 = self.entrop(T[5],VL[5], self.getArr2dCol (XL,5) ) # XL[1][5]
		SV5 = self.entrop(T[5],VV[5], self.getArr2dCol (XV,5) ) # XV[1][5]
		S[5] = min(1.0,1.0-XQ[5]) * SL5 + max(0.0,XQ[5]) * SV5
		
		SL8 = self.entrop(T[8],VL[8],  self.getArr2dCol (XL,8) ) # XL[1][8]
		SV8 = self.entrop(T[8],VV[8],  self.getArr2dCol (XV,8) ) # XV[1][8]
		S[8] = min(1.0,1.0-XQ[8]) * SL8 + max(0.0,XQ[8]) * SV8
		
		SL9 = self.entrop(T[9],VL[9], self.getArr2dCol (XL,9) ) # XL[1][9]
		SV9 = self.entrop(T[9],VV[9], self.getArr2dCol (XV,9) ) # XV[1][9]
		S[9] = min(1.0,1.0-XQ[9]) * SL9 + max(0.0,XQ[9]) * SV9
		#
		#	CONVERT FROM MOLAR QUANTITIES TO MASS BASIS
		#
		#J = 1
		
		for J in range (1, 16 + 1): #DO WHILE (J  <=  16)
			WMAVGL[J] = 0.0
			WMAVGV[J] = 0.0
			#I = 1
			for I in range (1, NC + 1): #DO WHILE (I  <=  NC)
				WMAVGL[J] = WMAVGL[J] + (XL[I][J] ) * (Data.WM[I])
				WMAVGV[J] = WMAVGV[J] + (XV[I][J] ) * (Data.WM[I])
				#I = I + 1
			#END DO
			
			for I in range (1, NC + 1): #DO WHILE (I  <=  NC)
				if (XL[I][J] >  0.0): XL[I][J] = XL[I][J] * Data.WM[I]/WMAVGL[J]
				if (XV[I][J] >  0.0): XV[I][J] = XV[I][J] * Data.WM[I]/WMAVGV[J]
				#I = I + 1
			#END DO
			
			V[J] = V[J]/WMAVG
			H[J] = H[J]/WMAVG
			S[J] = S[J]/WMAVG
			#J = J + 1
		
		#
		#	COMPUTE WORK, CAPACITY, COP, ETC.
		#
		HOUT = HOUT/WMAVG
		VSUC = VSUC/WMAVG
		Data.W = (HOUT - H[1])/(1.0 - QCAN)
		Data.QE = H[7] - H[5]
		QC = H[14] - H[4]
		Data.QZ = H[9] - H[8]
		Data.COPR = (Data.QE + Data.QZ)/Data.W
		
		TH = TS1
		TL1 = TS3
		TL2 = TS5
		DENOM = TH * (Data.QE * (1./TL1-1./TH)+ Data.QZ * (1./TL2-1./TH))
		COPI = (Data.QE+ Data.QZ)/DENOM
		PR = P[2]/P[1]
		TSUPC = T[2] - T[3]
		
		#
		#	CORRECT COP DUR TO CYCLING LOSSES
		#
		if (Data.I_CYCLE  ==  0) :
			Data.CORR_COP = 1.0
		else:
			TENV = (Data.TROOM + 459.6)/1.8
			TMID_COND = (T[3] + T[11])/2.0
			TMID_EVAP = (T[8] + T[9])/2.0
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
				TE[1] = TE[1] - 273.11
				TE[2] = TE[2] - 273.11

				# WRITE (IO_Cycle,2200) TE[1],TE[2]'
				objCycOut.write_or_terminate  ('EVAPORATOR ITERATION DID NOT CONVERGE,  %9.3f, %9.3f ' %( TE[1],TE[2]) )

				#CALL GOTOXY(22,11)
				#CALL PRINT('EVAPORATOR ITERATION DID NOT CONVERGE$',37,-2)
				self.showError ("EVAPORATOR ITERATION DID NOT CONVERGE")
			# End if

			if (LCCON) :
				TC[1] = TC[1] - 273.11
				TC[2] = TC[2] - 273.11

				#'WRITE (IO_Cycle,2202) TC[1],TC[2]'
				objCycOut.write_or_terminate  ('CONDENSER ITERATION DID NOT CONVERGE,  %9.3f, %9.3f ' %( TE[1],TE[2]) )
				
				#CALL GOTOXY(22,6+LWIND)
				#CALL PRINT('CONDENSER ITERATION DID NOT CONVERGE$',36,-2)
				self.showError ("CONDENSER ITERATION DID NOT CONVERGE")
			# End if

			if (I_ERROR_INTER >  0) :
				#"WRITE (IO_Cycle,2203)"
				objCycOut.write_or_terminate  ('INTERCHANGER SUPERHEAT NOT POSSIBLE')
				
				#CALL GOTOXY(22,6+LWIND)
				#CALL PRINT('INTERCHANGER SUPERHEAT NOT POSSIBLE$',36,-2)
				self.showError ("INTERCHANGER SUPERHEAT NOT POSSIBLE")
			# End if


			# say beep CALL WARBLE
			# say beep CALL WARBLE
			# say beep CALL WARBLE

			#READ( * , * )
			input("Press Enter to continue...")
		# End if

		#
		#	OUTPUT RESULTS.  BEGIN BY CONVERTING TO ENGLISH UNITS.
		#

		TENV = (Data.TROOM + 459.6)/1.8

		if (T[16] <  TENV):  Data.I_LIQUID_LINE = 1

		#WRITE (IO_Cycle,1010)
		objCycOut.write_or_terminate (",STATE, T(C), T(C), P, H, V, S, XL, XV, XQ")
		objCycOut.write_or_terminate (",,AIR,   REF, (KPa), (KJ/KG), (M3/KG), (KJ/KG-C), (MASS FRAC),(MASS FRAC),(MASS FRAC)")
		print (",STATE, T(C),T(C), P, H, V, S, XL, XV, XQ")
		print (",,AIR,   REF, (KPA), (KJ/KG), (M3/KG), (KJ/KG-C), (MASS FRAC),(MASS FRAC),(MASS FRAC)")

		K = 1
		if (ICYCL  ==  2) :
			while (K <= 15): #DO WHILE (K  <=  15)
				J = LPNT[K]
				TS[J] = TS[J] - 273.11
				T[J] = T[J] - 273.11
				V[J] = V[J]/10.0

				if (XQ[J] >  1.0): XQ[J] = 1.0
				if (XQ[J] <  0.0): XQ[J] = 0.0
				
				if (AIRTMP[K]) :
					#WRITE (8,1020) K,HSTATE(K),TS[J],T[J],P[J],H[J],V[J],'
					objCycOut.write_or_terminate ( "%d, %s , %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,HSTATE[K],TS[J],T[J],P[J],H[J],V[J],  S[J],XL[1][J],XV[1][J],XQ[J]) )
					print ( "%d, %d, %s , %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,J,HSTATE[K],TS[J],T[J],P[J],H[J],V[J],  S[J],XL[1][J],XV[1][J],XQ[J]) )
				else:
					#'WRITE (8,1021) K,HSTATE(K),		T[J],P[J],H[J],V[J],') print ('S[J],XL(1,J),XV(1,J),XQ[J]')
					objCycOut.write_or_terminate ( " %d, %s,  N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,HSTATE[K], T[J],P[J],H[J],V[J],  S[J],XL[1][J],XV[1][J],XQ[J]) )
					print ( "%d,%d, %s , N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,J,HSTATE[K], T[J],P[J],H[J],V[J],  S[J],XL[1][J],XV[1][J],XQ[J]) )
					
				# End if
				K = K + 1
			#END DO
		else:
			M = 0
			K = 0
			while (M <= 14): #DO WHILE (M  <=  14)
				M = M + 1
				J = LPNT[M]

				TS[J] = TS[J] - 273.11
				T[J] = T[J] - 273.11
				V[J] = V[J]/10.0
				if (XQ[J] >  1.0): XQ[J] = 1.0
				if (XQ[J] <  0.0): XQ[J] = 0.0

				if (M  >=  9  and  M  <=  11): continue
				K = K + 1
				if ( AIRTMP[M] ) :
					#WRITE (8,1020) K,MSTATE(K),TS[J],T[J],P[J],H[J],V[J],'  'S[J],XL(1,J),XV(1,J),XQ[J]
					objCycOut.write_or_terminate ( "%d, %s, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,MSTATE[K], TS[J],T[J],P[J],H[J],V[J],  S[J],XL[1][J],XV[1][J],XQ[J]) )
						
					print ("%d,%d, %s , %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,J, MSTATE[K], TS[J],T[J],P[J],H[J],V[J],  S[J],XL[1][J],XV[1][J],XQ[J]) )
				else:
					
					#WRITE (8,1021) K,MSTATE(K),		T[J],P[J],H[J],V[J],'\	'S[J],XL(1,J),XV(1,J),XQ[J]
					objCycOut.write_or_terminate ( "%d, %s , N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K, MSTATE[K],  T[J],P[J],H[J],V[J],  S[J],XL[1][J],XV[1][J],XQ[J]) )
					print ("%d,%d, %s , N/A, %9.3f, %9.3f, %9.3f, %9.3f, %9.3f, %s, %s, %s"	\
						%(K,J, MSTATE[K], T[J],P[J],H[J],V[J],  S[J],XL[1][J],XV[1][J],XQ[J]) )						
						
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
		FSUPE = 100. * FSUPE
		Data.ETAF = 100. * Data.ETAF
		Data.ETAC = 100. * Data.ETAC
		FSUBC = 100. * FSUBC
		FSUPC = 100. * FSUPC
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

		if (IRFTYP  <=  3  and  ICYCL  ==  1  and  ICAB  !=  0
								and  Data.IFRSH  !=  0):
			#WRITE(8,1055) FF_FRACT
			objCycOut.write_or_terminate  ('FRACTION AIR TO FRESH FOOD,     %9.3f, (SINGLE EVAPORATOR CYCLE)' %( FF_FRACT) )

		#write(8, '( )')
		objCycOut.write_or_terminate (" ")
		
		if (Data.IMAP  ==  1) :
			objCycOut.write_or_terminate  ('ESTIMATED COMPRESSION EFFICIENCY, %9.3f, (COMPRESSOR EER MODEL)' %( ETAS) )
			objCycOut.write_or_terminate  ('ESTIMATED MOTOR-PUMP EFFICIENCY,  %9.3f, (COMPRESSOR EER MODEL)' %( Data.EFFC/ETAS) )
			objCycOut.write_or_terminate  ('ESTIMATED CLEARANCE VOLUME,       %9.3f, (COMPRESSOR EER MODEL)' %( Data.CE) )
			objCycOut.write_or_terminate  ('ESTIMATED SHELL LOSS,       %9.3f, (COMPRESSOR EER MODEL)' %( QCAN) )
			objCycOut.write_or_terminate  ('ESTIMATED DISC TUBE HEAT LOSS,       %9.3f, (COMPRESSOR EER MODEL)' %( QHILO) )
			
			#WRITE(8,2215) ETAS
			#WRITE(8,2211) Data.EFFC/ETAS
			#WRITE(8,2212) Data.CE
			#WRITE(8,2213) QCAN
			#WRITE(8,2214) QHILO
		# End if

		objCycOut.write_or_terminate ("HEAT EXCHANGER PERFORMANCE SUMMARY")
		objCycOut.write_or_terminate ("EXCHANGER, EFFECTIVENESS, SUBCOOLED FRACTION, SUPERHEATED FRACTION")
		#WRITE(8,1060)
		#WRITE(8,1065)

		if (Data.ITYPE  ==  1) :
			if (Data.IFRSH  !=  0) :
				#WRITE(8,1110) Data.ETAE,FSUPE
				objCycOut.write_or_terminate  ('EVAPORATOR,     %9.3f, N/A, ,%9.3f' %(Data.ETAE,FSUPE ) )
			else:
				#WRITE(8,1111) FSUPE
				objCycOut.write_or_terminate  ('EVAPORATOR,     , N/A, N/A, %9.3f ' %(FSUPE ) )
		else:
			if (Data.IFRSH  !=  0) :
				#WRITE(8,1070) Data.ETAE,FSUPE
				objCycOut.write_or_terminate  ('FRESH FOOD EVAP.,     %9.3f, N/A, %9.3f ' %(Data.ETAE,FSUPE ) )
			else:
				#WRITE(8,1071) FSUPE
				objCycOut.write_or_terminate  ('FRESH FOOD EVAP.,  , N/A, N/A, %9.3f ' %(FSUPE ) )
				
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
			objCycOut.write_or_terminate  ('CONDENSER,     %9.3f,%9.3f, %9.3f ' %(Data.ETAC,FSUBC,FSUPC ) )
		else:
			#WRITE(8,1081) FSUBC,FSUPC
			objCycOut.write_or_terminate  ('CONDENSER,     , N/A, %9.3f,%9.3f ' %(FSUBC,FSUPC ) )
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
		#CALL OUTPUT(ICYCL,T,Data.W,QC,Data.QE,Data.QZ)
		self.showError (" Check cycle fig number ...")
		#
		#	return TO CALLER
		#
		#WRITE(Data.IM_Err,'(''LEAVING CYCLE WITH IC: '',I5,''  Data.IE: '',I5)') Data.IC, Data.IE")
		self.showMsg("LEAVING CYCLE WITH IC:" + str(Data.IC) + str(Data.IE) )
		
		#CLOSE(IO_Cycle)
		#CLOSE(IM_Err)
		
		return
	#
	#	OUTPUT FORMATS
	#
		'''
		1010 FORMAT(
				2X,'STATE			T (C)		P		H		V		S',
					'		XL	XV	XQ'/
				14X,'  AIR	REF	(KPA) (KJ/KG)  (M3/KG) (KJ/KG-C)',
					' (MASS FRAC)'/)
		1020 FORMAT (1X,I2,1X,A8,2X,2(F5.1,2X),F6.1,2X,F6.1,2X,1PF7.4,
				2X,0PF6.3,2(2X,F5.3),1X,F6.3)
		1021 FORMAT (1X,I2,1X,A8,4X,'N/A',F7.1,2X,F6.1,2X,F6.1,2X,1PF7.4,
				2X,0PF6.3,2(2X,F5.3),1X,F6.3)
		1025 FORMAT(1X,/,25X,'CYCLE PERFORMANCE SUMMARY'/)
		1030 FORMAT(1X,'FRESH FOOD EVAPORATOR CAPACITY --- ',F6.1,' KJ/HR',
						'	(',F5.1,' Data.W)')
		1035 FORMAT(1X,'FREEZER EVAPORATOR CAPACITY ------ ',F6.1,' KJ/HR',
						'	(',F5.1,' Data.W)')
		1040 FORMAT(1X,'CONDENSER HEAT REJECTION RATE ---- ',F6.1,' KJ/HR',
						'	(',F5.1,' Data.W)')
		1045 FORMAT(1X,'COMPRESSOR POWER REQUIREMENT ----- ',F6.1,' KJ/HR',
						'	(',F5.1,' Data.W)')
		1050 FORMAT(1X,'COEFFICIENT OF PERFORMANCE ------- ',F6.3)
		1055 FORMAT(1X,'FRACTION AIR TO FRESH FOOD -------  ',F5.3,
					'  (SINGLE EVAPORATOR CYCLE)')
		1060 FORMAT(25X,'HEAT EXCHANGER PERFORMANCE SUMMARY',/)
		1065 FORMAT(3X,'EXCHANGER',3X,'EFFECTIVENESS, %',3X,
						'SUBCOOLED FRACTION, %', 3X,'SUPERHEATED FRACTION, %',/)
		1070 FORMAT(1X,'FRESH FOOD EVAP. --- ',F5.1,14X'  N/A',20X,F5.1)
		1071 FORMAT(1X,'FRESH FOOD EVAP. --- ',2X,'N/A',14X'  N/A',20X,F5.1)
		1075 FORMAT(1X,'FREEZER EVAP. ------ ',F5.1,14X,'  N/A',20X,'-----')
		1076 FORMAT(1X,'FREEZER EVAP. ------ ',2X,'N/A',15X,' N/A',20X,'-----')
		1080 FORMAT(1X,'CONDENSER ---------- ',F5.1,14X,F5.1,20X,F5.1,/)
		1081 FORMAT(1X,'CONDENSER ---------- ',2X,'N/A',14X,F5.1,20X,F5.1,/)
		1085 FORMAT(25X,'COMPRESSOR PERFORMANCE SUMMARY',/)
		1090 FORMAT(1X,'REFRIGERANT MASS FLOW RATE ----- ',F6.3,' KG/HR')
		1091 FORMAT(1X,'VOLUMETRIC EFFICIENCY ----------  ',F5.3)
		1092 FORMAT(1X,'PRESSURE RATIO_Cycle -----------------  ',F5.2)
		1093 FORMAT(1X,'SUCTION PORT TEMPERATURE -------  ',F5.1,' C')
		1094 FORMAT(1X,'DISCHARGE PORT TEMPERATURE -----  ',F5.1,' C')
		1095 FORMAT(1X,'DISCHARGE SUPERHEAT ------------  ',F5.1,' C')
		1097 FORMAT(1X,'THE REFRIGERANT MIXTURE CONSISTS OF ',F4.0,'%',1X,A6)
		1098 FORMAT(1X,'												',F4.0,'%',1X,A6)
		1099 FORMAT(1X,/)
		1100 FORMAT(35X,'OUTPUT RESULTS'//)
		1105 FORMAT(1X,'EVAPORATOR CAPACITY--------------- ',F6.1,' KJ/HR',
						'	(',F5.1,'W)')
		1110 FORMAT(1X,'EVAPORATOR --------- ',F5.1,14X'  N/A',20X,F5.1)
		1111 FORMAT(1X,'EVAPORATOR --------- ',2X,'N/A',14X'  N/A',20X,F5.1)
		1200 FORMAT(50X,'RUN AT ',A2,':',A2,':',A2,' ON ',A2,1X,A3,I5/)
		2200 FORMAT (/1X,' *  *  *  EVAPORATOR ITERATION DID NOT CONVERGE  *  *  * ',
						3F9.3/)
		2202 FORMAT (/1X,' *  *  *  CONDENSER ITERATION DID NOT CONVERGE  *  *  * ',
						3F9.3/)
		2203 FORMAT (/1X,' *  *  *  INTERCHANGER SUPERHEAT NOT POSSIBLE  *  *  * ',
						3F9.3/)
		2204 FORMAT (/1X,' *  *  *  CRITICAL TEMPERATURE EXCEEDED IN CONDENSER  *  *  * ')
		#2210 FORMAT(1X,'ISENTROPIC EFFICIENCY OVER SHELL - ',F6.3,/)
		2211 FORMAT(1X,'ESTIMATED MOTOR-PUMP EFFICIENCY  - ',F6.3,
						'  (COMPRESSOR EER MODEL)')
		2212 FORMAT(1X,'ESTIMATED CLEARANCE VOLUME		- ',F6.3,
						'  (COMPRESSOR EER MODEL)')
		2213 FORMAT(1X,'ESTIMATED SHELL LOSS				- ',F6.3,
						'  (COMPRESSOR EER MODEL)')
		2214 FORMAT(1X,'ESTIMATED DISC TUBE HEAT LOSS	- ',F6.3,
						'  (COMPRESSOR EER MODEL)'/)
		2215 FORMAT(1X,'ESTIMATED COMPRESSION EFFICIENCY - ',F6.3,
						'  (COMPRESSOR EER MODEL)')
		'''

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
		#	QFF,QFZ,TS3,TS5,   T,IDFRST, DUTYR)
		# output    QFF,QFZ,DUTYR
		#	*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
		#	* 	CALCULATE DUTY CYCLE AND THE AVERAGE CABINET LOADS			*
		#	*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
		#
		#DIMENSION T[16]
		#COMMON /CABLOD/ FFASH,FAUXF,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
		#	FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
		#	FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
		#COMMON /FANS/ FANE,FANZ,FANC,DUTYC,W,COPR
		#COMMON /LORENZ/ DUTYE,DUTYZ,PWRL,PWRE,CAPE,CAPZ,DUTYL,DUTYS,
		#	FANEL,FANCL,FANES,FANCS
		#COMMON / CHINA / INCTRL
		#COMMON / INWALL / UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
		#	Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
		#	CAPZ_IN_WALL, Q_FZ_FF
		#COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
		#	Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
		#	CONDF_IN_WALL, CONDZ_IN_WALL
		#COMMON / LIQLIN / FFREFQ, FZREFQ, CONDHT[2], CONDVP[2]
		#COMMON / CYCLIC / DFSTCYC, FFCYC, FZCYC, OUTCYC
		#
		#	return if NO CABINET LOADS
		#

		if (ICAB  ==  0): return
		#
		#	CALCULATE IN-WALL HEAT LOADS
		#
		TENV = (Data.TROOM + 459.6)/1.8
		TCND = 0.2 * T[14] + 0.4 * T[3] + 0.4 * T[11]

		if (TS5 >  -300.0) :									#Freezer evaporator
			TRFZ = (T[8] + T[9])/2.0
			Data.Q_FZ_IN_WALL = 1.8 * Data.UA_FZ * (TENV - TS5)
			Data.Q_ML_IN_WALL = 1.8 * Data.UA_ML * (TS3  - TS5)

			Data.CAPZ_IN_WALL = 1.8 * Data.UA_FZ * (TENV - TRFZ)
			Data.CAPM_IN_WALL = 1.8 * Data.UA_ML * (TS3  - TRFZ)

			Data.Q_FZ_FF = 1.8 * Data.UA_ML * (TS5 - TRFZ)
		else:
			Data.Q_FZ_IN_WALL = 0
			Data.Q_ML_IN_WALL = 0

			Data.CAPZ_IN_WALL = 0
			Data.CAPM_IN_WALL = 0

			Data.Q_FZ_FF = 0
		# End if

		TRFF = (T[5] + T[7])/2.0
		Data.Q_FF_IN_WALL = 1.8 * Data.UA_FF * (TENV - TS3)
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
				if (IDFRST  ==  0) :
					QFF = QFF + FROSTF
					QFZ = QFZ + FROSTZ
				# End if

				Data.CAPE = QFRSH/1.0548 - 3.413 * Data.FANE - 3.413 * Data.DFSTCYC	\
					- 3.413 * Data.FFCYC	- 3.413 * Data.FZCYC	\
					- Data.CONDF_IN_WALL - Data.CONDZ_IN_WALL

				Data.DUTYC = (QFF + QFZ)/Data.CAPE
				if (Data.DUTYC >  1.0): Data.DUTYC = 1.0
				DUTYR = Data.DUTYC
			# End if

			if (ICYCL  ==  2) :
				QFF = QFF - FROSTF
				if (IDFRST  ==  0): QFZ = QFZ + FROSTZ

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
					if (IDFRST  ==  0): QFZ = QFZ + FROSTZ

					Data.CAPZ = QFRSH/1.0548 - 3.413 * Data.FANE	- 3.413 * (Data.DFSTCYC + Data.FZCYC)
					Data.DUTYZ = QFZ/Data.CAPZ

					Data.DUTYC = min(Data.DUTYZ,1.0)
					Data.DUTYZ = Data.DUTYC

				else:
					Data.CAPE = QFRSH/1.0548 - 3.413 * (Data.FANE + Data.FFCYC) + Data.Q_FF_IN_WALL - Data.CAPE_IN_WALL
					QFF = QFF - FROSTF
					Data.DUTYE = QFF/Data.CAPE
					Data.DUTYC = min(Data.DUTYE,1.0)
					Data.DUTYE = Data.DUTYC
				# End if

				DUTYR = Data.DUTYC
			# End if
			else :
				#CASE DEFAULT					!One door type units
				if (IDFRST  ==  0): QFZ = QFZ + FROSTZ
				Data.CAPE = QFRSH/1.0548 - 3.413 * (Data.FANE + Data.DFSTCYC + Data.FZCYC)	\
					+ Data.Q_FF_IN_WALL - Data.CAPE_IN_WALL						\
					- Data.CONDF_IN_WALL - Data.Q_HXS_FF/1.0548

				Data.DUTYE = QFZ/Data.CAPE
				Data.DUTYC = min(Data.DUTYE,1.0)
				DUTYR = Data.DUTYC

		#END SELECT
		return [QFF,QFZ,DUTYR]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def exf (self, LOC, AREA, U, CMIN, CMAX):
		# [P6, P7] = self.exf (P1 ... P5)
		#	SUBROUTINE EXF(LOC, AREA, U, CMIN, CMAX, EFF, DEFFDA)
		#     ******************************************************************
		#     *    CALCULATE COUNTER FLOW EFFICIENCY PARAMETERS                *
		#     ******************************************************************
		#
		#	REAL NTU

		#	DIMENSION coff_A(4,6), EFF_CROSS(2)
		#
		#	DATA (coff_A(I,1),I=1,4)/2.394292,2.410798,2.399687,2.359642/
		#	DATA (coff_A(I,2),I=1,4)/-1.19402,-2.23391,-2.96882,-3.37650/
		#	DATA (coff_A(I,3),I=1,4)/-1.45067,0.825900,2.367080,3.04862/

		#	DATA (coff_A(I,4),I=1,4)/1.938453,0.051006,-1.23009,-1.63421/
		#	DATA (coff_A(I,5),I=1,4)/-0.81305,-0.11891,0.373338,0.468741/
		#	DATA (coff_A(I,6),I=1,4)/0.118651,0.023360,-0.04886,-0.05492/
		#
		#          CALCULATE NTU AND CAPACITY RATIO
		#
		EFF_CROSS = [0.0] * (2+1)
		coff_A = [	\
			[2.394292, -1.19402,	-1.45067,	1.938453,	-0.81305,	0.118651],	\
			[2.410798, -2.23391,	0.825900,	0.051006,	-0.11891,	0.023360],	\
			[2.399687, -2.96882,	2.367080,	-1.23009,	0.373338,	-0.04886],	\
			[2.359642, -3.37650,	3.048620,	-1.63421,	0.468741,	-0.05492]	\
			]

		NTU = AREA*U/CMIN
		CRAT = CMIN/CMAX

		if LOC == 1 :		 #Counter-flow
			XX = 1.0 - CRAT
			XXX = math.exp(-NTU*XX)
			EFF = (1.0 - XXX)/(1.0 - CRAT*XXX)
			DEFFDA = (U/CMIN)*XX*XXX*(1.0 - CRAT*EFF)/(1.0 - CRAT*XXX)
		
		int_row = 0
		if LOC == 2 :			#Cross-flow
			if (CRAT  >=  0.00  and  CRAT <=  0.25): int_row = 1
			if (CRAT  >   0.25  and  CRAT <=  0.50): int_row = 2
			if (CRAT  >   0.50  and  CRAT <=  0.75): int_row = 3
			if (CRAT  >   0.75  and  CRAT <=  1.00): int_row = 4

			if (NTU <=  0.0): NTU = 0.0
			
			for L in range (1, 2+1): #DO L = 1, 2
				BETA = math.log10(NTU+1.0)
				EFFA = 0.0
				EFFB = 0.0

				for J in range (1, 6+1): # DO J = 1, 6
					EX = 1.0*J
					if (int_row  ==  1):
						EFFA = 1.0 - math.exp(-NTU)
					else:
						EFFA = EFFA + coff_A[int_row-1-1][J-1] * BETA**EX
					#END if
					EFFB = EFFB + coff_A[int_row -1][J-1] * BETA**EX
				#END DO

				FRAC = (CRAT-(int_row-1)*0.25)/(int_row *0.25-(int_row-1)*0.25)
				EFFECT = EFFA + FRAC*(EFFB-EFFA)
				
				if (EFFECT  >  1.0): EFFECT = 1.0

				EFF_CROSS[L] = EFFECT
				NTU = 0.9*NTU
			#END DO
			EFF = EFF_CROSS[1]
			DEFFDA = 10.0*(EFF_CROSS[1] - EFF_CROSS[2])/AREA

		if(DEFFDA <=  0.0): DEFFDA = 0.0001
		return [EFF, DEFFDA]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
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
			[T,XCALC,XL,XV,VL,V,HL,HV] = self.hpin ( HGUESS,P,X )	#CALL HPIN(HGUESS,P,X,  T,XCALC,XL,XV,VL,V,HL,HV)

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
	def showMsg (self, strMsg, fltValue=0.0, isOnlyTest = False):
		return
		if isOnlyTest :
			print ("\t ", strMsg)
		else:
			#print('{}\t\t{}'.format(strMsg, fltValue))
			print ("\t ", strMsg + "\t\t %10.3f"  %(fltValue))	