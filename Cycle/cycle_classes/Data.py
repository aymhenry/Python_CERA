#-----------------------------------------------------------
# Job 			: 
#
# Editor		: aymhenry@gmail.com
#-----------------------------------------------------------
#--
#--   THIS ROUTINE INITIALIZES THE COMMON BLOCKS CONTAINING INFORMATION
#--   ABOUT THE PURE COMPONENTS.  IT IS NOT REFERENCED DIRECTLY BY ANY
#--   OTHER SUBROUTINE BUT MUST BE INCLUDED IN THE EXECUTABLE ELEMENT.
#--   DATA ARRAYS ARE DIMENSIONED TO ACCOMODATE ADDITIONAL
#--   PURE COMPONENTS.
#--
#--   EXPLANATION OF CONSTANTS:
#--      COEFF(I,J) - FOR REFRIGERANT J, COEFFICIENTS OF A, B, CP0
#--         CURVE FITS:
#--         A = A0 * EXP(A1*T + A2*T*T)  (KJ M**3/KMOL**2)
#--         B = B0 + B1*T + B2*T*T  (M**3/KMOL)
#--         CP0 = C0 + C1*T + C2*T*T (KJ/KMOL K)
#--         (STORED IN ORDER A0,A1,A2,B0,B1,B2,C0,C1,C2)
#--      CRIT(I,J) - FOLLOWING INFORMATION FOR REFRIGERANT J:
#--         I = 1 - MOLECULAR WEIGHT
#--             2 - REFERENCE TEMPERATURE FOR ENTHALPY AND ENTROPY (K)
#--             3 - CRITICAL TEMPERATURE (K)
#--             4 - CRITICAL PRESSURE (KPA)
#--             5 - CRITICAL VOLUME (M**3/KMOL)
#--      HREF(J) - REFRIGERANT NAME (ASHRAE DESIGNATION)
#--      HZERO(J) - VALUE OF SATURATED LIQUID ENTHALPY OF REFRIGERANT
#--         J AT ITS REFERENCE TEMPERATURE (KJ/KMOL)
#--      SZERO(J) - VALUE OF SATURATED LIQUID ENTROPY AT REFERENCE
#--         TEMPERATURE (KJ/KMOL K)
#--      R - GAS CONSTANT (KJ/KMOL K)
#--      TOLR - RELATIVE CONVERGENCE TOLERANCE FOR ITERATION LOOPS
#--         SHOULD BE AT LEAST 10 TIMES LARGER THAN MACHINE PRECISION
#--      ITMAX - MAXIMUM ITERATION COUNT FOR ITERATIVE LOOPS
#--      LUP - LOGICAL UNIT TO WHICH ANY WARNING MESSAGES ARE WRITTEN
#--
#--   A, B COEFFFICIENTS EVALUATED FROM ASHRAE (1981) SATURATION
#--   DATA UNLESS INDICATED.
#--
#--   DATA VALUES UPDATED ON 3/15/94 TO BE CONSISTENT WITH REFPROP 4.0
#--
#--   REFRIGERANTS NOW ARE:
#--       1: R11     2: R12     3: R13     4: n-C5     5: R14     6: R22
#--       7: R23     8: R113    9: R114   10: R142B   11: R152A  12: R216A
#--      13: R125   14: R143A  15: R134A  16: R123    17: RC318  18: R134
#--      19: RC270  20: R141B  21: i-C5   22: R290    23: R600   24: R600A
#--      25: R32    26: R1270  27: R124   28: R115    29: CE-216 30: E-125
#--      31: R123A  32: R143   33: R218   34: E134
#--
#--  NOTE: REFPROP 4.0 ALLOWS USE OF A MODIFIED B-W-R EQUATION OF STATE FOR
#--        R32, R123, R124, R125, AND R134A AS PURE FULIDS.  THE CSD EQUATION
#--        OF STATE IS USED BY REFPROP FOR MIXTURES.
#--
#--  NOTE: THE FOLLOWING REFRIGERANTS WERE FIT WITH THE OLDER REFPROP (3.0)
#--        COEFFICIENTS (BECAUSE OF A BETTER MATCH TO ASHRAE DATA OVER THE
#--        -20 F TO + 130 F TEMPERATURE RANGE): R114
#--
#--  NOTE: THE COEFFICIENTS FOR R12 ARE THE SAME AS USED IN REFPROP 3.O TO
#--        PROVIDE CONSISTENCY WITH EARLIER ANALYSES.  THE REFPROP 4.0
#--        COEFFICIENTS CHANGE THE ENERGY BY ABOUT 0.001 KWH/DAY.  IT IS NOT
#--        WORTH EXPLAINING AWAY DIFFERENCES BETWEEN THE RESULTS IN THE ERA
#--        DOCUMENTATION AND A REVISED ERA USING THE REFPROP 4.O COEFFICIENTS
#--        FOR R12.


class Data:
	
	#=======================================================
	# Set Static Vars
	#=======================================================
	
	#-- In Python Only Data for condenser iteration-----------
	#MROLD = 0.0
	#ICNT = 0.0
	
	#-- Save group ---------------------------------
	TNEG = 9999.0
	TPOS = -999.0
	#-- Common HREF1 group ---------------------------------
	HREF = [] #[[" "] * (34+1)]		# CHARACTER*6 HREF(34),REFH(34)
	REFH = [] #[[" "] * (34+1)]
	
	#-- Common TOLSH group ----------------------------------
	TOLH = 0.010
	TOLS = 0.001
	
	#-- Common TOL group ----------------------------------
	TOLR  = 1.0E-7*10 # SHOULD BE AT LEAST 10 TIMES LARGER THAN MACHINE PRECISION
	ITMAX = 20
	LUP   = 9
	  
	#-- Common RDATA4 group --------------------------------
	R = 8.314
	
	#-- Common NCOMP group ---------------------------------
	NC = 0	# integer
	
	#-- Common HSPURE group ---------------------------------
	HP = [0.0] * (5+1)
	SP = [0.0] * (5+1)
	CP = [0.0] * (5+1)

	#-- Common HSZERO group ---------------------------------
	HZERO = [0.0] * (34+1)
	SZERO = [0.0] * (34+1)

	#-- Common CPDATA group ---------------------------------
	# Create zero base array
	C = [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]

	#-- Common RDATA1 group ---------------------------------
	# Create zero base array
	A = [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
	B = [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]

	#-- Common RDATA2 group ---------------------------------
	WM =  [0.0] * (5+1)
	TC =  [0.0] * (5+1)

	#-- Common ESDATA group ---------------------------------
	COEFF = []	
	CRIT  = [] 

	#-- Common REF group ---------------------------------
	TREF= [0.0] * (5+1)
	HR =  [0.0] * (5+1)
	SR =  [0.0] * (5+1)
	VR =  [0.0] * (5+1)

	#-- Common ESPAR1 group ---------------------------------
	F  =  [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
	AP =  [0.0] * (5+1)
	BP =  [0.0] * (5+1)
	DADT = 0.0; DBDT = 0.0; D2ADT= 0.0; D2BDT = 0.0
	
	# --- GROUP CONDEN ------ 
	UDSC = 0.0
	UTPC = 0.0
	USCC = 0.0
	ATOTC = 0.0
	UACOND = 0.0
	# --- GROUP SPECS ------ 
	DTSUPE = 0.0
	DTSUBC = 0.0
	# --- GROUP SPECS2 ------ 
	ISPEC = 0
	XEXITE = 0.0
	DTSUPI = 0.0
	# --- GROUP EVAPS ------ 
	ITYPE = 0
	FRACT_FF = 0.0
	FRACT_FZ = 0.0
	# --- GROUP CABLOD ------ 
	FFASH = 0.0
	FFAUX = 0.0
	FZASH = 0.0
	FZAUX = 0.0
	TROOM = 0.0
	FFTEMP = 0.0
	OTHERW = 0.0
	FZTEMP = 0.0
	FFQ = 0.0
	FZQON = 0.0
	FZQOFF = 0.0
	FFLAT = 0.0
	FZLAT = 0.0
	FFSEN = 0.0
	FZSEN = 0.0
	FFHTQ = 0.0
	FZHTQ = 0.0
	CONDF = 0.0
	CONDZ = 0.0
	QMUL = 0.0
	
	# --- GROUP CNDWAL ------ 
	UA_FF_CND = 0.0
	UA_FZ_CND = 0.0
	UA_FF_HXS = 0.0
	UA_FZ_HXS = 0.0
	Q_CND_FF = 0.0
	Q_CND_FZ = 0.0
	Q_HXS_FF = 0.0
	Q_HXS_FZ = 0.0
	
	# --- GROUP LIQLIN ------ 
	FFREFQ = 0.0
	FZREFQ = 0.0
	
	CONDHT = [0.0] * (2+1)
	CONDVP = [0.0] * (2+1)

	# --- GROUP CYCLIC ------ 
	DFSTCYC = 0.0
	FFCYC = 0.0
	FZCYC = 0.0
	OUTCYC = 0.0
	
	# --- GROUP MAPDAT ------ 
	IMAP = 0
	ICOMP = 0
	ICOOL = 0
	EER = 0.0
	SIZE = 0.0
	DISPL = 0.0
	EFFC = 0.0
	SPEEDN = 0.0
	IREAD = 0
	
	# --- GROUP TLRNCE ------ 
	TOL_COND = 0.0
	TOL_MASS = 0.0
	TOL_FRSH = 0.1 # data given from Eracyc line 95
	TOL_FRZ = 0.0
	TOL_HX = 0.0
	N_EVAP = 0
	N_COND = 0
	
	# --- GROUP PLSTIC ------ 
	IWALL_FF = 0
	IWALL_FZ = 0
	
	# --- GROUP MAPNAM ------ 
	FILMAP = 0.0
	FILMAP1 = ""
	FILMAP2 = ""
	
	# --- GROUP PENAT ------ 
	FFPENA = 0.0
	FZPENA = 0.0
	
	# --- GROUP REFRIG ------
	#G:\Ayman_APP\Fortran\Source\Cycle\Eracyc.for  
	#Line 45:       COMMON /REFRIG/ NC(2), IR(5,2), X(5,2), F(5,5,2)
	#Line 1085:       COMMON /REFRIG/ NC(2), IR(5,2), X(5,2), F(5,5,2)
	
	# F(5,5,2)
	# Canceled shared with local group REFRIG
	#F =  [  [[0.0] * (2+1) for i in range(5+1)] for j in range(5+1)  ]
	
	pythNC = [0.0] * (2+1)
	##  X(5,2)  
	X  =  [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
	
	## IR(5,2) 
	IR  =  [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
	## F(5,5,2)
	
	# --- GROUP FILINF ------ 
	FILERA = 0.0
	# --- GROUP CHINA ------ 
	INCTRL = 0
	HRSOFF = 0.0
	# --- GROUP BALNCE ------ 
	IBLNCE = 0
	BAFFLF = 0.0
	BAFFLZ = 0.0
	AREAFZ = 0.0
	ATOTE_S = 0.0
	AREAFZ_S = 0.0
	ATOTE_A = 0.0
	AREAFZ_A = 0.0
	FFTEMP_A = 0.0
	FZTEMP_A = 0.0
	# --- GROUP INWALL ------ 
	UA_FZ = 0.0
	UA_FF = 0.0
	UA_ML = 0.0
	Q_FZ_IN_WALL = 0.0
	Q_FF_IN_WALL = 0.0
	Q_ML_IN_WALL = 0.0
	CAPE_IN_WALL = 0.0
	CAPM_IN_WALL = 0.0
	CAPZ_IN_WALL = 0.0
	Q_FZ_FF = 0.0
	CONDF_IN_WALL = 0.0
	CONDZ_IN_WALL = 0.0
	# --- GROUP FANS ------ 
	FANE = 0.0
	FANZ = 0.0
	FANC = 0.0
	DUTYC = 0.0
	W = 0.0
	COPR = 0.0
	# --- GROUP LORENZ ------ 
	DUTYE = 0.0
	DUTYZ = 0.0
	PWRL = 0.0
	PWRE = 0.0
	CAPE = 0.0
	CAPZ = 0.0
	DUTYL = 0.0
	DUTYS = 0.0
	FANEL = 0.0
	FANCL = 0.0
	FANES = 0.0
	FANCS = 0.0
	# --- GROUP FIGURE ------ 
	IEVAP = 0
	# --- GROUP RESULT ------ 
	QE = 0.0
	QZ = 0.0
	FLOW = 0.0
	
	QEN = [0.0] * (2+1)
	FLOWN = [0.0] * (2+1)
	COPRN = [0.0] * (2+1)
	
	# --- GROUP CYCLNG ------ 
	CORR_COP = 0.0
	COPCYC = [0.0] * (2+1)
	
	I_CYCLE = 0
	I_VALVE = 0
	T_CYCLE = 0.0

	# --- GROUP PARMS ------ 
	ICOND = 0
	IFRSH = 0
	IFREZ = 0
	DISP = 0.0
	SPEED = 0.0
	CE = 0.0
	CREF = 0.0
	#MDOTR = 0.0  use MREF
	MREF = 0
	ETAV = 0.0
	SEFF = 0.0
	# --- GROUP PARMS2 ------ 
	TSPEC = 0.0
	I_LIQUID_LINE = 0
	# --- GROUP HTEXS ------ 
	CFMC = 0.0
	CFME = 0.0
	CFMF = 0.0
	UAF = 0.0
	ETAC = 0.0
	ETAE = 0.0
	ETAF = 0.0
	# --- GROUP FEVAP ------ 
	UTPE = 0.0
	USUPE = 0.0
	ATOTE = 0.0
	FF_AIR = 0.0
	UAFF = 0.0
	UAFZ = 0.0
	
	# --- Group TIME ------ 
	#CHOUR = 0.0
	#CMIN = 0.0
	#CSEC = 0.0
	#CDAY = 0.0
	#CMON = 0.0
	#CMONN = 0.0
	#IYEAR = 0
	#CYEAR = 0.0
	
	# --- Group DIAG ------ 
	IM = 0
	IC = 0
	IE = 0

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	@staticmethod
	def setup():
		# Ref 00
		#   add one more Col and Row, to keep FORTRAN none zero Ref.
		Data.HREF.append  (" ")
		Data.REFH.append  (" ")
		
		#-- zero based Data.CRIT.append  ([0.0] * 5)
		#-- zero based Data.COEFF.append ([0.0] * 9)
		#---------------------------------------
		# Ref 01
		#   R11, TRICHLOROFLUOROMETHANE (CFCL3)
		#
		Data.HREF.append ("R11")
		Data.REFH.append ("   R11")
		
		Data.CRIT.append  ([137.37, 296.91, 471.2, 4467.0, 0.247])
		
		Data.COEFF.append ([ 4967.07,  -2.23098E-3, -5.59203E-7,		\
					   0.178148,  -1.82363E-4, -2.54131E-8,	\
					    23.4805,     0.251072, -2.28722E-4 ])

		# Ref 02
		#  R12, DICHLORODIFLUOROMETHANE (CF2CL2)
		#
		Data.HREF.append ("R12")
		Data.REFH.append ("   R12")
		
		Data.CRIT.append ([120.91, 243.39, 384.95, 4180.0, 0.241]) # error last item fixed (was 0.181)
		Data.COEFF.append ([ 3819.88, -3.31988E-3,  2.41944E-7,	\
					        0.165350, -2.65758E-4,  9.13878E-8,	\
					         18.4874,    0.241782, -2.04594E-4])
		# Ref 03
		#   R13, CHLOROTRIFLUOROMETHANE (CF3CL)
		#
		Data.HREF.append ("R13")
		Data.REFH.append ("   R13")
		
		Data.CRIT.append ([104.46, 191.67, 302.0, 3870.0, 0.181])
		Data.COEFF.append([ 2157.20, -2.84478E-3, -2.75122E-6,	\
					  0.129485, -1.93746E-4, -9.01119E-8,	\
					   13.8691,    0.232370, -1.83095E-4 ])
		# Ref 04
		#  n-C5, n-Pentane (C5H12)
		#
		Data.HREF.append ("n-C5")
		Data.REFH.append ("   n-C5")
		Data.CRIT.append ([72.15, 309.34, 469.5, 3359.9, 0.295])
		Data.COEFF.append ([6745.80, -2.29793E-3, -0.70747E-6,	\
			          0.228716, -2.36350E-4, -0.32793E-7,	\
			           54.7577,    0.143042,  2.53720E-4 ])
		 
		# Ref 05
		#   R14, TETRAFLUOROMETHANE (CF4)
		#
		Data.HREF.append ("R14")
		Data.REFH.append ("   R14")
		Data.CRIT.append ([88.00, 145.17, 227.5, 3795.0, 0.141])
		Data.COEFF.append ([ 1272.41, -3.42946E-3, -6.47573E-6,	\
			           0.099664, -1.57113E-4, -2.95020E-7,	\
			            14.4296,    0.184530, -9.51890E-5 ])
		# Ref 06
		#   R22, CHLORODIFLUOROMETHANE (CHF2CL)
		#
		Data.HREF.append ("R22")
		Data.REFH.append ("   R22")
		Data.CRIT.append ( [86.47, 232.29, 369.3, 5054.0, 0.169 ] )

		Data.COEFF.append ([2624.62, -2.67304E-3, -1.33238E-6,	\
			          0.117395, -1.40272E-4, -0.52161E-7,	\
			           21.9839,    0.127744, -4.78872E-5 ] )
		# Ref 07
		#   R23, TRIFLUOROMETHANE (CHF3)
		#
		Data.HREF.append ("R23")
		Data.REFH.append ("   R23")
		Data.CRIT.append ( [70.01, 191.12, 299.1, 4900.0, 0.133 ] )

		Data.COEFF.append ( [ 1743.89, -3.52595E-3, -1.12774E-6,
			            0.090205, -1.25602E-4, -0.50675E-7,
			             23.6029,    0.082287,  3.18265E-5 ] )
		# Ref 08
		#   R113, 1,1,2-TRICHLOROTRIFLUOROETHANE (CF2CL-CFCL2)
		#
		Data.HREF.append ("R113")
		Data.REFH.append ("  R113")
		Data.CRIT.append ( [ 187.38, 320.80, 487.5, 3456.0, 0.329 ] )

		Data.COEFF.append ( [ 7284.48, -2.15870E-3, -8.03754E-7,		\
			            0.234712, -2.11131E-4, -7.33758E-8,		\
			             76.2637,    0.119641,  7.18786E-5 ] )
		# Ref 09
		#   R114, 1,2-DICHLOROTETRAFLUOROETHANE  (CF2CL-CF2CL): Version 3.0 Refprop
		#
		Data.HREF.append ("R114")
		Data.REFH.append ("  R114")
		Data.CRIT.append ( [ 170.92, 276.80, 418.80, 3248.0, 0.307 ] )

		Data.COEFF.append ( [ 5929.74, -2.86018E-3, -4.81520E-7,		\
			            0.221874, -2.88304E-4,  1.81892E-8,		\
			             37.2482,    0.337339, -2.39995E-4 ] )
		 
		# Ref 10
		#   R142B, 1-CHLORO-1,1-DIFLUOROETHANE (CF2CL-CH3)
		#
		Data.HREF.append ("R142B")
		Data.REFH.append (" R142B")
		Data.CRIT.append ( [100.49, 264.01, 410.3, 4120.0, 0.231 ] )

		Data.COEFF.append ( [ 4180.75, -2.73043E-3, -5.43638E-7,		\
			            0.169138, -2.41068E-4,  0.67566E-7,		\
			             16.3914,    0.271719, -1.58933E-4 ] )
		# Ref 11
		#   R152A, 1,1-DIFLUOROETHANE  (CHF2-CH3)
		#
		Data.HREF.append ("R152A")
		Data.REFH.append (" R152A")
		Data.CRIT.append ( [ 66.05, 248.50, 386.7, 4492.0, 0.181 ] )

		Data.COEFF.append ( [3198.63, -2.96134E-3,  -0.32190E-6,		\
			           0.133264, -2.03633E-4,  0.777251E-7,		\
			            22.2832,    0.153987, -3.015434E-6 ] )
		# Ref 12
		#   R216A, 1,3-DICHLOROHEXAFLUOROPROPANE [NOT IN REFPROP4.0]
		#
		Data.HREF.append ("R216A")
		Data.REFH.append (" R216A")
		Data.CRIT.append ( [ 220.93, 233.15, 453.14, 2754.1, 0.3847 ] )

		Data.COEFF.append ( [ 8431.44, -2.45916E-3, -9.91754E-7,		\
			            0.265720, -2.20418E-4, -1.68111E-7,		\
			             8.79769,    0.654246, -5.39923E-4 ] )
		# Ref 13
		#  R125, PENTAFLUOROETHANE (C2HF5)
		#
		Data.HREF.append ("R125")
		Data.REFH.append ("  R125")
		Data.CRIT.append ( [120.03, 224.6, 339.4, 3629.0, 0.2099 ] )

		Data.COEFF.append ( [ 3427.92, -3.17461E-3, -1.75729E-6,		\
			            0.149380, -1.80851E-4, -1.18813E-7,		\
			            22.65024,    0.295668, -1.69490E-4 ] )
		# Ref 14
		#   R143A, 1,1,1-TRIFLUOROETHANE   (CF3-CH3)
		#
		Data.HREF.append ("R143A")
		Data.REFH.append (" R143A")
		Data.CRIT.append ( [ 84.04, 225.8, 346.3, 3811., 0.194] )

		Data.COEFF.append ( [ 2763.90920, -2.509056E-3, -1.797108E-6,		\
			             0.133153E0, -1.589538E-4, -0.583311E-7,		\
			             13.89426E0,     .2554913, -1.300829E-4] )
		# Ref 15
		#   R134A:  1,1,1,2-TETRAFLUOROETHANE  (CF3-CH2F)
		#
		Data.HREF.append ("R134a")
		Data.REFH.append (" R134a")
		Data.CRIT.append ( [ 102.030, 247.0, 374.3, 4067.0, 0.199 ] )

		Data.COEFF.append ( [ 3582.17, -2.81114E-3, -1.44679E-6,		\
			            0.141750, -1.62763E-4, -.628933E-7,		\
			             19.4006,    0.258531, -1.29665E-4 ] )
		# Ref 16
		#   R123, 1,1-DICHLORO-2,2,2-TRIFLUOROETHANE (CHCL2-CF3)
		#
		Data.HREF.append ("R123")
		Data.REFH.append ("  R123")
		Data.CRIT.append ( [ 152.93, 301.02, 456.9, 3674.0, 0.278 ] )

		Data.COEFF.append ( [ 6033.29, -2.37891E-3, -0.84728E-6,		\
			            0.199549, -1.89493E-4, -0.67680E-7,		\
			             29.2604,    0.302994, -1.92907E-4 ] )
		 
		# Ref 17
		#  RC318, PERFLUOROCYCLOBUTANE (C4F8)
		#
		Data.HREF.append ("RC-318")
		Data.REFH.append ("RC-318")
		Data.CRIT.append ( [ 200.04, 266.1, 388.4, 2778., 0.3248 ] )

		Data.COEFF.append ( [  6182.614E0, -2.536687E-3, -2.265766E-6,		\
			              .2255416E0, -1.898425E-4, -2.635465E-7,		\
			             28.972075E0,     .5333363, -3.557726E-4] )
		# Ref 18
		#  R134, 1,1,2,2-TETRAFLOUROETHANE (CHF2-CHF2)
		#
		Data.HREF.append ("R134")
		Data.REFH.append ("  R134")
		Data.CRIT.append ( [ 102.03, 253.34, 392.1, 4562.0, 0.189 ] )

		Data.COEFF.append ( [ 3547.10, -2.68720E-3, -1.41578E-6,		\
			             0.13856,  -1.5991E-4, -0.55880E-7,		\
			             32.5208,    0.222819, -1.06829E-4 ] )
		# Ref 19
		#   RC270, CYCLOPROPANE (C3H6)
		#
		Data.HREF.append ("RC270")
		Data.REFH.append (" RC270")
		Data.CRIT.append ( [42.081, 240.25, 398.30, 5580.0, 0.194 ] )

		Data.COEFF.append ( [ 2745.00, -2.98122E-3,  1.64391E-7,		\
			            0.125065, -2.01031E-4,   7.8506E-8,		\
			             8.19470,     0.136885, 0.777583E-4 ] )
		# Ref 20
		#  R141b,  1,1-DICHLORO-1-FLUOROETHANE (CFCL2-CH3)
		#
		Data.HREF.append ("R141B")
		Data.REFH.append (" R141b")
		Data.CRIT.append ( [116.94, 305.35, 477.3, 4120., 0.217 ] )

		Data.COEFF.append ( [ 5422.38, -2.24167E-3, -6.04435E-7,		\
			            0.180853, -1.61856E-4, -6.23542E-8,		\
			             35.8434, 0.175268,     0.0 ] )
		# Ref 21
		#  i-C5 ISO-PENTANE (C4H9-CH3)
		#
		Data.HREF.append ("i-C5")
		Data.REFH.append ("  i-C5")
		Data.CRIT.append ( [ 72.150, 300.9, 460.51, 3370.7, 0.306 ] )

		Data.COEFF.append ( [ 6408.1, -2.3216E-3, -0.7087E-6,		\
			           0.227727, -2.4414E-4, -2.9694E-8,		\
			             12.216,    0.37563, -5.9925E-5 ] )
		# Ref 22
		#  R290,  PROPANE (C3H8)
		#
		Data.HREF.append ("R290")
		Data.REFH.append ("  R290")
		Data.CRIT.append ( [ 44.10, 231.1, 369.85, 4247.7, 0.220 ] )

		Data.COEFF.append ( [ 2988.28, -2.62902E-3, -1.09706E-6,		\
			            0.142963, -1.76519E-4, -5.78514E-8,		\
			            26.88900,   0.1250300,  1.07890E-4 ] )
		# Ref 23
		#  R600,  N-BUTANE (C4H10)
		#
		Data.HREF.append ("R600")
		Data.REFH.append ("  R600")
		Data.CRIT.append ( [ 58.124, 272.6, 425.16, 3796., 0.2548 ] )

		Data.COEFF.append ( [4822.7, -2.6499E-3, -0.4397E-6,		\
			            0.1908, -2.4836E-4,  0.2846E-7,		\
			             9.442,     0.3317, -1.1297E-4 ] )
		# Ref 24
		#  R600a,  ISOBUTANE [C(CH3)3]
		#
		Data.HREF.append ("R600a")
		Data.REFH.append (" R600a")
		Data.CRIT.append ( [58.124, 261.39, 407.9, 3630.6, 0.256 ] )

		Data.COEFF.append ([ 4197.24, -2.1894E-3, -1.3004E-6,	\
			            0.1803, -1.8719E-4, -8.1778E-8,		\
			           27.6833,   0.199384, 1.06305E-4 ] )
		# Ref 25
		#   R32:  DIFLUOROMETHANE (CH2F2)
		#
		Data.HREF.append ("R32")
		Data.REFH.append ("   R32")
		Data.CRIT.append ( [52.024, 221.40, 351.36, 5791.0, .120 ] )

		Data.COEFF.append ([ 1662.27, -2.19753E-3, -1.88903E-6,	\
			         0.0779879, -0.75238E-4, -0.53011E-7,	\
			           29.2127,   0.0192902,  8.91429E-5 ] )
		# Ref 26
		#  R1270,  PROPYLENE (C3H6)
		#
		Data.HREF.append ("R1270")
		Data.REFH.append (" R1270")
		Data.CRIT.append ( [42.09, 255.46, 364.9, 4621.7, 0.1937 ] )
		Data.HZERO [26] = -8695.95 ; Data.SZERO [26] = 170.53 # DATA HZERO(26),SZERO(26)  -8695.95, 170.53 ] )
			  
		Data.COEFF.append ([ 2294.38, -1.57422E-03, -2.98847E-06,\
			           .1253157, -1.28616E-04, -1.09990E-07,	\
			              3.856,     0.2321, -1.0308E-4 ] )
		 
		# Ref 27
		#  R124, 1-CHLORO-1,2,2,2-TETRAFLOUROETHANE (C2HF5)
		#
		Data.HREF.append ("R124")
		Data.REFH.append ("  R124")
		Data.CRIT.append ( [136.48, 259.96, 395.62, 3637., 0.244 ] )

		Data.COEFF.append ([ 4504.401, -2.574376E-3,  -1.4705E-6,	\
			           0.173954,  -1.79579E-4, -1.04407E-7,		\
			            30.9777,     0.254206, -9.36414E-5 ] )
		# Ref 28
		#   R115, CHLOROPENTAFLOUROETHANE  (CF2CL-CF3)
		#
		Data.HREF.append ("R115")
		Data.REFH.append ("  R115")
		Data.CRIT.append ( [154.47, 233.98, 353.05, 3153.0, 0.252 ] )

		Data.COEFF.append ([ 3968.734, -2.471498E-3, -2.656280E-6,	\
			           .1817131, -1.797986E-4, -2.305032E-7,	\
			            20.0246,  .3765849E0,  -2.703487E-4 ] )
		# Ref 29
		# CE-216, FROM JIM SANDS OF ORNL RECEIVED 12] )23] )91
		#
		Data.HREF.append ("CE-216")
		Data.REFH.append ("CE-216")
		Data.CRIT.append ( [166.02, 233.15, 361.8, 3094.0, 0.272 ] )

		Data.COEFF.append ([ 3808.5,  -0.0017285, -3.81991E-6,	\
			       0.16412557, -6.60150E-5, -3.83529E-7,	\
			      -52.9448624,   0.6902447, -0.0006871 ] )
		# Ref 30
		# E-125, FROM CYNTHIA GAGE 2-11-93
		#
		Data.HREF.append ("E-125")
		Data.REFH.append (" E-125")
		Data.CRIT.append ( [136.02, 238.55, 353.8, 3330.0, 0.2385 ] )

		Data.COEFF.append ([ 3112.3, -0.0013240, -4.48727E-6,	\
			        0.15782070, -0.0001235, -2.51097E-7,	\
			        31.5556400,  0.3137960, -0.0001836 ] )
		# Ref 31
		#   R123a 1,2-DICHLORO-1,1,2-TRIFLUOROETHANE
		#
		Data.HREF.append ("R213A")
		Data.REFH.append (" R123A")
		Data.CRIT.append ([ 152.93, 303.2, 461.1, 3741.0, 0.2812 ] )

		Data.COEFF.append ([6376.995, -2.691077E-3, -2.524465E-7,	\
			           .2016864, -2.035804E-4, -3.644260E-8,	\
			           48.23970,     .1856480,          0.0 ] )
		# Ref 32
		#   R143, 1,1,2-TRIFLUOROETHANE (CF2H-CFH2)
		#
		Data.HREF.append ("R143")
		Data.REFH.append ("  R143")
		Data.CRIT.append ( [84.04, 277.2, 429.9, 4520.0, 0.190 ] )

		Data.COEFF.append ([3680.023, -2.4128619E-3, -1.183791E-6,	\
			           .1221286, -8.9741778E-5, -1.068718E-7,	\
			            24.9639,       .187598, -4.031996E-5 ] )
		# Ref 33
		#   R218, PERFLUOROPROPANE (C3F8)
		#
		Data.HREF.append ("R218")
		Data.REFH.append ("  R218")
		Data.CRIT.append ([ 188.03, 236.4, 345.1, 2680.1, 0.2994 ] )

		Data.COEFF.append ([  4486.64, -1.952581E-3, -4.49894E-6,	\
			             .205911, -1.493288E-4, -4.30009E-7,	\
			             23.2683,      .536728, -3.97647E-4 ] )
		# Ref 34
		#  E134, BIS(DIFLUOROMETHYL) (CHF2-O-CHF2)
		#
		Data.HREF.append ("E134")
		Data.REFH.append ("  E134")
		Data.CRIT.append ([ 118.03, 279.3, 420.3, 4228.0, 0.224 ] )

		Data.COEFF.append([ 6016.695, -4.051717E-3, 8.906450E-7,		\
			           .1718950, -2.308880E-4, 2.837796E-8,		\
			           -26.7633,     .6152671, -6.58095E-4 ] )	
	
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=

