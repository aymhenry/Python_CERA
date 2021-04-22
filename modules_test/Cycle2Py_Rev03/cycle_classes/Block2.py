# Python Import ==================
import math,sys

# User Import
#from Data import Data

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# Job 			:
#
# Editor		: aymhenry@gmail.com
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
#--    PROPERTY ROUTINES REFERENCED:
#--         bconst	initializes arrays of property coefficients
#--         bublp	saturation properties at given pressure
#--         bublt	saturation properties at given temperature
#--         entrop	molar entropy
#--         espar	set up coefficients for equation of state
#--         hcvcps	molar enthalpy and heat capacity
#--         hpin	temperature, quality, etc. as a function of enthalpy and pressure
#--         spin	temperature, quality, etc. as a function of entropy and temperature
#--         vit		calculate specifi volume

class BData:
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
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
	

	#-- Common HREF1 group ---------------------------------
	HREF = [] #[[" "] * (34+1)]		# CHARACTER*6 HREF(34),REFH(34)
	REFH = [] #[[" "] * (34+1)]
	
	#-- Common ESDATA group ---------------------------------
	COEFF = []	
	CRIT  = [] 

	#-- Common HSZERO group ---------------------------------
	HZERO = [0.0] * (34+1)
	SZERO = [0.0] * (34+1)

	#-- Common REF group ---------------------------------
	TREF= [0.0] * (5+1)
	HR =  [0.0] * (5+1)
	SR =  [0.0] * (5+1)
	VR =  [0.0] * (5+1)
	
	#-- Common RDATA2 group ---------------------------------
	WM =  [0.0] * (5+1)
	TC =  [0.0] * (5+1) 

	#-- Common ESPAR1 group ---------------------------------
	F  =  [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
	AP =  [0.0] * (5+1)
	BP =  [0.0] * (5+1)
	DADT = 0.0; DBDT = 0.0; D2ADT= 0.0; D2BDT = 0.0
	
	#-- Common CPDATA group ---------------------------------
	# Create zero base array
	C = [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]

	#-- Common RDATA1 group ---------------------------------
	# Create zero base array
	A = [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
	B = [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]

	#-- Common RDATA4 group --------------------------------
	R = 8.314
	
	#-- Common TOL group ----------------------------------
	TOLR  = 1.0E-7*10 # SHOULD BE AT LEAST 10 TIMES LARGER THAN MACHINE PRECISION
	ITMAX = 20
	LUP   = 9

	#-- Common HSPURE group ---------------------------------
	HP = [0.0] * (5+1)
	SP = [0.0] * (5+1)
	CP = [0.0] * (5+1)

	#-- Common TOLSH group ----------------------------------
	TOLH = 0.010
	TOLS = 0.001
	
	@staticmethod
	def setup():
		# Ref 00
		#   add one more Col and Row, to keep FORTRAN none zero Ref.
		BData.HREF.append  (" ")
		BData.REFH.append  (" ")
		
		#-- zero based BData.CRIT.append  ([0.0] * 5)
		#-- zero based BData.COEFF.append ([0.0] * 9)
		#---------------------------------------
		# Ref 01
		#   R11, TRICHLOROFLUOROMETHANE (CFCL3)
		#
		BData.HREF.append ("R11")
		BData.REFH.append ("   R11")
		
		BData.CRIT.append  ([137.37, 296.91, 471.2, 4467.0, 0.247])
		
		BData.COEFF.append ([ 4967.07,  -2.23098E-3, -5.59203E-7,		\
					   0.178148,  -1.82363E-4, -2.54131E-8,	\
					    23.4805,     0.251072, -2.28722E-4 ])

		# Ref 02
		#  R12, DICHLORODIFLUOROMETHANE (CF2CL2)
		#
		BData.HREF.append ("R12")
		BData.REFH.append ("   R12")
		
		BData.CRIT.append ([120.91, 243.39, 384.95, 4180.0, 0.241]) # error last item fixed (was 0.181)
		BData.COEFF.append ([ 3819.88, -3.31988E-3,  2.41944E-7,	\
					        0.165350, -2.65758E-4,  9.13878E-8,	\
					         18.4874,    0.241782, -2.04594E-4])
		# Ref 03
		#   R13, CHLOROTRIFLUOROMETHANE (CF3CL)
		#
		BData.HREF.append ("R13")
		BData.REFH.append ("   R13")
		
		BData.CRIT.append ([104.46, 191.67, 302.0, 3870.0, 0.181])
		BData.COEFF.append([ 2157.20, -2.84478E-3, -2.75122E-6,	\
					  0.129485, -1.93746E-4, -9.01119E-8,	\
					   13.8691,    0.232370, -1.83095E-4 ])
		# Ref 04
		#  n-C5, n-Pentane (C5H12)
		#
		BData.HREF.append ("n-C5")
		BData.REFH.append ("   n-C5")
		BData.CRIT.append ([72.15, 309.34, 469.5, 3359.9, 0.295])
		BData.COEFF.append ([6745.80, -2.29793E-3, -0.70747E-6,	\
			          0.228716, -2.36350E-4, -0.32793E-7,	\
			           54.7577,    0.143042,  2.53720E-4 ])
		 
		# Ref 05
		#   R14, TETRAFLUOROMETHANE (CF4)
		#
		BData.HREF.append ("R14")
		BData.REFH.append ("   R14")
		BData.CRIT.append ([88.00, 145.17, 227.5, 3795.0, 0.141])
		BData.COEFF.append ([ 1272.41, -3.42946E-3, -6.47573E-6,	\
			           0.099664, -1.57113E-4, -2.95020E-7,	\
			            14.4296,    0.184530, -9.51890E-5 ])
		# Ref 06
		#   R22, CHLORODIFLUOROMETHANE (CHF2CL)
		#
		BData.HREF.append ("R22")
		BData.REFH.append ("   R22")
		BData.CRIT.append ( [86.47, 232.29, 369.3, 5054.0, 0.169 ] )

		BData.COEFF.append ([2624.62, -2.67304E-3, -1.33238E-6,	\
			          0.117395, -1.40272E-4, -0.52161E-7,	\
			           21.9839,    0.127744, -4.78872E-5 ] )
		# Ref 07
		#   R23, TRIFLUOROMETHANE (CHF3)
		#
		BData.HREF.append ("R23")
		BData.REFH.append ("   R23")
		BData.CRIT.append ( [70.01, 191.12, 299.1, 4900.0, 0.133 ] )

		BData.COEFF.append ( [ 1743.89, -3.52595E-3, -1.12774E-6,
			            0.090205, -1.25602E-4, -0.50675E-7,
			             23.6029,    0.082287,  3.18265E-5 ] )
		# Ref 08
		#   R113, 1,1,2-TRICHLOROTRIFLUOROETHANE (CF2CL-CFCL2)
		#
		BData.HREF.append ("R113")
		BData.REFH.append ("  R113")
		BData.CRIT.append ( [ 187.38, 320.80, 487.5, 3456.0, 0.329 ] )

		BData.COEFF.append ( [ 7284.48, -2.15870E-3, -8.03754E-7,		\
			            0.234712, -2.11131E-4, -7.33758E-8,		\
			             76.2637,    0.119641,  7.18786E-5 ] )
		# Ref 09
		#   R114, 1,2-DICHLOROTETRAFLUOROETHANE  (CF2CL-CF2CL): Version 3.0 Refprop
		#
		BData.HREF.append ("R114")
		BData.REFH.append ("  R114")
		BData.CRIT.append ( [ 170.92, 276.80, 418.80, 3248.0, 0.307 ] )

		BData.COEFF.append ( [ 5929.74, -2.86018E-3, -4.81520E-7,		\
			            0.221874, -2.88304E-4,  1.81892E-8,		\
			             37.2482,    0.337339, -2.39995E-4 ] )
		 
		# Ref 10
		#   R142B, 1-CHLORO-1,1-DIFLUOROETHANE (CF2CL-CH3)
		#
		BData.HREF.append ("R142B")
		BData.REFH.append (" R142B")
		BData.CRIT.append ( [100.49, 264.01, 410.3, 4120.0, 0.231 ] )

		BData.COEFF.append ( [ 4180.75, -2.73043E-3, -5.43638E-7,		\
			            0.169138, -2.41068E-4,  0.67566E-7,		\
			             16.3914,    0.271719, -1.58933E-4 ] )
		# Ref 11
		#   R152A, 1,1-DIFLUOROETHANE  (CHF2-CH3)
		#
		BData.HREF.append ("R152A")
		BData.REFH.append (" R152A")
		BData.CRIT.append ( [ 66.05, 248.50, 386.7, 4492.0, 0.181 ] )

		BData.COEFF.append ( [3198.63, -2.96134E-3,  -0.32190E-6,		\
			           0.133264, -2.03633E-4,  0.777251E-7,		\
			            22.2832,    0.153987, -3.015434E-6 ] )
		# Ref 12
		#   R216A, 1,3-DICHLOROHEXAFLUOROPROPANE [NOT IN REFPROP4.0]
		#
		BData.HREF.append ("R216A")
		BData.REFH.append (" R216A")
		BData.CRIT.append ( [ 220.93, 233.15, 453.14, 2754.1, 0.3847 ] )

		BData.COEFF.append ( [ 8431.44, -2.45916E-3, -9.91754E-7,		\
			            0.265720, -2.20418E-4, -1.68111E-7,		\
			             8.79769,    0.654246, -5.39923E-4 ] )
		# Ref 13
		#  R125, PENTAFLUOROETHANE (C2HF5)
		#
		BData.HREF.append ("R125")
		BData.REFH.append ("  R125")
		BData.CRIT.append ( [120.03, 224.6, 339.4, 3629.0, 0.2099 ] )

		BData.COEFF.append ( [ 3427.92, -3.17461E-3, -1.75729E-6,		\
			            0.149380, -1.80851E-4, -1.18813E-7,		\
			            22.65024,    0.295668, -1.69490E-4 ] )
		# Ref 14
		#   R143A, 1,1,1-TRIFLUOROETHANE   (CF3-CH3)
		#
		BData.HREF.append ("R143A")
		BData.REFH.append (" R143A")
		BData.CRIT.append ( [ 84.04, 225.8, 346.3, 3811., 0.194] )

		BData.COEFF.append ( [ 2763.90920, -2.509056E-3, -1.797108E-6,		\
			             0.133153E0, -1.589538E-4, -0.583311E-7,		\
			             13.89426E0,     .2554913, -1.300829E-4] )
		# Ref 15
		#   R134A:  1,1,1,2-TETRAFLUOROETHANE  (CF3-CH2F)
		#
		BData.HREF.append ("R134a")
		BData.REFH.append (" R134a")
		BData.CRIT.append ( [ 102.030, 247.0, 374.3, 4067.0, 0.199 ] )

		BData.COEFF.append ( [ 3582.17, -2.81114E-3, -1.44679E-6,		\
			            0.141750, -1.62763E-4, -.628933E-7,		\
			             19.4006,    0.258531, -1.29665E-4 ] )
		# Ref 16
		#   R123, 1,1-DICHLORO-2,2,2-TRIFLUOROETHANE (CHCL2-CF3)
		#
		BData.HREF.append ("R123")
		BData.REFH.append ("  R123")
		BData.CRIT.append ( [ 152.93, 301.02, 456.9, 3674.0, 0.278 ] )

		BData.COEFF.append ( [ 6033.29, -2.37891E-3, -0.84728E-6,		\
			            0.199549, -1.89493E-4, -0.67680E-7,		\
			             29.2604,    0.302994, -1.92907E-4 ] )
		 
		# Ref 17
		#  RC318, PERFLUOROCYCLOBUTANE (C4F8)
		#
		BData.HREF.append ("RC-318")
		BData.REFH.append ("RC-318")
		BData.CRIT.append ( [ 200.04, 266.1, 388.4, 2778., 0.3248 ] )

		BData.COEFF.append ( [  6182.614E0, -2.536687E-3, -2.265766E-6,		\
			              .2255416E0, -1.898425E-4, -2.635465E-7,		\
			             28.972075E0,     .5333363, -3.557726E-4] )
		# Ref 18
		#  R134, 1,1,2,2-TETRAFLOUROETHANE (CHF2-CHF2)
		#
		BData.HREF.append ("R134")
		BData.REFH.append ("  R134")
		BData.CRIT.append ( [ 102.03, 253.34, 392.1, 4562.0, 0.189 ] )

		BData.COEFF.append ( [ 3547.10, -2.68720E-3, -1.41578E-6,		\
			             0.13856,  -1.5991E-4, -0.55880E-7,		\
			             32.5208,    0.222819, -1.06829E-4 ] )
		# Ref 19
		#   RC270, CYCLOPROPANE (C3H6)
		#
		BData.HREF.append ("RC270")
		BData.REFH.append (" RC270")
		BData.CRIT.append ( [42.081, 240.25, 398.30, 5580.0, 0.194 ] )

		BData.COEFF.append ( [ 2745.00, -2.98122E-3,  1.64391E-7,		\
			            0.125065, -2.01031E-4,   7.8506E-8,		\
			             8.19470,     0.136885, 0.777583E-4 ] )
		# Ref 20
		#  R141b,  1,1-DICHLORO-1-FLUOROETHANE (CFCL2-CH3)
		#
		BData.HREF.append ("R141B")
		BData.REFH.append (" R141b")
		BData.CRIT.append ( [116.94, 305.35, 477.3, 4120., 0.217 ] )

		BData.COEFF.append ( [ 5422.38, -2.24167E-3, -6.04435E-7,		\
			            0.180853, -1.61856E-4, -6.23542E-8,		\
			             35.8434, 0.175268,     0.0 ] )
		# Ref 21
		#  i-C5 ISO-PENTANE (C4H9-CH3)
		#
		BData.HREF.append ("i-C5")
		BData.REFH.append ("  i-C5")
		BData.CRIT.append ( [ 72.150, 300.9, 460.51, 3370.7, 0.306 ] )

		BData.COEFF.append ( [ 6408.1, -2.3216E-3, -0.7087E-6,		\
			           0.227727, -2.4414E-4, -2.9694E-8,		\
			             12.216,    0.37563, -5.9925E-5 ] )
		# Ref 22
		#  R290,  PROPANE (C3H8)
		#
		BData.HREF.append ("R290")
		BData.REFH.append ("  R290")
		BData.CRIT.append ( [ 44.10, 231.1, 369.85, 4247.7, 0.220 ] )

		BData.COEFF.append ( [ 2988.28, -2.62902E-3, -1.09706E-6,		\
			            0.142963, -1.76519E-4, -5.78514E-8,		\
			            26.88900,   0.1250300,  1.07890E-4 ] )
		# Ref 23
		#  R600,  N-BUTANE (C4H10)
		#
		BData.HREF.append ("R600")
		BData.REFH.append ("  R600")
		BData.CRIT.append ( [ 58.124, 272.6, 425.16, 3796., 0.2548 ] )

		BData.COEFF.append ( [4822.7, -2.6499E-3, -0.4397E-6,		\
			            0.1908, -2.4836E-4,  0.2846E-7,		\
			             9.442,     0.3317, -1.1297E-4 ] )
		# Ref 24
		#  R600a,  ISOBUTANE [C(CH3)3]
		#
		BData.HREF.append ("R600a")
		BData.REFH.append (" R600a")
		BData.CRIT.append ( [58.124, 261.39, 407.9, 3630.6, 0.256 ] )

		BData.COEFF.append ([ 4197.24, -2.1894E-3, -1.3004E-6,	\
			            0.1803, -1.8719E-4, -8.1778E-8,		\
			           27.6833,   0.199384, 1.06305E-4 ] )
		# Ref 25
		#   R32:  DIFLUOROMETHANE (CH2F2)
		#
		BData.HREF.append ("R32")
		BData.REFH.append ("   R32")
		BData.CRIT.append ( [52.024, 221.40, 351.36, 5791.0, .120 ] )

		BData.COEFF.append ([ 1662.27, -2.19753E-3, -1.88903E-6,	\
			         0.0779879, -0.75238E-4, -0.53011E-7,	\
			           29.2127,   0.0192902,  8.91429E-5 ] )
		# Ref 26
		#  R1270,  PROPYLENE (C3H6)
		#
		BData.HREF.append ("R1270")
		BData.REFH.append (" R1270")
		BData.CRIT.append ( [42.09, 255.46, 364.9, 4621.7, 0.1937 ] )
		BData.HZERO [26] = -8695.95 ; BData.SZERO [26] = 170.53 # DATA HZERO(26),SZERO(26)  -8695.95, 170.53 ] )
			  
		BData.COEFF.append ([ 2294.38, -1.57422E-03, -2.98847E-06,\
			           .1253157, -1.28616E-04, -1.09990E-07,	\
			              3.856,     0.2321, -1.0308E-4 ] )
		 
		# Ref 27
		#  R124, 1-CHLORO-1,2,2,2-TETRAFLOUROETHANE (C2HF5)
		#
		BData.HREF.append ("R124")
		BData.REFH.append ("  R124")
		BData.CRIT.append ( [136.48, 259.96, 395.62, 3637., 0.244 ] )

		BData.COEFF.append ([ 4504.401, -2.574376E-3,  -1.4705E-6,	\
			           0.173954,  -1.79579E-4, -1.04407E-7,		\
			            30.9777,     0.254206, -9.36414E-5 ] )
		# Ref 28
		#   R115, CHLOROPENTAFLOUROETHANE  (CF2CL-CF3)
		#
		BData.HREF.append ("R115")
		BData.REFH.append ("  R115")
		BData.CRIT.append ( [154.47, 233.98, 353.05, 3153.0, 0.252 ] )

		BData.COEFF.append ([ 3968.734, -2.471498E-3, -2.656280E-6,	\
			           .1817131, -1.797986E-4, -2.305032E-7,	\
			            20.0246,  .3765849E0,  -2.703487E-4 ] )
		# Ref 29
		# CE-216, FROM JIM SANDS OF ORNL RECEIVED 12] )23] )91
		#
		BData.HREF.append ("CE-216")
		BData.REFH.append ("CE-216")
		BData.CRIT.append ( [166.02, 233.15, 361.8, 3094.0, 0.272 ] )

		BData.COEFF.append ([ 3808.5,  -0.0017285, -3.81991E-6,	\
			       0.16412557, -6.60150E-5, -3.83529E-7,	\
			      -52.9448624,   0.6902447, -0.0006871 ] )
		# Ref 30
		# E-125, FROM CYNTHIA GAGE 2-11-93
		#
		BData.HREF.append ("E-125")
		BData.REFH.append (" E-125")
		BData.CRIT.append ( [136.02, 238.55, 353.8, 3330.0, 0.2385 ] )

		BData.COEFF.append ([ 3112.3, -0.0013240, -4.48727E-6,	\
			        0.15782070, -0.0001235, -2.51097E-7,	\
			        31.5556400,  0.3137960, -0.0001836 ] )
		# Ref 31
		#   R123a 1,2-DICHLORO-1,1,2-TRIFLUOROETHANE
		#
		BData.HREF.append ("R213A")
		BData.REFH.append (" R123A")
		BData.CRIT.append ([ 152.93, 303.2, 461.1, 3741.0, 0.2812 ] )

		BData.COEFF.append ([6376.995, -2.691077E-3, -2.524465E-7,	\
			           .2016864, -2.035804E-4, -3.644260E-8,	\
			           48.23970,     .1856480,          0.0 ] )
		# Ref 32
		#   R143, 1,1,2-TRIFLUOROETHANE (CF2H-CFH2)
		#
		BData.HREF.append ("R143")
		BData.REFH.append ("  R143")
		BData.CRIT.append ( [84.04, 277.2, 429.9, 4520.0, 0.190 ] )

		BData.COEFF.append ([3680.023, -2.4128619E-3, -1.183791E-6,	\
			           .1221286, -8.9741778E-5, -1.068718E-7,	\
			            24.9639,       .187598, -4.031996E-5 ] )
		# Ref 33
		#   R218, PERFLUOROPROPANE (C3F8)
		#
		BData.HREF.append ("R218")
		BData.REFH.append ("  R218")
		BData.CRIT.append ([ 188.03, 236.4, 345.1, 2680.1, 0.2994 ] )

		BData.COEFF.append ([  4486.64, -1.952581E-3, -4.49894E-6,	\
			             .205911, -1.493288E-4, -4.30009E-7,	\
			             23.2683,      .536728, -3.97647E-4 ] )
		# Ref 34
		#  E134, BIS(DIFLUOROMETHYL) (CHF2-O-CHF2)
		#
		BData.HREF.append ("E134")
		BData.REFH.append ("  E134")
		BData.CRIT.append ([ 118.03, 279.3, 420.3, 4228.0, 0.224 ] )

		BData.COEFF.append([ 6016.695, -4.051717E-3, 8.906450E-7,		\
			           .1718950, -2.308880E-4, 2.837796E-8,		\
			           -26.7633,     .6152671, -6.58095E-4 ] )	


class Block2 (BData):
	# =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
	# Set Static Vars
	# =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
	def __init__(self):
		self.setup()

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	# in Python only, to fix Fortant issue
	def setArr2dCol(self, arry_2dim, int_col, arr_1dim ):
		for ncnt in range (len(arry_2dim)):
			arry_2dim [ncnt][int_col] = arr_1dim [ncnt]
		return

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
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
		
	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def bconst (self, NCIN, IR, FIN):
		#	  SUBROUTINE BCONST (NCIN,IR,FIN)
		#
		#   THIS ROUTINE ACCESSES THE CURVE FIT COEFFICIENTS TO THE EQUATION
		#   OF STATE PARAMETERS (STORED IN BLOCK DATA BDESC) FOR THE
		#   REFRIGERANT PAIR OF INTEREST.  THE REFERENCE STATES FOR ENTHALPY
		#   AND ENTROPY ARE ALSO COMPUTED.  THIS SUBROUTINE MUST BE CALLED
		#   BEFORE ANY OTHER PROPERTY ROUTINES ARE REFERENCED AND ALSO IF
		#   THE MIXTURE OR THE VALUES OF THE INTERACTION COEFFICIENTS, F(I,J),
		#   ARE CHANGED.
		#
		#   INPUTS:
		#      IR - ARRAY OF CODE NUMBERS FOR THE COMPONENTS OF THE MIXTURE
		#      FIN - MIXTURE INTERACTION PARAMETER FOR BINARY PAIRS OF
		#            PURE COMPONENTS
		#
		#   OUTPUTS (VIA COMMON BLOCKS):
		#      A - ARRAY OF A COEFFICIENTS FOR THE PURE COMPONENTS
		#      B - ARRAY OF B COEFFICIENTS FOR THE PURE COMPONENTS
		#      C - ARRAY OF PURE COMPONENT CP0 COEFFICIENTS
		#      HR - ARRAY OF PURE COMPONENT REFERENCE
		#        ENTHALPIES; THESE ARE EQUAL TO THE SATURATED LIQUID
		#        ENTHALPY AT THE REFERENCE TEMPERATURE MINUS THE PERFECT
		#        GAS ENTHALPY AT THE REFERENCE TEMPERATURE
		#      SR - REFERENCE ENTROPIES; EQUAL TO THE DIFFERENCE BETWEEN
		#        THE SATURATED LIQUID AND PERFECT GAS ENTROPIES AT THE
		#        REFERENCE TEMPERATURE
		#      TC - PURE COMPONENT CRITICAL TEMPERATURES
		#      TREF - REFERENCE TEMPERATURES AT WHICH HR AND SR ARE COMPUTED
		#      WM - PURE COMPONENT MOLECULAR WEIGHTS
		#
		#   OTHER SUBROUTINES REFERENCED:
		#      BUBLT - COMPUTE SATURATED LIQUID AND VAPOR CONDITIONS
		#      HCVCPS - COMPUTE ENTHALPY AT REFERENCE STATE
		#      ENTROP - COMPUTE REFERENCE ENTROPY
		#
		#	  IMPLICIT REAL (A-H,O-Z)
		#	  DIMENSION IR(5),X(5),XV(5),FIN(5,5)
		#	  CHARACTER*6 HREF(34),REFH(34)
		#	  COMMON /NCOMP/ NC
		#	  COMMON /ESDATA/ COEFF(9,34),CRIT(5,34)
		#	  COMMON /ESPAR1/ AP(5),BP(5),F(5,5),DADT,DBDT,D2ADT,D2BDT
		#	  COMMON /HREF1/ HREF,REFH
		#	  COMMON /RDATA1/ A(0:2,5),B(0:2,5)
		#	  COMMON /RDATA2/ WM(5),TC(5)
		#	  COMMON /CPDATA/ C(0:2,5)
		#	  COMMON /HSZERO/ HZERO(34),SZERO(34)
		#	  COMMON /REF/ TREF(5),HR(5),SR(5),VR(5)
		
		#self.setup() # only in Python
		
		XV = [0.0] *(5+1)
		BData.NC  =  NCIN
		
		for KR in range(1,BData.NC  +  1):			# DO 100 KR = 1,NC

			BData.WM  [KR]  =  BData.CRIT [ IR[KR] -1] [1-1]	#WM(KR) = CRIT(1,IR(KR))
			BData.TREF[KR]  =  BData.CRIT [ IR[KR] -1] [2-1]
		
			BData.TC  [KR]  =  BData.CRIT [ IR[KR] -1] [3-1]
			BData.HR  [KR]  =  0.0	# not required in Python
			BData.SR  [KR]  =  0.0	# not required in Python
			BData.VR  [KR]  =  1.0

			for J in range(KR+1 ,BData.NC  +  1): 	#DO 98 J = KR + 1,NC
				BData.F[KR][J]  =  FIN [KR][J]	#	F(KR,J) = FIN(KR,J)
				BData.F[J] [KR] =  FIN [KR][J]	#98 F(J,KR) = FIN(KR,J)

			BData.F[KR][KR]  =  0.0		#F(KR,KR) = 0.0

			for KC in range(0, 2+1):		#DO 100 KC = 0,2
				BData.A[KC][KR-1]  =  BData.COEFF [ IR[KR] -1][KC + 1 -1]		#A(KC,KR) = COEFF(KC + 1,IR(KR))
				BData.B[KC][KR-1]  =  BData.COEFF [ IR[KR] -1][KC + 4 -1]		#B(KC,KR) = COEFF(KC + 4,IR(KR))
				BData.C[KC][KR-1]  =  BData.COEFF [ IR[KR] -1][KC + 7 -1]		#  C(KC,KR) = COEFF(KC + 7,IR(KR))

		#   CALL BUBBLE POINT ROUTINE TO CALCULATE SATURATED LIQUID AND VAPOR
		#   VOLUMES AND  : CALL ENTHALPY AND ENTROPY ROUTINE TO DETERMINE
		#   REFERENCE VALUES.  THE HZERO AND SZERO ALLOW AN ARBITRARY VALUE
		#   TO BE ASSIGNED TO THE SATURATED LIQUID H OR S AT THE REFERENCE
		#   TEMPERATURE.

		for KR in range(1,BData.NC  +  1) :		#	DO 164 KR = 1,NC
			X     = [0.0]  * (BData.NC+1)			# DO 160 I = 1,NC	#160 X[I] = 0.0
			X[KR] = 1.0
		
			# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
			# CALL BUBLT (TREF(KR),X,XV,  P,VR(KR),VV,.  TRUE.,.FALSE.)
			[X, XV, P, BData.VR[KR], VV, _] = self.bublt (BData.TREF[KR], X, XV, True) 
			
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HRKR, CV, CPX, VS] = self.hcvcps (1, BData.TREF[KR], BData.VR[KR], X) 	# CALL HCVCPS (1,TREF(KR),VR(KR),X,   HRKR,CV,CPX,VS)
			
			BData.HR[KR]  =  HRKR  -  BData.HZERO [  IR[KR] ]	#HR(KR) = HRKR - HZERO(IR(KR))
			BData.SR[KR]  =  self.entrop ( BData.TREF[KR], BData.VR[KR], X )  -  BData.SZERO [ IR[KR]]
		return

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def plimit (self, T, A, B): 	# ,VL,VU,PLOW,PUP
		#  SUBROUTINE PLIMIT (T,A,B,VL,VU,PLOW,PUP)
		#
		#    GIVEN TEMPERATURE AND EQUATION OF STATE PARAMETERS, THIS
		#    ROUTINE CALCULATES THE UPPER AND LOWER BOUNDS ON PRESSURE
		#    FOR WHICH THERE ARE BOTH LIQUID AND VAPOR SOLUTIONS TO THE
		#    EQUATION OF STATE.  IT CARRIES OUT TWO BISECTION METHOD
		#    ITERATIONS TO FIND THE POINTS WHERE THE DERIVATIVE OF PRESSURE
		#    W.R.T. VOLUME IS ZERO.
		#
		#    INPUTS:
		#       T - TEMPERATURE (K)
		#       A,B - EQUATION OF STATE PARAMETERS AT TEMPERATURE T
		#
		#    OUTPUTS:
		#       PLOW - LOWER BOUND ON PRESSURE (PLOW CAN BE NEGATIVE, THE
		#           CALLING PROGRAM MUST CHECK AND CORRECT FOR NEGATIVE
		#           PRESSURES)
		#       PUP - UPPER BOUND ON PRESSURE (KPA)
		#       VL - MOLAR VOLUME AT PLOW (M**3/KMOL)
		#       VU - MOLAR VOLUME AT PUP (M**3/KMOL)
		#
		#    OTHER SUBROUTINES REFERENCED:
		#       NONE
		#
		#
		#	  IMPLICIT REAL (A-H,O-Z)
		#	  COMMON /RDATA4/ R
		#	  COMMON /TOL/ TOLR,ITMAX,LUP
		#
		#    STATEMENT FUNCTIONS FOR THE EVALUATION OF PRESSURE AS A
		#    FUNCTION OF V AND THE DERIVATIVE OF PRESSURE W.R.T
		#    VOLUME AS A FUNCTION OF V
		#
		def P(RT,V,Y,A,B):
			return (RT * (1.0 + (1.0 + (1.0 - Y ) * Y ) * Y ) / pow(1.0 - Y,3) - A/(V+B))/V

		def DP(RT,V,A,B,B4,B42) :
			reslt = (-RT * (B42 * B42 + (-4.0 * B42 * B4 \
				+ (4.0 * B42 + (4.0 * B4 + V) * V ) * V ) * V ) \
				/ pow(V - B4,4)\
				+ A * ( 2.0 * V + B ) / pow( V + B,2) \
				) / ( V * V)
			return reslt

		VPOS = 0.0 # Only in Python set starting value
		B4  = 0.25 * B
		B42 = B4 * B4
		RT  = BData.R * T
		#
		#    STARTING AT A VOLUME OF 12.0*B4 (WHICH HAS A POSITIVE SLOPE
		#    FOR ALL 'REASONABLE' VALUES OF A, B, T) REDUCE THE VOLUME
		#    UNTIL A NEGATIVE SLOPE OF P W.R.T. V IS FOUND AND THEN BEGIN
		#    BISECTION METHOD TO FIND LOWER BOUND ON VOLUME AND PRESSURE.
		#
		VC = 12.0272727 * B4
		V  = VC
		
		for IT in range (1, BData.ITMAX +1):	# DO 100 IT=1,ITMAX
			DPDV = DP(RT, V, A, B, B4, B42)
			if (DPDV <= 0.0):break	# GOTO 116

			VPOS = V
			V = 0.5 * ( V + B4)

		VNEG = V		# location 116

		for IT in range (1, 20+1):	#DO 120 IT=1,20
			VL   = 0.5 *( VNEG + VPOS)
			DPDV = DP(RT, VL, A, B, B4, B42)

			if  (DPDV < 0.0):
				VNEG = VL
			else:
				VPOS = VL

		Y = B4/VL
		
		PLOW = P(RT, VL, Y, A, B)
		#
		#    STARTING AT V = 2*A/RT INCREASE V UNTIL A NEGATIVE
		#    SLOPE IS FOUND; USE WITH V = 12.0*B TO BEGIN BISECTION
		#    ITERATION FOR UPPER BOUND ON PRESSURE
		#
		VPOS = VC
		V    = 2.0 * A /RT

		for IT in range (1, BData.ITMAX + 1):	#DO 160 IT=1,ITMAX
			DPDV = DP(RT,V,A,B,B4,B42)
			if (DPDV <= 0.0): break	# GOTO 164
			VPOS = V
			V = 2.0 * V

		VNEG = V		# location 164

		for IT in range (1,20 +1): # DO 180 IT=1,20
			VU = 0.5 * (VNEG+VPOS)
			DPDV = DP(RT,VU,A,B,B4,B42)
			if (DPDV  < 0.0) :
				VNEG = VU
			else :
				VPOS = VU

		Y=B4/VU
		PUP = P(RT,VU,Y,A,B)
		return [VL,VU,PLOW,PUP] 

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def espar (self, IQ,T,X): # ,AMIX,BMIX
		# [P4, P5] = self.espar [P1, P2, P3]
		# [AMIX,BMIX] = self.espar (IQ,T,X)
		#
		#	  SUBROUTINE espar (IQ,T,X,AMIX,BMIX)
		#
		#   THIS ROUTINE CALCULATES THE EQUATION OF STATE PARAMETERS AND THEIR
		#   TEMPERATURE DEVIVATIVES AS A FUNCTION OF TEMPERATURE AND COMPOSITION
		#   AS NEEDED BY THE OTHER PROPERTY ROUTINES.  BASED ON THE VALUE OF THE
		#   INPUT QUALifIER THE NECESSARY PARAMETERS ARE CALCULATED.
		#   THE TEMPERATURE DEPENDENCE OF THE
		#   A, B, AND CP0 PARAMETERS ARE CONTAINED ENTIRELY WITHIN espar;
		#   ALTERNATE EXPRESSIONS REQUIRE CHANGING ONLY THIS SUBROUTINE.
		#   (CHANGES IN THE COMPOSITION DEPENDENCE OF A AND B WOULD ALSO
		#   REQUIRE CHANGING THE FUNCTION SUBROUTINE FOR CHEMICAL POTENTIAL.)
		#
		#   INPUTS:
		#      IQ  -  INPUT QUALIfIER
		#          =  0 COMPUTE ONLY A AND B
		#         > =  1 ALSO COMPUTE TEMPERATURE DERIVATIVES OF A AND B
		#         > =  2 ALSO COMPUTE SECOND DERIVATIVE OF A AND B AND
		#            IDEAL GAS HEAT CAPACITY
		#          =  1, 2 OR 3 ALSO COMPUTE CONSTANTS FOR PURE COMPONENT ENTHALPY
		#           AND ENTROPY
		#      T  -  TEMPERATURE (K)
		#      X  -  COMPOSITION (MOLE FRACTION COMPONENT A)
		#
		#   OUTPUTS:
		#      AMIX  -  'A' PARAMETER FOR MIXTURE AT T, X
		#      BMIX  -  'B' PARAMETER FOR MIXTURE AT T, X
		#
		#   OUTPUTS (VIA COMMON BLOCKS):
		#      A(I)  -  'A' PARAMETER FOR PURE COMPONENT I
		#      B(I)  -  'B' PARAMETER FOR PURE COMPONENT I
		#      F(I,J)  -  MIXTURE INTERACTION PARAMETER FOR BINARY PAIR I,J
		#      BData.DADT  -  TEMPERATURE DERIVATIVE OF A
		#      DBDT  -  TEMPERATURE DERIVATIVE OF B
		#      D2ADT2  -  SECOND DERIVATIVE OF A WITH RESPECT TO TEMPERATURE
		#      D2BDT2  -  SECOND DERIVATIVE OF B WITH RESPECT TO TEMPERATURE
		#      BData.HP(I)  -  INTEGRAL OF CP(I) WITH RESPECT TO TEMP FOR PURE I
		#      SP(I)  -  INTEGRAL OF (CP(I)  -  R) / T WITH RESPECT TO TEMP FOR PURE I
		#      CP(I)  -  PERFECT GAS HEAT CAPACITY FOR COMPONENT I (KJ / (KG MOL K))
		#
		#

		#DIMENSION X(5),AJI(5,5),DAJI(5,5),DA(5,5)
		#COMMON  / NCOMP /  BData.NC
		#COMMON  / ESPAR1 /  BData.AP(5),BP(5),BData.F(5,5),DADT,DBDT,BData.D2ADT,BData.D2BDT
		#COMMON  / RDATA1 /  BData.A(0:2,5),BData.B(0:2,5)
		#COMMON  / CPDATA /  BData.C(0:2,5)
		#COMMON  / HSPURE /  BData.HP(5),BData.SP(5),BData.CP(5)
		#COMMON  / REF /  TREF(5),HR(5),SR(5),VR(5)
		#COMMON  / RDATA4 /  R

		# Local Vars
		AJI  =  [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		DA   =  [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		DAJI =  [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		AMIX =  0.0
		BMIX =  0.0

		for I in range(1, BData.NC + 1):	#DO 120 I = 1,NC
			BData.AP[I]  =  BData.A[0][I-1] *  math.exp((BData.A[1][I-1] + BData.A[2][I-1] * T) * T)
			BData.BP[I]  =  BData.B[0][I-1] + (BData.B[1][I-1] + BData.B[2][I-1] * T) * T
			
			AJI[I][I]  =  X[I] * X[I] * BData.AP[I]
			AMIX  =  AMIX + AJI[I][I]
			BMIX  =  BMIX + X[I] * BData.BP[I]

			for J in range(1, (I - 1)  +  1):	#DO 120 J  =  1,I - 1
				AJI[J][I]  =  X[J] * X[I] * (1.0 - BData.F[J][I]) * math.sqrt(BData.AP[J] * BData.AP[I])
				AMIX  =  AMIX + 2.0 * AJI[J][I]

		if IQ >=  1 :
			BData.DADT  =  0.0
			BData.DBDT  =  0.0

			for I in range(1,BData.NC  +  1): 	#DO 140 I  =  1,NC
				DA[I][I]  =  BData.A[1][I-1] + 2.0 * BData.A[2][I-1] * T
				DAJI[I][I]  =  AJI[I][I] * DA[I][I]
				BData.DADT  =  BData.DADT + DAJI[I][I]
				BData.DBDT  =  BData.DBDT + X[I] * (BData.B[1][I-1] + 2.0 * BData.B[2][I-1] * T)
				
				for J in range(1, (I - 1)  +  1):	#DO 140 J  =  1,I - 1
					DA[J][I]  =  0.5 * (BData.A[1][J-1] + BData.A[1][I-1] ) + (BData.A[2][J-1] + BData.A[2][I-1] ) * T
					DAJI[J][I]  =  AJI[J][I] * DA[J][I]
					BData.DADT  =  BData.DADT + 2.0 * DAJI[J][I]

			if IQ >=  2 :
				BData.D2ADT  =  0.0
				BData.D2BDT  =  0.0

				for I in range(1,BData.NC  +  1):	#DO 160 I  =  1,NC
					BData.CP[I]  =  BData.C[0][I-1] + (BData.C[1][I-1] + BData.C[2][I-1] * T) * T

					BData.D2BDT  =  BData.D2BDT + 2.0 * X[I] * BData.B[2][I-1]
					BData.D2ADT  =  BData.D2ADT + DAJI[I][I] * DA[I][I] + 2.0 * AJI[I][I] * BData.A[2][I-1]

					for J in range(1, (I - 1)  +  1): #DO 160 J  =  1,I - 1
						BData.D2ADT  =  BData.D2ADT + 2.0 * (DAJI[J][I] * DA[J][I] + AJI[J][I] * (BData.A[2][J-1] + BData.A[2][I-1]))

			if IQ <=  3 :
				if T <= 0:
					print ("\nespar : bad input parameter, T <= 0, T=",T)
					print ('Application terminated in python\n')
					sys.exit	#('1030') to show sorce of error
				
				for I in range(1,BData.NC  +  1):	#DO 180 I  =  1,NC
					
					BData.HP[I]  =  (BData.C[0][I-1] + (0.5 * BData.C[1][I-1] + BData.C[2][I-1] / 3.0 * T) * T) * T
					BData.SP[I]  =  (BData.C[0][I-1] - BData.R) * math.log(T / BData.TREF[I]) \
						 + BData.C[1][I-1] * (T - BData.TREF[I])		\
						 + 0.5 * BData.C[2][I-1] * (T * T -  (BData.TREF[I]**2))

		return [AMIX, BMIX ]

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def critx (self, X, TTC): #, TTC, PC, VC
		#	  SUBROUTINE CRITX (X,TTC,PC,VC)
		#
		#   DEVELOPED BY MARK MCLINDEN AND GRAHAM MORRISON AT THE
		#   NATIONAL BUREAU OF STANDARDS UNDER FUNDING FROM THE ELECTRIC
		#   POWER RESEARCH INSTITUTE AND NBS.
		#
		#   THIS ROUTINE FINDS THE CRITICAL POINT PREDICTED BY THE
		#   EQUATION OF STATE.  FOR A MIXTURE THE CRITICAL POINT IS
		#   THAT OF A PSEUDO-PURE COMPONENT HAVING THE SAME 'A' AND
		#   'B' PARAMETERS AS THE MIXTURE; IN GENERAL, SUCH A PSEUDO-
		#   PURE CRITICAL POINT WILL BE BELOW THE ACTUAL MIXTURE
		#   CRITICAL POINT.
		#
		#   INPUTS:
		#      X - MIXTURE COMPOSITION (MOL FRAC)
		#      TTC - INITIAL GUESS FOR THE CRITICAL TEMPERATURE (K)
		#
		#   OUTPUTS:
		#      TTC - CRITICAL TEMPERATURE (K)
		#      PC - CRITICAL PRESSURE (KPA)
		#      VC - CRITICAL VOLUME (M**3/KMOL)
		#
		#   OTHER SUBROUTINE REFERENCED:
		#      ESPAR - CALCULATION OF EQUATION OF STATE PARAMETERS
		#
		#	  COMMON /RDATA4/ R
		
		#-- Common REF group ---------------------------------
		TC = [0.0] * (3+1)
		FTC= [0.0] * (2+1)
		
		if (TTC <= 0.0):
			TTC = 300.0

		TC[1] = TTC
		J = 1
		
		b_python_flag = True # python flag to exit loop
		
		for IT in range (1, 20 +1 ):	#DO 200 IT=1,20
			# [P4, P5] = self.espar [P1, P2, P3]
			[AC, BC] = self.espar (0, TC[J], X)		#CALL ESPAR (0,TC(J),X,AC,BC)
			
			if (BC <= 0.0):
				TC[J] = 0.5 * TC[J]
				break	#GOTO 200
			
			TCC = 0.2273291 * AC / (BData.R*BC)
			FTC[J] = TCC - TC[J]
	
			if ( abs( FTC[J] ) < 0.01):
				b_python_flag = False	# exit flag
				break 					# GOTO 240
			
			if (J <= 1):
				J = 2
				TC[2] = min (TCC, 1.5 * TC[1] )
			else:
				TC[3] =  TC[2] - FTC[2] * ( TC[2] - TC[1] ) / ( FTC[2] - FTC[1] )
				TC[1] =  TC[2]
				TC[2] =  TC[3]
				FTC[1]= FTC[2]

		# end of loop 200 CONTINUE
		
		if b_python_flag :	# if false goto 240
			print ('*** CRITX DID NOT CONVERGE')
		
		# 240 TTC=TC[J]
		TTC = TC[J]
		PC = 0.02386944 * AC / BC**2
		VC = 3.006818 * BC
		return [TTC, PC, VC]
	
	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	@staticmethod
	def gibbs ( T,V,A,B) :
		'''
		statement function for GIBBS free energy
		note that since only differences of GIBBS are used, any terms which would cancel are omitted
		'''	
		return BData.R * T * ( -  math.log(V)  \
			+ 0.25 * B * ((8.0 * V - 2.25 * B) * V  \
			+ 0.1875 * B * B) /  ((V - 0.25 * B)**2)  / (V - 0.25 * B))  \
			+ A / B * math.log(V / (V + B)) - A / (V + B)
				
	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def bublt (self, T,XL,XV,LBUB):  
		#	SUBROUTINE BUBLT (P1, P2, P3   , P4, P5, P6,     P7,   P8)
		#===============if LBUB is true XL give ,return XV, XL no effect
		# [	XL, XV, P, VL, VV, LCRIT] = bublt ( T, XL, XV, LBUB )
		# [	P2,to P6, P8]   = bublt (P1, P2, P3 , P7 )
		# [	_, P3 to P6, P8]    = bublt (P1, P2, P3 , True )
		# [	P2, _ ,P4, P5, P6, P8]    = bublt (P1, P2, P3 , False )
		#======================
		#      SUBROUTINE BUBLT (T ,XL, XV,  P, VL, VV,  LBUB,  LCRIT)
		#
		#   GIVEN TEMPERATURE AND COMPOSITION OF ONE PHASE THIS ROUTINE
		#   CALCULATES THE SATURATION PRESSURE, THE COMPOSITION OF THE OTHER
		#   PHASE AND THE LIQUID AND VAPOR SPECifIC VOLUMES.
		#
		#   INPUTS:
		#      T  -  TEMPERATURE (K)
		#      ONLY ONE OF:  XL  -  LIQUID COMPOSITION (MOLE FRACTION)
		#               OR:  XV  -  VAPOR COMPOSITION (MOLE FRACTION)
		#      LBUB  -  LOGICAL VARIABLE
		#         if LBUB = True LIQUID COMPOSITION IS GIVEN (COMPUTE
		#         BUBBLE POINT)
		#         if LBUB = False VAPOR COMPOSITION IS GIVEN (COMPUTE
		#         DEW POINT)
		#
		#   OUTPUTS:
		#      XL OR XV  -  COMPOSITION OF CALCULATED PHASE
		#      P  -  SATURATION PRESSURE (kPa)
		#      VL  -  LIQUID VOLUME (M**3/ KG MOL)
		#      VV  -  VAPOR VOLUME (M**3/KG MOL)
		#      LCRIT  -  ERROR FLAG; if LCRIT = True THE INPUT TEMPERATURE
		#         EXCEEDS THE CRITICAL TEMPERATURE OF THE MIXTURE AND
		#         NO CALCULATIONS ARE DONE
		#
		#   OTHER SUBROUTINES REFERENCED:
		#      VIT  -  ITERATION FOR SPECifIC VOLUME
		#      PLIMIT  -  DETERMINES INITIAL BOUNDS ON PRESSURE AND VOLUME
		#      espar  -  COMPUTATION OF EQUATION OF STATE PARAMETERS
		#
		#   GENERAL NOMENCLATURE FOR FIRST LETTER OF VARIABLE NAMES
		#      A,B  -  EQUATION OF STATE PARAMETERS
		#      BData.F  -  MIXING PARAMETER
		#      T  -  TEMPERATURE
		#      P  -  PRESSURE
		#      V  -  SPECifIC VOLUME
		#      X  -  COMPOSITION
		#      gibbs  -  GIBBS FREE ENERGY
		#      U  -  CHEMICAL POTENTIAL
		#      Y  -  COMBINATION OF VARIABLES USED IN EQUATION OF STATE
		#      TOL  -  CONVERGENCE TOLERANCE FOR ITERATION LOOPS
		#      I,J  -  INDEX VARIABLES FOR ITERATION AND DO LOOPS
		#      L  -  LOGICAL VARIABLES SUCH AS NON - CONVERGENCE FLAGS
		#
		#   GENERAL NOMENCLATURE FOR SECOND OR THIRD LETTER OF VARIABLES
		#      L  -  LIQUID PHASE
		#      V  -  VAPOR PHASE
		#      1  -  PARENT PHASE (PHASE WITH SPECifIED COMPOSITION)
		#      2  -  INCIPIENT PHASE
		#
		#
		#IMPLICIT REAL (A - H,O - Z)
		#LOGICAL LBUB,LCRIT,LV1CON,LV2CON,LXCON,LXPOS,LXNEG,LPPOS,LPNEG,LPPCON
		#COMMON  / NCOMP /  BData.NC
		#COMMON  / ESPAR1 /  BData.AP(5),BData.BP(5),BData.F(5,5),BData.DADT,BData.DBDT,BData.D2ADT,D2BDT
		#COMMON  / RDATA4 /  R
		#COMMON  / TOL /  TOLR,ITMAX,LUP
		
		PP  =  [0.0]  *  (3 + 1)	#PP(3),FP(2),XL(5),XV(5)
		FP  =  [0.0]  *  (2 + 1)
		#input XL  =  [0.0]  *  (5 + 1)
		#input XV  =  [0.0]  *  (5 + 1)

		X1  =  [0.0]  *  (5 + 1)	# X1(5),X2(5),X2C(5),XX2(5)
		X2  =  [0.0]  *  (5 + 1)
		X2C =  [0.0]  *  (5 + 1)
		XX2 =  [0.0]  *  (5 + 1)

		Z   =  [0.0]  *  (5 + 1)	# Z(5),FX2(5),U1(5),U2(5),PL(3)
		FX2 =  [0.0]  *  (5 + 1)
		U1  =  [0.0]  *  (5 + 1)
		U2  =  [0.0]  *  (5 + 1)
		PL  =  [0.0]  *  (3 + 1)
		
		LCRIT  =  False
		
		#
		#   COMPUTE PURE COMPONENT E.S. COEFFICIENTS, THE MIXING PARAMETER,
		#   AND THE E.S. COEFFICIENTS FOR PHASE 1
		
		if LBUB:
			X1 = XL[:]
			XV = XL[:]
			#for I in range(1,BData.NC  + 1):
			#	X1[I] = XL[I]
			#	XV[I] = XL[I]
		else:
			X1 = XV[:]
			XL = XV[:]		
			#for I in range(1,BData.NC  + 1):
			#	X1[I] = XV[I]
			#	XL[I] = XV[I]

		[A1, B1] = self.espar (0,T,X1)	#CALL espar ((0,T,X1,A1,B1)
		
		#
		#   DETERMINE if INPUT TEMPERATURE EXCEEDS CRITICAL POINT;
		#   if SO, SET ERROR FLAG AND RETURN
		#
		loc_TC  =  A1 / (B1 * 4.398909 * BData.R)
	
		if T > 0.99 * loc_TC :
			LCRIT = True
			print ('bublt : critical point of pure or pseudo - pure material exceeded in bublt')
			print ('      in python, bad entry data, input temperature is more than critical point')
			print ('      input temp=' + str(T) + "K Critical point (with the giiven X) " + str(.099*loc_TC) + " K")
			print ('      input X ' , X1)
			print ('Application terminated in python\n')
			sys.exit('1000')
			return [XL, XV, 0.0, 0.0, 0.0, LCRIT]

		#
		#   ENTER ITERATION FOR PSEUDO - PURE COMPONENT.  THIS ITERATION
		#   YIELDS THE FINAL RESULT FOR A PURE COMPONENT AND PROVIDES
		#   A STARTING GUESS FOR THE PRESSURE OF A MIXTURE
		#
		#   CALL SUBROUTINE TO DETERMINE THE UPPER AND LOWER BOUNDS
		#   ON PRESSURE FOR WHICH THERE ARE BOTH LIQUID AND VAPOR
		#   SOLUTIONS OF THE EQUATION OF STATE
		#

		[VLOW, VUP, PLOW, PUP] = self.plimit (T,A1,B1)

		#   SET INITIAL GUESSES FOR PRESSURE NEAR THE UPPER AND
		#   LOWER BOUNDS.  if THE LOWER BOUND FOR PRESSURE IS NEGATIVE
		#   RESET IT TO A SMALL POSITIVE VALUE.

		if PLOW <= 0.0:
			VLOW = 0.8 * B1
			PC = 0.1049995 * BData.R * loc_TC / B1
			PLOW = (1.0E-12) * PC
			PP[1] = PLOW
		else:
			PP[1] = PLOW + 0.0001 * (PUP - PLOW)

		PP[2] = PUP - 0.0001 * (PUP - PLOW)
		PL[1] = math.log(PP[1])
		PL[2] = math.log(PP[2])
		
		VL = 0.9 * VLOW
		VV = 1.1 * VUP
		J = 1

		LPPOS   =  False
		LPNEG   =  False
		#LPPCON  =  False  # modifed in Python, work arounf Fortran Goto
		LPPCON  =  True 

		#
		#   STARTING WITH INITIAL VALUES OF PRESSURE CLOSE TO THE UPPER
		#   AND LOWER BOUNDS (FOUND BY SUBROUTINE PLIMIT) ITERATE ON
		#   LOG (P) UNTIL THE GIBBS FREE ENERGY OF BOTH PHASES ARE EQUAL.
		#   A COMBINATION OF SECANT AND REGULI - FALSI METHODS IS USED
		#   FOR THE ITERATION.
		
		PNEG = 0.0 # In Python 
		for IT in range (1, BData.ITMAX + 1):	#DO 400 IT = 1,ITMAX
			LV1CON = False
			LV2CON = False

			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[VL, LV1CON] = self.vit (T, PP[J], A1, B1, VL, True) 		# CALL VIT (T, PP[J], A1, B1, VL, True,  LV1CON)
			[VV, LV2CON] = self.vit (T, PP[J], A1, B1, VV, False)		# CALL VIT (T, PP[J], A1, B1, VV, False, LV2CON)

			GL = self.gibbs(T, VL, A1, B1)
			GV = self.gibbs(T, VV, A1, B1)
			FP [J] = GL - GV

			if FP[J] < 0.0:
				LPNEG = True
				FPNEG = FP[J]
				PNEG  = PL[J]
				PPOS = 0.0 # in python only
			else:
				LPPOS = True
				FPPOS = FP[J]
				PPOS  = PL[J]

			if IT <= 1 :
				J = 2
			else:
				DGDPL = (FP[2] - FP[1]) / (PL[2] - PL[1])
				if (DGDPL == 0.0) or \
					(abs(FP[J] / (PL[J] * DGDPL)) < BData.TOLR):	#GOTO 440
					#Python modification
					LPPCON = False
					break
					
				#   NEXT GUESS FOR LOG (P) GIVEN BY SECANT METHOD
				PL[3] = PL[2] - FP[2] / DGDPL
					
				#   IF NEXT GUESS FOR LOG (P) IS FURTHER FROM SOLUTION THAN
				#   PREVIOUS BEST GUESS, USE REGULI - FALSI METHOD FOR NEXT GUESS
				if  ( ( PL[3] > max(PNEG, PPOS) \
					or 	PL[3] < min(PNEG, PPOS) ) and LPNEG and LPPOS ) :

					PL[3] = PPOS - FPPOS * (PPOS - PNEG) / (FPPOS - FPNEG)

				PL[1] = PL[2]
				PL[2] = PL[3]
				FP[1] = FP[2]
				PP[2] = math.exp(PL[2])
				
					
			# 400 CONTINUE
			
			# Delete this comment  if ITERATION HAS NOT CONVERGED, SET ERROR FLAG.
			# ----deleted by python editorLPPCON = 1	#True
			
			#
			#   END OF PSEUDO - PURE COMPONENT ITERATION
			#
			#   FOR A PURE COMPONENT THE ABOVE ITERATION GIVES THE FINAL RESULT
			#	
		if BData.NC == 1:	#  440 if (BData.NC == 1)  :
			if LV1CON : print (BData.LUP,'bublt :volume iteration for parent phase did not converge')
			if LV2CON : print (BData.LUP,'bublt :volume iteration for incipient phase did not converge')
			if LPPCON : print (BData.LUP,'bublt :pure material pressure iteration in bublt did not converge')
			P = PP[J]
			return [XL, XV, P, VL, VV, LCRIT]
		#
		#   ENTER ITERATION FOR MIXTURE
		#
		#   THE MIXTURE ITERATION CONSISTS OF TWO CONCENTRIC ITERATION
		#   LOOPS WHICH VARY THE SATURATION PRESSURE OF THE MIXTURE AND THE
		#   COMPOSITION OF THE COMPUTED PHASE TO GIVE EQUAL CHEMICAL
		#   POTENTIALS FOR EACH OF THE COMPONENTS BETWEEN THE TWO PHASES.
		#   THE INITIAL GUESS FOR THE PRESSURE IS GIVEN BY THE PSEUDO - PURE
		#   ITERATION ABOVE; THE INITIAL GUESS FOR COMPOSITION IS THAT X2 = X1.
		#
		#   ASSIGN INITIAL VALUES OF LIQUID AND VAPOR VOLUMES FROM ABOVE
		#   ITERATION TO PHASE 1 AND 2 VOLUMES.
		#
		if LBUB :
			V1 = VL
			V2 = VV
		else:
			V1 = VV
			V2 = VL

		PP[1] = PP[J]

		#
		#   BEGIN ITERATION FOR SATURATION PRESSURE OF MIXTURE
		#
		J = 1
		X2C = X1[:]
		#for I in range(1, BData.NC +1):	# DO 500 I = 1,BData.NC
		#	X2C[I] = X1[I]

		LPNEG = False
		LPPOS = False
		
		b_pyth_exit_inner = False
		for ITP  in range (1, BData.ITMAX + 1): # DO 800 ITP = 1,ITMAX
			XX2 = X2C[:]
			#for I  in range (1, BData.NC + 1): # DO 520 I = 1,BData.NC
			#	XX2[I] = X2C[I]

			LXCON = False
			LV1CON = False
			
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[V1, LV1CON] = self.vit (T,PP[J],A1,B1,V1,LBUB)	#CALL VIT (T,PP(J),A1,B1,V1,LBUB,LV1CON)

			#
			#   if VOLUME ITERATION HAS NOT CONVERGED, TRY A NEW PRESSURE AND
			#   RETURN TO THE BEGINNING OF THE ITERATION
			#
			if (LV1CON  or  LXCON) :
				PP[2] = 0.5 * (PP[1] + PP[2])
				break # GOTO 800

			#   COMPUTE CHEMICAL POTENTIALS FOR PHASE 1
			for I in range (1, BData.NC + 1): #DO 540 I = 1,BData.NC
				U1[I] = self.U_Func (T,X1,I,V1,A1,B1,BData.AP,BData.BP,BData.F)

			#
			#   ENTER INNER ITERATION LOOP (FOR COMPOSITION OF PHASE 2)
			#
			LXNEG = False
			LXPOS = False
			
			C = 0.0 # prevent var is not found
			LXCON = True # add by python
			b_pyth_exit_outer = False
			for IT in range (1, BData.ITMAX + 1):	# DO 600 IT = 1,ITMAX
				LV2CON = False

				#   COMPUTE EQUATION OF STATE COEFFICIENTS FOR PHASE 2
				
				[A2, B2] = self.espar (0,T,XX2) 	#CALL espar (0,T,XX2, A2, B2)
				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[V2, LV2CON] = self.vit (T,PP[J], A2, B2,V2, not LBUB)	#CALL VIT (T,PP[J], A2, B2,V2,.NOT.LBUB,LV2CON)

				#
				#   if VOLUME ITERATION HAS NOT CONVERGED, TRY A NEW PRESSURE
				#   AND RETURN TO THE START OF THE PRESSURE ITERATION.
				#
				if LV2CON :
					PP[2] = 0.5 * (PP[1] + PP[2])
					b_pyth_exit_outer = True
					break	# GOTO 800

				#  COMPUTE CHEMICAL POTENTIALS OF PHASE 2
				for I in range (1, BData.NC + 1): #DO 560 I = 1,BData.NC
					U2[I] = self.U_Func(T,XX2,I,V2, A2, B2,BData.AP,BData.BP,BData.F)

				#
				#   CALCULATE THE COMPOSITION OF PHASE 2 FROM THE COMPOSITION
				#   OF PHASE 1 AND THE CHEMICAL POTENTIALS.  THE INNER ITERATION
				#   LOOP HAS CONVERGED WHEN THE CALCULATED COMPOSITION EQUALS
				#   (WITHIN A CONVERGENCE TOLERANCE) THE GUESSED VALUE OF X2.
				#
				FXSUM = 0.0
				C = 0.0
				for I in range (1, BData.NC + 1): 	#DO 580 I = 1,BData.NC
					Z[I] = X1[I] * math.exp(U1[I] - U2[I])
					C = C + Z[I]

				for I in range (1, BData.NC + 1): 	# DO 584 I = 1,BData.NC
					X2C[I] = Z[I] / C
					FX2[I] = X2C[I] - XX2[I]
					XX2[I] = X2C[I]
					FXSUM = FXSUM + abs(FX2[I])
				
				if (FXSUM < BData.NC * BData.TOLR) :
					# break is done exit 800 loop
					LXCON = False # add by python
					break	#GOTO 640
				#End of loop ---- 600 CONTINUE

			if b_pyth_exit_outer: break
			# deleted by python editor LXCON = True
			#
			#   END OF ITERATION LOOP FOR PHASE 2 COMPOSITION
			#

			FP[J] = 1.0 - C	# Fortran location 640

			#
			#   OUTER (PRESSURE) ITERATION HAS CONVERGED WHEN C  =  1.000
			#   (I.E. WHEN THE CHEMICAL POTENTIALS OF EACH COMPONENT ARE
			#   THE SAME IN BOTH PHASES).
			#

			if  (abs(FP[1]) <  100.0 * BData.TOLR):
				b_pyth_exit_inner = True
				# break is done exit 800 loop
				break	#GOTO 840

			#
			#   PROVIDED THAT THE X2 ITERATION HAS CONVERGED FOR THE CURRENT
			#   GUESS OF PRESSURE, UPDATE THE POSITIVE AND NEGATIVE
			#   BOUNDS FOR USE WITH THE REGULI - FALSI METHOD.
			#
			if not LXCON:
				if (FP[J] < 0.0):
					LPNEG = True
					FPNEG = FP[J]
					PNEG = PP[J]
				else:
					LPPOS = True
					FPPOS = FP[J]
					PPOS  = PP[J]

			#
			#   COMPUTE NEW GUESS FOR SATURATION PRESSURE.
			#
			if (ITP <= 2  or  J == 1  or  FP[1] == FP[2]) :
				PP[1] = PP[J]
				FP[1] = FP[J]

				if (LBUB) :
					PP[2] = PP[J] * C
				else :
					PP[2] = PP[J] / C

				J = 2
			else :
				PP[3] = PP[2] - FP[2] * (PP[2] - PP[1]) / (FP[2] - FP[1])

				if ((PP[3] > max(PNEG,PPOS)  or \
					 PP[3] < min(PNEG,PPOS) )  and  LPNEG  and  LPPOS):

					PP[3] = PPOS - FPPOS * (PPOS - PNEG) / (FPPOS - FPNEG)

				PP[1] = PP[2]
				PP[2] = PP[3]
				FP[1] = FP[2]
			#-  End of loop =========800 CONTINUE
		
		# location 800
		if not b_pyth_exit_inner:
			print (BData.LUP, 'bublt : mixture pressure iteration in bublt did not converge')

		P = PP[J] # location 840
		#
		#   ASSIGN RESULTS FOR PHASES 1 AND 2 TO LIQUID AND VAPOR PHASES
		#   DEPENDING ON WHETHER THE DEW OR BUBBLE POINT WAS CALCULATED.
		#
		if (LBUB)  :
			#for I in range (1, BData.NC + 1): # DO 860 I = 1,BData.NC
			#	XV[I] = XX2[I]
			XV = XX2 [:]
			VL = V1
			VV = V2
		else :
			#for I in range (1, BData.NC + 1): # DO 880 I = 1,BData.NC
			#	XL[I] = XX2[I]
			XL = XX2 [:]
			VL = V2
			VV = V1

		#
		#   PRINT WARNING MESSAGES FOR ANY CASES OF NON - CONVERGENCE OCCURING
		#   ON FINAL CALL TO EACH ITERATION AND RETURN.
		#
		if LV1CON : print (BData.LUP,'bublt : volume iteration for parent phase did not converge')
		if LV2CON : print (BData.LUP,'bublt : volume iteration for incipient phase did not converge')
		if LXCON  : print (BData.LUP,'bublt : composition iteration in bublt did not converge')

		return [ XL, XV, P, VL, VV, LCRIT] 

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def bublp (self, P, XL, XV, LBUB)	:	# T,VL,VV, LCRIT
		#	SUBROUTINE BUBLP (P1, P2, P3, P4, P5, P6, P7, P8)
		# [P2, P3 ,P4, P5, P6, P8] = self.bublp (P1, P2, P3 , P7 )
		#	  SUBROUTINE BUBLP (P,XL,XV   ,T,VL,VV,  LBUB   ,LCRIT)
		#
		#    DEVELOPED BY MARK MCLINDEN AND GRAHAM MORRISON AT THE
		#    NATIONAL BUREAU OF STANDARDS UNDER FUNDING FROM THE ELECTRIC
		#    POWER RESEARCH INSTITUTE AND NBS.
		#
		#    GIVEN PRESSURE AND COMPOSITION OF ONE PHASE THIS ROUTINE
		#    CALCULATES THE SATURATION TEMPERATURE, THE COMPOSITION OF THE OTHER
		#    PHASE AND THE LIQUID AND VAPOR MOLAR VOLUMES.
		#
		#    INPUTS:
		#       P  -  SATURATION PRESSURE (KPA)
		#       ONLY ONE OF:  XL  -  LIQUID COMPOSITION (MOLE FRACTION)
		#                OR:  XV  -  VAPOR COMPOSITION (MOLE FRACTION)
		#       LBUB  -  LOGICAL VARIABLE
		#          if LBUB = True LIQUID COMPOSITION IS GIVEN (COMPUTE
		#          BUBBLE POINT)
		#          if LBUB =  False  VAPOR COMPOSITION IS GIVEN (COMPUTE
		#          DEW POINT)
		#
		#    OUTPUTS:
		#       XL OR XV  -  COMPOSITION OF CALCULATED PHASE
		#       T  -  SATURATION TEMPERATURE (K)
		#       VL  -  LIQUID VOLUME (M *  * 3 / KMOL)
		#       VV  -  VAPOR VOLUME (M *  * 3 / KMOL)
		#       LCRIT  -  ERROR FLAG; SET TO True WHEN THE INPUT PRESSURE
		#          EXCEEDS THE CRITICAL PRESSURE.
		#
		#    OTHER SUBROUTINES REFERENCED:
		#       VIT  -  ITERATION FOR MOLAR VOLUME
		#       espar  -  COMPUTATION OF EQUATION OF STATE PARAMETERS
		#
		#    GENERAL NOMENCLATURE FOR FIRST LETTER OF VARIABLE NAMES
		#       A,B  -  EQUATION OF STATE PARAMETERS
		#       BData.F  -  MIXING PARAMETER
		#       T  -  TEMPERATURE
		#       P  -  PRESSURE
		#       V  -  MOLAR VOLUME
		#       X  -  COMPOSITION
		#       G  -  GIBBS FREE ENERGY
		#       U  -  CHEMICAL POTENTIAL
		#       Y  -  COMBINATION OF VARIABLES USED IN EQUATION OF STATE
		#       TOL  -  CONVERGENCE TOLERANCE FOR ITERATION LOOPS
		#       I,J  -  INDEX VARIABLES FOR ITERATION AND DO LOOPS
		#       L  -  LOGICAL VARIABLES SUCH AS NON - CONVERGENCE FLAGS
		#
		#    GENERAL NOMENCLATURE FOR SECOND OR THIRD LETTER OF VARIABLES
		#       A,B  -  COMPONENTS OF MIXTURE; COMPOSITION IS MOLE FRACTION BData.A
		#       L  -  LIQUID PHASE
		#       V  -  VAPOR PHASE
		#       1  -  PARENT PHASE (PHASE WITH SPECifIED COMPOSITION)
		#       2  -  INCIPIENT PHASE
		#       (FOR EXAMPLE UA1 REFERS TO CHEMICAL POTENTIAL OF COMPONENT BData.A
		#       IN PHASE 1)
		#
		#	  COMMON  / ESPAR1 /  BData.AP(5),BData.BP(5),BData.F(5,5),BData.DADT,BData.DBDT,BData.D2ADT,BData.D2BDT
		#	  COMMON  / NCOMP /  BData.NC
		#	  COMMON  / RDATA4 /  R
		#	  COMMON  / TOL /  TOLR,ITMAX,LUP

		# initialive basic vars
		LV1CON= False
		LV2CON= False
		LXCON = False
		
		LXPOS = False
		LXNEG = False
		
		LTPOS = False
		LTNEG = False
		
		LPPCON= False
		LCRIT = False

		#   DIMENSION TT(3),FT(2)
		TT = [0.0] * (3+1)
		FT = [0.0] * (2+1)
		#  DIMENSION XL(5),XV(5),  X1(5),X2(5),X2C(5),XX2(5)  ,Z(5),
		# FX2(5),U1(5),U2(5),UL(5),PL(5)
		X1 = [0.0] * (5+1)
		X2 = [0.0] * (5+1)
		
		X2C= [0.0] * (5+1)
		XX2= [0.0] * (5+1)

		Z  = [0.0] * (5+1)
		FX2= [0.0] * (5+1)
		U1 = [0.0] * (5+1)
		U2 = [0.0] * (5+1)
		UL = [0.0] * (5+1)
		PL = [0.0] * (5+1)

		TC = 340.0
		# SAVE BData.TNEG,BData.TPOS set as Data vars
		#BData.TNEG = 9999.0
		#BData.TPOS = -999.0

		#
		#    STATEMENT FUNCTIONS FOR GIBBS FREE ENERGY AND CHEMICAL POTENTIAL
		#    NOTE THAT SINCE ONLY DIFFERENCES OF G AND U ARE USED IN THE PROGRAM
		#    ANY TERMS WHICH WOULD CANCEL ARE OMITTED.  
		#    both g and u are  divided by r * t to obtain dimensionless quantities.
		#
				
		# Python: this is the original function for gibbs, gives bad results
		#def G ( T,V,A,B):
		#	# both G and U are  divided by r * t to obtain dimensionless quantities.
		#	return  - math.log(V) 
		#	+ 0.25 * B * ((8.0 * V - 2.25 * B) * V 	\
		#	+ 0.1875 * B * B) /  ((V - 0.25*B)**2) / (V - 0.25 * B) 	\
		#	+ (A / B * math.log(V / (V + B)) \
		#	- A / (V + B)  ) / (BData.R * T)
		
		
		#
		#    COMPUTE PURE COMPONENT E.S. COEFFICIENTS, THE MIXING PARAMETER,
		#    AND THE E.O.S. COEFFICIENTS FOR PHASE 1
		#
		if (LBUB) :
			X1 = XL [:]
			XV = XL [:]
			#for I in range(1, BData.NC + 1 ):   # DO 100 I = 1,BData.NC
			#	X1[I] = XL[I]
			#	XV[I] = XL[I]
		else:
			X1 = XV [:]
			XL = XV [:]
			#for I in range(1, BData.NC + 1 ):   # DO 120 I = 1,BData.NC
			#	X1[I] = XV[I]
			#	XL[I] = XV[I]
		#
		#    FIND CRITICAL POINT OF THE PURE (OR PSEUDO - PURE) MATERIAL
		#    AND BASE INITIAL GUESS FOR TEMPERATURE ON A SIMPLE, EMPIRICAL
		#    RELATIONSHIP BETWEEN REDUCED PRESSURE AND REDUCED TEMPERATURE
		#    THAT IS REASONABLY ACCURATE FOR REFRIGERANTS.
		#
		LCRIT =  False
		[TC, PC, VC] = self.critx(X1,TC)	#	CALL CRITX (X1,TC,PC,VC)
		
		TT[1] = TC / (1.0 - 0.153 * math.log(P / PC))

		# [P4, P5] = self.espar [P1, P2, P3]
		
		[A1, B1] =  self.espar(0, TT[1], X1 ) #	CALL espar (0,TT(1),X1,A1, B1)
		
		VL = 0.8 * B1
		VV = BData.R * TT[1] / P
		
		#
		#    ENTER ITERATION FOR PURE COMPONENT.  THIS ITERATION VARIES
		#    TEMPERATURE UNTIL THE GIBBS FREE ENERGY OF BOTH PHASES ARE EQUAL.
		#    A COMBINATION OF SECANT AND REGULI - FALSI METHODS IS USED
		#    FOR THE ITERATION.
		#
		if (BData.NC == 1) :
			if (P > PC) :
				LCRIT = True
				print ("bublp: critical point of pure or pseudo-pure material")
				return [0.0, 0.0, 0.0, True]

			J = 1
			LTPOS =  False
			LTNEG =  False
			LPPCON=  False
			
			b_python_flag_loop1 = False
			
			for  IT in range( 1, BData.ITMAX + 1 ):   # DO 400 IT = 1,ITMAX
				# [P4, P5] = self.espar [P1, P2, P3]
				[A1, B1] =  self.espar(0, TT[J], X1 ) #	CALL espar (0,TT(J),X1, A1, B1)
				LV1CON =  False
				LV2CON =  False
				
				if B1 <= 0:
					print ("bublp : bad input parameter for vit, B <= 0, B=",B1)
					print ("     input Temperature(K) =", TT[J] )
					print ('Application terminated in python\n')
					sys.exit('1021')
					
				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[VL, LV1CON] = self.vit (TT[J], P, A1, B1, VL, True) 	#CALL VIT (TT(J), P, A1,B1, VL, True,  LV1CON)
				[VV, LV2CON] = self.vit (TT[J], P, A1, B1, VV, False) 	#CALL VIT (TT(J), P, A1,B1, VV, False, LV2CON)

				GL = self.gibbs( TT[J], VL, A1, B1)
				GV = self.gibbs( TT[J], VV, A1, B1)

				FT[J] = GL - GV

				if (abs(FT[J]) < 100.0 * BData.TOLR):
					b_python_flag_loop1 = True
					break	#	GOTO 440

				if (FT[J] < 0.0) :
					LTNEG = True
					FTNEG = FT[J]
					BData.TNEG = TT[J]
				else:
					LTPOS = True
					FTPOS = FT[J]
					BData.TPOS = TT[J]

				if (IT <= 1):
					TT[2] = 0.95 * TT[1]
					J = 2
				else:
					DGDT = (FT[2] - FT[1]) / (TT[2] - TT[1] )
					if (DGDT == 0.0) :
						b_python_flag_loop1 = True
						break	#	GOTO 440
					#    NEXT GUESS FOR TEMPERATURE GIVEN BY SECANT METHOD
					TT[3] = TT[2] - FT[2] / DGDT
					
					#    IF NEXT GUESS FOR TEMPERATURE IS FURTHER FROM SOLUTION THAN
					#    PREVIOUS BEST GUESS, USE REGULI - FALSI METHOD FOR NEXT GUESS
					if (LTNEG and LTPOS)  :
						if (TT[3] < min(BData.TNEG, BData.TPOS)  or  TT[3] > max(BData.TNEG,BData.TPOS)) :
							TT[3] = BData.TPOS - FTPOS * (BData.TPOS - BData.TNEG) / (FTPOS - FTNEG)
					
					TT[1] = TT[2]
					FT[1] = FT[2]
					TT[2] = TT[3]

			# point 400   CONTINUE
			#    if ITERATION HAS NOT CONVERGED, SET ERROR FLAG.
			if not b_python_flag_loop1:
				LPPCON = True
			#    END OF PURE COMPONENT ITERATION
			#
			# point 440	CONTINUE
			if (LV1CON): print ('bublp: iteration in bublp for parent phase volume, did not converge')
			if (LV2CON): print ('bublp: iteration in bublp for incipient phase volume, did not converge')
			if (LPPCON): print ('bublp: pure material temperature iteration in bublp, did not converge')
			T = TT[J]
			return [XL,XV   ,T,VL,VV, LCRIT]
		# end of IF

		#
		#    ENTER ITERATION FOR MIXTURE
		#
		#    THE MIXTURE ITERATION CONSISTS OF TWO CONCENTRIC ITERATION
		#    LOOPS WHICH VARY THE TEMPERATURE OF THE MIXTURE AND THE
		#    COMPOSITION OF THE COMPUTED PHASE TO GIVE EQUAL CHEMICAL
		#    POTENTIALS FOR EACH OF THE COMPONENTS BETWEEN THE TWO PHASES.
		#    THE INITIAL GUESS FOR THE TEMPERATURE IS GIVEN BY THE CALCULATION
		#    ABOVE; THE INITIAL GUESS FOR COMPOSITION IS THAT X2 = X1.
		#
		J = 1
		X2C = X1 [:]
		#for I  in range(1, BData.NC +1 ):   # DO 500 I = 1,BData.NC
		#	X2C[I] = X1[I]

		if (LBUB) :
			V1 = VL
			V2 = VV
		else:
			V1 = VV
			V2 = VL

		LTNEG =  False
		LTPOS =  False

		b_python_flag_loop8 = False
		for ITT in range(1, BData.ITMAX + 1):   # DO 800 ITT = 1,ITMAX
			XX2 = X2C[:]
			#for I  in range(1, BData.NC +1 ):   # DO 520 I = 1,BData.NC
			#	XX2[I] = X2C[I]
				
			LXCON =  False
			LV1CON =  False
			# [P4, P5] = self.espar [P1, P2, P3]
			[A1, B1] =  self.espar(0, TT[J], X1 ) 					# CALL espar (0,TT(J),X1, A1, B1)
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[V1, LV1CON] = self.vit (TT[J] , P, A1, B1, V1, LBUB)		# CALL VIT (TT(J), P, A1, B1,V1,LBUB,LV1CON)
			#
			#    IF VOLUME ITERATION HAS NOT CONVERGED, TRY BData.A NEW TEMPERATURE AND
			#    RETURN TO THE BEGINNING OF THE ITERATION
			#
			if (LV1CON  or  LXCON)  :
				TT[J] = 0.95 * TT[J]
				continue # GOTO 800 exit loop
			
			#    COMPUTE CHEMICAL POTENTIALS FOR PHASE 1
			for I in range(1, BData.NC + 1):   # DO 540 I = 1,BData.NC
				U1[I] = self.U_Func (TT[J],X1,I,V1,  A1,B1,  BData.AP,BData.BP,BData.F)
			#
			#    ENTER INNER ITERATION LOOP (FOR COMPOSITION OF PHASE 2)
			#
			#JJ = 1 useless allway one, used once
			LXNEG =  False
			LXPOS =  False
			
			b_python_flag_loop3 = False
			b_python_flag_outer = False
			for IT in range(1, BData.ITMAX + 1):   # DO 600 IT = 1,ITMAX
				LV2CON =  False
				#    COMPUTE EQUATION OF STATE COEFFICIENTS FOR PHASE 2
				# [P4, P5] = self.espar [P1, P2, P3]
				[A2, B2] =  self.espar(0, TT[J], XX2 ) # JJ allway 1	CALL espar (0,TT(J),XX2(JJ), A2, B2)
				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[V2, LV2CON] = self.vit (TT[J], P, A2, B2, V2, not LBUB)		# CALL VIT (TT(J),P, A2, B2, V2,.NOT.LBUB, LV2CON)
				
				#
				#    if VOLUME ITERATION HAS NOT CONVERGED, TRY BData.A NEW TEMPERATURE
				#    AND RETURN TO THE START OF THE TEMPERATURE ITERATION.
				#
				if (LV2CON)  :
					b_python_flag_outer = True
					TT[J] = 0.95 * TT[J]
					continue	# GOTO 800

				#   COMPUTE CHEMICAL POTENTIALS OF PHASE 2

				for I in range(1, BData.NC + 1):  # DO 560 I = 1,BData.NC
					U2[I] = self.U_Func (TT[J], XX2,I,V2,  A2,B2,  BData.AP,BData.BP,BData.F)
				#
				#    CALCULATE THE COMPOSITION OF PHASE 2 FROM THE COMPOSITION
				#    OF PHASE 1 AND THE CHEMICAL POTENTIALS.  THE INNER ITERATION
				#    LOOP HAS CONVERGED WHEN THE CALCULATED COMPOSITION EQUALS
				#    (WITHIN BData.A CONVERGENCE TOLERANCE) THE GUESSED VALUE OF X2.
				#
				FXSUM = 0.0
				C = 0.0

				for I in range(1, BData.NC + 1):  # DO 580 I = 1,BData.NC
					Z[I] = X1[I] * math.exp(U1[I] - U2[I])
					C = C + Z[I]

				for I in range(1, BData.NC + 1):   # DO 584 I = 1,BData.NC
					X2C[I] = Z[I] / C
					FX2[I] = X2C[I] - XX2[I]
					XX2[I] = X2C[I]
					FXSUM = FXSUM + abs(FX2[I])

				if(IT <= 1): FXOLD = 1.0E6

				if (FXSUM < BData.NC * BData.TOLR):
					b_python_flag_loop3 = True
					break 	# GO TO 640

				FXDIF = abs(FXSUM - FXOLD)
				
				if(FXDIF <= 10.0 * BData.TOLR and IT >= BData.ITMAX):
					b_python_flag_loop3 = True
					break 	# GO TO 640

				FXOLD = FXSUM
			# end of loop 	600 CONTINUE
			
			if b_python_flag_outer :
				continue # goto 800
				
			#    IF INNER ITERATION LOOP HAS NOT CONVERGED, SET ERROR FLAG
			if not b_python_flag_loop3:
				print (IT, FXSUM, BData.NC * BData.TOLR,FXSUM / (BData.NC * BData.TOLR), FXDIF)
				LXCON = True
			#
			#    END OF ITERATION LOOP FOR PHASE 2 COMPOSITION

			# point 640 Con
			FT[J] = 1.0 - C
			#
			#    OUTER (TEMPERATURE) ITERATION HAS CONVERGED WHEN BData.C  =  1.000
			#    (I.E. WHEN THE CHEMICAL POTENTIALS OF EACH COMPONENT ARE
			#    THE SAME IN BOTH PHASES).
			#

			if (abs(FT[J]) < 100.0 * BData.TOLR):
				b_python_flag_loop8 = True
				break	#GOTO 840
			#
			#    PROVIDED THAT THE X2 ITERATION HAS CONVERGED FOR THE CURRENT
			#    GUESS OF TEMPERATURE, UPDATE THE POSITIVE AND NEGATIVE
			#    BOUNDS FOR USE WITH THE REGULI - FALSI METHOD.
			#
			if (not LXCON) :
				if (FT[J] < 0.0)  :
					LTNEG = True
					FTNEG = FT[J]
					BData.TNEG = TT[J]
				else:
					LTPOS = True
					FTPOS = FT[J]
					BData.TPOS = TT[J]
			#
			#    COMPUTE NEW GUESS FOR SATURATION TEMPERATURE.
			#    FOR THE SECOND ITERATION, COMPUTE AN APPROXIMATE SATURATION
			#    PRESSURE CORRESPONDING TO THE CURRENT GUESS OF TEMPERATURE
			#    AND ADJUST THE GUESS FOR TEMPERATURE ACCORDING TO THE
			#    DEVIATION BETWEEN THE CALCULATED AND INPUT PRESSURES
			#
			if (ITT <= 2)  :
				if (LBUB)  :
					P2 = P * C
				else:
					P2 = P / C
				
				TT[3] = 1.0 / (1.0 / TT[J] + 0.0004 * (math.log(P2) - math.log(P)))
				
				if (J <= 1)  :
					J = 2
				else:
					TT[1] = TT[2]
					FT[1] = FT[2]
				
				TT[2] = TT[3]
				
			else:
				#    FOR THIRD AND SUBSEQUENT ITERATIONS, USE SECANT / REGULI - FALSI
				TT[3] = TT[2] - FT[2] * (TT[2] - TT[1]) / (FT[2] - FT[1])
				
				if ((TT[3] > max(BData.TNEG,BData.TPOS)  or 	\
					TT[3] < min(BData.TNEG,BData.TPOS))  and  LTNEG  and  LTPOS):
					
					TT[3] = BData.TPOS - FTPOS * (BData.TPOS - BData.TNEG) / (FTPOS - FTNEG)
			 
				TT[1] = TT[2]
				TT[2] = TT[3]
				FT[1] = FT[2]
			
		#
		# End of loop  800 CONTINUE

		if not	b_python_flag_loop8 :
			print (BData.LUP, 'MIXTURE TEMPERATURE ITERATION IN BUBLP DID NOT, CONVERGE')
		
		# point 840
		T = TT[J]
		# 
		#
		#    ASSIGN RESULTS FOR PHASES 1 AND 2 TO LIQUID AND VAPOR PHASES
		#    DEPENDING ON WHETHER THE DEW OR BUBBLE POINT WAS CALCULATED.
		#
		if (LBUB)  :
			#for I in range(1, BData.NC + 1):   # DO 860 I = 1,BData.NC
			#	XV[I] = XX2[I]
			XV = XX2[:]
			VL = V1
			VV = V2
			
		else:
			#for I in range(1, BData.NC + 1):   # DO 880 I = 1,BData.NC
			#	XL[I] = XX2[I]
			XL = XX2[:]
			VL = V2
			VV = V1
		#
		#    PRINT WARNING MESSAGES FOR ANY CASES OF NON - CONVERGENCE OCCURING
		#    ON FINAL CALL TO EACH ITERATION AND RETURN.
		#
		if (abs(1.0 - VL / VV) < BData.TOLR)  :
			LCRIT = True
			print(BData.LUP, 'CRITICAL POINT EXCEEDED IN BUBLP')
		
		if (LV1CON): print (BData.LUP, 'ITERATION IN BUBLP FOR PARENT PHASE VOLUME DID')
		if (LV2CON): print (BData.LUP, 'ITERATION IN BUBLP FOR INCIPIENT PHASE VOLUME DID, NOT CONVERGE')
		if (LXCON) : print (BData.LUP, 'COMPOSITION ITERATION IN BUBLP DID NOT CONVERGE')

		return [XL,XV   ,T,VL,VV,  LCRIT]

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def entrop (self, T,V,X ):

		#	  FUNCTION ENTROP(T,V,X)
		#
		#   COMPUTE SPECIFIC ENTROPY OF A SINGLE PHASE TWO-COMPONENT MIXTURE
		#   AS A FUNCTION OF TEMPERATURE, SPECIFIC VOLUME, AND COMPOSITION
		#
		#   INPUTS:
		#      T - TEMPERATURE (K)
		#      V - SPECIFIC VOLUME (M**3/KG MOL)
		#      X - COMPOSITION (MOLE FRACTION)
		#
		#   OUTPUT:
		#      S - SPECIFIC ENTROPY (KJ/(KG MOL K))
		#
		#   OTHER SUBROUTINES REFERENCED BY ENTROP:
		#      espar - COMPUTATION OF EQUATION OF STATE PARAMETERS
		#
		#
		#	  IMPLICIT REAL (A-H,O-Z)
		#	  DIMENSION X(5)
		#	  COMMON /NCOMP/ NC

		#	  COMMON /ESPAR1/ AP(5),BP(5),F(5,5),   C1,D1,C2,D2
		# similar to COMMON /ESPAR1/ AP(5),BP(5),F(5,5),   DADT,DBDT,D2ADT,D2BDT

		#	  COMMON /HSPURE/ HP(5),SP(5),CP(5)
		#	  COMMON /REF/ TREF(5),HR(5),SR(5),VR(5)
		#	  COMMON /RDATA4/ R
		[A, B] = self.espar (1, T, X)	#  CALL espar (1,T,X   ,A,B)
		
		B4 = 0.25 * B
		S = (BData.DADT * B - A * BData.DBDT)/ (B**2 ) * math.log((V+B)/V)  +  A * BData.DBDT/B/(V+B) \
			-BData.R*B4/(V-B4)**2  *(4.0*V-3*B4)		\
			-BData.R*T*BData.DBDT*0.5*V/(V-B4)**3   *(2.0*V-B4)

		for I in range (1, BData.NC +1): 	#DO 120 I=1,NC
			S = S + X[I] * (BData.SP[I] - BData.SR[I] + BData.R * math.log(V/ BData.VR[I]))

			if (X[I] > 0.0 and  X[I]  < 1.0) :
				S = S - BData.R * X [I] * math.log(X[I])

		return S
	
	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def hcvcps (self, IQ, T, V, X):	# H, CV, CP, VS
		# [P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		# SUBROUTINE HCVCPS (IQ, T, V, X, H, CV, CP, VS)
		#
		#    GIVEN TEMPERATURE, SPECifIC VOLUME AND COMPOSITION COMPUTE ENTHALPY
		#    AND / OR HEAT CAPACITY AT CONSTANT VOLUME AND PRESSURE AS SPECifIED
		#    BY OUTPUT QUALifIER IQ.  (SINGLE PHASE ONLY)
		#
		#    INPUTS:
		#       IQ  -  OUTPUT QUALifIER
		#           =  1 COMPUTE ENTHALPY ONLY
		#           =  2 ENTHALPY AND CONSTANT VOLUME HEAT CAPACITY
		#           =  3 ENTHALPY AND HEAT CAPACITY AT CONSTANT VOLUME AND PRESSURE
		#           =  4 COMPUTE HEAT CAPACITY AT CONSTANT VOLUME ONLY
		#           =  5 HEAT CAPACITY AT CONSTANT VOLUME AND AT CONSTANT PRESSURE
		#       T  -  TEMPERATURE (K)
		#       V  -  SPECIFIC VOLUME (M *  * 3 / KG MOL)
		#       X  -  COMPOSITION (MOLE FRACTION)
		#
		#    OUTPUTS:
		#       H  -  SPECIFIC ENTHALPY (KJ / KG MOL)
		#       CV  -  HEAT CAPACITY AT CONSTANT VOLUME (KJ / (KG MOL K))
		#       CP  -  HEAT CAPACITY AT CONSTANT PRESSURE (KJ / (KG MOL K))
		#
		#    OTHER SUBROUTINES REFERENCED BY HCVCP:
		#       espar  -  COMPUTATION OF EQUATION OF STATE PARAMETERS
		#
		#
		#	  IMPLICIT REAL (A - H,O - Z)
		#	  COMMON  / NCOMP /  BData.NC
		#	  COMMON  / ESPAR1 /  BData.AP(5),BData.BP(5),BData.F(5,5),    C1,D1,C2,D2
		#    simlar to /ESPAR1/        AP(5),BP(5),F(5,5),          DADT,DBDT,D2ADT,D2BDT
		#	  COMMON  / HSPURE /  HP(5),SP(5),BData.CP(5)
		#	  COMMON  / REF /  BData.TREF(5),BData.HR(5),SR(5),VR(5)
		#	  COMMON  / RDATA2 /  BData.WM(5),TC(5)
		#	  COMMON  / RDATA4 /  R
		#	  DIMENSION X(5)
		#X = [0.0] * (5+1)

		[A, B] = self.espar (IQ,T,X)	#CALL espar (IQ,T,X,A,B)
		B4 = 0.25 * B
		VB = V + B
		
		if (V / VB) <= 0 or B<=0:
			print ("hcvcps : bad input parameter, V/(V+B)<= 0 or B<=0")
			print ("     input B for the given X =", B)
			print ("     input Temperature(K) =", T)
			print ('      input X ' , X)
			print ('      input V ' , V)
			print ('Application terminated in python\n')
			sys.exit 	#  ('1030')
			
		VBL = math.log(V / VB)
		VB4 = V - B4
		
		VB43 = VB4**3	#pow(VB4, 3)
		RT = BData.R * T
		
		H  = 0.0 # python in case if IQ =4 H will have no value
		VS = 0.0 # python in case if IQ =4 H will have no value
		CV = 0.0 # python in case if IQ =4 H will have no value
		CP = 0.0 
		
		if (IQ <= 3) :
			#    COMPUTE ENTHALPY AS A FUNCTION OF T, V, X
			H = ( (A + (A * BData.DBDT / B - BData.DADT) * T) * VBL + A * (BData.DBDT * T - B) / VB) / B		\
				+ 2.0 * RT * V * (2.0 * V - B4) * (B4 - 0.25 * BData.DBDT * T) / VB43
				
			for I in range(1, BData.NC + 1):   # DO 120 I = 1,BData.NC
				H = H + X[I] * (BData.HP[I] - BData.HR[I])
		
		if (IQ >= 2) :
			#    COMPUTE CONSTANT VOLUME HEAT CAPACITY
			D12 = BData.DBDT * BData.DBDT
			CV = (BData.R * V * ((0.375 * D12 * T / VB4 	\
				+ 0.5 * BData.D2BDT * T + BData.DBDT) * (B4 - 2.0 * V)	\
				+ 0.125 * D12 * T) / VB43						\
				+ ((1.0 / VB + VBL / B) * (A * BData.D2BDT * B 	\
				+ 2.0 * (BData.DADT * BData.DBDT * B - A * D12)) / B	\
				- BData.D2ADT * VBL - A * D12 / (VB **2)) / B) * T - BData.R

			for I in range( BData.NC + 1):   # DO 160 I = 1,BData.NC
				CV = CV + X[I] * BData.CP[I]

			if (IQ == 3  or  IQ == 5):
				#    COMPUTE SPECIFIC HEAT AT CONSTANT PRESSURE USING CV
				Y = B4 / V
				DPDT = 2.0 * BData.R / VB4 * ( - 1.0 + ( - 0.25 * T * BData.DBDT + (V * V * (1.0 + 0.75 * T * BData.DBDT / VB4))		\
					/ VB4) / VB4) + (BData.R + ( - BData.DADT + A * BData.DBDT / VB) / VB) / V

				#DPDV = ( - RT * (1.0 + (4.0 + (4.0 + ( - 4.0 + Y) * Y) * Y) * Y) / (pow(1.0 - Y,4))	\
				#	+ A * (2.0 * V + B) / (VB *VB)) / (V * V )
					
				DPDV = ( - RT * (1.0 + (4.0 + (4.0 + ( - 4.0 + Y) * Y) * Y) * Y) / (   (1.0 - Y)**4   )	\
					+ A * (2.0 * V + B) / (VB *VB)) / (V * V )

				CP = CV - DPDT * DPDT * T / DPDV
				
				# COMPUTE VELOCITY OF SOUND USING C'S AND VOLUME DERIVATIVE OF P
				WMOL = 0.0
				for I in range(1, BData.NC+1 ):   # DO 180 I = 1,BData.NC
					WMOL = WMOL + X[I] * BData.WM[I]
				
				VS = V * math.sqrt( - 1000.0 * CP * DPDV / (WMOL * CV))

		return [H, CV, CP, VS]

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def vit (self, T, P, para_A, para_B, VS, LLIQI): # VS, LVCON
		# [P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
		#		  SUBROUTINE VIT (T,P,para_A,para_B,VS,LLIQI,LVCON)
		#
		#    DEVELOPED BY MARK MCLINDEN AND GRAHAM MORRISON AT THE
		#    NATIONAL BUREAU OF STANDARDS UNDER FUNDING FROM THE ELECTRIC
		#    POWER RESEARCH INSTITUTE AND NBS.
		#
		#    GIVEN TEMPERATURE, PRESSURE, AND EQUATION OF STATE
		#    PARAMETERS, THIS ROUTINE CALCULATES THE LIQUID OR VAPOR
		#    MOLAR VOLUME THAT SATISFIES THE EQUATION OF STATE.
		#
		#    INPUTS:
		#       T  -  TEMPERATURE (K)
		#       P  -  PRESSURE (KPA)
		#       para_A,para_B  -  EQUATION OF STATE PARAMETERS AT TEMPERATURE T
		#       VS  -  INITIAL GUESS FOR VOLUME.  IN ABSENCE OF BETTER
		#          GUESSES SUGGESTED VALUES ARE:
		#          LIQUID:  VS = 0.8 * para_B
		#          VAPOR:   VS = R * T / P
		#       LLIQI  -  LOGICAL VARIABLE
		#          if LLIQI  =  True COMPUTE LIQUID VOLUME
		#          if LLIQI  =   False  COMPUTE VAPOR VOLUME
		#       NOTE:  if EITHER THE TEMPERATURE OR THE PRESSURE IS ABOVE
		#          THE CRITICAL VALUE, ONLY ONE SOLUTION EXISTS AND THE
		#          VALUE OF LLIQI HAS NO EFFECT.
		#
		#    OUTPUTS:
		#       VS  -  MOLAR VOLUME (M *  * 3 / KG MOL)
		#       LVCON  -  ERROR FLAG; if LVCON  =  True THE ITERATION HAS
		#          NOT CONVERGED
		#
		#    OTHER SUBROUTINES REFERENCED:
		#       NONE
		#
		#    (FOR EXPLANATION OF NOMENCLATURE SEE BUBLT)
		#
		#    NOTE:  THIS ROUTINE IS WRITTEN IN DOUBLE PRECISION EXCEPT
		#       THAT THE ARGUMENTS ARE SINGLE PRECISION
		#
		#	  IMPLICIT DOUBLE PRECISION (para_A - H,O - Z)
		#	  LOGICAL LLIQ,LVCON,LLIQI
		#	  REAL T,P, para_A, para_B, R, VS, TOLR, TC, PC
		#	  COMMON  / RDATA4 /  R
		#	  COMMON  / TOL /  TOLR,ITmath.max,LUP
		
		if para_B <= 0 or VS <=0:
			print ("vit : bad input parameter, B <= 0, B=",para_B)
			print ("vit : bad input parameter, VS <= 0, VS=",VS)
			print ("     input Temperature(K) =", T, " pressure(kPa)=", P )
			print ('Application terminated in python\n')
			sys.exit	#('1020')
			
		LVCON =  False
		LLIQ = LLIQI
		V = VS
				
		VL = math.log(V)
		PL = math.log(P)
		RT = BData.R * T
		B4 = 0.25 * para_B
		
		B4L = math.log(B4)
			
		if (VL < B4L):
			VL = B4L + 0.5
		
		
		TC = para_A / (para_B * 4.398909 * BData.R)

		PC = 0.02386944 * para_A / pow(para_B, 2)

		VCL = math.log(12.0272727 * B4)

		if (P > PC) :
			LLIQ = True
		elif (T > TC) :
			LLIQ = False

		#
		#    ENTER NEWTONS METHOD ITERATION FOR VOLUME.  FOR LIQUIDS
		#    (OR FLUIDS ABOVE THE CRITICAL PRESSURE) THE ITERATION
		#    IS CARRIED OUT IN TRANSFORMED COORDINATES OF LOG (V). FOR
		#    VAPOR (OR FLUIDS AT SUPERCRITICAL TEMPERATURES BUT PRESSURES
		#    BELOW THE CRITICAL VALUE) THE ITERATION IS IN TERMS OF
		#    LOG (V) AND LOG (P).  THE ITERATION HAS CONVERGED WHEN
		#    THE PRESSURE CALCULATED FROM THE EQUATION OF STATE AGREES
		#    WITH THE INPUT PRESSURE.
		#
		for IT in range(1, BData.ITMAX + 1 ):   # DO 100 IT = 1,ITmath.max
			if (  ( (VL > VCL) == LLIQ ) and  P < PC):
				VL = VCL
			VLS = VL
			Y = B4 / V
			VB = V + para_B
			#
			#    CALCULATE PRESSURE AS para_A FUNCTION OF VOLUME AND THE
			#    DERIVATIVE OF THE PRESSURE W.R.T. LOG (VOLUME).
			#
			P2=( RT * (1.0+(1.0+(1.0-Y)*Y)*Y) / (1.0-Y)**3-para_A/VB)/V
			DPDLV = RT / V * ( - 1.0 + ( - 4.0 + ( - 4.0 + (4.0 - Y) * Y) * Y) * Y) / ( pow (1.0 - Y, 4) )	\
					+ para_A * (2.0 * V + para_B) / (V * VB * VB)

			if (LLIQ) :
				if (DPDLV >= 0.0) :
					VL = 0.5 * (B4L + VLS)
				else:
					FVDP = (P2 - P) / DPDLV
					#print ("Ayman Check .. need to be 100* TOLR, this value is not possible" )
					#if (abs(FVDP / P) < 0.001 * BData.TOLR) :
					if (abs(FVDP / P) <  BData.TOLR) :
						VS = math.exp(VL)
						return [VS, LVCON]
					else:
						VL = VL - FVDP
						if (VL <= B4L):
							VL = 0.5 * (B4L + VLS)
			else:
				if (DPDLV >= 0.0  or  P2 <= 0.0)  :
					VL = VL + 0.5
				else:
					FVDPL = (math.log(P2) - PL) * P2 / DPDLV
					if (abs(FVDPL) < 0.001 * BData.TOLR)  :
						VS = math.exp(VL)
						return	[VS, LVCON]

					VL = VL - FVDPL
					if (abs(VL - VLS) > 1.5):
						#----------------------------------------------
						if VL >= VLS :	#VL = VLS + SIGN(1.0D0,VL - VLS)
							VL = VLS + 1
						else:
							VL = VLS - 1
						#----------------------------------------------
					if (VL < VCL):
						VL = 0.5 * (VLS + VCL)
			V = math.exp(VL)
			# end of loop 100 ==================== CONTINUE
		LVCON = True
		VS = V
		return [VS, LVCON]

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def spxsp (self, S,P,X, TSAT,SSAT,VSAT, LLIQ) : # ,T2,V,LLIQ
		#  [T2,V] = spxsp (self, S,P,X,TSAT,S ,LLIQ)
		#	[p4,p8]= self.spxsp(p1 to p6, p9)
		#  SUBROUTINE SPXSP (S,P,X,  TSAT,SSAT,VSAT,  T2,V,LLIQ)
		#
		#    SUBROUTINE TO ITERATE FOR THE TEMPERATURE OF A SINGLE PHASE
		#    BINARY MIXTURE GIVEN ENTHALPY, PRESSURE, AND COMPOSITION.
		#    CALLED ONLY BY SPIN.

		#  COMMON  / TOL /  TOLR,ITmath.max,LUP
		#  COMMON  / TOLSH /  TOLH,TOLS
		
		LVCON = False

		T1 = TSAT
		FT1 = S - SSAT

		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[HSAT, CV, CP, VSND] = self.hcvcps (4, T1, VSAT, X ) # CALL HCVCPS (4,T1,VSAT,X,   HSAT,CV,CP,VSND)

		SSAT = self.entrop (T1,VSAT,X)
		T2 = TSAT + FT1 / CV
		V = VSAT

		for IT in range(1, BData.ITMAX + 1 ):   # DO 200 IT = 1,ITmath.max
			# [P4, P5] = self.espar [P1, P2, P3]
			[A, B] = self.espar (2, T2, X )					#CALL espar (2,T2,X, A, B)
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[V, LVCON] = self.vit (T2, P, A, B,  V, LLIQ)	# CALL VIT (T2,P, A, B,V,     LLIQ,LVCON)

			SSP = self.entrop (T2,V,X)
			FT2 = S - SSP
			if (abs(FT2) < BData.TOLS  or  abs(FT2 - FT1) < 0.02 * BData.TOLS):
				return [T2, V]

			T3 = T2 - FT2 * (T2 - T1) / (FT2 - FT1)
			T1 = T2
			T2 = T3
			FT1 = FT2

		print (BData.LUP, 'SINGLE PHASE ITERATION IN SPIN DID NOT CONVERGE' )
		return [T2, V]

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def hpxsp (self,H, P, X, TSAT, HSAT, VSAT, LLIQ): # T2,V, LLIQ
		# [T2,V] = self.hpxsp (H,P,X,  TSAT, LLIQ)
		# [P7,P8] = self.hpxsp (P1,P2,P3, P4,P5,P6, P9 )
		#		  SUBROUTINE HPXSP (H,P,X,  TSAT,HSAT,VSAT,  T2,V,LLIQ)
		#
		#   SUBROUTINE TO ITERATE FOR THE TEMPERATURE OF A SINGLE PHASE
		#   BINARY MIXTURE GIVEN ENTHALPY, PRESSURE, AND COMPOSITION.
		#   CALLED ONLY BY HPIN.
		
		#	  IMPLICIT REAL (A-H,O-Z)
		#	  DIMENSION X(5)
		#	  LOGICAL LLIQ,LVCON
		#	  COMMON /TOL/ TOLR,ITMAX,LUP
		#	  COMMON /TOLSH/ TOLH,TOLS
		
		
		T1 = TSAT
		FT1 = H - HSAT
		
		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[HSAT, CV, CP, VSND] = self.hcvcps ( 5, T1, VSAT, X ) # CALL HCVCPS (5,T1,VSAT,X,    HSAT ,CV,CP,VSND)

		T2 = TSAT + FT1 / CP
		V  = VSAT

		for IT in range (1, BData.ITMAX + 1) : # DO 200 IT=1,ITMAX
			# [P4, P5] = self.espar [P1, P2, P3]
			[A, B] =  self.espar(2, T2, X ) #CALL espar (2,T2,X,A,B)
			
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[V, LVCON ] = self.vit (T2,P,A,B,V,   LLIQ)	#CALL VIT (T2,P,A,B,V,   LLIQ,LVCON)
			
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HSP, CV, CP, VSND] = self.hcvcps ( 1, T2, V, X ) 	# CALL HCVCPS (1,T2,V,X,         HSP,CV,CP,VSND)

			FT2 = H - HSP
			
			if (abs(FT2) < BData.TOLH  or  abs(FT2-FT1) < 0.02 * BData.TOLH):
				return [T2,V]

			T3 = T2 - FT2 * (T2-T1) / (FT2-FT1)
			T1 = T2
			T2 = T3
			FT1 = FT2

		print (BData.LUP,'SINGLE PHASE ITERATION IN HPIN DID NOT CONVERGE')
		return [T2,V]

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def spin (self, S,P,X ): # T,XQ,XL,XV,VL,VV,SL,SV
		# [P4 .. P11] = self.spin (P1, P2, P3 )
		# [T,XQ,XL,XV,VL,VV,SL,SV] = self.spin (self, S,P,X )
		#	  SUBROUTINE SPIN (S,P,X,T,XQ,XL,XV,VL,VV,SL,SV)
		#
		#    DEVELOPED BY MARK MCLINDEN AND GRAHAM MORRISON AT THE
		#    NATIONAL BUREAU OF STANDARDS WITH FUNDING FROM THE ELECTRIC
		#    POWER RESEARCH INSTITUTE AND NBS.
		#
		#    THIS ROUTINE CALCULATES THE TEMPERATURE AND QUALITY OF .A SINGLE
		#    OR TWO - PHASE MIXTURE GIVEN THE PRESSURE AND THE OVERALL (BULK)
		#    ENTROPY AND COMPOSITION
		#
		#    INPUTS:
		#       S  -  ENTROPY (KJ / KMOL K) OF BULK MIXTURE
		#       P  -  PRESSURE (KPA)
		#       X  -  BULK COMPOSITION (MOLE FRACTION)
		#
		#    OUTPUTS:
		#       T  -  EQUILIBRIUM TEMPERATURE OF SYSTEM (K)
		#       XQ  -  EQUILIBRIUM QUALITY (MOLAR BASIS), NEGATIVE VALUES INDICATE
		#         SUBCOOLED LIQUID.  QUALITIES > 1 INDICATE SUPERHEATED VAPOR.
		#         NOTE:  THE QUALITIES COMPUTED FOR SINGLE PHASE CONDITIONS IN
		#         THE ROUTINES HPIN, HTIN, SPIN, AND VTIN WILL NOT BE THE SAME.
		#       XL  -  COMPOSITION OF LIQUID PHASE (MOLE FRACTION)
		#       XV  -  COMPOSITION OF VAPOR PHASE (MOLE FRACTION)
		#       VL  -  MOLAR VOLUME OF LIQUID PHASE (M**3/KMOL)
		#       VV  -  MOLAR VOLUME OF VAPOR PHASE (M**3/KMOL)
		#       SL  -  MOLAR ENTHALPY OF LIQUID PHASE (KJ/KMOL K)
		#       SV  -  MOLAR ENTHALPY OF VAPOR PHASE (KJ/KMOL K)
		#       NOTE:  if ONLY SUBCOOLED LIQUID IS PRESENT, VV AND HV ARE
		#         COMPUTED AT THE BUBBLE POINT TEMPERATURE AND VAPOR COMPOSITION
		#         IN EQUILIBRIUM WITH SATURATED LIQUID AT THE GIVEN P, XL.
		#         if ONLY SUPERHEATED VAPOR IS PRESENT, VL AND HV ARE
		#         COMPUTED AT THE DEWPOINT TEMPERATURE AND LIQUID
		#         COMPOSITION IN EQUILIBRIUM WITH SATURATED VAPOR AT THE
		#         GIVEN P, XV.
		#
		#	  IMPLICIT REAL (BData.A - H,O - Z)
		#	  LOGICAL LCRIT,LCONV
		#	  DIMENSION X(5),XL(5),XV(5),XLB(5),XVB(5),XLD(5),XVD(5)
		#	  COMMON  / NCOMP /  BData.NC
		#	  COMMON  / TOL /  TOLR,ITMAX,LUP
		#	  COMMON  / TOLSH /  TOLH,TOLS
		#
		#    COMPUTE BUBBLE AND DEW POINTS AT GIVEN CONDITIONS OF P AND X
		#
		LCRIT = False
		LCONV = False
		T=0.0
		XL = [0.0] * (5+1)
		XV = [0.0] * (5+1)

		XLB= [0.0] * (5+1)
		XVB= [0.0] * (5+1)
        
		XLD= [0.0] * (5+1)
		XVD= [0.0] * (5+1)
		
		#VVBUB = VVBUB # in Python only
		
		
		# [P2, P3 ,P4, P5, P6, P8] = self.bublp (P1, P2, P3 , P7 )
		[_, XV, TBUB, VL, VVBUB, LCRIT] = self.bublp ( P, X, XV, True) 	# CALL BUBLP (P,X,XV,  TBUB, VL, VVBUB,    True,LCRIT) 
		[XL,_,  TDEW, VLDEW, VV, LCRIT] = self.bublp ( P, XL, X, False)	# CALL BUBLP (P,XL,X,    TDEW,VLDEW,VV,   False ,LCRIT)
		
		VLBUB = VL # in Python only # in Python only
		
		SL = self.entrop (TBUB, VL, X)
		SV = self.entrop (TDEW, VV, X)
		
		#        WRITE ( * , * ) 'TBUB,TDEW,SLBUB,SVDEW   ',TBUB,TDEW,SL,SV
		if (S <= SL)  :
			#    SINGLE PHASE LIQUID
			#
			XL = X[:]
			#for I in range(1, BData.NC + 1):   # DO 120 I = 1,BData.NC
			#	XL[I] = X[I]
				
			VV = VVBUB
			VLBUB = VL
			#[p7,p8,p9]= self.spxsp(p1 to p6)
			[T,VL]= self.spxsp(S,P,X, TBUB,SL,VLBUB, True)
			#CALL SPXSP (S,P,X,  TBUB,SL,VLBUB,  T,VL,  .TRUE.)
				
		elif (S >= SV) :
			#    SINGLE PHASE VAPOR
			#
			XV = X[:]
			#for I in range(1,BData.NC):   # DO 140 I = 1,BData.NC
			#	XV[I] = X[I]
				
			VL = VLDEW
			VVDEW = VV
			
			[T,VV]= self.spxsp(S,P,X, TDEW,SV,VVDEW, False)
			# CALL SPXSP (S,P,X,   TDEW,SV,VVDEW,   T,VV,.FALSE.)
			
		else:
			#    TWO PHASE
			#
			NCC = BData.NC
			for I in range(1, BData.NC + 1):   # DO 210 I = 1,BData.NC
				if (X[I] < BData.TOLR):
					NCC = NCC - 1
					
				XLB[I] = X[I]
				XVB[I] = XV[I]
				XLD[I] = XL[I]
				XVD[I] = X[I]
				#210   CONTINUE
				
			SLB = SL
			SVD = SV
			SVB = self.entrop (TBUB,VVBUB,XVB)
			SLD = self.entrop (TDEW,VLDEW,XLD)
			
			b_python_flag = False
			for IT in range(1, int(BData.ITMAX/2) + 1):	# DO 260 IT = 1,ITMAX / 2
				#
				#    COMPUTE QUALITY BASED ON ENTHALPY; COMPUTED SEPARATELY FOR
				#    BUBBLE AD DEW POINT CONDITIONS.
				#
				XQSB = (S - SLB) / (SVB - SLB)
				XQSD = (S - SLD) / (SVD - SLD)
				#
				#    COMPUTE LENGTH OF TIE LINE CONNECTING LIQUID AND VAPOR
				#    COMPOSITIONS AND QUALITIES BASED ON COMPOSITION.
				#
				IXQCB = 0
				IXQCD = 0
				XQCB = 0.0
				XQCD = 0.0
				TLINEB = 0.0
				TLINED = 0.0
				
				for I in range(1, BData.NC + 1):   # DO 214 I = 1,BData.NC
					if (XLB[I] != XVB[I]):
						IXQCB = IXQCB + 1
						XQCB  = XQCB  + ( X[I] - XLB[I] ) / ( XVB[I] - XLB[I] )
					
					TLINEB = TLINEB + ( XVB[I] - XLB[I] ) **2
					
					if (XLD[I] !=  XVD[I] ) :
						IXQCD = IXQCD + 1
						XQCD = XQCD + ( X[I] - XLD[I] ) / ( XVD[I] - XLD[I] )
					
					TLINED = TLINED + (XVD[I] - XLD[I] ) **2
					#214   CONTINUE
					
				if (IXQCB > 0) :
					#XQCB = XQCB / REAL(IXQCB)
					XQCB = XQCB / (IXQCB + 0.0)
					
				else:
					XQCB = XQSB
				
				if (IXQCD > 0):
					#XQCD = XQCD / REAL(IXQCD)
					XQCD = XQCD / (IXQCD + 0.0)
				else:
					XQCD = XQSD
				
				TLINEB = math.sqrt(TLINEB)
				TLINED = math.sqrt(TLINED)
				#
				#    CHECK FOR CONVERGENCE
				#
				LCONV = True
				for I in range(1, BData.NC + 1):   # DO 220 I = 1,BData.NC
					if ( abs ( XVB[I] - XVD[I] ) > 0.0001):
						LCONV =  False
					
				if (abs(XQCB - XQSB) > 0.0001):
					LCONV =  False
					
				if (abs(XQCD - XQSD) > 0.0001):
					LCONV =  False
					
				if (LCONV):
					b_python_flag = True
					break	# GOTO 280
				#
				#    ASSIGN WEIGHTS TO BUBBLE AND DEW POINT CALCULATION
				#
				if (IT <= 1  or  NCC <= 2)  :
					WTB = 1.0 + (XQCB - XQSB) / ((XQCD - XQSD) - (XQCB - XQSB))
					WTD = 1.0 - (XQCD - XQSD) / ((XQCD - XQSD) - (XQCB - XQSB))
					
				else:
					DTSUMB = 0.0
					DTSUMD = 0.0
					
					for J in range(1, (BData.NC-1) + 1):  		# DO 222 J = 1,BData.NC - 1
						for K in range(  J+1 , BData.NC +1 ):		# DO 222 K = J + 1,BData.NC
							DTSUMB = DTSUMB + ( ( X[J] - XLB[J] ) * ( XVB[K] - XLB[K] ) 	\
								- ( X[K] - XLB[K] ) * ( XVB[J] - XLB[J] ) ) ** 2
								
							DTSUMD = DTSUMD + ( ( X[J] - XLD[J] ) * ( XVD[K] - XLD[K] )		\
								- ( X[K] - XLD[K] ) * ( XVD[J] - XLD[J] ) ) ** 2
							# 222 CONTINUE
			  
					DXBUB = 0.0
					DXDEW = 0.0
					
					for I in range( 1, BData.NC +1 ):   # DO 223 I = 1,BData.NC
						DXBUB = DXBUB + ( XLB[I] - XVB[I]) ** 2
						DXDEW = DXDEW + ( XLD[I] - XVD[I]) ** 2
			  
					DXBUB = math.sqrt(DTSUMB / DXBUB)
					DXDEW = math.sqrt(DTSUMD / DXDEW)
					
					WTB   = DXDEW / (DXDEW + DXBUB)
					WTD   = DXBUB / (DXBUB + DXDEW)
				#END if

				if (TLINEB / TLINED > 2.0  or  TLINEB / TLINED < 0.5)  :
					TLAVG = min(TLINEB, TLINED)
					
				else:
					TLAVG = (WTB * TLINEB + WTD * TLINED)
				
				#
				#    COMPUTE NEXT GUESSES FOR COMPOSITION OF LIQUID AND VAPOR
				#    AND CARRY OUT CORRESPONDING BUBBLE AND DEW POINT CALCULATIONS.
				#
				
				for I in range(1, BData.NC + 1):   # DO 224 I = 1,BData.NC
					XLB2I = X[I] + TLAVG * (WTB * XQSB + WTD * XQSD)	\
						* (WTB * (XLB[I] - XVB[I]) / TLINEB + WTD * (XLD[I] - XVD[I]) / TLINED)
						
					XVD2I = X[I] - TLAVG * (WTB * (1.0 - XQSB) + WTD * (1.0 - XQSD))	\
						* (WTB * (XLB[I] - XVB[I]) / TLINEB + WTD * (XLD[I] - XVD[I]) / TLINED)
				 
					XLB[I] = XLB2I
					XVD[I] = XVD2I
					# 224   CONTINUE
				
				# [P2, P3 ,P4, P5, P6, P8] = self.bublp (P1, P2, P3 , P7 )
				[_, XVB, TBUB, VLBUB, VVBUB, LCRIT] = self.bublp ( P, XLB, XVB, True) # CALL BUBLP (P,XLB,XVB,   TBUB,VLBUB,VVBUB,  True,  LCRIT)
				[XLD, _, TDEW, VLDEW, VVDEW, LCRIT] = self.bublp ( P, XLD, XVD, False)# CALL BUBLP (P,XLD,XVD,   TDEW,  VLDEW,VVDEW,  False ,LCRIT)
				
				SLB = self.entrop (TBUB,VLBUB, XLB)
				SVB = self.entrop (TBUB,VVBUB, XVB)
				SLD = self.entrop (TDEW,VLDEW, XLD)
				SVD = self.entrop (TDEW,VVDEW, XVD)

				# 260   CONTINUE
				
			# WRITE (LUP,1000) S,P,   (X(I),I = 1,BData.NC)
			if not b_python_flag :
				print (BData.LIP, "ROUTINE SPIN DID NOT CONVERGE S, P, X",S, P, X[1:BData.NC] )
			
			# 280	CONTINUE
			
			#
			#    SOLUTION HAS CONVERGED; WRITE OUTPUT VARIABLES.
			#
			XL = XLB[:]
			XV = XVB[:]
			
			#for I in range(1,BData.NC):   # DO 284 I = 1,BData.NC
			#	XL[I] = XLB[I]
			#	XV[I] = XVB[I]
		
			VL = VLBUB
			VV = VVBUB
			SL = SLB
			SV = SVB
			T = TBUB
			
			# ---END if
		XQ = (S - SL) / (SV - SL)
		return [T, XQ,XL,XV, VL,VV, SL,SV]

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def hpin (self, H,P,X ):
		# [T, XQ,XL,XV,VL,VV,HL,HV] = hpin ( H,P,X )
		# [P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
		# SUBROUTINE HPIN (H,P,X,   T,XQ,XL,XV,VL,VV,HL,HV)
		#
		#   DEVELOPED BY MARK MCLINDEN AND GRAHAM MORRISON AT THE
		#   NATIONAL BUREAU OF STANDARDS WITH FUNDING FROM THE ELECTRIC
		#   POWER RESEARCH INSTITUTE AND NBS.
		#
		#   THIS ROUTINE CALCULATES THE TEMPERATURE AND QUALITY OF A SINGLE
		#   OR TWO-PHASE MIXTURE GIVEN THE PRESSURE AND THE OVERALL (BULK)
		#   ENTHALPY AND COMPOSITION
		#
		#   INPUTS:
		#      H - ENTHALPY (KJ/KMOL) OF BULK MIXTURE
		#      P - PRESSURE (KPA)
		#      X - BULK COMPOSITION (MOLE FRACTION)
		#
		#   OUTPUTS:
		#      T - EQUILIBRIUM TEMPERATURE OF SYSTEM (K)
		#      XQ - EQUILIBRIUM QUALITY (MOLAR BASIS), NEGATIVE VALUES INDICATE
		#        SUBCOOLED LIQUID.  QUALITIES > 1 INDICATE SUPERHEATED VAPOR.
		#        NOTE:  THE QUALITIES COMPUTED FOR SINGLE PHASE CONDITIONS IN
		#        THE ROUTINES HPIN, HTIN, SPIN, AND VTIN WILL NOT BE THE SAME.
		#      XL - COMPOSITION OF LIQUID PHASE (MOLE FRACTION)
		#      XV - COMPOSITION OF VAPOR PHASE (MOLE FRACTION)
		#      VL - MOLAR VOLUME OF LIQUID PHASE (M**3/KMOL)
		#      VV - MOLAR VOLUME OF VAPOR PHASE (M**3/KMOL)
		#      HL - MOLAR ENTHALPY OF LIQUID PHASE (KJ/KMOL)
		#      HV - MOLAR ENTHALPY OF VAPOR PHASE (KJ/KMOL)
		#      NOTE:  IF ONLY SUBCOOLED LIQUID IS PRESENT, VV AND HV ARE
		#        COMPUTED AT THE BUBBLE POINT TEMPERATURE AND VAPOR COMPOSITION
		#        IN EQUILIBRIUM WITH SATURATED LIQUID AT THE GIVEN P, XL.
		#        IF ONLY SUPERHEATED VAPOR IS PRESENT, VL AND HL ARE
		#        COMPUTED AT THE DEWPOINT TEMPERATURE AND LIQUID
		#        COMPOSITION IN EQUILIBRIUM WITH SATURATED VAPOR AT THE
		#        GIVEN P, XV.
		#
	
		#LOGICAL LCRIT,LCONV
		#DIMENSION X(5),XL(5),XV(5),XLB(5),XVB(5),XLD(5),XVD(5)
		#COMMON /NCOMP/ NC
		#COMMON /TOL/ TOLR,ITMAX,LUP
		#COMMON /TOLSH/ TOLH,TOLS
		#
		#   COMPUTE BUBBLE AND DEW POINTS AT GIVEN CONDITIONS OF P AND X
		#
		
		#X   = [[0.0] * (5+1)]
		XL  = [0.0] * (5+1)
		XV  = [0.0] * (5+1)
		
		XLB = [0.0] * (5+1)
		XVB = [0.0] * (5+1)
		
		XLD = [0.0] * (5+1)
		XVD = [0.0] * (5+1)
		
		# [P2, P3, P4, P5, P6, P8] = bublp ( P1, P2, P3,    P7)
		[X, XV, TBUB, VL, VVBUB, LCRIT] = self.bublp (P, X, XV, True)  # CALL BUBLP (P,X,XV  ,TBUB, VL,VVBUB,  .TRUE.,LCRIT)		
		[XL,X,  TDEW, VLDEW, VV, LCRIT] = self.bublp (P, XL, X, False) # CALL BUBLP (P,XL,X,  TDEW,VLDEW,VV,.FALSE.,LCRIT)
		
		VLBUB = VL # in Python only
		
		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[HL, CV, CP, VSND] = self.hcvcps( 1, TBUB, VL, X )	#  CALL HCVCPS (1,TBUB,VL,X, HL,CV,CP,VSND)
		[HV, CV, CP, VSND] = self.hcvcps( 1, TDEW, VV, X )	#  CALL HCVCPS (1,TDEW,VV,X ,HV,CV,CP,VSND)
		
		if (H <= HL) :
			#   SINGLE PHASE LIQUID
			#for I in range(1, BData.NC + 1): # DO 120 I=1,NC
			#	XL[I] = X[I]

			XL = X[:]
			VV    = VVBUB
			VLBUB = VL
			
			#[P7,P8] = self.hpxsp (P1,P2,P3, P4,P5,P6, P9)
			[T, VL] =self.hpxsp (H, P, X,  TBUB, HL, VLBUB, True) #CALL HPXSP (H,P,X,TBUB,HL,VLBUB,T,VL,.TRUE.)
			
		elif (H >= HV) :
			#   SINGLE PHASE VAPOR
			#for I in range(1, BData.NC + 1): # DO 140 I=1,NC
			#	XV[I] = X[I]

			XV = X[:]
			VL   = VLDEW
			VVDEW= VV
			
			#[P7,P8] = self.hpxsp (P1,P2,P3, P4,P5,P6,P9 )
			[T, VV]  = self.hpxsp (H, P, X, TDEW, HV, VVDEW, False) #CALL HPXSP (H,P,X,TDEW,HV,VVDEW,T,VV,.FALSE.)
			
		else:

			#   TWO PHASE
			NCC = BData.NC
			for I in range(1, BData.NC + 1): # DO 210 I=1,NC
				if (X[I] < BData.TOLR): NCC = NCC-1
				
				XLB[I] = X [I]
				XVB[I] = XV[I]
				
				XLD[I] = XL[I]
				XVD[I] = X [I]
				
			HLB = HL
			HVD = HV
			
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HVB, CV, CP, VSND] = self.hcvcps( 1, TBUB, VVBUB, XVB ) # CALL HCVCPS (1,TBUB,VVBUB,XVB  ,HVB,CV,CP,VSND)
			[HLD, CV, CP, VSND] = self.hcvcps( 1, TDEW, VLDEW, XLD)  # CALL HCVCPS (1,TDEW,VLDEW,XLD  ,HLD,CV,CP,VSND)
			
			b_python_flag = False
			for IT in range(1, int(BData.ITMAX/2 + 1)): #DO 260 IT=1,ITMAX/2
				#   COMPUTE QUALITY BASED ON ENTHALPY; COMPUTED SEPARATELY FOR
				#   BUBBLE AD DEW POINT CONDITIONS.
				#
				XQHB = (H-HLB) / (HVB-HLB)
				XQHD = (H-HLD) / (HVD-HLD)
				#
				#   COMPUTE LENGTH OF TIE LINE CONNECTING LIQUID AND VAPOR
				#   COMPOSITIONS AND QUALITIES BASED ON COMPOSITION.
				#
				IXQCB = 0
				IXQCD = 0
				XQCB  = 0.0
				XQCD  = 0.0
				TLINEB= 0.0
				TLINED= 0.0
				
				for I in range(1, BData.NC + 1): #DO 214 I=1,NC
					if (XLB[I] != XVB[I] ):
						IXQCB = IXQCB + 1
						XQCB  = XQCB  +( X[I] - XLB[I] ) / ( XVB[I] - XLB[I] )

					TLINEB = TLINEB + ( XVB[I] - XLB[I] ) **2
					
					if (XLD[I] != XVD[I] ) :
						IXQCD = IXQCD + 1
						XQCD  = XQCD  + ( X[I] - XLD[I] ) / ( XVD[I] - XLD[I] )
						
					TLINED = TLINED + (XVD[I] - XLD[I] ) **2
					
				if (IXQCB > 0) :
					XQCB = XQCB / (IXQCB )
					
				else:
					XQCB=XQHB
				
				if (IXQCD > 0) :
					XQCD=XQCD / (IXQCD )
					
				else:
					XQCD=XQHD
				
				TLINEB = math.sqrt(TLINEB)
				TLINED = math.sqrt(TLINED)
				#
				#   CHECK FOR CONVERGENCE
				#
				LCONV = True
				for I in range (1, BData.NC + 1): # DO 220 I=1,NC
					if (abs(XVB[I] - XVD[I] ) > 0.0001): LCONV = False
					
				if (abs(XQCB-XQHB) > 0.0001): LCONV = False
				if (abs(XQCD-XQHD) > 0.0001): LCONV = False
				
				if (LCONV):
					b_python_flag = True
					break	#GOTO 280
				
				#
				#   ASSIGN WEIGHTS TO BUBBLE AND DEW POINT CALCULATION
				#
				if (IT < 1  or  NCC < 2) :
					WTB = 1.0 + (XQCB-XQHB) / ( (XQCD-XQHD) - (XQCB-XQHB) )
					WTD = 1.0 - (XQCD-XQHD) / ( (XQCD-XQHD) - (XQCB-XQHB) )
				else:
					DTSUMB = 0.0
					DTSUMD = 0.0
					
					for J in range (1, (BData.NC-1) + 1):	#DO 222 J=1,NC-1
						for K in range (J+1, (BData.NC + 1) ): #DO 222 K=J+1,NC
							DTSUMB = DTSUMB + ((X[J] - XLB[J] ) * ( XVB[K] - XLB[K])	\
								- (X[K] - XLB[K]) * (XVB[J] - XLB[J] ) )**2
								
							DTSUMD = DTSUMD + ((X[J] - XLD[J] ) * ( XVD[K] - XLD[K])	\
								- (X[K] - XLD[K]) * (XVD[J] - XLD[J] ) )**2
						
					DXBUB = 0.0
					DXDEW = 0.0
					
					for I in range(1, BData.NC + 1): #DO 223 I=1,NC
						DXBUB = DXBUB + ( XLB[I] - XVB[I] ) **2
						DXDEW = DXDEW + ( XLD[I] - XVD[I] ) **2
						
					DXBUB = math.sqrt(DTSUMB/DXBUB)
					DXDEW = math.sqrt(DTSUMD/DXDEW)
					WTB   = DXDEW / (DXDEW + DXBUB)
					WTD   = DXBUB / (DXBUB + DXDEW)
					#============== END IF
				
				if (TLINEB/TLINED > 2.0  or  TLINEB/TLINED < 0.5) :
					TLAVG = min(TLINEB,TLINED)
					
				else:
					TLAVG = (WTB*TLINEB+WTD*TLINED)
				
				#
				#   COMPUTE NEXT GUESSES FOR COMPOSITION OF LIQUID AND VAPOR
				#   AND CARRY OUT CORRESPONDING BUBBLE AND DEW POINT CALCULATIONS.
				#
				
				for I in range(1,BData.NC + 1): # DO 224 I=1,NC
					XLB2I = X[I] + TLAVG * ( WTB * XQHB + WTD * XQHD)	\
						* ( WTB * ( XLB[I] - XVB[I] ) / TLINEB + WTD * ( XLD[I] - XVD[I] ) / TLINED)
						
					XVD2I = X[I] - TLAVG * ( WTB * (1.0 - XQHB ) + WTD * (1.0 - XQHD) )	\
						* ( WTB * ( XLB[I] - XVB[I]) / TLINEB + WTD * ( XLD[I] - XVD[I] ) / TLINED )
						
					XLB[I] = XLB2I
					XVD[I] = XVD2I
				
				# [P2, P3, P4, P5, P6, P8] = bublp ( P1, P2, P3,    P7)
				[XLB,XVB, TBUB, VLBUB, VVBUB, LCRIT] = self.bublp (P, XLB, XVB, True)  # CALL BUBLP (P,XLB,XVB, TBUB,VLBUB,VVBUB,.TRUE.,LCRIT)
				[XLD,XVD, TDEW, VLDEW, VVDEW, LCRIT] = self.bublp (P, XLD, XVD, False) # CALL BUBLP (P,XLD,XVD, TDEW,VLDEW,VVDEW, False,LCRIT)
	
				
				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[HLB, CV, CP, VSND] = self.hcvcps( 1, TBUB, VLBUB, XLB) # CALL HCVCPS (1,TBUB,VLBUB,XLB,  HLB,CV,CP,VSND)
				[HVB, CV, CP, VSND] = self.hcvcps( 1, TBUB, VVBUB, XVB) # CALL HCVCPS (1,TBUB,VVBUB,XVB  ,HVB,CV,CP,VSND)
				[HLD, CV, CP, VSND] = self.hcvcps( 1, TDEW, VLDEW, XLD) # CALL HCVCPS (1,TDEW,VLDEW,XLD,  HLD,CV,CP,VSND)
				[HVD, CV, CP, VSND] = self.hcvcps( 1, TDEW, VVDEW, XVD) # CALL HCVCPS (1,TDEW,VVDEW,XVD,  HVD,CV,CP,VSND)
				
				# loop ===260   CONTINUE
				
			if not b_python_flag:
				print (BData.LUP, 'ROUTINE HPIN DID NOT CONVERGE; H,P,X:', H,P,  X ) # print array x
				#1000 FORMAT (1X,'ROUTINE HPIN DID NOT CONVERGE; H,P,X:',2F12.4,5F8.5)
			
			# come at this point 280   CONTINUE
			#
			#   SOLUTION HAS CONVERGED; WRITE OUTPUT VARIABLES.
			#
			
			#for I in range(1,BData.NC + 1): #DO 284 I=1,NC
			#	XL[I] = XLB[I]
			#	XV[I] = XVB[I]
			XL = XLB [:]
			XV = XVB [:]
			
			VL = VLBUB
			VV = VVBUB
			HL = HLB
			HV = HVB
			T  = TBUB
		# END IF
		
		XQ=(H-HL)/(HV-HL)
		
		return [T, XQ,XL,XV, VL,VV, HL,HV]
		
	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
	def U_Func (self, T,X,K,V,A,B,AP,BP,F ):
		#
		#   FUNCTION SUBROUTINE FOR THE EVALUATION OF CHEMICAL POTENTIAL
		#   (THE QUANTITY EVALUATED IS U/RT; ALSO SINCE ONLY DIFFERENCES
		#   IN U ARE REQUIRED IN THE PROGRAM, ANY TERMS WHICH WOULD
		#   CANCEL ARE OMITTED)
		#
		#	  FUNCTION U (T,X,K,V,A,B,AP,BP,F)
		#	  IMPLICIT REAL (A-H,O-Z)
		#	  COMMON /RDATA4/ R
		#	  COMMON /NCOMP/ NC
		#	  DIMENSION AP(5),BP(5),F(5,5),X(5)
		DA = -2.0 * A
		DB = BP[K] - B
		SQAK= math.sqrt( AP[K] )
		
		for I in range (1, BData.NC + 1 ):# DO 120 I=1,NC
			DA = DA + 2.0 * X[I] * math.sqrt( AP[I] ) * SQAK
			if (I != K): DA= DA - 2.0 * X[I] * F[K][I] * SQAK * math.sqrt( AP[I] )
			#120 CONTINUE
		
		B4V = 4.0 * V - B
		
		U = - math.log(V) 	\
			- ( math.log ( (V+B) / V ) * ( A * (1.0-DB/B) + DA ) 	\
			+ A * (DB+B)/(V+B))/(B* BData.R * T)	\
			+ (8.0 * V * BP[K] *( 8.0 * V-B ) / B4V + B * (16.0 * V - 3.0 * B))/(B4V*B4V)
		return U

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 

#-----------------------------------------------------------
# Job 			: 
#
# Editor		: aymhenry@gmail.com
#-----------------------------------------------------------

