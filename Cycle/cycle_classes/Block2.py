# Python Import ==================
import math,sys

# User Import
from Data import Data

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

class Block2 (Data):
	# =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
	# Set Static Vars
	# =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
	def __init__(self):
		self.setup()

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
		
		XV = [0.0] *(5+1)
		Data.NC  =  NCIN
		
		for KR in range(1,Data.NC  +  1):			# DO 100 KR = 1,NC

			Data.WM  [KR]  =  Data.CRIT [ IR[KR] -1] [1-1]	#WM(KR) = CRIT(1,IR(KR))
			Data.TREF[KR]  =  Data.CRIT [ IR[KR] -1] [2-1]
		
			Data.TC  [KR]  =  Data.CRIT [ IR[KR] -1] [3-1]
			Data.HR  [KR]  =  0.0	# not required in Python
			Data.SR  [KR]  =  0.0	# not required in Python
			Data.VR  [KR]  =  1.0

			for J in range(KR+1 ,Data.NC  +  1): 	#DO 98 J = KR + 1,NC
				Data.F[KR][J]  =  FIN [KR][J]	#	F(KR,J) = FIN(KR,J)
				Data.F[J] [KR] =  FIN [KR][J]	#98 F(J,KR) = FIN(KR,J)

			Data.F[KR][KR]  =  0.0		#F(KR,KR) = 0.0

			for KC in range(0, 2+1):		#DO 100 KC = 0,2
				Data.A[KC][KR-1]  =  Data.COEFF [ IR[KR] -1][KC + 1 -1]		#A(KC,KR) = COEFF(KC + 1,IR(KR))
				Data.B[KC][KR-1]  =  Data.COEFF [ IR[KR] -1][KC + 4 -1]		#B(KC,KR) = COEFF(KC + 4,IR(KR))
				Data.C[KC][KR-1]  =  Data.COEFF [ IR[KR] -1][KC + 7 -1]		#  C(KC,KR) = COEFF(KC + 7,IR(KR))

		#   CALL BUBBLE POINT ROUTINE TO CALCULATE SATURATED LIQUID AND VAPOR
		#   VOLUMES AND  : CALL ENTHALPY AND ENTROPY ROUTINE TO DETERMINE
		#   REFERENCE VALUES.  THE HZERO AND SZERO ALLOW AN ARBITRARY VALUE
		#   TO BE ASSIGNED TO THE SATURATED LIQUID H OR S AT THE REFERENCE
		#   TEMPERATURE.

		for KR in range(1,Data.NC  +  1) :		#	DO 164 KR = 1,NC
			X     = [0.0]  * (Data.NC+1)			# DO 160 I = 1,NC	#160 X[I] = 0.0
			X[KR] = 1.0
		
			# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
			# CALL BUBLT (TREF(KR),X,XV,  P,VR(KR),VV,.  TRUE.,.FALSE.)
			[X, XV, P, Data.VR[KR], VV, _] = self.bublt (Data.TREF[KR], X, XV, True) 
			
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HRKR, CV, CPX, VS] = self.hcvcps (1, Data.TREF[KR], Data.VR[KR], X) 	# CALL HCVCPS (1,TREF(KR),VR(KR),X,   HRKR,CV,CPX,VS)
			
			Data.HR[KR]  =  HRKR  -  Data.HZERO [  IR[KR] ]	#HR(KR) = HRKR - HZERO(IR(KR))
			Data.SR[KR]  =  self.entrop ( Data.TREF[KR], Data.VR[KR], X )  -  Data.SZERO [ IR[KR]]
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
		RT  = Data.R * T
		#
		#    STARTING AT A VOLUME OF 12.0*B4 (WHICH HAS A POSITIVE SLOPE
		#    FOR ALL 'REASONABLE' VALUES OF A, B, T) REDUCE THE VOLUME
		#    UNTIL A NEGATIVE SLOPE OF P W.R.T. V IS FOUND AND THEN BEGIN
		#    BISECTION METHOD TO FIND LOWER BOUND ON VOLUME AND PRESSURE.
		#
		VC = 12.0272727 * B4
		V  = VC
		
		for IT in range (1, Data.ITMAX +1):	# DO 100 IT=1,ITMAX
			DPDV = DP(RT, V, A, B, B4, B42)
			if (DPDV < 0.0):break	# GOTO 116

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

		for IT in range (1, Data.ITMAX + 1):	#DO 160 IT=1,ITMAX
			DPDV = DP(RT,V,A,B,B4,B42)
			if (DPDV < 0.0): break	# GOTO 164
			VPOS = V
			V = 2.0 * V

		VNEG = V		# location 164

		for IT in range (1,20): # DO 180 IT=1,20
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
		#      Data.DADT  -  TEMPERATURE DERIVATIVE OF A
		#      DBDT  -  TEMPERATURE DERIVATIVE OF B
		#      D2ADT2  -  SECOND DERIVATIVE OF A WITH RESPECT TO TEMPERATURE
		#      D2BDT2  -  SECOND DERIVATIVE OF B WITH RESPECT TO TEMPERATURE
		#      Data.HP(I)  -  INTEGRAL OF CP(I) WITH RESPECT TO TEMP FOR PURE I
		#      SP(I)  -  INTEGRAL OF (CP(I)  -  R) / T WITH RESPECT TO TEMP FOR PURE I
		#      CP(I)  -  PERFECT GAS HEAT CAPACITY FOR COMPONENT I (KJ / (KG MOL K))
		#
		#

		#DIMENSION X(5),AJI(5,5),DAJI(5,5),DA(5,5)
		#COMMON  / NCOMP /  Data.NC
		#COMMON  / ESPAR1 /  Data.AP(5),BP(5),Data.F(5,5),DADT,DBDT,Data.D2ADT,Data.D2BDT
		#COMMON  / RDATA1 /  Data.A(0:2,5),Data.B(0:2,5)
		#COMMON  / CPDATA /  Data.C(0:2,5)
		#COMMON  / HSPURE /  Data.HP(5),Data.SP(5),Data.CP(5)
		#COMMON  / REF /  TREF(5),HR(5),SR(5),VR(5)
		#COMMON  / RDATA4 /  R

		# Local Vars
		AJI  =  [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		DA   =  [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		DAJI =  [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		AMIX =  0.0
		BMIX =  0.0

		for I in range(1, Data.NC + 1):	#DO 120 I = 1,NC
			Data.AP[I]  =  Data.A[0][I-1] *  math.exp((Data.A[1][I-1] + Data.A[2][I-1] * T) * T)
			Data.BP[I]  =  Data.B[0][I-1] + (Data.B[1][I-1] + Data.B[2][I-1] * T) * T
			
			AJI[I][I]  =  X[I] * X[I] * Data.AP[I]
			AMIX  =  AMIX + AJI[I][I]
			BMIX  =  BMIX + X[I] * Data.BP[I]

			for J in range(1, (I - 1)  +  1):	#DO 120 J  =  1,I - 1
				AJI[J][I]  =  X[J] * X[I] * (1.0 - Data.F[J][I]) * math.sqrt(Data.AP[J] * Data.AP[I])
				AMIX  =  AMIX + 2.0 * AJI[J][I]

		if IQ >=  1 :
			Data.DADT  =  0.0
			Data.DBDT  =  0.0

			for I in range(1,Data.NC  +  1): 	#DO 140 I  =  1,NC
				DA[I][I]  =  Data.A[1][I-1] + 2.0 * Data.A[2][I-1] * T
				DAJI[I][I]  =  AJI[I][I] * DA[I][I]
				Data.DADT  =  Data.DADT + DAJI[I][I]
				Data.DBDT  =  Data.DBDT + X[I] * (Data.B[1][I-1] + 2.0 * Data.B[2][I-1] * T)
				
				for J in range(1, (I - 1)  +  1):	#DO 140 J  =  1,I - 1
					DA[J][I]  =  0.5 * (Data.A[1][J-1] + Data.A[1][I-1] ) + (Data.A[2][J-1] + Data.A[2][I-1] ) * T
					DAJI[J][I]  =  AJI[J][I] * DA[J][I]
					Data.DADT  =  Data.DADT + 2.0 * DAJI[J][I]

			if IQ >=  2 :
				Data.D2ADT  =  0.0
				Data.D2BDT  =  0.0

				for I in range(1,Data.NC  +  1):	#DO 160 I  =  1,NC
					Data.CP[I]  =  Data.C[0][I-1] + (Data.C[1][I-1] + Data.C[2][I-1] * T) * T

					Data.D2BDT  =  Data.D2BDT + 2.0 * X[I] * Data.B[2][I-1]
					Data.D2ADT  =  Data.D2ADT + DAJI[I][I] * DA[I][I] + 2.0 * AJI[I][I] * Data.A[2][I-1]

					for J in range(1, (I - 1)  +  1): #DO 160 J  =  1,I - 1
						Data.D2ADT  =  Data.D2ADT + 2.0 * (DAJI[J][I] * DA[J][I] + AJI[J][I] * (Data.A[2][J-1] + Data.A[2][I-1]))

			if IQ <=  3 :
				if T <= 0:
					print ("\nespar : bad input parameter, T <= 0, T=",T)
					print ('Application terminated in python\n')
					sys.exit	#('1030') to show sorce of error
				
				for I in range(1,Data.NC  +  1):	#DO 180 I  =  1,NC
					
					Data.HP[I]  =  (Data.C[0][I-1] + (0.5 * Data.C[1][I-1] + Data.C[2][I-1] / 3.0 * T) * T) * T
					Data.SP[I]  =  (Data.C[0][I-1] - Data.R) * math.log(T / Data.TREF[I]) \
						 + Data.C[1][I-1] * (T - Data.TREF[I])		\
						 + 0.5 * Data.C[2][I-1] * (T * T -  (Data.TREF[I]**2))

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
			
			TCC = 0.2273291 * AC / (Data.R*BC)
			FTC[J] = TCC - TC[J]
	
			if ( abs( FTC[J] ) <= 0.01):
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
		return Data.R * T * ( -  math.log(V)  \
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
		#      Data.F  -  MIXING PARAMETER
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
		#COMMON  / NCOMP /  Data.NC
		#COMMON  / ESPAR1 /  Data.AP(5),Data.BP(5),Data.F(5,5),Data.DADT,Data.DBDT,Data.D2ADT,D2BDT
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
			#for I in range(1,Data.NC  + 1):
			#	X1[I] = XL[I]
			#	XV[I] = XL[I]
		else:
			X1 = XV[:]
			XL = XV[:]		
			#for I in range(1,Data.NC  + 1):
			#	X1[I] = XV[I]
			#	XL[I] = XV[I]

		[A1, B1] = self.espar (0,T,X1)	#CALL espar ((0,T,X1,A1,B1)
		
		#
		#   DETERMINE if INPUT TEMPERATURE EXCEEDS CRITICAL POINT;
		#   if SO, SET ERROR FLAG AND RETURN
		#
		loc_TC  =  A1 / (B1 * 4.398909 * Data.R)
	
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

		if PLOW < 0.0:
			VLOW = 0.8 * B1
			PC = 0.1049995 * Data.R * loc_TC / B1
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
		#LPPCON  =  False
		LPPCON  =  True # A

		#
		#   STARTING WITH INITIAL VALUES OF PRESSURE CLOSE TO THE UPPER
		#   AND LOWER BOUNDS (FOUND BY SUBROUTINE PLIMIT) ITERATE ON
		#   LOG (P) UNTIL THE GIBBS FREE ENERGY OF BOTH PHASES ARE EQUAL.
		#   A COMBINATION OF SECANT AND REGULI - FALSI METHODS IS USED
		#   FOR THE ITERATION.
		
		PNEG = 0.0 # In Python 
		for IT in range (1, Data.ITMAX + 1):	#DO 400 IT = 1,ITMAX
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
				PPOS = 0.0 # not set
			else:
				LPPOS = True
				FPPOS = FP[J]
				PPOS  = PL[J]

			if IT <= 1 :
				J = 2
			else:
				DGDPL = (FP[2] - FP[1]) / (PL[2] - PL[1])
				if (DGDPL == 0.0) or \
					(abs(FP[J] / (PL[J] * DGDPL)) < Data.TOLR):	#GOTO 440
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
		if Data.NC == 1:	#  440 if (Data.NC == 1)  :
			if LV1CON : print (Data.LUP,'bublt :volume iteration for parent phase did not converge')
			if LV2CON : print (Data.LUP,'bublt :volume iteration for incipient phase did not converge')
			if LPPCON : print (Data.LUP,'bublt :pure material pressure iteration in bublt did not converge')
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
		#for I in range(1, Data.NC +1):	# DO 500 I = 1,Data.NC
		#	X2C[I] = X1[I]

		LPNEG = False
		LPPOS = False
		
		b_pyth_exit_inner = False
		for ITP  in range (1, Data.ITMAX + 1): # DO 800 ITP = 1,ITMAX
			XX2 = X2C[:]
			#for I  in range (1, Data.NC + 1): # DO 520 I = 1,Data.NC
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
			for I in range (1, Data.NC + 1): #DO 540 I = 1,Data.NC
				U1[I] = self.U_Func (T,X1,I,V1,A1,B1,Data.AP,Data.BP,Data.F)

			#
			#   ENTER INNER ITERATION LOOP (FOR COMPOSITION OF PHASE 2)
			#
			LXNEG = False
			LXPOS = False
			
			C = 0.0 # prevent var is not found
			LXCON = True # add by python
			b_pyth_exit_outer = False
			for IT in range (1, Data.ITMAX + 1):	# DO 600 IT = 1,ITMAX
				LV2CON = False

				#   COMPUTE EQUATION OF STATE COEFFICIENTS FOR PHASE 2
				
				[A2, B2] = self.espar (0,T,X1) 	#CALL espar (0,T,XX2, A2, B2)
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
				for I in range (1, Data.NC + 1): #DO 560 I = 1,Data.NC
					U2[I] = self.U_Func(T,XX2,I,V2, A2, B2,Data.AP,Data.BP,Data.F)

				#
				#   CALCULATE THE COMPOSITION OF PHASE 2 FROM THE COMPOSITION
				#   OF PHASE 1 AND THE CHEMICAL POTENTIALS.  THE INNER ITERATION
				#   LOOP HAS CONVERGED WHEN THE CALCULATED COMPOSITION EQUALS
				#   (WITHIN A CONVERGENCE TOLERANCE) THE GUESSED VALUE OF X2.
				#
				FXSUM = 0.0
				C = 0.0
				for I in range (1, Data.NC + 1): 	#DO 580 I = 1,Data.NC
					Z[I] = X1[I] * math.exp(U1[I] - U2[I])
					C = C + Z[I]

				for I in range (1, Data.NC + 1): 	# DO 584 I = 1,Data.NC
					X2C[I] = Z[I] / C
					FX2[I] = X2C[I] - XX2[I]
					XX2[I] = X2C[I]
					FXSUM = FXSUM + abs(FX2[I])
				
				if (FXSUM < Data.NC * Data.TOLR) :
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

			if  (abs(FP[1]) <  100.0 * Data.TOLR):
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
			if (ITP < 2  or  J == 1  or  FP[1] == FP[2]) :
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
			print (Data.LUP, 'bublt : mixture pressure iteration in bublt did not converge')

		P = PP[J] # location 840
		#
		#   ASSIGN RESULTS FOR PHASES 1 AND 2 TO LIQUID AND VAPOR PHASES
		#   DEPENDING ON WHETHER THE DEW OR BUBBLE POINT WAS CALCULATED.
		#
		if (LBUB)  :
			for I in range (1, Data.NC + 1): # DO 860 I = 1,Data.NC
				XV[I] = XX2[I]
			VL = V1
			VV = V2
		else :
			for I in range (1, Data.NC + 1): # DO 880 I = 1,Data.NC
				XL[I] = XX2[I]
			VL = V2
			VV = V1

		#
		#   PRINT WARNING MESSAGES FOR ANY CASES OF NON - CONVERGENCE OCCURING
		#   ON FINAL CALL TO EACH ITERATION AND RETURN.
		#
		if LV1CON : print (Data.LUP,'bublt : volume iteration for parent phase did not converge')
		if LV2CON : print (Data.LUP,'bublt : volume iteration for incipient phase did not converge')
		if LXCON  : print (Data.LUP,'bublt : composition iteration in bublt did not converge')

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
		#       Data.F  -  MIXING PARAMETER
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
		#       A,B  -  COMPONENTS OF MIXTURE; COMPOSITION IS MOLE FRACTION Data.A
		#       L  -  LIQUID PHASE
		#       V  -  VAPOR PHASE
		#       1  -  PARENT PHASE (PHASE WITH SPECifIED COMPOSITION)
		#       2  -  INCIPIENT PHASE
		#       (FOR EXAMPLE UA1 REFERS TO CHEMICAL POTENTIAL OF COMPONENT Data.A
		#       IN PHASE 1)
		#
		#	  COMMON  / ESPAR1 /  Data.AP(5),Data.BP(5),Data.F(5,5),Data.DADT,Data.DBDT,Data.D2ADT,Data.D2BDT
		#	  COMMON  / NCOMP /  Data.NC
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
		# SAVE Data.TNEG,Data.TPOS set as Data vars
		#Data.TNEG = 9999.0
		#Data.TPOS = -999.0

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
		#	- A / (V + B)  ) / (Data.R * T)
		
		
		#
		#    COMPUTE PURE COMPONENT E.S. COEFFICIENTS, THE MIXING PARAMETER,
		#    AND THE E.O.S. COEFFICIENTS FOR PHASE 1
		#
		if (LBUB) :
			X1 = XL [:]
			XV = XL [:]
			#for I in range(1, Data.NC + 1 ):   # DO 100 I = 1,Data.NC
			#	X1[I] = XL[I]
			#	XV[I] = XL[I]
		else:
			X1 = XV [:]
			XL = XV [:]
			#for I in range(1, Data.NC + 1 ):   # DO 120 I = 1,Data.NC
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
		VV = Data.R * TT[1] / P
		
		#
		#    ENTER ITERATION FOR PURE COMPONENT.  THIS ITERATION VARIES
		#    TEMPERATURE UNTIL THE GIBBS FREE ENERGY OF BOTH PHASES ARE EQUAL.
		#    A COMBINATION OF SECANT AND REGULI - FALSI METHODS IS USED
		#    FOR THE ITERATION.
		#
		if (Data.NC == 1) :
			if (P > PC) :
				LCRIT = True
				print ("bublp: critical point of pure or pseudo-pure material")
				return [0.0, 0.0, 0.0, True]

			J = 1
			LTPOS =  False
			LTNEG =  False
			LPPCON=  False
			
			b_python_flag_loop1 = False
			
			for  IT in range( 1, Data.ITMAX + 1 ):   # DO 400 IT = 1,ITMAX
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

				if (abs(FT[J]) < 100.0 * Data.TOLR):
					b_python_flag_loop1 = True
					break	#	GOTO 440

				if (FT[J] < 0.0) :
					LTNEG = True
					FTNEG = FT[J]
					Data.TNEG = TT[J]
				else:
					LTPOS = True
					FTPOS = FT[J]
					Data.TPOS = TT[J]

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
						if (TT[3] < min(Data.TNEG, Data.TPOS)  or  TT[3] > max(Data.TNEG,Data.TPOS)) :
							TT[3] = Data.TPOS - FTPOS * (Data.TPOS - Data.TNEG) / (FTPOS - FTNEG)
					
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
		#for I  in range(1, Data.NC +1 ):   # DO 500 I = 1,Data.NC
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
		for ITT in range(1, Data.ITMAX + 1):   # DO 800 ITT = 1,ITMAX
			XX2 = X2C[:]
			#for I  in range(1, Data.NC +1 ):   # DO 520 I = 1,Data.NC
			#	XX2[I] = X2C[I]
				
			LXCON =  False
			LV1CON =  False
			# [P4, P5] = self.espar [P1, P2, P3]
			[A1, B1] =  self.espar(0, TT[J], X1 ) 					# CALL espar (0,TT(J),X1, A1, B1)
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[V1, LV1CON] = self.vit (TT[J] , P, A1, B1, V1, LBUB)		# CALL VIT (TT(J), P, A1, B1,V1,LBUB,LV1CON)
			#
			#    IF VOLUME ITERATION HAS NOT CONVERGED, TRY Data.A NEW TEMPERATURE AND
			#    RETURN TO THE BEGINNING OF THE ITERATION
			#
			if (LV1CON  or  LXCON)  :
				TT[J] = 0.95 * TT[J]
				continue # GOTO 800 exit loop
			
			#    COMPUTE CHEMICAL POTENTIALS FOR PHASE 1
			for I in range(1, Data.NC + 1):   # DO 540 I = 1,Data.NC
				U1[I] = self.U_Func (TT[J],X1,I,V1,  A1,B1,  Data.AP,Data.BP,Data.F)
			#
			#    ENTER INNER ITERATION LOOP (FOR COMPOSITION OF PHASE 2)
			#
			#JJ = 1 useless allway one, used once
			LXNEG =  False
			LXPOS =  False
			
			b_python_flag_loop3 = False
			b_python_flag_outer = False
			for IT in range(1, Data.ITMAX + 1):   # DO 600 IT = 1,ITMAX
				LV2CON =  False
				#    COMPUTE EQUATION OF STATE COEFFICIENTS FOR PHASE 2
				# [P4, P5] = self.espar [P1, P2, P3]
				[A2, B2] =  self.espar(0, TT[J], XX2 ) # JJ allway 1	CALL espar (0,TT(J),XX2(JJ), A2, B2)
				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[V2, LV2CON] = self.vit (TT[J], P, A2, B2, V2, not LBUB)		# CALL VIT (TT(J),P, A2, B2, V2,.NOT.LBUB, LV2CON)
				
				#
				#    if VOLUME ITERATION HAS NOT CONVERGED, TRY Data.A NEW TEMPERATURE
				#    AND RETURN TO THE START OF THE TEMPERATURE ITERATION.
				#
				if (LV2CON)  :
					b_python_flag_outer = True
					TT[J] = 0.95 * TT[J]
					continue	# GOTO 800

				#   COMPUTE CHEMICAL POTENTIALS OF PHASE 2

				for I in range(1, Data.NC + 1):  # DO 560 I = 1,Data.NC
					U2[I] = self.U_Func (TT[J], XX2,I,V2,  A2,B2,  Data.AP,Data.BP,Data.F)
				#
				#    CALCULATE THE COMPOSITION OF PHASE 2 FROM THE COMPOSITION
				#    OF PHASE 1 AND THE CHEMICAL POTENTIALS.  THE INNER ITERATION
				#    LOOP HAS CONVERGED WHEN THE CALCULATED COMPOSITION EQUALS
				#    (WITHIN Data.A CONVERGENCE TOLERANCE) THE GUESSED VALUE OF X2.
				#
				FXSUM = 0.0
				C = 0.0

				for I in range(1, Data.NC + 1):  # DO 580 I = 1,Data.NC
					Z[I] = X1[I] * math.exp(U1[I] - U2[I])
					C = C + Z[I]

				for I in range(1, Data.NC + 1):   # DO 584 I = 1,Data.NC
					X2C[I] = Z[I] / C
					FX2[I] = X2C[I] - XX2[I]
					XX2[I] = X2C[I]
					FXSUM = FXSUM + abs(FX2[I])

				if(IT <= 1): FXOLD = 1.0E6

				if (FXSUM < Data.NC * Data.TOLR):
					b_python_flag_loop3 = True
					break 	# GO TO 640

				FXDIF = abs(FXSUM - FXOLD)
				
				if(FXDIF <= 10.0 * Data.TOLR and IT >= Data.ITMAX):
					b_python_flag_loop3 = True
					break 	# GO TO 640

				FXOLD = FXSUM
			# end of loop 	600 CONTINUE
			
			if b_python_flag_outer :
				continue # goto 800
				
			#    IF INNER ITERATION LOOP HAS NOT CONVERGED, SET ERROR FLAG
			if not b_python_flag_loop3:
				print (IT, FXSUM, Data.NC * Data.TOLR,FXSUM / (Data.NC * Data.TOLR), FXDIF)
				LXCON = True
			#
			#    END OF ITERATION LOOP FOR PHASE 2 COMPOSITION

			# point 640 Con
			FT[J] = 1.0 - C
			#
			#    OUTER (TEMPERATURE) ITERATION HAS CONVERGED WHEN Data.C  =  1.000
			#    (I.E. WHEN THE CHEMICAL POTENTIALS OF EACH COMPONENT ARE
			#    THE SAME IN BOTH PHASES).
			#

			if (abs(FT[J]) < 100.0 * Data.TOLR):
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
					Data.TNEG = TT[J]
				else:
					LTPOS = True
					FTPOS = FT[J]
					Data.TPOS = TT[J]
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
				
				if ((TT[3] > max(Data.TNEG,Data.TPOS)  or 	\
					TT[3] < min(Data.TNEG,Data.TPOS))  and  LTNEG  and  LTPOS):
					
					TT[3] = Data.TPOS - FTPOS * (Data.TPOS - Data.TNEG) / (FTPOS - FTNEG)
			 
				TT[1] = TT[2]
				TT[2] = TT[3]
				FT[1] = FT[2]
			
		#
		# End of loop  800 CONTINUE

		if not	b_python_flag_loop8 :
			print (Data.LUP, 'MIXTURE TEMPERATURE ITERATION IN BUBLP DID NOT, CONVERGE')
		
		# point 840
		T = TT[J]
		# 
		#
		#    ASSIGN RESULTS FOR PHASES 1 AND 2 TO LIQUID AND VAPOR PHASES
		#    DEPENDING ON WHETHER THE DEW OR BUBBLE POINT WAS CALCULATED.
		#
		if (LBUB)  :
			#for I in range(1, Data.NC + 1):   # DO 860 I = 1,Data.NC
			#	XV[I] = XX2[I]
			XV = XX2[:]
			VL = V1
			VV = V2
			
		else:
			#for I in range(1, Data.NC + 1):   # DO 880 I = 1,Data.NC
			#	XL[I] = XX2[I]
			XL = XX2[:]
			VL = V2
			VV = V1
		#
		#    PRINT WARNING MESSAGES FOR ANY CASES OF NON - CONVERGENCE OCCURING
		#    ON FINAL CALL TO EACH ITERATION AND RETURN.
		#
		if (abs(1.0 - VL / VV) < Data.TOLR)  :
			LCRIT = True
			print(Data.LUP, 'CRITICAL POINT EXCEEDED IN BUBLP')
		
		if (LV1CON): print (Data.LUP, 'ITERATION IN BUBLP FOR PARENT PHASE VOLUME DID')
		if (LV2CON): print (Data.LUP, 'ITERATION IN BUBLP FOR INCIPIENT PHASE VOLUME DID, NOT CONVERGE')
		if (LXCON) : print (Data.LUP, 'COMPOSITION ITERATION IN BUBLP DID NOT CONVERGE')

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
		S = (Data.DADT * B - A * Data.DBDT)/ (B**2 ) * math.log((V+B)/V)  +  A * Data.DBDT/B/(V+B) \
			-Data.R*B4/(V-B4)**2  *(4.0*V-3*B4)		\
			-Data.R*T*Data.DBDT*0.5*V/(V-B4)**3   *(2.0*V-B4)

		for I in range (1, Data.NC +1): 	#DO 120 I=1,NC
			S = S + X[I] * (Data.SP[I] - Data.SR[I] + Data.R * math.log(V/ Data.VR[I]))

			if (X[I] > 0.0 and  X[I]  < 1.0) :
				S = S - Data.R * X [I] * math.log(X[I])

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
		#	  COMMON  / NCOMP /  Data.NC
		#	  COMMON  / ESPAR1 /  Data.AP(5),Data.BP(5),Data.F(5,5),    C1,D1,C2,D2
		#    simlar to /ESPAR1/        AP(5),BP(5),F(5,5),          DADT,DBDT,D2ADT,D2BDT
		#	  COMMON  / HSPURE /  HP(5),SP(5),Data.CP(5)
		#	  COMMON  / REF /  Data.TREF(5),Data.HR(5),SR(5),VR(5)
		#	  COMMON  / RDATA2 /  Data.WM(5),TC(5)
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
		RT = Data.R * T
		
		H  = 0.0 # python in case if IQ =4 H will have no value
		VS = 0.0 # python in case if IQ =4 H will have no value
		CV = 0.0 # python in case if IQ =4 H will have no value
		CP = 0.0 
		
		if (IQ <= 3) :
			#    COMPUTE ENTHALPY AS A FUNCTION OF T, V, X
			H = ( (A + (A * Data.DBDT / B - Data.DADT) * T) * VBL + A * (Data.DBDT * T - B) / VB) / B		\
				+ 2.0 * RT * V * (2.0 * V - B4) * (B4 - 0.25 * Data.DBDT * T) / VB43
				
			for I in range(1, Data.NC + 1):   # DO 120 I = 1,Data.NC
				H = H + X[I] * (Data.HP[I] - Data.HR[I])
		
		if (IQ >= 2) :
			#    COMPUTE CONSTANT VOLUME HEAT CAPACITY
			D12 = Data.DBDT * Data.DBDT
			CV = (Data.R * V * ((0.375 * D12 * T / VB4 	\
				+ 0.5 * Data.D2BDT * T + Data.DBDT) * (B4 - 2.0 * V)	\
				+ 0.125 * D12 * T) / VB43						\
				+ ((1.0 / VB + VBL / B) * (A * Data.D2BDT * B 	\
				+ 2.0 * (Data.DADT * Data.DBDT * B - A * D12)) / B	\
				- Data.D2ADT * VBL - A * D12 / (VB **2)) / B) * T - Data.R

			for I in range( Data.NC + 1):   # DO 160 I = 1,Data.NC
				CV = CV + X[I] * Data.CP[I]

			if (IQ == 3  or  IQ == 5):
				#    COMPUTE SPECIFIC HEAT AT CONSTANT PRESSURE USING CV
				Y = B4 / V
				DPDT = 2.0 * Data.R / VB4 * ( - 1.0 + ( - 0.25 * T * Data.DBDT + (V * V * (1.0 + 0.75 * T * Data.DBDT / VB4))		\
					/ VB4) / VB4) + (Data.R + ( - Data.DADT + A * Data.DBDT / VB) / VB) / V

				#DPDV = ( - RT * (1.0 + (4.0 + (4.0 + ( - 4.0 + Y) * Y) * Y) * Y) / (pow(1.0 - Y,4))	\
				#	+ A * (2.0 * V + B) / (VB *VB)) / (V * V )
					
				DPDV = ( - RT * (1.0 + (4.0 + (4.0 + ( - 4.0 + Y) * Y) * Y) * Y) / (   (1.0 - Y)**4   )	\
					+ A * (2.0 * V + B) / (VB *VB)) / (V * V )

				CP = CV - DPDT * DPDT * T / DPDV
				
				# COMPUTE VELOCITY OF SOUND USING C'S AND VOLUME DERIVATIVE OF P
				WMOL = 0.0
				for I in range(1, Data.NC+1 ):   # DO 180 I = 1,Data.NC
					WMOL = WMOL + X[I] * Data.WM[I]
				
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
		RT = Data.R * T
		B4 = 0.25 * para_B
		
		B4L = math.log(B4)
			
		if (VL < B4L):
			VL = B4L + 0.5
		
		
		TC = para_A / (para_B * 4.398909 * Data.R)

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
		for IT in range(1, Data.ITMAX + 1 ):   # DO 100 IT = 1,ITmath.max
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
					#if (abs(FVDP / P) < 0.001 * Data.TOLR) :
					if (abs(FVDP / P) <  Data.TOLR) :
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
					if (abs(FVDPL) < 0.001 * Data.TOLR)  :
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

		for IT in range(1, Data.ITMAX + 1 ):   # DO 200 IT = 1,ITmath.max
			# [P4, P5] = self.espar [P1, P2, P3]
			[A, B] = self.espar (2, T2, X )					#CALL espar (2,T2,X, A, B)
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[V, LVCON] = self.vit (T2, P, A, B,  V, LLIQ)	# CALL VIT (T2,P, A, B,V,     LLIQ,LVCON)

			SSP = self.entrop (T2,V,X)
			FT2 = S - SSP
			if (abs(FT2) < Data.TOLS  or  abs(FT2 - FT1) < 0.02 * Data.TOLS):
				return [T2, V]

			T3 = T2 - FT2 * (T2 - T1) / (FT2 - FT1)
			T1 = T2
			T2 = T3
			FT1 = FT2

		print (Data.LUP, 'SINGLE PHASE ITERATION IN SPIN DID NOT CONVERGE' )
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

		for IT in range (1, Data.ITMAX + 1) : # DO 200 IT=1,ITMAX
			# [P4, P5] = self.espar [P1, P2, P3]
			[A, B] =  self.espar(2, T2, X ) #CALL espar (2,T2,X,A,B)
			
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[V, LVCON ] = self.vit (T2,P,A,B,V,   LLIQ)	#CALL VIT (T2,P,A,B,V,   LLIQ,LVCON)
			
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HSP, CV, CP, VSND] = self.hcvcps ( 1, T2, V, X ) 	# CALL HCVCPS (1,T2,V,X,         HSP,CV,CP,VSND)

			FT2 = H - HSP
			
			if (abs(FT2) < Data.TOLH  or  abs(FT2-FT1) < 0.02 * Data.TOLH):
				return [T2,V]

			T3 = T2 - FT2 * (T2-T1) / (FT2-FT1)
			T1 = T2
			T2 = T3
			FT1 = FT2

		print (Data.LUP,'SINGLE PHASE ITERATION IN HPIN DID NOT CONVERGE')
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
		#	  IMPLICIT REAL (Data.A - H,O - Z)
		#	  LOGICAL LCRIT,LCONV
		#	  DIMENSION X(5),XL(5),XV(5),XLB(5),XVB(5),XLD(5),XVD(5)
		#	  COMMON  / NCOMP /  Data.NC
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
			#for I in range(1, Data.NC + 1):   # DO 120 I = 1,Data.NC
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
			#for I in range(1,Data.NC):   # DO 140 I = 1,Data.NC
			#	XV[I] = X[I]
				
			VL = VLDEW
			VVDEW = VV
			
			[T,VV]= self.spxsp(S,P,X, TDEW,SV,VVDEW, False)
			# CALL SPXSP (S,P,X,   TDEW,SV,VVDEW,   T,VV,.FALSE.)
			
		else:
			#    TWO PHASE
			#
			NCC = Data.NC
			for I in range(1, Data.NC + 1):   # DO 210 I = 1,Data.NC
				if (X[I] < Data.TOLR):
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
			for IT in range(1, int(Data.ITMAX/2) + 1):	# DO 260 IT = 1,ITMAX / 2
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
				
				for I in range(1, Data.NC + 1):   # DO 214 I = 1,Data.NC
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
				for I in range(1, Data.NC + 1):   # DO 220 I = 1,Data.NC
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
					
					for J in range(1, (Data.NC-1) + 1):  		# DO 222 J = 1,Data.NC - 1
						for K in range(  J+1 , Data.NC +1 ):		# DO 222 K = J + 1,Data.NC
							DTSUMB = DTSUMB + ( ( X[J] - XLB[J] ) * ( XVB[K] - XLB[K] ) 	\
								- ( X[K] - XLB[K] ) * ( XVB[J] - XLB[J] ) ) ** 2
								
							DTSUMD = DTSUMD + ( ( X[J] - XLD[J] ) * ( XVD[K] - XLD[K] )		\
								- ( X[K] - XLD[K] ) * ( XVD[J] - XLD[J] ) ) ** 2
							# 222 CONTINUE
			  
					DXBUB = 0.0
					DXDEW = 0.0
					
					for I in range( 1, Data.NC +1 ):   # DO 223 I = 1,Data.NC
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
				
				for I in range(1, Data.NC + 1):   # DO 224 I = 1,Data.NC
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
				
			# WRITE (LUP,1000) S,P,   (X(I),I = 1,Data.NC)
			if not b_python_flag :
				print (Data.LIP, "ROUTINE SPIN DID NOT CONVERGE S, P, X",S, P, X[1:Data.NC] )
			
			# 280	CONTINUE
			
			#
			#    SOLUTION HAS CONVERGED; WRITE OUTPUT VARIABLES.
			#
			XL = XLB[:]
			XV = XVB[:]
			
			#for I in range(1,Data.NC):   # DO 284 I = 1,Data.NC
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
			#for I in range(1, Data.NC + 1): # DO 120 I=1,NC
			#	XL[I] = X[I]

			XL = X[:]
			VV    = VVBUB
			VLBUB = VL
			
			#[P7,P8] = self.hpxsp (P1,P2,P3, P4,P5,P6, P9)
			[T, VL] =self.hpxsp (H, P, X,  TBUB, HL, VLBUB, True) #CALL HPXSP (H,P,X,TBUB,HL,VLBUB,T,VL,.TRUE.)
			
		elif (H >= HV) :
			#   SINGLE PHASE VAPOR
			#for I in range(1, Data.NC + 1): # DO 140 I=1,NC
			#	XV[I] = X[I]

			XV = X[:]
			VL   = VLDEW
			VVDEW= VV
			
			#[P7,P8] = self.hpxsp (P1,P2,P3, P4,P5,P6,P9 )
			[T, VV]  = self.hpxsp (H, P, X, TDEW, HV, VVDEW, False) #CALL HPXSP (H,P,X,TDEW,HV,VVDEW,T,VV,.FALSE.)
			
		else:

			#   TWO PHASE
			NCC = Data.NC
			for I in range(1, Data.NC + 1): # DO 210 I=1,NC
				if (X[I] < Data.TOLR): NCC = NCC-1
				
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
			for IT in range(1, int(Data.ITMAX/2 + 1)): #DO 260 IT=1,ITMAX/2
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
				
				for I in range(1, Data.NC + 1): #DO 214 I=1,NC
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
				for I in range (1, Data.NC + 1): # DO 220 I=1,NC
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
					
					for J in range (1, (Data.NC-1) + 1):	#DO 222 J=1,NC-1
						for K in range (J+1, (Data.NC + 1) ): #DO 222 K=J+1,NC
							DTSUMB = DTSUMB + ((X[J] - XLB[J] ) * ( XVB[K] - XLB[K])	\
								- (X[K] - XLB[K]) * (XVB[J] - XLB[J] ) )**2
								
							DTSUMD = DTSUMD + ((X[J] - XLD[J] ) * ( XVD[K] - XLD[K])	\
								- (X[K] - XLD[K]) * (XVD[J] - XLD[J] ) )**2
						
					DXBUB = 0.0
					DXDEW = 0.0
					
					for I in range(1, Data.NC + 1): #DO 223 I=1,NC
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
				
				for I in range(1,Data.NC + 1): # DO 224 I=1,NC
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
				print (Data.LUP, 'ROUTINE HPIN DID NOT CONVERGE; H,P,X:', H,P,  X ) # print array x
				#1000 FORMAT (1X,'ROUTINE HPIN DID NOT CONVERGE; H,P,X:',2F12.4,5F8.5)
			
			# come at this point 280   CONTINUE
			#
			#   SOLUTION HAS CONVERGED; WRITE OUTPUT VARIABLES.
			#
			
			#for I in range(1,Data.NC + 1): #DO 284 I=1,NC
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
		
		for I in range (1, Data.NC + 1 ):# DO 120 I=1,NC
			DA = DA + 2.0 * X[I] * math.sqrt( AP[I] ) * SQAK
			if (I != K): DA= DA - 2.0 * X[I] * F[K][I] * SQAK * math.sqrt( AP[I] )
			#120 CONTINUE
		
		B4V = 4.0 * V - B
		
		U = - math.log(V) 	\
			- ( math.log ( (V+B) / V ) * ( A * (1.0-DB/B) + DA ) 	\
			+ A * (DB+B)/(V+B))/(B* Data.R * T)	\
			+ (8.0 * V * BP[K] *( 8.0 * V-B ) / B4V + B * (16.0 * V - 3.0 * B))/(B4V*B4V)
		return U

	# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
