# Python import
import math, sys
from abc import ABC,abstractmethod

# User import
from .Data import Data
from .Block2 import Block2
from .CompMap import CompMap

from common_classes.FileAccess import FileAccess
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Evaprator configration
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Comp_Abstract (ABC, Block2, Data):
	FLDER_COMPMAP_DAT = sys.path[0] + "\\" + "compmap"
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def __init__ (self, objdata, str_Comp_File =""):
		self.str_Comp_File = str_Comp_File
		self.objData = objdata

	# Abstract methods
	#-----------------------------------------------------------
	# Job 			: Compressor balance
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def comp_balance (self):
		pass
		
	# Abstract methods
	#-----------------------------------------------------------
	# Job 			: Compressor balance fpr IMAP=1  EER
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def map (self, ICOMP, ICOOL, EER, SIZE, DISPL , SPEEDN):
		pass
		
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def comp (self, H, P, X, T, MEFF, QHILO, QCAN, V, TAMB):
		
		#	 SUBROUTINE COMP(H, P, X, T, CV, CP, HOUT, MEFF, QHILO,
		#	           QCAN, VSUC, V, VV2, TSUC, TDISC,    TAMB, GAMA, RN, ETAS)
		# CV, CP not used, P5.P6
		# input	 H, P, X, T, MEFF, QHILO, QCAN, V, TAMB
		# output T, HOUT, QHILO, QCAN, VSUC, VV2, TSUC, TDISC, GAMA, RN, ETAS
		#		[P4, P7 , P9 to P11,  P13, P15, P17, P18,P19 ] = self.comp (P1 to P4, P8to P10, P12, P16 )
		#     *****************************************************************
		#     *    COMPRESSOR MODEL                                           *
		#     *****************************************************************
		#
		#	 LOGICAL LCRIT
		#	 REAL MEFF, MREF
		#	 DIMENSION H(16),P(16),X(5),XQ(16),XL(5,16),XV(5,16),T(16),V(16)
		
		#	 COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
		#	 COMMON / MAPDAT / IMAP, ICOMP, ICOOL, EER, SIZE, DISPL, EFFC,
		#	                  SPEEDN, IREAD
		
		R    = 8.314
		TOLS = 0.1
		
		XQ= [0.0] * (16+1)
		XL= [0.0] * (5+1) # modification in Python
		XV= [0.0] * (5+1) # modification in Python
		
		#XL= [[0.0] * (16+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		#XV= [[0.0] * (16+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
			
		#          CONVERSION FUNCTIONS
		def F_TO_K(T):
			return (T + 459.7)/1.8
				
		#
		#          SET UP INITIAL GUESS FOR SUCTION PORT CONDITIONS
		TSUC = T[1]
		VSUC = V[1]
		
		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[HSUC,CV,CP,VS] = self.hcvcps (1,TSUC,VSUC,X) #	CALL HCVCPS (1,TSUC,VSUC,X,HSUC,CV,CP,VS)
		
		# [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
		[XL, X, TDEW, XXX, VDEW, LCRIT] = self.bublp ( P[2], XL, X, False )
		#CALL BUBLP(P[2], XL[1][2], X, TDEW, XXX, VDEW, .FALSE., LCRIT)
		
		#          CALCULATE ISENTROPIC CONDITIONS BASED ON SHELL INLET
		#
		SSUC = self.entrop(TSUC,VSUC,X)

		
		#[P4 .. P11] = self.spin (P1, P2, P3 )
		[T[2],XQ[2], XL,XV,VL2, VV2,SL,SV] = self.spin (SSUC,P[2],X ) #	CALL SPIN (SSUC,P[2],X,T[2],  XQ[2],XL[1][2],XV[1][2],VL2, VV2,SL,SV)
		
		if(XQ[2] < 1.0) :
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HL2,CV,CP,VS] = self.hcvcps (1,T[2], VL2,XL) # CALL HCVCPS (1,T[2],VL2, XL[1][2],HL2, CV,CP,VS)
			[HV2,CV,CP,VS] = self.hcvcps (3,T[2], VV2,XV) # CALL HCVCPS (3,T[2],VV2, XV[1][2],HV2, CV,CP,VS)
			
			H[2] = XQ[2]*HV2 + (1.0-XQ[2])*HL2
		else:
			[H[2],CV,CP,VS] = self.hcvcps (3,T[2],VV2,X) #CALL HCVCPS (3,T[2],VV2,X,H[2],CV,CP,VS)
		#End if
		HISEN = H[2]
		#
		#          SELECT MODEL
		#
		if(Data.obj_cdata.IMAP == 1) :                                 ###Map model
			if(Data.obj_cdata.ICOOL == 0) :
				TSUC = 389.59 - 64.815 * Data.obj_cdata.EER                     ###Degrees F
			else:
				TSUC = 337.84 - 57.895 * Data.obj_cdata.EER
			#End if

			TIN = 1.8*T[1] - 459.7
			TSUC = TSUC + TIN

			T_EVAP = 1.8*T[12] - 459.7
			T_COND = 1.8*TDEW  - 459.7

			TSUC = TSUC + 0.2*(T_COND - 130.0) - 0.2*(T_EVAP + 10.0)

			if(Data.obj_cdata.ICOMP == 2): TSUC = TIN + 30.0                ###Rotary

			TSUC = F_TO_K(TSUC)                               ###K
			VSUC = VSUC*TSUC/T[1]                             ###Suction density

			[AMIX, BMIX] = self.espar (0, TSUC, X) #CALL ESPAR(0, TSUC, X, AMIX, BMIX)
			
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[VSUC, LCRIT] = self.vit (TSUC, P[1], AMIX, BMIX, VSUC, False)
			#CALL VIT(TSUC, P[1], AMIX, BMIX, VSUC, .FALSE., LCRIT)
			
			[H_SUC, CV, CP, DUM1] = self.hcvcps (3, TSUC, VSUC, X)	#CALL HCVCPS (3, TSUC, VSUC, X, H_SUC, CV, CP, DUM1)
			
			SSUC = self.entrop(TSUC, VSUC, X)                      ###Suction entropy

			#[P4 .. P11] = self.spin (P1, P2, P3 )
			[T2S, XQ[2], XL, XV,  VL2S, VV2S, SL2S, SV2S] = self.spin (SSUC, P[2], X )
			#CALL SPIN(SSUC, P[2], X, T2S, XQ[2], XL[1][2], XV[1][2], VL2S, VV2S, SL2S, SV2S)
					 
			[H2S, DUM1, DUM2, DUM3] = self.hcvcps (1, T2S, VV2S, X)	# CALL HCVCPS (1, T2S, VV2S, X, H2S, DUM1, DUM2, DUM3)

			if(Data.obj_cdata.ICOMP == 2) :
				ETAS = Data.obj_cdata.EFFC * (1.0 - 0.0010*(T_COND - 130.0)) * (1.0 + 0.0030*(T_EVAP + 10))
			else:
				ETAS = Data.obj_cdata.EFFC * (1.0 + 0.0010*(T_COND - 130.0)) * (1.0 + 0.0020*(T_EVAP + 10))
			#End if


			W = (H2S - H_SUC)/Data.obj_cdata.EFFC
			if(Data.obj_cdata.ICOOL == 1): W = (H2S - H_SUC)/ETAS

			GAMA = CP/CV
			RN = 0.97*GAMA
			RINV = 1.0/RN
			PR = P[2]/P[1]
			
			#
			#          ESTIMATE CYCLINDER TEMPERATURE AND CAN OUTLET TEMPERATURE
			#
			TDISC = TSUC*(P[2]/P[1])**(1.0-1.0/RN)
			
			[AMIX, BMIX] = self.espar (0, TDISC, X) #  CALL ESPAR(0, TDISC, X, AMIX, BMIX)
			VVD = R*TDISC/P[2]
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[VVD, LCRIT] = self.vit (TDISC, P[2], AMIX, BMIX, VVD, False)
			#CALL VIT(TDISC, P[2], AMIX, BMIX, VVD, .FALSE., LCRIT)
			
			[HDISC, CV, CP, DUM1] = self.hcvcps (1, TDISC, VVD, X) # CALL HCVCPS (1, TDISC, VVD, X, HDISC, CV, CP, DUM1)

			ETA_ISEN = (H2S - H_SUC)/(HDISC - H_SUC)

			if(Data.obj_cdata.ICOOL == 0) :
				RATIO = 0.68 - 0.05 * Data.obj_cdata.EER
			else:
				RATIO = 0.90 - 0.07 * Data.obj_cdata.EER
			#End if

			T[2] = TDISC - RATIO*(TDISC - TAMB)

			[AMIX, BMIX] = self.espar (0, T[2], X) # CALL ESPAR(0, T[2], X, AMIX, BMIX)
			
			VV2 = R*T[2]/P[2]
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[VV2, LCRIT] = self.vit (T[2], P[2], AMIX, BMIX, VV2, False)
			#CALL VIT(T[2], P[2], AMIX, BMIX, VV2, .FALSE., LCRIT)
			
			[H[2], CV, CP, DUM1] = self.hcvcps (1, T[2], VV2, X) # CALL HCVCPS (1, T[2], VV2, X, H[2], CV, CP, DUM1)

			QCAN  = 1.0 - (H[2] - H[1])/W
			QHILO = (HDISC - H[2])/W
		else:                                                 ###Physical model
			#
			#          FIND ENTROPY OF SUCTION GAS AND TEMPERATURE FOR DISCHARGE
			#          GAS FROM AN ISENTROPIC EXPANSION
			#
			ITER = 0
			ERROR = TOLS + 1

			TSUC = T[1] + 3.0
			VSUC = V[1]*TSUC/T[1]                             ###Suction density
			
			[HSUC,CV,CP,VS] = self.hcvcps (1,TSUC,VSUC,X) # CALL HCVCPS (1,TSUC,VSUC,X, HSUC,CV,CP,VS)
			
			while (ERROR > TOLS and ITER < 10):
				ITER = ITER + 1
				SSUC = self.entrop(TSUC,VSUC,X)
				#[P4 .. P11] = self.spin (P1, P2, P3 )
				[T[2],XQ[2],XL,XV,VL2, VV2,SL,SV] = self.spin (SSUC,P[2],X)
				#CALL SPIN (SSUC,P[2],X,  T[2],XQ[2],XL[1][2],XV[1][2],VL2, VV2,SL,SV)
				if(XQ[2] < 1.0) :
					[HL2,CV,CP,VS] = self.hcvcps (1,T[2],VL2,XL)	#CALL HCVCPS (1,T[2],VL2,XL[1][2],HL2,CV,CP,VS)
					[HV2,CV,CP,VS] = self.hcvcps (3,T[2],VV2,XV) 	#CALL HCVCPS (3,T[2],VV2,XV[1][2],HV2,CV,CP,VS)
					
					H[2] = XQ[2]*HV2 + (1.0-XQ[2])*HL2
				else:
					[H[2],CV,CP,VS] = self.hcvcps (3,T[2],VV2,X) # CALL HCVCPS (3,T[2],VV2,X,H[2],CV,CP,VS)
				#End if
				#
				#               DETERMINE ISENTROPIC EFFICIENCY
				#
				GAMA = CP/CV
				RN = 0.97*GAMA
				RINV = 1.0/RN
				PR = P[2]/P[1]
				H[2]=HSUC+(H[2]-HSUC)/SEFF
				#
				#               RE-CALCULATE SUCTION TEMPERATURE AND COMPARE
				#               WITH OLD VALUE
				#
				COEF1 = (1.0 - MEFF - (MEFF*QCAN-QHILO)/(1.0- QCAN))/(1.0 + QHILO/(1.0 - QCAN))
				H1P = H[1] + COEF1*(H[2] - H[1])
				
				[T1P, XQ1, XL, XV, VL2, VV2, HL2, HV2] = self.hpin ( H1P, P[1], X )
				#CALL HPIN(H1P, P[1], X,   T1P, XQ1, XL[1][2], XV[1][2], VL2, VV2, HL2, HV2)
				
				if(Data.obj_cdata.ICOMP == 2): T1P = TSUC
				ERROR = abs(T1P - TSUC)
				
				if(Data.obj_cdata.ICOMP == 1) :
					TSUC = T1P
					HSUC = H1P
					VSUC = VV2
				#End if
			#END DO
			#
			#          CORRECT DISCHARGE CONDITION FOR CAN LOSS
			#
			HDISC = H[2]
			#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
			[TDISC, XQ[2], XL, XV, VL2, VV2, HL2, HV2] = self.hpin ( HDISC, P[2], X )
			
			#CALL HPIN(HDISC, P[2], X, TDISC, XQ[2], XL[1][2], XV[1][2], VL2, VV2, HL2, HV2)
			H[2] = H[1] + ((H[2] - H[1]))/(1.0 + QHILO/(1.0 - QCAN))
			
			[T[2], XQ[2], XL, XV, VL2, VV2, HL2, HV2] = self.hpin ( H[2], P[2], X )
			#CALL HPIN(H[2], P[2], X,   T[2], XQ[2], XL[1][2], XV[1][2], VL2, VV2, HL2, HV2)
		
		#End if
		#
		#          CALCULATE MASS FLOW RATE
		#
		if(Data.obj_cdata.ICOMP == 1) :
			Data.obj_cdata.ETAV = 0.92*(1.0 - Data.obj_cdata.CE*(PR**RINV - 1.0))
		else:
			Data.obj_cdata.ETAV = 1.00*(1.0 - Data.obj_cdata.CE*(PR**RINV - 1.0))

		print ("aym =====1================ Data.obj_cdata.CE =", Data.obj_cdata.CE)
		print ("aym ===================== CP =", CP)
		print ("aym ===================== CV =", CV)
		print ("aym ===================== GAMA = CP/CV =", GAMA)
		print ("aym ===================== RN=0.97*GAMA =", RN)

		print ("aym ===================== RINV =1/RN=", RINV)
		print ("aym ===================== PR =", PR)
		print ("aym ===================== Data.obj_cdata.CE =", Data.obj_cdata.CE)
		print ("aym Data.obj_cdata.ETAV = 0.92*(1.0 - Data.obj_cdata.CE*(PR**RINV - 1.0))=", Data.obj_cdata.ETAV)
		
		Data.obj_cdata.DISP = Data.obj_cdata.MREF * VSUC/(60.0 * Data.obj_cdata.SPEED * Data.obj_cdata.ETAV)
		
		HOUT = H[2]
		ETAS = ETA_ISEN
				
		return [T,HOUT, QHILO, QCAN, VSUC, VV2, TSUC, TDISC, GAMA, RN, ETAS]


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Compressor Type : Map
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Comp_Map (Comp_Abstract): #Data.obj_cdata.IMAP== 0
	def comp_balance (self):
		OLDMAS = Data.obj_cdata.MREF
		lstRes = self.compcall (self.objData.H, self.objData.P, self.objData.X, self.objData.T, self.objData.V,   self.objData.TS1)

		Data.obj_cdata.MREF = (Data.obj_cdata. MREF + 2.0*OLDMAS)/ 3.0
		if (Data.obj_cdata.MREF >  1.05*OLDMAS): Data.obj_cdata.MREF = 1.05*OLDMAS
		if (Data.obj_cdata.MREF <  0.95*OLDMAS): Data.obj_cdata.MREF = 0.95*OLDMAS

		return lstRes

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def compcall(self, H,P,X,T,V,   TAMB):
		#	  SUBROUTINE COMPCALL (H,P,X,T,   CV,CP,    HOUT, MEFF, QHILO,
		#	 .                     QCAN, VSUC, V, VV2, TSUC, TDISC, TAMB,
		#	 .                     GAMA, RN,   ETAS)
		# CV, CP, MEFF not input nor output
		# input  H,    P,    X, T, V, TAMB, 
		# output (H, T ) both not_used in output) , CV, CP, HOUT, QHILO, QCAN, VSUC, VV2, TSUC, TDISC, GAMA. RN, ETAS
		#
		# [P5, P7, P9 to P11, P13,P14, P15,P17 to P19 ] = self.compcall (P1 to P6, P12, P16)
		#     ******************************************************************
		#     *    TRUE COMPRESSOR MAP ROUTINE.  APPLIES TO REFRIGERANT        *
		#     *    SUBROUTINE COMPCALL CALCULATES ISENTROPIC COMPRESSOR        *
		#     *    PERFORMANCE AND CALLS SUBROUTINE COMPMAP                    *
		#     ******************************************************************
		#
		#	  LOGICAL         LCRIT
		#	  REAL            MEFF, MREF
		#	  DIMENSION       H(16), P(16), X(5), XQ(16), XL(5,16), XV(5,16),
		#	 .                T(16), V(16), XL2S(5,16), XV2S(5,16)
		
		#	  COMMON /PARMS/  ICOND, IFRSH, IFREZ, DISP, SPEED, CE, CREF, MREF,
		#	 .                ETAV, SEFF
		#
		# determine isentropic compressor performance
		#
		XL= [0.0] * (5+1) # modification in Python
		XV= [0.0] * (5+1)
		
		TSUCT = T[1]
		PSUCT = P[1]
		VSUCT = V[1]
		
		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[HVSUCT, CV, CP, VS] = self.hcvcps (3, TSUCT, VSUCT, X)
		#CALL HCVCPS(3, TSUCT, VSUCT, X, HVSUCT, CV, CP, VS)
		
		GAMA = CP/CV
		SSUCT = self.entrop(TSUCT, VSUCT, X)
		
		# [P2, P3 ,P4, P5, P6, P8] = self.bublp (P1, P2, P3 , P7 ) self.getArr2dCol (XL,2)
		[ XL, X, TDEW, XXX, VDEW, LCRIT] = self.bublp (P[2], XL, X , False )
		#CALL BUBLP(P[2], XL[1][2], X, TDEW, XXX, VDEW, False, LCRIT)
		
		#[P4 .. P11] = self.spin (P1, P2, P3 )
		[T2S, XQ2S, XL2S, XV2S, VL2, VV2, SL, SV] = self.spin (SSUCT, P[2], X )
		#CALL SPIN(SSUCT, P[2], X,    T2S, XQ2S, XL2S[1][2], XV2S[1][2], VL2,       VV2, SL, SV)
		
		[A2S, B2S] = self.espar (0, T2S, X)	#	CALL ESPAR(0, T2S, X, A2S, B2S)
		
		VGUESS = V[1]*T2S/T[1]*P[1]/P[2]
		
		#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
		[VGUESS, LCRIT] = self.vit (T2S, P[2], A2S, B2S, VGUESS, False)
		#CALL VIT(T2S, P[2], A2S, B2S, VGUESS, False, LCRIT)
		
		VV2 = VGUESS
		if (XQ2S  <  1.0):
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HL2, CV, CP, VS] = self.hcvcps (1, T2S, VL2, XL2S)
			#CALL HCVCPS(1, T2S, VL2, XL2S[1][2], HL2, CV, CP, VS)
			
			[HL2, CV, CP, VS] = self.hcvcps (1, T2S, VV2, XV2S)
			#CALL HCVCPS(1, T2S, VV2, XV2S[1][2],  HV2, CV, CP, VS)
			
			H2S = XQ2S*HV2 + (1.0-XQ2S)*HL2
		else:
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HV2, CV, CP, VS] = self.hcvcps (1, T2S, VV2, XV2S)
			#CALL HCVCPS(1, T2S, VV2, XV2S[1][2], HV2, CV, CP, VS)
			
			H2S = HV2
		#END if 
		#
		#          CALCULATE ISENTROPIC POWER REQUIREMENT
		#
		WDOTS = Data.obj_cdata.MREF*(H2S-H[1])
		#
		#          DETERMINE ACTUAL COMPRESSOR PERFORMANCE
		#
		#[ P8, P9, P10, P11 , P12] = self.compmap (P1..P7 )
		[ TSUC, WDOT, Data.obj_cdata.MREF, QSHELL, Data.obj_cdata.SPEED] = self.compmap (P[1], P[2], T[1], V[1], TAMB, X, GAMA )
		#CALL COMPMAP(P[1], P[2], T[1], V[1], TAMB, X, GAMA, TSUC, WDOT, Data.obj_cdata.MREF, QSHELL, Data.obj_cdata.SPEED)
		#
		#          CALCULATE REFRIGERANT EXIT ENTHALPY AND TEMPERATURE
		#
		fact = QSHELL / WDOT  # useless not used anywhere
		
		H[2] = H[1] + (WDOT-QSHELL)/Data.obj_cdata.MREF
		HOUT = H[2] # send to output
		QCAN = QSHELL/WDOT
		
		#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
		[T[2], XQ[2], XL, XV, VL2, VV2,  HL2, HV2] = self.hpin ( H[2], P[2], X )
		#CALL HPIN(H[2], P[2], X, T[2], XQ[2], XL[1][2], XV[1][2], VL2, VV2,  HL2, HV2)
		
		TDISC = T[2] # send to output
		#
		#          CALCULATE ISENTROPIC EFFICIENCY
		#
		ETAS = WDOTS/WDOT
		#
		#           USE CALL STATEMENT ARGUMENTS TO AVOID COMPILIER WARNING
		#
		# MEFF = MEFF useless
		VSUC = V[1]
		QHILO = 0.0
		RN = 0.97*GAMA
		return [T, HOUT, QHILO, QCAN, VSUC, VV2, TSUC, TDISC, GAMA. RN, ETAS]
	
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def compmap (self, PSUCT, PDISC, TSUCT, VSUCT, TAMB, X, GAMA):
		#	  SUBROUTINE COMPMAP (PSUCT, PDISC, TSUCT, VSUCT, TAMB, X, GAMA,
		#	 .                    TSP, WDOT, MDOT, QSHELL, SPEED)
		#[ P8, P9, P10, P11 , P12] = self.compmap (P1..P7 )
		#     ******************************************************************
		#     *    CALCULATES COMPRESSOR PERFORMANCE BASED ON TABULAR MAP      *
		#     *    DATA AND CORRECTS FOR SUCTION TEMPERATURE OTHER THAN 90F    *
		#     ******************************************************************
		#
		#
		#	  LOGICAL         FOUND_DATA
		#	  REAL            MDOT, MDOT90
		#	  DIMENSION       TEDATA(20), TCDATA(20), CAPAC(20,20), POWER(20,20)
		#	  DIMENSION       X(5), XX(5)
		#	  LOGICAL         LCRIT, LCONV
		#	  COMMON /MAPDAT/ IMAP, ICOMP, ICOOL, EER, SIZE, DISPL, EFFC,
		#	 .                SPEEDN, IREAD
					
		# INCOMP =15 not requirted in Python
		QLOSS = 1.00
		Data.obj_cdata.TEDATA= [0.0] * (20+1)
		Data.obj_cdata.TCDATA= [0.0] * (20+1)
		
		X     = [0.0] * (5+1)
		XX    = [0.0] * (5+1)
		
		Data.obj_cdata.CAPAC = [[0.0] * (20+1) for i in range(20+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		Data.obj_cdata.POWER = [[0.0] * (20+1) for i in range(20+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		#============Python commnet : data description and sample data
		'''
			SAMPLE CALORIMETER-BASED MAP 
			default data:
			4.5 mass flow at standard rating point (lb/hr or kg/hr) 
			3	number of data points along evaporating temperature axis 
			3 	number of data points along condensing temperature axis 
			1 	compressor type (1 - reciprocating; 2 - rotary) 
			1	units for capacity, temperature data, and mass flow (1 - btu/hr, deg f, lb/hr; 2 - kcal/hr, deg c, kg/hr) power data must be in watts

			CAPACITY DATA, BTU/HR
			COND TEMP (F) 	EVAPORATING TEMPERATURE (F)  
					-20 	-10 	0
			110		113.6	127.7 	142.6 
			120		115.8 	132.4 	148.9 
			130		118.3 	135.9 	154.4 

			SMOOTHED MAP CREATED BY REMAP
			default data
			12.2	mass flow at standard rating point (lb/hr or kg/hr)
			6	number of data points along evaporating temperature axis
			7	number of data points along condensing temperature axis
			1	compressor type (1 - reciprocating; 2 - rotary)
			1	units for capacity, temperature data, and mass flow (1 - btu/hr, deg f, lb/hr; 2 - kcal/hr, deg c, kg/hr) power data must be in watts

			CAPACITY DATA, BTU/HR 

			COND TEMP (F) 	EVAPORATING TEMPERATURE(F)
					-40		-30		-20 	10		0		10
			70		370.4	500.5	659.3	851.1	1080.8	1353.5
			80		358.1	488.2	647.0	838.8	1068.5	1341.2
			90		344.3	474.4	633.1	824.9	1054.6	1327.2
			100		328.9	458.9	617.7	809.4	1039.0	1311.6
			110		311.8	441.8	600.4	792.1	1021.6	1294.1
			120		292.9	422.8	581.4	772.9	1002.3	1274.6
			130		272.1	401.9	560.3	751.8	980.9	1253.0
			 
			 
			COMPRESSOR POWER (WATTS)
			COND TEMP(F) 	EVAPORATING TEMPERATURE (F)
					-40 	-30		-20		-10		0 		10
			70		65.5	75.6	84.8	92.3	97.7	100.1
			80		69.1	80.9	91.9	101.7	109.5	114.7
			90		72.2	85.7	98.5	110.4	120.6	128.6
			100		74.6	89.8	104.6	118.5	131.1	141.8
			110		76.4	93.3	110.0	126.0	141.0	154.2
			120		77.3	96.1	114.7	132.9	150.2	166.0
			130		77.3	98.1	118.7	139.0	158.6	177.0		
		'''
		#==============================================================
		if (Data.obj_cdata.IREAD == 0) :
			obj_comp_map = CompMap (self.str_Comp_File, self.FLDER_COMPMAP_DAT)

			if obj_comp_map.isError():
				print (obj_comp_map.err_description())
				sys.exit('6000')
				
			obj_comp_map.readMapData()
			if obj_comp_map.isError():
				print (obj_comp_map.err_description())
				sys.exit('6001')
				
			# Python comment:
			# NEVAP : I3 number of data points along evaporating temperature axis
			# NCOND : I3 number of data points along condensing temperature axis 
			# Data.obj_cdata.ICOMP : I1 compressor type (1 - reciprocating; 2 - rotary) 
			# IUNITS : I1 units for capacity, temperature data, and mass flow (1 - btu/hr, deg f, lb/hr; 2 - kcal/hr, deg c, kg/hr) power data must be in watts
			
			NEVAP = obj_comp_map.getX_count ()
			NCOND = NEVAP #obj_comp_map.getY_count () 
			
			Data.obj_cdata.ICOMP = 1 # no info in file
			Data.obj_cdata.IUNITS = obj_comp_map.getUnit ()

			# Python commnet : read EVAPORATING TEMPERATURE - x axis
			#for II in range (1, NEVAP+1 ):
			#	TEDATA[II] = inastk ( objCompMap, "float" ) 
			
			Data.obj_cdata.TEDATA = obj_comp_map.getX_values ()
				
			# READ COMPRESSOR CAPACITY DATA

			#for I in range (1, NCOND +1) : #DO I = 1,NCOND
			#	TCDATA[I] = inastk ( objCompMap, "float" ) # Python commnet: first number is COND TEMP (y axis)
				
			#	for J in range (1, NEVAP+1 ):
			#		CAPAC[I][J] = inastk ( objCompMap, "float" )
					
			Data.obj_cdata.TCDATA = obj_comp_map.getY1_values ()
			Data.obj_cdata.CAPAC = obj_comp_map.getCapacity ()
				
			# READ COMPRESSOR POWER DATA
			#for I in range (1, NCOND +1) : #DO I = 1,NCOND
			#	DUMMY = inastk ( objCompMap, "float" ) # Python commnet: first number is COND TEMP (y axis)
				
			#	for J in range (1, NEVAP+1 ):
			#		POWER[I][J] = inastk ( objCompMap, "float" )
			
			Data.obj_cdata.POWER = obj_comp_map.getPower ()
			
			Data.obj_cdata.IREAD = 1
			del(obj_comp_map) 	# close file
			
		#
		#          DETERMINE THE SATURATION TEMPERATURES CORRESPONDING TO PSUCT, PDISC
		#
		# [P2, P3 ,P4, P5, P6, P8] = self.bublp (P1, P2, P3 , P7 )
		[XX, X, TEVAPK, VL, VDEW, LCRIT] = self.bublp (PSUCT, XX, X , False )
		#CALL BUBLP(PSUCT, XX, X, TEVAPK, VL, VDEW, False, LCRIT)
		
		[ X, XX, TCONDK, VBUB, VV, LCRIT] = self.bublp (PDISC, X, XX , True )
		#CALL BUBLP(PDISC, X, XX, TCONDK, VBUB, VV, True, LCRIT)
		#
		#          DETERMINE THE ENTHALPIES AT EACH PRESSURE FOR THE FOLLOWING:
		#            VAPOR - 90F
		#            LIQUID - 90F
		#
		 
		TEMPV = (90.0+459.67)/1.8
		TEMPL = (90.0+459.67)/1.8
		#
		#          FIRST CALCULATE THE SPECIFIC VOLUMES
		#
		VGUESS = VDEW*TEMPV/TEVAPK
		# [P4, P5] = self.espar (P1 to P3)
		[A, B] = self.espar (0, TEMPV, X)		#CALL ESPAR(0, TEMPV, X, A, B)
		
		#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
		[VGUESS, LCONV] = self.vit (TEMPV, PSUCT, A, B, VGUESS, False)
		#CALL VIT(TEMPV, PSUCT, A, B, VGUESS, False, LCONV)
		
		VVAP = VGUESS    # vapor specific volume

		VGUESS = VBUB
		[A, B] = self.espar (0, TEMPL, X)	#CALL ESPAR(0, TEMPL, X, A, B)
		
		#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
		[VGUESS, LCONV] = self.vit (TEMPL, PDISC, A, B, VGUESS, True)
		#CALL VIT(TEMPL, PDISC, A, B, VGUESS, True, LCONV)
		
		VLIQ = VGUESS    # liquid specific volume
		#
		#          VAPOR ENTERING THE COMPRESSOR
		#
		
		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[HIN, CV, CP, VS] = self.hcvcps (1, TEMPV, VVAP, X)
		#CALL HCVCPS(1, TEMPV, VVAP, X, HIN, CV, CP, VS)
		
		#
		#          LIQUID LEAVING CONDENSER
		#
		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[HOUT, CV, CP, VS] = self.hcvcps (1, TEMPL, VLIQ, X)
		#CALL HCVCPS(1, TEMPL, VLIQ, X,  HOUT, CV, CP, VS)
		#
		#           DETERMINE ISENTROPIC COMPRESSION ENTHALPY (HS)
		#
		SSUC = self.entrop(TEMPV, VVAP, X)
		#[P4 .. P11] = self.spin (P1, P2, P3 )
		[TS, XQS, XLS, XVS, VLS, VVS, SL, SV] = self.spin (SSUC, PDISC, X)
		#CALL SPIN (SSUC, PDISC, X, TS, XQS, XLS, XVS, VLS, VVS, SL, SV)
		
		VGUESS = VVAP
		[AE, BE] = self.espar (00, TS, X)	#CALL ESPAR(0, TS, X, AE, BE)
		
		#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
		[VGUESS, LCONV] = self.vit (TS, PDISC, AE, BE, VGUESS, False)
		#CALL VIT(TS, PDISC, AE, BE, VGUESS, False, LCONV)
		
		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[HS, CVF, CPF, VSND] = self.hcvcps (1, TS, VGUESS, X)
		#CALL HCVCPS(1, TS, VGUESS, X,   HS, CVF, CPF, VSND)
		#
		#           CONVERT THE SATURATION TEMPERATURES TO CORRESSPOND TO MAP DATA UNITS
		#
		if (IUNITS  ==  1) :
			TEVAP = TEVAPK*1.8 - 459.67
			TCOND = TCONDK*1.8 - 459.67
		else:
			TEVAP = TEVAPK - 273.16
			TCOND = TCONDK - 273.16
		#END if 
		#
		#          CHECK if  TCOND AND/OR TEVAP IS OFF MAP DATA
		#
		ICOND = 1
		if (TCOND  <  Data.obj_cdata.TCDATA[1]): ICOND = 0
		if (TCOND  >  Data.obj_cdata.TCDATA[NCOND]): ICOND = NCOND
		IEVAP = 1
		if (TEVAP  <  Data.obj_cdata.TEDATA[1]): IEVAP = 0
		if (TEVAP  >  Data.obj_cdata.TEDATA[NEVAP] ): IEVAP = NEVAP
		 
		#
		#          THIS CODING WILL INTERPOLATE IF DATA IS WITHIN THE MAP OR
		#          EXTROPOLATE IF TCOND AND/OR TEVAP ARE/IS LESS THAN MAP DATA
		#
		#          DETERMINE LOCATION WITHIN MAP
		#
		if (ICOND <=  1  and IEVAP <=  1) :
			if (ICOND  ==  1) :
				I = 1
				while (TCOND  >  TCDATA[I]):
					I = I + 1
					
				ICOND = I - 1
		 
			else:
				ICOND = 1
		 
			I = 1
			if (IEVAP  ==  1) :
				while (TEVAP  >  TEDATA[I]):
					I = I + 1

				IEVAP = I - 1
		 
			else:
				IEVAP = 1

			#
			#          COMPRESSOR CAPACITY INTERPOLATION
			#
			DELTC = Data.obj_cdata.TCDATA[ICOND+1] - Data.obj_cdata.TCDATA[ICOND]
			DELTE = Data.obj_cdata.TEDATA[IEVAP+1] - Data.obj_cdata.TEDATA[IEVAP]
			
			FRAC = (TCOND - Data.obj_cdata.TCDATA[ICOND])/DELTC
			CAP1 = CAPAC[ICOND][IEVAP]   + ( CAPAC[ICOND+1][IEVAP]  - CAPAC[ICOND][IEVAP]   ) * FRAC
			CAP2 = CAPAC[ICOND][IEVAP+1] + ( CAPAC[ICOND+1][IEVAP+1]- CAPAC[ICOND][IEVAP+1] ) * FRAC
			
			FRAC = (TEVAP-TEDATA[IEVAP] )/DELTE
			CAP = CAP1 + (CAP2-CAP1)*FRAC
			#
			#          COMPRESSOR POWER INTERPOLATION
			#
			FRAC = (TCOND - Data.obj_cdata.TCDATA[ICOND])/DELTC
			POW1 = Data.obj_cdata.POWER[ICOND][IEVAP]   + (Data.obj_cdata.POWER[ICOND+1][IEVAP]  - Data.obj_cdata.POWER[ICOND][IEVAP]   ) * FRAC
			POW2 = Data.obj_cdata.POWER[ICOND][IEVAP+1] + (Data.obj_cdata.POWER[ICOND+1][IEVAP+1]- Data.obj_cdata.POWER[ICOND][IEVAP+1] ) * FRAC
			FRAC = (TEVAP-TEDATA[IEVAP] )/DELTE
			POW = POW1 + (POW2-POW1)*FRAC
 
		#
		#          TCOND GREATER THAN OR EQUAL THE MAXIMUM CONDENSING TEMP DATA POINT
		#
		if (ICOND  ==  NCOND) :
			if (IEVAP <=  1) :
				I = 1
				if (IEVAP  ==  1) :
					while (TEVAP  >  Data.obj_cdata.TEDATA[I]):
						I = I + 1
					#END DO
					IEVAP = I-1
				else:
					IEVAP = 1
				#END if 
				
				#          COMPRESSOR CAPACITY CALCULATION
				#
				DELTC = Data.obj_cdata.TCDATA[ICOND] - Data.obj_cdata.TCDATA[ICOND-1]
				DELTE = Data.obj_cdata.TEDATA[IEVAP+1] - Data.obj_cdata.TEDATA[IEVAP]
				FRAC = (TCOND - Data.obj_cdata.TCDATA[ICOND])/DELTC
				FRAC2 = FRAC
				CAP1 = Data.obj_cdata.CAPAC[ICOND][IEVAP]   + (CAPAC[ICOND][IEVAP]   - Data.obj_cdata.CAPAC[ICOND-1][IEVAP]   ) * FRAC
				CAP2 = Data.obj_cdata.CAPAC[ICOND][IEVAP+1] + (CAPAC[ICOND][IEVAP+1] - Data.obj_cdata.CAPAC[ICOND-1][IEVAP+1] ) * FRAC
				
				FRAC = (TEVAP-TEDATA[IEVAP] )/DELTE
				CAP = CAP1 + (CAP2-CAP1)*FRAC
				#
				#          COMPRESSOR POWER CALCULATION
				#
				FRAC = (TCOND-TCDATA[ICOND])/DELTC
				
				POW1 = POWER[ICOND][IEVAP]   + (POWER[ICOND][IEVAP]   - POWER[ICOND-1][IEVAP]   ) * FRAC
				POW2 = POWER[ICOND][IEVAP+1] + (POWER[ICOND][IEVAP+1] - POWER[ICOND-1][IEVAP+1] ) * FRAC
			
				FRAC = (TEVAP - Data.obj_cdata.TEDATA[IEVAP] )/DELTE
				POW = POW1 + (POW2-POW1)*FRAC
			else:
				#
				#          COMPRESSOR CAPACITY CALCULATION
				#
				DELTC = TCDATA[ICOND] - TCDATA[ICOND-1]
				DELTE = TEDATA(IEVAP) - TEDATA[IEVAP-1]
				FRAC = (TCOND-TCDATA[ICOND] )/DELTC
				
				CAP1 = CAPAC[ICOND][IEVAP-1] + (CAPAC[ICOND][IEVAP-1] - CAPAC[ICOND-1][IEVAP-1] ) * FRAC
				CAP2 = CAPAC[ICOND][IEVAP]   + (CAPAC[ICOND][IEVAP]   - CAPAC[ICOND-1][IEVAP]   )  * FRAC
				
				FRAC = (TEVAP - TEDATA[IEVAP])/DELTE
				CAP = CAP2 + (CAP2-CAP1)*FRAC
				#
				#          COMPRESSOR POWER CALCULATION
				#
				FRAC = (TCOND-TCDATA[ICOND])/DELTC
				POW1 = POWER[ICOND][IEVAP-1] + (POWER[ICOND][IEVAP-1]- POWER[ICOND-1][IEVAP-1] )*FRAC
				POW2 = POWER[ICOND][IEVAP]   + (POWER[ICOND][IEVAP]  - POWER[ICOND-1][IEVAP]   )*FRAC
				
				FRAC = (TEVAP-TEDATA[IEVAP])/DELTE
				POW = POW2 + (POW2-POW1)*FRAC
			#END if 
		#END if 
		#
		#          CONDENSING TEMPERATURE NOT GREATER THAN MAXIMUM OF MAP DATA
		#          EVAPORATING TEMPERATURE GREATER THAN MAXIMUM OF MAP DATA
		#
		if (IEVAP  ==  NEVAP  and ICOND  <  NCOND) :
			if (ICOND  ==  1) :
				I = 1
				while (TCOND > TCDATA[I]):
					I = I + 1
				#END DO
				ICOND = I - 1
			else:
				ICOND = 1
			#END if 
			#
			#          COMPRESSOR CAPACITY CALCULATION
			#
			DELTC = TCDATA[ICOND+1] - TCDATA[ICOND]
			DELTE = TEDATA[IEVAP] - TEDATA[IEVAP-1]
			FRAC = (TCOND-TCDATA[ICOND])/DELTC
			
			CAP1 = CAPAC[ICOND][IEVAP-1] + (CAPAC[ICOND+1][IEVAP-1] - CAPAC[ICOND][IEVAP-1] ) * FRAC
			CAP2 = CAPAC[ICOND][IEVAP]   + (CAPAC[ICOND+1][IEVAP]   - CAPAC[ICOND][IEVAP]   ) * FRAC
			
			FRAC = (TEVAP-TEDATA[IEVAP] )/DELTE
			CAP = CAP2 + (CAP2-CAP1)*FRAC
			#
			#          COMPRESSOR POWER CALCULATION
			#
			FRAC = (TCOND-TCDATA[ICOND])/DELTC
			
			POW1 = POWER[ICOND][IEVAP-1] + (POWER[ICOND+1][IEVAP-1] - POWER[ICOND][IEVAP-1] ) * FRAC
			POW2 = POWER[ICOND][IEVAP]   + (POWER[ICOND+1][IEVAP]   - POWER[ICOND][IEVAP] )   * FRAC			
			
			FRAC = (TEVAP-TEDATA[IEVAP] )/DELTE
			POW = POW2 + (POW2-POW1)*FRAC
		#END if 
		 
		## handle off-speed operation (use Danfoss variable speed data)
		REL_CAP = -0.046073 + 1.41364 * SPEED - 0.366744 * SPEED * SPEED
		CAP = CAP * REL_CAP
		POW = POW * REL_CAP

		## correct for power term based on UI document UILU-ENG_96-4022
		REL_POW = 0.9535 + 0.04565 * SPEED
		POW = POW * REL_POW
		
		#
		#          CONVERT THE CAPACITY TO KJ/HR
		#
		if (IUNITS  ==  1) :
			CAP = CAP*1.0548
		else:
			CAP = CAP*4.184
		#END if 
		
		if (IUNITS  != 1  and IUNITS  != 2) :
			#print ("WRITE(6, '(''CHECK COMPRESSOR MAP UNITS##'')'")
			print ("###CHECK COMPRESSOR MAP UNITS###")
		#END if 
		#
		WDOT = POW
		#
		#          CONVERT TO KJ/HR
		#
		WDOT = WDOT/1000.0*3600.0
		WDOT90 = WDOT
		#
		#          CALCULATE THE MASS FLOW RATE IN MOLES/HR
		#
		MDOT = CAP/(HIN-HOUT)
		MDOT90 = MDOT
		#
		#          CORRECT MASS FLOW RATE FOR SUCTION TEMPERATURE OTHER THAN 90 F
		#
		T90 = (90 + 459.67) / 1.8
		MDOT = MDOT90 * VVAP / VSUCT
		#
		#          ESTIMATE EFFECT ON COMPRESSOR POWER AS RATIO OF
		#          SUCTION PLENUM TEMPERATURES
		#
		EFFS = MDOT90 * (HS - HIN) / WDOT90
		
		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[ HSUCT, CV, CP, VS] = self.hcvcps (1, TSUCT, VSUCT, X)
		#CALL HCVCPS(1, TSUCT, VSUCT, X, HSUCT, CV, CP, VS)
		 
		SSUC = self.entrop(TSUCT, VSUCT, X)
		#[P4 .. P11] = self.spin (P1, P2, P3 )
		[TS, XQS, XLS, XVS, VLS, VVS, SL, SV] = self.spin (SSUC, PDISC, X)
		#CALL SPIN (SSUC, PDISC, X, TS, XQS, XLS, XVS, VLS, VVS, SL, SV)
		VGUESS = VSUCT
		
		[AE, BE] = self.espar (0, TS, X)	#		CALL ESPAR(0, TS, X, AE, BE)
		
		#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
		[VGUESS, LCONV] = self.vit (TS, PDISC, AE, BE, VGUESS, False)
		#CALL VIT(TS, PDISC, AE, BE, VGUESS, False, LCONV)
		
		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[ CVF, CPF, VSND] = self.hcvcps (1, TS, VGUESS, X, HS)
		#CALL HCVCPS(1, TS, VGUESS, X, HS,  CVF, CPF, VSND)
		 
		WDOT = MDOT * (HS - HSUCT) / EFFS
		#
		#          ESTIMATE SHELL HEAT LOSS INCLUDING EFFECT OF DIFFERENT AMBIENT
		#
		if  (Data.obj_cdata.ICOMP  ==  1) :                    # Reciprocating compressor
			DELTIN = 67.0
			TSUCTF = TSUCT * 1.8 - 459.67
			DTSUCT = 67.0 - 0.43333 * (TSUCTF - 90.0)
			TSP = TSUCT + DTSUCT / 1.8

		else:                                     # Rotary compressor
			 DELTIN = 30.0
			 TSP = TSUCT + DELTIN / 1.8
		#END if 
	
		EXP = (GAMA - 1.0) / GAMA
		PRAT = PDISC / PSUCT
		TSP90 = (90.0 + DELTIN + 459.67) / 1.8
		AAA = PRAT**EXP
		TMAX90= TSP90 * AAA
		TMAX = TSP * AAA
		T90K = (90.0 + 459.67) / 1.8
		RATIO = (TMAX - TAMB) / (TMAX90 - T90K)

		QLOSS = 0.90
		QLOSS = 0.80
		QSHELL = WDOT * QLOSS * RATIO
		 
		return [TSP, WDOT, MDOT, QSHELL, SPEED]

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	def map (self, ICOMP, ICOOL, EER, SIZE, DISPL , SPEEDN):
		return [0.0,0.0]
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Compressor Type : EER
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Comp_ERR (Comp_Abstract): #Data.obj_cdata.IMAP== 1
	def comp_balance (self):
		# CALL THEORETICALLY BASED MODEL
		
		lstRes = self.comp (self.objData.H, self.objData.P, 	 self.objData.X, \
						  self.objData.T, self.objData.MEFF, self.objData.QHILO, \
						  self.objData.QCAN, self.objData.V, self.objData.TS1 )
		#
		#	UPDATE THE MASS FLOW TO CORRESPOND TO THE DISPLACEMENT
		
		DISPI = Data.obj_cdata.DISP/1.6387E-05
		OLDMAS = Data.obj_cdata.MREF

		Data.obj_cdata.MREF = (Data.obj_cdata.MREF * (self.objData.DISPLC/DISPI) + 2.0*OLDMAS)/3.0
		if (Data.obj_cdata.MREF >  1.05*OLDMAS): Data.obj_cdata.MREF = 1.04*OLDMAS
		if (Data.obj_cdata.MREF <  0.95*OLDMAS): Data.obj_cdata.MREF = 0.96*OLDMAS
		return lstRes		
		
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def map (self, ICOMP, ICOOL, EER, SIZE, DISPL , SPEEDN):
		# [ P6 P7] = self.cnat (P1 to P5, P8 )
		#	  SUBROUTINE MAP(ICOMP, ICOOL, EER, SIZE, DISPL, ETAC, CE, SPEEDN)
		#     ******************************************************************
		#     *    USE COMPRESSOR MAP DATA TO ESTIMATE COMPRESSOR BEHAVIOR     *
		#     ******************************************************************
		#
		#     INPUT PARAMETERS
		#     ----------------
		#        ICOMP                    TYPE OF COMPRESSOR:
		#                                   1=RECIPROCATING, 2=ROTARY
		#        ICOOL                    TYPE OF COMPRESSOR CAN COOLING:
		#                                   0=STATIC, 1=FAN FORCED
		#        EER                      EER AT RATING CONDITIONS
		#        SIZE                     CAPACITY (BTUH) AT RATING CONDITIONS
		#        DISPL                    DISPLACEMENT (CU-IN)
		#
		#     OUTPUT PARAMETERS
		#     ----------------
		#        ETAC                     COMPRESSOR ISENTROPIC EFFICIENCY,
		#                                 INCLUDING MECHANICAL AND MOTOR LOSSES
		#        CE                       CLEARANCE VOLUME (FRACTION)
		#
		#	  LOGICAL LCRIT, EXISTS
		#	  REAL K, MASS, MOTOR

		#	  DIMENSION X(5),  IR(5),  F(5,5), XL(5,5), XV(5,5)

		#	  DIMENSION CAP(3,3), PWR(3,3), ETAS(3,3), ETAV(3,3), MASS(3,3),
		#	 .          CEIJ(3,3)

		#Note: 	first  index = cond temperature from 110 to 130 F
		#		second index = evap temperature from -20 to   0 F

		#IN = 1
		T90 = 305.3889			# 90F in Kelvin
		ETAP =0.97 				# Get k from gamma

		#          CONVERSION FUNCTIONS
		def F_TO_K(T):
			return (T + 459.7)/1.8


		ETAS = [[0.0] * (3+1) for i in range(3+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		ETAV = [[0.0] * (3+1) for i in range(3+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		MASS = [[0.0] * (3+1) for i in range(3+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		CEIJ = [[0.0] * (3+1) for i in range(3+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]

		XL= [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		XV= [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		IR= [0.0] * (5+1)

		#          NORMALIZE
		#
		ITAB = 0
		X = [0.0] * (5+1)
		F = [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]

		X[1]  = 1
		IR[1] = 2		# Assume CFC-12

		self.bconst (1, IR, F) #CALL BCONST(1,IR,F)

		if(ITAB == 0): EXISTS = False # Python comment allways  False

		MOTOR = 3.413 * SIZE/EER 		# Motor power in Btuh

		#for I in range (1, 3+1) #DO I = 1,3
		#	for J in range (1, 3+1) #DO J = 1,3
		#		if(not EXISTS) :
		#			CAP[I][J] = 1.0
		#			PWR[I][J] = 1.0

		#		CAP[I][J] = 1.0548 * CAP[I][J] * SIZE       #kJ/hr
		#		PWR[I][J] = 1.0548 * PWR[I][J] * MOTOR      #kj/hr

		CAP = [ [1.0548 * SIZE ] * (3+1) for i in range(3+1) ]	# kJ/hr
		PWR = [ [1.0548 * MOTOR] * (3+1) for i in range(3+1) ]	# kJ/hr

		# CALCULATE THE MASS FLOWS ASSUMING 90 F LIQUID AND VAPOR TEMPS
		#
		for I in range (1, 3+1): #DO I = 1, 3
			T_COND = 100 + 10 * I
			T_COND = F_TO_K(T_COND)

			#CALL BUBLT(T_COND, X, XV, P_COND, VL, VV, .TRUE., LCRIT)
			# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
			[ X, XV, P_COND, VL, VV, LCRIT] = self.bublt( T_COND, X, XV, True)

			#CALL ESPAR(0, T90, X, AMIX, BMIX)
			# [P4, P5] = self.espar (P1, P2, P3)
			[AMIX, BMIX] = self.espar (0, T90, X )

			VS = VL
			#CALL VIT(T90, P_COND, AMIX, BMIX, VS, .TRUE., LCRIT)
			#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
			[VS, LCRIT] = self.vit ( T90, P_COND, AMIX, BMIX, VS ,True)

			#CALL HCVCPS (1, T90, VS, X, H_LIQ, DUM1, DUM2, DUM3)
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[H_LIQ, DUM1, DUM2, DUM3] = self.hcvcps (1, T90, VS, X)

			for J in range (1, 3+1): #DO J = 1, 3
				T_EVAP = -30 + 10*J
				T_EVAP = F_TO_K(T_EVAP)

				#CALL BUBLT(T_EVAP, XL, X, P_EVAP, VL, VV, False, LCRIT)
				# [P2, P3 ,P4, P5, P6, P8] = bublt (P1, P2, P3 , P7 )
				[ XL, X, P_EVAP, VL, VV, LCRIT] = self.bublt( T_EVAP, XL, X, False)

				#CALL ESPAR(0, T90, X, AMIX, BMIX)
				# [P4, P5] = self.espar (P1, P2, P3)
				[AMIX, BMIX] = self.espar (0, T90, X )

				VS = VV * T90/T_EVAP

				#CALL VIT(T90, P_EVAP, AMIX, BMIX, VS, False, LCRIT)
				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[VS, LCRIT] = self.vit ( T90,P_EVAP, AMIX, BMIX, VS ,False)

				#CALL HCVCPS (1, T90, VS, X, H_VAP, DUM1, DUM2, DUM3)
				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[H_VAP, DUM1, DUM2, DUM3] = self.hcvcps (1, T90, VS, X)

				MASS[I][J]  = CAP[I][J]/(H_VAP - H_LIQ)     #kg-mole/hr
				#
				#          ESTIMATE THE SUCTION GAS TEMPERATURE AND THE EFFICIENCIES
				#
				if(ICOOL == 0) :
				   TSUC = 479.59 - 64.815 * EER
				else:
				   TSUC = 427.84 - 57.895 * EER
				#end if

				TSUC = TSUC - 2.0*(3-I) - 2.0*(J-2)

				if(ICOMP == 2): TSUC = 120.0                  #Rotary

				TSUC = F_TO_K(TSUC)                       #K
				VSUC = VV*T90/TSUC                        #Suction density

				#CALL ESPAR(0, TSUC, X, AMIX, BMIX)
				# [P4, P5] = self.espar (P1, P2, P3)
				[AMIX, BMIX] = self.espar (0, TSUC, X )

				#CALL VIT(TSUC, P_EVAP, AMIX, BMIX, VSUC, False, LCRIT)
				#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
				[VSUC, LCRIT] = self.vit ( TSUC, P_EVAP, AMIX, BMIX, VSUC ,False)

				#CALL HCVCPS (3, TSUC, VSUC, X, H_SUC, CV, CP, DUM1)
				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[H_SUC, CV, CP, DUM1] = self.hcvcps (3, TSUC, VSUC, X)

				SSUC = self.entrop(TSUC, VSUC, X)              #Suction entropy

				#CALL SPIN(SSUC, P_COND, X, T2S, XQ, XL, XV, VL2S, VV2S, SL2S, SV2S)
				[T2S, XQ, XL, XV, VL2S, VV2S, SL2S, SV2S] = self.spin (SSUC, P_COND, X )

				#CALL HCVCPS (1, T2S, VV2S, X, H2S, DUM1, DUM2, DUM3)
				#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
				[H2S, DUM1, DUM2, DUM3] = self.hcvcps (1, T2S, VV2S, X)

				ETAS[I][J] = MASS[I][J]*(H2S - H_SUC)/PWR[I][J]

				# not the ETAV in common
				ETAV[I][J] = MASS[I][J]*VSUC/(60.0*SPEEDN)/(DISPL/61023.6)

				K = ETAP*CP/CV
				PR = P_COND/P_EVAP

				if(ICOMP == 1) :
					CEIJ[I][J] = ( (0.92 - ETAV[I][J] )/0.92)/(PR**(1.0/K) - 1.0)
				else:
					CEIJ[I][J] = ( (1.00 - ETAV[I][J] )/1.00)/(PR**(1.0/K) - 1.0)
				
				
				print ("aym I=", I, "J=", J)
				print ("aym --------------------")
				R12_TABLE_READING = 200.0 - 27.10795349661135	# kj/kg   200-app result at 0C need to be 200, valid only for RF12
				MOLAR_WEIGHT = 120.91

				H = H_LIQ
				print ("		H_LIQ in kJ/k-mole= %5.3f,  kJ/kg=%5.3f,  Reading in ASHRAE kJ/kg = %5.3f" %(H, H/MOLAR_WEIGHT, H/MOLAR_WEIGHT + R12_TABLE_READING) )

				H = H_VAP
				print ("		H_VAP in kJ/k-mole= %5.3f,  kJ/kg=%5.3f,  Reading in ASHRAE kJ/kg = %5.3f" %(H, H/MOLAR_WEIGHT, H/MOLAR_WEIGHT + R12_TABLE_READING) )

				H= H_SUC				
				print ("		H_SUC in kJ/k-mole= %5.3f,  kJ/kg=%5.3f,  Reading in ASHRAE kJ/kg = %5.3f" %(H, H/MOLAR_WEIGHT, H/MOLAR_WEIGHT + R12_TABLE_READING) )
				
				H= H2S
				print ("		H2S in kJ/k-mole= %5.3f,  kJ/kg=%5.3f,  Reading in ASHRAE kJ/kg = %5.3f" %(H, H/MOLAR_WEIGHT, H/MOLAR_WEIGHT + R12_TABLE_READING) )
				
				print ("aym --------------------")
				print ("aym PR = P_COND/P_EVAP = ",PR, " P_COND=", P_COND, " P_EVAP=",P_EVAP   )
				
				print ("aym CAP[I][J] = ", CAP[I][J])
				print ("aym  ETAP constatnt = ", ETAP)
				
				print ("aym K = ETAP*CP/CV = ", K, " ETAP=", ETAP, " CP=", CP, " CV=", CV)
				
				print ("aym T90 = 305.3889K K = ", T90-273.11, "C")
				
				print ("aym =============")
				print ("aym  VSUC=", VSUC , "VSUC/MOLAR_WEIGHT m3/kg =", VSUC/MOLAR_WEIGHT, "  SPEEDN=", SPEEDN, "  DISPL=", DISPL )
				
				print ("aym MASS[I][J]  = CAP[I][J]/(H_VAP - H_LIQ)     #kg-mole/hr = ", MASS[I][J])
				print ("aym ETAV[I][J] = MASS[I][J]*VSUC/(60.0*SPEEDN)/(DISPL/61023.6) = ", ETAV[I][J] )
				
				print ("aym CEIJ[I][J] = ( (0.92 - ETAV[I][J] )/0.92)/(PR**(1.0/K) - 1.0) = ", CEIJ[I][J] )
		
				print ("aym ---=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n\n")
		
				#
				#          ESTIMATE CYCLINDER TEMPERATURE AND CAN OUTLET TEMPERATURE
				#
				TCYL = TSUC * (P_COND / P_EVAP) ** (1.0-1.0/K)

				COP = CAP[I][J]/PWR[I][J]

				if(ICOOL == 0) :
					RATIO = 0.68 - 0.05*3.413 *COP
				else:
					RATIO = 0.90 - 0.07*3.413 * COP
				#end if

				TOUT = TCYL - RATIO * (TCYL - T90)

			#END DO
		#END DO

		#          CALCULATE THE OUTPUT VARIABLES
		#
		Data.obj_cdata.ETAC = 0
		Data.obj_cdata.CE   = 0
		for I in range (1, 3+1): #DO I = 1, 3
			for J in range (1, 3+1): #DO J = 1, 3
				Data.obj_cdata.ETAC = Data.obj_cdata.ETAC + ETAS[I][J]
				Data.obj_cdata.CE   = Data.obj_cdata.CE   + CEIJ[I][J]

		Data.obj_cdata.ETAC = Data.obj_cdata.ETAC/9.0
		Data.obj_cdata.CE   = Data.obj_cdata.CE/9.0

		if (not EXISTS) :
			Data.obj_cdata.ETAC = ETAS[3][2]
			Data.obj_cdata.CE   = CEIJ[3][2]

		print ("aym ===Final============== Data.obj_cdata.ETAC= ETAS[3][2] =", ETAS[3][2])
		
		print ("aym ===Final============== Data.obj_cdata.CE  = CEIJ[3][2] =",CEIJ[3][2])
		print ("aym ===Final===============              Data.obj_cdata.CE =", Data.obj_cdata.CE)
		return [Data.obj_cdata.ETAC, Data.obj_cdata.CE]
	
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Compressor Type : Efficiency Model
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class Comp_EMOD (Comp_Abstract): #Data.obj_cdata.IMAP== 2
	def comp_balance (self):
		# CALL THEORETICALLY BASED MODEL
		lstRes = self.comp (self.objData.H, self.objData.P, 	 self.objData.X, \
						  self.objData.T, self.objData.MEFF, self.objData.QHILO, \
						  self.objData.QCAN, self.objData.V, self.objData.TS1 )
		#
		#	UPDATE THE MASS FLOW TO CORRESPOND TO THE DISPLACEMENT
		
		DISPI = Data.obj_cdata.DISP/1.6387E-05
		OLDMAS = Data.obj_cdata.MREF

		Data.obj_cdata.MREF = (Data.obj_cdata.MREF * (self.objData.DISPLC/DISPI) + 2.0*OLDMAS)/3.0
		if (Data.obj_cdata.MREF >  1.05*OLDMAS): Data.obj_cdata.MREF = 1.04*OLDMAS
		if (Data.obj_cdata.MREF <  0.95*OLDMAS): Data.obj_cdata.MREF = 0.96*OLDMAS
		return lstRes		
		
	def map (self, ICOMP, ICOOL, EER, SIZE, DISPL , SPEEDN):
		return [0.0,0.0]
		
		