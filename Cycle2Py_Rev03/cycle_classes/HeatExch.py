# Python import
import math, sys, datetime

# User import
from Data import Data
from Block2 import Block2

class HeatExch (Block2):
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def lowevp(self, ICYCL,ICNTRL,H,  P,X,T,  XQ,XL,XV, VL,VV,HL, TS3, TS5, DPF, ETHX2  ):
		# Input    ICYCL,ICNTRL,H,  P,X,T,  XQ,XL,XV, TS5, DPF , ETHX2
		# output   H, P,X,T,  XQ,XL,XV, VL,VV,HL,       HV,TS6 QFREZ
		# [P2 to P12, P13 ,P16, P19] = self.lowevp (P1... P12, P14, P15, P17..P18)
		# P20 is NA
		#	 SUBROUTINE LOWEVP(ICYCL,ICNTRL,H,  P,X,T,  XQ,XL,XV,  VL,VV,HL, HV,TS3,TS5   #15
		#	.     , TS6,DPF,ETHX2,   QFREZ,LQUIT)
		#     *****************************************************************
		#     *    FREEZER EVAPORATOR AND LOWER INTERCHANGER                  *
		#     *****************************************************************
		#
		#	 CHARACTER KEY
		#	 LOGICAL LCRIT,LQUIT
		#	 REAL MREF
		#
		#	DIMENSION H(16),P(16),X[5],XQ(16),XL(5,16),XV(5,16),VL(16)
		#	DIMENSION VV(16),T(16)
		#	COMMON/PARMS/ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
		#	COMMON/EVAPS/ITYPE, FRACT_FF, FRACT_FZ
		#	COMMON /FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
		#	COMMON/HTEXS/CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
		#	COMMON/RDATA4/R
		#	COMMON / INWALL / UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
		#	.                  Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
		#	.                  CAPZ_IN_WALL, Q_FZ_FF
		#	COMMON /CABLOD/ FFASH,FAUXF,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
		#	.                FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
		#	.                FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
		#
		#	COMMON/TLRNCE/TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
		#	.              N_EVAP, N_COND
		#
		#	COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
		#	.                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
		#	.                  CONDF_IN_WALL, CONDZ_IN_WALL

		#	COMMON / PLSTIC / IWALL_FF, IWALL_FZ
		#
		
		NCALL = 0
		SIGMA = 2.0432E-7
		EPS   = 0.8
		#
		#          SET UP PRESSURES AND QUALITIES
		#
		P[10] = P[6]
		P[9] = P[5]
		P[8] = P[9] + DPF
		XQ[10] = 0
		ETHX = ETHX2
		TSAV = T[9]
		
		XL_Temp = [0.0] * len(XL) # in python only
		XV_Temp = [0.0] * len(XV) # in python only
		
		#
		#          FIND BUBBLE AND DEW POINT ENTHALPIES AT FREEZER PRESSURE
		#
		# [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
		[X,XV_Temp , TBUB,VL[8],VV[8], LCRIT] = self.bublp ( P[8],X,  XV_Temp, True) #  CALL BUBLP(P[8],X,XV(1,8),TBUB,VL[8],VV[8],True,LCRIT)
		self.setArr2dCol (XV, 8, XV_Temp)

		#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
		[HBUB,CV,CP,VS] = self.hcvcps (1,TBUB,VL[8],X) # CALL HCVCPS(1,TBUB,VL[8],X,HBUB,CV,CP,VS)
		#
		
		if(NCALL  ==  0) :
			# [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
			[XL_Temp ,X, TDEW,VL[9],VV[9], LCRIT] = self.bublp ( P[9], XL_Temp ,X, False) # CALL BUBLP(P[9],XL(1,9),X,TDEW,VL[9],VV[9],.FALSE.,LCRIT)
			self.setArr2dCol (XL, 9, XL_Temp)
			
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[HDEW,CV,CP,VS] = self.hcvcps (1,TDEW,VV[9],X) #  CALL HCVCPS(1,TDEW,VV[9],X,   HDEW,CV,CP,VS)

			Data.CREF = Data.MREF * (HDEW-HBUB)/(TDEW-TBUB+0.001)
			if(Data.CREF  <=  0.1) :
				Data.CREF = 1000000.0  ####   5/9/94
			#END if

			T[10] = TS5
			NCALL = 1
		#END if
		#
		#          SET FLAG FOR POSSIBLE SOLUTION AND HANDLE SINGLE EVAP CASE
		#
		IFREZ2 = 1
		if(TBUB  >=  TS5): IFREZ2 = 0
		
		VL[10] = VL[6]
		if(IFREZ2  ==  0) :
			H[9] = H[6]
			H[10] = H[6]
			T[10] = T[6]
		#END if

		if(Data.ITYPE  ==  1):  ETHX = 0
		
		#
		#          BEGIN ITERATION FOR TEMPERATURE AT POINT 10
		#
		
		ITER = 1
		#10 CONTINUE
		while ( True):
			ITER = ITER + 1
			
			# wait a key to exit app
			#CALL INCHR(0,J,KEY)
			#if(J  ==  1): sys.exit(100)#  CALL FINISH
			#if(J  ==  68): LQUIT = True

			#TSHOW = T[10] - 273.11
			if(ICYCL  ==  2) :
				#CALL GOTOXY(2,21)
				#CALL PRINT(TSHOW,5,1)
				self.showMsg ("LIQUID LINE OUTLET FROM LOW TEMP INTERCHANGER - point 10 ", T[10] - 273.11 )
			#END if
			
			#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
			[H[10],CV,CP,VS] = self.hcvcps (1,T[10],VL[10],X) # CALL HCVCPS(1,T[10],VL[10],X,H[10],CV,CP,VS)
			
			#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
			[T10,XQ[10],XL_Temp, XV_Temp, VL[10],VV[10],HL,HV] = self.hpin (H[10],P[10],X )
			self.setArr2dCol (XL, 10, XL_Temp)
			self.setArr2dCol (XV, 10, XV_Temp)
			
			#CALL HPIN(H[10],P[10],X,  T10,XQ[10],XL(1,10),XV(1,10), VL[10],VV[10],HL,HV)

			H[8] = H[10]
			
			[T[8],XQ[8], XL_Temp, XV_Temp, VL[8],VV[8],HL,HV] = self.hpin ( H[8],P[8],X )
			self.setArr2dCol (XL, 8, XL_Temp)
			self.setArr2dCol (XV, 8, XV_Temp)
			
			#CALL HPIN(H[8],P[8],X,  T[8],XQ[8],XL(1,8),XV(1,8),     VL[8],VV[8],HL,HV)
			
			
			#TSHOW = T[8] - 273.11
			#if(ICYCL  ==  2) :
			#	CALL GOTOXY(14,21)
			#else:
			#	CALL GOTOXY(14,15)
			#END if

			#CALL PRINT(TSHOW,5,1)
			self.showMsg ("INLET TO FREEZER EVAPORATOR - point 8",  T[8] - 273.11 )
			
			#
			#          DETERMINE CMIN AND CMAX
			#
			if(Data.CFMF  <=  Data.CREF) :
				CMIN = Data.CFMF
				CMAX = Data.CREF
			else:
				CMIN = Data.CREF
				CMAX = Data.CFMF
			#END if

			CAPRAT = CMIN/CMAX
			if(CMIN  <=  0.0): CMIN=0.001

			FNTU = Data.UAF/CMIN
			if(FNTU  <  0.0): FNTU = 0.0

			#
			#          CALCULATE EFFECTIVENESS
			#
			Data.UAFZ = Data.UAF
			if(IFREZ2  ==  1) :
				if Data.IFREZ ==0:
					#SELECT CASE (IFREZ)
					#CASE (0)
					TAVE = (T[8] + T[9])/2.0

					if(T[9]  <  -1000.0): TAVE = T[8]   ### Jan 20, 1993
					if(TAVE  >  TS5): TAVE = TS5 - 1.0

					QMAX = 0.90*Data.MREF*(H(7)- H[6])       ## 5/9/94
					HRAD = SIGMA*(TAVE + TS5)*(TAVE**2 + TS5**2)*EPS
					DELTAT = TS5 - TAVE

					if(DELTAT  <=  0.0): DELTAT = 0.0001

					DELTA = DELTAT*1.8
					TBAR = 0.67*TAVE + 0.33*TS5
					A_NAT = 0.239 + 3.34E-04*(273.0 - TBAR)
					HNAT = A_NAT*(DELTA**0.33)*20.44
					#
					#          MAKE APPROXIMATE CORRECTIONS FOR VIEW FACTORS AND ASSUMED
					#          ORIENTATION OF THE EVAPORATOR PANELS.
					#
					#              HRAD = (1.0 - FRACT_FZ)*HRAD
					#              HNAT = 0.5*HNAT
					UAIR = HRAD + HNAT
					if(Data.IWALL_FZ  ==  1) :
						UAIR = 1.0/(1.0/UAIR + 0.1389/20.44)
					#END if

					QFREZ = Data.UAF*UAIR*DELTAT
					Data.UAFZ = Data.UAF * UAIR
					
					TENV = (TROOM + 459.6)/1.8
					
					QFREZ = QFREZ + 1.8*UA_FZ*(TENV - TAVE)*1.0548 \
						+ 1.8*UA_ML*(TS3  - TAVE)*1.0548 + Q_HXS_FZ

					if(QFREZ  >  QMAX): QFREZ = QMAX

				elif Data.IFREZ ==1:
					#CASE (1)
					EXFR = self.efcross (CAPRAT,FNTU) # CALL EFCROSS(CAPRAT,FNTU,EXFR)

					QFREZ = EXFR*CMIN*(TS5 - T[8])
					ETAF = EXFR

				elif Data.IFREZ ==2:
					#CASE (2)
					XX = 1.0 - CAPRAT
					XXX = EXP(-FNTU*XX)
					EXFR = (1.0-XXX)/(1.0-CAPRAT*XXX)
					QFREZ = EXFR*CMIN*(TS5 - T[8])
					ETAF = EXFR
				#END SELECT
				TS6 = TS5 - QFREZ/Data.CFMF

				if(IFREZ  ==  0): TS6 = 0.9*TAVE + 0.1*TS5
			else:
				QFREZ = 0.0
				TS6 = TS5
			#END if

			#
			#          UPDATE ENTHALPY ACROSS EVAPORATOR
			#
			H[9] = H[8] + QFREZ/Data.MREF

			#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
			[T[9], XQ[9], XL_Temp, XV_Temp, VL[9],VV[9],HL,HV] = self.hpin ( H[9],P[9],X )
			#CALL HPIN(H[9],P[9],X,  T[9],XQ[9],XL(1,9),XV(1,9), VL[9],VV[9],HL,HV)
			self.setArr2dCol (XL, 9, XL_Temp)
			self.setArr2dCol (XV, 9, XV_Temp)

			#TSHOW = T[9] - 273.11
			if(ICYCL  ==  2) :
				#CALL GOTOXY(52,21)
				#CALL PRINT(TSHOW,5,1)
				self.showMsg ("OUTLET FROM FREEZER EVAPORATOR - point 9", TSHOW )
			#END if

			if(IFREZ2  ==  0) : break #GO TO 20
			#
			#          GET NEW GUESS FOR TEMPERATURE AT POINT 10
			#
			TOLD = T[10]
			if(ICYCL  !=  2 .OR. ICNTRL  !=  2) :
				TNEW  = T[6] - ETHX*(T[6] - T[9])
			else:
				# [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
				[XLD,X,TD,VLD,VVD, LCRIT] = self.bublp ( P[9],XLD,X, False) #  CALL BUBLP(P[9],XLD,X,TD,VLD,VVD,.FALSE.,LCRIT)

				if(TD  >  T[6]) :
					TNEW  = T[6] - ETHX*(T[6] - T[9])
				else:
					#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
					[HHIGH,CV,CP,VS] = self.hcvcps (1,T[9],VL[10],X) # CALL HCVCPS(1,T[9],VL[10],X,HHIGH,CV,CP,VS)

					# [P4, P5] = self.espar [P1, P2, P3]
					[A1,B1] = self.espar [0,T[6],X] # CALL ESPAR(0,T[6],X,A1,B1)

					VGUESS = R*T[6]/P[9]

					#[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
					[VGUESS, LCRIT] = self.vit (T[6],P[9],A1,V1,VGUESS, False) # CALL VIT(T[6],P[9],A1,V1,VGUESS,.FALSE.,LCRIT)

					#[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
					[HLOW,CV,CP,VS] = self.hcvcps (1,T[6],VGUESS,X) # CALL HCVCPS(1,T[6],VGUESS,X,HLOW,CV,CP,VS)

					DH = min((HLOW - H[9]),(H[6]-HHIGH))
					H[10] = H[6] - DH

					#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
					[ T[10],XQ[10], XL_Temp, XV_Temp,VL[10],VV[10],HL,HV] = self.hpin ( H[10],P[10],X )
					self.setArr2dCol (XL, 10, XL_Temp)
					self.setArr2dCol (XV, 10, XV_Temp)

					#CALL HPIN(H[10],P[10],X, T[10],XQ[10],XL(1,10), XV(1,10),VL[10],VV[10],HL,HV)
				#END if
			#END if
			#
			#          CORRECT GUESS if NECESSARY AND CALCULATE ERROR
			#
			if(T[9]  >  T[6]) :
				TNEW = T[10] - 0.9*ERROR
				T[10] = TNEW
			#END if

			T[10] = TNEW
			ERROR = TSAV - T[9]
			TSAV = T[9]

			if(abs(ERROR)  <  TOL_FRZ): break #GO TO 20

			Data.CREF = Data.MREF*abs((H[9] - H[8])/(T[9]-T[8]+0.0001))
			if(Data.CREF  <=  0.1) :
				Data.CREF = 1000000.0  ####   /5/9/94
			#END if
			ITER = ITER + 1

			if (abs(TOLD - TNEW)  >  2.0) :            ## 5/9/94
				if (TOLD  >  TNEW): TNEW = TOLD - 2.0      ## 5/9/94
				if (TOLD  <  TNEW): TNEW = TOLD + 2.0      ## 5/9/94
			#END if

			if(ITER  >  2) : T[10] = 0.5*(TOLD + TNEW)      ## 5/9/94
			if(ITER  >  10): break #GO TO 20
		#GO TO 10

		#
		#          END OF ITERATION.  CALCULATE NEEDED PARAMETERS
		#
		# 20 CONTINUE

		H[5] = H[9] + (H[6] - H[10])
		#[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )

		[T[5], XQ[5], XL_Temp, XV_Temp, VL[5], VV[5], HL,HV] = self.hpin ( H[5], P[5], X )
		self.setArr2dCol (XL, 5, XL_Temp)
		self.setArr2dCol (XV, 5, XV_Temp)
					
		[T[10],XQ[10],XL_Temp, XV_Temp, VL[10],VV[10],HL,HV] = self.hpin ( H[10],P[10],X )
		self.setArr2dCol (XL, 10, XL_Temp)
		self.setArr2dCol (XV, 10, XV_Temp)
		
		#CALL HPIN(H[5],P[5],X, T[5],XQ[5],XL(1,5),XV(1,5),VL[5],VV[5],   HL,HV)
		#CALL HPIN(H[10],P[10],X, T[10],XQ[10],XL(1,10),XV(1,10),VL[10],  VV[10],HL,HV)

		#TSHOW = T[5] - 273.11
		#if(ICYCL  ==  2) :
		#	CALL GOTOXY(52,11)
		#	CALL PRINT(TSHOW,5,1)
		#END if
		self.showMsg ("INLET TO FRESH FOOD EVAPORATOR  - point 5 ", T[5] - 273.11 )
		
		return [ H,  P,X,T,  XQ,XL,XV, VL,VV,HL,  HV,TS6, QFREZ]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def exf (self, LOC, AREA, U, CMIN, CMAX):
		# [P6, P7] = self.exf (P1 ... P5)
		#	  SUBROUTINE EXF(LOC, AREA, U, CMIN, CMAX, EFF, DEFFDA)
		#     ******************************************************************
		#     *    CALCULATE COUNTER FLOW EFFICIENCY PARAMETERS                *
		#     ******************************************************************
		#
		#	  REAL NTU

		#	  DIMENSION coff_A(4,6), EFF_CROSS(2)
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

		if LOC == 1 :		   #Counter-flow
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

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def efcross (self,CRAT,NTU):
		# P3 = self.efcross (P1, P2)
		#	  SUBROUTINE EFCROSS(CRAT,NTU,EFFECT)
		#     ******************************************************************
		#     *     CALCULATES THE HEAT TRANSFER EFFECTIVENESS FOR A CROSS     *
		#     *     FLOW HEAT EXCHANGER WITH BOTH FLUIDS UNMIXED               *
		#     ******************************************************************

		#DIMENSION A(4,6)

		A = [	[0.0, 0.0,           0.0,      0.0,      0.0,      0.0,  0.0],\
				[0.0, 2.394292, -1.19402, -1.45067, 1.938453, -0.81305,  0.118651],	\
				[0.0, 2.410798, -2.23391, 0.8259,   0.051006, -0.11891,  0.02336],	\
				[0.0, 2.399687, -2.96882, 2.36708,  -1.23009, 0.373338, -0.04886],	\
				[0.0, 2.359642, -3.3765,  3.04862,  -1.63421, 0.468741, -0.05492],	\
			]

		#  DATA (A(I,1),I=1,4)/2.394292,2.410798,2.399687,2.359642/
		#  DATA (A(I,2),I=1,4)/-1.19402,-2.23391,-2.96882,-3.37650/
		#  DATA (A(I,3),I=1,4)/-1.45067,0.825900,2.367080,3.04862/

		#  DATA (A(I,4),I=1,4)/1.938453,0.051006,-1.23009,-1.63421/
		#  DATA (A(I,5),I=1,4)/-0.81305,-0.11891,0.373338,0.468741/
		#  DATA (A(I,6),I=1,4)/0.118651,0.023360,-0.04886,-0.05492/
		#
		#          FIND POSITION IN ARRAY BASED ON THE CAPACITY RATIO
		#          OF THE TWO STREAMS
		#
		if(CRAT >= 0.00 and CRAT <=  0.25) : I=1
		if(CRAT >  0.25 and CRAT <=  0.50) :I=2
		if(CRAT >  0.50 and CRAT <=  0.75) :I=3
		if(CRAT >  0.75 and CRAT <=  1.00) :I=4
		if(NTU <=  0.0): NTU = 0.0

		BETA = LOG10(NTU+1.0)
		EFFA = 0.0
		EFFB = 0.0
		#J = 1
		while J in range (1, 6 + 1): #DO WHILE (J  <=  6)
			EX = 1.0*J
			if (I == 1) :
				EFFA = 1.0 - EXP(-NTU)
			else:
				EFFA = EFFA + A[I-1][J]  * BETA**EX
			#END if
			EFFB = EFFB + A[I][J] * BETA**EX
			#J = J + 1
		#END DO

		FRAC = (CRAT-(I-1)*0.25)/(I*0.25-(I-1)*0.25)
		EFFECT = EFFA + FRAC*(EFFB-EFFA)
		if (EFFECT > 1.0): EFFECT = 1.0
		return

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
		Data.FF_AIR = X
		return [TIN,X]

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
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

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
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
