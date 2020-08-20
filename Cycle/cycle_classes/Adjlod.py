# Python import
import math, sys, datetime

# User import
from Data import Data
# not required from Block2 import Block2

class Adjlod:
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def adjlod (self, ICYCL,IC,TS3, TS5, FROSTF,FROSTZ,IDFRST):
		# [p3, p4] = self.adjlod (all)
		#	SUBROUTINE ADJLOD(ICYCL,IC,TS3,TS5,FROSTF,FROSTZ,IDFRST)
		#
		# ADJUST THE CABINET LOADS AND SET POINT TEMPERATURES *

		# COMMON BLOCKS
		#
		#	REAL MREF
		#	no 		COMMON / PARMS / ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
		#	no 		COMMON / PARMS2 / TSPEC,TDROPC
		#	COMMON / HTEXS / CFMC,CFME,CFMF,Data.UAF,ETAC,ETAE,ETAF

		#	COMMON / FEVAP / UTPE,USUPE,Data.ATOTE
		#	no		COMMON / CONDEN / UDSC,UTPC,USCC,ATOTC,UACOND
		#	no		COMMON / SPECS / DTSUPE,DTSUBC
		#	no		COMMON  / SPECS2 /  ISPEC,XEXITE,DTSUPI
		#	COMMON / CABLOD / FFASH,FFAUX,FZASH,FZAUX,Data.TROOM,Data.FFTEMP,OTHERW,
		#	 .Data.FZTEMP,Data.FFQ,Data.FZQON,Data.FZQOFF,Data.FFLAT,FZLAT,Data.FFSEN,Data.FZSEN,
		#	 .Data.FFHTQ,Data.FZHTQ,Data.CONDF,Data.CONDZ,Data.QMUL
		#	COMMON / FANS / FANE,FANZ,FANC,DUTYC,W,COPR
		#	COMMON / LORENZ / Data.DUTYE,Data.DUTYZ,PWRL,PWRE,Data.CAPE,Data.CAPZ,DUTYL,DUTYS,
		#	 .FANEL,FANCL,FANES,FANCS
		#	COMMON / FIGURE / IEVAP
		#	COMMON  /  RESULT  /  QE, QZ, FLOW, QEN(2), FLOWN(2), COPRN(2)
		#	COMMON  /  CHINA  /  Data.INCTRL
		#	COMMON  /  BALNCE  /  Data.IBLNCE, BAFFLF, BAFFLZ, Data.AREAFZ, Data.ATOTE_S,
		#	 .Data.AREAFZ_S, Data.ATOTE_A, Data.AREAFZ_A, Data.FFTEMP_A, Data.FZTEMP_A
		#	COMMON  /  INWALL  /  Data.UA_FZ, Data.UA_FF, Data.UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
		#	 .Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
		#	 .CAPZ_IN_WALL, Data.Q_FZ_FF
		#	COMMON  /  PENAT  /  Data.FFPENA, Data.FZPENA
		#
		# BRANCH ON THE VALUE IC.AVE VARIABLES AND INITIALIZE ON THE
		# FIRST CALL AND : WAIT UNTIL THE 4TH CALL TO MAKE ADJUSTMENTS
		#
		IRET = 0 # in Python only
		
		if IC == 1:
			#SELECT CASE (IC)
			#	 CASE [1]#Initialize
			Data.IBLNCE = 0
			IRET = 0

			if(ICYCL  !=  2): IRET = 1
			
			FFTEMP_S = Data.FFTEMP
			FZTEMP_S = Data.FZTEMP
			FFQ_S = Data.FFQ
			FZQON_S = Data.FZQON
			FZQOFF_S = Data.FZQOFF
			FFLAT_S = Data.FFLAT
			FZLAT_S = Data.FZLAT
			FFSEN_S = Data.FFSEN
			FZSEN_S = Data.FZSEN
			FFHTQ_S = Data.FFHTQ
			FZHTQ_S = Data.FZHTQ
			FROSTF_S = FROSTF
			FROSTZ_S = FROSTZ

			CONDF_S = Data.CONDF
			CONDZ_S = Data.CONDZ

			Data.ATOTE_S = Data.ATOTE
			Data.AREAFZ_S = Data.AREAFZ
			UAF_S = Data.UAF
			Data.ATOTE_A = Data.ATOTE
			Data.AREAFZ_A = Data.AREAFZ

			UFF = (Data.FFQ - Data.FFLAT - Data.FFPENA - Data.FFHTQ - FROSTF + Data.QMUL)/(Data.TROOM - Data.FFTEMP)

			FZQ = Data.FZQON
			FZQ_S = FZQ

			UFZ = (FZQ - Data.FZLAT - Data.FZPENA - Data.FZHTQ - FROSTZ - Data.QMUL)/(Data.TROOM - Data.FZTEMP)

			UFF_SEN = FFSEN_S/(Data.TROOM - FFTEMP_S)
			UFZ_SEN = FZSEN_S/(Data.TROOM - FZTEMP_S)

			UCND_F = (Data.CONDF + Data.QMUL)/(Data.TROOM - FFTEMP_S)
			UCND_Z = (Data.CONDZ - Data.QMUL)/(Data.TROOM - FZTEMP_S)

			TS3_S = TS3
			TS5_S = TS5
			Data.FFTEMP_A = Data.FFTEMP
			Data.FZTEMP_A = Data.FZTEMP

			DELTS5_OLD = 0

			UA_FZ_S = Data.UA_FZ
			UA_ML_S = Data.UA_ML
			UA_FF_S = Data.UA_FF

		elif IC in [2,3]:
			pass
			
		else:
			# CASE DEFAULT
			if  (IRET  ==  1): return [TS3, TS5]

			#Determine needed rebalancing of cabinet loads
			FFLOAD = Data.DUTYE * Data.CAPE + Data.DUTYZ * Data.Q_FZ_FF
			FZLOAD = Data.DUTYZ * Data.CAPZ
			DELLOD = (FFLOAD * Data.CAPZ - FZLOAD * Data.CAPE)/(Data.CAPE + Data.CAPZ)

			DUTMAX = max(Data.DUTYE, Data.DUTYZ)
			DUTMIN = min(Data.DUTYE, Data.DUTYZ)
			DUTDIF = DUTMAX - DUTMIN

			Data.IBLNCE = 0
			DUTERR = DUTDIF / DUTMAX

			if (DUTERR  <=  0.001): return [TS3, TS5]
			if (DUTDIF  >=  0.025): Data.IBLNCE = 1

			if Data.INCTRL == 0:
				#SELECT CASE (Data.INCTRL)
				#CASE (0)#No control
				return [TS3, TS5]

			elif Data.INCTRL == 1:
				#CASE [1]#Evap area ratio
				FFNEW = FFLOAD + DELLOD
				FZNEW = FZLOAD - DELLOD
				DAREAF = (FFNEW / FFLOAD - 1.0)*Data.ATOTE
				DAREAZ = (FZNEW / FZLOAD - 1.0)*Data.AREAFZ

				DUTY_AVE = (Data.DUTYE + Data.DUTYZ)/2.0
				DAREAF = (Data.DUTYE / DUTY_AVE - 1.0)*Data.ATOTE
				DAREAZ = (Data.DUTYZ / DUTY_AVE - 1.0)*Data.AREAFZ

				RATIOF = DAREAF / Data.ATOTE

				if (RATIOF  <  -0.5): RATIOF = -0.5

				DAREAF = RATIOF * Data.ATOTE * 0.5
				RATIOZ = DAREAZ / Data.AREAFZ

				if (RATIOZ  <  -0.5): RATIOZ = -0.5

				DAREAZ = RATIOZ * Data.AREAFZ * 0.5

				if (abs(DAREAF)  <  abs(DAREAZ)) :
					Data.ATOTE = Data.ATOTE + DAREAF
					Data.AREAFZ = Data.AREAFZ - DAREAF
				else:
					Data.AREAFZ = Data.AREAFZ + DAREAZ
					Data.ATOTE = Data.ATOTE - DAREAZ
				#END if

				Data.UAF = UAF_S * Data.AREAFZ / Data.AREAFZ_S
				Data.ATOTE_A = Data.ATOTE
				Data.AREAFZ_A = Data.AREAFZ

				Data.UA_FZ = UA_FZ_S * Data.AREAFZ / Data.AREAFZ_S
				Data.UA_ML = UA_ML_S * Data.AREAFZ / Data.AREAFZ_S
				Data.UA_FF = UA_FF_S * Data.ATOTE / Data.ATOTE_S

			elif Data.INCTRL == 2:
				#CASE (2)#FF Cabinet temp
				DUTYN = 0.5*(Data.DUTYE + Data.DUTYZ)
				Data.FFQ = DUTYN * Data.CAPE + DUTYN * Data.Q_FZ_FF + FROSTF_S
				DELTS3 = (Data.FFQ - FFQ_S)/UFF

				TS3 = TS3_S - DELTS3 / 1.8
				Data.FFTEMP_A = 1.8 * TS3 - 459.6

				Data.FFSEN = UFF_SEN*(Data.TROOM - Data.FFTEMP_A)
				Data.CONDF = UCND_F*(Data.TROOM - Data.FFTEMP_A) - Data.QMUL

			elif Data.INCTRL == 3:
				#CASE (3)#Freezer temp
				DUTYN = 0.25 * Data.DUTYE + 0.75 * Data.DUTYZ
				FZQ = DUTYN * Data.CAPZ
				if (IDFRST  ==  0): FZQ = FZQ - FROSTZ_S

				DELTS5 = 0.3*(FZQ - FZQ_S)/UFZ + 0.7 * DELTS5_OLD
				DELTS5_OLD = DELTS5

				FZQ = FZQ_S + UFZ * DELTS5
				Data.FZQON = FZQ
				Data.FZQOFF = FZQ

				TS5 = TS5_S - DELTS5 / 1.8
				Data.FZTEMP_A = 1.8 * TS5 - 459.6

				Data.FZSEN = UFZ_SEN*(Data.TROOM - Data.FZTEMP_A)
				Data.CONDZ = UCND_Z*(Data.TROOM - Data.FZTEMP_A) + Data.QMUL

			elif Data.INCTRL in [4,5]:
				pass
			
		return [TS3, TS5]
