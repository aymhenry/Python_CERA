# Python Import
import math, sys, datetime

# User Import
from Data import Data
from FileAccess import FileAccess
from Cycle import Cycle

class Eravyv ( Cycle):
	FILE_ERROR_OUT = 'ERROR.OUT'
	FILE_CYCLE_TOL = "CYCLE.TOL"
	FILE_CYCLE_DAT = 'CYCLE.DAT'
	FILE_COMPMAP_DAT  = 'COMPMAP.DAT'
	FILE_COMPMAP_NAM  = 'COMPMAP.NAM'
	FILE_ERA_DAT  = 'ERA.OUT'
		
	#      PROGRAM ERACYC
	#        *****************************************************************
	#        *     CYCLE PROGRAM FOR ERA MODEL.  DEVELOPED BY Data.A. D. LITTLE     *
	#        *****************************************************************
	#
	#CHARACTER       key, TITLE
	#CHARACTER * 13    FILE_ERROR_OUT, filera
	#CHARACTER * 13    filmap, filmap1, filmap2
	#CHARACTER * 64    DOSCHR

	#REAL            meff, mref, mrefi
	#LOGICAL         exists, found_data

	#DIMENSION       TS1[2], ICONDI[2], CFMCI[2], UDSCI[2], UTPCI[2],
	#.                USCCI[2], FNPWRC[2], ATOTCI[2], DTSBCI[2],
	#.                IFRSHI[2], TS3[2], CFMEI[2], DPE[2], UTPEI[2],
	#.                USUPEI[2], ATOTEI[2], DTSPEI[2], IFREZI[2],
	#.                FNPWRE[2], MREFI[2], DISPLC[2], SPEEDI[2], CEI[2],
	#.                SEFFI[2], MEFF[2], QCAN[2], QHILO[2],
	#.                TSPECI[2], ELOSS[2], ETHX[2], DPC[2]
	#DIMENSION       DUTYN[2], WCOMP[2], QEL[2]
	#DIMENSION       ISPECI[2], QUALTY[2], SUPIHX[2]
	#DIMENSION       TITLE(68,5)
	#DIMENSION       ICOOLN[2], SIZEN[2], EERN[2], SPDNOM[2]
	#
	#            COMMON BLOCKS
	#
	#COMMON / PARMS / ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
	#COMMON / PARMS2 / TSPEC,I_LIQUID_LINE
	#COMMON / HTEXS / CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
	#COMMON / FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
	#COMMON / CONDEN / UDSC,UTPC,USCC,ATOTC,UACOND
	#COMMON / SPECS / DTSUPE,DTSUBC
	#COMMON  / SPECS2 /  ISPEC,XEXITE,DTSUPI
	#COMMON / EVAPS / ITYPE, FRACT_FF, FRACT_FZ
	#COMMON / CABLOD / FFASH,FFAUX,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
	#.              FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
	#.              FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
	#COMMON / FANS / FANE,FANZ,FANC,DUTYC,W,COPR
	#COMMON / LORENZ / DUTYE,DUTYZ,PWRL,PWRE,CAPE,CAPZ,DUTYL,DUTYS,

	#COMMON / CYCLNG / CORR_COP, COPCYC[2], I_CYCLE, I_VALVE, T_CYCLE
	
	#COMMON  / REFRIG /  Data.pythNC[2], IR(5,2), Data.X(5,2), loc_F[5,5,2)
	#COMMON / FILINF / FILERA
	#COMMON / CHINA / INCTRL, HRSOFF
	#COMMON / BALNCE / IBLNCE, BAFFLF, BAFFLZ, AREAFZ, ATOTE_S,
	#.  AREAFZ_S, ATOTE_A, AREAFZ_A, FFTEMP_A, FZTEMP_A
	#COMMON / INWALL / UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
	#.  Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
	#.  CAPZ_IN_WALL, Q_FZ_FF
	#COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
	#.  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
	#.  CONDF_IN_WALL, CONDZ_IN_WALL
	#COMMON / LIQLIN / FFREFQ, FZREFQ, CONDHT[2], CONDVP[2]
	#COMMON / CYCLIC / DFSTCYC, FFCYC, FZCYC, OUTCYC

	#COMMON / TLRNCE / TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
	#.              N_EVAP, N_COND
	#COMMON / MAPDAT / IMAP, ICOMP, ICOOL, EER, SIZE, DISPL, EFFC,
	#.  SPEEDN, IREAD
	#COMMON / PLSTIC / IWALL_FF, IWALL_FZ
	#COMMON / MAPNAM / filmap, filmap1, filmap2
	#COMMON / PENAT / FFPENA, FZPENA
	#
	#            DATA STATEMENTS

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def start (self ):
		Data.setup()
		QZ_NET = 0.0
		
		# Input 		: File Handle to read from ( ONLY in Python )
		#				: String 'int', 'string', 'string' according to required type.
		# Output		: one line from the input file,
		#				 if type is 'int or float", and given string, App terminate
		# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
		def inastk (h_FileHanndle, strType="" ):
			# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
			# * THIS ROUTINE READS IN ONE LINE OF INPUT DATA AND SCREENS OUT *
			# * ALL LINES THAT BEGIN WITH AN ASTERICK *
			# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
			DATA_TYPES = {'int':'i', 'float':'f', 'string:':'s'} #Input types
			if not ( strType in DATA_TYPES ): # fixed if bad required type
				strType = "string" # set it as string

			while True:
				is_can_read = h_FileHanndle.readline( )	# if you can read, gives true
				if is_can_read:
					str_text = h_FileHanndle.getText( )
					if str_text == "":
						h_FileHanndle.terminate( 'EAR App Error: End of file for input file' )

					elif str_text [0] == "*":
						continue

					else:
						break
				else: # error in reading file stop app.
					h_FileHanndle.terminate ( "EAR App Error: Can't read input file" )

			if strType == "string":
				return str_text # String return the value

			# else number is required
			str_text = str_text.strip( )	# remove space

			# check that no more than one(-) & ".",
			n_count_muns = str_text.count("-")
			n_count_dec = str_text.count(".")

			b_is_int = str_text.replace( '.','',1 ).replace( '-','',1 ).isdigit( )

			if not b_is_int or n_count_muns > 1 or n_count_dec > 1:
				h_FileHanndle.terminate ( "EAR App Error: Data Error, Required Number, given Text ->" + str_text + "<-" )
				return 0

			# (-) must be on left.
			str_text = str_text.replace('-','',1)

			flt_value = float(str_text)
			if  n_count_muns ==1 :
				flt_value = -1.0 * flt_value

			if strType == "float":
				return flt_value

			int_value = int( flt_value )

			return int_value # int return

		TS5 = 256.0
		IN = 5
		
		TS1 = [0.0] * (2+1)
		ICONDI= [0.0] * (2+1)
		CFMCI= [0.0] * (2+1)
		UDSCI= [0.0] * (2+1)
		UTPCI= [0.0] * (2+1)
		USCCI= [0.0] * (2+1)
		FNPWRC= [0.0] * (2+1)
		ATOTCI= [0.0] * (2+1)
		DTSBCI= [0.0] * (2+1)
		IFRSHI= [0.0] * (2+1)
		TS3= [0.0] * (2+1)
		CFMEI= [0.0] * (2+1)
		DPE= [0.0] * (2+1)
		UTPEI= [0.0] * (2+1)
		USUPEI= [0.0] * (2+1)
		ATOTEI= [0.0] * (2+1)
		DTSPEI= [0.0] * (2+1)
		IFREZI= [0.0] * (2+1)
		FNPWRE= [0.0] * (2+1)
		MREFI= [0.0] * (2+1)
		DISPLC= [0.0] * (2+1)
		SPEEDI= [0.0] * (2+1)
		CEI= [0.0] * (2+1)
		SEFFI= [0.0] * (2+1)
		MEFF= [0.0] * (2+1)
		QCAN= [0.0] * (2+1)
		QHILO= [0.0] * (2+1)
		TSPECI= [0.0] * (2+1)
		ELOSS= [0.0] * (2+1)
		ETHX= [0.0] * (2+1)
		DPC= [0.0] * (2+1)
		
		DUTYN= [0.0] * (2+1)
		WCOMP= [0.0] * (2+1)
		QEL= [0.0] * (2+1)

		ISPECI= [0.0] * (2+1)
		QUALTY= [0.0] * (2+1)
		SUPIHX= [0.0] * (2+1)
		
		ICOOLN= [0.0] * (2+1)
		SIZEN= [0.0] * (2+1)
		EERN= [0.0] * (2+1)
		SPDNOM= [0.0] * (2+1)
		
		# F(5,5,2)
		loc_F =  [  [[0.0] * (2+1) for i in range(5+1)] for j in range(5+1)  ]
		
		#loc_NC = [0.0] * (2+1)
		#  X(5,2)  
		Data.X  =  [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		#  IR(5,2)  
		Data.IR  =  [[0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
			
		# setup DOS screen, not required in Python CALL SETPIC

		#
		#            INITIALIZE ERROR CODE FOR LIQUID LINE ANTI-SWEAT HEAT
		#
		I_LIQUID_LINE = 0

		#
		#            READ IN SOLUTION TOLERANCE DATA (IF FILE EXISTS)
		#
		objCycle = FileAccess (Eravyv.FILE_CYCLE_TOL) # open file for read
		
		if not objCycle.isError():	# if file found
			#print ("file Found")
			Data.TOL_FRSH = inastk ( objCycle, "float" ) # ask for float
			Data.TOL_FRZ  = inastk ( objCycle, "float" ) # ask for float
			Data.TOL_COND = inastk ( objCycle, "float" ) # ask for float

			Data.TOL_MASS = inastk ( objCycle, "float" ) # ask for float
			Data.TOL_HX   = inastk ( objCycle, "float" ) # ask for float

			Data.N_EVAP   = inastk ( objCycle, "int" ) # ask for int
			Data.N_COND   = inastk ( objCycle, "int" ) # ask for int
		else:
			#print ("file NOT Found " + Eravyv.FILE_CYCLE_TOL)
			Data.TOL_FRSH = 0.1
			Data.TOL_FRZ = 0.1
			Data.TOL_COND = 0.075
			Data.TOL_MASS = 0.01
			Data.TOL_HX = 0.001
			
			Data.N_EVAP = 1
			Data.N_COND = 1

		del(objCycle) # close

		#
		#            CLEAR KEYBOARD AND OPEN THE DATA FILE Eravyv.FILE_CYCLE_DAT
		#
		#	CALL INCHR(3,J,KEY)
		objCycleData = FileAccess ( Eravyv.FILE_CYCLE_DAT ) # open file for read
		if objCycleData.isError():	# if file not found
			self.showMsg('Execution Terminated.')
			self.showMsg( objCycleData.err_description() )
			del (objCycleData) # = "" # close habdler
			sys.exit('5000')

		#
		#            READ HEADER DATA AND SAVE
		dummy =  inastk ( objCycleData ) # row_no=1 read heading row, has no use
		TITLE =  inastk ( objCycleData ) # row_no=2
		
		#
		#            READ FILE INFORMATION
		TITLE2  = inastk ( objCycleData ) # row_no=3
		Data.FILERA  = inastk ( objCycleData ) + " " + str(Data.FILERA ) # row_no=4
		#
		#            CABINET AND CYCLE DEFINITIONS
		# Python comment
		#IRFTYP as configration in Cab app.
			#1- two-door top-mount refrigerator/freezer
			#2- two-door bottom-mount befrigerator/freezer
			#3- side-by-side refrigerator/freezer
			#4- chest freezer
			#5- upright freezer
			#6- one-door refrigerator
			#7-one-door refrigerator/freezer
		IRFTYP = inastk ( objCycleData, "int" ) # ask for int # row_no=5
		
		# Python comment: ICYCL - cycle type (1=standard, 2=lorenz, 3=dual loop, 4=dual evap)
		ICYCL  = inastk ( objCycleData, "int" ) # ask for int # row_no=6

		#
		#            HANDLE DUAL EVAP CYCLE
		ICYCLS = ICYCL	# Python comment: save original ICYCL value
		
		#Python comment: if 4- chest freezer, change it to 2- two-door bottom-mount befrigerator/freezer
		if(ICYCL == 4): ICYCL = 2

		#
		#            READ IN MANUAL DEFROST CONTROL
		#
		IDFRST = inastk ( objCycleData, "int" ) # Manual Defrost No=0, Yes=1 # row_no=7
		Data.HRSOFF = inastk ( objCycleData, "int" ) #  Hours Shutdown For Cycle Defrost  # row_no=8

		#
		#            READ IN CONTROL OPTION if LORENZ OR DUAL EVAPORATOR CYCLE
		
		# Python comment
		# INCTRL: 	0 = none, 
		#			1 = adjust evaporator areas,
		#			2 = adjust fresh food section tempeature,
		#			3 = adjust freezer    section tempeature,
		# 			4 = switching valve (only one section is cooled  at a time)
		#			5 = solenoid valve or fan control provides evaporator capacity to only one cabinet
		#				during part of the cycle
		if(ICYCL == 2):
			Data.INCTRL = inastk ( objCycle, "int" ) # ask for int
			if(Data.INCTRL == 5): IDFRST = 1
			
		#
		#            READ IN COMPRESSOR OPTIONS
		Data.ICOMP   = inastk ( objCycleData, "int" ) # row_no=10 -Python comment type of compressor 1=reciprocating, 2=rotary
		Data.IMAP    = inastk ( objCycleData, "int" ) # row_no=11
		Data.I_CYCLE = inastk ( objCycleData, "int" ) # row_no=12
		Data.T_CYCLE = inastk ( objCycleData, "float" ) # row_no=13 Python comment:number of cycles per hour
		Data.I_VALVE = inastk ( objCycleData, "int" ) # row_no=14 Python comment: 1 if valve used, 0 if not
		
		#
		#            PROCESS COMPRESSOR MAP FILE DATA
		IFLAG = 0
		if(Data.IMAP == 0):
			objCompMap = FileAccess (Eracyc.FILE_COMPMAP_NAM) # open file for read

			if objCompMap.isError():	# if file not found
				IFLAG = 1
			else:
				Data.FILMAP1 = inastk ( objCompMap )

				if objCompMap.isError(): IFLAG = 1 # if(IOCHECK != 0.0) IFLAG = 1

				if(ICYCL == 3)  :
					Data.FILMAP2 = inastk ( objCompMap ) 	# READ(1, '(A13)', IOSTAT = IOCHECK) filmap2
					if objCompMap.isError(): IFLAG = 1	# if(IOCHECK != 0.0) IFLAG = 1
				del(objCompMap) # close

		if(IFLAG == 1):
			self.showMsg('Compressor Map Files Not Defined.')
			self.showMsg('Execution Terminated.')
			del (objCycleData) # close habdler
			sys.exit('36')

		#
		#            SET UP Data.A READ DATA LOOP WHICH DEPENDS ON THE CYCLE TYPE
		#
		#IOCHECK  = inastk ( objCycleData ) # ALL READLN(1,IN,9,IOCHECK)

		if(ICYCL == 3)  :
			NDATA = 2
		else:
			NDATA = 1
		
		for N in range (1, NDATA +1 ):	#DO WHILE (N <= NDATA) 
			#
			# REFRIGERANT DATA
			#
			#Python comment: IR - array of code numbers for the components of the mixture
			Data.IR[1][N] = inastk ( objCycleData, "int" ) # ask for int
			Data.IR[2][N] = inastk ( objCycleData, "int" ) # ask for int
			Data.IR[3][N] = inastk ( objCycleData, "int" ) # ask for int

			Data.pythNC[N] = inastk ( objCycleData, "int" ) # ask for int

			loc_F[1][2][N] = inastk ( objCycleData, "float" ) # ask for float
			loc_F[1][3][N] = inastk ( objCycleData, "float" ) # ask for float
			loc_F[2][3][N] = inastk ( objCycleData, "float" ) # ask for float

			loc_F[2][1][N] = loc_F[1][2][N]
			loc_F[3][1][N] = loc_F[1][3][N]
			loc_F[3][2][N] = loc_F[2][3][N]

			Data.X[1][N] = inastk ( objCycleData, "float" ) # ask for float
			Data.X[2][N] = inastk ( objCycleData, "float" ) # ask for float
			Data.X[3][N] = inastk ( objCycleData, "float" ) # ask for float

			#
			# CONDENSER DATA
			#
			ICONDI[N] = inastk ( objCycleData, "int" ) # ask for int
			
			TS1[N]   = inastk ( objCycleData, "float" ) # ask for float
			CFMCI[N] = inastk ( objCycleData, "float" ) # ask for float
			FNPWRC[N]= inastk ( objCycleData, "float" ) # ask for float
			
			DPC[N]   = inastk ( objCycleData, "float" ) # ask for float
			UDSCI[N] = inastk ( objCycleData, "float" ) # ask for float
			UTPCI[N] = inastk ( objCycleData, "float" ) # ask for float
			
			USCCI[N] = inastk ( objCycleData, "float" ) # ask for float
			ATOTCI[N]= inastk ( objCycleData, "float" ) # ask for float
			DTSBCI[N]= inastk ( objCycleData, "float" ) # ask for float

			Data.CONDHT[N] = inastk ( objCycleData, "float" ) # fixed not the same FORTAN
			Data.CONDVP[N] = inastk ( objCycleData, "float" ) # fixed not the same FORTAN

			Data.CONDHT[N] = 3.6 * Data.CONDHT[N] # fixed not the same FORTAN
			Data.CONDVP[N] = 3.6 * Data.CONDVP[N] # fixed not the same FORTAN
			#
			# FRESH FOOD SECTION DATA
			#
			#IOCHECK  = inastk ( objCycleData ) # READLN(1,IN,1,IOCHECK)

			ISPECI[N] = inastk ( objCycleData, "int" ) # ask for int
			#IOCHECK  = inastk ( objCycleData ) # READLN(1,IN,1,IOCHECK)

			IFRSHI[N] = inastk ( objCycleData, "int" ) # ask for int
			#IOCHECK  = inastk ( objCycleData ) # READLN(1,IN,1,IOCHECK)

			TS3[N]   = inastk ( objCycleData, "float" ) # ask for float
			CFMEI[N] = inastk ( objCycleData, "float" ) # ask for float
			FNPWRE[N]= inastk ( objCycleData, "float" ) # ask for float
			DPE[N]   = inastk ( objCycleData, "float" ) # ask for float
			UTPEI[N] = inastk ( objCycleData, "float" ) # ask for float
			USUPEI[N]= inastk ( objCycleData, "float" ) # ask for float
			ATOTEI[N]= inastk ( objCycleData, "float" ) # ask for float
			DTSPEI[N]= inastk ( objCycleData, "float" ) # ask for float

			QUALTY[N] = DTSPEI[N]
			#
			# FREEZER SECTION DATA
			#
			if(ICYCL == 2)  :
				IFREZI[N] = inastk ( objCycleData, "int" ) # ask for int
				TS5   = inastk ( objCycleData, "float" ) # ask for float
				Data.CFMF  = inastk ( objCycleData, "float" ) # ask for float
				Data.FANZ  = inastk ( objCycleData, "float" ) # ask for float
				Data.AREAFZ= inastk ( objCycleData, "float" ) # ask for float
				Data.UAF   = inastk ( objCycleData, "float" ) # ask for float
				DPF   = inastk ( objCycleData, "float" ) # ask for float
				
			#
			# COMPRESSOR DATA
			#
			#IOCHECK  = inastk ( objCycleData ) # READLN(1,IN,1,IOCHECK)

			if(Data.IMAP == 0)  :
				if (ICYCL == 2  and  Data.INCTRL  >= 4)  : # !!9-27-94
					MREFI[1] = inastk ( objCycleData, "float" ) # ask for float
					MREFI[2] = inastk ( objCycleData, "float" ) # ask for float

				else:
					MREFI[N] = inastk ( objCycleData, "float" ) # ask for float

				SPEEDI[N] = inastk ( objCycleData, "float" ) # ask for float
				TSPECI[N] = inastk ( objCycleData, "float" ) # ask for float
				
				dum = inastk ( objCycleData) # read line 1 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 2 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 3 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 4 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 5 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 6 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 7 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 8 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 9 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 10 not used for IMAP=0
				dum = inastk ( objCycleData) # read line 11 not used for IMAP=0

			if(Data.IMAP == 1)  :
				if (ICYCL == 2  and  Data.INCTRL  >=  4) : 	#!!!9-27-94
					MREFI[1] = inastk ( objCycleData, "float" )
					MREFI[2] = inastk ( objCycleData, "float" )
				else:
					MREFI[N] = inastk ( objCycleData, "float" )
				
				SPEEDI[N] = inastk ( objCycleData, "float" )
				TSPECI[N] = inastk ( objCycleData, "float" )
				
				DISPLC[N] = inastk ( objCycleData, "float" )
				
				SIZEN[N]  = inastk ( objCycleData, "float" )
				SPDNOM[N] = inastk ( objCycleData, "float" )
				EERN[N]   = inastk ( objCycleData, "float" )
				ICOOLN[N] = inastk ( objCycleData, "float" )
				
				dum = inastk ( objCycleData) # read line 1 not used for IMAP=1
				dum = inastk ( objCycleData) # read line 2 not used for IMAP=1
				dum = inastk ( objCycleData) # read line 3 not used for IMAP=1
				dum = inastk ( objCycleData) # read line 4 not used for IMAP=1
				dum = inastk ( objCycleData) # read line 5 not used for IMAP=1
				dum = inastk ( objCycleData) # read line 6 not used for IMAP=1
				
			if(Data.IMAP == 2)  :
				if (ICYCL == 2  and  Data.INCTRL  >=  4):
					MREFI[1] = inastk ( objCycleData, "float" )
					MREFI[2] = inastk ( objCycleData, "float" )
				else:
					MREFI[N] = inastk ( objCycleData, "float" )

				SPEEDI[N] = inastk ( objCycleData, "float" )
				TSPECI[N] = inastk ( objCycleData, "float" )
				
				DISPLC[N] = inastk ( objCycleData, "float" )
				
				dum = inastk ( objCycleData) # read line 1 not used for IMAP=2
				dum = inastk ( objCycleData) # read line 2 not used for IMAP=2
				dum = inastk ( objCycleData) # read line 3 not used for IMAP=2
				dum = inastk ( objCycleData) # read line 4 not used for IMAP=2
				
				CEI[N]    = inastk ( objCycleData, "float" )
				SEFFI[N]  = inastk ( objCycleData, "float" )
				MEFF[N]   = inastk ( objCycleData, "float" )
				
				ELOSS[N]  = inastk ( objCycleData, "float" )
				QCAN[N]   = inastk ( objCycleData, "float" )
				QHILO[N]  = inastk ( objCycleData, "float" )
				
			#
			# INTERCHANGER DATA
			#
			#IOCHECK  = inastk ( objCycleData ) # READLN(1,IN,1,IOCHECK)
			SUPIHX[N] = inastk ( objCycleData, "float" )

			if(ICYCL == 2)  :
				ETHX1 = inastk ( objCycleData, "float" )
				ETHX2 = inastk ( objCycleData, "float" )
			else:
				ETHX[N] = inastk ( objCycleData, "float" )
			
			#
			# IN-WALL EVAPORATOR DATA
			#
			if(N == NDATA) :
				#IOCHECK  = inastk ( objCycleData ) # READLN(1,IN,1,IOCHECK)
				Data.UA_FF = inastk ( objCycleData, "float" )
				Data.UA_FZ = inastk ( objCycleData, "float" )
				Data.UA_ML = inastk ( objCycleData, "float" )

				Data.UA_FF_CND = inastk ( objCycleData, "float" )
				Data.UA_FZ_CND = inastk ( objCycleData, "float" )

				Data.UA_FF_HXS = inastk ( objCycleData, "float" )
				Data.UA_FZ_HXS = inastk ( objCycleData, "float" )

				Data.FRACT_FF = inastk ( objCycleData, "float" )
				Data.FRACT_FZ = inastk ( objCycleData, "float" )

				Data.IWALL_FF = inastk ( objCycleData, "float" )
				Data.IWALL_FZ = inastk ( objCycleData, "float" )

				Data.UA_FF = Data.UA_FF * 1.8961
				Data.UA_FZ = Data.UA_FZ * 1.8961
				Data.UA_ML = Data.UA_ML * 1.8961

				Data.UA_FF_CND = Data.UA_FF_CND * 1.8961
				Data.UA_FZ_CND = Data.UA_FZ_CND * 1.8961

				Data.UA_FF_HXS = Data.UA_FF_HXS * 1.8961
				Data.UA_FZ_HXS = Data.UA_FZ_HXS * 1.8961
				
				Data.DFSTCYC = inastk ( objCycleData, "float" )
				if(IDFRST == 1): Data.DFSTCYC = 0.0

				if(IRFTYP < 3  or  IRFTYP == 7)  :
					Data.FZCYC = inastk ( objCycleData, "float" )
					Data.FFCYC = inastk ( objCycleData, "float" )
				else:
					Data.FZCYC = inastk ( objCycleData, "float" )
					dum = inastk ( objCycleData) # read line not used here

			Data.OUTCYC = inastk ( objCycleData, "float" )
			#
			# PROCESS DATA FOR DUAL LOOP CIRCUIT
			#
			#N = N + 1
			#END DO

		#
		#            ZERO CONDENSER HEAT LOADS TO CABINET AND EVAPORATORS
		#
		Data.Q_CND_FF = 0.0
		Data.Q_CND_FZ = 0.0
		Data.Q_HXS_FF = 0.0
		Data.Q_HXS_FZ = 0.0
		Data.CONDF_IN_WALL = 0.0
		Data.CONDZ_IN_WALL = 0.0
		#
		#            READ CABINET LOADS DATA (if PRESENT)
		#
		ICAB = 0
		#IOCHECK  = inastk ( objCycleData ) #  READLN(2,IN,2,IOCHECK)

		if not objCycleData.isError():
			ICAB = 1
			Data.FFASH = inastk ( objCycleData, "float" )
			Data.FFAUX = inastk ( objCycleData, "float" )
			Data.FZASH = inastk ( objCycleData, "float" )
			Data.FZAUX = inastk ( objCycleData, "float" )
			Data.OTHERW = inastk ( objCycleData, "float" )

			Data.TROOM = inastk ( objCycleData, "float" )
			Data.FFTEMP = inastk ( objCycleData, "float" )
			Data.FZTEMP = inastk ( objCycleData, "float" )

			Data.FFQ = inastk ( objCycleData, "float" )
			Data.FZQOFF = inastk ( objCycleData, "float" )

			Data.FFSEN = inastk ( objCycleData, "float" )
			Data.FFLAT = inastk ( objCycleData, "float" )
			FROSTF = inastk ( objCycleData, "float" )

			Data.FZSEN = inastk ( objCycleData, "float" )
			Data.FZLAT = inastk ( objCycleData, "float" )
			FROSTZ = inastk ( objCycleData, "float" )

			Data.FFPENA = inastk ( objCycleData, "float" )
			Data.FZPENA = inastk ( objCycleData, "float" )

			Data.FFHTQ = inastk ( objCycleData, "float" )
			Data.FZHTQ = inastk ( objCycleData, "float" )

			Data.FFREFQ = inastk ( objCycleData, "float" )
			Data.FZREFQ = inastk ( objCycleData, "float" )

			Data.QMUL = inastk ( objCycleData, "float" )

			#
			#            CHANGE UNITS ON REFRIGERATION LOAD DATA
			#
			Data.TROOM  = 1.8 * Data.TROOM  + 32.0
			Data.FFTEMP = 1.8 * Data.FFTEMP + 32.0
			Data.FZTEMP = 1.8 * Data.FZTEMP + 32.0

			Data.FFQ = Data.FFQ * 3.413
			Data.FZQOFF = Data.FZQOFF * 3.413
			Data.FZQON = Data.FZQOFF
			FZQ   = Data.FZQOFF

			Data.FFSEN = Data.FFSEN * 3.413
			Data.FFLAT = Data.FFLAT * 3.413
			FROSTF = FROSTF * 3.413
			FROSTFS = FROSTF

			Data.FZSEN = Data.FZSEN * 3.413
			Data.FZLAT = Data.FZLAT * 3.413
			FROSTZ = FROSTZ * 3.413
			FROSTZS = FROSTZ

			Data.FFHTQ = Data.FFHTQ * 3.413
			Data.FZHTQ = Data.FZHTQ * 3.413

			Data.FFPENA = Data.FFPENA * 3.413
			Data.FZPENA = Data.FZPENA * 3.413

			Data.QMUL = Data.QMUL * 3.413

			Data.FFREFQ = Data.FFREFQ * 3.413
			Data.FZREFQ = Data.FZREFQ * 3.413

			Data.CONDF = Data.FFQ - Data.FFSEN - Data.FFLAT - Data.FFHTQ - FROSTF - Data.FFREFQ - Data.FFPENA
			Data.CONDZ = FZQ - Data.FZSEN - Data.FZLAT - Data.FZHTQ - FROSTZ - Data.FZREFQ - Data.FZPENA

		# END if
		objCycleData = ""

		#
		#            NULL OUT THE ERROR FILE
		#
		#CALL BACKUP (FILE_ERROR_OUT)

		#            LOOP OVER THE NUMBER OF REFRIGERATION CIRCUITS
		#
		Data.FF_AIR = 0.0
		
		for N in range (1, NDATA+1 ): #DO WHILE (N <= NDATA)
			#
			# CONVERT UNITS
			#
			TS1[N]   = TS1[N] + 273.11
			TS3[N]   = TS3[N] + 273.11
			TS5      = TS5    + 273.11
			
			RHOCPC   = 316.8 / TS1[N]
			RHOCPE   = 316.8 / TS3[N]
			RHOCPF   = 316.8 / TS5
			
			Data.CFMC     = 1.8961 * (RHOCPC * CFMCI[N]) / 0.4720
			Data.CFME     = 1.8961 * (RHOCPE * CFMEI[N]) / 0.4720
			Data.CFMF     = 1.8961 * (RHOCPF * Data.CFMF) / 0.4720

			if(IFREZI[N] != 0)  :
				Data.UAF = 3.600 * Data.UAF

			Data.UTPE     = UTPEI[N] * 3.600
			Data.USUPE    = USUPEI[N] * 3.600
			Data.ATOTE    = ATOTEI[N]

			Data.DTSUPE   = DTSPEI[N]

			Data.UDSC     = UDSCI[N] * 3.600
			Data.UTPC     = UTPCI[N] * 3.600
			Data.USCC     = USCCI[N] * 3.600
			Data.ATOTC    = ATOTCI[N]

			Data.DTSUBC = DTSBCI[N]

			Data.TSPEC = TSPECI[N]
			if(TSPECI[N]  >  0.0): Data.TSPEC = TSPECI[N] + 273.11

			CEI[N]   = CEI[N] / 100.0
			SEFFI[N] = SEFFI[N] / 100.0
			MEFF[N]  = MEFF[N] / 100.0
			QCAN[N]  = QCAN[N] / 100.0
			QHILO[N] = QHILO[N] / 100.0

			Data.SIZE = SIZEN[N] / 0.252
			Data.EER = EERN[N]
			Data.ICOOL = ICOOLN[N]
			#
			# FILL THE COMMON BLOCK FOR REFRIGERATION LOOP N
			#
			Data.ICOND = ICONDI[N]
			Data.IFRSH = IFRSHI[N]
			Data.IFREZ = IFREZI[N]
			
			Data.MREF  = MREFI[N] * 2.20462
			Data.SPEEDN = SPDNOM[N]
			Data.SPEED  = SPEEDI[N]

			if(Data.IMAP == 1): Data.SPEED = Data.SPEEDN * SPEEDI[N]
			DISPLC[N] = DISPLC[N] / 16.3871 # from cu3 to cu-inch compressor displacement
		
			Data.CE = CEI[N]

			Data.SEFF  = SEFFI[N]
			Data.FANE  = FNPWRE[N]
			Data.FANC  = FNPWRC[N]
			Data.DUTYC = 0.5

			IRFTPL = IRFTYP
			IFAN = 0
			#
			# COMPRESSOR MAP FILE
			#
			if( Data.IMAP == 0 )  :
				Data.FILMAP = Data.FILMAP1
				if ( n == 1  and  ICYCL == 3): Data.FILMAP = Data.FILMAP2

				#CALL DOSFUN(1,filmap,'COMPMAP.DAT ',DOSCHR)# copy file to new file saved in filmap
				#CALL DOSCAL(.FALSE., .TRUE., False,False,DOSCHR)
				#CALL CURSOR[1]

				#OPEN(IN, FILE = 'COMPMAP.DAT', STATUS = 'OLD')
				objCompMap = FileAccess (Eravyv.FILE_COMPMAP_DAT) # open file for read

				FOUND_DATA = False
				#READ(IN,  * , IOSTAT = IOCHECK)
				_ = inastk ( objCompMap )

				while not FOUND_DATA: #WHILE( not FOUND_DATA)
					Data.MREF = inastk ( objCompMap, "float" ) # ask for float
					IOCHECK = objCompMap.isError()

					#if(IOCHECK != 0.0): continue
					if IOCHECK: continue

					FOUND_DATA =  True
					#READ(IN,  * )
					#READ(IN,  * )
					#READ(IN,  * )
					_ = inastk ( objCompMap )
					_ = inastk ( objCompMap )
					_ = inastk ( objCompMap )
					
					IUNITS = inastk ( objCompMap, "int" ) # ask for int

					if(IUNITS != 1): Data.MREF = 2.20462 * Data.MREF
					# END DO

				del(objCompMap)	 #close file
				Data.MREF = 2.20462 * MREFI[N]  # !!6-10-94
			#END if
			#
			# CYCLE SOLUTION CONTROL INDICES:
			# INCTRL: 0 = none,
			#         1 = adjust evaporator areas,
			#         2 = adjust fresh food section tempeature,
			#         3 = adjust freezer    section tempeature,
			#         4 = switching valve (only one section is cooled at a time)
			#         5 = solenoid valve or fan control provides evaporator capacity to only one cabinet
			#             during part of the cycle
			# ITYPE:  1 = standard cycle or dual-loop cycle also used in switching valve cycle
			#         2 = LORENZ cycle
			#         3 = 2nd call to CYCLE in switching valve cycle
			# IFAN:   0 = all cycles without switching valves
			#         1 = switching valve cycle
			#         2 = dual evaporator or LORENZ cycle with fan
			#             or valve control (INCTRL = 5).
			#
			# Strategy for Control Option 4: Redefine the cabinet as
			#         a freezer (one evaporator), which uses load 
			#         QFZ which is set equal to QFZOFF in DUTFND.
			#         Set up inputs twice for a call to a freezeer analysis.
			#
			# Strategy for Control Option 5: Solve first as an
			#         uncontrolled Lorenz cycle, and then call
			#         again for a cycle analysis for the cabinet with
			#         the highest duty cycle.  The second analysis
			#         uses the freezer model (IRFTYP = 4), with the
			#         appropriate variables for the load and evaporator
			#
			#         Option 5 requires a manual defrost R/F, with no
			#         cyclic dependent heater powers.  
			#         Fans may be used in the system.  Refrigerant line anti-sweat
			#         heat may be used, but it is assume (for simplicity)
			#         that all the heat is provided during the time
			#         when refrigerant flows through both evaporators.
			
			
			# CALL THE REFRIGERATION CYCLE ANALYSIS
			#
			if(ICYCL == 2)  :
				#
				#      LORENZ CYCLE ANALYSIS
				#
				Data.ITYPE = 2

				FZQOFF_S = Data.FZQOFF
				FROSTZ_S = FROSTZ
				DFSTCYC_S = Data.DFSTCYC

				if(Data.INCTRL == 4):
					Data.ITYPE = 1
					ICYCL = 1
					Data.IEVAP = 1
					IRFTPL = 4

					TS3[N] = (Data.FFTEMP + 459.6) / 1.8
					TS5 = -300.0
					Data.MREF = 2.20461 * MREFI[2]       # !!6-10-94

					Data.FZQOFF = Data.FFQ - FROSTF
					FROSTZ = 0.0
					Data.DFSTCYC = 0.0
				else:
					if(ICYCLS == 2)  :
						Data.IEVAP = 1
					else:
						Data.IEVAP = 2
					#END if
				#END if

				if(Data.INCTRL == 5):    # Restriction on
					Data.FFCYC = 0.0       #   option 5
					Data.FZCYC = 0.0
				#END if

				Data.ISPEC  = ISPECI[N]
				Data.XEXITE = QUALTY[N]
				Data.DTSUPI = SUPIHX[N]
				# getArr2dCol  Data.IR[1][N], Data.X[1][N], loc_F[1][1][N]
				self.cycle (Data.pythNC[N], self.getArr2dCol(Data.IR,N), self.getArr2dCol(Data.X,N), self.getArr3dLevel(F,N),	\
					TS1[N], TS3[N],	TS5,  MEFF[N], QHILO[N], QCAN[N],		\
					DPC[N], DPE[N], DPF, ETHX1,ETHX2,DISPLC[N],	N,	\
					FROSTF,FROSTZ,ICAB,IRFTPL,ICYCL,ICYCLS,IDFRST)
				#
				Data.PWRL = Data.W / Data.CORR_COP / 1.0548
				Data.DUTYL = Data.DUTYC
				Data.DUTYS = Data.DUTYL
				FANEL = FNPWRE[1]
				FANCL = FNPWRC[1]
				Data.QEN[1] = Data.QE + QZ
				QE_NET = Data.QE - Data.Q_HXS_FF
				QZ_NET = QZ - Data.Q_HXS_FZ
				QEL[1] = QE_NET + QZ_NET
				
				Data.FLOWN [1] = Data.FLOW
				Data.COPRN [1] = Data.COPR
				Data.COPCYC[1] = Data.CORR_COP
				#
				#           CALL AGAIN AS if Data.A STANDARD cycle if FLOW
				#           CONTROL OPTION USED
				#
				ICYCL = 1
				Data.ITYPE = 3
				TS5 = -300.0
				DPF = 0.0

				Data.ISPEC = ISPECI [N]
				Data.XEXITE = QUALTY[N]
				Data.DTSUPI = SUPIHX[N]
				#
				#           CALL cycle ANALYSIS FOR CONTROL MODES 4
				#           OR 5.
				#
				if(Data.INCTRL == 4)  :    # Switching valve
					Data.IBLNCE = 0
					IFAN = 1

					TS3[N] = (Data.FZTEMP + 459.6) / 1.8
					ETHX[N] = ETHX1
					CFMEI[N] = Data.CFMF
					FNPWRE[N] = Data.FANZ
					DPE[N] = DPF
					IFRSHI[N] = IFREZI[N]
					ATOTEI[N] = Data.UAF
					UTPEI[N] = 1

					Data.MREF  = 0.75 * Data.FLOW       # Next guess for FZ
					Data.MREF  = 2.20461 * MREFI[1]                # !!6-10-94
					Data.IEVAP = 2
					Data.CFME  = Data.CFMF
					Data.IFRSH = Data.IFREZ
					Data.FANE  = Data.FANZ
					Data.ATOTE = Data.UAF
					Data.UTPE  = 1.0
					IFANE = IFRSHI[N]

					FROSTZ = FROSTZ_S
					Data.FZQOFF = FZQOFF_S
					Data.DFSTCYC = DFSTCYC_S

					Data.UA_FF = Data.UA_FZ
					Data.UA_FF_HXS = Data.UA_FZ_HXS
					Data.UA_FF_CND = Data.UA_FZ_CND

					Q_FF_IN_WALL_S = Data.Q_FF_IN_WALL
					CAPE_IN_WALL_S = Data.CAPE_IN_WALL
					CONDF_IN_WALL_S = Data.CONDF_IN_WALL
					Q_HXS_FF_S = Data.Q_HXS_FF

					Data.DUTYS = 0.0
					# Data.IR[1][N], Data.X[1][N], loc_F[1][1][N]
					self.cycle (Data.pythNC[N], self.getArr2dCol(Data.IR,N), self.getArr2dCol(Data.X,N), self.getArr3dLevel(F,N),	\
						TS1[N],TS3[N], TS5, MEFF[N], QHILO[N],				\
						QCAN[N], DPC[N], DPE[N], DPF, ETHX[N], ETHX[N],		\
						DISPLC[N], N, FROSTF, FROSTZ, ICAB, IRFTPL, ICYCL,	\
						ICYCLS,IDFRST)

					Data.PWRE  = Data.W / Data.CORR_COP / 1.0548
					Data.DUTYS = Data.DUTYC
					FANES = FNPWRE[1]
					FANCS = FNPWRC[1]
					Data.QEN[2] = Data.QE + QZ
					QE_NET = Data.QE - Data.Q_HXS_FF
					
					QZ_NET = QZ - Q_HXS_FZ
					QEL[2] = QE_NET + QZ_NET
				   
					Data.FLOWN [2] = Data.FLOW
					Data.COPRN [2] = Data.COPR
					Data.COPCYC[2] = Data.CORR_COP

					Data.Q_FZ_IN_WALL = Data.Q_FF_IN_WALL
					Data.Q_FF_IN_WALL = Q_FF_IN_WALL_S

					Data.CAPZ_IN_WALL = Data.CAPE_IN_WALL
					Data.CAPE_IN_WALL = CAPE_IN_WALL_S

					Data.CONDZ_IN_WALL = Data.CONDF_IN_WALL
					Data.CONDF_IN_WALL = CONDF_IN_WALL_S

					Data.Q_HXS_FZ = Data.Q_HXS_FF
					Data.Q_HXS_FF = Q_HXS_FF_S
				#END if

				if(Data.INCTRL == 5)  :    # Refrigerant bypass
					IFAN = 2
					Data.IBLNCE =  0
					Data.DUTYE = min(Data.DUTYE,1.0)
					Data.DUTYZ = min(Data.DUTYZ,1.0)

					if(abs(Data.DUTYE -Data.DUTYZ) < 0.01)  :   # Loads balanced
						Data.INCTRL = 0
						IFAN = 0
					#END if
				#END if

				if(Data.INCTRL == 5):    # Refrigerant bypass
					ICYCL = 1
					IRFTPL = 4
					TS5 = -300.0
					Data.MREF = 0.75 * Data.FLOW
					Data.MREF = 2.20461 * MREFI[2]                # !!6-10-94

					Data.CONDHT[N] = 0.0          # No anti-sweat heat
					Data.CONDVP[N] = 0.0

					if (Data.DUTYE  >  Data.DUTYZ)  :             # Call FF cycle
						Data.IEVAP = 1
						Data.ITYPE = 4
						cycle_heat = Data.DUTYL * (Data.CAPE_IN_WALL - Data.Q_FF_IN_WALL)	\
							 + Data.DUTYL * (Data.CONDF_IN_WALL - Data.Q_FZ_FF)	\
							 + Data.DUTYL * 3.413 * fnpwre[1]

						TS3[N] = (Data.FFTEMP + 459.6) / 1.8
						ETHX[N] = ethx1
						Data.FZQOFF = Data.FFQ + cycle_heat - qe_net * Data.DUTYZ / 1.0548

					else:   # Call FZ cycle
						Data.IEVAP = 2
						Data.ITYPE = 3

						TS3[N] = (Data.FZTEMP + 459.6) / 1.8
						ETHX[N] = ethx1

						cycle_heat = Data.DUTYL * (Data.CAPZ_IN_WALL - Data.Q_FZ_IN_WALL)	\
							 + Data.DUTYL * (Data.CONDZ_IN_WALL + Data.Q_FZ_FF) + Data.DUTYL * 3.413 * Data.FANZ
						
						FZQOFF = FZQOFF + cycle_heat -QZ_NET * Data.DUTYE/1.0548

						CFMEI[N] = Data.CFMF
						FNPWRE[N] = Data.FANZ
						DPE[N] = dpf
						IFRSHI[N] = IFREZI[N]
						ATOTEI[N] = Data.UAF
						UTPEI[N] = 1

						Data.CFME  = Data.CFMF
						Data.IFRSH = Data.IFREZ
						Data.FANE  = Data.FANZ
						Data.ATOTE = Data.UAF
						Data.UTPE = 1.0
						IFANe = IFRSHI[N]

						Data.UA_FF = Data.UA_FZ
						Data.UA_FF_HXS = Data.UA_FZ_HXS
						Data.UA_FF_CND = Data.UA_FZ_CND

					#END if

					Q_FF_IN_WALL_S = Data.Q_FF_IN_WALL
					CAPE_IN_WALL_S = Data.CAPE_IN_WALL
					CONDF_IN_WALL_S = Data.CONDF_IN_WALL

					Q_FZ_IN_WALL_S = Data.Q_FZ_IN_WALL
					CAPZ_IN_WALL_S = Data.CAPZ_IN_WALL
					CONDZ_IN_WALL_S = Data.CONDZ_IN_WALL

					Q_FZ_FF_S = Data.Q_FZ_FF

					Data.DUTYS = 0.0
					# Data.IR[1][N], Data.X[1][N], loc_F[1][1][N]
					self.cycle (Data.pythNC[N], self.getArr2dCol(Data.IR,N), self.getArr2dCol(Data.X,N), self.getArr3dLevel(F,N),	\
						TS1[N],TS3[N], TS5, MEFF[N], QHILO[N],				\
						QCAN[N],DPC[N],DPE[N],DPF,ETHX[N],ETHX[N],		\
						DISPLC[N],N,FROSTF,FROSTZ,ICAB,IRFTPL,ICYCL,	\
						ICYCLS,IDFRST)

					Data.PWRE  = Data.W / Data.CORR_COP / 1.0548
					Data.DUTYS = Data.DUTYC
					FANES = FNPWRE[1]
					FANCS = FNPWRC[1]
					Data.QEN[2] = Data.QE + QZ
					QE_NET = Data.QE - Data.Q_HXS_FF

					QEL[2] = QE_NET + QZ_NET
					
					Data.FLOWN[2] = Data.FLOW
					Data.COPRN[2] = Data.COPR
					Data.COPCYC[2] = Data.CORR_COP

					DUTYN[1] = Data.DUTYL
					DUTYN[2] = Data.DUTYS

					if (Data.IEVAP == 1)  : # Correct FF cycle
						Data.CONDF_IN_WALL = (CONDF_IN_WALL_S * Data.DUTYL + Data.CONDF_IN_WALL * Data.DUTYS) / DUTYN[1]
						Data.CONDZ_IN_WALL = CONDZ_IN_WALL_S * Data.DUTYL / DUTYN[1]
						
						Data.CAPE_IN_WALL = (CAPE_IN_WALL_S * Data.DUTYL + Data.CAPE_IN_WALL * Data.DUTYS) / DUTYN[1]
						Data.CAPZ_IN_WALL = CAPZ_IN_WALL_S * Data.DUTYL / DUTYN[1]
						Data.Q_FF_IN_WALL = (Q_FF_IN_WALL_S * Data.DUTYL + Data.Q_FF_IN_WALL * Data.DUTYS) / DUTYN[1]
						Data.Q_FZ_IN_WALL = Q_FZ_IN_WALL_S * Data.DUTYL / DUTYN[1]
						Data.Q_FZ_FF = Q_FZ_FF_S * Data.DUTYL / DUTYN[1]

					else:   # Correct FZ cycle
						Data.CONDZ_IN_WALL = (CONDZ_IN_WALL_S * Data.DUTYL  + Data.CONDF_IN_WALL * Data.DUTYS) / DUTYN[1]
						Data.CONDF_IN_WALL = CONDF_IN_WALL_S * Data.DUTYL / DUTYN[1]
						Data.CAPZ_IN_WALL = (CAPZ_IN_WALL_S * Data.DUTYL + Data.CAPE_IN_WALL * Data.DUTYS) / DUTYN[1]
						Data.CAPE_IN_WALL = CAPE_IN_WALL_S * Data.DUTYL / DUTYN[1]
						Data.Q_FZ_IN_WALL = (Q_FZ_IN_WALL_S * Data.DUTYL + Data.Q_FF_IN_WALL * Data.DUTYS) / DUTYN[1]
						Data.Q_FF_IN_WALL = Q_FF_IN_WALL_S * Data.DUTYL / DUTYN[1]
						Data.Q_FZ_FF = Q_FZ_FF_S * Data.DUTYL / DUTYN[1]
					# END if
				# END if

				ICYCL = 2
			else:
				#
				#      STANDARD cycle OR DUAL LOOP cycle
				#
				Data.ITYPE = 1
				TS5 = -300.0
				DPF = 0.0
				if(ICYCL == 1)  :
					Data.IEVAP = 0
				else:
					if(N == 1)  :
						Data.IEVAP = 2
					else:
						Data.IEVAP = 1
					# END if
				# END if

				Data.ISPEC = ISPECI[N]
				Data.XEXITE = QUALTY[N]
				Data.DTSUPI = SUPIHX[N]
				# Data.IR[1][N], Data.X[1][N], loc_F[1][1][N]

				self.cycle ( Data.pythNC[N], self.getArr2dCol(Data.IR,N), self.getArr2dCol(Data.X,N), self.getArr2dCol(Data.X,N),	\
					TS1[N], TS3[N], TS5, MEFF[N], QHILO[N], QCAN[N],					\
					DPC[N], DPE[N], DPF,  ETHX[N], ETHX[N], DISPLC[N], N,			\
					FROSTF, FROSTZ, ICAB, IRFTPL,  ICYCL,   ICYCLS, IDFRST)
			
			# END if
			#
			# SET UP PARAMETERS FOR SUMMARY OUTPUT
			#
			if(IFAN != 2)  :
				DUTYN[N] = Data.DUTYC
				WCOMP[N] = Data.W / Data.CORR_COP / 1.0548
			# END if

			if(ICYCL != 2)  :
					QE_NET = Data.QE - Data.Q_HXS_FF
					QEL[N] = QE_NET
					Data.FLOWN[N] = Data.FLOW
					Data.COPRN[N] = Data.COPR
					Data.COPCYC[N] = Data.CORR_COP
			# END if
		#N = N + 1
		#==END DO
		#
		#            OUTPUT Data.A SUMMARY PAGE if CABINET LOADS ANALYSIS DONE
		#
		if(ICAB == 1) :
			self.summry(TITLE,IRFTYP,ICYCLS,DUTYN,FNPWRE,FNPWRC,
						ELOSS,WCOMP,IFAN,IDFRST,FROSTFS,FROSTZS,
						QEL,QE_NET, QZ_NET )

		# END if
		#
		#            finish UP HOUSEKEEPING DETAILS AND EXIT
		#
		sys.exit
		return

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def summry (self, TITLE,IRFTYP,ICYCL,DUTYN,FNPWRE,FNPWRC, 	\
				ELOSS,WCOMP,IFAN,IDFRST,FROSTF,FROSTZ, 		\
				QEL,QE_NET,QZ_NET ):
				
		#SUBROUTINE SUMMRY(TITLE,IRFTYP,ICYCL,DUTYN,FNPWRE,FNPWRC,
		#                  ELOSS,WCOMP,IFAN,IDFRST,FROSTF,FROSTZ,
		#                  QEL,QE_NET,QZ_NET)
		#        *****************************************************************
		#        *     OUTPUT Data.A SUMMARY OF THE RESULTS FOR THE ENTIRE ANALYSIS     *
		#        *****************************************************************
		#
		#CHARACTER TITLE
		#CHARACTER * 2 CHOUR,CMIN,CSEC,CDAY,CMONN,CYEAR
		#CHARACTER * 3 CMON
		#CHARACTER * 6 Data.HREF(34),Data.REFH(34)
		#CHARACTER * 13 FILERA
		#CHARACTER * 13    filmap, filmap1, filmap2

		#DIMENSION FNPWRE[2],FNPWRC[2],ELOSS[2],DUTYN[2],WCOMP[2],QEL[2]
		#DIMENSION TITLE(68,5)

		# COMMON / PARMS2 / TSPEC,I_LIQUID_LINE
		# COMMON / CABLOD / FFASH,FFAUX,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
		#              FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
		#              FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
		# COMMON / FANS / FANE,FANZ,FANC,DUTYC,W,COPR
		# COMMON / LORENZ / DUTYE,DUTYZ,PWRL,PWRE,CAPE,CAPZ,DUTYL,DUTYS,
		#              FANEL,FANCL,FANES,FANCS
		# COMMON / TIME / CHOUR,CMIN,CSEC,CDAY,CMON,CMONN,IYEAR,CYEAR
		# COMMON / RESULT / QE, QZ, FLOW, QEN[2], FLOWN[2], COPRN[2]
		# COMMON / CYCLNG / CORR_COP, COPCYC[2], I_CYCLE, I_VALVE, T_CYCLE
		# COMMON  / HREF1 / HREF,REFH
		# COMMON  / REFRIG /  pythNC[2], IR(5,2), X(5,2), loc_F[5,5,2)
		# COMMON / FILINF / FILERA
		# COMMON  / FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz
		# COMMON / BALNCE / IBLNCE, BAFFLF, BAFFLZ, AREAFZ, ATOTE_S,
		#      AREAFZ_S, ATOTE_A, AREAFZ_A, FFTEMP_A, FZTEMP_A

		# COMMON  / FIGURE / IEVAP
		# COMMON / CHINA / INCTRL, HRSOFF
		# COMMON / INWALL / Data.UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
		#.    Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
		#.    CAPZ_IN_WALL, Q_FZ_FF
		# COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
		#.      Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
		#.      CONDF_IN_WALL, CONDZ_IN_WALL
		# COMMON / LIQLIN / FFREFQ, FZREFQ, CONDHT[2], CONDVP[2]
		# COMMON / CYCLIC / DFSTCYC, FFCYC, FZCYC, OUTCYC
		# COMMON / MAPNAM / filmap, filmap1, filmap2
		# COMMON / MAPDAT / IMAP, ICOMP, ICOOL, EER, SIZE, DISPL, EFFC,
		#.     SPEEDN, IREAD
		# COMMON / PENAT / FFPENA, FZPENA
		#
		#DATA IO  / 8 /

		#
		#            OPEN OUTFILE FILE AND WRITE THE TITLE
		#

		objEraDat = FileAccess (Eravyv.FILE_ERA_DAT,"write") # open file for read
		
		now = datetime.datetime.now( )
		objEraDat.write_or_terminate( ( Data.FILERA + now.strftime( "%H %M %S %d %b %Y" ) ) + " - Python Output aymhenry@gmail")
		objEraDat.write_or_terminate( "SUMMARY RESULTS" )
		objEraDat.write_or_terminate( TITLE )
		#
		#            OUTPUT INFORMATION ON TYPE OF CABINET AND cycle
		#
		if IRFTYP == 1:
			objEraDat.write_or_terminate("TWO-DOOR TOP-MOUNT REFRIGERATOR")
			objEraDat.write_or_terminate("FREEZER")

		if IRFTYP == 2:
			objEraDat.write_or_terminate('TWO-DOOR BOTTOM-MOUNT BEFRIGERATOR')
			objEraDat.write_or_terminate("FREEZER")

		if IRFTYP == 3:
			objEraDat.write_or_terminate('SIDE-BY-SIDE REFRIGERATOR')
			objEraDat.write_or_terminate("FREEZER")

		if IRFTYP == 4:
			objEraDat.write_or_terminate("CHEST FREEZER")
			objEraDat.write_or_terminate(" ")
			
		if IRFTYP == 5:
			objEraDat.write_or_terminate("UPRIGHT FREEZER")
			objEraDat.write_or_terminate(" ")

		if IRFTYP == 6:
			objEraDat.write_or_terminate("ONE-DOOR REFRIGERATOR")
			objEraDat.write_or_terminate(" ")

		if IRFTYP == 7:
			objEraDat.write_or_terminate("ONE-DOOR REFRIGERATOR")
			objEraDat.write_or_terminate("FREEZER")

		if ICYCL == 1:
			objEraDat.write_or_terminate("STANDARD SINGLE EVAPORATOR cycle")

		if ICYCL == 2:
			objEraDat.write_or_terminate("LORENZ cycle")

		if ICYCL == 3:
			objEraDat.write_or_terminate("DUAL LOOP cycle")

		if ICYCL == 4:
			objEraDat.write_or_terminate("DUAL EVAP cycle")
			objEraDat.write_or_terminate(" ")

		#
		#            COMPRESSOR MAP USEDG
		#

		if Data.IMAP == 0:
			if(ICYCL != 3) :
				objEraDat.write_or_terminate("COMPRESSOR MAP FILE: " + Data.FILMAP1)
				
			else:
				objEraDat.write_or_terminate("FRESH FOOD COMPRESSOR MAP FILE: " + Data.FILMAP1)
				objEraDat.write_or_terminate("FREEZER    COMPRESSOR MAP FILE: " + Data.FILMAP2)
			# END if
		# END if

		#            HANDLE DUAL EVAP cycle
		#
		ICYCLS = ICYCL
		if(ICYCL == 4): ICYCL = 2

		#
		#            REFRIGERANT DATA
		#
		MAXLIN = Data.pythNC[1]
		if( ICYCL == 3  and  Data.pythNC[2]  >  Data.pythNC[1]): MAXLIN = Data.pythNC[2]
		
		if(IRFTYP < 3  or  IRFTYP == 7):
			objEraDat.write_or_terminate('REFRIGERANT DATA , FRESH FOOD ,FREEZER\n' )
				
		else:
			objEraDat.write_or_terminate('REFRIGERANT DATA , CABINET')
		# END if

		if ICYCL in [1,2]:
			X2 = Data.X[1][1]
			if(MAXLIN == 1):
				if(IRFTYP < 3  or  IRFTYP == 7)  :
					objEraDat.write_or_terminate (", " + Data.HREF[ Data.IR[1][1] ] + "," + Data.HREF[ Data.IR[1][1] ] )
					#WRITE(IO,812) Data.HREF(Data.IR(1,1)),Data.HREF(Data.IR(1,1))
				else:
					objEraDat.write_or_terminate (" ,%7.3f "  %(Data.HREF[ Data.IR[1][1] ] ))
					#WRITE(IO,812) Data.HREF(Data.IR(1,1))
				# END if
		else:
			if(IRFTYP < 3  or  IRFTYP == 7):
				objEraDat.write_or_terminate (", %7.3f, %7.3f" %( Data.HREF[ Data.IR[1][2] ] , Data.HREF[ Data.IR[1][1] ] ))
				#WRITE(IO,813) X2,Data.HREF(Data.IR(1,1)),X2,Data.HREF(Data.IR(1,1))
			else:
				objEraDat.write_or_terminate (" ,%7.3f "  %(Data.HREF[ Data.IR[1][1] ] ))
				#WRITE(IO,813) X2,Data.HREF(Data.IR(1,1))
				# END if
				
				for I in range (2, MAXLIN): #DO WHILE (I < MAXLIN)
					X2 = Data.X[I][1]
					if(IRFTYP < 3  or  IRFTYP == 7):
						objEraDat.write_or_terminate (", %7.3f, %7.3f" %( Data.HREF[ Data.IR[I][2] ] , Data.HREF[ Data.IR[I][1] ] ))
					
						#WRITE(IO,813) X2,Data.HREF(Data.IR(I,1)),  X2,Data.HREF(Data.IR(I,1))
					else:
						objEraDat.write_or_terminate (" ,%7.3f "  %(Data.HREF[ Data.IR[I][1] ] ))
						#WRITE(IO,813) X2,Data.HREF(Data.IR(I,1))
					# End IF
				#I = I + 1
				# End DO
		# End IF

		if ICYCL == 3:
			objEraDat.write_or_terminate (", %7.3f, %7.3f" %( Data.HREF[ Data.IR[1][2] ] , Data.HREF[ loc_IR[1][1] ] ))
		#
		#            CABINET LOADS SUMMARY
		#
		Data.FFTEMP = (Data.FFTEMP - 32.0) / 1.8
		Data.FFTEMP_A = (Data.FFTEMP_A - 32.0) / 1.8
		Data.FZTEMP = (Data.FZTEMP - 32.0) / 1.8
		Data.FZTEMP_A = (Data.FZTEMP_A - 32.0) / 1.8


		if IRFTYP in [1,2,3,7]:
			#SELECT CASE (IRFTYP)
			#
			# REFRIGERATOR - FREEZERS
			#
			if(Data.FFTEMP != Data.FFTEMP_A  or  Data.FZTEMP != Data.FZTEMP_A)  :
				objEraDat.write_or_terminate('DEFINED  SET POINTS (C), %7.3f, %7.3f ' %(Data.FFTEMP, Data.FZTEMP)     )
				objEraDat.write_or_terminate('ADJUSTED SET POINTS (C), %7.3f, %7.3f ' %(Data.FFTEMP_A, Data.FZTEMP_A) )
			else:
				objEraDat.write_or_terminate( 'SET POINTS (C) , %7.3f, %7.3f' %(Data.FFTEMP, Data.FZTEMP) )
				# End IF
			#
			#            HANDLE THE CASE WHERE THE AREAS WERE ADJUSTED
			#
			if(Data.AREAFZ != Data.AREAFZ_S) :
				objEraDat.write_or_terminate('DEFINED EVAP AREAS, %7.3f  , %7.3f ' %(Data.ATOTE_S, Data.AREAFZ_S) )
				objEraDat.write_or_terminate('ADJUSTED EVAP AREAS, %7.3f , %7.3f ' %(Data.ATOTE_A, Data.AREAFZ_A) )
			# End IF

			DOORFF = Data.FFSEN + Data.FFLAT
			DOORFZ = Data.FZSEN + Data.FZLAT

			#
			#            CORRECTION TO CABINET LOADS DUE TO IN-WALL EVAPORATOR
			#
			WALL_FF = DUTYN[1] * (Data.CAPE_IN_WALL - Data.Q_FF_IN_WALL)
			WALL_ML = DUTYN[1] * Data.Q_FZ_FF
			WALL_FZ = DUTYN[1] * (Data.CAPZ_IN_WALL - Data.Q_FZ_IN_WALL)

			WALL_CND_FF = DUTYN[1] * Data.CONDF_IN_WALL
			WALL_CND_FZ = DUTYN[1] * Data.CONDZ_IN_WALL

			if(ICYCL == 2  and  Data.INCTRL == 4)  :
				WALL_FF = Data.DUTYL * (Data.CAPE_IN_WALL - Data.Q_FF_IN_WALL)
				WALL_ML = 0.0
				WALL_FZ = Data.DUTYS * (Data.CAPZ_IN_WALL - Data.Q_FZ_IN_WALL)

				WALL_CND_FF = Data.DUTYL * Data.CONDF_IN_WALL
				WALL_CND_FZ = Data.DUTYS * Data.CONDZ_IN_WALL
			# End IF

			#
			#            FAN POWERS
			#
			if(ICYCL == 1)  :
				FFFAN = 3.413 * DUTYN[1] * FNPWRE[1] * Data.FF_AIR
				FZFAN = 3.413 * DUTYN[1] * FNPWRE[1] * (1.0 - Data.FF_AIR)

				Data.DFSTCYC = 3.413 * Data.DFSTCYC * DUTYN[1]
				Data.FFHTQ = Data.FFHTQ + 3.413 * Data.FFCYC * DUTYN[1]
				Data.FZHTQ = Data.FZHTQ + 3.413 * Data.FZCYC * DUTYN[1]
			# End IF

			if(ICYCL == 2)  :
				FFFAN = 3.413 * Data.DUTYL * FANEL
				FZFAN = 3.413 * Data.DUTYS * Data.FANZ

				Data.DFSTCYC = 3.413 * Data.DFSTCYC * Data.DUTYS
				Data.FFHTQ = Data.FFHTQ + 3.413 * Data.FFCYC * Data.DUTYL
				Data.FZHTQ = Data.FZHTQ + 3.413 * Data.FZCYC * Data.DUTYS
			# End IF

			if(IFAN == 2)  :
				Data.DUTYC = max(Data.DUTYE, Data.DUTYZ)
				if(Data.IEVAP == 1):                # FF cycle
					fffan = 3.413 * Data.FANE * Data.DUTYL + fanes * Data.DUTYS
					fzfan = 3.413 * Data.FANZ * Data.DUTYL
				else:
					fffan = 3.413 * fanel * Data.DUTYL
					fzfan = 3.413 * Data.FANZ * Data.DUTYL + 3.413 * fanes * Data.DUTYS
				# End IF
			# End IF

			if(ICYCL == 3)  :
				FZFAN = 3.413 * DUTYN[1] * FNPWRE[1]
				FFFAN = 3.413 * DUTYN[2] * FNPWRE[2]

				Data.DFSTCYC = 3.413 * Data.DFSTCYC * DUTYN[1]
				Data.FZHTQ = Data.FZHTQ + 3.413 * Data.FZCYC * DUTYN[1]
				Data.FFHTQ = Data.FFHTQ + 3.413 * Data.FFCYC * DUTYN[2]
			# End IF

			if(ICYCL != 1): FROSTF = 0.0

			Data.FFQ = Data.CONDF + DOORFF + Data.FFHTQ  + WALL_CND_FF + Data.FFREFQ	\
				+ FFFAN  + FROSTF + WALL_FF - WALL_ML + Data.FFPENA

			FZQ = Data.CONDZ + DOORFZ + Data.FZHTQ  + WALL_CND_FZ + Data.FZREFQ	\
				+ FZFAN  + FROSTZ + WALL_FZ + WALL_ML + Data.FZPENA
			#
			#      OUTPUT THE LOADS
			#
			CONDT  = Data.CONDF  + Data.CONDZ
			DOORT  = DOORFF + DOORFZ
			TPENA  = Data.FFPENA + Data.FZPENA
			THTQ   = Data.FFHTQ  + Data.FZHTQ
			TREFQ  = Data.FFREFQ + Data.FZREFQ
			TFAN   = FFFAN  + FZFAN
			FROSTT = FROSTF + FROSTZ
			TQ     = Data.FFQ    + FZQ

			DFRSTF = FROSTF / 3.413
			DFRSTZ = FROSTZ / 3.413

			if(IDFRST == 1)  :
				DFRSTF = 0.0
				DFRSTZ = 0.0
			else:
				Data.FFQ = Data.FFQ + FROSTF
				FZQ = FZQ + FROSTZ
				TQ  = TQ  + FROSTF + FROSTZ
			# End IF

			DFRSTZ = DFRSTZ + Data.DFSTCYC / 3.413
			DFRSTT = DFRSTF + DFRSTZ
			FZQ = FZQ + Data.DFSTCYC
			TQ  = TQ  + Data.DFSTCYC
			# 801
			objEraDat.write_or_terminate('CABINET LOADS (W) ')
			objEraDat.write_or_terminate('  ,FRESH FOOD, FREEZER, TOTAL ')
			objEraDat.write_or_terminate('CONDUCTION, %7.3f, %7.3f, %7.3f ' %(Data.CONDF / 3.413,  Data.CONDZ / 3.413,  CONDT / 3.413) )
			objEraDat.write_or_terminate('DOOR OPENINGS, %7.3f, %7.3f, %7.3f ' % (DOORFF / 3.413, DOORFZ / 3.413, DOORT / 3.413) )
			objEraDat.write_or_terminate('FROSTING, %7.3f, %7.3f, %7.3f ' % (FROSTF / 3.413, FROSTZ / 3.413, FROSTT / 3.413) )
			
			objEraDat.write_or_terminate('ELEC DEFROST, %7.3f, %7.3f, %7.3f ' % (DFRSTF,       DFRSTZ,       DFRSTT) )
			objEraDat.write_or_terminate('PENETRATIONS, %7.3f, %7.3f, %7.3f ' % (Data.FFPENA / 3.413, Data.FZPENA / 3.413, TPENA / 3.413) )
			objEraDat.write_or_terminate('HEATERS & CONTROLS, %7.3f, %7.3f, %7.3f ' % (Data.FFHTQ / 3.413,  Data.FZHTQ / 3.413,  THTQ / 3.413) )
			
			objEraDat.write_or_terminate('FAN HEAT, %7.3f, %7.3f, %7.3f ' % (FFFAN / 3.413,  FZFAN / 3.413,  TFAN / 3.413) )
			objEraDat.write_or_terminate('REFRIGERANT LINE, %7.3f, %7.3f, %7.3f ' % (Data.FFREFQ / 3.413, Data.FZREFQ / 3.413, TREFQ / 3.413) )
			objEraDat.write_or_terminate('IN_WALL EVAP, %7.3f, %7.3f, %7.3f ' % (WALL_FF / 3.413, WALL_FZ / 3.413, (WALL_FF + WALL_FZ) / 3.413) )
			
			objEraDat.write_or_terminate('IN_MULLION EVAP, %7.3f, %7.3f, %7.3f ' % ( -WALL_ML / 3.413, WALL_ML / 3.413, 0.0) )
			objEraDat.write_or_terminate('IN_WALL COND, %7.3f, %7.3f, %7.3f ' % (WALL_CND_FF / 3.413, WALL_CND_FZ / 3.413, (WALL_CND_FF + WALL_CND_FZ) / 3.413) )
			objEraDat.write_or_terminate('TOTAL LOADS, %7.3f, %7.3f, %7.3f ' % (Data.FFQ / 3.413,    FZQ / 3.413,    TQ / 3.413) )
	
			if Data.IBLNCE == 1: 
				objEraDat.write_or_terminate('NOTE: THE CABINET LOADS ARE OUT OF BALANCE WITH THE EVAPORATOR CAPACITIES\n' )
		else:
			#
			#      FREEZERS
			#
			if (IRFTYP == 6)  :
				objEraDat.write_or_terminate( 'FRESH FOOD TEMP (C), %7.3f' %(Data.FFTEMP))
				objEraDat.write_or_terminate( 'FREEZER TEMP    (C), %7.3f' %(Data.FZTEMP))
				
			else:
				objEraDat.write_or_terminate( " " )
				objEraDat.write_or_terminate( 'SET POINTS (C) , %7.3f ' %(Data.FZTEMP) )
			# End IF

			WALL_FF = DUTYN[1] * (Data.CAPE_IN_WALL - Data.Q_FF_IN_WALL)
			WALL_CND_FF = DUTYN[1] * Data.CONDF_IN_WALL
			DOORFZ = Data.FZSEN + Data.FZLAT
			FZQ = Data.FZQOFF
			Data.CONDZ = FZQ - DOORFZ - Data.FZHTQ - FROSTZ - Data.FZREFQ - Data.FZPENA

			FZFAN = 3.413 * DUTYN[1] * FNPWRE[1]
			Data.FZHTQ = Data.FZHTQ + 3.413 * DUTYN[1] * Data.FZCYC
			Data.DFSTCYC = 3.413 * DUTYN[1] * Data.DFSTCYC

			FZQ = FZQ + FZFAN + WALL_FF + WALL_CND_FF + 3.413 * DUTYN[1] * Data.FZCYC

			DFRSTZ = FROSTZ / 3.413
			if (IDFRST == 1)  :
				DFRSTZ = 0.0
			else:
				FZQ = FZQ + FROSTZ
			# End IF

			DFRSTZ = DFRSTZ + Data.DFSTCYC / 3.413
			FZQ = FZQ + Data.DFSTCYC
			#
			#      OUTPUT THE LOADS
			#
			objEraDat.write_or_terminate ('CABINET LOADS,(W)')

			objEraDat.write_or_terminate ('CONDUCTION,    ,%7.3f' %(Data.CONDZ  / 3.413) )
			objEraDat.write_or_terminate ('DOOR OPENINGS  ,%7.3f' %(DOORFZ / 3.413) )
			objEraDat.write_or_terminate ('FROSTING       ,%7.3f' %(FROSTZ / 3.413) )

			objEraDat.write_or_terminate ('ELEC DEFROST   ,%7.3f' %(DFRSTZ ) )

			objEraDat.write_or_terminate ('PENETRATIONS   ,%7.3f' %(Data.FZPENA / 3.413) )
			objEraDat.write_or_terminate ('HEATERS & CONTROLS,%7.3f' %(Data.FZHTQ / 3.413) )
			objEraDat.write_or_terminate ('FAN HEAT          ,%7.3f' %(FZFAN ) )

			objEraDat.write_or_terminate ('REFRIGERANT LINE,%7.3f' %(Data.FZREFQ / 3.413) )
			objEraDat.write_or_terminate ('IN_WALL EVAP,%7.3f' %(WALL_FF / 3.413) )
			objEraDat.write_or_terminate ('IN_WALL COND,%7.3f' %(WALL_CND_FF / 3.413) )
			objEraDat.write_or_terminate ('TOTAL LOADS,%7.3f' %(FZQ / 3.413) )
			#END SELECT
		#
		#            OUTPUT ERROR MESSAGE ABOUT LIQUID LINE ANTI-SWEAT
		#
		if(Data.I_LIQUID_LINE != 0):
			objEraDat.write_or_terminate('WARNING: SPECIFIED LIQUID-LINE ANTI-SWEAT MAY BE EXCESSIVE\n' )

		# End IF

		#            OUTPUT MESSAGE ABOUT EVAPORATOR BYPASS
		#
		if(IFAN == 2  and  Data.IEVAP == 1): 
			objEraDat.write_or_terminate('FREEZER EVAPORATOR BYPASSED DURING PART OF THE CYCLE\n'  )
			 
		if(IFAN == 2. and  Data.IEVAP == 2): 
			objEraDat.write_or_terminate('FRESH FOOD EVAPORATOR BYPASSED DURING PART OF THE CYCLE\n')

		#            OUTPUT THE cycle ENERGIES
		#
		if IRFTYP in [1,2,3,7]:
			#SELECT CASE (IRFTYP)
			#CASE (1, 2, 3, 7)
			# REFRIGERATOR - FREEZERS
			# STANDARD cycle
			if(ICYCL == 1)  :
				POWERF = 0.024 * (DUTYN[1] * WCOMP[1] / 3.413)
				FANEF  = 0.024 * (DUTYN[1] * (FNPWRE[1] + FNPWRC[1]))
				ELECF  = 0.024 * (DUTYN[1] * ELOSS[1])
				HTCNEF =  Data.FFASH + Data.FFAUX + Data.FZASH + Data.FZAUX + Data.OTHERW	\
					+ DUTYN[1] * (Data.FFCYC + Data.FZCYC + Data.OUTCYC)

				HTCNEF = 0.024 * HTCNEF

				if(IDFRST == 1):
					DFRSTF = 0.0
				else:
					DFRSTF = 0.024 * ((FROSTF + FROSTZ) / 3.413) + 0.024 * Data.DFSTCYC / 3.413
				# End IF

				TOTEF  = POWERF + FANEF + ELECF + HTCNEF + DFRSTF
				#
				#           OUTPUT THE ELECTRICAL BUDGET
				#
				objEraDat.write_or_terminate ('REFRIGERATION CYCLE ' )
				objEraDat.write_or_terminate ('EVAP LOAD (W)              ,%7.3f' %( Data.QEN[1] / 3.6 ) )
				objEraDat.write_or_terminate ('NET CAPACITY (W)           ,%7.3f' %( QEL[1] / 3.6 ) )
				
				objEraDat.write_or_terminate ('MASS FLOW (KG / HR)        ,%7.3f' %( Data.FLOWN[1] / 2.2046 ) )
				objEraDat.write_or_terminate ('COP - STEADY STATE         ,%7.3f' %( Data.COPRN[1] ) )
				objEraDat.write_or_terminate ('COP - CYCLING              ,%7.3f' %( Data.COPRN[1]  * Data.COPCYC[1] ) )
				
				objEraDat.write_or_terminate ('DUTY CYCLE                 ,%7.3f' %( DUTYN[1] ) ) 
				objEraDat.write_or_terminate ('COMPRESSOR (KWH / DAY)     ,%7.3f' %( POWERF   ) )
				objEraDat.write_or_terminate ('FANS (KWH / DAY)           ,%7.3f' %( FANEF    ) )
				
				objEraDat.write_or_terminate ('ELECTRONICS (KWH / DAY)        ,%7.3f' %( ELECF ) )
				objEraDat.write_or_terminate ('HEATERS & CONTROLS (KWH / DAY)),%7.3f' %( HTCNEF) )
				
				objEraDat.write_or_terminate ('DEFROST (KWH / DAY)            ,%7.3f' %( DFRSTF) )
				objEraDat.write_or_terminate ('TOTAL ENERGY USE (KWH / DAY)   ,%7.3f' %( TOTEF ) )
				
			# End IF ICYCL == 1
			#
			#           LORENZ cycle
			#
			if(ICYCL == 2  and  IFAN != 2)  :
				POWERF = 0.024 * Data.DUTYL * Data.PWRL / 3.413
				FANEF  = 0.024 * Data.DUTYL * (FANEL + FANCL)
				ELECF  = 0.024 * Data.DUTYL * ELOSS[1]
				HTCNEF = FFASH + Data.FFAUX + Data.OTHERW / 2.0 + Data.DUTYL * Data.FFCYC

				POWERZ = 0.024 * Data.DUTYS * Data.PWRE / 3.413
				FANEZ  = 0.024 * Data.DUTYS * (FANES + FANCS)
				ELECZ  = 0.024 * Data.DUTYS * ELOSS[1]
				HTCNEZ = Data.FZASH + Data.FZAUX + Data.OTHERW / 2.0 + Data.DUTYL * Data.FZCYC

				if(IDFRST == 1)  :
					DFRSTZ = 0.0
				else:
					DFRSTZ = 0.024 * (FROSTZ + Data.DFSTCYC) / 3.413
				# End IF

				if(Data.INCTRL  <  4)  :
					HTCNEF = HTCNEF + HTCNEZ + Data.DUTYL * Data.OUTCYC
					FANEF = FANEF + 0.024 * Data.DUTYL * Data.FANZ
					DFRSTF = DFRSTZ
				# End IF

				HTCNEF = 0.024 * HTCNEF
				HTCNEZ = 0.024 * HTCNEZ
				TOTEF  = POWERF + FANEF + ELECF + HTCNEF + DFRSTF
				TOTEZ  = POWERZ + FANEZ + ELECZ + HTCNEZ + DFRSTZ

				#           SUM THE ELECTRICAL BUDGET (SWITCHING VALVE CONTROL)
				#
				DUTYT  = Data.DUTYL  + Data.DUTYS
				POWERT = POWERF + POWERZ
				FANET  = FANEF  + FANEZ
				ELECT  = ELECF  + ELECZ
				HTCNET = HTCNEF + HTCNEZ
				DFRSTT = DFRSTF + DFRSTZ
				TOTET  = TOTEF  + TOTEZ

				if(IFAN == 0)  :#804
					objEraDat.write_or_terminate ('REFRIGERATION CYCLE ' )
					objEraDat.write_or_terminate ('EVAP LOAD (W)              ,%7.3f' %( Data.QEN[1] / 3.6 ) )
					objEraDat.write_or_terminate ('NET CAPACITY (W)           ,%7.3f' %( QEL[1] / 3.6 ) )
					
					objEraDat.write_or_terminate ('MASS FLOW (KG / HR)        ,%7.3f' %( Data.FLOWN[1] / 2.2046 ) )
					objEraDat.write_or_terminate ('COP - STEADY STATE         ,%7.3f' %( Data.COPRN[1] ) )
					objEraDat.write_or_terminate ('COP - CYCLING              ,%7.3f' %( Data.COPRN[1]  * Data.COPCYC[1] ) )
					
					objEraDat.write_or_terminate ('DUTY CYCLE                 ,%7.3f' %( DUTYN[1] ) ) 
					objEraDat.write_or_terminate ('COMPRESSOR (KWH / DAY)     ,%7.3f' %( POWERF   ) )
					objEraDat.write_or_terminate ('FANS (KWH / DAY)           ,%7.3f' %( FANEF    ) )
					
					objEraDat.write_or_terminate ('ELECTRONICS (KWH / DAY)        ,%7.3f' %( ELECF ) )
					objEraDat.write_or_terminate ('HEATERS & CONTROLS (KWH / DAY)),%7.3f' %( HTCNEF) )
					
					objEraDat.write_or_terminate ('DEFROST (KWH / DAY)            ,%7.3f' %( DFRSTF) )
					objEraDat.write_or_terminate ('TOTAL ENERGY USE (KWH / DAY)   ,%7.3f' %( TOTEF ) )
				
				# End IF   IFAN=0

				if(IFAN == 1)  :
					if (DUTYT  >  1.0): 
						objEraDat.write_or_terminate('WARNING: TOTAL DUTY CYCLE IS GREATER THAN 1\n' )
						
					DUTMAX = max(Data.DUTYL, Data.DUTYS)
					HTCNEF = HTCNEF + 0.024 * 0.5 * DUTMAX * Data.OUTCYC
					HTCNEZ = HTCNEZ + 0.024 * 0.5 * DUTMAX * Data.OUTCYC
					HTCNET = HTCNET + 0.024 * 1.0 * DUTMAX * Data.OUTCYC
					TOTEF = TOTEF + 0.024 * 0.5 * DUTMAX * Data.OUTCYC
					TOTEZ = TOTEZ + 0.024 * 0.5 * DUTMAX * Data.OUTCYC
					TOTET = TOTEF + TOTEZ
					# 805
					objEraDat.write_or_terminate ( 'REFRIGERATION CYCLE '  )
					objEraDat.write_or_terminate ( 'FRESH FOOD, FREEZER, TOTAL '  )
					
					objEraDat.write_or_terminate ('EVAP LOAD (W)   ,%7.3f, %7.3f' %( Data.QEN[1] / 3.6,   Data.QEN[2] / 3.6 ) )
					objEraDat.write_or_terminate ('NET CAPACITY (W)   ,%7.3f, %7.3f' %( QEL[1] / 3.6,   QEL[2] / 3.6 ) )
					objEraDat.write_or_terminate ('MASS FLOW (KG / HR)   ,%7.3f, %7.3f' %( Data.FLOWN[1] / 2.2046,Data.FLOWN[2] / 2.2046 ) )
					
					objEraDat.write_or_terminate ('COP - STEADY STATE   ,%7.3f, %7.3f' %( Data.COPRN[1], Data.COPRN[2] ) )
					objEraDat.write_or_terminate ('COP - CYCLING   ,%7.3f, %7.3f' %( Data.COPRN[2] * Data.COPCYC[2] ) )
					
					objEraDat.write_or_terminate ('DUTY CYCLE   ,%7.3f, %7.3f, %7.3f' %( Data.DUTYL,    Data.DUTYS,  DUTYT ) )
					objEraDat.write_or_terminate ('COMPRESSOR (KWH / DAY)  ,%7.3f, %7.3f, %7.3f' %( POWERF,   POWERZ, POWERT ) )
					objEraDat.write_or_terminate ('FANS (KWH / DAY) ,%7.3f, %7.3f, %7.3f' %( FANEF,    FANEZ,  Data.FANE ) )
					objEraDat.write_or_terminate ('ELECTRONICS (KWH / DAY) ,%7.3f, %7.3f, %7.3f' %( ELECF,    ELECZ,  ELECT ) )
					
					objEraDat.write_or_terminate ('EATERS & CONTROLS (KWH / DAY) ,%7.3f, %7.3f , %7.3f' %( HTCNEF,   HTCNEZ, HTCNET ) )
					objEraDat.write_or_terminate ('DEFROST (KWH / DAY) ,%7.3f, %7.3f, %7.3f' %( DFRSTF,   DFRSTZ, DFRSTT ) )
					objEraDat.write_or_terminate ('TOTAL ENERGY USE (KWH / DAY) ,%7.3f, %7.3f, %7.3f' %( TOTEF,    TOTEZ,  TOTET ) )
					

				# End IF IFAN=1
			# End IF ICYCL == 2  and  IFAN != 2

			#           REFRIGERANT BYPASS cycle
			if(IFAN == 2)  :
				POWERF = 0.024 * Data.DUTYL * Data.PWRL / 3.413
				FANEF  = 0.024 * Data.DUTYL * (FANEL + FANCL + Data.FANZ)
				ELECF  = 0.024 * Data.DUTYL * ELOSS[1]

				POWERZ = 0.024 * Data.DUTYS * Data.PWRE / 3.413
				FANEZ  = 0.024 * Data.DUTYS * (FANES + FANCS)
				ELECZ  = 0.024 * Data.DUTYS * ELOSS[1]

				HTCNEF = FFASH + Data.FFAUX + Data.OTHERW + Data.DUTYC * Data.OUTCYC + Data.FZASH + Data.FZAUX

				DFRSTF = 0.0
				DFRSTZ = 0.0

				HTCNEF = 0.024 * HTCNEF
				HTCNEZ = 0.0

				TOTEF  = POWERF + FANEF + ELECF + HTCNEF + DFRSTF
				TOTEZ  = POWERZ + FANEZ + ELECZ + HTCNEZ + DFRSTZ
				#
				#           SUM THE ELECTRICAL REFRIGERANT BYPASS CONTROL)
				#
				DUTYT  = Data.DUTYL  + Data.DUTYS
				POWERT = POWERF + POWERZ
				FANET  = FANEF  + FANEZ
				ELECT  = ELECF  + ELECZ
				HTCNET = HTCNEF + HTCNEZ
				DFRSTT = DFRSTF + DFRSTZ
				TOTET  = TOTEF  + TOTEZ

				if(ICYCLS == 2  and  Data.IEVAP == 1):
					#WRITE(IO,808)
					objEraDat.write_or_terminate('REFRIGERATION CYCLE ')
					objEraDat.write_or_terminate(' ,LORENZ, FRESH FOOD, TOTAL')


				if(ICYCLS == 2  and  Data.IEVAP == 2):
					#WRITE(IO,806)
					objEraDat.write_or_terminate('REFRIGERATION CYCLE ')
					objEraDat.write_or_terminate(' ,LORENZ, FREEZER, TOTAL')

				if(ICYCLS == 4  and  Data.IEVAP == 1):
					#WRITE(IO,809)
					objEraDat.write_or_terminate('REFRIGERATION CYCLE ')
					objEraDat.write_or_terminate(' ,DUAL EVAP, FRESH FOOD, TOTAL')					

				if(ICYCLS == 4  and  Data.IEVAP == 2):
					#WRITE(IO,807)
					objEraDat.write_or_terminate('REFRIGERATION CYCLE ')
					objEraDat.write_or_terminate(' ,DUAL EVAP, FREEZER, TOTAL')					

				if (DUTYT  >  1.0):
					objEraDat.write_or_terminate('WARNING: TOTAL DUTY CYCLE IS GREATER THAN 1\n' )

				#815
				objEraDat.write_or_terminate ('EVAP LOAD (W)      ,%7.3f, %7.3f' %(Data.QEN[1] / 3.6,   Data.QEN[2] / 3.6 ) )
				objEraDat.write_or_terminate ('NET CAPACITY (W)   ,%7.3f, %7.3f' %(QEL[1] / 3.6,   QEL[2] / 3.6 ) )
				objEraDat.write_or_terminate ('MASS FLOW (KG / HR),%7.3f, %7.3f' %(Data.FLOWN[1] / 2.2046,Data.FLOWN[2] / 2.2046 ) )

				objEraDat.write_or_terminate ('COP - STEADY STATE   ,%7.3f, %7.3f' %( Data.COPRN[1], Data.COPRN[2], ) )
				objEraDat.write_or_terminate ('COP - CYCLING        ,%7.3f, %7.3f' %( Data.COPRN[1] * Data.COPCYC[1]  ) )
				objEraDat.write_or_terminate ('                     ,%7.3f, %7.3f' %( Data.COPRN[2] * Data.COPCYC[2]  ) )
				
				objEraDat.write_or_terminate ('DUTY CYCLE           ,%7.3f, %7.3f,%7.3f' %( Data.DUTYL,  Data.DUTYS,  DUTYT ) )
				objEraDat.write_or_terminate ('COMPRESSOR (KWH / DAY)    ,%7.3f, %7.3f,%7.3f' %( POWERF,   POWERZ, POWERT ) )
				objEraDat.write_or_terminate ('FANS (KWH / DAY)          ,%7.3f, %7.3f,%7.3f' %( FANEF,    FANEZ,  FANET ) )
				objEraDat.write_or_terminate ('ELECTRONICS (KWH / DAY)   ,%7.3f, %7.3f,%7.3f' %( ELECF,    ELECZ,  ELECT  ) )
				
				objEraDat.write_or_terminate ('HEATERS & CONTROLS (KWH / DAY)),%7.3f' %( HTCNEF,   HTCNEZ, HTCNET) )
				objEraDat.write_or_terminate ('DEFROST (KWH / DAY)            ,%7.3f' %( HTCNEF,   HTCNEZ, HTCNET) )
				objEraDat.write_or_terminate ('TOTAL ENERGY USE (KWH / DAY)   ,%7.3f' %( TOTEF,    TOTEZ,  TOTET ) )

				# End IF (DUTYT  >  1.0) 

				#           DUAL LOOP cycle
				#
				if(ICYCL == 3)  :
					POWERZ = 0.024 * (DUTYN[1] * WCOMP[1] / 3.413)
					FANEZ  = 0.024 * (DUTYN[1] * (FNPWRE[1] + FNPWRC[1]))
					ELECZ  = 0.024 * (DUTYN[1] * ELOSS[1])

					HTCNEZ = Data.FZASH + Data.FZAUX + DUTYN[1] * Data.FZCYC + Data.OTHERW / 2

					if(DUTYN[1]  >=  DUTYN[2])  :
						HTCNEZ = HTCNEZ + 0.5 * DUTYN[1] * Data.OUTCYC
					else:
						HTCNEZ = HTCNEZ + 0.5 * DUTYN[2] * Data.OUTCYC
					# End IF

					HTCNEZ = 0.024 * HTCNEZ
					if(IDFRST == 1)  :
						DFRSTZ = 0.0
					else:
						DFRSTZ = 0.024 * (FROSTZ / 3.413 + Data.DFSTCYC / 3.413)
					# End IF

					TOTEZ  = POWERZ + FANEZ + ELECZ + HTCNEZ + DFRSTZ
					POWERF = 0.024 * (DUTYN[2] * WCOMP[2] / 3.413)
					FANEF  = 0.024 * (DUTYN[2] * (FNPWRE[2] + FNPWRC[2]))
					ELECF  = 0.024 * (DUTYN[2] * ELOSS[2])

					HTCNEF = FFASH + Data.FFAUX + DUTYN[2] * Data.FFCYC + Data.OTHERW / 2
					if(DUTYN[1]  >=  DUTYN[2])  :
						HTCNEF = HTCNEF + 0.5 * DUTYN[1] * Data.OUTCYC
					else:
						HTCNEF = HTCNEF + 0.5 * DUTYN[2] * Data.OUTCYC
					# End IF
					HTCNEF = 0.024 * HTCNEF

					DFRSTF = 0.0
					TOTEF  = POWERF + FANEF + ELECF + HTCNEF + DFRSTF
					#
					#           OUTPUT THE ELECTRICAL ENERGY ON Data.A DAILY BASIS
					#
					POWERT = POWERF + POWERZ
					FANET  = FANEF  + FANEZ
					ELECT  = ELECF  + ELECZ
					HTCNET = HTCNEF + HTCNEZ
					DFRSTT = DFRSTF + DFRSTZ
					TOTET  = TOTEF  + TOTEZ
					
					objEraDat.write_or_terminate ('REFRIGERATION CYCLE')
					objEraDat.write_or_terminate ('FRESH FOOD,FREEZER,TOTAL')

					objEraDat.write_or_terminate ('EVAP LOAD (W)      ,%7.3f, %7.3f,%7.3f' %(QEL[1] / 3.6,  Data.FLOWN[2] / 2.2046, Data.FLOWN[1] / 2.2046 ) )
					objEraDat.write_or_terminate ('NET CAPACITY (W)   ,%7.3f, %7.3f,%7.3f' %(Data.COPRN[2], Data.COPRN[1],   Data.COPRN[2] * Data.COPCYC[2] ) )
					objEraDat.write_or_terminate ('MASS FLOW (KG / HR),%7.3f, %7.3f,%7.3f' %(Data.COPRN[1] * Data.COPCYC[1],   DUTYN[2], DUTYN[1] ) )

					objEraDat.write_or_terminate ('COP - STEADY STATE   ,%7.3f, %7.3f,%7.3f' %( POWERF, POWERZ, POWERT ) )
					objEraDat.write_or_terminate ('COP - CYCLING        ,%7.3f, %7.3f,%7.3f' %( FANEF,  FANEZ,  FANET  ) )
					objEraDat.write_or_terminate ('DUTY CYCLE           ,%7.3f, %7.3f,%7.3f' %( ELECF,  ELECZ,  ELECT  ) )
					
					objEraDat.write_or_terminate ('COMPRESSOR (KWH / DAY)    ,%7.3f, %7.3f,%7.3f' %( HTCNEF, HTCNEZ, HTCNET ) )
					objEraDat.write_or_terminate ('FANS (KWH / DAY)          ,%7.3f, %7.3f,%7.3f' %( DFRSTF, DFRSTZ, DFRSTT ) )
					objEraDat.write_or_terminate ('ELECTRONICS (KWH / DAY)   ,%7.3f, %7.3f,%7.3f' %( TOTEF,  TOTEZ,  TOTET  ) )
					
					objEraDat.write_or_terminate ('HEATERS & CONTROLS (KWH / DAY)'  )
					
					
				# End IF (ICYCL == 3)
			else:
				#      FREEZERS
				#
				#CASE DEFAULT
				dhrs = 0.024  *  (24.0 - Data.HRSOFF) / 24.0
				POWERZ = dhrs * (DUTYN[1] * WCOMP[1] / 3.413)
				FANEZ  = dhrs * (DUTYN[1] * (FNPWRE[1] + FNPWRC[1]))
				ELECZ  = dhrs * (DUTYN[1] * ELOSS[1])
				HTCNEZ = Data.FZASH + Data.FZAUX + DUTYN[1] * (Data.FZCYC + Data.OUTCYC) + Data.OTHERW
				HTCNEZ = dhrs * HTCNEZ

				DFRSTZ = dhrs * (FROSTZ / 3.413 + Data.DFSTCYC / 3.413)
				if(IDFRST == 1): DFRSTZ = 0.0
				TOTEZ  = POWERZ + FANEZ + ELECZ + HTCNEZ + DFRSTZ
				#
				#      OUTPUT THE ELECTRICAL BUDGET
				# 804
				objEraDat.write_or_terminate ('REFRIGERATION CYCLE ' )
				objEraDat.write_or_terminate ('EVAP LOAD (W)              ,%7.3f' %( Data.QEN[1] / 3.6 ) )
				objEraDat.write_or_terminate ('NET CAPACITY (W)           ,%7.3f' %( QEL[1] / 3.6 ) )
				
				objEraDat.write_or_terminate ('MASS FLOW (KG / HR)        ,%7.3f' %( Data.FLOWN[1] / 2.2046 ) )
				objEraDat.write_or_terminate ('COP - STEADY STATE         ,%7.3f' %( Data.COPRN[1] ) )
				objEraDat.write_or_terminate ('COP - CYCLING              ,%7.3f' %( Data.COPRN[1]  * Data.COPCYC[1] ) )
				
				objEraDat.write_or_terminate ('DUTY CYCLE                 ,%7.3f' %( DUTYN[1] ) ) 
				objEraDat.write_or_terminate ('COMPRESSOR (KWH / DAY)     ,%7.3f' %( POWERZ   ) )
				objEraDat.write_or_terminate ('FANS (KWH / DAY)           ,%7.3f' %( FANEZ    ) )
				
				objEraDat.write_or_terminate ('ELECTRONICS (KWH / DAY)        ,%7.3f' %( ELECZ ) )
				objEraDat.write_or_terminate ('HEATERS & CONTROLS (KWH / DAY)),%7.3f' %( HTCNEZ) )
				
				objEraDat.write_or_terminate ('DEFROST (KWH / DAY)            ,%7.3f' %( DFRSTZ) )
				objEraDat.write_or_terminate ('TOTAL ENERGY USE (KWH / DAY)   ,%7.3f' %( TOTEZ ) )

		#  END IF i.e. SELECT
		#CLOSE(IO)
		return
	
			
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
#=======================================================
def main(): 
	obj_Eravyv = Eravyv()
	obj_Eravyv.start()

if __name__ == '__main__':
	main()
	print ("\n\n\n")
	print ("======================================")
	print ("    Applocation Created output File   ")
	print ("======================================")

