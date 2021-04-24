# Python Import


# User Import
from common_classes.DataModelBuiler import DataModelBuiler

# -----------------------------------------------------------
# Job 			: Creates object with data from file for Q class
#
# Editor		: aymhenry@gmail.com
# -----------------------------------------------------------


class CycleDataModelBuiler (DataModelBuiler):
    # Type 2 is not supported
    # required data for each configration
    DataModelBuiler.lst_required_data = [98 - 18]
    # maximum data file lines to read, this need to be updated if there is any
    # data line more than this
    DataModelBuiler.MAX_DATA_FILE_TO_READ = 150

    DataModelBuiler.CONFIGRATION_COUNT = 1   # max. number of configrations
    # +1 			# set row number that has the configration
    DataModelBuiler.CONFIGRATION_ROW = 5   # 4 sting lines found

    # -----------------------------------------------------------
    # Job 			: Assign values from table to selected class vars
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
    def assign_vars(self):
        lst_config1 = [  # 'TITLE','TITLE2','FILERA','FILE_NAME'
            # 'ICYCL',
            'IRFTYP',
            'IDFRST',
            # 'HRSOFF',  
            # 'TOL_FRSH',
            # 'TOL_FRZ',
            # 'TOL_COND',
            # 'TOL_MASS', 
            # 'TOL_HX',
            'N_EVAP', 
            'N_COND', 
            'ICOMP',
            # 'IMAP', 
            # 'I_CYCLE',
            'T_CYCLE', 
            'I_VALVE',
            'FILE_NAME_code',
            'REF', 
            # 'IR[2][1]',
            # 'IR[3][1]', 
            # 'NC[1]', 
            # 'F[1][2][1]',
            # 'F[1][3][1]', 
            # 'F[2][3][1]', 
            # 'X[1][1]', 
            # 'X[2][1]', 
            # 'X[3][1]', 
            'ICONDI[1]', 
            'TS1[1]', 
            'CFMCI[1]', 
            'FNPWRC[1]', 
            'DPC[1]', 
            'UDSCI[1]', 
            'UTPCI[1]',
            'USCCI[1]',
            'ATOTCI[1]',
            'DTSBCI[1]',
            'CONDHT[1]', 
            'CONDVP[1]',
            'ISPECI[1]', 
            'IFRSHI[1]', 
            'TS3[1]',
            'CFMEI[1]', 
            'FNPWRE[1]', 
            'DPE[1]', 
            'UTPEI[1]', 
            'USUPEI[1]',
            'ATOTEI[1]', 
            'DTSPEI[1]', 
            'QUALITY[1]', 
            'MREFI[1]', 
            'SPEEDI[1]', 
            'TSPECI[1]', 
            #  'DISPLC[1]',
            #  'SIZEN[1]',
            #  'SPDNOM[1]',
            #  'EERN[1]',
            #  'ICOOLN[1]',
            #  'CEI[1]',
            #  'SEFFI[1]',
            #  'MEFF[1]',
            #  'ELOSS[1]',
            #  'QCAN[1]',
            #  'QHILO[1]',
            'SUPIHX[1]', 
            'ETHX[1]',
            'UA_FF', 
            'UA_FZ', 
            'UA_ML', 
            'UA_FF_CND', 
            'UA_FZ_CND', 
            'UA_FF_HXS',
            'UA_FZ_HXS', 
            'FRACT_FF', 
            'FRACT_FZ', 
            'IWALL_FF', 
            'IWALL_FZ',
            'DFSTCYC',
            'FFCYC', 
            'FZCYC',
            'OUTCYC',
            'ICAB', 
            'FFASH', 
            'FFAUX', 
            'FZASH',
            'FZAUX', 
            'OTHERW', 
            'TROOM', 
            'FFTEMP', 
            'FZTEMP',
            'FFQ', 
            'FZQOFF', 
            'FFSEN', 
            'FFLAT', 
            'FROSTF',
            'FZSEN', 
            'FZLAT',
            'FROSTZ', 
            'FFPENA', 
            'FZPENA',
            'FFHTQ', 
            'FZHTQ', 
            'FFREFQ',
            'FZREFQ',
            'QMUL']

        # lst_config4 same as 2

        lst_var_names = []

        # creates blank array to put the basic inputs
        self.setup_array()

        # according to the given configration in data file, saved in int_configration
        # set the realtive list of variable name in list

        lst_var_names = lst_config1

        # send data list of variable, and relative list of values to data object.
        # data object put values of data list on variable
        self.obj_qdata.setup_vars(
            self.lst_data,
            lst_var_names,
            self.CONFIGRATION_ROW,
            self.int_parameter_count)

        # create a list with items title1 ,2 .. up to CONFIGRATION_ROW
        # lst_title = ['title'+str(n) for n in range(0, self.CONFIGRATION_ROW)]
        lst_title = ['TITLE', 'TITLE2', 'FILERA', 'FILE_NAME']

        # create vars title1 up to CONFIGRATION_ROW, with the given values in
        # list
        self.obj_qdata.setup_vars(
            self.lst_data,
            lst_title,
            1,
            self.CONFIGRATION_ROW - 1,
            True)

    # -----------------------------------------------------------
    # Job 			: creates blank array to put the basic inputs
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
    def setup_array(self):
        # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]

        self.obj_qdata.ICONDI = [0.0] * (2 + 1)
        self.obj_qdata.TS1 = [0.0] * (2 + 1)
        self.obj_qdata.CFMCI = [0.0] * (2 + 1)
        self.obj_qdata.FNPWRC = [0.0] * (2 + 1)
        self.obj_qdata.DPC = [0.0] * (2 + 1)
        self.obj_qdata.UDSCI = [0.0] * (2 + 1)
        self.obj_qdata.UTPCI = [0.0] * (2 + 1)
        self.obj_qdata.USCCI = [0.0] * (2 + 1)
        self.obj_qdata.ATOTCI = [0.0] * (2 + 1)
        self.obj_qdata.DTSBCI = [0.0] * (2 + 1)
        self.obj_qdata.CONDHT = [0.0] * (2 + 1)
        self.obj_qdata.CONDVP = [0.0] * (2 + 1)
        self.obj_qdata.ISPECI = [0.0] * (2 + 1)
        self.obj_qdata.IFRSHI = [0.0] * (2 + 1)
        self.obj_qdata.TS3 = [0.0] * (2 + 1)
        self.obj_qdata.CFMEI = [0.0] * (2 + 1)
        self.obj_qdata.FNPWRE = [0.0] * (2 + 1)
        self.obj_qdata.DPE = [0.0] * (2 + 1)
        self.obj_qdata.UTPEI = [0.0] * (2 + 1)
        self.obj_qdata.USUPEI = [0.0] * (2 + 1)
        self.obj_qdata.ATOTEI = [0.0] * (2 + 1)
        self.obj_qdata.DTSPEI = [0.0] * (2 + 1)
        self.obj_qdata.QUALITY = [0.0] * (2 + 1)
        self.obj_qdata.MREFI = [0.0] * (2 + 1)
        self.obj_qdata.SPEEDI = [0.0] * (2 + 1)
        self.obj_qdata.TSPECI = [0.0] * (2 + 1)
        self.obj_qdata.SUPIHX = [0.0] * (2 + 1)
        self.obj_qdata.ETHX = [0.0] * (2 + 1)

    # Control Variables definitions
    # =====================
    # INCTRL: 	0 = none,
    #            1 = adjust evaporator areas,
    #            2 = adjust fresh food section tempeature,
    #            3 = adjust freezer    section tempeature,
    # 			4 = switching valve (only one section is cooled  at a time)
    #            5 = solenoid valve or fan control provides evaporator capacity to only one cabinet
    #            	during part of the cycle

    # ICYCL		1 =	Standard

    # ICOMP		1 = Reciprocating
    #            2 = Rotary

    # ICONDI or	0 =	Natural Convection
    # ISPECI	 1 =	Cross-Flow
    #            2 =	Counter-Flow

    # ISPECI	1 =	Evap Exit Superheat
    # 			2 =	Interchanger Exit Superheat
    # 			3 =	Evap Exit Quality

    # ICOOLN	0 =	Static
    # 			1 =	Fan-Forced

    # FILMAP1_CODE and FILMAP2_CODE
    # 1  = 1ABB_EMX70HSC
    # 2  = 2BADBAD
    # 3  = 3DG57C84TAU6
    # 4  = 4DG73C12
    # 5  = 5DG73C12RAU6
    # 6  = 6DGH66C94
    # 7  = 7EGX90HLC
    # 8  = 8EGZ100HLP
    # 9  = 9EMBRACO MODEL
    # 10  = 10EMBRACO_NT6215Z
    # 11  = 11EMU30HSC
    # 12  = 12EMX70HSC
    # 13  = 13EMY60HER
    # 14  = 14FILES.TXT
    # 15  = 15GVT44AD
    # 16  = 16GVY44AD
    # 17  = 17SF51C97
    # 18  = 18SF51NEW
    # 19  = 19SMOOTHED
    # 20  = 20SP51C97
    # 21  = 21TESTMAP
    # 22  = 22TSA1374YAS
    # 23  = 23TTE46FK

    # Variables definitions
    # =====================
    # File informaton
        # FILERA	FILE NAME

    # Cabinet and cycle definitions
        # ICYCL		Cycle Type
        # IRFTYP	Refrigeration Type

    # Manual defrost control
        # IDFRST	Manual Defrost
        # HRSOFF	Hours Shutdown For Cycle Defrost

    # Calculation tolerance
        #  TOL_COND		Torelance in iteration for condenser
        #  TOL_MASS 	Torelance in iteration for mass
        #  TOL_HX		Torelance in iteration for heat exchanger

    # Evaporator and Condenser Zones
        #  N_EVAP		Number of Zones on Evaporator
        #  N_COND		Number of Zones on Condenser

    # Comprssor Options
        #  ICOMP		Compressor Type
        #  IMAP		Compressor Analysis
        #  I_CYCLE		Cycling Loss Analysis
        #  T_CYCLE		Cycles Per Hour
        #  I_VALVE		Shut-Off Valve

    # Condenser data
        #  ICONDI[1]	Heat Exchanger Configuration
        #  TS1[1]		Temp Of Air Entering Condenser (C)
        #  CFMCI[1]		Air Flow Rate Across Coil (L/S)
        #  FNPWRC[1]	Fan Power (W)

        #  DPC[1]		Pressure Drop Through Condenser (kpa)
        #  UDSCI[1]		Desuperheating Heat Transfer Conductance, W/M2-C
        #  UTPCI[1]		Two-Phase Heat Transfer Conductance, W/M2-C
        #  USCCI[1]		Subcooling Heat Transfer Conductance, W/M2-C
        #  ATOTCI[1]	Total Heat Transfer Surface Area, M2
        #  DTSBCI[1]	Refrigerant Exit Subcooling, Deg C
        #  CONDHT[1]	Liquid-Line Anti-Sweat Heat (W)
        #  CONDVP[1]	Vapor-Line Anti-Sweat Heat (W)

    # Fresh Food section data
        #  ISPECI[1]	Evaporator Specification
        #  IFRSHI[1]	Heat Exchanger Configuration
        #  TS3[1]		Temp Of Air Entering Fresh Food Section Evaporator (C)
        #  CFMEI[1]		Air Flow Rate Across Coil (L/S)
        #  FNPWRE[1]	Fan Power (W)
        #  DPE[1]		Pressure Drop Through Fresh Food Evaporator (Kpa)
        #  UTPEI[1]		Two-Phase Heat Transfer Conductance, W/M2-C
        #  USUPEI[1]	Superheat Region Conductance, W/M2-C
        #  ATOTEI[1]	Total Heat Transfer Surface Area, M2
        #  DTSPEI[1]	Refrigerant Exit Superheat (C)
        #  QUALITY[1]	Quality (0-1)

    # Compressor data
        #  MREFI[1]		Initial Guess For Refrigerant Mas Flow Rate (kg/hr)
        #  SPEEDI[1]	Nominal Speed (rpm)
        #  TSPECI[1]	Temp. At Comp., Inlet (C) [-1 If Unspecified]
        # inside the compressor shell to suction gas

    # Interchanger data
        #  SUPIHX[1]	Interchanger exit superheat
        #  ETHX[1]		Effectiveness Of High Temp Interchanger

    # In-wall evaporator data
        #  UA_FF		Evap: A/R In Fresh Food Section (Or Cabinet Walls)
        #  UA_FZ		Evap: A/R In Freezer Section Walls (If Separate Section)
        #  UA_ML		Evap: A/R In Mullion Section (If Present)
        #  UA_FF_CND	Cond: A/R In Fresh Food Section (Or Cabinet Walls)
        #  UA_FZ_CND	Cond: A/R In Freezer Section Walls (If Separate Section)
        #  UA_FF_HXS	Both: A/R In Fresh Food Section (Or Cabinet Walls)
        #  UA_FZ_HXS	Both: A/R In Freezer Section Walls (If Separate Section)
        #  FRACT_FF		Fraction Of Fresh Food Section (Or Cabinet Walls)
        #  FRACT_FZ		Fraction Of Freezer Section Walls (Including Mullion)
        #  IWALL_FF		FF (Or Cabinet) Evaporator Behind Liner
        #  IWALL_FZ		FZ Evaporator Behind Liner
        #  DFSTCYC		Closed-Door Automatic Defrost (W)

        #  FFCYC		Fresh Food Section
        #  FZCYC		Freezer Section
        #  OUTCYC		Outside Cabinet

        #  FFASH		Fresh Food Antisweat Heater (W)
        #  FFAUX		Fresh Food Auxiliary Power (W)
        #  FZASH		Freezer Antisweat Heater (W)
        #  FZAUX		Freezer Auxiliary Power (W)
        #  OTHERW		Other heat
        #  TROOM		Room Temperature (C)
        #  FFTEMP		Fresh Food Temperature (C)
        #  FZTEMP		Freezer Temperature (C)
        #  FFQ			Fresh Food Net Load (W)
        #  FZQOFF		Freezer Load (W)
        #  FFSEN		Fresh Food Door Sensible Load (W)
        #  FFLAT		Fresh Food Door Condensation (W)
        #  FROSTF		Fresh Food Door Frost Load (W)
        #  FZSEN		Freezer Door Sensible Load (W)
        #  FZLAT		Freezer Door Condensation Load (W)
        #  FROSTZ		Freezer Door Frost Load (W)
        #  FFPENA		Fresh Food Penetrations (W)
        #  FZPENA		Freezer Penetrations (W)
        #  FFHTQ		Fresh Food Heaters And Controls (W)
        #  FZHTQ		Freezer Heaters And Controls (W)
        #  FFREFQ		Fresh Food Refrigerant Line (W)
        #  FZREFQ		Freezer Refrigerant Line (W)
        #  QMUL			Mullion Heat Load (W)
