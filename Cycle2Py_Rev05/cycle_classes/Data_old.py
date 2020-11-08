# Python Import ==================
#import math,sys

# User Import
from .Block2 import BData


class Data (BData):

    # =======================================================
    # Set Static Vars
    # =======================================================

    # -- Common NCOMP group ---------------------------------
    # NC = 0	# integer

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

    CONDHT = [0.0] * (2 + 1)
    CONDVP = [0.0] * (2 + 1)

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
    TOL_FRSH = 0.1  # data given from Eracyc line 95
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
    # G:\Ayman_APP\Fortran\Source\Cycle\Eracyc.for
    # Line 45:       COMMON /REFRIG/ NC(2), IR(5,2), X(5,2), F(5,5,2)
    # Line 1085:       COMMON /REFRIG/ NC(2), IR(5,2), X(5,2), F(5,5,2)

    # F(5,5,2)
    # Canceled shared with local group REFRIG
    #F =  [  [[0.0] * (2+1) for i in range(5+1)] for j in range(5+1)  ]

    pythNC = [0.0] * (2 + 1)
    # X(5,2)
    # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
    X = [[0.0] * (2 + 1) for i in range(5 + 1)]

    # IR(5,2)
    # array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
    IR = [[0.0] * (2 + 1) for i in range(5 + 1)]
    # F(5,5,2)

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

    QEN = [0.0] * (2 + 1)
    FLOWN = [0.0] * (2 + 1)
    COPRN = [0.0] * (2 + 1)

    # --- GROUP CYCLNG ------
    CORR_COP = 0.0
    COPCYC = [0.0] * (2 + 1)

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
    # MDOTR = 0.0  use MREF
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

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
