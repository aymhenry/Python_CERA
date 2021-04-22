# Python import
import math
import sys
import datetime

# User import
from .Data import Data


class Adjlod (Data):
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def adjlod(self, ICYCL, IC, TS3, TS5, FROSTF, FROSTZ, IDFRST):
        # [p3, p4] = self.adjlod (all)
        #	SUBROUTINE ADJLOD(ICYCL,IC,TS3,TS5,FROSTF,FROSTZ,IDFRST)
        #
        # ADJUST THE CABINET LOADS AND SET POINT TEMPERATURES *

        # COMMON BLOCKS
        #
        #	REAL MREF
        #	no 		COMMON / PARMS / ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
        #	no 		COMMON / PARMS2 / TSPEC,TDROPC
        #	COMMON / HTEXS / CFMC,CFME,CFMF,Data.obj_cdata.UAF,ETAC,ETAE,ETAF

        #	COMMON / FEVAP / UTPE,USUPE,Data.obj_cdata.ATOTE
        #	no		COMMON / CONDEN / UDSC,UTPC,USCC,ATOTC,UACOND
        #	no		COMMON / SPECS / DTSUPE,DTSUBC
        #	no		COMMON  / SPECS2 /  ISPEC,XEXITE,DTSUPI
        #	COMMON / CABLOD / FFASH,FFAUX,FZASH,FZAUX,Data.obj_cdata.TROOM,Data.obj_cdata.FFTEMP,OTHERW,
        #	 .Data.obj_cdata.FZTEMP,Data.obj_cdata.FFQ,Data.obj_cdata.FZQON,Data.obj_cdata.FZQOFF,Data.obj_cdata.FFLAT,FZLAT,Data.obj_cdata.FFSEN,Data.obj_cdata.FZSEN,
        #	 .Data.obj_cdata.FFHTQ,Data.obj_cdata.FZHTQ,Data.obj_cdata.CONDF,Data.obj_cdata.CONDZ,Data.obj_cdata.QMUL
        #	COMMON / FANS / FANE,FANZ,FANC,DUTYC,W,COPR
        #	COMMON / LORENZ / Data.obj_cdata.DUTYE,Data.obj_cdata.DUTYZ,PWRL,PWRE,Data.obj_cdata.CAPE,Data.obj_cdata.CAPZ,DUTYL,DUTYS,
        #	 .FANEL,FANCL,FANES,FANCS
        #	COMMON / FIGURE / IEVAP
        #	COMMON  /  RESULT  /  QE, QZ, FLOW, QEN(2), FLOWN(2), COPRN(2)
        #	COMMON  /  CHINA  /  Data.obj_cdata.INCTRL
        #	COMMON  /  BALNCE  /  Data.obj_cdata.IBLNCE, BAFFLF, BAFFLZ, Data.obj_cdata.AREAFZ, Data.obj_cdata.ATOTE_S,
        #	 .Data.obj_cdata.AREAFZ_S, Data.obj_cdata.ATOTE_A, Data.obj_cdata.AREAFZ_A, Data.obj_cdata.FFTEMP_A, Data.obj_cdata.FZTEMP_A
        #	COMMON  /  INWALL  /  Data.obj_cdata.UA_FZ, Data.obj_cdata.UA_FF, Data.obj_cdata.UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
        #	 .Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
        #	 .CAPZ_IN_WALL, Data.obj_cdata.Q_FZ_FF
        #	COMMON  /  PENAT  /  Data.obj_cdata.FFPENA, Data.obj_cdata.FZPENA
        #
        # BRANCH ON THE VALUE IC.AVE VARIABLES AND INITIALIZE ON THE
        # FIRST CALL AND : WAIT UNTIL THE 4TH CALL TO MAKE ADJUSTMENTS
        #
        IRET = 0  # in Python only

        if IC == 1:
            # SELECT CASE (IC)
            #	 CASE [1]#Initialize
            Data.obj_cdata.IBLNCE = 0
            IRET = 0

            if(ICYCL != 2):
                IRET = 1

            FFTEMP_S = Data.obj_cdata.FFTEMP
            FZTEMP_S = Data.obj_cdata.FZTEMP
            FFQ_S = Data.obj_cdata.FFQ
            FZQON_S = Data.obj_cdata.FZQON
            FZQOFF_S = Data.obj_cdata.FZQOFF
            FFLAT_S = Data.obj_cdata.FFLAT
            FZLAT_S = Data.obj_cdata.FZLAT
            FFSEN_S = Data.obj_cdata.FFSEN
            FZSEN_S = Data.obj_cdata.FZSEN
            FFHTQ_S = Data.obj_cdata.FFHTQ
            FZHTQ_S = Data.obj_cdata.FZHTQ
            FROSTF_S = FROSTF
            FROSTZ_S = FROSTZ

            CONDF_S = Data.obj_cdata.CONDF
            CONDZ_S = Data.obj_cdata.CONDZ

            Data.obj_cdata.ATOTE_S = Data.obj_cdata.ATOTE
            Data.obj_cdata.AREAFZ_S = Data.obj_cdata.AREAFZ
            UAF_S = Data.obj_cdata.UAF
            Data.obj_cdata.ATOTE_A = Data.obj_cdata.ATOTE
            Data.obj_cdata.AREAFZ_A = Data.obj_cdata.AREAFZ

            UFF = (Data.obj_cdata.FFQ - Data.obj_cdata.FFLAT - Data.obj_cdata.FFPENA - Data.obj_cdata.FFHTQ -
                   FROSTF + Data.obj_cdata.QMUL) / (Data.obj_cdata.TROOM - Data.obj_cdata.FFTEMP)

            FZQ = Data.obj_cdata.FZQON
            FZQ_S = FZQ

            UFZ = (FZQ - Data.obj_cdata.FZLAT - Data.obj_cdata.FZPENA - Data.obj_cdata.FZHTQ -
                   FROSTZ - Data.obj_cdata.QMUL) / (Data.obj_cdata.TROOM - Data.obj_cdata.FZTEMP)

            UFF_SEN = FFSEN_S / (Data.obj_cdata.TROOM - FFTEMP_S)
            UFZ_SEN = FZSEN_S / (Data.obj_cdata.TROOM - FZTEMP_S)

            UCND_F = (Data.obj_cdata.CONDF + Data.obj_cdata.QMUL) / \
                (Data.obj_cdata.TROOM - FFTEMP_S)
            UCND_Z = (Data.obj_cdata.CONDZ - Data.obj_cdata.QMUL) / \
                (Data.obj_cdata.TROOM - FZTEMP_S)

            TS3_S = TS3
            TS5_S = TS5
            Data.obj_cdata.FFTEMP_A = Data.obj_cdata.FFTEMP
            Data.obj_cdata.FZTEMP_A = Data.obj_cdata.FZTEMP

            DELTS5_OLD = 0

            UA_FZ_S = Data.obj_cdata.UA_FZ
            UA_ML_S = Data.obj_cdata.UA_ML
            UA_FF_S = Data.obj_cdata.UA_FF

        elif IC in [2, 3]:
            pass

        else:
            # CASE DEFAULT
            if (IRET == 1):
                return [TS3, TS5]

            # Determine needed rebalancing of cabinet loads
            FFLOAD = Data.obj_cdata.DUTYE * Data.obj_cdata.CAPE + \
                Data.obj_cdata.DUTYZ * Data.obj_cdata.Q_FZ_FF
            FZLOAD = Data.obj_cdata.DUTYZ * Data.obj_cdata.CAPZ
            DELLOD = (FFLOAD * Data.obj_cdata.CAPZ - FZLOAD * \
                      Data.obj_cdata.CAPE) / (Data.obj_cdata.CAPE + Data.obj_cdata.CAPZ)

            DUTMAX = max(Data.obj_cdata.DUTYE, Data.obj_cdata.DUTYZ)
            DUTMIN = min(Data.obj_cdata.DUTYE, Data.obj_cdata.DUTYZ)
            DUTDIF = DUTMAX - DUTMIN

            Data.obj_cdata.IBLNCE = 0
            DUTERR = DUTDIF / DUTMAX

            if (DUTERR <= 0.001):
                return [TS3, TS5]
            if (DUTDIF >= 0.025):
                Data.obj_cdata.IBLNCE = 1

            if Data.obj_cdata.INCTRL == 0:
                # SELECT CASE (Data.obj_cdata.INCTRL)
                # CASE (0)#No control
                return [TS3, TS5]

            elif Data.obj_cdata.INCTRL == 1:
                # CASE [1]#Evap area ratio
                FFNEW = FFLOAD + DELLOD
                FZNEW = FZLOAD - DELLOD
                DAREAF = (FFNEW / FFLOAD - 1.0) * Data.obj_cdata.ATOTE
                DAREAZ = (FZNEW / FZLOAD - 1.0) * Data.obj_cdata.AREAFZ

                DUTY_AVE = (Data.obj_cdata.DUTYE + Data.obj_cdata.DUTYZ) / 2.0
                DAREAF = (Data.obj_cdata.DUTYE / DUTY_AVE - 1.0) * \
                    Data.obj_cdata.ATOTE
                DAREAZ = (Data.obj_cdata.DUTYZ / DUTY_AVE - 1.0) * \
                    Data.obj_cdata.AREAFZ

                RATIOF = DAREAF / Data.obj_cdata.ATOTE

                if (RATIOF < -0.5):
                    RATIOF = -0.5

                DAREAF = RATIOF * Data.obj_cdata.ATOTE * 0.5
                RATIOZ = DAREAZ / Data.obj_cdata.AREAFZ

                if (RATIOZ < -0.5):
                    RATIOZ = -0.5

                DAREAZ = RATIOZ * Data.obj_cdata.AREAFZ * 0.5

                if (abs(DAREAF) < abs(DAREAZ)):
                    Data.obj_cdata.ATOTE = Data.obj_cdata.ATOTE + DAREAF
                    Data.obj_cdata.AREAFZ = Data.obj_cdata.AREAFZ - DAREAF
                else:
                    Data.obj_cdata.AREAFZ = Data.obj_cdata.AREAFZ + DAREAZ
                    Data.obj_cdata.ATOTE = Data.obj_cdata.ATOTE - DAREAZ
                # END if

                Data.obj_cdata.UAF = UAF_S * Data.obj_cdata.AREAFZ / Data.obj_cdata.AREAFZ_S
                Data.obj_cdata.ATOTE_A = Data.obj_cdata.ATOTE
                Data.obj_cdata.AREAFZ_A = Data.obj_cdata.AREAFZ

                Data.obj_cdata.UA_FZ = UA_FZ_S * Data.obj_cdata.AREAFZ / Data.obj_cdata.AREAFZ_S
                Data.obj_cdata.UA_ML = UA_ML_S * Data.obj_cdata.AREAFZ / Data.obj_cdata.AREAFZ_S
                Data.obj_cdata.UA_FF = UA_FF_S * Data.obj_cdata.ATOTE / Data.obj_cdata.ATOTE_S

            elif Data.obj_cdata.INCTRL == 2:
                # CASE (2)#FF Cabinet temp
                DUTYN = 0.5 * (Data.obj_cdata.DUTYE + Data.obj_cdata.DUTYZ)
                Data.obj_cdata.FFQ = DUTYN * Data.obj_cdata.CAPE + \
                    DUTYN * Data.obj_cdata.Q_FZ_FF + FROSTF_S
                DELTS3 = (Data.obj_cdata.FFQ - FFQ_S) / UFF

                TS3 = TS3_S - DELTS3 / 1.8
                Data.obj_cdata.FFTEMP_A = 1.8 * TS3 - 459.6

                Data.obj_cdata.FFSEN = UFF_SEN * \
                    (Data.obj_cdata.TROOM - Data.obj_cdata.FFTEMP_A)
                Data.obj_cdata.CONDF = UCND_F * \
                    (Data.obj_cdata.TROOM - Data.obj_cdata.FFTEMP_A) - Data.obj_cdata.QMUL

            elif Data.obj_cdata.INCTRL == 3:
                # CASE (3)#Freezer temp
                DUTYN = 0.25 * Data.obj_cdata.DUTYE + 0.75 * Data.obj_cdata.DUTYZ
                FZQ = DUTYN * Data.obj_cdata.CAPZ
                if (IDFRST == 0):
                    FZQ = FZQ - FROSTZ_S

                DELTS5 = 0.3 * (FZQ - FZQ_S) / UFZ + 0.7 * DELTS5_OLD
                DELTS5_OLD = DELTS5

                FZQ = FZQ_S + UFZ * DELTS5
                Data.obj_cdata.FZQON = FZQ
                Data.obj_cdata.FZQOFF = FZQ

                TS5 = TS5_S - DELTS5 / 1.8
                Data.obj_cdata.FZTEMP_A = 1.8 * TS5 - 459.6

                Data.obj_cdata.FZSEN = UFZ_SEN * \
                    (Data.obj_cdata.TROOM - Data.obj_cdata.FZTEMP_A)
                Data.obj_cdata.CONDZ = UCND_Z * \
                    (Data.obj_cdata.TROOM - Data.obj_cdata.FZTEMP_A) + Data.obj_cdata.QMUL

            elif Data.obj_cdata.INCTRL in [4, 5]:
                pass

        return [TS3, TS5]
