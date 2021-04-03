# Python import
import math
import sys
import datetime

# User import
from cycle_classes.CoolPrp import *

class CycleUtils ():
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def adjlod(self, dt, MREF, ICYCL, IC, TS3, TS5, FROSTF, FROSTZ, IDFRST):
        # ADJUST THE CABINET LOADS AND SET POINT TEMPERATURES *

        # IC conderser trail number
        # CYCL : Cycle Type (1 to 5)

        # TS3 - K - HTF temperature entering fresh food evaporator
        # TS5 - K - HTF temperature entering freezer evaporator
         
        # FROSTF : watt,Fresh Food Door Frost Load
        # FROSTZ : watt,Freezer Door Sensible Load

        # IDFRST : #, if 0 Manual Defrost, auto 1     




        # [p3, p4] = self.adjlod (all)
        #	SUBROUTINE ADJLOD(ICYCL,IC,TS3,TS5,FROSTF,FROSTZ,IDFRST)
        #
        
        # COMMON BLOCKS
        #
        #	REAL MREF
        #	no 		COMMON / PARMS / ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
        #	no 		COMMON / PARMS2 / TSPEC,TDROPC
        #	COMMON / HTEXS / CFMC,CFME,CFMF,self.dt.UAF,ETAC,ETAE,ETAF

        #	COMMON / FEVAP / UTPE,USUPE,self.dt.ATOTE
        #	no		COMMON / CONDEN / UDSC,UTPC,USCC,ATOTC,UACOND
        #	no		COMMON / SPECS / DTSUPE,DTSUBC
        #	no		COMMON  / SPECS2 /  ISPEC,XEXITE,DTSUPI
        #	COMMON / CABLOD / FFASH,FFAUX,FZASH,FZAUX,self.dt.TROOM,self.dt.FFTEMP,OTHERW,
        #	 .self.dt.FZTEMP,self.dt.FFQ,self.dt.FZQON,self.dt.FZQOFF,self.dt.FFLAT,FZLAT,self.dt.FFSEN,self.dt.FZSEN,
        #	 .self.dt.FFHTQ,self.dt.FZHTQ,self.dt.CONDF,self.dt.CONDZ,self.dt.QMUL
        #	COMMON / FANS / FANE,FANZ,FANC,DUTYC,W,COPR
        #	COMMON / LORENZ / self.dt.DUTYE,self.dt.DUTYZ,PWRL,PWRE,self.dt.CAPE,self.dt.CAPZ,DUTYL,DUTYS,
        #	 .FANEL,FANCL,FANES,FANCS
        #	COMMON / FIGURE / IEVAP
        #	COMMON  /  RESULT  /  QE, QZ, FLOW, QEN(2), FLOWN(2), COPRN(2)
        #	COMMON  /  CHINA  /  self.dt.INCTRL
        #	COMMON  /  BALNCE  /  self.dt.IBLNCE, BAFFLF, BAFFLZ, self.dt.AREAFZ, self.dt.ATOTE_S,
        #	 .self.dt.AREAFZ_S, self.dt.ATOTE_A, self.dt.AREAFZ_A, self.dt.FFTEMP_A, self.dt.FZTEMP_A
        #	COMMON  /  INWALL  /  self.dt.UA_FZ, self.dt.UA_FF, self.dt.UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
        #	 .Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
        #	 .CAPZ_IN_WALL, self.dt.Q_FZ_FF
        #	COMMON  /  PENAT  /  self.dt.FFPENA, self.dt.FZPENA
        #
        # BRANCH ON THE VALUE IC.AVE VARIABLES AND INITIALIZE ON THE
        # FIRST CALL AND : WAIT UNTIL THE 4TH CALL TO MAKE ADJUSTMENTS
        #
        IRET = 0  # in Python only

        if IC == 1:
            # SELECT CASE (IC)
            #	 CASE [1]#Initialize
            dt.IBLNCE = 0
            IRET = 0

            if(ICYCL != 2):
                IRET = 1

            FFTEMP_S = dt.FFTEMP
            FZTEMP_S = dt.FZTEMP
            
            FFQ_S = dt.FFQ
            FZQON_S = dt.FZQON
            FZQOFF_S = dt.FZQOFF
            
            FFLAT_S = dt.FFLAT
            FZLAT_S = dt.FZLAT
            
            FFSEN_S = dt.FFSEN
            FZSEN_S = dt.FZSEN
            
            FFHTQ_S = dt.FFHTQ
            FZHTQ_S = dt.FZHTQ
            
            FROSTF_S = FROSTF
            FROSTZ_S = FROSTZ

            CONDF_S = dt.CONDF
            CONDZ_S = dt.CONDZ

            dt.ATOTE_S = dt.ATOTE
            dt.AREAFZ_S = dt.AREAFZ
            
            UAF_S = dt.UAF
            dt.ATOTE_A = dt.ATOTE
            dt.AREAFZ_A = dt.AREAFZ

            UFF = (dt.FFQ - dt.FFLAT - dt.FFPENA - dt.FFHTQ -
                   FROSTF + dt.QMUL) / (dt.TROOM - dt.FFTEMP)

            FZQ = dt.FZQON
            FZQ_S = FZQ

            UFZ = (FZQ - dt.FZLAT - dt.FZPENA - dt.FZHTQ -
                   FROSTZ - dt.QMUL) / (dt.TROOM - dt.FZTEMP)

            UFF_SEN = FFSEN_S / (dt.TROOM - FFTEMP_S)
            UFZ_SEN = FZSEN_S / (dt.TROOM - FZTEMP_S)

            UCND_F = (dt.CONDF + dt.QMUL) / \
                (dt.TROOM - FFTEMP_S)
            UCND_Z = (dt.CONDZ - dt.QMUL) / \
                (dt.TROOM - FZTEMP_S)

            TS3_S = TS3
            TS5_S = TS5
            
            dt.FFTEMP_A = dt.FFTEMP
            dt.FZTEMP_A = dt.FZTEMP

            DELTS5_OLD = 0

            UA_FZ_S = dt.UA_FZ
            UA_ML_S = dt.UA_ML
            UA_FF_S = dt.UA_FF

        elif IC in [2, 3]:
            pass

        else:
            # CASE DEFAULT
            if (IRET == 1):
                return [TS3, TS5]

            # Determine needed rebalancing of cabinet loads
            FFLOAD = dt.DUTYE * dt.CAPE + dt.DUTYZ * dt.Q_FZ_FF
            FZLOAD = dt.DUTYZ * dt.CAPZ
            
            DELLOD = (FFLOAD * dt.CAPZ - FZLOAD * \
                      dt.CAPE) / (dt.CAPE + dt.CAPZ)

            DUTMAX = max(dt.DUTYE, dt.DUTYZ)
            DUTMIN = min(dt.DUTYE, dt.DUTYZ)
            DUTDIF = DUTMAX - DUTMIN

            dt.IBLNCE = 0
            DUTERR = DUTDIF / DUTMAX

            if (DUTERR <= 0.001):
                return [TS3, TS5]
                
            if (DUTDIF >= 0.025):
                dt.IBLNCE = 1

            if dt.INCTRL == 0:
                # SELECT CASE (dt.INCTRL)
                # CASE (0)#No control
                return [TS3, TS5]

            elif dt.INCTRL == 1:
                # CASE [1]#Evap area ratio
                FFNEW = FFLOAD + DELLOD
                FZNEW = FZLOAD - DELLOD
                
                DAREAF = (FFNEW / FFLOAD - 1.0) * dt.ATOTE
                DAREAZ = (FZNEW / FZLOAD - 1.0) * dt.AREAFZ

                DUTY_AVE = (dt.DUTYE + dt.DUTYZ) / 2.0
                DAREAF = (dt.DUTYE / DUTY_AVE - 1.0) * dt.ATOTE
                DAREAZ = (dt.DUTYZ / DUTY_AVE - 1.0) * dt.AREAFZ

                RATIOF = DAREAF / dt.ATOTE

                if (RATIOF < -0.5):
                    RATIOF = -0.5

                DAREAF = RATIOF * dt.ATOTE * 0.5
                RATIOZ = DAREAZ / dt.AREAFZ

                if (RATIOZ < -0.5):
                    RATIOZ = -0.5

                DAREAZ = RATIOZ * dt.AREAFZ * 0.5

                if (abs(DAREAF) < abs(DAREAZ)):
                    dt.ATOTE = dt.ATOTE + DAREAF
                    dt.AREAFZ = dt.AREAFZ - DAREAF
                    
                else:
                    dt.AREAFZ = dt.AREAFZ + DAREAZ
                    dt.ATOTE = dt.ATOTE - DAREAZ

                dt.UAF = UAF_S * dt.AREAFZ / dt.AREAFZ_S
                dt.ATOTE_A = dt.ATOTE
                dt.AREAFZ_A = dt.AREAFZ

                dt.UA_FZ = UA_FZ_S * dt.AREAFZ / dt.AREAFZ_S
                dt.UA_ML = UA_ML_S * dt.AREAFZ / dt.AREAFZ_S
                dt.UA_FF = UA_FF_S * dt.ATOTE / dt.ATOTE_S

            elif dt.INCTRL == 2:
                # CASE (2)#FF Cabinet temp
                DUTYN = 0.5 * (dt.DUTYE + dt.DUTYZ)
                dt.FFQ = DUTYN * dt.CAPE + DUTYN * dt.Q_FZ_FF + FROSTF_S
                DELTS3 = (dt.FFQ - FFQ_S) / UFF

                # check temp units--- dr. Omar
                TS3 = TS3_S - DELTS3 / 1.8
                dt.FFTEMP_A = 1.8 * TS3 - 459.6

                dt.FFSEN = UFF_SEN * (dt.TROOM - dt.FFTEMP_A)
                dt.CONDF = UCND_F * (dt.TROOM - dt.FFTEMP_A) - dt.QMUL

            elif dt.INCTRL == 3:
                # CASE (3)#Freezer temp
                DUTYN = 0.25 * dt.DUTYE + 0.75 * dt.DUTYZ
                FZQ = DUTYN * dt.CAPZ
                
                if (IDFRST == 0):
                    FZQ = FZQ - FROSTZ_S

                DELTS5 = 0.3 * (FZQ - FZQ_S) / UFZ + 0.7 * DELTS5_OLD
                DELTS5_OLD = DELTS5

                FZQ = FZQ_S + UFZ * DELTS5
                dt.FZQON = FZQ
                dt.FZQOFF = FZQ

                TS5 = TS5_S - DELTS5 / 1.8
                dt.FZTEMP_A = 1.8 * TS5 - 459.6

                dt.FZSEN = UFZ_SEN * (dt.TROOM - dt.FZTEMP_A)
                dt.CONDZ = UCND_Z * (dt.TROOM - dt.FZTEMP_A) + dt.QMUL

            elif dt.INCTRL in [4, 5]:
                pass

        return [TS3, TS5]
    
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def lowevp(self, dt, objCP, MREF, ICYCL, ICNTRL
            ,H, P, T
            # XQ, XL, XV, not used
            # ,VL, VV
            # HL, not clear its use
            ,TS3, TS5
            ,DPF, ETHX2):
        # ICNTRL - CONTROL METHOD FOR EVAPORATOR LOAD
        
        # Input P5, P8
        #           , T6, T9, 
        #           ,TS5
        #           ,H5,H6
        #           VL6
        #           MREF, CFMF, CREF, UAF, IWALL_FZ, IFREZ
        
        # Output P8, P8, P10
        #           ,T5,T8,T9,T10
        #           ,TS6
        #           , H5,H8,H9,H10
        #           VL10
        #           CREF, UAFZ, ETAF
        #           QFREZ
        
        
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
        EPS = 0.8
        #
        #          SET UP PRESSURES AND QUALITIES
        #
        P[10] = P[6]
        P[9] = P[5]
        P[8] = P[9] + DPF
        #XQ[10] = 0
        ETHX = ETHX2
        TSAV = T[9]

        # XL_Temp = [0.0] * len(XL)  # in python only
        # XV_Temp = [0.0] * len(XV)  # in python only

        # Find bubble and dew point enthalpies at freezer pressure
            # [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
            # CALL BUBLP(P[8],X,XV(1,8),TBUB,VL[8],VV[8],True,LCRIT)
            
        # [X, XV_Temp, TBUB, VL[8], VV[8], LCRIT] = self.bublp(
            # P[8], X, XV_Temp, True)
        # self.setArr2dCol(XV, 8, XV_Temp)
        
        TBUB = objCP.Property('T', P=P[8], X=0)  # K
        
            #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
            # CALL HCVCPS(1,TBUB,VL[8],X,HBUB,CV,CP,VS)
        #[HBUB, CV, CP, VS] = self.hcvcps(1, TBUB, VL[8], X)
        
        HBUB = self.objCP.Property('H', P=P[8], X=0)  # K

        if(NCALL == 0): # Python all times NCALL = 0 !!!
                # [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
                # CALL BUBLP(P[9],XL(1,9),X,TDEW,VL[9],VV[9],.FALSE.,LCRIT)
            # [XL_Temp, X, TDEW, VL[9], VV[9], LCRIT] = self.bublp(
                # P[9], XL_Temp, X, False)
            # self.setArr2dCol(XL, 9, XL_Temp)
            
            TDEW = objCP.Property('T', P=P[9], X=1)  # K
            
                #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
                # CALL HCVCPS(1,TDEW,VV[9],X,   HDEW,CV,CP,VS)
            #[HDEW, CV, CP, VS] = self.hcvcps(1, TDEW, VV[9], X)
            
            HDEW = objCP.Property('H', P=P[9], X=1)  # j/kg
            
            dt.CREF = MREF * (HDEW - HBUB) / (TDEW - TBUB + 0.001)
            if(dt.CREF <= 0.1):
                dt.CREF = 1000000.0  # 5/9/94
            # END if

            T[10] = TS5
            NCALL = 1

        # set flag for possible solution and handle single evap case

        IFREZ2 = 1
        if(TBUB >= TS5):
            IFREZ2 = 0

        # VL[10] = VL[6]
        if(IFREZ2 == 0):
            H[9] = H[6]
            H[10] = H[6]
            T[10] = T[6]

        if(dt.ITYPE == 1):
            ETHX = 0
        
        # begin iteration for temperature at point 10

        ITER = 1
        # 10 CONTINUE
        while (True):
            ITER = ITER + 1

            # wait a key to exit app
            # CALL INCHR(0,J,KEY)
            # if(J  ==  1): sys.exit(100)#  CALL FINISH
            #if(J  ==  68): LQUIT = True

            #TSHOW = T[10] - 273.11
            # if(ICYCL == 2):
                # # CALL GOTOXY(2,21)
                # # CALL PRINT(TSHOW,5,1)
                # self.showMsg(
                    # "LIQUID LINE OUTLET FROM LOW TEMP INTERCHANGER - point 10 ",
                    # T[10] - 273.11)
 
                #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
                # CALL HCVCPS(1,T[10],VL[10],X,H[10],CV,CP,VS)
            #[H[10], CV, CP, VS] = self.hcvcps(1, T[10], VL[10], X)

            H[10] = objCP.Property('H', T=T[10], P=P[10])  # j/kg
            
                #[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
                # CALL HPIN(H[10],P[10],X,  T10,XQ[10],XL(1,10),XV(1,10),
                # VL[10],VV[10],HL,HV)
                
            # [T10, XQ[10], XL_Temp, XV_Temp, VL[10], VV[10],
                # HL, HV] = self.hpin(H[10], P[10], X)
            # self.setArr2dCol(XL, 10, XL_Temp)
            # self.setArr2dCol(XV, 10, XV_Temp)

            # I think it is T[10]
            T[10] = objCP.Property('T', P=P[10], H=H[10])  # K
            
            H[8] = H[10]

                # CALL HPIN(H[8],P[8],X,  T[8],XQ[8],XL(1,8),XV(1,8),
                # VL[8],VV[8],HL,HV)
            # [T[8], XQ[8], XL_Temp, XV_Temp, VL[8],
                # VV[8], HL, HV] = self.hpin(H[8], P[8], X)
            # self.setArr2dCol(XL, 8, XL_Temp)
            # self.setArr2dCol(XV, 8, XV_Temp)

            T[8] = objCP.Property('T', P=P[8], H=H[8])  # K
            
            #TSHOW = T[8] - 273.11
            # if(ICYCL  ==  2) :
            #	CALL GOTOXY(14,21)
            # else:
            #	CALL GOTOXY(14,15)
            # END if

            # CALL PRINT(TSHOW,5,1)
            # self.showMsg(
                # "INLET TO FREEZER EVAPORATOR - point 8",
                # T[8] - 273.11)


            # DETERMINE CMIN AND CMAX

            if(dt.CFMF <= dt.CREF):
                CMIN = dt.CFMF
                CMAX = dt.CREF
            else:
                CMIN = dt.CREF
                CMAX = dt.CFMF
            # END if

            CAPRAT = CMIN / CMAX
            if(CMIN <= 0.0):
                CMIN = 0.001

            FNTU = dt.UAF / CMIN
            if(FNTU < 0.0):
                FNTU = 0.0

            #          CALCULATE EFFECTIVENESS
            #
            dt.UAFZ = dt.UAF
            if(IFREZ2 == 1):
                if dt.IFREZ == 0:
                    # SELECT CASE (IFREZ)
                    #CASE (0)
                    TAVE = (T[8] + T[9]) / 2.0

                    if(T[9] < -1000.0):
                        TAVE = T[8]  # Jan 20, 1993
                    if(TAVE > TS5):
                        TAVE = TS5 - 1.0

                    QMAX = 0.90 * MREF * (H(7) - H[6])  # 5/9/94
                    HRAD = SIGMA * (TAVE + TS5) * (TAVE**2 + TS5**2) * EPS
                    DELTAT = TS5 - TAVE

                    if(DELTAT <= 0.0):
                        DELTAT = 0.0001

                    # Dr. Omar Units
                    DELTA = DELTAT * 1.8
                    TBAR = 0.67 * TAVE + 0.33 * TS5
                    A_NAT = 0.239 + 3.34E-04 * (273.0 - TBAR)
                    HNAT = A_NAT * (DELTA**0.33) * 20.44

                    #  MAKE APPROXIMATE CORRECTIONS FOR VIEW FACTORS AND ASSUMED
                    #    ORIENTATION OF THE EVAPORATOR PANELS.
                    #
                    #  HRAD = (1.0 - FRACT_FZ)*HRAD
                    #  HNAT = 0.5*HNAT
                    UAIR = HRAD + HNAT
                    if(dt.IWALL_FZ == 1):
                        UAIR = 1.0 / (1.0 / UAIR + 0.1389 / 20.44)
                    # END if

                    QFREZ = dt.UAF * UAIR * DELTAT
                    dt.UAFZ = dt.UAF * UAIR

                    # Dr Omar Temp Unit
                    TENV = (TROOM + 459.6) / 1.8
                    
                    # Dr Omar Heat Unit
                    QFREZ = QFREZ + 1.8 * UA_FZ * (TENV - TAVE) * 1.0548 \
                        + 1.8 * UA_ML * (TS3 - TAVE) * 1.0548 + Q_HXS_FZ

                    if(QFREZ > QMAX):
                        QFREZ = QMAX

                elif dt.IFREZ == 1:
                    #CASE (1)
                    # CALL EFCROSS(CAPRAT,FNTU,EXFR)
                    EXFR = self.efcross(CAPRAT, FNTU)

                    QFREZ = EXFR * CMIN * (TS5 - T[8])
                    dt.ETAF = EXFR

                elif dt.IFREZ == 2:
                    #CASE (2)
                    XX = 1.0 - CAPRAT
                    XXX = EXP(-FNTU * XX)
                    EXFR = (1.0 - XXX) / (1.0 - CAPRAT * XXX)
                    QFREZ = EXFR * CMIN * (TS5 - T[8])
                    dt.ETAF = EXFR
                # END SELECT
                TS6 = TS5 - QFREZ / dt.CFMF

                if(IFREZ == 0):
                    TS6 = 0.9 * TAVE + 0.1 * TS5
            else:
                QFREZ = 0.0
                TS6 = TS5
            # END if

            #
            #          UPDATE ENTHALPY ACROSS EVAPORATOR
            #
            H[9] = H[8] + QFREZ / MREF

                #[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
                # [T[9], XQ[9], XL_Temp, XV_Temp, VL[9],
                # VV[9], HL, HV] = self.hpin(H[9], P[9], X)

            # CALL HPIN(H[9],P[9],X,  T[9],XQ[9],XL(1,9),XV(1,9),
            # VL[9],VV[9],HL,HV)
            # self.setArr2dCol(XL, 9, XL_Temp)
            # self.setArr2dCol(XV, 9, XV_Temp)
            
            T[9] = objCP.Property('T', P=P[9], H=H[9])  # K
            
            #TSHOW = T[9] - 273.11
            # if(ICYCL == 2):
                # # CALL GOTOXY(52,21)
                # # CALL PRINT(TSHOW,5,1)
                # self.showMsg("OUTLET FROM FREEZER EVAPORATOR - point 9", TSHOW)
            # # END if

            if(IFREZ2 == 0):
                break  # GO TO 20
            #
            #          GET NEW GUESS FOR TEMPERATURE AT POINT 10
            #
            TOLD = T[10]
            if(ICYCL != 2 .OR. ICNTRL != 2):
                TNEW = T[6] - ETHX * (T[6] - T[9])
            else:
                    # [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
                    # CALL BUBLP(P[9],XLD,X,TD,VLD,VVD,.FALSE.,LCRIT)
                #[XLD, X, TD, VLD, VVD, LCRIT] = self.bublp(P[9], XLD, X, False)
                TD = self.objCP.Property('T', X=1, P=P[9])  # K
                if(TD > T[6]):
                    TNEW = T[6] - ETHX * (T[6] - T[9])
                else:
                        #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
                        # CALL HCVCPS(1,T[9],VL[10],X,HHIGH,CV,CP,VS)
                    #[HHIGH, CV, CP, VS] = self.hcvcps(1, T[9], VL[10], X)
                    HHIGH = self.objCP.Property('H', V=VL[10], T=T[9])  # j/kg
                
                        # [P4, P5] = self.espar [P1, P2, P3]
                        # CALL ESPAR(0,T[6],X,A1,B1)
                    #[A1, B1] = self.espar[0, T[6], X]

                        #VGUESS = R*T[6]/P[9]
                    #VGUESS = R * T[6] / P[9] / 1000  # must be in Pa

                        #[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
                        # CALL VIT(T[6],P[9],A1,V1,VGUESS,.FALSE.,LCRIT)
                    #[VGUESS, LCRIT] = self.vit(T[6], P[9], A1, V1, VGUESS, False)

                        #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
                        # CALL HCVCPS(1,T[6],VGUESS,X,HLOW,CV,CP,VS)
                    #[HLOW, CV, CP, VS] = self.hcvcps(1, T[6], VGUESS, X)

                    HLOW = self.objCP.Property('H', X=0, T=T[6])  # j/kg
                    
                    DH = min((HLOW - H[9]), (H[6] - HHIGH))
                    H[10] = H[6] - DH

                        #[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
                        # CALL HPIN(H[10],P[10],X, T[10],XQ[10],XL(1,10), XV(1,10),VL[10],VV[10],HL,HV)
                    # [T[10], XQ[10], XL_Temp, XV_Temp, VL[10],
                        # VV[10], HL, HV] = self.hpin(H[10], P[10], X)
                    # self.setArr2dCol(XL, 10, XL_Temp)
                    # self.setArr2dCol(XV, 10, XV_Temp)
                    
                    T[10] = objCP.Property('T', P=P[10], H=H[10])  # K
            
            # correct guess if necessary and calculate error
           
            if(T[9] > T[6]):
                TNEW = T[10] - 0.9 * ERROR
                T[10] = TNEW
            # END if

            T[10] = TNEW
            ERROR = TSAV - T[9]
            TSAV = T[9]

            if(abs(ERROR) < TOL_FRZ):
                break  # GO TO 20

            dt.CREF = MREF * abs((H[9] - H[8]) / (T[9] - T[8] + 0.0001))
                
            if(dt.CREF <= 0.1):
                dt.CREF = 1000000.0  # /5/9/94

            ITER = ITER + 1

            if (abs(TOLD - TNEW) > 2.0):  # 5/9/94
                if (TOLD > TNEW):
                    TNEW = TOLD - 2.0  # 5/9/94
                if (TOLD < TNEW):
                    TNEW = TOLD + 2.0  # 5/9/94

            if(ITER > 2):
                T[10] = 0.5 * (TOLD + TNEW)  # 5/9/94

            if(ITER > 10):
                break  # GO TO 20
        # GO TO 10

        #
        # END OF ITERATION.  CALCULATE NEEDED PARAMETERS
        #
        # 20 CONTINUE

        H[5] = H[9] + (H[6] - H[10])
            #[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )

        # [T[5], XQ[5], XL_Temp, XV_Temp, VL[5],
            # VV[5], HL, HV] = self.hpin(H[5], P[5], X)
        # self.setArr2dCol(XL, 5, XL_Temp)
        # self.setArr2dCol(XV, 5, XV_Temp)
        
        T[5] = objCP.Property('T', P=P[5], H=H[5])  # K

        # [T[10], XQ[10], XL_Temp, XV_Temp, VL[10],
            # VV[10], HL, HV] = self.hpin(H[10], P[10], X)
        # self.setArr2dCol(XL, 10, XL_Temp)
        # self.setArr2dCol(XV, 10, XV_Temp)
        
        T[10] = objCP.Property('T', P=P[10], H=H[10])  # K

        # CALL HPIN(H[5],P[5],X, T[5],XQ[5],XL(1,5),XV(1,5),VL[5],VV[5],   HL,HV)
        # CALL HPIN(H[10],P[10],X, T[10],XQ[10],XL(1,10),XV(1,10),VL[10],
        # VV[10],HL,HV)

        #TSHOW = T[5] - 273.11
        # if(ICYCL  ==  2) :
        #	CALL GOTOXY(52,11)
        #	CALL PRINT(TSHOW,5,1)
        # END if
        # self.showMsg(
            # "INLET TO FRESH FOOD EVAPORATOR  - point 5 ",
            # T[5] - 273.11)

        #return [H, P, X, T, XQ, XL, XV, VL, VV, HL, HV, TS6, QFREZ]
        return [H, P, T, TS6, QFREZ]

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def efcross(self, CRAT, NTU):
        # P3 = self.efcross (P1, P2)
        #	  SUBROUTINE EFCROSS(CRAT,NTU,EFFECT)
        #     ******************************************************************
        #     *     CALCULATES THE HEAT TRANSFER EFFECTIVENESS FOR A CROSS     *
        #     *     FLOW HEAT EXCHANGER WITH BOTH FLUIDS UNMIXED               *
        #     ******************************************************************

        # DIMENSION A(4,6)

        A = [	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
              [0.0, 2.394292, -1.19402, -1.45067, 1.938453, -0.81305, 0.118651],
              [0.0, 2.410798, -2.23391, 0.8259, 0.051006, -0.11891, 0.02336],
              [0.0, 2.399687, -2.96882, 2.36708, -1.23009, 0.373338, -0.04886],
              [0.0, 2.359642, -3.3765, 3.04862, -1.63421, 0.468741, -0.05492],
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
        if(CRAT >= 0.00 and CRAT <= 0.25):
            I = 1
        if(CRAT > 0.25 and CRAT <= 0.50):
            I = 2
        if(CRAT > 0.50 and CRAT <= 0.75):
            I = 3
        if(CRAT > 0.75 and CRAT <= 1.00):
            I = 4
        if(NTU <= 0.0):
            NTU = 0.0

        BETA = LOG10(NTU + 1.0)
        EFFA = 0.0
        EFFB = 0.0
        #J = 1
        while J in range(1, 6 + 1):  # DO WHILE (J  <=  6)
            EX = 1.0 * J
            if (I == 1):
                EFFA = 1.0 - EXP(-NTU)
            else:
                EFFA = EFFA + A[I - 1][J] * BETA**EX
            # END if
            EFFB = EFFB + A[I][J] * BETA**EX
            #J = J + 1
        # END DO

        FRAC = (CRAT - (I - 1) * 0.25) / (I * 0.25 - (I - 1) * 0.25)
        EFFECT = EFFA + FRAC * (EFFB - EFFA)
        if (EFFECT > 1.0):
            EFFECT = 1.0
        return

    # -----------------------------------------------------------
    # job interchanger for subcooling condenser liquid
    #     used when the inlet states of both streams specified
    # -----------------------------------------------------------
    def inter1(self, objCP, T4, H4, T7, H7, ETHX1):
        # objCP cool prob object
        # ETHX1 - effectiveness of high temp interchanger
        # T temp in KEY
        # H Enthalpy j/kg
        # 4 - condenser outlet
        # 7 - outlet from fresh food evaporator
        
    #def inter1(self, objCP, X, P4, T4, H4, V4, P7, T7, H7, V7, ETHX1):
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

        # LCONV = False

        # determine state 6 for case of refrigerant exit TEMP=T[7]

        #P6STAR = P4
        # T6STAR = T7
        #VGUESS = V4
        
            # [P4, P5] = self.espar [P1, P2, P3]
            # CALL ESPAR(0,T6STAR,X,A6STAR,B6STAR)
        #[A6STAR, B6STAR] = self.espar(0, T6STAR, X)

            #[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
            # CALL VIT(T6STAR,P6STAR,A6STAR,B6STAR,VGUESS,True,LCONV)
        # [VGUESS, LCONV] = self.vit(T6STAR, P6STAR, A6STAR, B6STAR, VGUESS, True)

        # V6STAR = VGUESS
        
            #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
            # CALL HCVCPS(1,T6STAR,V6STAR,X,  H6STAR,CV,CP,VS)
        # [H6STAR, CV, CP, VS] = self.hcvcps(1, T6STAR, V6STAR, X)

        H6STAR = objCP.Property('H', X=1, T=T7)  # j/kg
        
        # determine state 13 if refrigerant exit TEMP=T(4)
        # for the case of evaporator exit superheat specified
        
        #P13STR = P7
        # T13STR = T4
        #VGUESS = V7 * T13STR / T7

            # [P4, P5] = self.espar [P1, P2, P3]
            # CALL ESPAR(0,T13STR,X,A13STR,B13STR)
        #[A13STR, B13STR] = self.espar(0, T13STR, X)

            #[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
            # CALL VIT(T13STR,P13STR,A13STR,B13STR,VGUESS,.FALSE.,LCONV)
        #[VGUESS, LCONV] = self.vit(T13STR, P13STR, A13STR, B13STR, VGUESS, False)
        #V13STR = VGUESS

            #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
            # CALL HCVCPS(1,T13STR,V13STR,X,   H13STR,CV,CP,VS)
        # [H13STR, CV, CP, VS] = self.hcvcps(1, T13STR, V13STR, X)
        
        H13STR = objCP.Property('H', X=0, T=T4)  # j/kg
        
        #
        #          FIND THE MAXIMUM AND ACTUAL HEAT TRANSFER
        #
        # DELH1 = H4 - H6STAR
        # DELH2 = H13STR - H7
        # QBEST = min(H4 - H6STAR, H13STR - H7)
        # QACT = ETHX1 * QBEST
        # return QACT
        
        return ETHX1 * min(H4 - H6STAR, H13STR - H7)

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def dutfnd(self, dt, ICAB
            ,IRFTYP, ICYCL, N
            ,QFRSH, QFREZ
            ,FROSTF ,FROSTZ
            ,TS3, TS5, T, IDFRST):
        # Dr. Omar Unit
        # [P9, P10, P15] = self.dutfnd (P1 ... P8, P11 to P15 )
        #	SUBROUTINE DUTFND(ICAB,IRFTYP,ICYCL,N,   QFRSH,QFREZ,FROSTF,FROSTZ,
        #	QFF,QFZ,self.dt.TS3,TS5,   T,IDFRST, DUTYR)
        # output    QFF,QFZ,DUTYR
        #	*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
        #	* 	CALCULATE DUTY CYCLE AND THE AVERAGE CABINET LOADS			*
        #	*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *

        if (ICAB == 0):
            return [0,0,0]
        #
        #	CALCULATE IN-WALL HEAT LOADS
        #
        TENV = (self.dt.TROOM + 459.6) / 1.8
        TCND = 0.2 * T[14] + 0.4 * T[3] + 0.4 * T[11]

        if (TS5 > -300.0):  # Freezer evaporator
            TRFZ = (T[8] + T[9]) / 2.0
            self.dt.Q_FZ_IN_WALL = 1.8 * \
                self.dt.UA_FZ * (TENV - TS5)
            self.dt.Q_ML_IN_WALL = 1.8 * \
                self.dt.UA_ML * (self.dt.TS3 - TS5)

            self.dt.CAPZ_IN_WALL = 1.8 * \
                self.dt.UA_FZ * (TENV - TRFZ)
            self.dt.CAPM_IN_WALL = 1.8 * \
                self.dt.UA_ML * (self.dt.TS3 - TRFZ)

            self.dt.Q_FZ_FF = 1.8 * self.dt.UA_ML * (TS5 - TRFZ)
        else:
            self.dt.Q_FZ_IN_WALL = 0
            self.dt.Q_ML_IN_WALL = 0

            self.dt.CAPZ_IN_WALL = 0
            self.dt.CAPM_IN_WALL = 0

            self.dt.Q_FZ_FF = 0
        # End if

        # Dr. Omar units
        TRFF = (T[5] + T[7]) / 2.0
        self.dt.Q_FF_IN_WALL = 1.8 * \
            self.dt.UA_FF * (TENV - self.dt.TS3)
            
        self.dt.CAPE_IN_WALL = 1.8 * \
            self.dt.UA_FF * (TENV - TRFF)
            
        self.dt.CONDF_IN_WALL = 1.8 * \
            self.dt.UA_FF_CND * (TCND - TENV)
            
        self.dt.CONDZ_IN_WALL = 1.8 * \
            self.dt.UA_FZ_CND * (TCND - TENV)
        #
        #	BRANCH ACCORDING TO THE TYPE OF REFRIGERATOR
        #
        QFF = self.dt.FFQ
        QFZ = self.dt.FZQOFF
        #

        if IRFTYP in [1, 3]:
            if (ICYCL == 1):
                if (self.dt.IDFRST == 0):
                    QFF = QFF + self.dt.FROSTF
                    QFZ = QFZ + self.dt.FROSTF

                # Dr. Omar Unit
                self.dt.CAPE = QFRSH / 1.0548 - 3.413 * self.dt.FANE - 3.413 * self.dt.DFSTCYC	\
                    - 3.413 * self.dt.FFCYC - 3.413 * self.dt.FZCYC	\
                    - self.dt.CONDF_IN_WALL - self.dt.CONDZ_IN_WALL

                self.dt.DUTYC = (QFF + QFZ) / self.dt.CAPE
                if (self.dt.DUTYC > 1.0):
                    self.dt.DUTYC = 1.0
                DUTYR = self.dt.DUTYC

            if (ICYCL == 2):
                QFF = QFF - self.dt.FROSTF
                if (self.dt.IDFRST == 0):
                    QFZ = QFZ + self.dt.FROSTF
                
                # Dr. Omar Unit
                self.dt.CAPZ = QFREZ / 1.0548 - 3.413 * self.dt.FANZ - 3.413 * self.dt.DFSTCYC	\
                    + self.dt.Q_FZ_IN_WALL + self.dt.Q_ML_IN_WALL	\
                    - self.dt.CAPZ_IN_WALL - self.dt.CAPM_IN_WALL	\
                    - self.dt.CONDZ_IN_WALL - 3.413 * self.dt.FZCYC	\
                    - self.dt.Q_HXS_FZ / 1.0548

                if (self.dt.CAPZ <= 0.0):
                    self.showError("Incorrect Solution, Check Mass Flow")
                    self.showError("Solution being Terminated")
                    sys.exit(100)  # STOP ' '

                self.dt.DUTYZ = QFZ / self.dt.CAPZ

                # Dr. Omar Unit
                self.dt.CAPE = QFRSH / 1.0548 - 3.413 * self.dt.FANE - 3.413 * self.dt.FFCYC	\
                    + self.dt.Q_FF_IN_WALL - self.dt.CAPE_IN_WALL	\
                    - self.dt.CONDF_IN_WALL + self.dt.Q_FZ_FF	\
                    - self.dt.Q_HXS_FF / 1.0548

                self.dt.DUTYE = QFF / self.dt.CAPE

                self.dt.DUTYC = min(
                    self.dt.DUTYE, self.dt.DUTYZ)

                if (self.dt.DUTYC > 1.0):
                    self.dt.DUTYC = 1.0
                DUTYR = max(self.dt.DUTYE, self.dt.DUTYZ)

                if (DUTYR > 1.0):
                    DUTYR = 1.0

            if (ICYCL == 3):
                if (N == 1):
                    if (self.dt.IDFRST == 0):
                        QFZ = QFZ + self.dt.FROSTF

                    self.dt.CAPZ = QFRSH / 1.0548 - 3.413 * self.dt.FANE - \
                        3.413 * (self.dt.DFSTCYC + self.dt.FZCYC)
                    self.dt.DUTYZ = QFZ / self.dt.CAPZ

                    self.dt.DUTYC = min(self.dt.DUTYZ, 1.0)
                    self.dt.DUTYZ = self.dt.DUTYC

                else:
                    self.dt.CAPE = QFRSH / 1.0548 - 3.413 * \
                        (self.dt.FANE + self.dt.FFCYC) + self.dt.Q_FF_IN_WALL - self.dt.CAPE_IN_WALL
                    QFF = QFF - self.dt.FROSTF
                    self.dt.DUTYE = QFF / self.dt.CAPE
                    self.dt.DUTYC = min(self.dt.DUTYE, 1.0)
                    self.dt.DUTYE = self.dt.DUTYC

                DUTYR = self.dt.DUTYC

            else:
                if (self.dt.IDFRST == 0):
                    QFZ = QFZ + self.dt.FROSTF
                self.dt.CAPE = QFRSH / 1.0548 - 3.413 * (self.dt.FANE + self.dt.DFSTCYC + self.dt.FZCYC)	\
                    + self.dt.Q_FF_IN_WALL - self.dt.CAPE_IN_WALL						\
                    - self.dt.CONDF_IN_WALL - self.dt.Q_HXS_FF / 1.0548

                self.dt.DUTYE = QFZ / self.dt.CAPE
                self.dt.DUTYC = min(self.dt.DUTYE, 1.0)
                DUTYR = self.dt.DUTYC

        return [QFF, QFZ, DUTYR]

    def mixair(self, CAP, QFF, QFZ, TFF, TFZ, CFME):
        # [P7, P8] = self.mixair (P1 to P6)
        #	  SUBROUTINE MIXAIR(CAP,QFF,QFZ,TFF,TFZ,CFME   ,TIN,X)
        #     ******************************************************************
        #     *     CALCULATE INLET TEMPERATURE TO THE EVAPORATOR              *
        #     ******************************************************************

        #          SET UP THE QUADRATIC EQUATION COEFFICIENTS
        #
        #	  COMMON /FEVAP / UTPE,USUPE,ATOTE, FF_AIR, UAFF, uafz

        A = 1.08 * CFME * (TFF - TFZ) / CAP
        B = - (A + 1.0)
        C = QFF / (QFF + QFZ)

        # Solve the quadratic equation
        X = - B / (2.0 * A) - math.sqrt(B**2 - 4.0 * A * C) / (2.0 * A)
        TIN = X * TFF + (1.0 - X) * TFZ
        
        Data.obj_cdata.FF_AIR = X
        
        return [TIN, X]