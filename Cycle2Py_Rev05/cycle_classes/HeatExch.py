# Python import
import math
import sys
import datetime

# User import
from .Data import Data
from .Block2 import Block2


class HeatExch (Block2, Data):
    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def lowevp(
            self,
            ICYCL,
            ICNTRL,
            H,
            P,
            X,
            T,
            XQ,
            XL,
            XV,
            VL,
            VV,
            HL,
            TS3,
            TS5,
            DPF,
            ETHX2):
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
        XQ[10] = 0
        ETHX = ETHX2
        TSAV = T[9]

        XL_Temp = [0.0] * len(XL)  # in python only
        XV_Temp = [0.0] * len(XV)  # in python only

        #
        #          FIND BUBBLE AND DEW POINT ENTHALPIES AT FREEZER PRESSURE
        #
        # [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
        # CALL BUBLP(P[8],X,XV(1,8),TBUB,VL[8],VV[8],True,LCRIT)
        [X, XV_Temp, TBUB, VL[8], VV[8], LCRIT] = self.bublp(
            P[8], X, XV_Temp, True)
        self.setArr2dCol(XV, 8, XV_Temp)

        #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
        # CALL HCVCPS(1,TBUB,VL[8],X,HBUB,CV,CP,VS)
        [HBUB, CV, CP, VS] = self.hcvcps(1, TBUB, VL[8], X)
        #

        if(NCALL == 0):
            # [P2, P3, P4, P5, P6, P8] = self.bublp ( P1, P2, P3,    P7)
            # CALL BUBLP(P[9],XL(1,9),X,TDEW,VL[9],VV[9],.FALSE.,LCRIT)
            [XL_Temp, X, TDEW, VL[9], VV[9], LCRIT] = self.bublp(
                P[9], XL_Temp, X, False)
            self.setArr2dCol(XL, 9, XL_Temp)

            #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
            # CALL HCVCPS(1,TDEW,VV[9],X,   HDEW,CV,CP,VS)
            [HDEW, CV, CP, VS] = self.hcvcps(1, TDEW, VV[9], X)

            Data.obj_cdata.CREF = Data.obj_cdata.MREF * \
                (HDEW - HBUB) / (TDEW - TBUB + 0.001)
            if(Data.obj_cdata.CREF <= 0.1):
                Data.obj_cdata.CREF = 1000000.0  # 5/9/94
            # END if

            T[10] = TS5
            NCALL = 1
        # END if
        #
        #          SET FLAG FOR POSSIBLE SOLUTION AND HANDLE SINGLE EVAP CASE
        #
        IFREZ2 = 1
        if(TBUB >= TS5):
            IFREZ2 = 0

        VL[10] = VL[6]
        if(IFREZ2 == 0):
            H[9] = H[6]
            H[10] = H[6]
            T[10] = T[6]
        # END if

        if(Data.obj_cdata.ITYPE == 1):
            ETHX = 0

        #
        #          BEGIN ITERATION FOR TEMPERATURE AT POINT 10
        #

        ITER = 1
        # 10 CONTINUE
        while (True):
            ITER = ITER + 1

            # wait a key to exit app
            # CALL INCHR(0,J,KEY)
            # if(J  ==  1): sys.exit(100)#  CALL FINISH
            #if(J  ==  68): LQUIT = True

            #TSHOW = T[10] - 273.11
            if(ICYCL == 2):
                # CALL GOTOXY(2,21)
                # CALL PRINT(TSHOW,5,1)
                self.showMsg(
                    "LIQUID LINE OUTLET FROM LOW TEMP INTERCHANGER - point 10 ",
                    T[10] - 273.11)
            # END if

            #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
            # CALL HCVCPS(1,T[10],VL[10],X,H[10],CV,CP,VS)
            [H[10], CV, CP, VS] = self.hcvcps(1, T[10], VL[10], X)

            #[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
            [T10, XQ[10], XL_Temp, XV_Temp, VL[10], VV[10],
                HL, HV] = self.hpin(H[10], P[10], X)
            self.setArr2dCol(XL, 10, XL_Temp)
            self.setArr2dCol(XV, 10, XV_Temp)

            # CALL HPIN(H[10],P[10],X,  T10,XQ[10],XL(1,10),XV(1,10),
            # VL[10],VV[10],HL,HV)

            H[8] = H[10]

            [T[8], XQ[8], XL_Temp, XV_Temp, VL[8],
                VV[8], HL, HV] = self.hpin(H[8], P[8], X)
            self.setArr2dCol(XL, 8, XL_Temp)
            self.setArr2dCol(XV, 8, XV_Temp)

            # CALL HPIN(H[8],P[8],X,  T[8],XQ[8],XL(1,8),XV(1,8),
            # VL[8],VV[8],HL,HV)

            #TSHOW = T[8] - 273.11
            # if(ICYCL  ==  2) :
            #	CALL GOTOXY(14,21)
            # else:
            #	CALL GOTOXY(14,15)
            # END if

            # CALL PRINT(TSHOW,5,1)
            self.showMsg(
                "INLET TO FREEZER EVAPORATOR - point 8",
                T[8] - 273.11)

            #
            #          DETERMINE CMIN AND CMAX
            #
            if(Data.obj_cdata.CFMF <= Data.obj_cdata.CREF):
                CMIN = Data.obj_cdata.CFMF
                CMAX = Data.obj_cdata.CREF
            else:
                CMIN = Data.obj_cdata.CREF
                CMAX = Data.obj_cdata.CFMF
            # END if

            CAPRAT = CMIN / CMAX
            if(CMIN <= 0.0):
                CMIN = 0.001

            FNTU = Data.obj_cdata.UAF / CMIN
            if(FNTU < 0.0):
                FNTU = 0.0

            #
            #          CALCULATE EFFECTIVENESS
            #
            Data.obj_cdata.UAFZ = Data.obj_cdata.UAF
            if(IFREZ2 == 1):
                if Data.obj_cdata.IFREZ == 0:
                    # SELECT CASE (IFREZ)
                    #CASE (0)
                    TAVE = (T[8] + T[9]) / 2.0

                    if(T[9] < -1000.0):
                        TAVE = T[8]  # Jan 20, 1993
                    if(TAVE > TS5):
                        TAVE = TS5 - 1.0

                    QMAX = 0.90 * Data.obj_cdata.MREF * (H(7) - H[6])  # 5/9/94
                    HRAD = SIGMA * (TAVE + TS5) * (TAVE**2 + TS5**2) * EPS
                    DELTAT = TS5 - TAVE

                    if(DELTAT <= 0.0):
                        DELTAT = 0.0001

                    DELTA = DELTAT * 1.8
                    TBAR = 0.67 * TAVE + 0.33 * TS5
                    A_NAT = 0.239 + 3.34E-04 * (273.0 - TBAR)
                    HNAT = A_NAT * (DELTA**0.33) * 20.44
                    #
                    #          MAKE APPROXIMATE CORRECTIONS FOR VIEW FACTORS AND ASSUMED
                    #          ORIENTATION OF THE EVAPORATOR PANELS.
                    #
                    #              HRAD = (1.0 - FRACT_FZ)*HRAD
                    #              HNAT = 0.5*HNAT
                    UAIR = HRAD + HNAT
                    if(Data.obj_cdata.IWALL_FZ == 1):
                        UAIR = 1.0 / (1.0 / UAIR + 0.1389 / 20.44)
                    # END if

                    QFREZ = Data.obj_cdata.UAF * UAIR * DELTAT
                    Data.obj_cdata.UAFZ = Data.obj_cdata.UAF * UAIR

                    TENV = (TROOM + 459.6) / 1.8

                    QFREZ = QFREZ + 1.8 * UA_FZ * (TENV - TAVE) * 1.0548 \
                        + 1.8 * UA_ML * (TS3 - TAVE) * 1.0548 + Q_HXS_FZ

                    if(QFREZ > QMAX):
                        QFREZ = QMAX

                elif Data.obj_cdata.IFREZ == 1:
                    #CASE (1)
                    # CALL EFCROSS(CAPRAT,FNTU,EXFR)
                    EXFR = self.efcross(CAPRAT, FNTU)

                    QFREZ = EXFR * CMIN * (TS5 - T[8])
                    Data.obj_cdata.ETAF = EXFR

                elif Data.obj_cdata.IFREZ == 2:
                    #CASE (2)
                    XX = 1.0 - CAPRAT
                    XXX = EXP(-FNTU * XX)
                    EXFR = (1.0 - XXX) / (1.0 - CAPRAT * XXX)
                    QFREZ = EXFR * CMIN * (TS5 - T[8])
                    Data.obj_cdata.ETAF = EXFR
                # END SELECT
                TS6 = TS5 - QFREZ / Data.obj_cdata.CFMF

                if(IFREZ == 0):
                    TS6 = 0.9 * TAVE + 0.1 * TS5
            else:
                QFREZ = 0.0
                TS6 = TS5
            # END if

            #
            #          UPDATE ENTHALPY ACROSS EVAPORATOR
            #
            H[9] = H[8] + QFREZ / Data.obj_cdata.MREF

            #[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
            [T[9], XQ[9], XL_Temp, XV_Temp, VL[9],
                VV[9], HL, HV] = self.hpin(H[9], P[9], X)
            # CALL HPIN(H[9],P[9],X,  T[9],XQ[9],XL(1,9),XV(1,9),
            # VL[9],VV[9],HL,HV)
            self.setArr2dCol(XL, 9, XL_Temp)
            self.setArr2dCol(XV, 9, XV_Temp)

            #TSHOW = T[9] - 273.11
            if(ICYCL == 2):
                # CALL GOTOXY(52,21)
                # CALL PRINT(TSHOW,5,1)
                self.showMsg("OUTLET FROM FREEZER EVAPORATOR - point 9", TSHOW)
            # END if

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
                [XLD, X, TD, VLD, VVD, LCRIT] = self.bublp(P[9], XLD, X, False)

                if(TD > T[6]):
                    TNEW = T[6] - ETHX * (T[6] - T[9])
                else:
                    #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
                    # CALL HCVCPS(1,T[9],VL[10],X,HHIGH,CV,CP,VS)
                    [HHIGH, CV, CP, VS] = self.hcvcps(1, T[9], VL[10], X)

                    # [P4, P5] = self.espar [P1, P2, P3]
                    # CALL ESPAR(0,T[6],X,A1,B1)
                    [A1, B1] = self.espar[0, T[6], X]

                    #VGUESS = R*T[6]/P[9]
                    VGUESS = R * T[6] / P[9] / 1000  # must be in Pa

                    #[P5, P7] = self.vit (P1, P2, P3, P4, P5, P6)
                    # CALL VIT(T[6],P[9],A1,V1,VGUESS,.FALSE.,LCRIT)
                    [VGUESS, LCRIT] = self.vit(
                        T[6], P[9], A1, V1, VGUESS, False)

                    #[P5, P6, P7, P8] = self.hcvcps (P1, P2, P3, P4)
                    # CALL HCVCPS(1,T[6],VGUESS,X,HLOW,CV,CP,VS)
                    [HLOW, CV, CP, VS] = self.hcvcps(1, T[6], VGUESS, X)

                    DH = min((HLOW - H[9]), (H[6] - HHIGH))
                    H[10] = H[6] - DH

                    #[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )
                    [T[10], XQ[10], XL_Temp, XV_Temp, VL[10],
                        VV[10], HL, HV] = self.hpin(H[10], P[10], X)
                    self.setArr2dCol(XL, 10, XL_Temp)
                    self.setArr2dCol(XV, 10, XV_Temp)

                    # CALL HPIN(H[10],P[10],X, T[10],XQ[10],XL(1,10), XV(1,10),VL[10],VV[10],HL,HV)
                # END if
            # END if
            #
            #          CORRECT GUESS if NECESSARY AND CALCULATE ERROR
            #
            if(T[9] > T[6]):
                TNEW = T[10] - 0.9 * ERROR
                T[10] = TNEW
            # END if

            T[10] = TNEW
            ERROR = TSAV - T[9]
            TSAV = T[9]

            if(abs(ERROR) < TOL_FRZ):
                break  # GO TO 20

            Data.obj_cdata.CREF = Data.obj_cdata.MREF * \
                abs((H[9] - H[8]) / (T[9] - T[8] + 0.0001))
            if(Data.obj_cdata.CREF <= 0.1):
                Data.obj_cdata.CREF = 1000000.0  # /5/9/94
            # END if
            ITER = ITER + 1

            if (abs(TOLD - TNEW) > 2.0):  # 5/9/94
                if (TOLD > TNEW):
                    TNEW = TOLD - 2.0  # 5/9/94
                if (TOLD < TNEW):
                    TNEW = TOLD + 2.0  # 5/9/94
            # END if

            if(ITER > 2):
                T[10] = 0.5 * (TOLD + TNEW)  # 5/9/94
            if(ITER > 10):
                break  # GO TO 20
        # GO TO 10

        #
        #          END OF ITERATION.  CALCULATE NEEDED PARAMETERS
        #
        # 20 CONTINUE

        H[5] = H[9] + (H[6] - H[10])
        #[P4, P5, P6, P7, P8, P9, P10, P11] = self.hpin ( P1,P2,P3 )

        [T[5], XQ[5], XL_Temp, XV_Temp, VL[5],
            VV[5], HL, HV] = self.hpin(H[5], P[5], X)
        self.setArr2dCol(XL, 5, XL_Temp)
        self.setArr2dCol(XV, 5, XV_Temp)

        [T[10], XQ[10], XL_Temp, XV_Temp, VL[10],
            VV[10], HL, HV] = self.hpin(H[10], P[10], X)
        self.setArr2dCol(XL, 10, XL_Temp)
        self.setArr2dCol(XV, 10, XV_Temp)

        # CALL HPIN(H[5],P[5],X, T[5],XQ[5],XL(1,5),XV(1,5),VL[5],VV[5],   HL,HV)
        # CALL HPIN(H[10],P[10],X, T[10],XQ[10],XL(1,10),XV(1,10),VL[10],
        # VV[10],HL,HV)

        #TSHOW = T[5] - 273.11
        # if(ICYCL  ==  2) :
        #	CALL GOTOXY(52,11)
        #	CALL PRINT(TSHOW,5,1)
        # END if
        self.showMsg(
            "INLET TO FRESH FOOD EVAPORATOR  - point 5 ",
            T[5] - 273.11)

        return [H, P, X, T, XQ, XL, XV, VL, VV, HL, HV, TS6, QFREZ]

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

    # =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    def exf(self, LOC, AREA, U, CMIN, CMAX):
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
        EFF_CROSS = [0.0] * (2 + 1)
        coff_A = [
            [2.394292, -1.19402, -1.45067, 1.938453, -0.81305, 0.118651],
            [2.410798, -2.23391, 0.825900, 0.051006, -0.11891, 0.023360],
            [2.399687, -2.96882, 2.367080, -1.23009, 0.373338, -0.04886],
            [2.359642, -3.37650, 3.048620, -1.63421, 0.468741, -0.05492]
        ]

        NTU = AREA * U / CMIN
        CRAT = CMIN / CMAX

        if LOC == 1:  # Counter-flow
            XX = 1.0 - CRAT
            XXX = math.exp(-NTU * XX)
            EFF = (1.0 - XXX) / (1.0 - CRAT * XXX)
            DEFFDA = (U / CMIN) * XX * XXX * \
                (1.0 - CRAT * EFF) / (1.0 - CRAT * XXX)

        int_row = 0
        if LOC == 2:  # Cross-flow
            if (CRAT >= 0.00 and CRAT <= 0.25):
                int_row = 1
            if (CRAT > 0.25 and CRAT <= 0.50):
                int_row = 2
            if (CRAT > 0.50 and CRAT <= 0.75):
                int_row = 3
            if (CRAT > 0.75 and CRAT <= 1.00):
                int_row = 4

            if (NTU <= 0.0):
                NTU = 0.0

            for L in range(1, 2 + 1):  # DO L = 1, 2
                BETA = math.log10(NTU + 1.0)
                EFFA = 0.0
                EFFB = 0.0

                for J in range(1, 6 + 1):  # DO J = 1, 6
                    EX = 1.0 * J
                    if (int_row == 1):
                        EFFA = 1.0 - math.exp(-NTU)
                    else:
                        EFFA = EFFA + coff_A[int_row - 1 - 1][J - 1] * BETA**EX
                    # END if
                    EFFB = EFFB + coff_A[int_row - 1][J - 1] * BETA**EX
                # END DO

                FRAC = (CRAT - (int_row - 1) * 0.25) / \
                    (int_row * 0.25 - (int_row - 1) * 0.25)
                EFFECT = EFFA + FRAC * (EFFB - EFFA)

                if (EFFECT > 1.0):
                    EFFECT = 1.0

                EFF_CROSS[L] = EFFECT
                NTU = 0.9 * NTU
            # END DO
            EFF = EFF_CROSS[1]
            DEFFDA = 10.0 * (EFF_CROSS[1] - EFF_CROSS[2]) / AREA

        if(DEFFDA <= 0.0):
            DEFFDA = 0.0001
        return [EFF, DEFFDA]
