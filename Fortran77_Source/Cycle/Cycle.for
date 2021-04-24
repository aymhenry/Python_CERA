$DEBUG
      SUBROUTINE CYCLE (NC,IR,XM,F,TS1,TS3,TS5,MEFF,QHILO,QCAN,
     .     DPC,DPE,DPF,ETHX1,ETHX2,DISPLC,NCYC,FROSTF,FROSTZ,ICAB,
     .     IRFTYPe,ICYCL,ICYCLS,IDFRST)
C     ******************************************************************
C     *     CYCLE ANALYSIS FOR ONE OR TWO EVAPORATOR SYSTEM. BASED     *
C     *     ON CYCLE7 PROGRAM DEVELOPED BY NIST.  MODIFIED BY ADL      *
C     *     TO REPRESENT LORENZ CYCLE AND TO INCORPORATE HEAT          *
C     *     EXCHANGER ALGORITHMS AND A COMPRESSOR MODEL.               *
C     ******************************************************************
C
C     UNITS:
C          SI UNITS ARE USED THROUGHOUT;  INPUTS AND OUTPUTS ARE
C          ON A MASS BASIS WHILE MOST INTERNAL CALCULATIONS ARE ON A
C          MOLAR BASIS
C
C          PRESSURE - KPA
C          TEMPERATURE - K
C          ENTHALPY - KJ/KG
C          ENTROPY - KJ/KG K
C          VOLUME - M**3/KG
C
C     INPUTS:
C          NC - NUMBER OF COMPONENTS
C          IR(I) - CODE NUMBERS FOR THE I'TH COMPONENT OF THE
C                  REFRIGERANT MIXTURE (REFER TO PROPERTIES
C                  DOCUMENTATION)
C          F(I1,I2) - MIXTURE INTERACTION PARAMETER BETWEEN COMPONENT
C                     I1 AND I2
C          XM(I) - COMPOSITION OF CIRCULATING REFRIGERANT (MASS
C                  FRACTION OF IR(I))
C          TS1 - HEAT TRANSFER FLUID (HTF) TEMPERATURE ENTERING CONDENSER
C          TS3 - HTF TEMPERATURE ENTERING FRESH FOOD EVAPORATOR
C          TS5 - HTF TEMPERATURE ENTERING FREEZER EVAPORATOR
C          MEFF - MECHANICAL EFFICIENCY
C          QHILO - NORMALIZED HEAT LOSS FROM DISCHANGE LINE INSIDE
C                  THE COMPRESSOR SHELL TO SUCTION GAS
C          QCAN - COMPRESSOR SHELL LOSS NORMALIZED TO POWER INPUT
C          DPC - PRESSURE DROP THROUGH CONDENSER
C          DPE - PRESSURE DROP THROUGH FRESH FOOD EVAPORATOR
C          DPF - PRESSURE DROP THROUGH FREEZER EVAPORATOR
C          ETHX1 - EFFECTIVENESS OF HIGH TEMP INTERCHANGER
C          ETHX2 - EFFECTIVENESS OF LOW  TEMP INTERCHANGER
C          DISPLC - COMPRESSOR DISPLACEMENT (CU-IN)
C          NCYC - NUMBER OF CALL TO CYCLE (1 OR 2 FOR DUAL LOOP)
C          ICAB - FLAG TO REPRESENT PRESENCE OF CABINET LOADS IN INPUT
C          ICYCL - CYCLE TYPE (1=STANDARD, 2=LORENZ, 3=DUAL LOOP, 4=DUAL EVAP)
C          ICNTRL - CONTROL METHOD FOR EVAPORATOR LOAD
C                   0 = NONE
C                   1 = FRESH FOOD FAN OFF
C                   2 = FRESH FOOD EVAPORATOR SUPERHEAT CONTROL
C                   3 = FREEZER FAN OFF
C                   4 = FREEZER AIR DAMPER CONTROL
C
C     PROPERTY ROUTINES REFERENCED:
C          BCONST - INITIALIZES ARRAYS OF PROPERTY COEFFICIENTS
C          BUBLP - SATURATION PROPERTIES AT GIVEN PRESSURE
C          BUBLT - SATURATION PROPERTIES AT GIVEN TEMPERATURE
C          ENTROP - MOLAR ENTROPY
C          ESPAR - SET UP COEFFICIENTS FOR EQUATION OF STATE
C          HCVCPS - MOLAR ENTHALPY AND HEAT CAPACITY
C          HPIN - TEMPERATURE, QUALITY, ETC. AS A FUNCTION OF ENTHALPY
C                 AND PRESSURE
C          SPIN - TEMPERATURE, QUALITY, ETC. AS A FUNCTION OF ENTROPY
C                 AND TEMPERATURE
C          VIT - CALCULATE SPECIFIC VOLUME
C
C     NOTE:  THE ABOVE ROUTINES REFERENCE ADDITIONAL PROPERTY ROUTINES
C            THE ENTIRE SET SHOULD BE INCLUDED IN THE EXECUTABLE ELEMENT
C
C     ADDITIONAL SUBROUTINES REFERENCED:
C          CCROSS - CROSS FLOW CONDENSER
C          CCOUNT - COUNTER FLOW CONDENSER
C          COMP - COMPRESSOR MODEL
C          COND - CONDENSER ALGORITHMS
C          FFCROSS - CROSS FLOW EVAPORATOR
C          FFCOUNTER - COUNTER FLOW EVAPORATOR
C          FRSH - FRESH FOOD EVAPORATOR ALGORITHMS
C          LOWEVP - FREEZER EVAPORATOR ALGORITHMS
C          PROGRS - DISPLAY CURRENT VALUES ON SCREEN
C          SHWFIG - DISPLAY A DRAWING OF THE CYCLE ON THE SCREEN
C          SHWOUT - PRINT OUT CALCULATED VALUES OT THE SCREEN
C
C     NOMENCLATURE FOR FIRST LETTER(S) OF VARIABLE NAMES:
C          DT - TEMPERATURE DIFFERENCE
C          FT - CONVERGENCE VARIABLE, LOOP HAS CONVERGED WHEN FT=0
C          H - ENTHALPY
C          HREF - CHARACTER VARIABLE FOR REFRIGERANT NAMES
C          HSTATE - CHARACTER VARIABLE FOR CYCLE STATE POINTS:
C                   (INLET AND OUTLET REFER TO REFRIGERANT FLOW)
C              1 - COMPRESSOR INLET (SATURATED VAPOR)
C              2 - COMPRESSOR DISCHARGE
C              3 - CONDENSER DEW POINT
C              4 - CONDENSER OUTLET
C              5 - INLET TO FRESH FOOD EVAPORATOR
C              6 - LIQUID LINE OUTLET FROM HIGH TEMP INTERCHANGER
C              7 - OUTLET FROM FRESH FOOD EVAPORATOR
C              8 - INLET TO FREEZER EVAPORATOR
C              9 - OUTLET FROM FREEZER EVAPORATOR
C             10 - LIQUID LINE OUTLET FROM LOW TEMP INTERCHANGER
C             11 - CONDENSER BUBBLE POINT
C             12 - FRESH FOOD EVAPORATOR DEW POINT
C             13 - SUPERHEATED GAS LEAVING THE HIGH TEMP INTERCHANGER
C             14 - CONDENSER INLET
C             15 - INTERNAL VARIABLE (NOT SHOWN) FOR EVAP DEW POINT
C             16 - LIQUID LINE STATE AFTER HEAT LOSS TO CABINET AND MULLION
C
C          L - LOGICAL VARIABLE (ERROR FLAG, ETC.)
C          P - PRESSURE
C          T - TEMPERATURE
C          TC - REFRIGERANT AT CONDENSER OUTLET
C          TE - REFRIGERANT AT EVAPORATOR OUTLET
C          TOL - CONVERGENCE TOLERANCE
C          TS - TEMPERATURE OF HEAT TRANSFER FLUID
C          V - VOLUME
C          X - COMPOSITION
C          XL(I,J) - LIQUID PHASE COMPOSITION AT STATE J
C          XV(I,J) - VAPOR PHASE COMPOSITION AT STATE J
C          XQ - QUALITY
C
C          I REFERS TO COMPONENT
C          J REFERS TO STATE
C
      LOGICAL LCRIT,LCCON,LECON,LCONV,LQUIT
      LOGICAL AIRTMP
      CHARACTER*2 IRUN(100),CHOUR,CMIN,CSEC,CDAY,CMONN,CYEAR
      CHARACTER*3 CMONTH(12),CMON
      CHARACTER*6 HREF(34),REFH(34)
      CHARACTER*8 HSTATE(15), mstate(12)
      REAL MEFF,MREF,MREFSV
C
      DIMENSION T(16),S(16),H(16),XQ(16),P(16),V(16),XL(5,16),XV(5,16),
     .     TS(16),VL(16),VV(16),F(5,5),XM(5),X(5),IR(5),XREF(5)
      DIMENSION TC(3),TE(3),TCRIT(5),WM(5)
      DIMENSION WMAVGL(16),WMAVGV(16),LPNT(15)
      DIMENSION AIRTMP(15)
C
      COMMON /RDATA2/ WM,TCRIT
      COMMON /HREF1/ HREF,REFH
      COMMON /PARMS/ ICOND,IFRSH,IFREZ,DISP,SPEED,CE,CREF,MREF,ETAV,SEFF
      COMMON /PARMS2/ TSPEC,I_LIQUID_LINE
      COMMON /HTEXS/ CFMC,CFME,CFMF,UAF,ETAC,ETAE,ETAF
      COMMON /SPECS/ DTSUPE,DTSUBC
      COMMON /SPECS2/ ISPEC,XEXITE,DTSUPI
      COMMON /EVAPS/ ITYPE, FRACT_FF, FRACT_FZ
      COMMON /CABLOD/ FFASH,FAUXF,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
     .                FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
     .                FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
      COMMON /FANS/ FANE,FANZ,FANC,DUTYC,W,COPR
      COMMON / DIAG / IM,IC,IE
      COMMON / TIME / CHOUR,CMIN,CSEC,CDAY,CMON,CMONN,IYEAR,CYEAR
      COMMON / RESULT / QE, QZ, FLOW, QEN(2), FLOWN(2), COPRN(2)
      COMMON / CYCLNG / CORR_COP, COPCYC(2), I_CYCLE, I_VALVE, T_CYCLE
      COMMON / TLRNCE / TOL_COND, TOL_MASS, TOL_FRSH, TOL_FRZ, TOL_HX,
     .                  N_EVAP, N_COND
 
      COMMON / CHINA / INCTRL
      COMMON / MAPDAT / IMAP, ICOMP, ICOOL, EER, SIZE, DISPL, EFFC,
     .                  SPEEDN, IREAD
      COMMON / CONDEN / UDSC,UTPC,USCC,ATOTC,UACOND
      COMMON / LIQLIN / FFREFQ, FZREFQ, CONDHT(2), CONDVP(2)
      COMMON / CYCLIC / DFSTCYC, FFCYC, FZCYC, OUTCYC
C
      DATA ITMAXC/100/, ITMAXE/40/
      DATA HSTATE /'COMP IN','COMP DIS','COND IN','COND DEW',
     .     'COND BUB','COND OUT','LIQ LINE',
     .     'SUBCOOL1','SUBCOOL2','FREZ IN ','FREZ OUT','FRSH IN ',
     .     'FRSH DEW','FRSH OUT','HX1 OUT '/
 
      DATA mSTATE /'COMP IN','COMP DIS','COND IN','COND DEW',
     .     'COND BUB','COND OUT','LIQ LINE',
     .     'SUBCOOL ','EVAP IN ', 'EVAP DEW','EVAP OUT','HX OUT  '/
 
      DATA T,S,H,XQ,P,V,XL,XV/256*-9999999.9/
      DATA FSUPC /0.1/
      DATA LPNT/1,2,14,3,11,4,16,6,10,8,9,5,12,7,13/
      DATA CMONTH / 'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     .              'SEP','OCT','NOV','DEC' /
      DATA IRUN/'00','01','02','03','04','05','06','07','08','09',
     .          '10','11','12','13','14','15','16','17','18','19',
     .          '20','21','22','23','24','25','26','27','28','29',
     .          '30','31','32','33','34','35','36','37','38','39',
     .          '40','41','42','43','44','45','46','47','48','49',
     .          '50','51','52','53','54','55','56','57','58','59',
     .          '60','61','62','63','64','65','66','67','68','69',
     .          '70','71','72','73','74','75','76','77','78','79',
     .          '80','81','82','83','84','85','86','87','88','89',
     .          '90','91','92','93','94','95','96','97','98','99'/
      DATA AIRTMP /.FALSE., .FALSE., .TRUE.,  .FALSE., .FALSE.,
     .             .TRUE.,  .FALSE., .FALSE., .FALSE., .FALSE.,
     .             .FALSE., .TRUE.,  .FALSE., .TRUE.,  .FALSE./
      DATA IO/8/, IM/9/
      DATA TEMIN /210.0/
 
      CALL GETCOL('OLD_MODEL=YES$','SETUP.DAT ',IRET)
      IF (IRET .EQ. 1) THEN
         IRET = 7
      ELSE
         IRET = 6
      END IF
 
      !! Don't allow adjustment to freezer evap if not one-door refrigerator
      IF (IRFTYPE .NE. 6) IRET = 7
C
C          GET DATE AND TIME
C
      irftyp = irftype
      if (irftype .eq. 7) irftyp = 1
      IF(NCYC .NE. 2) THEN
           CALL GETDAT(IYEAR,IMONTH,IDAY)
           CALL GETTIM(IHOUR,IMIN,ISEC)
           IHOUR = IHOUR + 1
           IMIN  = IMIN + 1
           ISEC  = ISEC + 1
           IDAY  = IDAY + 1
           CHOUR = IRUN(IHOUR)
           CMIN  = IRUN(IMIN)
           CSEC  = IRUN(ISEC)
           CDAY  = IRUN(IDAY)
           CMON  = CMONTH(IMONTH)
           CMONN = IRUN(IMONTH+1)
 
           IPNT = IYEAR - 1900
           IF (IPNT .GE. 100) IPNT = IPNT - 100
           CYEAR = IRUN(IPNT + 1)
 
      END IF
C
C          SET UP LOGICAL VECTOR ON AIR TEMPERATURES
C
      IF(ICYCL .EQ. 2) THEN
           AIRTMP(10) = .TRUE.
           AIRTMP(11) = .TRUE.
      ELSE
           AIRTMP(10) = .FALSE.
           AIRTMP(11) = .FALSE.
      END IF
C
C          OPEN OUTPUT FILE
C
      IF (NCYC .EQ. 1 .AND. ITYPE .LT. 3) THEN
           OPEN(IO,FILE='CYCLE.OUT',STATUS='UNKNOWN')
           OPEN(IM,FILE='ERROR.OUT',STATUS='UNKNOWN')
      ELSE
           OPEN(IO,FILE='CYCLE.OUT',STATUS='OLD',ACCESS='APPEND')
           OPEN(IM,FILE='ERROR.OUT',STATUS='OLD',ACCESS='APPEND')
      END IF
      WRITE(IM,'(''ENTERING SUBROUTINE CYCLE''/)')
      WRITE(IO,'(A1)') CHAR(12)
      IF(NCYC .NE. 2) THEN
           WRITE(IO,1200) IRUN(IHOUR),IRUN(IMIN),IRUN(ISEC),IRUN(IDAY),
     .                    CMONTH(IMONTH),IYEAR
      END IF
C
C          OUTPUT INFORMATION ON TYPE OF CYCLE
C
      SELECT CASE (ICYCL)
           CASE (1)
                IF(ICYCLS .EQ. 1) THEN
                   WRITE(IO,'('' STANDARD ONE EVAPORATOR CYCLE'')')
                ELSE
                  IF(ITYPE .EQ. 1 .OR. ITYPE .EQ. 4) THEN
                     WRITE(IO,'('' DUAL EVAP CYCLE: FRESH FOOD LOOP'')')
                  ELSE
                     WRITE(IO,'('' DUAL EVAP CYCLE: FREEZER LOOP'')')
                  END IF
                END IF
 
           CASE (2)
                IF(ICYCLS .EQ. 2) THEN
                   WRITE(IO,'('' LORENZ CYCLE'')')
                ELSE
                   WRITE(IO,'('' DUAL EVAP CYCLE'')')
                END IF
 
           CASE (3)
                IF(NCYC .EQ. 1) THEN
                     WRITE(IO,'('' DUAL LOOP CYCLE - FREEZER'')')
                ELSE
                     WRITE(IO,1099)
                     WRITE(IO,'('' DUAL LOOP CYCLE - FRESH FOOD'')')
                END IF
 
      END SELECT
      WRITE(IO,1099)
 
      IF(ITYPE .EQ. 3) ITYPE = 1
C
C          OUTPUT REFRIGERATION MIXTURE INFORMATION
C
      X2 = 100.0*XM(1)
      WRITE(IO,1097) X2,HREF(IR(1))
      IF(NC .GT. 1) THEN
           I = 2
           DO WHILE (I .LE. NC)
                X2 = 100.0*XM(I)
                WRITE(IO,1098) X2,HREF(IR(I))
                I = I + 1
           END DO
      END IF
      WRITE(IO,1099)
      WRITE(IO,1100)
      WRITE(IO,'(1X)')
C
C          INITIALIZE COMPRESSOR MAP ANALYSIS
C
      dutyr = 0.5
      IREAD = 0
      DISPL = DISPLC
      IF(IMAP .EQ. 1) THEN
         CALL MAP(ICOMP, ICOOL, EER, SIZE, DISPL, EFFC, CE, SPEEDN)
      END IF
C
C          INITIALIZE THERMODYNAMIC DATA
c
      CALL BCONST (NC,IR,F)
 
C
C          CONVERT TO MOLAR COMPOSITION FOR THE CALCULATION OF
C          ALL PROPERTIES
C
      WMSUM=0.0
      WMAVG=0.0
      I = 1
      DO WHILE (I .LE. NC)
           WMSUM = WMSUM+XM(I)/WM(I)
           I = I + 1
      END DO
      I = 1
      DO WHILE (I .LE. NC)
           X(I) = XM(I)/WM(I)/WMSUM
           WMAVG = WMAVG+X(I)*WM(I)
           I = I + 1
      END DO
 
C
C          INITIAL GUESSES FOR TC AND TE
C
C          ASSUME TEMP RISE OF COND IS 0.5 F PER LBM
C
      TC(1) = TS1 + MREF/3.6
 
C----------NEW CODE FOR EVAPORATOR WITH INCOMPLETE EVAPORATION----------
C
C     TE(1) = TS3 - 6.0  -  OLD REPLACED CODE
C
C          GUESS A DEW POINT TEMPERATURE AT THE EVAPORATOR EXIT
C
      IF(ISPEC .EQ. 1) THEN                                !Evap superheat
                                                           !specified
         T(15) = TS3 - (DTSUPE+2.0)
         CALL BUBLT(T(15),XL(1,15),X,P(15),VL(15),V(15),.FALSE.,LCRIT)
         TE(1) = T(15) + DTSUPE
         T(7) = TE(1)
         P(7) = P(15)
      END IF
C
      IF(ISPEC .EQ. 2) THEN                                !Interchanger super-
                                                           !heat specified
         T(15) = TS3 - 2.0
         T(13) = T(15) + DTSUPI
         IF(T(13) .GT. TC(1)) THEN
            T(13) = TC(1) - 5.0
            T(15) = T(13) - DTSUPI
         END IF
         CALL BUBLT(T(15),XL(1,15),X,P(15),VL(15),V(15),.FALSE.,LCRIT)
         TE(1) = T(15)
         T(7) = TE(1)
         P(7) = P(15)
      END IF
C
      IF(ISPEC .EQ. 3) THEN                                !Evap exit quality
                                                           !specified
         T(15) = TS3 - 2.0
         CALL BUBLT(T(15),XL(1,15),X,P(15),VL(15),V(15),.FALSE.,LCRIT)
         CALL BUBLP(P(15),X,XV15,TBUB15,VBUB15,VV15,.TRUE.,LCRIT)
         TE(1) = T(15) - (T(15)-TBUB15)*(1.0-XEXITE)
         T(7) = TE(1)
         P(7) = P(15)
      END IF
C--------------------------END OF NEW CODE (12/29/90)-------------------
      JC = 1
      LCCON = .TRUE.
      LQUIT = .FALSE.
C
C          SET UP TEMPERATURES AND CABINET LOADS FOR INLET TEMPERATURE
C          CALCULATION FOR EVAPORATOR OF A STANDARD DESIGN (TYPE 1)
C
      TFF = FFTEMP
      TFZ = FZTEMP
C
C          INITIAL GUESS FOR REFRIGERANT MASS FLOW (ASSUME 10 LB/HR)
C
      FLOW = MREF
      FLWREF = FLOW
      FLOW2 = FLOW
      MREF = MREF/(2.20462*WMAVG)
      MREFSV=MREF
C
C          BEGIN ITERATION FOR CONDENSER OUTLET TEMPERATURE
C
      CALL SHWFIG(ICYCL)
      ICONC = 0
C
C          BEGIN MAIN ITERATION LOOP ON THE CONDENSER TEMPERATURE
C
      IC = 1
      DO WHILE (IC .LE. ITMAXC .AND. LCCON)
           IF(ICAB .EQ. 1) THEN
              CALL ADJLOD(ICYCL,IC,TS3,TS5,FROSTF,FROSTZ,IDFRST)
           END IF
           T(4) = TC(JC)
C
C               FIND CONDENSER PRESSURE FOR CURRENT GUESS OF TC
C               IF BUBLT ROUTINE EXCEEDS THE CRITICAL POINT WRITE
C               A WARNING MESSAGE AND RETURN
C
           CALL GOTOXY(45,9)
           CALL PRINT(IC,2,-1)
           TBUB4 = TC(JC) + DTSUBC
           CALL BUBLT (TBUB4,X,XV(1,4),P(4),VBUB4,VV(4),.TRUE.,LCRIT)
C
C               DETERMINE THE SPECIFIC VOLUME OF THE LIQUID
C
           IF(DTSUBC .GT. 0.0) THEN
                VGUESS=VBUB4
                CALL ESPAR(0,TC(JC),X,A4,B4)
                CALL VIT(TC(JC),P(4),A4,B4,VGUESS,.TRUE.,LCONV)
                V(4)=VGUESS
           ELSE
                V(4)=VBUB4
           END IF
C
C               CONDENSER DEW POINT
C
           P(3) = P(4) + (1.0-FSUPC)*DPC
           TSHOW = TC(JC) - 273.11
           CALL GOTOXY(12,2)
           CALL PRINT(TSHOW,5,1)
           IF (LCRIT) THEN
                WRITE (IO,2204)
                CLOSE(IO)
                CLOSE(IM)
                RETURN
           END IF
C
C               ENTHALPY AT STATE 4 (CONDENSER OUTLET)
C
           CALL HCVCPS (1,TC(JC),V(4),X,H(4),CV,CP,VSND)
           JE=1
           LECON=.TRUE.
C
C          ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
C
           H(16) = H(4) - CONDHT(NCYC)/MREF/DUTYr
 !         H(16) = H(4) - CONDHT(NCYC)/FLOW2/DUTYC
           P(16) = P(4)
           CALL HPIN (H(16),P(16),X,T(16),XQ(16),XL(1,16),XV(1,16),
     .                VL(16),VV(16),HL16,HV16)
           IF(VL(16) .EQ. 0.0) VL(16) = V(4)
           V(16) = VL(16)
C
C          ENTER ITERATION FOR EVAPORATOR OUTLET TEMPERATURE
C
           IE = 1
           DO WHILE (IE .LE. ITMAXE .AND. LECON)
                IF(ICYCL .EQ. 2) THEN
                     CALL GOTOXY(45,17)
                ELSE
                     CALL GOTOXY(45,11)
                END IF
                CALL PRINT(IE,2,-1)
 
C----------------------------OLD CODE REPLACED BELOW-------------------
C                   FIND EVAPORATOR PRESSURE FOR CURRENT GUESS OF TE
C
C               TDEWE = TE(JE) - DTSUPE
C               CALL BUBLT (TDEWE,XL(1,7),X,P(7),VL(7),V(7),
C    .                      .FALSE.,LCRIT)
C               P(13) = P(7)
C               XL(1,13) = XL(1,7)
C               XQ(13) = 1.0
C               T(7) = TE(JE)
C               IF(ICYCL .EQ. 2) THEN
C                    CALL GOTOXY(14,11)
C               ELSE
C                    CALL GOTOXY(14,13)
C               END IF
C               TSHOW = T(7) - 273.11
C               CALL PRINT(TSHOW,5,1)
C
C                    INTERCHANGER FOR SUBCOOLING CONDENSER LIQUID
C
C               T(13) = TE(JE) + ETHX1*(TC(JC) - TE(JE))
C               TSHOW = T(13) - 273.11
C               IF(ICYCL .EQ. 2) THEN
C                    CALL GOTOXY(14,5)
C               ELSE
C                    CALL GOTOXY(14,6)
C               END IF
C               CALL PRINT(TSHOW,5,1)
C               CALL HCVCPS (1,TE(JE),V(7),X,H(7),CV,CP,VS)
C               VGUESS=V(7)*T(13)/T(7)
C               CALL ESPAR(0,T(13),X,A13,B13)
C               CALL VIT(T(13),P(13),A13,B13,VGUESS,.FALSE.,LCONV)
C               V(13)=VGUESS
C               CALL HCVCPS (1,T(13),V(13),X,H(13),CV,CP,VS)
C               CALL HPIN (H(13),P(13),X,T13,XQ13,XL(1,13),XV(1,13),
C    .               VL13,V(13),HL,HV)
C               CALL HCVCPS (1,T(13),V(13),X,H(13),CV,CP,VS)
C               H(6) = H(4) - (H(13) - H(7))
C               IF(H(6) .GT. H(4)) H(6) = 0.9*H(6)
C
C                    FIND CONDITIONS AT EVAPORATOR INLET ASSUMING
C                    ISENTHALPIC EXPANSION
C
C               P(5)=P(13)+DPE
C               P(6) = P(4)
C               CALL HPIN (H(6),P(6),X,T(6),XQ(6),XL(1,6),XV(1,6),
C    .                     V(6),VV6,HL,HV)
C               VL(6) = V(6)
C               TSHOW = T(6) - 273.11
C               IF(ICYCL .EQ. 2) THEN
C                    CALL GOTOXY(2,13)
C               ELSE
C                    CALL GOTOXY(2,13)
C               END IF
C               CALL PRINT(TSHOW,5,1)
C----------------------END OF OLD CODE REPLACED BELOW-------------------
C
C--------------------NEW CODE FOR INCOMPLETE EVAPOPRATION---------------
C
                I_ERROR_INTER = 0
                SELECT CASE (ISPEC)
                   CASE (1)                                !Evap superheat
                      TE(JE) = T(15) + DTSUPE
                      CALL BUBLT(T(15),XL(1,15),X,P(15),VL(15),V(15),
     .                           .FALSE.,LCRIT)
                      P(7) = P(15)
                      XQ(15) = 1.0
                      XQ(7) = XQ(15)
 
                   CASE (2)                                !IHX superheat
                      CALL BUBLT(T(15),XL(1,15),X,P(15),VL(15),V(15),
     .                           .FALSE.,LCRIT)
                         P(13) = P(15)
                         T(13) = T(15) + DTSUPI
                         VGUESS = V(15)*T(13)/T(15)
                         CALL ESPAR(0,T(13),X,A13,B13)
                         CALL VIT(T(13),P(13),A13,B13,VGUESS,
     .                            .FALSE.,LCONV)
                         V(13) = VGUESS
                         CALL HCVCPS(1,T(13),V(13),X,H(13),CV,CP,VS)
                         P(7) = P(15)
                         TE(JE) = T(7)
                         IF(T(13) .GE. T(16)) THEN
                            LECON = .FALSE.
                            I_ERROR_INTER = 1
                            CYCLE
                         END IF
 
                   CASE (3)                                !Evap quality
                      XQ(15) = 1.0
                      CALL BUBLT(T(15),XL(1,15),X,P(15),VL(15),V(15),
     .                           .FALSE.,LCRIT)
                      P(7) = P(15)
 
                END SELECT
C
C          DETERMINE THE BUBBLE POINT AT THE EVAP EXIT PRESSURE
C
                CALL BUBLP(P(15),X,XV15,TBUB15,VBUB15,VV15,.TRUE.,LCRIT)
C
C          DETERMINE THE BUBBLE AND DEW POINT ENTHALPIES
C
                CALL HCVCPS(1,T(15),V(15),X,H(15),CV,CP,VS)
                CALL HCVCPS(1,TBUB15,VBUB15,X,HBUB15,CV,CP,VS)
C
                INC=1
                DO WHILE(INC .LE. NC)
                   XL(INC,13) = XL(INC,15)
                   INC = INC + 1
                END DO
                XQ(13) = 1.0
C
C
C          DETERMINE THE ENTHALPY AT (7)
C
                SELECT CASE (ISPEC)
                   CASE (1)                                !Exit superheat
                      VGUESS = V(15)*T(7)/T(15)
                      CALL ESPAR(0,T(7),X,A7,B7)
                      CALL VIT(T(7),P(7),A7,B7,VGUESS,.FALSE.,LCONV)
                      V(7) = VGUESS
                      CALL HCVCPS(1,T(7),V(7),X,H(7),CV,CP,VS)
                      XQ(7) = 1.0
                      VV(7) = V(7)
                      T(7) = TE(JE)
 
                      INC = 1
                      DO WHILE(INC .LE. NC)
                         XL(INC,7) = 0.0
                         XV(INC,7) = X(INC)
                         INC = INC + 1
                      END DO
 
                   CASE (2)                                !IHX superheat
                      CALL INTER2(X,P(16),T(16),H(16),V(16),P(13),H(13),
     .                           T(15),H(15),V(15),ETHX1,T(7),H(7),QINT)
                      CALL HPIN(H(7),P(7),X,T(7),XQ(7),XL(1,7),XV(1,7),
     .                          VL(7),VV(7),HL7,HV7)
                      V(7) = (1.0-XQ(7))*VL(7) + XQ(7)*VV(7)
                      TE(JE) = T(7)
 
                   CASE (3)                                !Exit quality
                      XQ(7) = XEXITE
                      CALL ENTHAL(HBUB15,H(15),XQ(7),X,P(7),H(7))
                      CALL HPIN(H(7),P(7),X,T(7),XQ(7),XL(1,7),XV(1,7),
     .                          VL(7),VV(7),HL7,HV7)
                      V(7) = (1.0-XQ(7))*VL(7) + XQ(7)*VV(7)
                      T(7) = TE(JE)
                END SELECT
C
C
                IF(ICYCL .EQ. 2) THEN
                   CALL GOTOXY(14,11)
                ELSE
                   CALL GOTOXY(14,13)
                END IF
                TSHOW = T(7) - 273.11
                CALL PRINT(TSHOW,5,1)
C
C          INTERCHANGER FOR SUBCOOLING CONDENSER LIQUID
C
                IF(ISPEC .NE. 2) THEN
                   CALL INTER1(X,P(16),T(16),H(16),V(16),P(7),T(7),H(7),
     .                         V(7),ETHX1,QINT)
                   H(13) = H(7) + QINT
                END IF
 
                H(6) = H(16) - QINT
C
C          DETERMINE THE REMAINDER OF THE PROPERTIES AT (6) AND (13)
C
                P(6) = P(4)
                P(13) = P(7)
c               CALL HPIN (H(6),P(6),X,T(6),XQ6,XL(1,6),XV(1,6),VL(6),
c    .                     V(6),HL,HV)
c               vl(6) = v(6)  !Tony
                CALL HPIN (H(6),P(6),X,T(6),XQ6,XL(1,6),XV(1,6),V(6),
     .                     vv6,HL,HV)
                vl(6) = v(6)  !Tony
                XQ(6) = 0.0
 
                IF(ISPEC .NE. 2) THEN
                   CALL HPIN (H(13),P(13),X,T(13),XQ13,XL(1,13),
     .                        XV(1,13),VL13,V(13),HL,HV)
                END IF
 
                XQ(13) = 1.0
C
C
                IF(ICYCL .EQ. 2) THEN
                   CALL GOTOXY(2,5)
                ELSE
                   CALL GOTOXY(2,6)
                END IF
                TSHOW = T(16) - 273.11
                CALL PRINT(TSHOW,5,1)
 
                IF(ICYCL .EQ. 2) THEN
                   CALL GOTOXY(14,5)
                ELSE
                   CALL GOTOXY(14,6)
                END IF
                TSHOW = T(13) - 273.11
                CALL PRINT(TSHOW,5,1)
C
C          FIND CONDITIONS AT EVAPORATOR INLET ASSUMING ISENTHALPIC
C          EXPANSION
C
                P(5) = P(13) + DPE
                TSHOW = T(6) - 273.11
                CALL GOTOXY(2,13)
                CALL PRINT(TSHOW,5,1)
C
C-----------------------END OF NEW CODE (12/29/90)----------------------
C
C                    CALL ANALYSIS OF FREEZER SECTION AND THE LOWER
C                    INTERCHANGER (LOOKS LIKE A NON-ADIABATIC
C                    EXPANSION VALVE TO REST OF SYSTEM
C
                CALL LOWEVP(ICYCL,ICNTRL,H,P,X,T,XQ,XL,XV,VL,VV,HL,HV,
     .                      TS3,TS5,TS6,DPF,ETHX2,QFREZ,LQUIT)
C
C                    CALCULATE FRESH FOOD SECTION HEAT EXCHANGE
C
                PDEWE = P(5) - (1.0-FSUPE)*DPE
                IF(PDEWE .GT. P(5)) PDEWE = P(5)
                CALL BUBLP(PDEWE,XREF,X,TDEW,VLDEW,VDEW,.FALSE.,LCRIT)
                POLDE = PDEWE
                IF(TDEW .GE. TS3) TDEW = TS3 - 1.0
                IF(T(5) .GE. TS3) T(5) = TS3 - 1.0
                CALL HCVCPS(3,TDEW,VDEW,X,HDEW,CVRVAP,CPRVAP,VS)
C
C                    STATE 12 IS THE POINT AT WHICH THE DEW POINT IS REACHED
C                    IN THE EVAPORATOR
C
                P(12) = PDEWE
                T(12) = TDEW
                V(12) = VDEW
                H(12) = HDEW
                XQ(12) = 1.0
 
C
C                    FIND DUTY CYCLE, NET CAPACITY AND AVERAGE
C                    FREEZER LOAD IF ICYCL = 1
C
                IF(IC .NE. 1) THEN
                     CALL DUTFND(ICAB,IRFTYP,ICYCL,NCYC,QFRSH,QFREZ,
     .                           FROSTF,FROSTZ,QFF,QFZ,TS3,TS5,T,IDFRST,
     .                           dutyr)
                END IF
C
C                    CALCULATE FRESH FOOD EVAPORATOR HEAT TRANSFER.
C
C                    TEST FOR STANDARD DESIGN.
C
                IF(IRFTYP .LE. 3) THEN
                     IF(ICYCL .EQ. 1 .AND. ICAB  .NE. 0
     .                               .AND. IFRSH .NE. 0) THEN
                          IF(IC .EQ. 1) THEN
                               TIN = 0.15*TFF + 0.85*TFZ
                          ELSE
                               CAPE = QFRSH/1.0548 - 3.413*FANE
     .                              - 3.413*(DFSTCYC + FZCYC)
                               CFMA = CFME/(1.08*1.8961)
                               QFM = QFF +3.413*DUTYC*FFCYC
                               CALL MIXAIR(CAPE,QFM,QFZ,TFF,TFZ,
     .                                     CFMA,TIN,FF_FRACT)
                          END IF
                     TS3 = (TIN + 459.6)/1.8
                     END IF
                END IF
                IF(IFRSH .EQ. 0) THEN
C
C                         NATURAL CONVECTION
C
                     CALL FFNAT(T(5),H(5),T(7),TDEW,HDEW,TS3,CPRVAP,
     .                          QFRSH,FSUPE, IRET)
  !  .                          QFRSH,FSUPE, IRFTYP)
                END IF
                IF(IFRSH .EQ. 1) THEN
C
C                         CROSS FLOW
C
                     CALL FFCROSS(H(5),T(5),HDEW,TDEW,TS3,CPRVAP,
     .                            P(5), P(7), X, N_EVAP, QFRSH, FSUPE)
C
                END IF
                IF(IFRSH .EQ. 2) THEN
C
C                         COUNTERFLOW
C
                     CALL FFCOUNT(H(5),T(5),HDEW,TDEW,TS3,CPRVAP,
     .                            P(5), P(7), X, N_EVAP, QFRSH,FSUPE)
                END IF
C
C                    SUPERHEATING FRACTION
C
                FSHOLD = FSUPE
                FSUPE = (FSHOLD + FSUPE)/2.0
                IF(FSUPE .GT. 1.05*FSHOLD) FSUPE = 1.05*FSHOLD
                IF(FSUPE .LT. 0.95*FSHOLD) FSUPE = 0.95*FSHOLD
C
C                    FRESH FOOD SECTION EVAPORATOR
C
                CALL FRSH(H,T,TS3,TS4,TE,JE,QFRSH,ICONE)
C
C---------------------------ADDED NEW CODE (12/29/90)-------------------
                T(15) = T(15) + TE(2) - T(7)
C-----------------------------END OF NEW CODE---------------------------
C
C
C                    CALCULATE THE AVERAGE EFFECTIVENESS OF THE FF EVAPORATOR
C                    CALCULATE THE HEAT TRANSFER IF THE REFRIGERANT LEFT AT TS3
C
                VGUESS = VDEW*TS3/TDEW
                CALL ESPAR(0,TS3,X,AS3,BS3)
                CALL VIT(TDEW,P(7),AS3,BS3,VGUESS,.FALSE.,LCONV)
                VS3 = VGUESS
                CALL HCVCPS(1,TS3,VS3,X,HS3,CV,CP,VS)
                QRMAX = MREF*(HS3-H(5))
C
C                    CALCULATE THE HEAT TRANSFER IF THE AIR LEFT AT T(5)
C
                QAMAX = CFME*(TS3-T(5))
                QMAXE = QAMAX
                IF(QRMAX.LT.QAMAX) QMAXE = QRMAX
                ETAE = QFRSH/QMAXE
                IF(ICONE .EQ. 1) LECON = .FALSE.
                IF(TE(JE) .LE. TEMIN) LECON = .FALSE.
                IE = IE + 1
           END DO
C
C               END OF EVAPORATOR ITERATION
C
           T(7) = TE(JE)
           IF(ICYCL .EQ. 2) THEN
                CALL GOTOXY(14,11)
           ELSE
                CALL GOTOXY(14,13)
           END IF
           TSHOW = T(7) - 273.11
           CALL PRINT(TSHOW,5,1)
C
C               INTERCHANGER FOR SUBCOOLING CONDENSER LIQUID
C
C------------------------OLD CODE REPLACED BY CODE BELOW----------------
C
C          T(13) = TE(JE) + ETHX1*(TC(JC) - TE(JE))
C          TSHOW = T(13) - 273.11
C          IF(ICYCL .EQ. 2) THEN
C               CALL GOTOXY(14,5)
C          ELSE
C               CALL GOTOXY(14,6)
C          END IF
C          CALL PRINT(TSHOW,5,1)
C          CALL HCVCPS (1,TE(JE),V(7),X,H(7),CV,CP,VS)
C          VGUESS = V(7)*T(13)/T(7)
C          CALL ESPAR(0,T(13),X,A13,B13)
C          CALL VIT(T(13),P(13),A13,B13,VGUESS,.FALSE.,LCONV)
C          V(13) = VGUESS
C          CALL HCVCPS (1,T(13),V(13),X,H(13),CV,CP,VS)
C          CALL HPIN (H(13),P(13),X,T13,XQ13,XL(1,13),XV(1,13),VL13,
C    .                V(13),HL,HV)
C          CALL HCVCPS (1,T(13),V(13),X,H(13),CV,CP,VS)
C          H(6) = H(4) - (H(13) - H(7))
C          TE(1) = TE(JE)
C-----------------------------END OF OLD CODE---------------------------
C-----------------NEW CODE ADDED FOR INCOMPLETE EVAPORATION-------------
 
           SELECT CASE (ISPEC)
              CASE (1)                                     !Exit superheat
                 VGUESS = V(15)*T(7)/T(15)
                 CALL ESPAR(0,T(7),X,A7,B7)
                 CALL VIT(T(7),P(7),A7,B7,VGUESS,.FALSE.,LCONV)
                 V(7) = VGUESS
                 CALL HCVCPS(1,T(7),V(7),X,H(7),CV,CP,VS)
                 XQ(7) = 1.0
                 VV(7) = V(7)
                 T(7) = TE(JE)
 
                 INC = 1
                 DO WHILE(INC .LE. NC)
                    XL(INC,7) = 0.0
                    XV(INC,7) = X(INC)
                    INC = INC + 1
                 END DO
 
              CASE (2)                                     !IHX Superheat
                 CALL INTER2(X,P(16),T(16),H(16),V(16),P(13),H(13),
     .                       T(15),H(15),V(15),ETHX1,T(7),H(7),QINT)
 
                 CALL HPIN(H(7),P(7),X,T(7),XQ(7),XL(1,7),XV(1,7),VL(7),
     .                     VV(7),HL7,HV7)
                 V(7) = (1.0-XQ(7))*VL(7) + XQ(7)*VV(7)
                 TE(JE) = T(7)
 
              CASE (3)                                     !Exit quality
                 XQ(7) = XEXITE
                 CALL ENTHAL(HBUB15,H(15),XQ(7),X,P(7),H(7))
                 CALL HPIN(H(7),P(7),X,T(7),XQ(7),XL(1,7),XV(1,7),VL(7),
     .                     VV(7),HL7,HV7)
                 V(7) = (1.0-XQ(7))*VL(7) + XQ(7)*VV(7)
                 T(7) = TE(JE)
 
           END SELECT
 
           IF(ISPEC .NE. 2) THEN
              CALL INTER1(X,P(16),T(16),H(16),V(16),P(7),T(7),H(7),V(7),
     .                    ETHX1,QINT)
              H(13) = H(7) + QINT
           END IF
 
           H(6) = H(16) - QINT
c          CALL HPIN (H(6),P(6),X,T(6),XQ6,XL(1,6),XV(1,6),VL(6),
c    .                V(6),HL,HV)
c-----
           CALL HPIN (H(6),P(6),X,T(6),XQ6,XL(1,6),XV(1,6),V(6),
     .                Vv6,HL,HV)
           vl(6) = v(6)
c-------
 
           XQ(6) = 0.0
 
           IF(ISPEC .NE. 2) THEN
              CALL HPIN (H(13),P(13),X,T(13),XQ13,XL(1,13),XV(1,13),
     .                   VL13,V(13),HL,HV)
           END IF
C
           XQ(13) = 1.0
 
           TSHOW = T(13) - 273.11
           IF(ICYCL .EQ. 2) THEN
                CALL GOTOXY(14,5)
           ELSE
                CALL GOTOXY(14,6)
           END IF
           CALL PRINT(TSHOW,5,1)
 
           TE(1) = TE(JE)
C--------------------------END OF NEW CODE (12/29/90)-------------------
C
C               FIND ENTROPY AT COMPRESSOR INLET AND COMPUTE CONDITIONS AT
C               COMPRESSOR OUTLET (TSPEC IS DEFINED INLET TEMP TO THE
C               COMPRESSOR (-1 MEANS NO TEMPERATURE CHANGE)
C
           P(1) = P(13)
           IF(TSPEC .GT. 0.0) THEN
                T(1) = TSPEC
                VGUESS = V(7)*T(1)/T(7)
                CALL ESPAR(0,T(1),X,A1,B1)
                CALL VIT(T(1),P(1),A1,B1,VGUESS,.FALSE.,LCONV)
                V(1) = VGUESS
                CALL HCVCPS (1,T(1),V(1),X,H(1),CV,CP,VS)
           ELSE
                T(1) = T(13)
                H(1) = H(13)
                V(1) = V(13)
                XQ(1) = XQ(13)
           END IF
           S(1) = ENTROP(T(1),V(1),X)
           P(2) = P(3) + FSUPC*DPC
           TSHOW = T(1) - 273.11
           IF(ICYCL .EQ. 2) THEN
                CALL GOTOXY(43,5)
           ELSE
                CALL GOTOXY(43,6)
           END IF
           CALL PRINT(TSHOW,5,1)
C
C CALL COMPRESSOR MODEL
C
           IF(IMAP.EQ.0) THEN
C
C CALL TABULAR MAP BASED MODEL
C
           OLDMAS = MREF
           CALL COMPCALL(H, P, X, T, CV, CP, HOUT, MEFF,
     .               QHILO, QCAN, VSUC, V, VV2, TSUC, TDISC,
     .               TS1, GAMA, RN, ETAS)
           mref = (mref + 2.0*oldmas)/ 3.0
           IF(MREF .GT. 1.05*OLDMAS) MREF = 1.05*OLDMAS
           IF(MREF .LT. 0.95*OLDMAS) MREF = 0.95*OLDMAS
  !        IF(MREF .GT. 1.02*OLDMAS) MREF = 1.02*OLDMAS
  !        IF(MREF .LT. 0.98*OLDMAS) MREF = 0.98*OLDMAS
           FLOW2 = FLWREF*MREF/MREFSV
C
           END IF
C
           IF(IMAP.EQ.1.OR.IMAP.EQ.2) THEN
C
C CALL THEORETICALLY BASED MODEL
C
           CALL COMP(H, P, X, T, CV, CP, HOUT, MEFF, QHILO, QCAN,
     .               VSUC, V, VV2, TSUC, TDISC, TS1, GAMA, RN, ETAS)
C
C               UPDATE THE MASS FLOW TO CORRESPOND TO THE DISPLACEMENT
C
           DISPI = DISP/1.6387E-05
           OLDMAS = MREF
   !            write(*,*) 'oldmas ,mref', oldmas, mref
           MREF = (MREF*(DISPLC/DISPI)+2.0*OLDMAS)/3.0
           IF(MREF .GT. 1.05*OLDMAS) MREF = 1.04*OLDMAS
           IF(MREF .LT. 0.95*OLDMAS) MREF = 0.96*OLDMAS
           FLOW2 = FLWREF*MREF/MREFSV
 
   !       write(*,*) " mref, oldmas: ", mref, oldmas
C
           END IF
C
C               SHOW THE PROGRESS IN THE SOLUTION
C
           CALL PROGRS(ICYCL,H,HOUT,WMAVG,FLOW2,QCAN)
C
C               CONDITIONS OF GAS LEAVING COMPRESSOR SHELL
C
           CALL HCVCPS (1,T(2),VV2,X,H(2),CV,CP,VS)
           CALL HPIN (H(2),P(2),X,T(2),XQ(2),XL(1,2),
     .                XV(1,2),VL2,VV2,HL2,HV2)
           V(2) = VV2
C
C               ENTROPY OF GAS LEAVING COMPRESSOR
C
           IF (XQ(2) .LT. 1.0) THEN
                SL2 = ENTROP(T(2),VL2,XL(1,2))
                SV2 = ENTROP(T(2),VV2,XV(1,2))
                S(2) = XQ(2)*SV2 + (1.0-XQ(2))*SL2
           ELSE
                S(2) = ENTROP(T(2),VV2,X)
           END IF
           TSHOW = T(2) - 273.11
           CALL GOTOXY(63,2)
           CALL PRINT(TSHOW,5,1)
           IF(ICONC .NE. 1) THEN
C
C                    CALCULATE CONDENSER HEAT EXCHANGE
C                    DETERMINE THE DEW POINT CONDITIONS
C
                CALL BUBLP(P(3),XL(1,3),X,TDEW,VL,VDEW,.FALSE.,LCRIT)
                T(3) = TDEW
                V(3) = VDEW
                CALL HCVCPS(3,TDEW,VDEW,X,HDEW,CVRVAP,CPRVAP,VS)
                H(3) = HDEW
C
C                    DETERMINE BUBBLE POINT CONDITIONS
C                    ASSUME A LINEAR PRESSURE DROP THROUGHOUT THE CONDENSER
C
                PBUB = P(4) + DPC*FSUBC
                CALL BUBLP(PBUB,X,XV(1,4),TBUB,VBUB,V4,.TRUE.,LCRIT)
                CALL HCVCPS(3,TBUB,VBUB,X,HBUB,CVRLIQ,CPRLIQ,VS)
C
C                    STATE 11 IS THE POINT AT WHICH THE BUBBLE POINT
C                    IS REACHED IN THE CONDENSER
C
                P(11) = PBUB
                T(11) = TBUB
                V(11) = VBUB
                H(11) = HBUB
C
C                    DETERMINE CONDITIONS ENTERING THE CONDENSER
C
                HDROP = CONDVP(NCYC)/MREF/DUTYC
 !              HDROP = CONDVP(NCYC)/FLOW2/DUTYC
 
                P(14) = P(2)
                HINCND = H(2) - HDROP
 !              IF(HINCND .GE. H(3)) THEN
 !                 T(14) = T(2) - TDROPC
 !                 XQ(14) = 1.0
 !                 VGUESS = V(2)*T(14)/T(2)
 !                 CALL ESPAR(0,T(14),X,A14,B14)
 !                 CALL VIT(T(14),P(14),A14,B14,VGUESS,.FALSE.,LCONV)
 !                 V(14) = VGUESS
 !                 CALL HCVCPS(1,T(14),V(14),X,H(14),CV,CP,VS)
 !              ELSE
                   H(14) = HINCND
                   CALL HPIN(H(14),P(2),X,T(14),XQ(14),XL(1,14),
     .                       XV(1,14),VL(14),VV(14),HL14,HV14)
                   V(14) = (1.0-XQ(14))*VL(14) + XQ(14)*VV(14)
 !!             END IF
C
                IF(ICOND .EQ. 0) THEN
C
C                         NATURAL CONVECTION
C
                     CALL CNAT(TS1,TS3,TS5,T(14),H(14),TDEW,HDEW,T,
     .                         TBUB,HBUB,CPRLIQ,QDSC,QTPC,QSCC,QTOTC,
     .                         FSUPC,FSUBC)
                END IF
C
                IF(ICOND .EQ. 1) THEN
C
C                         CROSS FLOW
C
                     CALL CCROSS(TS1, T(14), H(14), TDEW, HDEW, TBUB,
     .                           HBUB, CPRLIQ, CPRVAP, PBUB, P(4), X,
     .                           N_COND, QDSC, QTPC, QSCC, QTOTC,
     .                           FSUPC, FSUBC)
                END IF
C
                IF (ICOND .EQ. 2) THEN
C
C                         COUNTERFLOW
C
                     CALL CCOUNT(TS1, T(14), H(14), TDEW, HDEW, TBUB,
     .                           HBUB, CPRLIQ, CPRVAP, PBUB, P(4), X,
     .                           N_COND, QDSC, QTPC, QSCC, QTOTC,
     .                           FSUPC, FSUBC)
                END IF
 
                CALL COND(T,H,TBUB,HBUB,TS1,TS2,TC,CPRLIQ,QDSC,
     .                    QTPC,QSCC,JC,ICONC)
  !        write(*,*) " "
  !        write(*,*) " TCOND: TC(1), TC(2), TC(3)", TC
C
C          ACCOUNT FOR HEAT LOSS FROM LIQUID LINE
C
                H(16) = H(4) - CONDHT(NCYC)/MREF/DUTYr
 !              H(16) = H(4) - CONDHT(NCYC)/FLOW2/DUTYC
                P(16) = P(4)
                CALL HPIN (H(16),P(16),X,T(16),XQ(16),XL(1,16),XV(1,16),
     .                     VL(16),VV(16),HL16,HV16)
                IF(VL(16) .EQ. 0.0) VL(16) = V(4)
                V(16) = VL(16)
 
c               CALL COND2(T,H,TS1,TS2,TC,QDSC,QTPC,QSCC,CPRLIQ,
c    .                    JC,ICONC)
C
C                    CALCULATE THE AVERAGE EFFECTIVENESS OF THE HEAT EXCHANGER
C                    CALCULATE THE HEAT TRANSFER IF THE REFRIGERANT LEFT AT TS1
C
C                    DETERMINE THE SPECIFIC VOLUME OF THE LIQUID
C
                IF(TS1 .LT. T(4)) THEN
                     VGUESS = V(4)
                     CALL ESPAR(0,TS1,X,AS1,BS1)
                     CALL VIT(TS1,P(4),AS1,BS1,VGUESS,.TRUE.,LCONV)
                     VS1 = VGUESS
                ELSE
                     VS1 = V(4)
                END IF
                CALL HCVCPS(1,TS1,VS1,X,HS1,CV,CP,VS)
                QRMAX = MREF*(H(14)-HS1)
C
C                    CALCULATE THE HEAT TRANSFER IF THE AIR LEFT AT T(14)
C
                QAMAX = CFMC*(T(14)-TS1)
                QMAXC = QAMAX
                IF(QRMAX .LT. QAMAX) QMAXC = QRMAX
                ETAC = QTOTC/QMAXC
C
           ELSE
                LCCON = .FALSE.
           END IF
           IC = IC + 1
           SELECT CASE (INCTRL)                            !Adjust loads
              CASE (0, 3)
                 IF(IC .LE. 4) THEN
                    LCCON = .TRUE.
                    ICONC = 0
                 END IF
 
              CASE (1, 2, 4)
                 IF(IC .LE. 10) THEN
                    LCCON = .TRUE.
                    ICONC = 0
                 END IF
 
           END SELECT
           IF(LQUIT) LCCON = .FALSE.
      END DO
C
C          END OF CONDENSER ITERATION
C
C
C          ONCE CONDENSER AND EVAPORATOR HAVE CONVERGED COMPUTE
C          REMAINING PROPERTIES AT EACH STATE POINT IN THE CYCLE
C          AND PRINT RESULTS
C
C          BEGIN WITH HEAT TRANSFER FLUID TEMPERATURES
C
      T(4) = TC(JC)
      TS(1) = 0.0
      TS(4) = TS1
      TS(5) = TS4
      TS(6) =0.0
      TS(7) = TS3
      TS(8) = TS6
      TS(9) = TS5
      TS(10) =0.0
      TS(14) =TS2
      TS(16) = 0.0
      TGC = T(3) - T(4)
C
C          SPECIFIC VOLUMES
C
      V(5) = MIN(1.0,1.0-XQ(5))*VL(5) + MAX(0.0,XQ(5))*VV(5)
      V(8) = MIN(1.0,1.0-XQ(8))*VL(8) + MAX(0.0,XQ(8))*VV(8)
      V(9) = MIN(1.0,1.0-XQ(9))*VL(9) + MAX(0.0,XQ(9))*VV(9)
      V(10) = MIN(1.0,1.0-XQ(10))*VL(10) + MAX(0.0,XQ(10))*VV(10)
C
C          QUALITIES
C
      XQ(1) = 1.0
      XQ(3) = 1.0
      XQ(4) = 0.0
      XQ(6) = 0.0
      XQ(10) = 0.0
C
C          LIQUID AND VAPOR COMPOSITIONS AROUND THE CYCLE LOOP
C
      I = 1
      DO WHILE (I .LE. NC)
           XL(I,1) = 0.0
           XV(I,1) = X(I)
           XL(I,2) = 0.0
           XV(I,2) = X(I)
           XL(I,3) = 0.0
           XV(I,3) = X(I)
           XL(I,4) = X(I)
           XV(I,4) = 0.0
           XL(I,6) = X(I)
           XV(I,6) = 0.0
           XL(I,7) = 0.0
           XV(I,7) = X(I)
           XL(I,10) = X(I)
           XV(I,10) = 0.0
           XL(I,11) = X(I)
           XV(I,11) = 0.0
           XL(I,12) = 0.0
           XV(I,12) = X(I)
           XL(I,13) = 0.0
           XV(I,13) = X(I)
           XL(I,14) = 0.0
           XV(I,14) = X(I)
           XL(I,16) = X(I)
           XV(I,16) = 0.0
           I = I + 1
      END DO
C
C          ENTROPIES AROUND THE CYCLE
C
      J = 3
      DO WHILE (J .LE. 16)
           IF(J .NE. 5) S(J) = ENTROP(T(J),V(J),X)
           J = J + 1
      END DO
      SL5 = ENTROP(T(5),VL(5),XL(1,5))
      SV5 = ENTROP(T(5),VV(5),XV(1,5))
      S(5) = MIN(1.0,1.0-XQ(5))*SL5 + MAX(0.0,XQ(5))*SV5
      SL8 = ENTROP(T(8),VL(8),XL(1,8))
      SV8 = ENTROP(T(8),VV(8),XV(1,8))
      S(8) = MIN(1.0,1.0-XQ(8))*SL8 + MAX(0.0,XQ(8))*SV8
      SL9 = ENTROP(T(9),VL(9),XL(1,9))
      SV9 = ENTROP(T(9),VV(9),XV(1,9))
      S(9) = MIN(1.0,1.0-XQ(9))*SL9 + MAX(0.0,XQ(9))*SV9
C
C          CONVERT FROM MOLAR QUANTITIES TO MASS BASIS
C
      J = 1
      DO WHILE (J .LE. 16)
           WMAVGL(J) = 0.0
           WMAVGV(J) = 0.0
           I = 1
           DO WHILE (I .LE. NC)
                WMAVGL(J) = WMAVGL(J) + XL(I,J)*WM(I)
                WMAVGV(J) = WMAVGV(J) + XV(I,J)*WM(I)
                I = I + 1
           END DO
           I = 1
           DO WHILE (I .LE. NC)
                IF(XL(I,J) .GT. 0.0) XL(I,J) = XL(I,J)*WM(I)/WMAVGL(J)
                IF(XV(I,J) .GT. 0.0) XV(I,J) = XV(I,J)*WM(I)/WMAVGV(J)
                I = I + 1
           END DO
           V(J) = V(J)/WMAVG
           H(J) = H(J)/WMAVG
           S(J)=S(J)/WMAVG
           J = J + 1
      END DO
C
C          COMPUTE WORK, CAPACITY, COP, ETC.
C
      HOUT = HOUT/WMAVG
      VSUC = VSUC/WMAVG
      W = (HOUT - H(1))/(1.0 - QCAN)
      QE = H(7) - H(5)
      QC = H(14) - H(4)
      QZ = H(9) - H(8)
      COPR = (QE + QZ)/W
      TH = TS1
      TL1 = TS3
      TL2 = TS5
      DENOM = TH*(QE*(1./TL1-1./TH)+QZ*(1./TL2-1./TH))
      COPI = (QE+QZ)/DENOM
      PR = P(2)/P(1)
      TSUPC = T(2) - T(3)
C
C          CORRECT COP DUR TO CYCLING LOSSES
C
      IF(I_CYCLE .EQ. 0) THEN
         CORR_COP = 1
      ELSE
         TENV = (TROOM + 459.6)/1.8
         TMID_COND = (T(3) + T(11))/2.0
         TMID_EVAP = (T(8) + T(9))/2.0
         CALL CYCLOS(I_VALVE, T_CYCLE, TS3, TENV, TMID_EVAP, TMID_COND,
     .               DUTYC, CORR_COP)
      END IF
C
C          OUTPUT WARNING IF CONVERGENCE FORCED BY HOT KEY <F10>.
C
      IF(LQUIT) WRITE(IO,'('' CONVERGENCE FORCED AFTER '',I2,
     .                     '' ITERATIONS''/)') IC
C
C          PRINT ERROR MESSAGES IF NON-CONVERGENCE
C
      IF(LECON .OR. LCCON .OR. I_ERROR_INTER . GT. 0) THEN
           LWIND = 5
           IF(LECON .AND. LCCON) LWIND = 7
           CALL WINDOW(8,8+LWIND,20,60,32,1)
           IF (LECON) THEN
                TE(1) = TE(1) - 273.11
                TE(2) = TE(2) - 273.11
                WRITE (IO,2200) TE(1),TE(2)
                CALL GOTOXY(22,11)
                CALL PRINT('EVAPORATOR ITERATION DID NOT CONVERGE$',
     .                      37,-2)
           END IF
           IF (LCCON) THEN
                TC(1) = TC(1) - 273.11
                TC(2) = TC(2) - 273.11
                WRITE (IO,2202) TC(1),TC(2)
                CALL GOTOXY(22,6+LWIND)
                CALL PRINT('CONDENSER ITERATION DID NOT CONVERGE$',
     .                      36,-2)
           END IF
 
           IF (I_ERROR_INTER .GT. 0) THEN
                WRITE (IO,2203)
                CALL GOTOXY(22,6+LWIND)
                CALL PRINT('INTERCHANGER SUPERHEAT NOT POSSIBLE$',
     .                      36,-2)
           END IF
 
      CALL WARBLE
      CALL WARBLE
      CALL WARBLE
      READ(*,*)
      END IF
C
C          OUTPUT RESULTS.  BEGIN BY CONVERTING TO ENGLISH UNITS.
C
      TENV = (TROOM + 459.6)/1.8
  !   IF(T(16) .LT. TS1) I_LIQUID_LINE = 1
      IF(T(16) .LT. Tenv) I_LIQUID_LINE = 1
 
      WRITE (IO,1010)
      K = 1
      IF (icycl .eq. 2) then
         DO WHILE (K .LE. 15)
           J = LPNT(K)
           TS(J) = TS(J) - 273.11
           T(J) = T(J) - 273.11
           V(J) = V(J)/10.0
           IF(XQ(J) .GT. 1.0) XQ(J) = 1.0
           IF(XQ(J) .LT. 0.0) XQ(J) = 0.0
           IF(AIRTMP(K)) THEN
                WRITE (8,1020) K,HSTATE(K),TS(J),T(J),P(J),H(J),V(J),
     .               S(J),XL(1,J),XV(1,J),XQ(J)
           ELSE
                WRITE (8,1021) K,HSTATE(K),      T(J),P(J),H(J),V(J),
     .               S(J),XL(1,J),XV(1,J),XQ(J)
           END IF
           K = K + 1
         END DO
      else
 
         M = 0
         k = 0
         DO WHILE (M .LE. 14)
           M = M + 1
           J = LPNT(m)
           TS(J) = TS(J) - 273.11
           T(J) = T(J) - 273.11
           V(J) = V(J)/10.0
           IF(XQ(J) .GT. 1.0) XQ(J) = 1.0
           IF(XQ(J) .LT. 0.0) XQ(J) = 0.0
           if(m .ge. 9 .and. m .le. 11) cycle
           k = k + 1
           IF(AIRTMP(m)) THEN
                WRITE (8,1020) K,mSTATE(K),TS(J),T(J),P(J),H(J),V(J),
     .               S(J),XL(1,J),XV(1,J),XQ(J)
           ELSE
                WRITE (8,1021) K,mSTATE(K),      T(J),P(J),H(J),V(J),
     .               S(J),XL(1,J),XV(1,J),XQ(J)
           END IF
         END DO
      end if
 
C
C          NORMALIZE BY THE MASS FLOW
C
      FLOW = FLOW*MREF/MREFSV
      DISP = DISP/1.6387E-05
      W = 0.4302*W*FLOW*1.0548
      QZ = 0.4302*QZ*FLOW*1.0548
      QE = 0.4302*QE*FLOW*1.0548
      QC = 0.4302*QC*FLOW*1.0548
C
C          REST OF THE CONVERSIONS
C
      TSUC = TSUC - 273.11
      TDISC = TDISC - 273.11
      ETAE = 100.*ETAE
      FSUPE = 100.*FSUPE
      ETAF = 100.*ETAF
      ETAC = 100.*ETAC
      FSUBC = 100.*FSUBC
      FSUPC = 100.*FSUPC
C
C          OUTPUT SUMMARY TABLE OF RESULTS
C
      WRITE(8,1025)
      IF(ITYPE .EQ. 1) THEN
           WRITE(8,1105) QE, QE/3.6
      ELSE
           WRITE(8,1030) QE, QE/3.6
           WRITE(8,1035) QZ, QZ/3.6
      END IF
      WRITE(8,1040) QC, QC/3.6
      WRITE(8,1045) W, W/3.6
      WRITE(8,1050) COPR
      IF(IRFTYP .LE. 3 .AND. ICYCL .EQ. 1 .AND. ICAB .NE. 0
     .                 .AND. IFRSH .NE. 0) WRITE(8,1055) FF_FRACT
      write(8, '( )')
 
      IF(IMAP .EQ. 1) THEN
         WRITE(8,2215) ETAS
         WRITE(8,2211) EFFC/ETAS
         WRITE(8,2212) CE
         WRITE(8,2213) QCAN
         write(8,2214) QHILO
      END IF
 
      WRITE(8,1060)
      WRITE(8,1065)
      IF(ITYPE .EQ. 1) THEN
           IF(IFRSH .NE. 0) THEN
                WRITE(8,1110) ETAE,FSUPE
           ELSE
                WRITE(8,1111) FSUPE
           END IF
      ELSE
           IF(IFRSH .NE. 0) THEN
                WRITE(8,1070) ETAE,FSUPE
           ELSE
                WRITE(8,1071) FSUPE
           END IF
           IF(IFREZ .NE. 0) THEN
                WRITE(8,1075) ETAF
           ELSE
                WRITE(8,1076)
           END IF
      END IF
      IF(ICOND .NE. 0) THEN
           WRITE(8,1080) ETAC,FSUBC,FSUPC
      ELSE
           WRITE(8,1081) FSUBC,FSUPC
      END IF
      WRITE(8,1085)
      WRITE(8,1090) FLOW*0.45359
      IF(IMAP.NE.0) WRITE(8,1091) ETAV
      WRITE(8,1092) PR
      IF(IMAP.NE.0) THEN
        WRITE(8,1093) TSUC
        WRITE(8,1094) TDISC
        WRITE(8,1095) TSUPC
      END IF
      WRITE(8,'(A1)') CHAR(12)
C
C          OUTPUT A FIGURE OF THE RESULTS
C
      CALL OUTPUT(ICYCL,T,W,QC,QE,QZ)
C
C          RETURN TO CALLER
C
      WRITE(IM,'(''LEAVING CYCLE WITH IC: '',I5,''  IE: '',I5)') IC, IE
      CLOSE(IO)
      CLOSE(IM)
      RETURN
C
C          OUTPUT FORMATS
C
 1010 FORMAT(
     .    2X,'STATE           T (C)        P       H       V        S',
     .     '      XL     XV     XQ'/
     .    14X,'  AIR   REF    (KPA) (KJ/KG)  (M3/KG) (KJ/KG-C)',
     .     ' (MASS FRAC)'/)
 1020 FORMAT (1X,I2,1X,A8,2X,2(F5.1,2X),F6.1,2X,F6.1,2X,1PF7.4,
     .  2X,0PF6.3,2(2X,F5.3),1X,F6.3)
 1021 FORMAT (1X,I2,1X,A8,4X,'N/A',F7.1,2X,F6.1,2X,F6.1,2X,1PF7.4,
     .  2X,0PF6.3,2(2X,F5.3),1X,F6.3)
 1025 FORMAT(1X,/,25X,'CYCLE PERFORMANCE SUMMARY'/)
 1030 FORMAT(1X,'FRESH FOOD EVAPORATOR CAPACITY --- ',F6.1,' KJ/HR',
     .          '   (',F5.1,' W)')
 1035 FORMAT(1X,'FREEZER EVAPORATOR CAPACITY ------ ',F6.1,' KJ/HR',
     .          '   (',F5.1,' W)')
 1040 FORMAT(1X,'CONDENSER HEAT REJECTION RATE ---- ',F6.1,' KJ/HR',
     .          '   (',F5.1,' W)')
 1045 FORMAT(1X,'COMPRESSOR POWER REQUIREMENT ----- ',F6.1,' KJ/HR',
     .          '   (',F5.1,' W)')
 1050 FORMAT(1X,'COEFFICIENT OF PERFORMANCE ------- ',F6.3)
 1055 FORMAT(1X,'FRACTION AIR TO FRESH FOOD -------  ',F5.3,
     .       '  (SINGLE EVAPORATOR CYCLE)')
 1060 FORMAT(25X,'HEAT EXCHANGER PERFORMANCE SUMMARY',/)
 1065 FORMAT(3X,'EXCHANGER',3X,'EFFECTIVENESS, %',3X,
     .          'SUBCOOLED FRACTION, %', 3X,'SUPERHEATED FRACTION, %',/)
 1070 FORMAT(1X,'FRESH FOOD EVAP. --- ',F5.1,14X'  N/A',20X,F5.1)
 1071 FORMAT(1X,'FRESH FOOD EVAP. --- ',2X,'N/A',14X'  N/A',20X,F5.1)
 1075 FORMAT(1X,'FREEZER EVAP. ------ ',F5.1,14X,'  N/A',20X,'-----')
 1076 FORMAT(1X,'FREEZER EVAP. ------ ',2X,'N/A',15X,' N/A',20X,'-----')
 1080 FORMAT(1X,'CONDENSER ---------- ',F5.1,14X,F5.1,20X,F5.1,/)
 1081 FORMAT(1X,'CONDENSER ---------- ',2X,'N/A',14X,F5.1,20X,F5.1,/)
 1085 FORMAT(25X,'COMPRESSOR PERFORMANCE SUMMARY',/)
 1090 FORMAT(1X,'REFRIGERANT MASS FLOW RATE ----- ',F6.3,' KG/HR')
 1091 FORMAT(1X,'VOLUMETRIC EFFICIENCY ----------  ',F5.3)
 1092 FORMAT(1X,'PRESSURE RATIO -----------------  ',F5.2)
 1093 FORMAT(1X,'SUCTION PORT TEMPERATURE -------  ',F5.1,' C')
 1094 FORMAT(1X,'DISCHARGE PORT TEMPERATURE -----  ',F5.1,' C')
 1095 FORMAT(1X,'DISCHARGE SUPERHEAT ------------  ',F5.1,' C')
 1097 FORMAT(1X,'THE REFRIGERANT MIXTURE CONSISTS OF ',F4.0,'%',1X,A6)
 1098 FORMAT(1X,'                                    ',F4.0,'%',1X,A6)
 1099 FORMAT(1X,/)
 1100 FORMAT(35X,'OUTPUT RESULTS'//)
 1105 FORMAT(1X,'EVAPORATOR CAPACITY--------------- ',F6.1,' KJ/HR',
     .          '   (',F5.1,' W)')
 1110 FORMAT(1X,'EVAPORATOR --------- ',F5.1,14X'  N/A',20X,F5.1)
 1111 FORMAT(1X,'EVAPORATOR --------- ',2X,'N/A',14X'  N/A',20X,F5.1)
 1200 FORMAT(50X,'RUN AT ',A2,':',A2,':',A2,' ON ',A2,1X,A3,I5/)
 2200 FORMAT (/1X,'*** EVAPORATOR ITERATION DID NOT CONVERGE ***',
     .        3F9.3/)
 2202 FORMAT (/1X,'*** CONDENSER ITERATION DID NOT CONVERGE ***',
     .        3F9.3/)
 2203 FORMAT (/1X,'*** INTERCHANGER SUPERHEAT NOT POSSIBLE ***',
     .        3F9.3/)
 2204 FORMAT (/1X,'*** CRITICAL TEMPERATURE EXCEEDED IN CONDENSER ***')
c2210 FORMAT(1X,'ISENTROPIC EFFICIENCY OVER SHELL - ',F6.3,/)
 2211 FORMAT(1X,'ESTIMATED MOTOR-PUMP EFFICIENCY  - ',F6.3,
     .          '  (COMPRESSOR EER MODEL)')
 2212 FORMAT(1X,'ESTIMATED CLEARANCE VOLUME       - ',F6.3,
     .          '  (COMPRESSOR EER MODEL)')
 2213 FORMAT(1X,'ESTIMATED SHELL LOSS             - ',F6.3,
     .          '  (COMPRESSOR EER MODEL)')
 2214 FORMAT(1X,'ESTIMATED DISC TUBE HEAT LOSS    - ',F6.3,
     .          '  (COMPRESSOR EER MODEL)'/)
 2215 FORMAT(1X,'ESTIMATED COMPRESSION EFFICIENCY - ',F6.3,
     .          '  (COMPRESSOR EER MODEL)')
      END
C
      SUBROUTINE DUTFND(ICAB,IRFTYP,ICYCL,N,QFRSH,QFREZ,FROSTF,FROSTZ,
     .                  QFF,QFZ,TS3,TS5,T,IDFRST, dutyr)
C     ******************************************************************
C     *    CALCULATE DUTY CYCLE AND THE AVERAGE CABINET LOADS          *
C     ******************************************************************
C
      DIMENSION T(16)
      COMMON /CABLOD/ FFASH,FAUXF,FZASH,FZAUX,TROOM,FFTEMP,OTHERW,
     .                FZTEMP,FFQ,FZQON,FZQOFF,FFLAT,FZLAT,FFSEN,FZSEN,
     .                FFHTQ,FZHTQ,CONDF,CONDZ,QMUL
      COMMON /FANS/ FANE,FANZ,FANC,DUTYC,W,COPR
      COMMON /LORENZ/ DUTYE,DUTYZ,PWRL,PWRE,CAPE,CAPZ,DUTYL,DUTYS,
     .                FANEL,FANCL,FANES,FANCS
      COMMON / CHINA / INCTRL
      COMMON / INWALL / UA_FZ, UA_FF, UA_ML, Q_FZ_IN_WALL, Q_FF_IN_WALL,
     .                  Q_ML_IN_WALL, CAPE_IN_WALL, CAPM_IN_WALL,
     .                  CAPZ_IN_WALL, Q_FZ_FF
      COMMON / CNDWAL / UA_FF_CND, UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS,
     .                  Q_CND_FF,  Q_CND_FZ,  Q_HXS_FF,  Q_HXS_FZ,
     .                  CONDF_IN_WALL, CONDZ_IN_WALL
      COMMON / LIQLIN / FFREFQ, FZREFQ, CONDHT(2), CONDVP(2)
      COMMON / CYCLIC / DFSTCYC, FFCYC, FZCYC, OUTCYC
C
C          RETURN IF NO CABINET LOADS
C
      IF(ICAB .EQ. 0) RETURN
C
C          CALCULATE IN-WALL HEAT LOADS
C
      TENV = (TROOM + 459.6)/1.8
      TCND = 0.2*T(14) + 0.4*T(3) + 0.4*T(11)
 
      IF(TS5 .GT. -300.0) THEN                             !Freezer evaporator
         TRFZ = (T(8) + T(9))/2.0
         Q_FZ_IN_WALL = 1.8*UA_FZ*(TENV - TS5)
         Q_ML_IN_WALL = 1.8*UA_ML*(TS3  - TS5)
 
         CAPZ_IN_WALL = 1.8*UA_FZ*(TENV - TRFZ)
         CAPM_IN_WALL = 1.8*UA_ML*(TS3  - TRFZ)
 
         Q_FZ_FF = 1.8*UA_ML*(TS5 - TRFZ)
      ELSE
         Q_FZ_IN_WALL = 0
         Q_ML_IN_WALL = 0
 
         CAPZ_IN_WALL = 0
         CAPM_IN_WALL = 0
 
         Q_FZ_FF = 0
      END IF
 
      TRFF = (T(5) + T(7))/2.0
      Q_FF_IN_WALL = 1.8*UA_FF*(TENV - TS3)
      CAPE_IN_WALL = 1.8*UA_FF*(TENV - TRFF)
      CONDF_IN_WALL = 1.8*UA_FF_CND*(TCND - TENV)
      CONDZ_IN_WALL = 1.8*UA_FZ_CND*(TCND - TENV)
C
C          BRANCH ACCORDING TO THE TYPE OF REFRIGERATOR
C
      QFF = FFQ
      QFZ = FZQOFF
C
      SELECT CASE (IRFTYP)
           CASE(1 : 3)
                IF(ICYCL . EQ. 1) THEN
                     IF(IDFRST .EQ. 0) THEN
                        QFF = QFF + FROSTF
                        QFZ = QFZ + FROSTZ
                     END IF
 
                     CAPE = QFRSH/1.0548 - 3.413*FANE - 3.413*DFSTCYC
     .                                 - 3.413*FFCYC   - 3.413*FZCYC
     .                                 - CONDF_IN_WALL - CONDZ_IN_WALL
                     DUTYC = (QFF + QFZ)/CAPE
                     IF(DUTYC .GT. 1.0) DUTYC = 1.0
                     dutyr = dutyc
                END IF
 
                IF(ICYCL .EQ. 2) THEN
                     QFF = QFF - FROSTF
                     IF(IDFRST .EQ. 0) QFZ = QFZ + FROSTZ
 
                     CAPZ = QFREZ/1.0548 - 3.413*FANZ - 3.413*DFSTCYC
     .                                    + Q_FZ_IN_WALL + Q_ML_IN_WALL
     .                                    - CAPZ_IN_WALL - CAPM_IN_WALL
     .                                    - CONDZ_IN_WALL - 3.413*FZCYC
     .                                    - Q_HXS_FZ/1.0548
 
 
 
                     IF (capz .LE. 0.0) THEN
                        CALL window (10, 15, 15, 65, 32, 1)
                        CALL gotoxy (22,12)
                        CALL print ('Incorrect Solution -- ',22,-2)
                        CALL print ('Check Mass Flow',15,-2)
 
                        CALL gotoxy (22,13)
                        CALL print ('Solution being Terminated',25,-2)
                        CALL setatr(1)
                        CALL gotoxy (0, 24)
                        CALL print (' ', 1, -2)
 
                        CALL warble
                        CALL warble
                        CALL warble
                        STOP ' '
                     END IF
 
                     DUTYZ = QFZ/CAPZ
 
                     CAPE = QFRSH/1.0548 - 3.413*FANE - 3.413*FFCYC
     .                                    + Q_FF_IN_WALL - CAPE_IN_WALL
     .                                    - CONDF_IN_WALL + q_fz_ff
     .                                    - Q_HXS_FF/1.0548
  !                  DUTYE = (QFF - DUTYZ*Q_FZ_FF)/CAPE
                     DUTYE = QFF/CAPE
 
                     DUTYC = AMIN1(DUTYE,DUTYZ)
                     IF(DUTYC .GT. 1.0) DUTYC = 1.0
                     dutyr = amax1(dutye, dutyz)
                     if(dutyr .gt. 1.0) dutyr = 1.0
                END IF
 
                IF(ICYCL .EQ. 3) THEN
                     IF(N .EQ. 1) THEN
                          IF(IDFRST .EQ. 0) QFZ = QFZ + FROSTZ
                          CAPZ = QFRSH/1.0548 - 3.413*FANE
     .                         - 3.413*(DFSTCYC + FZCYC)
                          DUTYZ = QFZ/CAPZ
 
                          DUTYC = AMIN1(DUTYZ,1.0)
                          DUTYZ = DUTYC
 
                     ELSE
                          CAPE = QFRSH/1.0548 - 3.413*(FANE + FFCYC)
     .                             + Q_FF_IN_WALL - CAPE_IN_WALL
     .
                          QFF = QFF - FROSTF
                          DUTYE = QFF/CAPE
                          DUTYC = AMIN1(DUTYE,1.0)
                          DUTYE = DUTYC
                     END IF
                     dutyr = dutyc
                END IF
 
           CASE DEFAULT               !One door type units
                IF(IDFRST .EQ. 0) QFZ = QFZ + FROSTZ
 
                CAPE = QFRSH/1.0548 - 3.413*(FANE + DFSTCYC + FZCYC)
     .                             + Q_FF_IN_WALL - CAPE_IN_WALL
     .                             - CONDF_IN_WALL - Q_HXS_FF/1.0548
                DUTYE = QFZ/CAPE
                DUTYC = AMIN1(DUTYE,1.0)
                dutyr = dutyc
 
      END SELECT
      RETURN
      END
