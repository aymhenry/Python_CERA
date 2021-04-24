      SUBROUTINE DOORPN(TSETFF, TSETFZ, HFFC, WFFC, DFFC, HFRZ, WFRZ,
     .                  DFRZ, QDFFCS, QDFFFS, QDFZCS, QDFZFS,
     .                        QDFFCL, QDFFFL, QDFZCL, QDFZFL)
C     ******************************************************************
C           Calculates the Latent and Sensible Loads for Door Openings *
C     ******************************************************************
 
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / ERA /    IRFTYP, FZHEAT, FFHEAT, WATERZ, WATERF, HXVUR,
     .                  HXVUZ
 
C           Input:  TDRAIR..... Ambient Air Temperature (Deg F)
C                   TSETFF..... Fresh Food Compartment Set Point (Deg F)
C                   TSETFZ..... Freezer Compartment Set Point (Deg F)
C                   RELHUM..... Relative Humidity (Deg F)
C                   HFRZ....... Internal Freezer Height (Inches)
C                   WFRZ....... Internal Freezer Width (Inches)
C                   DFRZ....... Internal Freezer Depth (Inches)
C                   HFFC....... Internal Fresh Food Height (Inches)
C                   WFFC....... Internal Fresh Food Width (Inches)
C                   DFFC....... Internal Fresh Food Depth (Inches)
C                   RELHUM..... Relative Humidity (Deg F)
C                   FFCOPN..... Number of door openings/hr for Fresh Food
C                               (this and nos below are the average per hour)
C                   FRZOPN..... Number of door openings/hr for Freezer
C                   HRFFC...... Hours/hr the Fresh Food door is open
C                   HRFRZ...... Hours/hr the Freezer door is open
C
C           Output: For the Fresh Food Compartment (FFC):
C                   QLFFC...... Immediate Door Opening Latent Heat Load (BTU)
C                   QSFFC...... Immediate Door Opening Sensible Heat Load (BTU)
C                   QNVFFC..... Time Dependant (Convective) Door Opening
C                               Latent Load (BTU)
C                   QNDFFC..... Time Dependant (Convective) Door Opening
C                               Sensible Load (BTU)
C                   QDFFCS..... Total sensible load in fresh food section
C                               assuming the moisture remains liquid
C                   QDFFCL..... Total latent load in fresh food section
C                               assuming the moisture remains liquid
C                   QDFFFS..... Total sensible load in fresh food section
C                               assuming the moisture is frozen
C                   QDFFFL..... Total latent load in fresh food section
C                               assuming the moisture is frozen
C
C                   For the Freezer Compartment (FRZ):
C                   QLFRZ...... Immediate Door Opening Latent Heat Load (BTU)
C                   QSFRZ...... Immediate Door Opening Sensible Heat Load (BTU)
C                   QNVFRZ..... Time Dependant (Convective) Door Opening
C                               Latent Load (BTU)
C                   QNDFRZ..... Time Dependant (Convective) Door Opening
C                               Sensible Load (BTU)
C                   QRFZCS..... Total sensible load in freezer section
C                               assuming the moisture remains liquid
C                   QRFZCL..... Total latent load in freezer section
C                               assuming the moisture remains liquid
C                   QRFZFS..... Total sensible load in freezer section
C                               assuming the moisture is frozen
C                   QRFZFL..... Total latent load in freezer section
C                               assuming the moisture is frozen
C
C           Sensible Heat Transfer Coefficients (BTU/hr-ft2-F)
C           HTRFFC - Fresh Food Compartment
C           HTRFRZ - Freezer Compartment
 
      DATA HTRFFC, HTRFRZ / 0.76, 0.99 /
C
C           Mass Transfer Coefficients (Pounds/hr)
C           HMFFC - Fresh Food Compartment
C           HMFRZ - Freezer Compartment
 
      DATA HMFFC, HMFRZ / 47.14, 61.41 /
C
C
C           Volume of Freezer & Fresh Food Compartment (Ft3)
C
      IF (NMOD .NE. 4) THEN
         VOLFRZ = VOLAZ                             !HFRZ*WFRZ*DFRZ
         VOLFFC = VOLAR                             !HFFC*WFFC*DFFC
      ELSE
         VOLFRZ = VOLAR
         VOLFFC = VOLAZ
      END IF
C
C           Area of Freezer & Fresh Food Compartment (Ft2)
C
      AFFC = 2.0*(HFFC*WFFC + HFFC*DFFC + WFFC*DFFC)
      AFRZ = 2.0*(HFRZ*WFRZ + HFRZ*DFRZ + WFRZ*DFRZ)
C
C           Set evaporator Temperatures
C
      TFFCAB = TSETFF
      TFZCAB = TSETFZ
 
      TFFC  = 32.0
      TFFF  = TSETFF - 15.0
      TFRZE = TSETFZ - 15.0
C
C           Enthalpy Changes for Fresh Food & Freezer
C
      RHAIR = RELHUM/100.0                                 !Ambient air
      CALL HUMRAT(TDRAIR, RHAIR, WAIR)
 
      RHFFC = 1.0                                          !Condensation in FF
      CALL HUMRAT(TFFC, RHFFC, WWFFC)
 
      RHFFF = 1.0                                          !Freezing in FF
      CALL HUMRAT(TFFF, RHFFF, WWFFF)
 
      RHFZC = 1.00                                         !Condensation in FZ
      CALL HUMRAT(TFFC, RHFZC, WWFZC)
 
      RHFZF = 1.00                                         !Freezing in FZ
      CALL HUMRAT(TFRZE, RHFZF, WWFZF)
 
      CALL HWTAIR(TDRAIR, WAIR, TFFC, WWFFC, TFFCAB, DUMS1, DUML1,
     .            DHFfCS, DHFfCL, DUMS2, DUML2)
 
      CALL HWTAIR(TDRAIR, WAIR, TFFF, WWFFF, TFFCAB, DUMS1, DUML1,
     .            DUMS2, DUML2, DHFfFS, DHFfFL)
 
      CALL HWTAIR(TDRAIR, WAIR, TFFC, WWFZC, TFZCAB, DUMS1, DUML1,
     .            DHFzCS, DHFzCL, DUMS2, DUML2)
 
      CALL HWTAIR(TDRAIR, WAIR, TFRZE, WWFZF, TFZCAB, DUMS1, DUML1,
     .            DUMS2, DUML2, DHFzFS, DHFzFL)
C
C           Immediate Fresh Food Compartment Door Opening Loads
C
      RHODRY = 0.0807*(492./(TDRAIR+460.))
      CALL DENH2O(TDRAIR, RHOH2O)
      RHOAIR = RHODRY + RHOH2O
 
      QLFFC = FFCOPN*DHFFCL*VOLFFC*RHOAIR                  !FF Condensation
     .      + WATERF*(1061.0 + 0.444*TFFCAB)
      QSFFC = FFCOPN*DHFFCS*VOLFFC*RHOAIR
 
      QLFFF = FFCOPN*DHFFFL*VOLFFC*RHOAIR                  !FF Freezing
     .      + WATERF*((1061.0 + 0.444*TFFCAB) + (158.9 - 0.488*TFFF))
      QSFFF = FFCOPN*DHFFFS*VOLFFC*RHOAIR
 
      QLFZC = FRZOPN*DHFZCL*VOLFRZ*RHOAIR                  !FZ Condensation
     .      + WATERZ*(1061.0 + 0.444*TFZCAB)
      QSFZC = FRZOPN*DHFZCS*VOLFRZ*RHOAIR
 
      QLFZF = FRZOPN*DHFZFL*VOLFRZ*RHOAIR                  !FZ Freezing
     .      + WATERZ*((1061.0 + 0.444*TFZCAB) + (158.9 - 0.488*TFRZE))
      QSFZF = FRZOPN*DHFZFS*VOLFRZ*RHOAIR
C
C           Transient Sensible Heat Transfer
C
      QNDFFC = HTRFFC*AFFC*(TDRAIR - TFFCAB)*HRFFC
      QNDFFF = HTRFFC*AFFC*(TDRAIR - TFFCAB)*HRFFC
      QNDFZC = HTRFRZ*AFRZ*(TDRAIR - TFZCAB)*HRFRZ
      QNDFZF = HTRFRZ*AFRZ*(TDRAIR - TFZCAB)*HRFRZ
C
C           Set evaporator Temperatures
C
      TFFC  = TSETFF
      TFRZE = TSETFZ
C
C           Transient Latent Heat Transfer
C
      CALL DENH2O(TFFC, RHFFC)
      CALL DENH2O(TFRZE, RHFRZ)
 
      HIG = 1220.0
 
      QNVFFC = HMFFC*HIG*AFFC*(RHAIR*RHOH2O-RHFFC)*HRFFC
      IF(QNVFFC .LT. 0.0) QNVFFC = 0
 
      QNVFFF = QNVFFC
 
      QNVFZC = HMFRZ*HIG*AFRZ*(RHAIR*RHOH2O-RHFRZ)*HRFRZ
      IF(QNVFZC .LT. 0.0) QNVFRZ = 0
 
      QNVFZF = QNVFZC
C
C          TOTAL HEAT TRANSFER RATES
C
      IF(NMOD .NE. 5) THEN                                 !Upright door
         QDFFCS =  QSFFC + QNDFFC
         QDFFFS =  QSFFF + QNDFFF
         QDFZCS =  QSFZC + QNDFZC
         QDFZFS =  QSFZF + QNDFZF
 
         QDFFCL =  QLFFC + QNVFFC
         QDFFFL =  QLFFF + QNVFFF
         QDFZCL =  QLFZC + QNVFZC
         QDFZFL =  QLFZF + QNVFZF
 
      ELSE                                                 !Chest freezer
         QDFZCS =  0.45*QNDFZC
         QDFZFS =  0.45*QNDFZF
 
         QDFZCL =  0.25*QLFZC + 0.45*QNVFZC
     .                        + WATERZ*(1061.0 + 0.444*TFZCAB)
         QDFZFL =  0.25*QLFZF + 0.45*QNVFZF
     .                        + WATERZ*((1061.0 + 0.444*TFZCAB)
     .                                + (158.9 - 0.488*TFRZE))
 
      END IF
 
      RETURN
      END
C
      SUBROUTINE SATPS(TF, PS)
C     ******************************************************************
C     *     Calculates the saturation pressure of Water                *
C     ******************************************************************
C
C           Input:  TF...... Saturation Temperature (Deg F)
C
C           Output: PS...... Saturation Pressure (PSIA)
C
      IMPLICIT REAL*8(A-H,O-Z)
 
      TR = TF + 460.0
      PS = DEXP(-9560.8/TR + 17.0234)
 
      RETURN
      END
C
      SUBROUTINE DENH2O(TF, RHOS)
C     ******************************************************************
C     *     Calculates the saturation Density of Water                 *
C     ******************************************************************
C
C           Input:  TF........ Saturation Temperature (Deg F)
C
C           Output: RHOS...... Saturation Density (#/ft3)
C
      IMPLICIT REAL*8(A-H,O-Z)
 
      TR = TF + 460.0
      CALL SATPS(TF, PS)
      RHOS = 1.677*PS/TR
 
      RETURN
      END
C
      SUBROUTINE HUMRAT(TF, RELHUM, W)
C     ******************************************************************
C     *     Calculates the Humidity Ratio of Air                       *
C     ******************************************************************
C
C           Input:  TF.......... Saturation Temperature (Deg F)
C                   RELHUM...... Relative Humidity
C
C           Output: W........... Humidity Ratio
C
C
      IMPLICIT REAL*8(A-H,O-Z)
 
      PT = 14.7
      CALL SATPS(TF, PS)
      PG = RELHUM*PS
      W = 0.622*PG/(PT-PG)
 
      RETURN
      END
C
      SUBROUTINE HWTAIR(TF1, W1, TF2, W2, TFCAB, HVAPS, HVAPL,
     .                  HLIQS, HLIQL, HSLDS, HSLDL)
C     ******************************************************************
C     *     Calculates the Enthalpy Change of Moist Air                *
C     ******************************************************************
C
C           Input:  TF1......... Start Temperature (Deg F)
C                   TF2......... Final Temperature (Deg F)
C                   TFCAB....... Final Cabinet Temperature (Deg F)
C                   W1.......... Start Humidity Ratio
C                   W2.......... Final Humidity Ratio
C
C           Output: HVAPS....... Sensible Enthalpy Change of the
C                                Air/Water Mixture assuming the water
C                                remains vapor (BTU/lb)
C                   HVAPL....... Latent Enthalpy Change of the
C                                Air/Water Mixture assuming the water
C                                remains vapor (BTU/lb)
C                   HLIQS....... Sensible Enthalpy Change of the
C                                Air/Water Mixture assuming the water
C                                condenses as liquid (BTU/lb)
C                   HLIQL....... Latent Enthalpy Change of the
C                                Air/Water Mixture assuming the water
C                                condenses as liquid (BTU/lb)
C                   HSLDS....... Sensible Enthalpy Change of the
C                                Air/Water Mixture assuming the water
C                                condenses as ice (BTU/lb)
C                   HSLDL....... Latent Enthalpy Change of the
C                                Air/Water Mixture assuming the water
C                                condenses as ice (BTU/lb)
C
      IMPLICIT REAL*8(A-H,O-Z)
 
      PNDH2O = W1-W2
      IF(PNDH2O.LT.0.0) W1 = W2
 
      HVAPS = 0.24*TF1   + W1*(0.444*TF1)
     .      - 0.24*TFCAB - W1*(0.444*TFCAB)
      HVAPL = 0.0
 
      HLIQS = HVAPS + PNDH2O*0.444*(TFCAB - TF2)
      HLIQL = PNDH2O*(1061.0 + 0.444*TFCAB)
 
      HSLDS = HLIQS
      HSLDL = PNDH2O*((1061.0 + 0.444*TFCAB) + (158.9 - 0.488*TF2))
 
      RETURN
      END
