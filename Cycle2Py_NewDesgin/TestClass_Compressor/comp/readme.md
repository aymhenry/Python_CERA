# Tesing method : comp
    run comp.py

# Input vars
    T1 = 296     # K suction temp.
    T12 = 249.5  # K discharge temp.

    P1 = 123.05 * 1000  # Suction pressure Pa
    P2 = 928.18 * 1000  # Discharge pressire Pa

    R12_TABLE_READING = 200.0 - 27.10795349661135 # 26.2257538946007 	# kj/kg   200-app result at 0C need to be 200, valid only for RF12
    H1 = 23726 *1000 /120.91 + R12_TABLE_READING # J/kg
    V1 = 19.490 / 120.91 # m3/kg

    TAMB = 308.11 # K
    # MEFF used only in case of IMAP = 0 (MAP) or 2 (Efficiency Model)
    MEFF = 0.8  # MEFF - MECHANICAL EFFICIENCY

    QHILO = 0.0 # QHILO - NORMALIZED HEAT LOSS FROM DISCHANGE LINE INSIDE
    QCAN = 0.0  # QCAN - COMPRESSOR SHELL LOSS NORMALIZED TO POWER INPUT
    IMAP = 1 # Compressor Analysis method values 0 MAP, 1 ERR, 2 Efficiency mode

    EER = 5.28  # Rated EER used only if IMAP = 1
    SEFF =  0.9 # isentropic efficiency

    ICOOL =  1  # Fan cooling method 0 Static, 1 Fan-Forced
    ICOMP =  1  # Compressor Type 1   Reciprocating, 2   Rotary

    SPEED =  3450 # Nominal Speed (rpm =3450)
    MREF = 5.8 # kg/hr ===>0.04796956413861551 kmol/hr # Initial Guess For Refrigerant Mas Flow Rate (kg/hr) * 2.20462 lbs/hr

    # used only if IMAP =1 useless otherwise
    EFFC =  0.6058868684272488 # used only if IMAP =1 useless otherwise

    # used only if IMAP =1 useless otherwise
    CE =  0.022338000722326633 # CLEARANCE VOLUME (FRACTION) output of map


# Sample output
    ==Test 1 =========================
    #-- Function Name: comp ===============
        #-- inputs:
            arg no 1 =  <CoolPrp.CoolPrp object at 0x02AE6388>
            H1 =  196401.49183146743
            P1 =  123050.0
            P2 =  928180.0
            T1 =  296
            T12 =  249.5
            MEFF =  0.8
            QHILO =  0.0
            QCAN =  0.0
            V1 =  0.1611942767347614
            TAMB =  308.11
            EER =  5.28
            SEFF =  0.9
            SPEED =  3450
            MREF =  5.8
            IMAP =  1
            EFFC =  0.6058868684272488
            CE =  0.022338000722326633
            ICOOL =  1
            ICOMP =  1
      ................ K
      T2     359.69963263382215
      TSUC   310.7748441372224
      TDISC  417.96867255924644
      ................j/kg
      HOUT   403097.31377610774
      QHILO  -3388.3198977108036
      QCAN   16711.98457433579
      ................m3/kg
      VV2    0.02527842089025558
      VSUC   0.16924029131100593
      ................Other
      GAMA   1.13670426118834
      RN     1.1026031333526898
      ETAS   -0.00010992036444251721
      DISP   5.83914771144516e-06


    ==Test 2 =========================
    #-- Function Name: comp ===============
        #-- inputs:
            arg no 1 =  <CoolPrp.CoolPrp object at 0x02AE6388>
            H1 =  196401.49183146743
            P1 =  123050.0
            P2 =  928180.0
            T1 =  296
            T12 =  249.5
            MEFF =  0.8
            QHILO =  0.0
            QCAN =  0.0
            V1 =  0.1611942767347614
            TAMB =  308.11
            EER =  5.28
            SEFF =  0.9
            SPEED =  3450
            MREF =  5.8
            IMAP =  2
            EFFC =  0.6058868684272488
            CE =  0.022338000722326633
            ICOOL =  1
            ICOMP =  1
      ................ K
      T2     280.41139340315004
      TSUC   248.06176364182318
      TDISC  280.41139340315004
      ................j/kg
      HOUT   206976.61493547607
      QHILO  0.0
      QCAN   0.0
      ................m3/kg
      VV2    0.0007273342627909525
      VSUC   0.02527842089025558
      ................Other
      GAMA   1.606715909388518
      RN     1.5585144321068622
      ETAS   0
      DISP   8.184417302123101e-07


    ==Test 3 =========================
    Not Applicable for IMPA=0






