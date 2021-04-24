      PROGRAM ERACOND
C     ******************************************************************
C     *    CALCULATES THE AIR SIDE HEAT TRANSFER COEFICIENT FOR        *
C     *    CROSS FLOW HEAT EXCHANGER: BOTH WIRE-TUBE AND TUBE-FIN      *
C     ******************************************************************
C
      REAL LT, LF, LS, LTWIRE, LWIRE, LMIN, NT, NR, MUAIR, KAIR, NWIRE,
     .     KWIRE, NU, LTUBE, MWIRE, MLWIRE, LPASS, MDOTR, NFIN, KFIN,
     .     LTUBE0
      real nuw
 
      DIMENSION A(0:4)
 
      DATA PI,SIGMA / 3.14159,0.1713E-8 /
      DATA IN, IO / 8, 9 /
      DATA PAIR / 14.696 /
      DATA A / 1.0, -0.02121, -0.66304, 0.403412, -0.07526 /
 
       OPEN(UNIT=IN, FILE='ERACOND.DAT', STATUS='OLD')
       OPEN(UNIT=IO, FILE='ERACOND.OUT', STATUS='UNKNOWN')
C
C        READ INPUT FILE
C
      READ(IN,'(1X)')
      READ(IN,'(1X)')
      READ(IN,*,END=999) ICONV
      READ(IN,'(1X)')
      READ(IN,'(1X)')
      IF(ICONV.EQ.1) THEN
C
C         FORCED CONVECTION INPUT DATA
C
        READ(IN,'(1X)')
        READ(IN,*,END=999) IHX
        READ(IN,'(1X)')
        READ(IN,*,END=999) TUBE_OD
        READ(IN,*,END=999) TUBE_WALL
           DTO = TUBE_OD/25.4
           DT  = (TUBE_OD - 2.0*TUBE_WALL)/25.4
 
        READ(IN,*,END=999) NT
        READ(IN,*,END=999) LT
           LT = LT/2.54
 
        READ(IN,*,END=999) NR
        READ(IN,*,END=999) LF
           LF = LF/2.54
 
        READ(IN,*,END=999) LPASS
           LPASS = LPASS/2.54
 
        READ(IN,*,END=999) WIDTH
        WIDTH = WIDTH*LPASS
 
        SELECT CASE (IHX)
           CASE (1)                                          !Wire-tube
              READ(IN,*,END=999) NWIRE
                 NWIRE = 2.54*NWIRE
                 NWIRE = NWIRE*12.0
                 LTWIRE = 1.0/NWIRE
              READ(IN,*,END=999) DWIRE
                 DWIRE = DWIRE/25.4
 
              READ(IN,*,END=999) KWIRE
                 KWIRE = KWIRE/1.7296
 
              READ(IN,*,END=999) DELTA
 
              READ(IN,'(1X)')
              READ(IN,*,END=999) IWIRE
              IF(DELTA .EQ. 1.0) IWIRE=0
 
           CASE (2)                                          !Tube-fin
              READ(IN,*,END=999) NFIN
                 NFIN = 2.54*NFIN
              READ(IN,*,END=999) TFIN
                 TFIN = TFIN/25.4
 
              READ(IN,*,END=999) KFIN
                 KFIN = KFIN/1.7296
 
              READ(IN,*,END=999) IFIN
 
              READ(IN,'(1X)')
              READ(IN,*,END=999) ITYPE
 
        END SELECT
        READ(IN,'(1X)')
        READ(IN,*,END=999) VOLDOT
           VOLDOT = 2.119*VOLDOT
        DO 5 III=1,13
5       READ(IN,'(1X)')
C
      ELSE
C
C       NATURAL CONVECTION INPUTS
C
        DO 10 III=1,19
10      READ(IN,'(1X)')
C
        READ(IN,'(1X)')
        READ(IN,'(1X)')
        READ(IN,*,END=999) INAT
        READ(IN,'(1X)')
        READ(IN,'(1X)')
        SELECT CASE(INAT)
        CASE(1)                       !VERTICAL FREE PLATE
          READ(IN,*,END=999) AEX1
            AEX1=AEX1/0.3048**2.0
          READ(IN,*,END=999) LTUBE0
            LTUBE0=LTUBE0/0.3048
          READ(IN,*,END=999) LPASS
            LPASS=LPASS/2.54
          READ(IN,*,END=999) TUBE_OD
          READ(IN,*,END=999) TUBE_WALL
             DTO = TUBE_OD/25.4
             DT  = (TUBE_OD - 2.0*TUBE_WALL)/25.4
          READ(IN,'(1X)')
          READ(IN,'(1X)')
          READ(IN,'(1X)')
        CASE(2)                       !WIRE TUBE W/VERTICAL WIRES
          READ(IN,'(1X)')
          READ(IN,*,END=999) LTUBE0
            LTUBE0=LTUBE0/0.3048
          READ(IN,*,END=999) LPASS
            LPASS=LPASS/2.54
          READ(IN,*,END=999) TUBE_OD
          READ(IN,*,END=999) TUBE_WALL
             DTO = TUBE_OD/25.4
             DT  = (TUBE_OD - 2.0*TUBE_WALL)/25.4
          READ(IN,*,END=999) NWIRE
          READ(IN,*,END=999) DWIRE
             DWIRE=DWIRE/25.4
          READ(IN,*,END=999) LWIRE
             LWIRE=LWIRE/2.54/12.0
        CASE(3)                       !VERTICAL HOT WALL
          READ(IN,*,END=999) AEX1
            AEX1=AEX1/0.3048**2.0
          READ(IN,*,END=999) LTUBE0
            LTUBE0=LTUBE0/0.3048
          READ(IN,*,END=999) LPASS
            LPASS=LPASS/2.54
          READ(IN,*,END=999) TUBE_OD
          READ(IN,*,END=999) TUBE_WALL
             DTO = TUBE_OD/25.4
             DT  = (TUBE_OD - 2.0*TUBE_WALL)/25.4
          READ(IN,'(1X)')
          READ(IN,'(1X)')
          READ(IN,'(1X)')
        END SELECT
      END IF
C
C       REMAINING HEAT TRANSFER INFORMATION
C
      READ(IN,'(1X)')
      READ(IN,'(1X)')
      READ(IN,*,END=999) TAIR
         TAIR = 1.8*TAIR + 32.0
      READ(IN,*,END=999) TSAT
         TSAT = 1.8*TSAT + 32.0
      READ(IN,*,END=999) MDOTR
         MDOTR = 2.20452*MDOTR
 
C
999   CONTINUE
      CLOSE (IN)
C
C          CALCULATE AIR PROPERITES
C             RHOAIR - LBM/FT**3
C             MUAIR -  LBM/FT-HR
C             CPAIR -  BTU/LBM-F
C             KAIR -   BTU/HR-FT-F
C
      RHOAIR = PAIR*144.0/(1545.33/28.97*(TAIR+459.67))
      CPAIR = 0.24
      MUAIR = (6.3*TAIR+3996.0)/1.0E5
      KAIR = 2.1E-5*TAIR + 0.0133
      PRAIR = MUAIR*CPAIR/KAIR
C
C          FINAL UNITS CONVERSION (TO FT)
C
      DTO = DTO/12.0
      LT = LT/12.0
      LF = LF/12.0
      DWIRE = DWIRE/12.0
      WIDTH = WIDTH/12.0
      LPASS = LPASS/12.0
      VOLDOT = VOLDOT*60.0
      DEPTH = NR*LF
      LS = 0.0
      LMIN = 0.0
 
      NFIN = NFIN*12.0
      TFIN = TFIN/12.0
C
C
      IF(ICONV.EQ.1) THEN
C
        SELECT CASE (IHX)
           CASE (1)                                          !Wire-tube
C
C          CALCULATE THE FREE FLOW AREA
C
              IF (IWIRE .EQ. 0) THEN
                 LMIN = LTWIRE                               !In-line tube
              ELSE
 
                 LS = 2.0*(LTWIRE**2/4+(DTO+DWIRE)**2)**0.5  !Staggered tube
                 LMIN = LTWIRE
                 IF(LS .LT. LTWIRE) LMIN = LS
              END IF
C
C          FRONTAL AREA CALCULATION
C
              HEIGHT = NT*LT
C    !        AFRONT = WIDTH*HEIGHT
              AFRONT = LPASS*HEIGHT
C
C          FREE FLOW AREA CALCULATION
C
              AFREE = NT*NWIRE*WIDTH*(LMIN-DWIRE)*(LT-DTO)
              afree = afront
C
C          HEAT EXCHANGE AREA CALCULATION
C
              ATUBES = NT*NR*DTO*LPASS*PI
  !           AWIRE = NT*NR*WIDTH*PI*DWIRE*LT*NWIRE*DELTA
              AWIRE = NT*NR*WIDTH*PI*DWIRE*LF*NWIRE*DELTA  ! 3/25/93
              AEX = ATUBES + AWIRE
 
              DH = DTO                                       !Hydraulic diam
 
           CASE (2)                                          !Fin-tube
C
C          CALCULATE THE FREE FLOW AREA
C
              IF (ITYPE .EQ. 0) THEN
                 LMIN = LT                                   !In-line tube
              ELSE
                 LS = 2.0*(LT**2/4+LF**2)**0.5
                 LMIN = LT
                 IF(LS .LT. LT) LMIN = LS
              END IF
C
C          FRONTAL AREA CALCULATION
C
              HEIGHT = NT*LT
C     !       AFRONT = WIDTH*HEIGHT
              AFRONT = LPASS*HEIGHT
C
C          FREE FLOW AREA CALCULATION
C
              AFREE = NT*WIDTH*(LMIN-(DTO+(LMIN-DTO)*TFIN*NFIN))
C
C          HEAT EXCHANGE AREA CALCULATION
C
              ATUBES = PI*DTO*WIDTH*(1.0-TFIN*NFIN)*NT*NR
     .             + NT*NR*DTO*PI*(LPASS - WIDTH)
              AFINS = 2.0*(LT*LF-PI*DTO**2/4.0)*NFIN*WIDTH*NT*NR
              AEX = ATUBES + AFINS
 
              DH = 4.0*AFREE/AEX*DEPTH                       !Hydraulic diam
        END SELECT
C
C          CALCULATE AIR MASS FLOW RATE
C
        MDOTA = RHOAIR*VOLDOT
C
C          CALCULATE THE FRONTAL MASS FLUX AND FACE VELOCITY
C
        GAIR = MDOTA/AFRONT
        VFACE = (VOLDOT/60.)/AFRONT
C
C          CALCULATE THE MAXIMUM MASS FLUX
C
        GMAX = GAIR*AFRONT/AFREE
C
C          CALCULATE THE REYNOLDS NUMBER
C
        RE = GMAX*DH/MUAIR
C
C          CALCULATE THE NUSSELT NUMBER
C
        ICALL = IHX
        IF(ICALL .EQ. 2) ICALL = IFIN + 1
        CALL NUSELT(ICALL,  RE, PRAIR, NU)
C
C          CALCULATE THE AIR SIDE HEAT TRANSFER COEFFICIENT
C
        HAIR = NU*KAIR/DH
        emis = 0.2
        HRAD=4*emis*SIGMA*(TSAT+459.67)**3.0   !!! 2/13/93
C
        SELECT CASE (IHX)
           CASE (1)                                          !Wire-tube
C
C          CALCULATE THE WIRE FIN EFFICIENCY
C
              MWIRE = (4.0*HAIR/(KWIRE*DWIRE))**0.5
              LWIRE = LT/2.0
              MLWIRE = MWIRE*LWIRE
              A1 = EXP(MLWIRE)
              A2 = EXP(-MLWIRE)
              TANH = (A1-A2)/(A1+A2)
              ETA = TANH/MLWIRE
 
             rew = rhoair*depth*60.0*vface/muair
             nuw = 0.0288 * (rew**0.8) * (prair**0.333)
             hwire = kair/depth * nuw
C
C          CALCULATE THE AIR SIDE THE CONDUCTANCE PER UNIT LENGHT
C
              UAIR = (hrad + HAIR)*ATUBES + (hwire + hrad)*ETA*AWIRE !!! 2/13/93
    !         UAIR = HAIR*ATUBES + hwire*ETA*AWIRE
C   !         LTUBE = NT*NR*WIDTH
              LTUBE = NT*NR*LPASS
              ALENTH = AEX/LTUBE
              ULENTH = UAIR/LTUBE
 
           CASE (2)                                          !Fin-tube
C
C          ESTIMATE THE FIN EFFICIENCY
C
              RI = DTO/2.0
              RO = 0.5*(LT**2+LF**2)**0.5
              TERM1 = (RO+TFIN/2.0-RI)**1.5
              TERM2 = (2.0*HAIR/(KFIN*TFIN*(RO-RI)))**0.5
              BETA = TERM1*TERM2
              RRAT = (RO+TFIN/2.0)/RI
 
              SUM = A(0)
              DO I = 1,4
                 EX = 1.0*I
                 SUM = SUM + A(I)*BETA**EX
              END DO
              ETAFIN = SUM
C
C          CALCULATE THE AIR SIDE THE CONDUCTANCE PER UNIT LENGHT
C
              UAIR = (HAIR + hrad)*(ATUBES+ETAFIN*AFINS)   !!! 2/13/93
   !          UAIR = HAIR*(ATUBES+ETAFIN*AFINS)
              LTUBE = NT*NR*LPASS
              ALENTH = AEX/LTUBE
              ULENTH = UAIR/LTUBE
        END SELECT
      END IF
C
C
      IF(ICONV.EQ.2) THEN
C
C NATURAL CONVECTION
C
        emis = 0.8
        SELECT CASE(INAT)
        CASE(1)
C
C        VERTICAL FREE PLATE
C
          HAIR=0.19*(TSAT-TAIR)**0.333
          HRAD=4*emis*SIGMA*(TSAT+459.67)**3.0
          AEX=2.0*AEX1
          UAIR=(HAIR+HRAD)*AEX                  ! ASSUME "FIN EFFICIENCY" = 1
          ULENTH=UAIR/LTUBE0
          ALENTH=AEX/LTUBE0
C
        CASE(2)
C
C        VERTICALLY MOUNTED WIRE-TUBE HEAT EXCHANGER W/VERTICAL WIRES
C
          AWIRE=NWIRE*LWIRE*(PI*DWIRE)
          ATUBE=LTUBE0*PI*DTO
          HWIRE=0.19*(TSAT-TAIR)**0.333
          HTUBE=0.18*(TSAT-TAIR)**0.333
          HRAD=4*emis*SIGMA*(TSAT+459.67)**3.0
          AEX=AWIRE+ATUBE
          UAIR=(HWIRE+HRAD)*AWIRE+(HTUBE+HRAD)*ATUBE
          ULENTH=UAIR/LTUBE0
          ALENTH=AEX/LTUBE0
C
        CASE(3)
C
C        VERTICAL HOT WALL
C
          HAIR=0.19*(TSAT-TAIR)**0.333
          HRAD=4*emis*SIGMA*(TSAT+459.67)**3.0
          AEX=AEX1
          UAIR=(HAIR+HRAD)*AEX                  ! ASSUME "FIN EFFICIENCY" = 1
          ULENTH=UAIR/LTUBE0
          ALENTH=AEX/LTUBE0
        END SELECT
      END IF
C
      WRITE(IO,920) AEX
      WRITE(IO,900) ULENTH
      WRITE(IO,910) ALENTH
 
C
C          SET UP THE DATA FOR THE CONDENSER ROUTINE AND CALL IT
C
      CALL CONDHX(ICONV, TAIR, TSAT, MDOTR, DT, LPASS, ULENTH, ALENTH)
 
      CLOSE (IO)
   !  call exit
C
C          FORMAT STATEMENTS
C
 900  FORMAT(F10.3, 9X, 'AIR SIDE CONDUCTANCE/LENGTH, BTU/HR-FT-F')
 910  FORMAT(F10.3, 9X, 'AIR SIDE AREA/LENGTH, FT**2/FT')
 920  FORMAT(F10.3, 9X, 'AIR SIDE HEAT TRANSFER AREA, FT**2')
      END
C
      SUBROUTINE NUSELT(ICALL, RE, PR, NU)
C     ******************************************************************
C     *    CORRELATION FOR FLOW OVER LONG CYLINDERS FOR KREITH         *
C     *    ASSUME CONSTANT PROPERTIES                                  *
C     ******************************************************************
C
      REAL NU
 
      SELECT CASE (ICALL)
         CASE (1)                                          !Wire-tube
            NU = (0.4*RE**0.5 + 0.06*RE**0.67)*PR**0.4
 
         CASE (2)                                          !Fin-tube
            NU = 0.14*RE**0.61*PR**0.333                   !Straight fin
 
         CASE (3)                                          !Fin-tube
            NU = 0.188*RE**0.614*PR**0.333                 !Wavy
      END SELECT
 
      RETURN
      END
C
      SUBROUTINE CONDHX(ICONV, TAIN, TSAT, MDOTR, DT, LPASS,
     +                  UALNTH, ALNTH)
C     ******************************************************************
C     *    CALCULATES CONDENSATION HEAT TRANSFER COEFFICIENTS FOR      *
C     *    HORIZONTAL FLOW CONDENSATION                                *
C     ******************************************************************
C
      REAL MDOT, MDOTL, MDOTV, MUL, MUV, KL, NUCOND, NUCHEN, KV, MDOTR,
     .     NTUBE, NUL, NUV, LOGF, LPASS, LB, LMTD
 
      COMMON /PROPS / PSAT, RHOL, RHOV, HL, HV, MUL, MUV, CPL, CPV, KL,
     .                KV, HFG, PRL, PRV
      DATA RB / 0.75 /, NTUBE / 1.0 /
      DATA PI, GC / 3.14159, 32.174 /
      DATA IO / 9 /
C
C          SET UP REMAINING NEEDED DATA
C
C      TSAT  = TAIN + 20.0
      IF(ICONV.EQ.1) THEN
        TAOUT = TAIN + 0.75*(TAIN-TSAT)
      ELSE
        TAOUT=TAIN+1.0
      END IF
C
C          FIND SATURATION PROPERTIES
C
      CALL PREAD
      CALL SATPRP(TSAT)
C
C          CALCULATE THE TUBE CROSS SECTIONAL AREA
C
      DT = DT/12.0
      AT = PI*DT**2/4.0
      PT = PI*DT
      PTE = PT
      UAIRE = UALNTH
C
C          CALCULATE THE MASS FLOW RATE PER TUBE
C
      MDOT=MDOTR/NTUBE
C
C          CALCULATE THE MASS FLUX PER TUBE
C
      GREF = MDOT/AT
C
C          CALCULATE REFRIGERANT VELOCITIES IN FT/HR AND FT/SEC
C
      VV = GREF/RHOV
      VVSEC = VV/3600.0
C
C          CALCULATE VAPOR QUANTITIES
C
      REV = GREF*DT/MUV
C
C          FRICTION FACTOR AND NUSSELT NUMBER
C
      IF (REV .LT. 2000.) THEN
         FV = 64.0/REV
         NUV = 3.66
      ELSE
 
         FV = 0.184/REV**0.2
         NUV = 0.023*REV**0.8*PRV**0.4
 
      END IF
C
C          VAPOR FRICTION PRESSURE DROP PER UNIT LENGTH
C
      DPDZV = FV/DT*RHOV/GC*VVSEC**2/2.0
C
C         CONVERT TO PSI
C
      DPDZV = DPDZV/144.0
C
C          HEAT TRANSFER COEFFICIENT
C
      HV = NUV*KV/DT
C
C          CONDUCTANCE PER UNIT LENGHT
C
      UVLNTH = HV*PT
C
C          OVERALL CONDUCTANCE BASE ON AIR SIDE AREA
C
      UDS = (1.0/(1.0/UVLNTH+1.0/UALNTH))/ALNTH
C
C          CALCULATE LIQUID QUANTITIES
C
C          LIQUID VELOCITY IN FT/HR AND FT/SEC
C
      VL = GREF/RHOL
      VLSEC = VL/3600.0
C
C          REYNOLDS NUMBER
C
      REL = GREF*DT/MUL
      IF (REL .LT. 2000.) THEN
         FL = 64.0/REL
         NUL = 3.66
 
      ELSE
         FL = 0.184/REL**0.2
         NUL = 0.023*REL**0.8*PRL**0.4
 
      END IF
C
C          LIQUID PRESSURE DROP PER UNIT LENGTH
C
      DPDZL = FL/DT*RHOL/GC*VLSEC**2/2.0
C
C          CONVERT TO PSI
C
      DPDZL = DPDZL/144.0
C
C          HEAT TRANSFER COEFFICIENT
C
      HL = NUL*KL/DT
C
C          CONDUCTANCE PER UNIT LENGHT
C
      ULLNTH = HL*PT
C
C          OVERALL CONDUCTANCE BASE ON AIR SIDE AREA
C
      USC = (1.0/(1.0/ULLNTH+1.0/UALNTH))/ALNTH
C
      WRITE(IO,920) UDS
      WRITE(IO,930) USC
C
C          BEGIN CALCULATION FOR CONDENSATION REGION
C
C          CONVERT UNITS TO SI UNITS
C
      RHOL = RHOL/(2.2*0.3048**3)
      RHOV = RHOV/(2.2*0.3048**3)
      MUL = MUL/(2.2*3600.*0.3048)
      MUV = MUV/(2.2*3600.*0.3048)
      KL = KL*1.8*1055.0/(3600.0*0.3048)
      CPL = CPL*1055.0*2.2*1.8
      HFG = HFG*1055.0*2.2
      MDOT = MDOT/(2.2*3600.0)
      DT = DT*0.3048
      AT = PI/4.0*DT**2
      LPASS = LPASS*0.3048
      RB = RB/12.*0.3048
      DB = 2.0*RB
      LB = PI*DB/2.0
      PRL = MUL*CPL/KL
      TSAT = (TSAT+460.0)/1.8
      TAIN = (TAIN+460.0)/1.8
      TAOUT = (TAOUT+460.0)/1.8
      UALNTH = UALNTH*1.8*1055.0/(3600.0*0.3048)
C
C          CALCULATE THE LMTD
C
      DTIN = TSAT - TAIN
      DTOUT = TSAT - TAOUT
      IF(TAOUT .GT. TAIN) THEN
         LMTD = (DTIN-DTOUT)/LOG(DTIN/DTOUT)
         DELT = LMTD
      ELSE
         DELT = TSAT - TAIN
      END IF
C
C
C          CALCULATE THE MASS FLUX
C
      G = MDOT/AT
C
C          CALCULATE FLOW PARAMETERS FOR X=1.0 (ALL VAPOR FLOW)
C
      X = 1.0
      GL = G*(1.0-X)
      GV = G*X
      REL = 0.000001
      REV = GV*DT/MUV
C
C          CALCULATE THE CONDENSATION HEAT TRANSFER COEFFICIENT
C          AS A FUNCTION OF QUALITY
C
      DELP = 0.0
      DELTAX = 0.001
      Z = 0.0
      DO WHILE (X .GT. 0.0)
C
C          CALCULATE THE MASS FLOW RATE OF EACH PHASE
C
         MDOTL = MDOT*(1.0-X)
         MDOTV = MDOT*X
C
C          CALCULATE THE MASS FLUX OF EACH PHASE
C
         GL = G*(1.0-X)
         GV = G*X
C
C          CALCULATE THE SUPERFICIAL REYNOLDS NUMBER FOR EACH PHASE
C
         REL = GL*DT/MUL
         IF(REL .EQ. 0.0) REL = 0.00001
         REV = GV*DT/MUV
C
C          CALCULATE THE VOID FRACTION
C
         IF (X .GT. 0.0) THEN
            ALPHA = 1.0/((1.0-X)/X*RHOV/RHOL+1.0)
         ELSE
            ALPHA = 0.0
         END IF
C
C          CALCULATE THE FRICTION FACTOR OF EACH PHASE
C
         CALL FFACT(REL,FL)
         CALL FFACT(REV,FV)
C
C          CALCULATE THE FRICTIONAL PRESSURE DROP FOR EACH PHASE
C
         DPDZL = FL/DT*GL**2/(2.0*RHOL)
         DPDZV = FV/DT*GV**2/(2.0*RHOV)
C
C          CALCULATE THE TWO-PHASE FRICTION FACTOR MULTIPLIER
C
         IF(REL .GT. 2000.) THEN
            CHITT = (MUL/MUV)**0.111*((1.0-X)/X)*(RHOV/RHOL)**0.555
            CHI = CHITT
            CALL DPTT(CHITT,PHITT)
            TPMULT = PHITT
         ELSE
            CHIVT = (64.0/0.184)/REV**0.8*(MUL/MUV)*((1.0-X)/X)
     .                                             *(RHOV/RHOL)
            IF(CHIVT .EQ. 0.0) CHIVT = 0.000000001
 
            CHI = CHIVT
            CALL DPVT(CHIVT,PHIVT)
            TPMULT = PHIVT
         END IF
C
C          CALCULATE THE FRICTIONAL PRESSURE GRADIENT
C
         DPDZTP = TPMULT**2*DPDZV
C
C          CALCULATE THE FRICTIONAL PRESSURE DROP IN THE RETURN BENDS
C
         DPB = 0.0
         IF(ZPASS .GE. LPASS .AND. DB .GT. 0.0) THEN
            FBF = (REV*(DT/DB)**2)**0.05
            DPB = FBF*DPDZTP*LB
            ZPASS = 0.0
         END IF
C
         A1 = GV*DT/MUL*(RHOL/RHOV)**0.5
         A2 = (HFG/(CPL*DELT/2.0))**(1./6.)
C
         IF(REL .LT. 5000.) THEN
            IF(A1 .GT. 1000. .AND. A1 .LT. 20000.)
     .            NUCOND = 13.8*PRL**0.3333* *A2*A1**0.2
 
            IF(A1 .GE. 20000. .AND. A1 .LT. 100000.)
     .            NUCOND = 0.1*PRL**0.3333* A2*A1**0.6667
         END IF
C
         IF(REL .GE. 5000. .AND. A1 .GE. 20000.) THEN
            GEQUIV = GV*(RHOL/RHOV)+GL
            REEQ = GEQUIV*DT/MUL
            NUCOND = 0.026*PRL**0.3333*REEQ**0.8
         END IF
C
C
C          CALCULATE THE F FACTOR
C
C          CALCULATE XTT FOR THE CHEN CORRELATION
C
         XTT = (MUL/MUV)**0.1*((1.0-X)/X)**0.9*(RHOV/RHOL)**0.5
         IF(XTT .LE. 0.0) XTT = 0.0001
         BETA = LOG10(1.0/XTT)
         LOGF = 0.395 + 0.663*BETA + 0.177*BETA**2 - 0.0713*BETA**3
         FCHEN = 10.0**LOGF
         IF(XTT .GT. 10.0) FCHEN = 1.0
C
         IF(REL .GE. 5000.0 .AND. A1 .LT. 20000.) THEN
C
C          CALCULATE THE CONVECTIVE HEAT TRANSFER COEFFICIENT USING THE
C          CHEN CORRELATION
C
C          CALCULATE THE CONVECTIVE NUSSELT NUMBER
C
            NUCHEN = 0.023*REL**0.8*PRL**0.4*FCHEN
C
            NUCOND = NUCHEN
         END IF
C
C          CALCULATE NUSSELT NUMBER FOR SINGLE PHASE LIQUID FLOW
C
         IF(REL .LT. 5000. .AND. A1. LT. 1000.) THEN
            NUCOND = 3.66
            IF(REL .GE. 2000.) NUCOND = 0.023*REL**0.8*PRL**0.4
         END IF
C
C          CALCULATE THE CONDENSATION HEAT TRANSFER COEFFICIENT
C
         HCOND = NUCOND*KL/DT
 
         HREF = HCOND
         HARL = PI*DT*HREF
C
C          CALCULATE THE CONDUCTANCE PER UNIT LENGTH
C
         IF(HARL .EQ. 0.0) HARL = 100000.
         UAPERL = 1.0/(1.0/HARL+1.0/UALNTH)
C
C          CALCULATE THE HEAT TRANSFER PER UNIT LENGTH
C
         QLENTH = UAPERL*DELT
         DELTAZ = MDOT*HFG*DELTAX/QLENTH
         DP = DPDZTP*DELTAZ
 
         IF(X .GT. 0.01) THEN
            DELP = DELP + DP + DPB
         END IF
 
         Z = Z + DELTAZ
         ZPASS = ZPASS + DELTAZ
C
C          CONVERT TO ENGLISH UNITS
C
         DELPE = 0.00014504*DELP
         DHSUM = HREF*DELTAZ
         HSUM = HSUM + DHSUM
         HAVE = HSUM/Z
C
C          CONVERT TO ENGLISH
C
         HAVEE = HAVE/1055.0*0.3048**2/1.8*3600.0
 
         X = X - DELTAX
      END DO
C
C          OVERALL CONDUCTANCE BASE ON AIR SIDE AREA
C
      UCLNTH = HAVEE*PTE
      UTP = (1.0/(1.0/UCLNTH+1.0/UAIRE))/ALNTH
C
      WRITE(IO,940) UTP
      WRITE(IO,950) DELPE
 
!     write (*,*) ' '
!     write (*,*) 'uclnth, uaire, alnth, havee, have',
!    .             uclnth, uaire, alnth, havee, have
!     write (*,*) ' hl, hv', hl, hv
!     read (*,*)
 
      RETURN
C
C          FORMAT STATEMENTS
C
  920 FORMAT(F10.3, 9X, 'VAPOR     REGION CONDUCTANCE, BTU/HR-FT**2-F')
  930 FORMAT(F10.3, 9X, 'LIQUID    REGION CONDUCTANCE, BTU/HR-FT**2-F')
  940 FORMAT(F10.3, 9X, 'TWO-PHASE REGION CONDUCTANCE, BTU/HR-FT**2-F')
  950 FORMAT(F10.3, 9X, 'TWO-PHASE PRESSURE DROP, PSI')
      END
C
      SUBROUTINE FFACT(RE,F)
C     ******************************************************************
C     *    CALCULATE FRICTION FACTOR                                   *
C     ******************************************************************
C
      IF (RE.LT.2000.) THEN
         F = 64.0/RE
      ELSE
         F = 0.184/RE**0.2
      END IF
 
      RETURN
      END
C
      SUBROUTINE DPTT(CHITT,PHITT)
C     ******************************************************************
C     *    CALCULATES THE TWO-PHASE FRICTION MULTIPLIER, PHITT         *
C     ******************************************************************
C
      A1 = CHITT**0.5
      IF(A1 .LT. 46.) THEN
         BETA = LOG10(A1+1.0)
         A2 = 1.739*BETA + 0.07042
         PHITT = 10.0**A2
      ELSE
 
         PHITT = A1**1.8
      END IF
 
      RETURN
      END
C
      SUBROUTINE DPVT(CHIVT,PHIVT)
C     ******************************************************************
C     *    CALCULATES THE TWO-PHASE FRICTION MULTIPLIER, PHIVT         *
C     ******************************************************************
C
      A1 = CHIVT**0.5
      IF(A1 .LE. 200.0) THEN
         BETA = LOG10(A1+1.0)
         A2 = 0.85074*BETA + 0.308771
         IF(A1 .LT. 0.07) A2 = 0.32193*BETA
         PHIVT = 10.0**A2
      ELSE
 
         PHIVT= 1 .0
      END IF
 
      RETURN
      END
C
      SUBROUTINE PREAD
C     ******************************************************************
C     *    subroutine reads props from tables                          *
C     ******************************************************************
c
      real tsata(300), psata(300), rhofa(300), hfa(300), cpfa(300),
     .     mufa(300), kfa(300), hfga(300), rhoga(300), hga(300),
     .     cpga(300), muga(300), kga(300)
      common /array/ tsata, psata, rhofa, rhoga, hfa, hga, mufa, muga,
     .               cpfa, cpga, kfa, kga, hfga
C
C          OPEN AND READ CFC-12 DATA
C
      open(unit=10, file='refrig.prp', status='old')
c
c          increment pass file headers
c
      do 10 i = 1,100
         read(10,*, err=10) iii
         if(iii.eq.1) go to 15
   10 continue
c
c    read saturated refrigerant properties
c
   15 continue
      do 20 i = 1,300
         read(10,*,end=30) tsata(i), psata(i), rhofa(i), vg, hfa(i),
     .                     hga(i), mufa(i), muga(i), cpfa(i), cpga(i),
     .                     kfa(i), kga(i)
 
      rhoga(i) = 1.0/vg
      hfga(i) = hga(i) - hfa(i)
   20 continue
   30 continue
c
      close(unit=10)
 
      return
      end
C
      SUBROUTINE SATPRP(tsat)
C     ******************************************************************
C     *    subroutine determines refrigerant properties when           *
C     *    temperature is known                                        *
C     ******************************************************************
c
      real tsata(300), psata(300), rhofa(300), hfa(300), cpfa(300),
     .     mufa(300), kfa(300), hfga(300), rhoga(300), hga(300),
     .     cpga(300), muga(300),kga(300), muf, mug, kf, kg
 
      common /array/ tsata, psata, rhofa, rhoga, hfa, hga, mufa, muga,
     .               cpfa, cpga, kfa,kga,hfga
 
      common /props/ psat, rhof, rhog, hf, hg, muf, mug, cpf, cpg,
     .               kf, kg, hfg, prf, prg
c
c          scan array to determine where refrigerant temperature falls
c
      do 10 j = 1,300
         if(tsat .ge. tsata(j) .and. tsat .le. tsata(j+1)) go to 15
   10 continue
   15 continue
c
c          linearly interpolate between points to calculate properties
c
      frac = (tsat-tsata(j))/(tsata(j+1)-tsata(j))
      psat = psata(j) + frac*(psata(j+1)-psata(j))
      rhof = rhofa(j) + frac*(rhofa(j+1)-rhofa(j))
      rhog = rhoga(j) + frac*(rhoga(j+1)-rhoga(j))
      hf = hfa(j) + frac*(hfa(j+1)-hfa(j))
      hg = hga(j) + frac*(hga(j+1)-hga(j))
      hfg = hg - hf
      cpf = cpfa(j) + frac*(cpfa(j+1)-cpfa(j))
      cpg = cpga(j) + frac*(cpga(j+1)-cpga(j))
      muf = mufa(j) + frac*(mufa(j+1)-mufa(j))
      mug = muga(j) + frac*(muga(j+1)-muga(j))
      kf = kfa(j) + frac*(kfa(j+1)-kfa(j))
      kg = kga(j) + frac*(kga(j+1)-kga(j))
      prf = muf*cpf/kf
      prg = mug*cpg/kg
 
      return
      end
