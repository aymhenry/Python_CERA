      PROGRAM ERAEVAP
C     ******************************************************************
C     *    CALCULATES THE AIR SIDE HEAT TRANSFER COEFICIENT FOR        *
C     *    CROSS FLOW HEAT EXCHANGER: EVAPORATOR ANALYSIS              *
C     ******************************************************************
C
      REAL LT, LF, LS, LMIN, NT, NR, MUAIR, KAIR, NU, LTUBE, LPASS,
     .     MDOTR, NFIN, KFIN, LTUBE0
 
      DIMENSION A(0:4)
 
      DATA PI,SIGMA / 3.14159,0.1713E-8 /
      DATA IN, IO / 8, 9 /
      DATA PAIR / 14.696 /
      DATA A / 1.0, -0.02121, -0.66304, 0.403412, -0.07526 /
 
      OPEN(UNIT=IN, FILE='ERAEVAP.DAT', STATUS='OLD')
      OPEN(UNIT=IO, FILE='ERAEVAP.OUT', STATUS='UNKNOWN')
C
C        READ INPUT FILE
C
      READ(IN,'(1X)')
      READ(IN,'(1X)')
 
      READ(IN,*)ICONV
      READ(IN,'(1X)')
      IF(ICONV.EQ.1) THEN
      READ(IN,'(1X)')
      READ(IN,'(1X)')
        READ(IN,*) TUBE_OD
        READ(IN,*) TUBE_WALL
           DTO = TUBE_OD/25.4
           DTI = (TUBE_OD - 2.0*TUBE_WALL)/25.4
 
        READ(IN,*) NT
        READ(IN,*) LT
           LT = LT/2.54
 
        READ(IN,*) NR
        READ(IN,*) LF
           LF = LF/2.54
 
        READ(IN,*) LPASS
           LPASS = LPASS/2.54
           LTUBE0 = LPASS*NT*NR/12.0
 
        READ(IN,*) WIDTH
           WIDTH = WIDTH*LPASS
 
        READ(IN,*) NFIN
           NFIN = 2.54*NFIN
        READ(IN,*) TFIN
           TFIN = TFIN/25.4
 
        READ(IN,*) KFIN
           KFIN = KFIN/1.7296
 
        READ(IN,*) IFIN
 
        READ(IN,'(1X)')
        READ(IN,*) ITYPE
 
        READ(IN,'(1X)')
        READ(IN,*) VOLDOT
           VOLDOT = 2.119*VOLDOT
 
        DO 5 III=1,10
5       READ(IN,'(1X)')
 
      ELSE
C
C NATURAL CONVECTION INPUTS
C
        DO 10 III=1,18
10      READ(IN,'(1X)')
 
        READ(IN,'(1X)')
        READ(IN,'(1X)')
        READ(IN,*) INAT
        READ(IN,'(1X)')
        READ(IN,'(1X)')
        READ(IN,*) AEX1
          AEX1=AEX1/0.3048**2.0
        READ(IN,*) LTUBE0
          LTUBE0=LTUBE0/0.3048
        READ(IN,*) LPASS
           LPASS = LPASS/2.54
        READ(IN,*) TUBE_OD
        READ(IN,*) TUBE_WALL
           DTO = TUBE_OD/25.4
           DTI = (TUBE_OD - 2.0*TUBE_WALL)/25.4
      END IF
C
C OTHER INPUTS REQUIRED FOR BOTH HEAT EXCHANGERS
C
      READ(IN,'(1X)')
      READ(IN,'(1X)')
      READ(IN,*) TAIR
         TAIR = 1.8*TAIR + 32.0
      READ(IN,*) TSAT
         TSAT = 1.8*TSAT + 32.0
      READ(IN,*) MDOTR
         MDOTR = 2.20452*MDOTR
 
      READ(IN,*) FSUP
 
      READ(IN,*) XIN
      READ(IN,*) XOUT
        IF(XOUT.GT.1.0) XOUT=1.0
        IF(XOUT.LT.1.0) FSUP=0.0
C
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
C
      IF(ICONV.EQ.1.) THEN
C
C          FINAL UNITS CONVERSION (TO FT)
C
        DTO = DTO/12.0
        LT = LT/12.0
        LF = LF/12.0
        WIDTH = WIDTH/12.0
        LPASS = LPASS/12.0
        VOLDOT = VOLDOT*60.0
        DEPTH = NR*LF
        LS = 0.0
        LMIN = 0.0
 
        NFIN = NFIN*12.0
        TFIN = TFIN/12.0
 
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
        AFRONT = WIDTH*HEIGHT
C
C          FREE FLOW AREA CALCULATION
C
        AFREE = NT*WIDTH*(LMIN-(DTO+(LMIN-DTO)*TFIN*NFIN))
C
C          HEAT EXCHANGE AREA CALCULATION
C
        ATUBES = PI*DTO*WIDTH*(1.0-TFIN*NFIN)*NT*NR
     .       + PI*DTO*(LPASS - WIDTH)*NT*NR
        AFINS = 2.0*(LT*LF-PI*DTO**2/4.0)*NFIN*WIDTH*NT*NR
        AEX = ATUBES + AFINS
C
C          CALCULATE THE HYDRAULIC DIAMETER
C
        DH = 4.0*AFREE/AEX*DEPTH
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
        ICALL = IFIN + 1
        CALL NUSELT(ICALL, RE, PRAIR, NU)
C
C          CALCULATE THE AIR SIDE HEAT TRANSFER COEFFICIENT
C
        HAIR = NU*KAIR/DH
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
C          CALCULATE THE AIR SIDE THE CONDUCTANCE PER UNIT LENGTH
C
        emiss = 0.2
        HRAD=4.0*emiss*SIGMA*(TSAT+459.67)**3.0       !!! 2/13/93
        UAIR = (HAIR + hrad)*(ATUBES+ETAFIN*AFINS)    !!! 2/13/93
  !     UAIR = HAIR*(ATUBES+ETAFIN*AFINS)    !!!???
        LTUBE = NT*NR*LPASS
        ALENTH = AEX/LTUBE
        ULENTH = UAIR/LTUBE
      END IF
C
C
      IF(ICONV.EQ.2) THEN
C
C        NATURAL CONVECTION HEAT TRANSFER
C
        emiss = 0.2
        SELECT CASE(INAT)
C
          CASE(1)
C
C        VERTICAL FREE PLATE
C
           HAIR=0.19*(TAIR-TSAT)**0.333
           HRAD=4.0*emiss*SIGMA*(TSAT+459.67)**3.0
           AEX=2.0*AEX1
           UAIR=(HAIR+HRAD)*AEX             ! ASSUME "FIN EFFICIENCY" = 1
           ALENTH = AEX/LTUBE0
           ULENTH = UAIR/LTUBE0
C
          CASE(2)
C
C        VERTICAL PLATE IN WALL
C
           HAIR=0.19*(TAIR-TSAT)**0.333
           HRAD=4.0*emiss*SIGMA*(TSAT+459.67)**3.0
           AEX=AEX1
           UAIR=(HAIR+HRAD)*AEX             ! ASSUME "FIN EFFICIENCY" = 1
           ALENTH = AEX/LTUBE0
           ULENTH = UAIR/LTUBE0
C
          CASE(3)
C
C       HORIZONTAL PLATE NEAT TOP OF CABINET
C
           HATOP=0.055*(TAIR-TSAT)**0.333  ! NEED TO FIND CORRELATION
           HABOT=0.22*(TAIR-TSAT)**0.333
           HRAD=4.0*emiss*SIGMA*(TSAT+459.67)**3.0
           AEX=2.0*AEX1
           UAIR=((HATOP+HRAD)*AEX1+(HABOT+HRAD))*AEX1   ! ASSUME "FIN EFFICIENCY" = 1
           ALENTH = AEX/LTUBE0
           ULENTH = UAIR/LTUBE0
        END SELECT
      END IF
      WRITE(IO,920) AEX
      WRITE(IO,900) ULENTH
      WRITE(IO,910) ALENTH
 
C
C          SET UP THE DATA FOR THE EVAPORATOR ROUTINE AND CALL IT
C
      Tain = tair
      CALL EVAPHX(ICONV, TAIN, TSAT, MDOTR, DTI, LPASS, LTUBE0,
     .            XIN, XOUT, FSUP, ULENTH, ALENTH)
 
      CLOSE (IO)
   !  STOP
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
      SUBROUTINE EVAPHX(ICONV, TAIN, TSAT, MDOTR, DTI, LPASS, LTUBE0,
     .                  XIN, XOUT, FSUP, UALNTH, APERL)
C     ******************************************************************
C     *    CALCULATES EVAPORATION HEAT TRANSFER COEFFICIENTS          *
C     ******************************************************************
C
      REAL MDOT, MDOTL, MDOTV, MUL, MUV, KL, KV, MDOTR, NTUBE, NUV,
     .     LPASS, LB, LTUBE0, LSUP, LTUBE, JCONV
 
      COMMON /PROPS / PSAT, RHOL, RHOV, HL, HV, MUL, MUV, CPL, CPV, KL,
     .                KV, HFG, PRL, PRV
      DATA RB / 0.75 /, NTUBE / 1.0 /
      DATA JCONV / 778.16 /
      DATA PI, GC / 3.14159, 32.174 /
      DATA IO / 9 /
C
C          SET UP REMAINING NEEDED DATA
C
c      TSAT  = TAIN - 20.0
C      TAOUT = TAIN - 10.0
       IF(ICONV.EQ.1) THEN
         TAOUT = TAIN - 0.75*(TAIN-TSAT)
       END IF
       IF(ICONV.EQ.2) THEN
         TAOUT = TAIN-1.0
       END IF
C
C          FIND SATURATION PROPERTIES
C
      CALL PREAD
      CALL SATPRP(TSAT)
C
C          CALCULATE THE TUBE CROSS SECTIONAL AREA
C
      DTI = DTI/12.0
      AT = PI*DTI**2/4.0
      PT = PI*DTI
      LSUP = FSUP*LTUBE0
      LTUBE = (1.0 - FSUP)*LTUBE0
      DELX = XOUT - XIN
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
      REV = GREF*DTI/MUV
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
      DPDZV = FV/DTI*RHOV/GC*VVSEC**2/2.0
C
C         CONVERT TO PSI
C
      DPDZV = DPDZV/144.0
      DELPV = DPDZV*LSUP
C
C          HEAT TRANSFER COEFFICIENT
C
      HV = NUV*KV/DTI
C
C          CONDUCTANCE PER UNIT LENGHT
C
      UVLNTH = HV*PT
C
C          OVERALL CONDUCTANCE BASE ON AIR SIDE AREA
C
      USH = (1.0/(1.0/UVLNTH+1.0/UALNTH))/APERL
C
 
C
C          CALCULATE THE AVERAGE BOILING HEAT TRANSFER COEFFICIENT
C          USING THE BO PIERRE CORRELATION
C
      REL = GREF*DTI/MUL
      AAA = JCONV*DELX*HFG/LTUBE
 !    IF(XOUT.LE.0.9) THEN
 !      HBOIL = 0.0009*KL/DTI*REL*AAA**0.5
 !    ELSE
        HBOIL = 0.0082*KL/DTI*REL**0.8*AAA**0.4
 !    END IF
C
C          CALCULATE THE CONDUCTANCE PER UNIT LENGTH
C
      UBLNTH = HBOIL*PT
C
C          CALCULATE THE OVERALL CONDUCTANCE BASED ON AIR SIDE AREA
C
      UTPL = 1.0/(1.0/UBLNTH+1.0/UALNTH)
      UTP = UTPL/APERL
 
      WRITE(IO,920) USH
      WRITE(IO,930) UTP
C
C          BEGIN CALCULATION FOR CONDENSATION REGION
C
C          CONVERT UNITS TO SI UNITS
C
      RHOL = RHOL/(2.2*0.3048**3)
      RHOV = RHOV/(2.2*0.3048**3)
      MUL = MUL/(2.2*3600.*0.3048)
      MUV = MUV/(2.2*3600.*0.3048)
      HFG = HFG*1055.0*2.2
      MDOT = MDOT/(2.2*3600.0)
      GREF = GREF/(3600.0*0.3048**2*2.2)
      DTI = DTI*0.3048
      AT = PI/4.0*DTI**2
      PT = PT*0.3048
      LPASS = LPASS*0.3048
      RB = RB/12.*0.3048
      DB = 2.0*RB
      LB = PI*DB/2.0
      TSAT = (TSAT+460.0)/1.8
      TAIN = (TAIN+460.0)/1.8
      TAOUT = (TAOUT+460.0)/1.8
      UTPL = UTPL*1.8*1055.0/(3600.0*0.3048)
 
C
C          CALCULATE THE LMTD
C
      DTIN =  TAIN - TSAT
      DTOUT = TAOUT - TSAT
      DELT = (DTIN-DTOUT)/LOG(DTIN/DTOUT)
C
C          CALCULATE THE MASS FLUX
C
      Z = 0.0
      X = XIN
      DELTAX = 0.001
      ZPASS = 0
 
      DO WHILE (X .LT. XOUT)
C
C          CALCULATE THE MASS FLOW RATE OF EACH PHASE
C
         MDOTL = MDOT*(1.0-X)
         MDOTV = MDOT*X
C
C          CALCULATE THE MASS FLUX OF EACH PHASE
C
         GL = GREF*(1.0-X)
         GV = GREF*X
C
C          CALCULATE THE SUPERFICIAL REYNOLDS NUMBER FOR EACH PHASE
C
         REL = GL*DTI/MUL
         IF(REL .EQ. 0.0) REL = 0.00001
         REV = GV*DTI/MUV
C
C          CALCULATE THE FRICTION FACTOR OF EACH PHASE
C
         CALL FFACT(REL,FL)
         CALL FFACT(REV,FV)
C
C          CALCULATE THE FRICTIONAL PRESSURE DROP FOR EACH PHASE
C
         DPDZL = FL/DTI*GL**2/(2.0*RHOL)
         DPDZV = FV/DTI*GV**2/(2.0*RHOV)
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
            IF(CHIVT.EQ.0.0) CHIVT = 0.000000001
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
            FBF = (REV*(DTI/DB)**2)**0.05
            DPB = FBF*DPDZTP*LB
            ZPASS = 0.0
         END IF
C
         QLENTH = UTPL*DELT
         DELTAZ = MDOT*HFG*DELTAX/QLENTH
         DP = DPDZTP*DELTAZ
         DELP = DELP + DP + DPB
         Z = Z + DELTAZ
         ZPASS = ZPASS + DELTAZ
C
C          CONVERT TO ENGLISH UNITS
C
         DELPE = 0.00014504*DELP
         ZE = Z/0.3048
         X=X+DELTAX
      END DO
 
      DELPTP = DELPE
      DPEVAP = DELPTP + DELPV
 
      WRITE(IO,940) DPEVAP
 
    ! STOP
      RETURN
C
C          FORMAT STATEMENTS
C
  920 FORMAT(F10.3, 9X, 'VAPOR     REGION CONDUCTANCE, BTU/HR-FT**2-F')
  930 FORMAT(F10.3, 9X, 'TWO-PHASE REGION CONDUCTANCE, BTU/HR-FT**2-F')
  940 FORMAT(F10.3, 9X, 'TWO-PHASE PRESSURE DROP, PSI')
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
      IF(frac .lt. 0.0) THEN
         j = 1
         frac = 0.0
      END IF
 
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