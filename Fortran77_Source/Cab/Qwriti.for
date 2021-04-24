$DEBUG
      SUBROUTINE QWRITI(VFF,VFZ)
C     ******************************************************************
C     *    ECHO BACK THE INPUT DATA AND PERFORM ERROR ANALYSIS         *
C     ******************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER TITLE(68,5)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / QLTWO /  DCOMP, WALL
      COMMON / QLTF /   HIWP
      COMMON / QLONE /  TMF,TMS,TMB
      COMMON / QLFIVE / CWIDE,CHGT,SCIN,STIN,TKIN
      COMMON / QLMISC / TOPMUL,THMUL,TIFT,TIRT,CINSUL,HLRG,CKMUL,HLFZG,
     .                  DGSKT,CDUP,CDDN,CCHGT,NCCTYPE                     !!!
      COMMON / QLN5 /   WKIN,WEDGE,FLANGE,FLGB,WEDGER,FLANGER,wkinr       !!!
      COMMON / REDWRI / TITLE
      COMMON / CNDNSR / TACOND
      COMMON / INPTAB / EDH, TCYDEF, EHTR, EFAN, EMISC, TCOMON,
     .                  FEFLOW, REFLOW, RLSAV, TWSAV, FLOW, IAIRWT
      COMMON / ERA /    IRFTYP, FZHEAT, FFHEAT, WATERZ, WATERF, HXVUR,
     .                  HXVUZ
      COMMON / LINER / DOL, DIL, COL, CIL
      DATA CAL_BTU / 0.069380 /
C
C          UNIT CONVERSION FUNCTIONS
C
      F1(T) = (T - 32.0)/1.8                 !F to C
      F2(X) = 2.54*X                         !in to cm
      F3(V) = 28.317*V                       !ft3 to liters
      F4(R) = CAL_BTU*R                      !resistivity
      F5(G) = G/0.048174
      F7(W) = W/2.200264
C
C          WRITE OUT THE RUN TITLE
C
      DO I = 1, 5
         WRITE(IO,1000) (TITLE(IW,I), IW = 1,68)
      END DO
C
C          OUTPUT THE TYPE OF REFRIGERATOR
C
      SELECT CASE (IRFTYP)
         CASE (1)
            WRITE(IO,1002)
 
         CASE (2)
            WRITE(IO,1005)
 
         CASE (3)
            WRITE(IO,1001)
 
         CASE (4)
            WRITE(IO,1003)
 
         CASE (5)
            WRITE(IO,1004)
 
         CASE (6)
            WRITE(IO,1006)
 
         CASE (7)
            WRITE(IO,1007)
 
      END SELECT
 
      RMOD=NMOD
C
C          DIMENSIONAL SPECIFICATION
C
      WRITE(IO,900) IRFTYP
      WRITE(IO,901)
      WRITE(IO,902) F2(HEIGHT), F2(WIDTH), F2(DEPTH)
      WRITE(IO,924) 25.4*DOL, 25.4*DIL
C
C         CHECK FOR NON-ZERO WEDGE THICKNESS
C
      SELECT CASE (NMOD)                                                  !!!
         CASE (2, 3, 8)                                                   !!!
            WRITE(IO,890) F2(WEDGE), F2(FLANGE)                           !!!
            WRITE(IO,891) F2(WEDGER),F2(FLANGER)                          !!!
            IF(WEDGE .EQ. 0.0) THEN                                       !!!
               CALL GOTOXY(12,12)
               CALL PRINT('Freezer Wedge Should not be Zero ',33,-2)
               CALL PRINT('Thickness.  Run Aborted',23,-2)
               call wait (36)
               CALL EXIT                                                  !!!
            END IF                                                        !!!
         CASE (4, 7)
            WRITE(IO,903) F2(WEDGE), F2(FLANGE)
            IF(WEDGE .EQ. 0.0 .and. nmod .eq. 7) THEN
               CALL GOTOXY(12,12)
               CALL PRINT('Freezer Wedge Should not be Zero ',33,-2)
               CALL PRINT('Thickness.  Run Aborted',23,-2)
               call wait (36)
               CALL EXIT
            END IF
      END SELECT                                                          !!!
C
C          COMPLETE OUTPUT OF DIMENSIONAL SPECIFICATION
C
      SELECT CASE (NMOD)
         CASE (2)                                          !Side by side
            WRITE(IO,904)  F2(WALL), F2(THMUL)
            WRITE(IO,892)  F2(CDUP), F2(CDDN), F2(CCHGT)                  !!!
            WRITE(IO,905)  F2(TIFT), F2(TIFRS), F2(TIFF), F2(TIFB)
            WRITE(IO,906)  F2(TIRT), F2(TIRLS), F2(TIRF), F2(TIRB)
 
            WRITE(IO,907)  F2(BINSUL), F2(BINFRZ)
 
         CASE (3)                                          !Top mount
            WRITE(IO,908)  F2(TOPMUL), F2(THMUL)
            WRITE(IO,892)  F2(CDUP), F2(CDDN), F2(CCHGT)                  !!!
            WRITE(IO,909)  F2(TIFT),   F2(TIFLS), F2(TIFRS),
     .                     F2(TIFF),   F2(TIFB)
            WRITE(IO,910)  F2(TIRLS),  F2(TIRRS), F2(TIRF), F2(TIRB),
     .                     F2(BINSUL)
 
         CASE (5)
            WRITE(IO,911) F2(CWIDE), F2(CHGT)
            WRITE(IO,912) F2(TIFT),  F2(TIFRS), F2(TIFF), F2(TIFB)
            TIFLS = TIFRS
 
            WRITE(IO,913) F2(BINSUL)
            WRITE(IO,914) F2(SCIN), F2(STIN)
 
         CASE (4, 7)
            WRITE(IO,892)  F2(CDUP), F2(CDDN), F2(CCHGT)                  !!!
            WRITE(IO,915)  F2(TIFT), F2(TIFLS), F2(TIFRS), F2(TIFF),
     .                     F2(TIFB)
 
            WRITE(IO,913) F2(BINSUL)
 
         CASE (8)
            WRITE(IO,908) F2(TOPMUL), F2(THMUL)
            WRITE(IO,892)  F2(CDUP), F2(CDDN), F2(CCHGT)                  !!!
            WRITE(IO,917) F2(TIFRS), F2(TIFF),  F2(TIFB), F2(BINSUL)
            WRITE(IO,918) F2(TIRT),  F2(TIRLS), F2(TIRF), F2(TIRB)
 
      END SELECT
C
C          OUTPUT CALCULATED AND INPUT INTERNAL VOLUME VALUES
C
      WRITE(IO,919)
      WRITE(IO,920)
      IF (NMOD .EQ. 4) THEN
         WRITE(IO,923)  F3(VOLAZ)
      ELSE
         WRITE(IO,921)  F3(VFZ), F3(HXVUZ), F3(VOLAZ)
      END IF
 
      IF((NMOD .LE. 4) .OR. (NMOD .EQ. 8)) THEN
         WRITE(IO,922)
         WRITE(IO,921)  F3(VFF), F3(HXVUR), F3(VOLAR)
      END IF
C
C          OUTPUT TEMPERATURES AND THERMAL CHARACTERISTICS
C
      SELECT CASE (NMOD)
         CASE (2, 3, 8)
            WRITE(IO,1100) F1(TROOM), F1(TFRZ), F1(TFF), F1(TBTM)
 
            RFRESH = 1.0/(RKINFF * 12.0)
            RFREEZ = 1.0/(RKINFZ * 12.0)
            RWEDGEr = 1.0/(WKINr   * 12.0)
            RWEDGE = 1.0/(WKIN   * 12.0)
            RDRFF  = 1.0/(DKINFF * 12.0)
            RDRFZ  = 1.0/(DKINFZ * 12.0)
            RMUL   = 1.0/(CKMUL  * 12.0)
 
            WRITE(IO,1101) F4(RFRESH), F4(RFREEZ), F4(RWEDGEr),
     .                     F4(RWEDGE), F4(RDRFF),  F4(RDRFZ),  F4(RMUL)
 
            IF(NMOD .EQ. 3) THEN
               WRITE(IO,1102) F5(HLFZG), F5(HLRG)
            ELSE
               WRITE(IO,1103) F5(HLGZF), F5(HLRG)
            END IF
 
         CASE (4)
            WRITE(IO,1100) F1(TROOM), F1(TFRZ), F1(TFF), F1(TBTM)
 
            RCAB = 1.0/(RKIN * 12.0)
            RWEDGE = 1.0/(WKIN * 12.0)
            RDOOR = 1.0/(DKIN * 12.0)
            WRITE(IO,1107) F4(RCAB), F4(RWEDGE), F4(RDOOR)
 
            WRITE(IO,1106) F5(HLFZG)
 
         CASE (5)
            WRITE(IO,1104) F1(TROOM), F1(TFRZ), F1(TBTM)
 
            RFREEZ = 1.0/(RKIN * 12.0)
            RTOP = 1.0/(TKIN * 12.0)
            WRITE(IO,1105) F4(RFREEZ), F4(RTOP)
 
            WRITE(IO,1106) F5(HLFZG)
 
         CASE (7)
            WRITE(IO,1104) F1(TROOM), F1(TFRZ), F1(TBTM)
 
            RCAB = 1.0/(RKIN * 12.0)
            RWEDGE = 1.0/(WKIN * 12.0)
            RDOOR = 1.0/(DKIN * 12.0)
            WRITE(IO,1107) F4(RCAB), F4(RWEDGE), F4(RDOOR)
 
            WRITE(IO,1106) F5(HLFZG)
 
      END SELECT
C
C           Door Opening Characterization
C
      SELECT CASE (NMOD)
         CASE (2, 3, 8)
            WRITE(IO,1200) F1(TDRAIR), RELHUM, FFCOPN, SECFFC,
     .                     FRZOPN, SECFRZ
 
         CASE (4, 5, 7)
            WRITE(IO,1201) F1(TDRAIR), RELHUM, FRZOPN, SECFRZ
 
      END SELECT
C
C           Anti-Sweat Heater & Auxiliary Energy
C
      SELECT CASE (NMOD)
         CASE (2, 3, 8)
            WRITE(IO,1300) FFASHW, FZASHW, FFAUXW, FZAUXW, OTHERW
 
         CASE (4, 5, 7)
            WRITE(IO,1301) FZASHW, FZAUXW, OTHERW
 
      END SELECT
C
C          PASS DATA VALUES TO THE CYCLE MODEL
C
      IF(ICYCL .EQ. 4) THEN
         SELECT CASE (NMOD)
            CASE (2, 3, 8)
               WRITE(ICYCL,800) FFASHW,    FFAUXW,
     .                          FZASHW,    FZAUXW,  OTHERW,
     .                          F1(TROOM), F1(TFF), F1(TFRZ)
 
            CASE (4)
               WRITE(ICYCL,800) FZASHW,    FZAUXW,
     .                          FZASHW,    FZAUXW,   OTHERW,
     .                          F1(TROOM), F1(TFF), F1(TFRZ)
 
            CASE (5, 7)
               WRITE(ICYCL,800) FZASHW,    FZAUXW,
     .                          FZASHW,    FZAUXW,   OTHERW,
     .                          F1(TROOM), F1(TFRZ), F1(TFRZ)
 
         END SELECT
      END IF
 
      RETURN
C
C     FORMAT STATEMENTS
C
  800 FORMAT(/'CABINET LOADS DATA'/
     .      F10.2,T19,'FRESH FOOD ANTISWEAT HEATER (W)'/
     .      F10.2,T19,'FRESH FOOD AUXILIARY POWER (W)'/
     .      F10.2,T19,'FREEZER ANTISWEAT HEATER (W)'/
     .      F10.2,T19,'FREEZER AUXILIARY POWER (W)'/
     .      F10.2,T19,'OUTSIDE CABINET POWER (W)'/
     .      F10.2,T19,'ROOM TEMPERATURE (C)'/
     .      F10.2,T19,'FRESH FOOD TEMPERATURE (C)'/
     .      F10.2,T19,'FREEZER TEMPERATURE (C)')
 
  890 FORMAT(T15,'FREEZER:'/                                              !!!
     .     T20,'THE WEDGE IS',F7.2,' CM DEEP'/                            !!!
     .     T20,'THE FLANGES ARE',F7.2,' CM WIDE')                         !!!
  891 FORMAT(T15,'FRESH FOOD COMPARTMENT:'/                               !!!
     .     T20,'THE WEDGE IS',F7.2,' CM DEEP'/                            !!!
     .     T20,'THE FLANGES ARE',F7.2,' CM WIDE')                         !!!
  892 FORMAT(T15,'COMPRESSOR COMPARTMENT DIMENSIONS:'/                    !!!
     .     T20,'TOP DEPTH:',F6.2,' CM',' BOTTOM DEPTH:',                  !!!
     .     F6.2,' CM',' HEIGHT:',F6.2,' CM')                              !!!
 
  900 FORMAT(/T10,'CONFIGURATION TYPE:',I2)
 
  901 FORMAT(//T10,'DIMENSIONAL SPECIFICATIONS'/)
  902 FORMAT(T15,'OVERALL:',F7.2,'  CM HIGH X',F7.2,
     .     '  CM WIDE X',F7.2,'  CM DEEP')
  903 FORMAT(T15,'THE WEDGE IS',F7.2,' CM DEEP'/
     .     T15'THE FLANGES ARE',F7.2,' CM WIDE')
  904 FORMAT(T15,'THE SIDE OF THE MULLION IS',F7.2,' CM FROM THE',
     .     ' OUTSIDE WALL'/T20,'OF THE FRESH FOOD COMPARTMENT'/T15,
     .     'THE MULLION IS',F7.2,' CM THICK')
  905 FORMAT(//T15,'INSULATION THICKNESS (INCHES)'/
     .     T20,'FREEZER'/
     .     T25,'TOP',         T46,F7.2,' CM'/
     .     T25,'RIGHT SIDE',  T46,F7.2,' CM'/
     .     T25,'FRONT (DOOR)',T46,F7.2,' CM'/
     .     T25,'BACK',        T46,F7.2,' CM')
  906 FORMAT(T20,'FRESH FOOD COMPARTMENT'/
     .     T25,'TOP',         T46,F7.2,' CM'/
     .     T25,'LEFT SIDE',   T46,F7.2,' CM'/
     .     T25,'FRONT (DOOR)',T46,F7.2,' CM'/
     .     T25,'BACK',        T46,F7.2,' CM')
  907 FORMAT(T20,'BOTTOM'/
     .       T25,'FRESH FOOD',T46,F7.2,' CM'/
     .       T25,'FREEZER',   T46,F7.2,' CM')
  908 FORMAT(T15,'THE TOP OF THE MULLION IS',F7.2,' CM FROM THE TOP ',
     .     'OF THE CABINET'/T15,'THE MULLION IS',F7.2,' CM THICK')
  909 FORMAT(//T15,'INSULATION THICKNESS'/
     .     T20,'FREEZER'/
     .     T25,'TOP  ',       T46,F7.2,' CM'/
     .     T25,'LEFT SIDE',   T46,F7.2,' CM'/
     .     T25,'RIGHT SIDE',  T46,F7.2,' CM'/
     .     T25,'FRONT (DOOR)',T46,F7.2,' CM'/
     .     T25,'BACK ',       T46,F7.2,' CM')
  910 FORMAT(//T15,'INSULATION THICKNESS'/
     .     T20,'FRESH FOOD'/
     .     T25,'LEFT SIDE',   T46,F7.2,' CM'/
     .     T25,'RIGHT SIDE',  T46,F7.2,' CM'/
     .     T25,'FRONT (DOOR)',T46,F7.2,' CM'/
     .     T25,'BACK ',       T46,F7.2,' CM'/
     .     T25,'BOTTOM',      T46,F7.2,' CM')
  911 FORMAT(T15,'HEIGHT OF COMPRESSOR COMPARTMENT: ',F7.2,' CM'/
     .       T15,'WIDTH  OF COMPRESSOR COMPARTMENT: ',F7.2,' CM')
  912 FORMAT(//T15,'INSULATION THICKNESS (CM)'/
     .     T20,'FREEZER'/
     .     T25,'TOP',         T46,F7.2,' CM'/
     .     T25,'SIDE',        T46,F7.2,' CM'/
     .     T25,'FRONT (DOOR)',T46,F7.2,' CM'/
     .     T25,'BACK',        T46,F7.2,' CM')
  913 FORMAT(T25,'BOTTOM',    T46,F7.2,' CM')
  914 FORMAT(T25,'COMPARTMENT SIDE',T46,F7.2,' CM'/
     .       T25,'COMPARTMENT TOP ',T46,F7.2,' CM')
  915 FORMAT(//T15,'INSULATION THICKNESS (CM)'/
     .     T25,'TOP',         T46,F7.2,' CM'/
     .     T25,'LEFT SIDE',   T46,F7.2,' CM'/
     .     T25,'RIGHT SIDE',  T46,F7.2,' CM'/
     .     T25,'FRONT (DOOR)',T46,F7.2,' CM'/
     .     T25,'BACK',        T46,F7.2,' CM')
  917 FORMAT(//T15,'INSULATIONS'/
     .      T20,'FREEZER'/
     .      T25,'SIDE',        T46,F7.2,' CM'/
     .      T25,'FRONT (DOOR)',T46,F7.2,' CM'/
     .      T25,'BACK',        T46,F7.2,' CM'/
     .      T25,'BOTTOM',      T46,F7.2,' CM')
  918 FORMAT(T20,'FRESH FOOD COMPARTMENT'/
     .     T25,'TOP',         T46,F7.2,' CM'/
     .     T25,'SIDE',        T46,F7.2,' CM'/
     .     T25,'FRONT (DOOR)',T46,F7.2,' CM'/
     .     T25,'BACK' ,       T46,F7.2,' CM')
  919 FORMAT(/T10,'VOLUMES'/)
  920 FORMAT(T15,'FREEZER REFRIGERATED VOLUME')
  921 FORMAT(T20,'CALCULATED',T46,F7.2,' LITER'/
     .     T20,'SPECIFIED'/
     .     T25,'SHELF/EVAP',  T46,F7.2,' LITER'/
     .     T25,'NET VOLUME',  T46,F7.2,' LITER')
  922 FORMAT(/T15,'GENERAL REFRIGERATED VOLUME')
  923 FORMAT(T20,'SPECIFIED',T46,F7.2,' LITER')
  924 FORMAT(T15,'OUTER LINER:', F5.1, '  MM,   INNER LINER:',
     .     F5.1, ' MM')
 
 1000 FORMAT(T10,68A1)
 1001 FORMAT(T10,'THERMAL ANALYSIS OF A ',
     .     'SIDE-BY-SIDE REFRIGERATOR/FREEZER'/)
 1002 FORMAT(T10,'THERMAL ANALYSIS OF A TWO-DOOR TOP-MOUNT',
     .     ' REFRIGERATOR/FREEZER'/)
 1003 FORMAT(T10,'THERMAL ANALYSIS OF A CHEST FREEZER'/)
 1004 FORMAT(T10,'THERMAL ANALYSIS OF AN UPRIGHT',
     .     ' FREEZER'/)
 1005 FORMAT(T10,'THERMAL ANALYSIS OF A TWO-DOOR',
     .      'BOTTOM-MOUNT REFRIGERATOR/FREEZER')
 1006 FORMAT(T10,'THERMAL ANALYSIS OF A ',
     .      'ONE-DOOR REFRIGERATOR')
 1007 FORMAT(T10,'THERMAL ANALYSIS OF A ',
     .      'ONE-DOOR REFRIGERATOR/FREEZER')
 
 1100 FORMAT(/T10,'TEMPERATURES'/
     .       T15,'ROOM',                  T46,F7.2,' DEG C'/
     .       T15,'FREEZER CABINET',       T46,F7.2,' DEG C'/
     .       T15,'FRESH FOOD CABINET',    T46,F7.2,' DEG C'/
     .       T15,'AIR UNDER REFRIGERATOR',T46,F7.2,' DEG C')
 1101 FORMAT(/T10,'THERMAL CHARACTERISTICS'/
     .     T15,'THERMAL RESISTIVITY'/
     .     T20,'FRESH FOOD CABINET',T46,F7.4,' m2-C/W-cm'/
     .     T20,'FREEZER CABINET',   T46,F7.4,' m2-C/W-cm'/
     .     T20,'FRESH FOOD WEDGE',  T46,F7.4,' m2-C/W-cm'/
     .     T20,'FREEZER WEDGE',     T46,F7.4,' m2-C/W-cm'/
     .     T20,'FRESH FOOD DOOR',   T46,F7.4,' m2-C/W-cm'/
     .     T20,'FREEZER DOOR',      T46,F7.4,' m2-C/W-cm'/
     .     T20,'MULLION',           T46,F7.4,' m2-C/W-cm'/)
 1102 FORMAT(T15,'GASKET HEAT LEAK'/T20,'FREEZER',16X,F10.4,' W/m-C'/
     .      T20,'REFRIGERATOR',11X,F10.4,' W/m-C')
 1103 FORMAT(T15,'GASKET HEAT LEAK'/
     .     T20,'FREEZER        ',T43,F10.4,' W/m-C'/
     .     T20,'REFRIGERATOR',   T43,F10.4,' W/m-C')
 1104 FORMAT(/T10,'TEMPERATURES'/
     .       T15,'ROOM',                  T46,F7.2,' DEG C'/
     .       T15,'FREEZER CABINET',       T46,F7.2,' DEG C'/
     .       T15,'AIR UNDER CABINET',     T46,F7.2,' DEG C')
 1105 FORMAT(/T10,'THERMAL CHARACTERISTICS'/
     .     T15,'THERMAL RESISTIVITY'/
     .     T20,'FREEZER CABINET',      T46,F7.4,' m2-C/W-cm'/
     .     T20,'FREEZER TOP (DOOR)',   T46,F7.4,' m2-C/W-cm')
 1106 FORMAT(T15,'GASKET HEAT LEAK',T43,F10.4,' W/m-C')
 1107 FORMAT(/T10,'THERMAL CHARACTERISTICS'/
     .     T15,'THERMAL RESISTIVITY'/
     .     T20,'CABINET',              T46,F7.4,' m2-C/W-cm'/
     .     T20,'WEDGE',                T46,F7.4,' m2-C/W-cm'/
     .     T20,'DOOR',                 T46,F7.4,' m2-C/W-cm')
 
 1200 FORMAT(//T10,'DOOR OPENINGS'/
     .     T20,'AIR TEMPERATURE',      T46,F7.1,' Deg C'/
     .     T20,'RELATIVE HUMIDITY',    T46,F7.1,' (%)'/
     .     T20,'FRESH FOOD COMPARTMENT',/
     .     T22,'OPENINGS/HR',          T46,F7.1,' #/HR'/
     .     T22,'DURATION OF 1 OPENING',T46,F7.1,' SECONDS'/
     .     T20,'FREEZER COMPARTMENT',/
     .     T22,'OPENINGS/HR',          T46,F7.1,' #/HR'/
     .     T22,'DURATION OF 1 OPENING',T46,F7.1,' SECONDS')
 1201 FORMAT(//T10,'DOOR OPENINGS'/
     .     T20,'AIR TEMPERATURE',      T46,F7.1,' Deg C'/
     .     T20,'RELATIVE HUMIDITY',    T46,F7.1,' (%)'/
     .     T20,'OPENINGS/HR',          T46,F7.1,' #/HR'/
     .     T20,'DURATION OF 1 OPENING',T46,F7.1,' SECONDS')
 
 1300 FORMAT(/T10,'ANTI-SWEAT HEATERS'/
     .     T20,'FRESH FOOD CABINET',T46,F7.2,' W'/
     .     T20,'FREEZER CABINET',   T46,F7.2,' W'/
     .     /T10,'AUXILIARY ENERGY'/
     .     T20,'FRESH FOOD CABINET',T46,F7.2,' W'/
     .     T20,'FREEZER CABINET',   T46,F7.2,' W'/
     .     T20,'OUTSIDE CABINET',   T46,F7.2,' W')
 
 1301 FORMAT(/T10,'ANTI-SWEAT HEATERS'/
     .     T20,'CABINET',           T46,F7.2,' W'/
     .     /T10,'AUXILIARY ENERGY'/
     .     T20,'INSIDE CABINET',    T46,F7.2,' W'/
     .     T20,'OUTSIDE CABINET',   T46,F7.2,' W')
 
      END
