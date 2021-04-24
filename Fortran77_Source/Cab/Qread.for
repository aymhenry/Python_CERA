$debug
      SUBROUTINE QREAD(VFF, VFZ)
C     ******************************************************************
C     *    READ THE INPUT DATA
C     ******************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER TITLE(68,5)
      CHARACTER*80 A
      CHARACTER*3 CMONTH(12)
 
$INCLUDE:'COMMON.FOR'
 
      DIMENSION ITYMOD(7)
 
      COMMON / QLTWO /  DCOMP, WALL
      COMMON / QLONE /  TMF,TMS,TMB
      COMMON / QLFOUR / FH, FW, FD
      COMMON / QLFIVE / CWIDE,CHGT,SCIN,STIN,TKIN
      COMMON / QLTF /   HIWP
      COMMON / QLMISC / TOPMUL,THMUL,TIFT,TIRT,CINSUL,HLRG,CKMUL,HLFZG,
     .                  DGSKT,CDUP,CDDN,CCHGT,NCCTYPE                     !!!
      COMMON / QLN5 /   WKIN,WEDGE,FLANGE,FLGB,WEDGER,FLANGER,wkinr      !!!
      COMMON / QLEEK /  AMUL, UMUL, QINTER
      COMMON / REDWRI / TITLE
      COMMON / ERA /    IRFTYP, FZHEAT, FFHEAT, WATERZ, WATERF, HXVUR,
     .                  HXVUZ
      COMMON / PENAT / FFPENA, FZPENA
      COMMON / LINER / DOL, DIL, COL, CIL
C
      DATA CMONTH / 'JAN','FEB','MAR','APR','MAY','JUN',
     +              'JUL','AUG','SEP','OCT','NOV','DEC' /
      DATA ITYMOD / 3, 8, 2, 5, 7, 4, 3 /
      DATA CAL_BTU / 0.069380 /
C
C        INITIALIZE VARIABLES
C
      UMUL = 0.
      AMUL = 0.
      FHOT = 0
      ACOMP = 0
      BCOMP = 0
      DCOMP = 0
      BOTTOM = 0
C
C        READ TITLE LINES
C
      DO I = 1, 5
         CALL INASTK(A)
         READ(A,2000) (TITLE(IR,I),IR=1,68)
      END DO
C
C        PRINT THE DATE AND TIME
C
      CALL GETDAT(IYEAR, IMONTH, IDAY)
      CALL GETTIM(IHOUR, IMIN, ISEC)
      WRITE(IO,900) IHOUR, IMIN, ISEC, IDAY, CMONTH(IMONTH), IYEAR
C
C          CABINET TYPE CODE
C
C     IRFTYP IS THE CABINET TYPE USED BY ERA
C          1: TWO-DOOR TOP-MOUNT REFRIGERATOR/FREEZER
C          2: TWO-DOR BOTTOM-MOUNT REFRIGERATOR/FREEZER
C          3: SIDE-BY-SIDE REFRIGERATOR/FREEZER
C          4: CHEST FREEZER
C          5: UPRIGHT FREEZER
C          6: ONE-DOOR REFRIGERATOR
C          7: TWO-DOOR REFRIGERATOR/FREEZER
C
C     RMOD IS THE INTERNAL NUMBER OF THE CONFIGURATION TO BE RUN
C          2: SIDE-BY-SIDE REFRIGERATOR/FREEZER
C          3: TOP-MOUNT REFRIGERATOR/FREEZER
C          5: CHEST FREEZER
C          7: UPRIGHT FREEZER
C          8: BOTTOM-MOUNT REFRIGERATOR/FREEZER
C          4: SINGLE-DOOR REFRIGERATOR
C
      CALL INASTK(A)
      READ(A,2002) NMOD
      IRFTYP = NMOD
      NMOD = ITYMOD (IRFTYP)
C
C        CABINET DIMENSIONS
C
C        HEIGHT-OVERALL HEIGHT OF THE UNIT (INCHES)
C        WIDTH-OVERALL WIDTH OF THE UNIT (INCHES)
C        DEPTH-OVERALL DEPTH OF THE UNIT, EXCLUDING HANDLE (INCHES)
C        BOTTOM-HEIGHT FROM COMPARTMENT FLOOR TO BOTTOM OF UNIT (INCHES)
C        WEDGE-DEPTH OF THE WEDGE (INCHES)
C        FLANGE-WIDTH OF THE WEDGE ON TOP AND SIDES OF UNIT (INCHES)
C        FLGB-WIDTH OF WEDGE ON BOTTOM OF UNIT (INCHES)
C
      SELECT CASE (NMOD)
         CASE (3,8,2)
            CALL RDINDT (HEIGHT)
            CALL RDINDT (WIDTH)
            CALL RDINDT (DEPTH)
            CALL RDINDT (WEDGE)
            CALL RDINDT (FLANGE)
            CALL RDINDT (WEDGER)
            CALL RDINDT (FLANGER)
            CALL RDINDT (DOOREDG)
            CALL RDINDT (DGSKT)
            FLGB = FLANGER
 
         CASE (5)
            CALL RDINDT (HEIGHT)
            CALL RDINDT (WIDTH)
            CALL RDINDT (DEPTH)
            CALL RDINDT (DOOREDG)
            CALL RDINDT (DGSKT)
 
         CASE (4, 7)
            CALL RDINDT (HEIGHT)
            CALL RDINDT (WIDTH)
            CALL RDINDT (DEPTH)
            CALL RDINDT (WEDGE)
            CALL RDINDT (FLANGE)
            CALL RDINDT (DOOREDG)
            CALL RDINDT (DGSKT)
            FLGB = FLANGE
      END SELECT
 
      HEIGHT = HEIGHT/2.54
      WIDTH  = WIDTH/2.54
      DEPTH  = DEPTH/2.54
      WEDGE  = WEDGE/2.54
      FLANGE = FLANGE/2.54
      FLGB   = FLGB/2.54
      WEDGER = WEDGER/2.54                                                !!!
      FLANGER = FLANGER/2.54                                              !!!
      DGSKT = DGSKT/2.54                                                  !!!
C
C        COMPRESSOR COMPARTMENT DIMENSIONS
C
C        CWIDE-WIDTH OF COMPRESSOR COMPARTMENT FROM OUTSIDE WALL TO INNER
C             (FREEZER SIDE) WALL (INCHES)
C        CHGT-HEIGHT OF COMPRESSOR COMPARTMENT (INCHES)
C
      SELECT CASE (NMOD)
 
         CASE (2, 3, 4, 7, 8)
            CALL RDINDT (CDUP)
            CALL RDINDT (CDDN)
            CALL RDINDT (CCHGT)
            IF(CCHGT.NE.0.0) THEN                                         !!!
                IF(CDUP.NE.0.0) THEN                                      !!!
                    NCCTYPE = 3                                           !!!
                ELSE                                                      !!!
                    NCCTYPE = 2                                           !!!
                ENDIF                                                     !!!
            ELSE                                                          !!!
                NCCTYPE = 1                                               !!!
            ENDIF                                                         !!!
 
         CASE (5)
            CALL RDINDT (CWIDE)
            CALL RDINDT (CHGT)
 
      END SELECT
 
      CDUP  = CDUP/2.54                                                   !!!
      CDDN  = CDDN/2.54                                                   !!!
      CCHGT = CCHGT/2.54                                                  !!!
      CWIDE = CWIDE/2.54
      CHGT  = CHGT /2.54
C
C        LINER DATA
C
C        DOL-THICKNESS OF OUTER LINER (INCHES)
C        DIL-THICKNESS OF INNER LINER (INCHES)
C        COL-CONDUCTIVITY OF OUTER LINER (BTU/HR-FT-F)
C        CIL-CONDUCTIVITY OF INNER LINER (BTU/HR-FT-F)
C
      CALL RDINDT (DOL)
      CALL RDINDT (COL)
      CALL RDINDT (DIL)
      CALL RDINDT (CIL)
      DOL = DOL / 25.4
      DIL = DIL / 25.4
      COL = COL / 1.7307
      CIL = CIL / 1.7307
      DINS = DOL+DIL
C
C        MULLION DATA
C
C        TOPMUL-DISTANCE FROM OUTER TOP OF UNIT TO TOP OF MULLION (INCHES)
C        THMUL-TOTAL THICKNESS OF THE MULLION SECTION (INCHES)
C        WALL-THE DISTANCE FROM THE OUTSIDE WALL OF THE FRESH FOOD
C             COMPARTMENT TO THE MULLION
C
      SELECT CASE (NMOD)
         CASE (2)
            CALL RDINDT (WALL)
            CALL RDINDT (THMUL)
 
         CASE (3, 8)
            CALL RDINDT (TOPMUL)
            CALL RDINDT (THMUL)
 
      END SELECT
 
      WALL   = WALL/2.54
      THMUL  = THMUL/2.54
      TOPMUL = TOPMUL/2.54
C
C        FREEZER INSULATION THICKNESS
C
C
C        TIFT - THICKNESS OF INSULATION ON TOP OF FREEZER        (INCHES)
C        TIFLS- THICKNESS OF INSULATION ON LEFT SIDE OF FREEZER  (INCHES)
C        TIFRS- THICKNESS OF INSULATION ON RIGHT SIDE OF FREEZER (INCHES)
C        TIFF - THICKNESS OF INSULATION ON FRONT OF FREEZER      (INCHES)
C        TIFB - THICKNESS OF INSULATION ON BACK OF FREEZER       (INCHES)
C
      SELECT CASE (NMOD)
         CASE (2)
            CALL RDINDT (TIFT)
            CALL RDINDT (TIFRS)
            CALL RDINDT (TIFF)
            CALL RDINDT (TIFB)
 
         CASE (3)
            CALL RDINDT (TIFT)
            CALL RDINDT (TIFLS)
            CALL RDINDT (TIFRS)
            CALL RDINDT (TIFF)
            CALL RDINDT (TIFB)
 
         CASE (5)
            CALL RDINDT (TIFT)
            CALL RDINDT (TIFRS)
            CALL RDINDT (TIFF)
            CALL RDINDT (TIFB)
            TIFLS = TIFRS                                                 !!!
 
         CASE (4, 7)
            CALL RDINDT (TIFT)
            CALL RDINDT (TIFLS)
            CALL RDINDT (TIFRS)
            CALL RDINDT (TIFF)
            CALL RDINDT (TIFB)
 
         CASE (8)
            CALL RDINDT (TIFRS)
            CALL RDINDT (TIFF)
            CALL RDINDT (TIFB)
            TIFLS = TIFRS                                                 !!!
 
      END SELECT
 
      TIFT  = TIFT/2.54
      TIFRS = TIFRS/2.54
      TIFLS = TIFLS/2.54
      TIFF  = TIFF/2.54
      TIFB  = TIFB/2.54
C
C        REFRIGERATOR SECTION INSULATION
C
C
C        TIRT-THICKNESS OF INSULATION ON TOP OF FRESH FOOD COMPARTMENT
C             (INCHES)
C        TIRS-THICKNESS OF INSULATION ON SIDES OF FRESH FOOD COMPARTMENT
C             (INCHES)
C        TIRF-THICKNESS OF INSULATION ON FRONT OF FRESH FOOD COMPARTMENT
C             (INCHES)
C        TIRB-THICKNESS OF INSULATION ON BACK OF FRESH FOOD COMPARTMENT
C             (INCHES)
C
      SELECT CASE (NMOD)
         CASE (2)
            CALL RDINDT (TIRT)
            CALL RDINDT (TIRLS)
            CALL RDINDT (TIRF)
            CALL RDINDT (TIRB)
 
         CASE (3)
            CALL RDINDT (TIRLS)
            CALL RDINDT (TIRRS)
            CALL RDINDT (TIRF)
            CALL RDINDT (TIRB)
 
         CASE (8)
            CALL RDINDT (TIRT)
            CALL RDINDT (TIRLS)
            CALL RDINDT (TIRF)
            CALL RDINDT (TIRB)
            TIRRS = TIRLS
 
      END SELECT
 
      TIRT  = TIRT/2.54
      TIRRS = TIRRS/2.54
      TIRLS = TIRLS/2.54
      TIRF = TIRF/2.54
      TIRB = TIRB/2.54
C
C        INSULATION AROUND SIDE OF THE COMPRESSOR COMPARTMENT
C
C        BINSUL-MAXIMUM THICKNESS OF BOTTOM INSULATION FRESH FOOD (INCHES)
C        CINSUL-THICKNESS OF INSULATION OVER COMPRESSOR (INCHES)
C        BINFRZ-MAXIMUM THICKNESS OF BOTTOM INSULATION FREEZER (INCHES)
C        SCIN-THICKNESS OF INSULATION ON SIDE OF COMPRESSOR COMPARTMENT
C             (INCHES)
C        STIN-THICKNESS OF INSULATION ON TOP OF COMPRESSOR COMPARTMENT
C             (INCHES)
C
      SELECT CASE (NMOD)
         CASE (2)
            CALL RDINDT (BINSUL)
            CALL RDINDT (CINSUL)
            CALL RDINDT (BINFRZ)
 
         CASE (3, 4, 7)
            CALL RDINDT (BINSUL)
            CALL RDINDT (CINSUL)
 
         CASE (5)
            CALL RDINDT (BINSUL)
            CALL RDINDT (CINSUL)
            CALL RDINDT (SCIN)
            CALL RDINDT (STIN)
 
         CASE (8)
            CALL RDINDT (BINSUL)
 
      END SELECT
 
      BINSUL = BINSUL/2.54
      CINSUL = CINSUL/2.54
      BINFRZ = BINFRZ/2.54
      SCIN   = SCIN  /2.54
      STIN   = STIN  /2.54
 
C
C        CABINET SECTION INTERNAL VOLUMES
C
C        HXVUZ IS THE FREEZER VOLUME USED FOR HEAT EXCHANGERS (CU. FT.)
C        VOLAZ IS THE ADJUSTED FREEZER VOLUME (CU. FT.)
C        HXVUR IS THE FRESH FOOD VOLUME USED FOR HEAT EXCHANGERS (CU. FT.)
C        VOLAR IS THE ADJUSTED GENERAL REFRIGERATED VOLUME (CU. FT.)
C
      SELECT CASE (NMOD)
         CASE (2, 3, 4, 8)
            CALL RDINDT (HXVUZ)
            CALL RDINDT (VOLAZ)
            CALL RDINDT (HXVUR)
            CALL RDINDT (VOLAR)
 
         CASE DEFAULT
            CALL RDINDT (HXVUZ)
            CALL RDINDT (VOLAZ)
 
      END SELECT
 
      HXVUZ = HXVUZ/28.317
      VOLAZ = VOLAZ/28.317
      HXVUR = HXVUR/28.317
      VOLAR = VOLAR/28.317
C
C        SINGLE-DOOR REFRIGERATOR FREEZER COMPARTMENT
C
      IF (NMOD .EQ. 4) THEN
         CALL RDINDT (FH)
         CALL RDINDT (FW)
         CALL RDINDT (FD)
         FH = FH / 2.54
         FW = FW / 2.54
         FD = FD / 2.54
      END IF
C
C        SET OR READ IN THE TEMPERATURES AND THERMAL VALUES
C
C        TROOM-ROOM TEMPERATURE (DEG F)
C        TFRZ-FREEZER TEMPERATURE (DEG F)
C        TFF-FRESH FOOD COMPARTMENT TEMPERATURE (DEG F)
C        TBTM-UNDERSIDE AIR TEMPERATURE (DEG F)
C
      SELECT CASE (NMOD)
         CASE (2, 3, 4, 8)
            CALL RDINDT (TROOM)
            CALL RDINDT (TFRZ)
            CALL RDINDT (TFF)
            CALL RDINDT (TBTM)
 
         CASE DEFAULT
            CALL RDINDT (TROOM)
            CALL RDINDT (TFRZ)
            CALL RDINDT (TBTM)
            TFF = TFRZ
 
      END SELECT
      TROOM = 1.8*TROOM + 32.0
      TFRZ  = 1.8*TFRZ  + 32.0
      TFF   = 1.8*TFF   + 32.0
      TBTM  = 1.8*TBTM  + 32.0
C
C
C        QRDSET sets the Radiation emissivities, temperatures, and natural
C        convection coefficients for the non-detailed case
C
      CALL QRDSET
C
C        READ THE THERMAL CHARACTERISITICS
C
C        DKIN IS THE THERMAL CONDUCTIVITY OF THE DOOR
C             (BTU/HR-FT-DEG F)
C        HIWP IS THE WALL PANEL GAP FILM COEFFICIENT
C             (BTU/HR-FT2-DEG F)
C        HO-OUTSIDE FILM COEFFICIENT (BTU/HR FT 2 DEG F)
C        HI-INSIDE FILM COEFFICIENT (BTU/HR FT 2 DEG F)
C        HLRG-GASKET HEAT LEAK FOR FRESH FOOD COMPARTMENT FOR
C             UNITS 1,2,3 OR 4 (BTU/HR IN DEG F)
C        HLGZN-FAN ON GASKET HEAT LEAK FOR FREEZER COMPARTMENT FOR
C             UNITS 1,2 OR 3 OR TOTAL UNIT FOR UNITS 5,6 OR 7
C             (BTU/HR IN DEG F)
C        HLGZF-FAN OFF GASKET HEAT LEAK FOR FREEZER COMPARTMENT FOR
C             UNITS 1,2 OR 3 OR TOTAL UNIT FOR UNITS 5,6 OR 7
C             (BTU/HR IN DEG F)
C        WKIN-THERMAL CONDUCTIVITY OF FZ WEDGE INSULATION (BTU/HR FT DEG F)
C        WKINr-THERMAL CONDUCTIVITY OF FF WEDGE INSULATION (BTU/HR FT DEG F)
C        RKIN-THERMAL CONDUCTIVITY OF REFRIGERATOR INSULATION
C             (BTU/HR FT DEG F)
C        TKIN-THERMAL CONDUCTIVITY OF INSULATION IN TOP OF CHEST FREEZER
C             (BTU/HR FT DEG F)
C        CKMUL-THERMAL CONDUCTIVITY OF THE MULLION INSULATION (BTU/HR FT
C             DEG F)
C
      SELECT CASE (NMOD)
         CASE (2, 8)
            CALL RDINDT (RFF)
            CALL RDINDT (RFRZ)
            CALL RDINDT (RWEDGEr)
            CALL RDINDT (RWEDGE)
            CALL RDINDT (RDRFF)
            CALL RDINDT (RDRFZ)
            CALL RDINDT (RMUL)
 
            CALL RDINDT (HLGZF)
            CALL RDINDT (HLRG)
 
            RKIN = 0.0
            RKINFF = CAL_BTU/(RFF*12.0)
            RKINFZ = CAL_BTU/(RFRZ*12.0)
            WKINr  = CAL_BTU/(RWEDGEr*12.0)
            WKIN   = CAL_BTU/(RWEDGE*12.0)
            DKINFF = CAL_BTU/(RDRFF*12.0)
            DKINFZ = CAL_BTU/(RDRFZ*12.0)
            CKMUL  = CAL_BTU/(RMUL*12.0)
 
            HLGZF = 0.048174*HLGZF
            HLRG  = 0.048174*HLRG
 
         CASE (3)
            CALL RDINDT (RFF)
            CALL RDINDT (RFRZ)
            CALL RDINDT (RWEDGEr)
            CALL RDINDT (RWEDGE)
            CALL RDINDT (RDRFF)
            CALL RDINDT (RDRFZ)
            CALL RDINDT (RMUL)
 
            CALL RDINDT (HLFZG)
            CALL RDINDT (HLRG)
 
            RKIN = 0.0
            RKINFF = CAL_BTU/(RFF*12.0)
            RKINFZ = CAL_BTU/(RFRZ*12.0)
            WKINr  = CAL_BTU/(RWEDGEr*12.0)
            WKIN   = CAL_BTU/(RWEDGE*12.0)
            DKINFF = CAL_BTU/(RDRFF*12.0)
            DKINFZ = CAL_BTU/(RDRFZ*12.0)
            CKMUL  = CAL_BTU/(RMUL*12.0)
 
            HLFZG = 0.048174*HLFZG
            HLRG  = 0.048174*HLRG
 
         CASE (5)
            CALL RDINDT (RCAB)
            CALL RDINDT (RTOP)
 
            CALL RDINDT (HLFZG)
 
            RKIN = CAL_BTU/(RCAB*12.0)
            TKIN = CAL_BTU/(RTOP*12.0)
            HIWP =  0.0
 
            HLFZG = 0.048174*HLFZG
 
         CASE (4, 7)
            CALL RDINDT (RCAB)
            CALL RDINDT (RWEDGE)
            CALL RDINDT (RDOOR)
 
            CALL RDINDT (HLFZG)
 
            RKIN = CAL_BTU/(RCAB*12.0)
            WKIN = CAL_BTU/(RWEDGE*12.0)
            DKIN = CAL_BTU/(RDOOR*12.0)
 
            HLFZG = 0.048174*HLFZG
 
      END SELECT
C
C
C           READ DOOR OPENING PARAMETERS
C
      CALL RDINDT (TDRAIR)
      CALL RDINDT (RELHUM)
      IF (NMOD .LE. 3 .OR. NMOD .EQ. 8) THEN
         CALL RDINDT (FFCOPN)
         CALL RDINDT (SECFFC)
      END IF
      CALL RDINDT (FRZOPN)
      CALL RDINDT (SECFRZ)
      HRFFC = (SECFFC/3600.)*FFCOPN
      HRFRZ = (SECFRZ/3600.)*FRZOPN
 
      TDRAIR = 1.8*TDRAIR + 32.0
      TA = TDRAIR
C
C           Read in the Door Gasket Heater Watts and calculate the BTU's
C           of heat into the cabinet, the ANTI-SWEAT heater watss and the
C           BTU's into the cabinet and finally the AUXILIARY energy and
C           the BTU's into the cabinet.
C
      SELECT CASE (NMOD)
         CASE (2, 3, 8)
            CALL RDINDT (FFASHW)
            CALL RDINDT (FZASHW)
            CALL RDINDT (FFAUXW)
            CALL RDINDT (FZAUXW)
            CALL RDINDT (OTHERW)
 
            CALL RDINDT (FFPENA)
            CALL RDINDT (FZPENA)
 
            CALL RDINDT (FFASHQ)
            CALL RDINDT (FZASHQ)
            CALL RDINDT (FFREFQ)
            CALL RDINDT (FZREFQ)
            CALL RDINDT (FFHEAT)
            CALL RDINDT (FZHEAT)
 
            FFASHQ = 3.413*FFASHQ
            FZASHQ = 3.413*FZASHQ
            FFREFQ = 3.413*FFREFQ
            FZREFQ = 3.413*FZREFQ
            FFHEAT = 3.413*FFHEAT
            FZHEAT = 3.413*FZHEAT
            FFPENA = 3.413*FFPENA
            FZPENA = 3.413*FZPENA
 
         CASE DEFAULT
            CALL RDINDT (FZASHW)
            CALL RDINDT (FZAUXW)
            CALL RDINDT (OTHERW)
 
            CALL RDINDT (FZPENA)
 
            CALL RDINDT (FZASHQ)
            CALL RDINDT (FZREFQ)
            CALL RDINDT (FZHEAT)
 
            FZASHQ = 3.413*FZASHQ
            FZREFQ = 3.413*FZREFQ
            FZHEAT = 3.413*FZHEAT
            FZPENA = 3.413*FZPENA
 
      END SELECT
C
C     TEMPORARILY "CORRECT" THE INSULATION THICKNESSES FOR THE LINER
C
      TIFRS = TIFRS + DINS
      TIFLS = TIFLS + DINS
      TIRRS = TIRRS + DINS
      TIRLS = TIRLS + DINS
 
      TIFF = TIFF + DINS
      TIFB = TIFB + DINS
      TIFT = TIFT + DINS
 
      TIRF = TIRF + DINS
      TIRB = TIRB + DINS
      TIRT = TIRT + DINS
 
      BINSUL = BINSUL + DINS
      BINFRZ = BINFRZ + DINS
      CINSUL = CINSUL + DINS
 
      SCIN = SCIN + DINS
      STIN = STIN + DINS
C
C
C     CALCULATE THE VOLUMES
C
      SELECT CASE (NMOD)
         CASE (2)
            VFZ = ((WIDTH-WALL-THMUL-TIFRS)*(DEPTH-TIFF-TIFB)*
     .             (HEIGHT-BINFRZ-TIFT))/1728.0 - HXVUZ
            VFF = (WALL-TIRLS)*(DEPTH-TIRF-TIRB)*
     .             (HEIGHT-BINSUL-TIRT)/1728.0 - HXVUR
 
            SELECT CASE (NCCTYPE)                                         !!!
               CASE (2)                                                   !!!
                  BETA = ATAN(CDDN/CCHGT)                         !!!
                  HTRIAN = CCHGT-BINSUL+binsul/sin(beta)
     .                   - tirb/tan(beta)
                  VFF = VFF-0.5*HTRIAN*HTRIAN*TAN(BETA)*                  !!!
     .                   (WALL-TIRLS)/1728.0                       !!!
 
                  BETA = ATAN(CDDN/CCHGT)                         !!!
                  HTRIAN = CCHGT-BINFRZ+binfrz/sin(beta)
     .                   - tifb/tan(beta)
                  VFZ = VFZ-0.5*HTRIAN*HTRIAN*TAN(BETA)*                  !!!
     .                   (WIDTH-WALL-THMUL-TIFRS)/1728.0                       !!!
 
               CASE (3)                                                   !!!
                  BETA = ATAN((CDDN-CDUP)/CCHGT)                    !!!
                  ALPHA = 3.14159/4.0-BETA/2.0                            !!!
                  UPPERL = CDUP-TIRB+binsul*TAN(ALPHA)                    !!!
                  VFF = VFF-0.5*(2.0*UPPERL+CCHGT*TAN(BETA))*             !!!
     .                   CCHGT*(WALL-TIRLS)/1728.0                 !!!
 
                  BETA = ATAN((CDDN-CDUP)/CCHGT)                    !!!
                  ALPHA = 3.14159/4.0-BETA/2.0                            !!!
                  UPPERL = CDUP-TIFB+binfrz*TAN(ALPHA)                    !!!
                  VFZ = VFZ-0.5*(2.0*UPPERL+CCHGT*TAN(BETA))*             !!!
     .                   CCHGT*(WIDTH-WALL-THMUL-TIFRS)/1728.0                 !!!
            END SELECT                                                    !!!
 
         CASE (3)
            VFZ = (WIDTH-TIFLS-TIFRS)*(DEPTH-TIFF-TIFB)*
     .             (TOPMUL-TIFT)/1728.0 - HXVUZ
            VFF = (DEPTH-TIRF-TIRB)*(WIDTH-TIRLS-TIRRS)*
     .             (HEIGHT-TOPMUL-THMUL-BINSUL)/1728.0 - HXVUR
 
            SELECT CASE (NCCTYPE)                                         !!!
               CASE (2)                                                   !!!
                  BETA = ATAN(CDDN/CCHGT)                         !!!
                  HTRIAN = CCHGT-BINSUL+binsul/sin(beta)
     .                   - tirb/tan(beta)
                  VFF = VFF-0.5*HTRIAN*HTRIAN*TAN(BETA)*                  !!!
     .                   (WIDTH-TIRLS-TIRRS)/1728.0                       !!!
               CASE (3)                                                   !!!
                  BETA = ATAN((CDDN-CDUP)/CCHGT)                    !!!
                  ALPHA = 3.14159/4.0-BETA/2.0                            !!!
                  UPPERL = CDUP-TIRB+binsul*TAN(ALPHA)                    !!!
                  VFF = VFF-0.5*(2.0*UPPERL+CCHGT*TAN(BETA))*             !!!
     .                   CCHGT*(WIDTH-TIRLS-TIRRS)/1728.0                 !!!
            END SELECT                                                    !!!
 
         CASE (4)
            VFZ = VOLAZ
            VFF = (WIDTH-TIFLS-TIFRS)*(HEIGHT-BINSUL-TIFT)*
     .             (DEPTH-TIFF-TIFB)/1728.0 - VOLAZ
 
            SELECT CASE (NCCTYPE)                                         !!!
               CASE (2)                                                   !!!
                  BETA = ATAN(CDDN/CCHGT)                         !!!
                  HTRIAN = CCHGT-BINSUL+binsul/sin(beta)                !!!
     .                   - tifb/tan(beta)
                  VFF = VFF-0.5*HTRIAN*HTRIAN*TAN(BETA)*                  !!!
     .                   (WIDTH-TIFLS-TIFRS)/1728.0                       !!!
               CASE (3)                                                   !!!
                  BETA = ATAN((CDDN-CDUP)/CCHGT)                    !!!
                  ALPHA = 3.14159/4.0-BETA/2.0                            !!!
                  UPPERL = CDUP-TIFB+binsul*TAN(ALPHA)                    !!!
                  VFF = VFF-0.5*(2.0*UPPERL+CCHGT*TAN(BETA))*             !!!
     .                   CCHGT*(WIDTH-TIFLS-TIFRS)/1728.0                 !!!
            END SELECT                                                    !!!
 
         CASE (5)
            VFZ = (((WIDTH-TIFLS-TIFRS)*(DEPTH-TIFF-TIFB)*
     .             (HEIGHT-BINSUL-TIFT))-((DEPTH-TIFF-TIFB)*(CWIDE-
     .             (TIFLS+TIFRS)/2.)*(CHGT-CINSUL)))/1728.0 - HXVUZ
 
         CASE (7)
            VFZ = (WIDTH-TIFLS-TIFRS)*(HEIGHT-BINSUL-TIFT)*
     .             (DEPTH-TIFF-TIFB)/1728.0 - HXVUZ
 
            SELECT CASE (NCCTYPE)                                         !!!
               CASE (2)                                                   !!!
                  BETA = ATAN(CDDN/CCHGT)                         !!!
                  HTRIAN = CCHGT-BINSUL+binsul/sin(beta)                !!!
     .                   - tifb/tan(beta)
                  VFZ = VFZ-0.5*HTRIAN*HTRIAN*TAN(BETA)*                  !!!
     .                   (WIDTH-TIFLS-TIFRS)/1728.0                       !!!
               CASE (3)                                                   !!!
                  BETA = ATAN((CDDN-CDUP)/CCHGT)                    !!!
                  ALPHA = 3.14159/4.0-BETA/2.0                            !!!
                  UPPERL = CDUP-TIFB+binsul*TAN(ALPHA)                    !!!
                  VFZ = VFZ-0.5*(2.0*UPPERL+CCHGT*TAN(BETA))*             !!!
     .                   CCHGT*(WIDTH-TIFLS-TIFRS)/1728.0                 !!!
            END SELECT                                                    !!!
 
         CASE (8)
            VFZ = ((HEIGHT-TOPMUL-THMUL-BINSUL)*(WIDTH-2.0*TIFRS)*
     .             (DEPTH-TIFF-TIFB))/1728.0 - HXVUZ
            VFF = ((TOPMUL-TIRT)*(WIDTH-2.0*TIRLS)*(DEPTH-TIRF-TIRB))
     .             /1728.0 - HXVUR
 
            SELECT CASE (NCCTYPE)                                         !!!
               CASE (2)                                                   !!!
                  BETA = ATAN(CDDN/CCHGT)                         !!!
                  HTRIAN = CCHGT-BINSUL+binsul/sin(beta)
     .                   - tifb/tan(beta)
                  VFZ = VFZ-0.5*HTRIAN*HTRIAN*TAN(BETA)*                  !!!
     .                   (WIDTH-TIFLS-TIFRS)/1728.0                       !!!
               CASE (3)                                                   !!!
                  BETA = ATAN((CDDN-CDUP)/CCHGT)                    !!!
                  ALPHA = 3.14159/4.0-BETA/2.0                            !!!
                  UPPERL = CDUP-TIFB+binsul*TAN(ALPHA)                    !!!
                  VFZ = VFZ-0.5*(2.0*UPPERL+CCHGT*TAN(BETA))*             !!!
     .                   CCHGT*(WIDTH-TIFLS-TIFRS)/1728.0                 !!!
            END SELECT                                                    !!!
 
      END SELECT
C
C     RESET THE INSULATION THICKNESSES NOT INCLUDING THE LINER
C
      TIFRS = TIFRS - DINS
      TIFLS = TIFLS - DINS
      TIRRS = TIRRS - DINS
      TIRLS = TIRLS - DINS
 
      TIFF = TIFF - DINS
      TIFB = TIFB - DINS
      TIFT = TIFT - DINS
 
      TIRF = TIRF - DINS
      TIRB = TIRB - DINS
      TIRT = TIRT - DINS
 
      BINSUL = BINSUL - DINS
      BINFRZ = BINFRZ - DINS
      CINSUL = CINSUL - DINS
 
      SCIN = SCIN - DINS
      STIN = STIN - DINS
 
      RETURN
C
C          FORMAT STATEMENTS
C
  900 FORMAT(''/' RUN AT ',I2,':',I2,':',I2,' ON',I3,1X,A3,I5)
 2000 FORMAT(68A1)
 2002 FORMAT(I10)
      END
 
      SUBROUTINE RDINDT(CBDT)
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*80 A
      CALL INASTK(A)
      READ(A,2001) CBDT
 2001 FORMAT(F10.2)
      RETURN
      END
