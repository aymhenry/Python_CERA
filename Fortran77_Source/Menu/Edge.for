      SUBROUTINE EDGE(WID_IN, LEN_IN, H0_IN, HI_IN, THICK,
     .                FRACTION_OUTER, FRACTION_INNER, R_FOAM, DEL_IN,
     .                K_ENCLO, R_PANEL, R_NET)
C     ******************************************************************
C     *   CALCULATE EDGE HEAT TRANSFER THROUGH INSULATION WITH         *
C     *   A VACUUM ENCLOSURE.                                          *
C     ******************************************************************
C
      REAL K_ENCLO, LENGTH, LEN_IN
 
      H0 = 1.0/(1.0/H0_IN + 12.0*FRACTION_OUTER*THICK*R_FOAM)
      HI = 1.0/(1.0/HI_IN + 12.0*FRACTION_INNER*THICK*R_FOAM)
      S  = (1.0 - FRACTION_OUTER - FRACTION_INNER)*THICK
      HP = 1.0/(12.0*S*R_PANEL)
 
      DELTA  = DEL_IN
      WIDTH  = WID_IN
      LENGTH = LEN_IN
 
      T0 = 1.0
      TI = 0.0
C
C          DO THE EDGE LENGTH AND WIDTH CALCULATIONS SEPARATELY
C          AND THEN APPLY APPROXIMATE CORRECTION
C
      CALL FINDRP(H0, HI, HP, K_ENCLO, DELTA, T0, TI, S, WIDTH,  Q_EDGE)
      Q_PERIM_WIDTH  = LENGTH*Q_EDGE
      CORR_WIDTH     = 1.0 - HI/H0*Q_EDGE/WIDTH
 
      CALL FINDRP(H0, HI, HP, K_ENCLO, DELTA, T0, TI, S, LENGTH, Q_EDGE)
      Q_PERIM_LENGTH = WIDTH*Q_EDGE
      CORR_LENGTH    = 1.0 - HI/H0*Q_EDGE/LENGTH
 
      Q_PERIM = Q_PERIM_LENGTH*CORR_WIDTH + Q_PERIM_WIDTH*CORR_LENGTH
 
      Q_PRIME = Q_PERIM/(WIDTH*LENGTH)
      TA_AVE = (H0 - HI*Q_PRIME/(HI + HP))/(H0 + HI*HP/(HI + HP))
 
      Q_TOTAL = H0*(1.0 - TA_AVE)
 
      R_NET = (1.0/Q_TOTAL - 1.0/H0 - 1.0/HI)/(12.0*S)
 
      RETURN
      END
C
      SUBROUTINE CALINS(VAL, I_PANEL, R_NET)
C     ******************************************************************
C     *   CALCULATE THE NET RESISTIVITY OF AN AN INSULATION PANEL      *
C     ******************************************************************
C
C         I_PANEL CODE:           1 = Side panel, 2 = Back panel,
C                                 3 = Top panel,  4 = Bottom panel,
C                                 5 = Door
C
C                                    Width                 Length
C          Side   Panel           Front to back         Top to bottom
C          Back   Panel           Side to side          Top to bottom
C          Top    Panel           Side to side          Front to back
C          Bottom Panel           Side to side          Front to back
C          Door   Panel           Side to side          Top to bottom
C
      REAL K_ENCLO, K_FOAM, K_PANEL, LENGTH
      DIMENSION VAL(1)
      DATA H0_Film, HI_Film /1.47, 1.0/
C
C          ACCOUNT FOR CABINET LINERS
C
      H0 = 1.0 / (1.0 / H0_Film + 0.0056781 * VAL(786) / VAL(787))
      HI = 1.0 / (1.0 / HI_Film + 0.0056781 * VAL(789) / VAL(790))
C
C          SET UP DATA VALUES (ENGLISH UNITS)
C
      SELECT CASE (I_PANEL)
         CASE (1)                                          !Side panel
            ITYPE = 3
            WIDTH  = VAL(140)/(2.54*12.0)
            LENGTH = VAL(141)/(2.54*12.0)
            E1 = VAL(142)/(2.54*12.0)
            E2 = VAL(143)/(2.54*12.0)
            E3 = VAL(144)/(2.54*12.0)
            E4 = VAL(145)/(2.54*12.0)
 
         CASE (3, 4)                                       !Top and bottom
            ITYPE = 3
            WIDTH  = VAL(141)/(2.54*12.0)
            LENGTH = VAL(140)/(2.54*12.0)
            E2 = VAL(142)/(2.54*12.0)
            E3 = VAL(143)/(2.54*12.0)
            E4 = VAL(144)/(2.54*12.0)
            E1 = VAL(145)/(2.54*12.0)
 
         CASE (2)                                          !Back panel
            ITYPE = 4
            WIDTH  = VAL(140)/(2.54*12.0)
            LENGTH = VAL(141)/(2.54*12.0)
            E1 = VAL(142)/(2.54*12.0)
            E2 = VAL(143)/(2.54*12.0)
            E3 = VAL(144)/(2.54*12.0)
            E4 = VAL(145)/(2.54*12.0)
 
         CASE DEFAULT                                      !Door
            ITYPE = 5
            WIDTH  = VAL(140)/(2.54*12.0)
            LENGTH = VAL(141)/(2.54*12.0)
            E1 = VAL(142)/(2.54*12.0)
            E2 = VAL(143)/(2.54*12.0)
            E3 = VAL(144)/(2.54*12.0)
            E4 = VAL(145)/(2.54*12.0)
 
      END SELECT
 
      THICK = VAL(146)/(2.54*12.0)
      FRACTION_OUTER = VAL(147)
      FRACTION_INNER = VAL(148)
      R_FOAM = 14.4133*VAL(149)
      R_PANEL = 14.4133*VAL(150)
      DELTA = VAL(151)/(25.4*12.0)
      K_ENCLO = VAL(152)/1.7296
C
C          SET UP THE CALL VARIABLES
C
      T1 = FRACTION_INNER*THICK
      T3 = FRACTION_OUTER*THICK
      T2 = THICK - T1 - T3
 
      K_PANEL = 1.0/(12.0*R_PANEL)
      K_FOAM  = 1.0/(12.0*R_FOAM)
 
      CALL GENERIC(ITYPE, E1, E2, E3, E4, T1, T2, T3, H0, HI, K_FOAM,
     .             K_PANEL, K_ENCLO, DELTA, LENGTH, WIDTH, R_NET)
     .
C
C          CONVERT R_NET BACK TO METRIC UNITS
C
      R_NET = R_NET/14.4133
 
      RETURN
      END
C
      SUBROUTINE GENERIC(ITYPE, E1, E2, E3, E4, T1, T2, T3, H0, HI,
     .                   K_FOAM, K_PANEL, K_LAM, D_LAM, LENGTH,
     .                   WIDTH, R_NET)
C     ******************************************************************
C     *    GENERIC REPRESENTATION OF VACUUM PANEL: 5 TYPES             *
C     ******************************************************************
C
C     INPUT PARAMETERS
C        ITYPE:              CONFIGURATION  1: 1 BEVELED EDGE PANEL,
C                                           2: 2 BEVELED EDGE PANEL,
C                                           3: 3 BEVELED EDGE PANEL,
C                                           4: 4 BEVELED EDGE PANEL,
C                                           5: DOOR PANEL
C
C        E1, E2, E3, E4      FOAM WIDTHS ALONG EDGES (FT)
C                            NOTE: MEASURED FROM OUTSIDE DIMENSIONS
C        T1                  INNER FOAM THICKNESS (FT)
C        T2                  VACUUM PANEL THICKNESS (FT)
C        T3                  OUTER FOAM THICKNESS (FT)
C        H0, HI              OUTER AND INNER FILM COEFFICIENTS
C        K_FOAM              FOAM THERMAL CONDUCTIVITY
C        K_PANEL             VACUUM PANEL THERMAL CONDUCTIVITY
C        K_LAM               LAMINATE THERMAL CONDUCTIVITY
C        D_LAM               THICKNESS OF LAMINATE (FT)
C        LENGTH, WIDTH       OVERALL OUTSIDE WALL DIMENSIONS (FT)
C     OUTPUT PARAMENTER
C        R_NET               NET RESISTIVITY OF SECTION, BASED ON INSIDE
C                            DIMENSIONS
 
      REAL K_LAM, K_FOAM, K_PANEL, LENGTH, L_PANEL, LEN_INNER
C
C          CONVERT CALL VARIABLES
C
      THICK = T1 + T2 + T3
      FRACTION_OUTER = T3/THICK
      FRACTION_INNER = T1/THICK
      R_FOAM  = 1.0/(12.0*K_FOAM)
      R_PANEL = 1.0/(12.0*K_PANEL)
 
      W_PANEL = (WIDTH  - E2 - E4)/2.0
      L_PANEL = (LENGTH - E1 - E3)/2.0
 
C
C          CENTRAL SECTION - OVER THE VACUUM PANEL.  FIRST PERFORM
C          THE ANALYSIS FOR THE WHOLE PANEL BASED ON OUTSIDE DIMENSIONS
C
      CALL EDGE(W_PANEL, L_PANEL, H0, HI, THICK, FRACTION_OUTER,
     .          FRACTION_INNER, R_FOAM, D_LAM, K_LAM, R_PANEL,
     .          R_NET)
 
      AREA_PANEL = 4.0*W_PANEL*L_PANEL
      R_CEN = 12.0*R_NET*T2 + 12.0*R_FOAM*(T1 + T3)
C
C          ACCOUNT FOR BEVELED EDGES BY AREA AVERAGING
C
      SELECT CASE (ITYPE)
         CASE (1)                                          !Top edge bevel
            LEN_INNER = LENGTH - THICK
            WID_INNER = WIDTH
 
         CASE (2)                                          !Right side bevel
            LEN_INNER = LENGTH - THICK
            WID_INNER = WIDTH  - THICK
 
         CASE (3)                                          !Bottom side bevel
            LEN_INNER = LENGTH - 2.0*THICK
            WID_INNER = WIDTH  - THICK
 
         CASE (4)                                          !Left side bevel
            LEN_INNER = LENGTH - 2.0*THICK
            WID_INNER = WIDTH  - 2.0*THICK
 
         CASE (5)                                          !Left side bevel
            LEN_INNER = LENGTH
            WID_INNER = WIDTH
 
      END SELECT
 
      AREA_INNER = LEN_INNER*WID_INNER
      AREA_OUTER = LENGTH*WIDTH
      AREA_EFFEC = AREA_INNER
      IF(AREA_OUTER .GT. AREA_INNER) THEN
         AREA_EFFEC = (AREA_OUTER - AREA_INNER)
     .                / ALOG(AREA_OUTER/AREA_INNER)
      END IF
     .
C
C          EDGES - FOAM ONLY.
C
      R_EDG = 12.0*THICK*R_FOAM
C
C          HEAT BALANCE METHOD FOR NET RESISTIVITY
C
      UA_CENT = AREA_PANEL/R_CEN
      UA_EDGE = (AREA_OUTER - AREA_PANEL)/R_EDG*AREA_EFFEC/AREA_INNER
      Q_TOTAL = UA_CENT + UA_EDGE
      Q_AREA = (Q_TOTAL/AREA_OUTER)
      R_NET  = 1.0 / Q_AREA / (T1 + T2 + T3) / 12.0
     .
C
C          CORRECT BACKWARDS FOR THE BEVELS
C
      IF(ITYPE .EQ. 5) RETURN
      R_NET = R_NET*AREA_EFFEC/AREA_INNER
 
      RETURN
      END
C
      SUBROUTINE FINDRP(H0, HI, HP, K_ENCLO, DELTA, T0, TI, S, LENGTH,
     .                  Q_EDGE)
C     ******************************************************************
C     *   ONE-DIMENSIONAL EDGE LOSS CALCULATION                        *
C     ******************************************************************
C
      LOGICAL NOT_DONE
      REAL K_ENCLO, LENGTH
C
      ARG0 = LENGTH*SQRT((H0+HP)/(K_ENCLO*DELTA))
      ARGI = LENGTH*SQRT((HI+HP)/(K_ENCLO*DELTA))
C
C          INITIAL GUESS
C
      TA_AVE = 0.9
      TB_AVE = 0.1
      TA = 0.8
      TB = 0.2
C
C          START ITERATIVE SOLUTION
C
      NOT_DONE = .TRUE.
 
      DO WHILE (NOT_DONE)
         TA_STAR = (H0*T0 + HP*TB_AVE)/(H0 + HP)
         TB_STAR = (HI*TI + HP*TA_AVE)/(HI + HP)
 
         DUMMT = ARG0*TANH(ARG0)/(ARGI*TANH(ARGI))*TA_STAR
     .         + (K_ENCLO*DELTA/S)*TB_STAR/(K_ENCLO*DELTA/S
     .         + (K_ENCLO*DELTA/LENGTH)*ARGI*TANH(ARGI))
         DUMMB = ARG0*TANH(ARG0)/(ARGI*TANH(ARGI))
     .         + (K_ENCLO*DELTA/S)/(K_ENCLO*DELTA/S
     .         + (K_ENCLO*DELTA/LENGTH)*ARGI*TANH(ARGI))
         TANEW = DUMMT/DUMMB
         ERROR = ABS(TANEW - TA)
         TA = TANEW
         TB = (ARG0*TANH(ARG0))/(ARGI*TANH(ARGI))*(TA_STAR - TA)
     .      + TB_STAR
 
         TA_AVE = TA_STAR + (TANH(ARG0)/ARG0)*(TA - TA_STAR)
 
         TB_AVE = TB_STAR + (TANH(ARGI)/ARGI)*(TB - TB_STAR)
 
         IF(ERROR .LE. 0.00001) NOT_DONE = .FALSE.
 
      END DO
 
      Q_EDGE = (K_ENCLO*DELTA/S)*(TA - TB)
 
      Q_TOTAL = H0*(1.0 - TA_AVE)
 
      R_NET = (1.0/Q_TOTAL - 1.0/H0 - 1.0/HI)/(12.0*S)
 
      RETURN
      END
