      SUBROUTINE CYCUAS(UA_FF_ENV, UA_FZ_ENV, UA_ML, UA_FF_CND,
     .                  UA_FZ_CND, UA_FF_HXS, UA_FZ_HXS)
C     ******************************************************************
C     *    CALCULATE THE UAS ASSOCIATED WITH IN-THE-WALL HXS           *
C     ******************************************************************
C
      COMMON/VALUES/VAL(800),VALMN(800),VALMX(800)
C
C          INITIALIZE
C
      R_I = 0.18 + 0.001*VAL(789)/VAL(790)
      R_O = 0.12 + 0.001*VAL(786)/VAL(787)
      IRFTYP = VAL(1)
      ICYCL = VAL(170)
 
      IF(IRFTYP .EQ. 5) VAL(372) = VAL(362)
      IF(IRFTYP .GE. 4 .AND. IRFTYP .LE. 6) VAL(362) = 0
 
      AREA_FZ_EVAP = VAL(416)
      AREA_FF_EVAP = VAL(412)
      AREA_COND    = VAL(512)
      IF(NFIX(VAL(585)) .EQ. 0) AREA_COND = 0
      IF(NFIX(VAL(362)) .EQ. 0) AREA_FZ_EVAP = 0
      IF(NFIX(VAL(372)) .EQ. 0) AREA_FF_EVAP = 0
      IF(ICYCL .EQ. 1) AREA_FZ_EVAP = 0
      IF(ICYCL .EQ. 3) AREA_COND = 0
 
      IF(NFIX(VAL(209)) .EQ. 1) AREA_FF_EVAP = 0
      IF(NFIX(VAL(229)) .EQ. 1) AREA_FZ_EVAP = 0
      IF(NFIX(VAL(246)) .EQ. 1) AREA_COND = 0
 
      DTE_FZ = VAL(366)
      DTE_FF = VAL(376)
      DTC    = VAL(588)
 
      IF (IRFTYP .EQ. 6) THEN
         AREA_FF_EVAP = VAL(775)
         DTE_FZ = 0
         DTE_FF = 0
      END IF
 
      UA_FF_ENV = 0
      UA_FZ_ENV = 0
      UA_ML = 0
      UA_FF_CND = 0
      UA_FZ_CND = 0
      UA_FF_HXS = 0
      UA_FZ_HXS = 0
C
C          BEGIN WITH MULLION
C
      IF(IRFTYP .LE. 3 .OR. IRFTYP .EQ. 7) THEN
         R_ML  = VAL(33)*(VAL(32) - VAL(366)) + 1.0
         UA_ML = VAL(370)*AREA_FZ_EVAP/R_ML
      ELSE
         UA_ML = 0
      END IF
C
C          TOP WALL CONTRIBUTION TO FRESH FOOD CABINET
C
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                          !Top-mount
            !Nothing. FF is in bottom section
 
         CASE (2)                                          !Bottom-mount
            R_FF_CND = VAL(22)*(VAL(51)-DTC)    + R_I
            AREA_COND_TOP = AREA_COND*VAL(596)
            UA_FF_CND = UA_FF_CND + AREA_COND_TOP/R_FF_CND
 
         CASE (3)                                          !Side-by-side
            !Nothing.  No top to FF evap and no in-wall cond
 
         CASE (4)                                          !Chest Freezer
            !Nothing.  No top on chest freezer (only door)
 
         CASE (5, 6)                                       !Upright freezer
            R_FF_EVP = VAL(92)*(VAL(41)-DTE_FZ) + R_O
            R_FF_CND = VAL(92)*(VAL(41)-DTC)    + R_I
            R_FF_HXS = VAL(92)*(VAL(41)-DTE_FZ-DTC)
 
            AREA_EVAP_TOP = AREA_FF_EVAP*VAL(369)
            IF (IRFTYP .EQ. 6) AREA_EVAP_TOP = AREA_FF_EVAP*VAL(162)
            AREA_COND_TOP = AREA_COND*VAL(596)
            DELTA_AREA = AREA_EVAP_TOP - AREA_COND_TOP
 
            IF(AREA_EVAP_TOP .GT. AREA_COND_TOP) THEN
               UA_FF_ENV = UA_FF_ENV + DELTA_AREA/R_FF_EVP
               UA_FF_HXS = UA_FF_HXS + AREA_COND_TOP/R_FF_HXS
            ELSE
               UA_FF_CND = UA_FF_CND - DELTA_AREA/R_FF_CND
               UA_FF_HXS = UA_FF_HXS + AREA_EVAP_TOP/R_FF_HXS
            END IF
 
      END SELECT
C
C          SIDE WALL CONTRIBUTION TO FRESH FOOD CABINET
C
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top-mount
            R_FF_EVP = VAL(22)*(VAL(52)-DTE_FF) + R_O
            R_FF_CND = VAL(22)*(VAL(52)-DTC)    + R_I
            R_FF_HXS = VAL(22)*(VAL(52)-DTE_FF-DTC)
 
            AREA_EVAP_SIDE = AREA_FF_EVAP*VAL(377)
            AREA_COND_SIDE = AREA_COND*VAL(592)
            DELTA_AREA = AREA_EVAP_SIDE - AREA_COND_SIDE
 
            IF(AREA_EVAP_SIDE .GT. AREA_COND_SIDE) THEN
               UA_FF_ENV = UA_FF_ENV + DELTA_AREA/R_FF_EVP
               UA_FF_HXS = UA_FF_HXS + AREA_COND_SIDE/R_FF_HXS
            ELSE
               UA_FF_CND = UA_FF_CND - DELTA_AREA/R_FF_CND
               UA_FF_HXS = UA_FF_HXS + AREA_EVAP_SIDE/R_FF_HXS
            END IF
 
         CASE (2)                                          !Bottom-mount
            R_FF_EVP = VAL(23)*(VAL(52)-DTE_FF) + R_O
            R_FF_CND = VAL(23)*(VAL(52)-DTC)    + R_I
            R_FF_HXS = VAL(23)*(VAL(52)-DTE_FF-DTC)
 
            AREA_EVAP_SIDE = AREA_FF_EVAP*VAL(377)
            AREA_COND_SIDE = AREA_COND*VAL(592)
            DELTA_AREA = AREA_EVAP_SIDE - AREA_COND_SIDE
 
            IF(AREA_EVAP_SIDE .GT. AREA_COND_SIDE) THEN
               UA_FF_ENV = UA_FF_ENV + DELTA_AREA/R_FF_EVP
               UA_FF_HXS = UA_FF_HXS + AREA_COND_SIDE/R_FF_HXS
            ELSE
               UA_FF_CND = UA_FF_CND - DELTA_AREA/R_FF_CND
               UA_FF_HXS = UA_FF_HXS + AREA_EVAP_SIDE/R_FF_HXS
            END IF
 
         CASE (3)                                          !Side-by-side
            R_FF_EVP = VAL(23)*(VAL(52)-DTE_FF) + R_O
            AREA_EVAP_SIDE = AREA_FF_EVAP*VAL(377)
            UA_FF_ENV = UA_FF_ENV + AREA_EVAP_SIDE/R_FF_EVP
 
         CASE (4)                                          !Chest Freezer
            R_FF_EVP = VAL(92)*(VAL(12)-DTE_FF) + R_O
            R_FF_CND = VAL(92)*(VAL(12)-DTC)    + R_I
            R_FF_HXS = VAL(92)*(VAL(12)-DTE_FF-DTC)
 
            AREA_EVAP_SIDE = AREA_FF_EVAP*VAL(377)
            AREA_COND_SIDE = AREA_COND*VAL(592)
            DELTA_AREA = AREA_EVAP_SIDE - AREA_COND_SIDE
 
            IF(AREA_EVAP_SIDE .GT. AREA_COND_SIDE) THEN
               UA_FF_ENV = UA_FF_ENV + DELTA_AREA/R_FF_EVP
               UA_FF_HXS = UA_FF_HXS + AREA_COND_SIDE/R_FF_HXS
            ELSE
               UA_FF_CND = UA_FF_CND - DELTA_AREA/R_FF_CND
               UA_FF_HXS = UA_FF_HXS + AREA_EVAP_SIDE/R_FF_HXS
            END IF
 
         CASE (5, 6)                                       !Upright freezer
            R_FF_EVP = VAL(92)*(VAL(42)-DTE_FZ) + R_O
            R_FF_CND = VAL(92)*(VAL(42)-DTC)    + R_I
            R_FF_HXS = VAL(92)*(VAL(42)-DTE_FZ-DTC)
 
            AREA_EVAP_SIDE = AREA_FF_EVAP*VAL(367)
            IF (IRFTYP .EQ. 6) AREA_EVAP_SIDE = AREA_FF_EVAP*VAL(163)
            AREA_COND_SIDE = AREA_COND*VAL(592)
            DELTA_AREA = AREA_EVAP_SIDE - AREA_COND_SIDE
 
            IF(AREA_EVAP_SIDE .GT. AREA_COND_SIDE) THEN
               UA_FF_ENV = UA_FF_ENV + DELTA_AREA/R_FF_EVP
               UA_FF_HXS = UA_FF_HXS + AREA_COND_SIDE/R_FF_HXS
            ELSE
               UA_FF_CND = UA_FF_CND - DELTA_AREA/R_FF_CND
               UA_FF_HXS = UA_FF_HXS + AREA_EVAP_SIDE/R_FF_HXS
            END IF
 
      END SELECT
C
C          BACK WALL CONTRIBUTION TO FRESH FOOD CABINET
C
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top-mount
            R_FF_EVP = VAL(23)*(VAL(53)-DTE_FF) + R_O
            R_FF_CND = VAL(23)*(VAL(53)-DTC)    + R_I
            R_FF_HXS = VAL(23)*(VAL(53)-DTE_FF-DTC)
 
            AREA_EVAP_BACK = AREA_FF_EVAP*VAL(378)
            AREA_COND_BACK = AREA_COND*VAL(593)
            DELTA_AREA = AREA_EVAP_BACK - AREA_COND_BACK
 
            IF(AREA_EVAP_BACK .GT. AREA_COND_BACK) THEN
               UA_FF_ENV = UA_FF_ENV + DELTA_AREA/R_FF_EVP
               UA_FF_HXS = UA_FF_HXS + AREA_COND_BACK/R_FF_HXS
            ELSE
               UA_FF_CND = UA_FF_CND - DELTA_AREA/R_FF_CND
               UA_FF_HXS = UA_FF_HXS + AREA_EVAP_BACK/R_FF_HXS
            END IF
 
         CASE (2)                                          !Bottom-mount
            R_FF_EVP = VAL(24)*(VAL(53)-DTE_FF) + R_O
            R_FF_CND = VAL(24)*(VAL(53)-DTC)    + R_I
            R_FF_HXS = VAL(24)*(VAL(53)-DTE_FF-DTC)
 
            AREA_EVAP_BACK = AREA_FF_EVAP*VAL(378)
            AREA_COND_BACK = AREA_COND*VAL(593)
            DELTA_AREA = AREA_EVAP_BACK - AREA_COND_BACK
 
            IF(AREA_EVAP_BACK .GT. AREA_COND_BACK) THEN
               UA_FF_ENV = UA_FF_ENV + DELTA_AREA/R_FF_EVP
               UA_FF_HXS = UA_FF_HXS + AREA_COND_BACK/R_FF_HXS
            ELSE
               UA_FF_CND = UA_FF_CND - DELTA_AREA/R_FF_CND
               UA_FF_HXS = UA_FF_HXS + AREA_EVAP_BACK/R_FF_HXS
            END IF
 
         CASE (3)                                          !Side-by-side
            R_FF_EVP = VAL(24)*(VAL(53)-DTE_FF) + R_O
            AREA_EVAP_BACK = AREA_FF_EVAP*VAL(378)
            UA_FF_ENV = UA_FF_ENV + AREA_EVAP_BACK/R_FF_EVP
 
         CASE (4)                                          !Chest Freezer
            !Nothing. No back wall.
 
         CASE (5, 6)                                       !Upright freezer
            R_FF_EVP = VAL(92)*(VAL(43)-DTE_FZ) + R_O
            R_FF_CND = VAL(92)*(VAL(43)-DTC)    + R_I
            R_FF_HXS = VAL(92)*(VAL(43)-DTE_FZ-DTC)
 
            AREA_EVAP_BACK = AREA_FF_EVAP*VAL(368)
            IF (IRFTYP .EQ. 6) AREA_EVAP_BACK = AREA_FF_EVAP*VAL(164)
            AREA_COND_BACK = AREA_COND*VAL(593)
            DELTA_AREA = AREA_EVAP_BACK - AREA_COND_BACK
 
            IF(AREA_EVAP_BACK .GT. AREA_COND_BACK) THEN
               UA_FF_ENV = UA_FF_ENV + DELTA_AREA/R_FF_EVP
               UA_FF_HXS = UA_FF_HXS + AREA_COND_BACK/R_FF_HXS
            ELSE
               UA_FF_CND = UA_FF_CND - DELTA_AREA/R_FF_CND
               UA_FF_HXS = UA_FF_HXS + AREA_EVAP_BACK/R_FF_HXS
            END IF
 
      END SELECT
C
C          BOTTOM WALL CONTRIBUTION TO FRESH FOOD CABINET
C
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top-mount
            !Nothing
 
         CASE (2)                                          !Bottom-mount
            !Nothing
 
         CASE (3)                                          !Side-by-side
            !Nothing
 
         CASE (4)                                          !Chest Freezer
            R_FF_EVP = VAL(92)*(VAL(13)-DTE_FF) + R_O
            AREA_EVAP_BOTTOM = AREA_FF_EVAP*VAL(378)
            UA_FF_ENV = UA_FF_ENV + AREA_EVAP_BOTTOM/R_FF_EVP
 
         CASE (5)                                          !Upright freezer
            R_FF_EVP = VAL(92)*(VAL(44)-DTE_FZ) + R_O
            AREA_EVAP_BOTTOM = AREA_FF_EVAP*VAL(370)
            UA_FF_ENV = UA_FF_ENV + AREA_EVAP_BOTTOM/R_FF_EVP
 
      END SELECT
C
C          TOP WALL CONTRIBUTION TO FREEZER SECTION
C
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top-mount
            R_FZ_EVP = VAL(96)*(VAL(41)-DTE_FZ) + R_O
            R_FZ_CND = VAL(96)*(VAL(41)-DTC)    + R_I
            R_FZ_HXS = VAL(96)*(VAL(41)-DTE_FZ-DTC)
 
            AREA_EVAP_TOP = AREA_FZ_EVAP*VAL(369)
            AREA_COND_TOP = AREA_COND*VAL(597)
            DELTA_AREA = AREA_EVAP_TOP - AREA_COND_TOP
 
            IF(AREA_EVAP_TOP .GT. AREA_COND_TOP) THEN
               UA_FZ_ENV = UA_FZ_ENV + DELTA_AREA/R_FZ_EVP
               UA_FZ_HXS = UA_FZ_HXS + AREA_COND_TOP/R_FZ_HXS
            ELSE
               UA_FZ_CND = UA_FZ_CND - DELTA_AREA/R_FZ_CND
               UA_FZ_HXS = UA_FZ_HXS + AREA_EVAP_TOP/R_FZ_HXS
            END IF
 
         CASE (2)                                          !Bottom-mount
            !No top
 
         CASE (3)                                          !Side-by-side
            R_FZ_EVP = VAL(96)*(VAL(41)-DTE_FZ) + R_O
            AREA_EVAP_TOP = AREA_FZ_EVAP*VAL(369)
            UA_FZ_ENV = UA_FZ_ENV + AREA_EVAP_TOP/R_FZ_EVP
 
      END SELECT
C
C          SIDE WALL CONTRIBUTION TO FREEZER SECTION
C
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top-mount
            R_FZ_EVP = VAL(97)*(VAL(42)-DTE_FZ) + R_O
            R_FZ_CND = VAL(97)*(VAL(42)-DTC)    + R_I
            R_FZ_HXS = VAL(97)*(VAL(42)-DTE_FZ-DTC)
 
            AREA_EVAP_SIDE = AREA_FZ_EVAP*VAL(367)
            AREA_COND_SIDE = AREA_COND*VAL(594)
            DELTA_AREA = AREA_EVAP_SIDE - AREA_COND_SIDE
 
            IF(AREA_EVAP_SIDE .GT. AREA_COND_SIDE) THEN
               UA_FZ_ENV = UA_FZ_ENV + DELTA_AREA/R_FZ_EVP
               UA_FZ_HXS = UA_FZ_HXS + AREA_COND_SIDE/R_FZ_HXS
            ELSE
               UA_FZ_CND = UA_FZ_CND - DELTA_AREA/R_FZ_CND
               UA_FZ_HXS = UA_FZ_HXS + AREA_EVAP_SIDE/R_FZ_HXS
            END IF
 
         CASE (2)                                          !Bottom-mount
            R_FZ_EVP = VAL(96)*(VAL(42)-DTE_FZ) + R_O
            R_FZ_CND = VAL(96)*(VAL(42)-DTC)    + R_I
            R_FZ_HXS = VAL(96)*(VAL(42)-DTE_FZ-DTC)
 
            AREA_EVAP_SIDE = AREA_FZ_EVAP*VAL(367)
            AREA_COND_SIDE = AREA_COND*VAL(594)
            DELTA_AREA = AREA_EVAP_SIDE - AREA_COND_SIDE
 
            IF(AREA_EVAP_SIDE .GT. AREA_COND_SIDE) THEN
               UA_FZ_ENV = UA_FZ_ENV + DELTA_AREA/R_FZ_EVP
               UA_FZ_HXS = UA_FZ_HXS + AREA_COND_SIDE/R_FZ_HXS
            ELSE
               UA_FZ_CND = UA_FZ_CND - DELTA_AREA/R_FZ_CND
               UA_FZ_HXS = UA_FZ_HXS + AREA_EVAP_SIDE/R_FZ_HXS
            END IF
 
         CASE (3)                                          !Side-by-side
            R_FZ_EVP = VAL(97)*(VAL(42)-DTE_FZ) + R_O
            AREA_EVAP_SIDE = AREA_FZ_EVAP*VAL(367)
            UA_FZ_ENV = UA_FZ_ENV + AREA_EVAP_SIDE/R_FZ_EVP
 
      END SELECT
C
C          BACK WALL CONTRIBUTION TO FREEZER SECTION
C
      SELECT CASE (IRFTYP)
         CASE (1, 7)                                       !Top-mount
            R_FZ_EVP = VAL(98)*(VAL(43)-DTE_FZ) + R_O
            R_FZ_CND = VAL(98)*(VAL(43)-DTC)    + R_I
            R_FZ_HXS = VAL(98)*(VAL(43)-DTE_FZ-DTC)
 
            AREA_EVAP_BACK = AREA_FZ_EVAP*VAL(368)
            AREA_COND_BACK = AREA_COND*VAL(595)
            DELTA_AREA = AREA_EVAP_BACK - AREA_COND_BACK
 
            IF(AREA_EVAP_BACK .GT. AREA_COND_BACK) THEN
               UA_FZ_ENV = UA_FZ_ENV + DELTA_AREA/R_FZ_EVP
               UA_FZ_HXS = UA_FZ_HXS + AREA_COND_BACK/R_FZ_HXS
            ELSE
               UA_FZ_CND = UA_FZ_CND - DELTA_AREA/R_FZ_CND
               UA_FZ_HXS = UA_FZ_HXS + AREA_EVAP_BACK/R_FZ_HXS
            END IF
 
         CASE (2)                                          !Bottom-mount
            R_FZ_EVP = VAL(97)*(VAL(43)-DTE_FZ) + R_O
            R_FZ_CND = VAL(97)*(VAL(43)-DTC)    + R_I
            R_FZ_HXS = VAL(97)*(VAL(43)-DTE_FZ-DTC)
 
            AREA_EVAP_BACK = AREA_FZ_EVAP*VAL(368)
            AREA_COND_BACK = AREA_COND*VAL(595)
            DELTA_AREA = AREA_EVAP_BACK - AREA_COND_BACK
 
            IF(AREA_EVAP_BACK .GT. AREA_COND_BACK) THEN
               UA_FZ_ENV = UA_FZ_ENV + DELTA_AREA/R_FZ_EVP
               UA_FZ_HXS = UA_FZ_HXS + AREA_COND_BACK/R_FZ_HXS
            ELSE
               UA_FZ_CND = UA_FZ_CND - DELTA_AREA/R_FZ_CND
               UA_FZ_HXS = UA_FZ_HXS + AREA_EVAP_BACK/R_FZ_HXS
            END IF
 
         CASE (3)                                          !Side-by-side
            R_FZ_EVP = VAL(98)*(VAL(43)-DTE_FZ) + R_O
            AREA_EVAP_BACK = AREA_FZ_EVAP*VAL(368)
            UA_FZ_ENV = UA_FZ_ENV + AREA_EVAP_BACK/R_FZ_EVP
 
      END SELECT
 
      RETURN
      END
