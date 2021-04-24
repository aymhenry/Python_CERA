      SUBROUTINE WALEVP(LOC, ANS1, ANS2)
C     ******************************************************************
C     *    PERFORM CALCULATIONS NECESSARY FOR AN IN-WALL EVAPORATOR    *
C     ******************************************************************
C
C
      LOGICAL ITERATING, IN_WALL_FZ, IN_WALL_FF
      COMMON/VALUES/VAL(800),VALMN(800),VALMX(800)
 
      DATA IFZ, IFF                     / 416, 412 /,
     .     IZE, IZL, IFE, IFL           / 229, 362, 209, 372 /,
     .     IFD, IFT, IFS, IFK, IFB      / 376, 020, 377, 378, 020 /,
     .     IZD, IZT, IZS, IZK, IZB, IZM / 366, 369, 367, 368, 020, 370 /
C
C          INITIALIZE
C
      R_IO = 0.30 + 0.001 * (VAL(786)/VAL(787) + VAL(789)/VAL(790))
      T_I = 0.1 * VAL(789) !! change to cm from mm
      T_O = 0.1 * VAL(786)
      T_L = T_O + T_I
      DOOR = VAL(06) + VAL(07)
 
      IRFTYP = NFIX(VAL(1))
      ICYCL  = NFIX(VAL(170))
 
      IFLP = IFL
      IF(IRFTYP .EQ. 5) IFLP = IZL
 
      IF(NFIX(VAL(IFE)) .EQ. 2 .AND. NFIX(VAL(IFLP)) .NE. 0) THEN
         IN_WALL_FF = .TRUE.
      ELSE
         IN_WALL_FF = .FALSE.
      END IF
 
      IF(NFIX(VAL(IZE)) .EQ. 2 .AND. NFIX(VAL(IZL)) .NE. 0
     .                         .AND. ICYCL .NE. 1) THEN
         IN_WALL_FZ = .TRUE.
      ELSE
         IN_WALL_FZ = .FALSE.
      END IF
 
      IF (IRFTYP .EQ. 6) THEN
         IN_WALL_FZ = .FALSE.
         IN_WALL_FF = .FALSE.
         IF (VAL(778) + VAL(779) + VAL(780) .GT. 0.0)
     .      IN_WALL_FF = .TRUE.
      END IF
C
C          SET UP CONSTANTS
C
      SELECT CASE (IRFTYP)                                 !R/F type
         CASE (1, 7)                                       !Top-mount
            WIDTH_FZ = VAL(03) - 2.0*VAL(42) - 2.0 * T_L
            HEIGT_FZ = VAL(31) - VAL(41) - T_L
            DEPTH_FZ = VAL(04) - VAL(43) - DOOR - 2.0 * T_L
 
            AREA_FZ_TOP  =  WIDTH_FZ*DEPTH_FZ
            AREA_FZ_SIDE =  HEIGT_FZ*DEPTH_FZ + HEIGT_FZ*DEPTH_FZ
            AREA_FZ_BACK =  HEIGT_FZ*WIDTH_FZ
            AREA_FZ_BOTM =  0
            AREA_FZ_MUL  =  WIDTH_FZ*DEPTH_FZ
            AREA_FZ_DOOR =  AREA_FZ_BACK
 
            THK_FZ_TOP  = VAL(41)
            THK_FZ_SIDE = VAL(42)
            THK_FZ_BACK = VAL(43)
            THK_FZ_MUL  = VAL(32)
            THK_FZ_BOTM = 1
 
            R_FZ_TOP  = VAL(96)
            R_FZ_SIDE = VAL(97)
            R_FZ_BACK = VAL(98)
            R_FZ_MUL  = VAL(33)
            R_FZ_BOTM = 1
 
            WIDTH_FF = VAL(03) - 2.0*VAL(52) - 2.0 * T_L
            HEIGT_FF = VAL(02) - VAL(31) - VAL(32) - T_L
            DEPTH_FF = VAL(04) - VAL(53) - DOOR - 2.0 * T_L
 
            AREA_FF_TOP  =  0
            AREA_FF_SIDE =  HEIGT_FF*DEPTH_FF + HEIGT_FF*DEPTH_FF
            AREA_FF_BACK =  HEIGT_FF*WIDTH_FF
            AREA_FF_BOTM =  WIDTH_FF*DEPTH_FF
            AREA_FF_DOOR =  AREA_FF_BACK
 
            THK_FF_TOP  = 1
            THK_FF_SIDE = VAL(52)
            THK_FF_BACK = VAL(53)
            THK_FF_BOTM = VAL(54)
 
            R_FF_TOP  = 1
            R_FF_SIDE = VAL(22)
            R_FF_BACK = VAL(23)
            R_FF_BOTM = VAL(24)
 
         CASE (2)                                          !Bottom-mount
            WIDTH_FZ = VAL(03) - 2.0*VAL(42) - 2.0 * T_L
            HEIGT_FZ = VAL(02) - VAL(31) - VAL(32) - VAL(44) - T_L
            DEPTH_FZ = VAL(04) - VAL(43) - DOOR - 2.0 * T_L
 
            AREA_FZ_TOP  =  0
            AREA_FZ_SIDE =  HEIGT_FZ*DEPTH_FZ + HEIGT_FZ*DEPTH_FZ
            AREA_FZ_BACK =  HEIGT_FZ*WIDTH_FZ
            AREA_FZ_BOTM =  WIDTH_FZ*DEPTH_FZ
            AREA_FZ_MUL  =  WIDTH_FZ*DEPTH_FZ
            AREA_FZ_DOOR =  AREA_FZ_BACK
 
            THK_FZ_TOP  = 1
            THK_FZ_SIDE = VAL(42)
            THK_FZ_BACK = VAL(43)
            THK_FZ_BOTM = VAL(44)
            THK_FZ_MUL  = VAL(32)
 
            R_FZ_TOP  = 1
            R_FZ_SIDE = VAL(96)
            R_FZ_BACK = VAL(97)
            R_FZ_BOTM = VAL(98)
            R_FZ_MUL  = VAL(33)
 
            WIDTH_FF = VAL(03) - 2.0 * VAL(52) - 2.0 * T_L
            HEIGT_FF = VAL(31) - VAL(51) - T_L
            DEPTH_FF = VAL(04) - VAL(53) - DOOR - 2.0 * T_L
 
            AREA_FF_TOP  =  WIDTH_FF*DEPTH_FF
            AREA_FF_SIDE =  HEIGT_FF*DEPTH_FF + HEIGT_FF*DEPTH_FF
            AREA_FF_BACK =  HEIGT_FF*WIDTH_FF
            AREA_FF_BOTM =  0
            AREA_FF_DOOR =  AREA_FF_BACK
 
            THK_FF_TOP  = VAL(51)
            THK_FF_SIDE = VAL(52)
            THK_FF_BACK = VAL(53)
            THK_FF_BOTM = 1
 
            R_FF_TOP  = VAL(22)
            R_FF_SIDE = VAL(23)
            R_FF_BACK = VAL(24)
            R_FF_BOTM = 1
 
         CASE (3)                                          !Side-by-side
            WIDTH_FZ = VAL(03) - VAL(42) - VAL(31) - VAL(32) - T_L
            HEIGT_FZ = VAL(02) - VAL(44) - VAL(41) - 2.0 * T_l
            DEPTH_FZ = VAL(04) - VAL(43) - DOOR - 2.0 * T_L
 
            AREA_FZ_TOP  =  WIDTH_FZ*DEPTH_FZ
            AREA_FZ_SIDE =  HEIGT_FZ*DEPTH_FZ
            AREA_FZ_BACK =  HEIGT_FZ*WIDTH_FZ
            AREA_FZ_MUL  =  HEIGT_FZ*DEPTH_FZ
            AREA_FZ_BOTM =  WIDTH_FZ*DEPTH_FZ
            AREA_FZ_DOOR =  AREA_FZ_BACK
 
            THK_FZ_TOP  = VAL(41)
            THK_FZ_SIDE = VAL(42)
            THK_FZ_BACK = VAL(43)
            THK_FZ_BOTM = VAL(44)
            THK_FZ_MUL  = VAL(32)
 
            R_FZ_TOP  = VAL(96)
            R_FZ_SIDE = VAL(97)
            R_FZ_BACK = VAL(98)
            R_FZ_BOTM = VAL(99)
            R_FZ_MUL  = VAL(33)
 
            WIDTH_FF = VAL(32) - VAL(52) - T_L
            HEIGT_FF = VAL(02) - VAL(54) - VAL(51) - 2.0 * T_L
            DEPTH_FF = VAL(04) - VAL(53) - DOOR - 2.0 * T_L
 
            AREA_FF_TOP  =  WIDTH_FF*DEPTH_FF
            AREA_FF_SIDE =  HEIGT_FF*DEPTH_FF
            AREA_FF_BACK =  HEIGT_FF*WIDTH_FF
            AREA_FF_BOTM =  WIDTH_FF*DEPTH_FF
            AREA_FF_DOOR =  AREA_FF_BACK
 
            THK_FF_TOP  = VAL(51)
            THK_FF_SIDE = VAL(52)
            THK_FF_BACK = VAL(53)
            THK_FF_BOTM = VAL(54)
 
            R_FF_TOP  = VAL(22)
            R_FF_SIDE = VAL(23)
            R_FF_BACK = VAL(24)
            R_FF_BOTM = VAL(25)
 
         CASE (4)                                          !Chest freezer
            WIDTH_FF = VAL(3) - 2.0*VAL(12) - 2.0 * T_L
            HEIGT_FF = VAL(2) - VAL(11) - VAL(13) - 2.0 * T_L
            DEPTH_FF = VAL(4) - 2.0*VAL(12) - 2.0 * T_L
 
            W_CM = VAL(35) + VAL(14)
            H_CM = VAL(36) + VAL(15)
            D_CM = DEPTH_FF
 
            AREA_FF_TOP  =  0
            AREA_FF_SIDE =  2.0*(HEIGT_FF*DEPTH_FF + HEIGT_FF*WIDTH_FF)
     .                   -  2.0*W_CM*H_CM
            AREA_FF_BACK =  0
            AREA_FF_BOTM =  WIDTH_FF*DEPTH_FF
            AREA_FF_DOOR =  AREA_FF_BOTM
            AREA_FZ_MUL = 0
 
            THK_FF_TOP  = 1
            THK_FF_SIDE = VAL(12)
            THK_FF_BACK = 1
            THK_FF_BOTM = VAL(13)
 
            R_FF_TOP  = 1
            R_FF_SIDE = VAL(92)
            R_FF_BACK = 1
            R_FF_BOTM = VAL(92)
 
         CASE (5, 6)                                       !Upright freezer
            WIDTH_FF = VAL(3) - 2.0*VAL(42) - 2.0 * T_L    !  or refrigerator
            HEIGT_FF = VAL(2) - VAL(41) - VAL(44) - 2.0 * T_L
            DEPTH_FF = VAL(4) - VAL(43) - DOOR - 2.0 * T_L
 
            AREA_FF_TOP  =  WIDTH_FF*DEPTH_FF
            AREA_FF_SIDE =  HEIGT_FF*DEPTH_FF + HEIGT_FF*DEPTH_FF
            AREA_FF_BACK =  HEIGT_FF*WIDTH_FF
            AREA_FF_BOTM =  WIDTH_FF*DEPTH_FF
            AREA_FF_DOOR =  AREA_FF_BACK
            AREA_FZ_MUL = 0
 
            THK_FF_TOP  = VAL(41)
            THK_FF_SIDE = VAL(42)
            THK_FF_BACK = VAL(43)
            THK_FF_BOTM = VAL(44)
 
            R_FF_TOP  = VAL(92)
            R_FF_SIDE = VAL(92)
            R_FF_BACK = VAL(92)
            R_FF_BOTM = VAL(92)
 
      END SELECT
 
      AREA_FZ_EVAP = 10000.0*VAL(IFZ)
      IF(.NOT. IN_WALL_FZ) AREA_FZ_EVAP = 0
 
      AREA_FF_EVAP = 10000.0*VAL(IFF)
      IF(.NOT. IN_WALL_FF) AREA_FF_EVAP = 0
 
      AREA_COND = 10000.0*VAL(512)
      IF(NFIX(VAL(246)) .EQ. 1 .OR. NFIX(VAL(585)) .EQ.0) AREA_COND = 0
 
      IF(IRFTYP .GE. 4 .and. irftyp .le. 6) AREA_FZ_EVAP = 0
 
      IF (IRFTYP .EQ. 6) AREA_FF_EVAP = 10000.0 * VAL(775)
C
C          BRANCH ON LOC TO PERFORM THE TASK
C
      SELECT CASE (LOC)
         CASE (1)                                          !Wall fractions
            AREA_FF_WALLS = AREA_FF_SIDE + AREA_FF_TOP + AREA_FF_BOTM
     .                    + AREA_FF_BACK + AREA_FF_DOOR+ AREA_FZ_MUL
 
            AREA_FZ_WALLS = AREA_FZ_SIDE + AREA_FZ_TOP + AREA_FZ_BOTM
     .                    + AREA_FZ_BACK + AREA_FZ_DOOR+ AREA_FZ_MUL
 
            IF(.NOT. IN_WALL_FF) THEN
               ANS1 = 0
            ELSE
               ANS1 = AREA_FF_EVAP/AREA_FF_WALLS
               if (irftyp .eq. 6)
     .            ans1 = ans1 * (val(162) + val(163) + val(164))
            END IF
 
            IF(.NOT. IN_WALL_FZ) THEN
               ANS2 = 0
            ELSE
               ANS2 = AREA_FZ_EVAP/AREA_FZ_WALLS
            END IF
 
         CASE (2)                                          !Freezer insulation
 
            AREA_FZ_EVAP_TOP  = AREA_FZ_EVAP*VAL(IZT)
            AREA_FZ_EVAP_SIDE = AREA_FZ_EVAP*VAL(IZS)
            AREA_FZ_EVAP_BACK = AREA_FZ_EVAP*VAL(IZK)
            AREA_FZ_EVAP_BOTM = AREA_FZ_EVAP*VAL(IZB)
            AREA_FZ_EVAP_MUL  = AREA_FZ_EVAP*VAL(IZM)
 
            AREA_FZ_COND_TOP  = AREA_COND*VAL(597)
            AREA_FZ_COND_SIDE = AREA_COND*VAL(594)
            AREA_FZ_COND_BACK = AREA_COND*VAL(595)
 
            IF(AREA_FZ_EVAP_TOP  .GT. AREA_FZ_TOP)
     .         AREA_FZ_EVAP_TOP    =  AREA_FZ_TOP
 
            IF(AREA_FZ_COND_TOP  .GT. AREA_FZ_TOP)
     .         AREA_FZ_COND_TOP    =  AREA_FZ_TOP
 
            IF(AREA_FZ_EVAP_SIDE .GT. AREA_FZ_SIDE)
     .         AREA_FZ_EVAP_SIDE   =  AREA_FZ_SIDE
 
            IF(AREA_FZ_COND_SIDE .GT. AREA_FZ_SIDE)
     .         AREA_FZ_COND_SIDE   =  AREA_FZ_SIDE
 
            IF(AREA_FZ_EVAP_BACK .GT. AREA_FZ_BACK)
     .         AREA_FZ_EVAP_BACK   =  AREA_FZ_BACK
 
            IF(AREA_FZ_COND_BACK .GT. AREA_FZ_BACK)
     .         AREA_FZ_COND_BACK   =  AREA_FZ_BACK
 
            IF(AREA_FZ_EVAP_BOTM .GT. AREA_FZ_BOTM)
     .         AREA_FZ_EVAP_BOTM   =  AREA_FZ_BOTM
 
            IF(AREA_FZ_EVAP_MUL  .GT. AREA_FZ_MUL)
     .         AREA_FZ_EVAP_MUL    =  AREA_FZ_MUL
 
            R_FZ_EVAP_S = R_FZ_SIDE*(THK_FZ_SIDE - VAL(IZD)) + R_IO
            R_FZ_EVAP_K = R_FZ_BACK*(THK_FZ_BACK - VAL(IZD)) + R_IO
            R_FZ_EVAP_T = R_FZ_TOP *(THK_FZ_TOP  - VAL(IZD)) + R_IO
            R_FZ_EVAP_B = R_FZ_BACK*(THK_FZ_BOTM - VAL(IZD)) + R_IO
 
            R_FZ_COND_S = R_FZ_SIDE*(THK_FZ_SIDE - VAL(588)) + R_IO
            R_FZ_COND_K = R_FZ_BACK*(THK_FZ_BACK - VAL(588)) + R_IO
            R_FZ_COND_T = R_FZ_TOP *(THK_FZ_TOP  - VAL(588)) + R_IO
 
            R_FZ_BOTH_S = R_FZ_SIDE*(THK_FZ_SIDE - VAL(588)
     .                                           - VAL(IZD)) + R_IO
            R_FZ_BOTH_K = R_FZ_BACK*(THK_FZ_BACK - VAL(588)
     .                                           - VAL(IZD)) + R_IO
            R_FZ_BOTH_T = R_FZ_TOP *(THK_FZ_TOP  - VAL(588)
     .                                           - VAL(IZD)) + R_IO
 
            R_FZ_EVAP_M = R_FZ_MUL *(THK_FZ_MUL  - VAL(IZD)) + 0.35
 
            R_FZ_S = R_FZ_SIDE*THK_FZ_SIDE + R_IO
            R_FZ_K = R_FZ_BACK*THK_FZ_BACK + R_IO
            R_FZ_T = R_FZ_TOP *THK_FZ_TOP  + R_IO
            R_FZ_B = R_FZ_BOTM*THK_FZ_BOTM + R_IO
 
            R_FZ_M = R_FZ_MUL *THK_FZ_MUL  + 0.35
 
            Q_FZ_WALLS = (AREA_FZ_BOTM - AREA_FZ_EVAP_BOTM)/R_FZ_B
     .                 +  AREA_FZ_EVAP_BOTM/R_FZ_EVAP_B
 
            IF(AREA_FZ_COND_TOP .GT. AREA_FZ_EVAP_TOP) THEN
               Q_FZ_WALLS = Q_FZ_WALLS + (AREA_FZ_TOP
     .                                 - AREA_FZ_COND_TOP )/R_FZ_T
     .                    + (AREA_FZ_COND_TOP
     .                                 - AREA_FZ_EVAP_TOP)/R_FZ_COND_T
     .                    + AREA_FZ_EVAP_TOP/R_FZ_BOTH_T
            ELSE
               Q_FZ_WALLS = Q_FZ_WALLS + (AREA_FZ_TOP
     .                                 - AREA_FZ_EVAP_TOP )/R_FZ_T
     .                    + (AREA_FZ_EVAP_TOP
     .                                 - AREA_FZ_COND_TOP)/R_FZ_EVAP_T
     .                    + AREA_FZ_COND_TOP/R_FZ_BOTH_T
            END IF
 
            IF(AREA_FZ_COND_SIDE .GT. AREA_FZ_EVAP_SIDE) THEN
               Q_FZ_WALLS = Q_FZ_WALLS + (AREA_FZ_SIDE
     .                                 - AREA_FZ_COND_SIDE )/R_FZ_S
     .                    + (AREA_FZ_COND_SIDE
     .                                 - AREA_FZ_EVAP_SIDE)/R_FZ_COND_S
     .                    + AREA_FZ_EVAP_SIDE/R_FZ_BOTH_S
            ELSE
               Q_FZ_WALLS = Q_FZ_WALLS + (AREA_FZ_SIDE
     .                                 - AREA_FZ_EVAP_SIDE )/R_FZ_S
     .                    + (AREA_FZ_EVAP_SIDE
     .                                 - AREA_FZ_COND_SIDE)/R_FZ_EVAP_S
     .                    + AREA_FZ_COND_SIDE/R_FZ_BOTH_S
            END IF
 
            IF(AREA_FZ_COND_BACK .GT. AREA_FZ_EVAP_BACK) THEN
               Q_FZ_WALLS = Q_FZ_WALLS + (AREA_FZ_BACK
     .                                 - AREA_FZ_COND_BACK )/R_FZ_K
     .                    + (AREA_FZ_COND_BACK
     .                                 - AREA_FZ_EVAP_BACK)/R_FZ_COND_K
     .                    + AREA_FZ_EVAP_BACK/R_FZ_BOTH_K
            ELSE
               Q_FZ_WALLS = Q_FZ_WALLS + (AREA_FZ_BACK
     .                                 - AREA_FZ_EVAP_BACK )/R_FZ_K
     .                    + (AREA_FZ_EVAP_BACK
     .                                 - AREA_FZ_COND_BACK)/R_FZ_EVAP_K
     .                    + AREA_FZ_COND_BACK/R_FZ_BOTH_K
            END IF
 
            IF(IRFTYP .LE. 3 .or. irftyp .eq. 7) THEN
               Q_FZ_MUL   = (AREA_FZ_MUL  - AREA_FZ_EVAP_MUL )/R_FZ_M
     .                    +  AREA_FZ_EVAP_MUL /R_FZ_EVAP_M
 
               R_MUL = (AREA_FZ_MUL/Q_FZ_MUL - 0.35)/THK_FZ_MUL
            END IF
 
            R_FZ = R_FZ_SIDE
            ITERATING = .TRUE.
            DO WHILE (ITERATING)
               F_R = Q_FZ_WALLS - AREA_FZ_TOP /(R_FZ*THK_FZ_TOP  + R_IO)
     .                          - AREA_FZ_SIDE/(R_FZ*THK_FZ_SIDE + R_IO)
     .                          - AREA_FZ_BACK/(R_FZ*THK_FZ_BACK + R_IO)
     .                          - AREA_FZ_BOTM/(R_FZ*THK_FZ_BOTM + R_IO)
               DFDR = THK_FZ_TOP *AREA_FZ_TOP
     .                            /(R_FZ*THK_FZ_TOP  + R_IO)**2
     .              + THK_FZ_SIDE*AREA_FZ_SIDE
     .                            /(R_FZ*THK_FZ_SIDE + R_IO)**2
     .              + THK_FZ_BACK*AREA_FZ_BACK
     .                            /(R_FZ*THK_FZ_BACK + R_IO)**2
     .              + THK_FZ_BOTM*AREA_FZ_BOTM
     .                            /(R_FZ*THK_FZ_BOTM + R_IO)**2
 
               IF(ABS(F_R) .LT. 0.001) THEN
                  ITERATING = .FALSE.
                  CYCLE
               ELSE
                  R_FZ = R_FZ - F_R/DFDR
               END IF
            END DO
 
            ANS1 = R_FZ                                    !Freezer resistivity
            ANS2 = R_MUL                                   !Mullion resistivity
 
         CASE (3)                                          !Fresh Food Insul
            IF(IRFTYP .EQ. 4) THEN
               IFK = 020
               IFB = 378
            END IF
 
            IF(IRFTYP .EQ. 5) THEN
               IFD = 366
               IFS = 367
               IFK = 368
               IFT = 369
               IFB = 370
            END IF
 
            IF (IRFTYP .EQ. 6) THEN
               IFD = 166
               IFS = 163
               IFK = 164
               IFT = 162
               IFB = 165
            END IF
 
            AREA_FF_EVAP_TOP  = AREA_FF_EVAP*VAL(IFT)
            AREA_FF_EVAP_SIDE = AREA_FF_EVAP*VAL(IFS)
            AREA_FF_EVAP_BACK = AREA_FF_EVAP*VAL(IFK)
            AREA_FF_EVAP_BOTM = AREA_FF_EVAP*VAL(IFB)
 
            AREA_FF_COND_TOP  = AREA_COND*VAL(596)
            AREA_FF_COND_SIDE = AREA_COND*VAL(592)
            AREA_FF_COND_BACK = AREA_COND*VAL(593)
 
            IF(AREA_FF_EVAP_TOP  .GT. AREA_FF_TOP)
     .         AREA_FF_EVAP_TOP    =  AREA_FF_TOP
 
            IF(AREA_FF_COND_TOP  .GT. AREA_FF_TOP)
     .         AREA_FF_COND_TOP    =  AREA_FF_TOP
 
            IF(AREA_FF_EVAP_SIDE .GT. AREA_FF_SIDE)
     .         AREA_FF_EVAP_SIDE   =  AREA_FF_SIDE
 
            IF(AREA_FF_COND_SIDE .GT. AREA_FF_SIDE)
     .         AREA_FF_COND_SIDE   =  AREA_FF_SIDE
 
            IF(AREA_FF_EVAP_BACK .GT. AREA_FF_BACK)
     .         AREA_FF_EVAP_BACK   =  AREA_FF_BACK
 
            IF(AREA_FF_COND_BACK .GT. AREA_FF_BACK)
     .         AREA_FF_COND_BACK   =  AREA_FF_BACK
 
            IF(AREA_FF_EVAP_BOTM .GT. AREA_FF_BOTM)
     .         AREA_FF_EVAP_BOTM   =  AREA_FF_BOTM
 
            R_FF_EVAP_T = R_FF_TOP *(THK_FF_TOP  - VAL(IFD)) + R_IO
            R_FF_EVAP_S = R_FF_SIDE*(THK_FF_SIDE - VAL(IFD)) + R_IO
            R_FF_EVAP_K = R_FF_BACK*(THK_FF_BACK - VAL(IFD)) + R_IO
            R_FF_EVAP_B = R_FF_BOTM*(THK_FF_BOTM - VAL(IFD)) + R_IO
 
            R_FF_COND_S = R_FF_SIDE*(THK_FF_SIDE - VAL(588)) + R_IO
            R_FF_COND_K = R_FF_BACK*(THK_FF_BACK - VAL(588)) + R_IO
            R_FF_COND_T = R_FF_TOP *(THK_FF_TOP  - VAL(588)) + R_IO
 
            R_FF_BOTH_S = R_FF_SIDE*(THK_FF_SIDE - VAL(588)
     .                                           - VAL(IFD)) + R_IO
            R_FF_BOTH_K = R_FF_BACK*(THK_FF_BACK - VAL(588)
     .                                           - VAL(IFD)) + R_IO
            R_FF_BOTH_T = R_FF_TOP *(THK_FF_TOP  - VAL(588)
     .                                           - VAL(IFD)) + R_IO
 
            R_FF_T = R_FF_TOP *THK_FF_TOP  + R_IO
            R_FF_S = R_FF_SIDE*THK_FF_SIDE + R_IO
            R_FF_K = R_FF_BACK*THK_FF_BACK + R_IO
            R_FF_B = R_FF_BOTM*THK_FF_BOTM + R_IO
 
            Q_FF_WALLS = (AREA_FF_BOTM - AREA_FF_EVAP_BOTM)/R_FF_B
     .                 +  AREA_FF_EVAP_BOTM/R_FF_EVAP_B
 
            IF(AREA_FF_COND_TOP .GT. AREA_FF_EVAP_TOP) THEN
               Q_FF_WALLS = Q_FF_WALLS + (AREA_FF_TOP
     .                                 - AREA_FF_COND_TOP )/R_FF_T
     .                    + (AREA_FF_COND_TOP
     .                                 - AREA_FF_EVAP_TOP)/R_FF_COND_T
     .                    + AREA_FF_EVAP_TOP/R_FF_BOTH_T
            ELSE
               Q_FF_WALLS = Q_FF_WALLS + (AREA_FF_TOP
     .                                 - AREA_FF_EVAP_TOP )/R_FF_T
     .                    + (AREA_FF_EVAP_TOP
     .                                 - AREA_FF_COND_TOP)/R_FF_EVAP_T
     .                    + AREA_FF_COND_TOP/R_FF_BOTH_T
            END IF
 
            IF(AREA_FF_COND_SIDE .GT. AREA_FF_EVAP_SIDE) THEN
               Q_FF_WALLS = Q_FF_WALLS + (AREA_FF_SIDE
     .                                 - AREA_FF_COND_SIDE )/R_FF_S
     .                    + (AREA_FF_COND_SIDE
     .                                 - AREA_FF_EVAP_SIDE)/R_FF_COND_S
     .                    + AREA_FF_EVAP_SIDE/R_FF_BOTH_S
            ELSE
               Q_FF_WALLS = Q_FF_WALLS + (AREA_FF_SIDE
     .                                 - AREA_FF_EVAP_SIDE )/R_FF_S
     .                    + (AREA_FF_EVAP_SIDE
     .                                 - AREA_FF_COND_SIDE)/R_FF_EVAP_S
     .                    + AREA_FF_COND_SIDE/R_FF_BOTH_S
            END IF
 
            IF(AREA_FF_COND_BACK .GT. AREA_FF_EVAP_BACK) THEN
               Q_FF_WALLS = Q_FF_WALLS + (AREA_FF_BACK
     .                                 - AREA_FF_COND_BACK )/R_FF_K
     .                    + (AREA_FF_COND_BACK
     .                                 - AREA_FF_EVAP_BACK)/R_FF_COND_K
     .                    + AREA_FF_EVAP_BACK/R_FF_BOTH_K
            ELSE
               Q_FF_WALLS = Q_FF_WALLS + (AREA_FF_BACK
     .                                 - AREA_FF_EVAP_BACK )/R_FF_K
     .                    + (AREA_FF_EVAP_BACK
     .                                 - AREA_FF_COND_BACK)/R_FF_EVAP_K
     .                    + AREA_FF_COND_BACK/R_FF_BOTH_K
            END IF
 
            R_FF = R_FF_SIDE
            ITERATING = .TRUE.
            DO WHILE (ITERATING)
               F_R = Q_FF_WALLS - AREA_FF_SIDE/(R_FF*THK_FF_SIDE + R_IO)
     .                          - AREA_FF_BACK/(R_FF*THK_FF_BACK + R_IO)
     .                          - AREA_FF_TOP /(R_FF*THK_FF_TOP  + R_IO)
     .                          - AREA_FF_BOTM/(R_FF*THK_FF_BOTM + R_IO)
 
               DFDR = THK_FF_TOP *AREA_FF_TOP
     .                            /(R_FF*THK_FF_TOP  + R_IO)**2
     .              + THK_FF_SIDE*AREA_FF_SIDE
     .                            /(R_FF*THK_FF_SIDE + R_IO)**2
     .              + THK_FF_BACK*AREA_FF_BACK
     .                            /(R_FF*THK_FF_BACK + R_IO)**2
     .              + THK_FF_BOTM*AREA_FF_BOTM
     .                            /(R_FF*THK_FF_BOTM + R_IO)**2
 
               IF(ABS(F_R) .LT. 0.001) THEN
                  ITERATING = .FALSE.
                  CYCLE
               ELSE
                  R_FF = R_FF - F_R/DFDR
               END IF
            END DO
 
            ANS1 = R_FF                                    !Correction
 
            IFK = 378
            IFB = 020
            IFD = 376
            IFT = 020
            IFS = 377
 
      END SELECT
 
      RETURN
      END
