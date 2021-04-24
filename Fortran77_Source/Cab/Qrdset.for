      SUBROUTINE QRDSET
C     ******************************************************************
C     *    SET UP THE CONVECTION COEFFICIENTS AROUND THE CABINET       *
C     *                                                                *
C     *    MODIFIED TO COMBINE RADIATION AND CONVECTION                *
C     ******************************************************************
 
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / QLTF / HIWP
      COMMON / LINER / DOL, DIL, COL, CIL
C
C          DEFINE THE TEMPERATURES SEEN BY EACH SIDE
C
      TFRONT = TROOM
      TLSIDE = TROOM
      TRSIDE = TROOM
      TTOP   = TROOM
      TBACK  = TROOM
      TFFRNT = TROOM
      TFBACK = TROOM
      TFLSID = TROOM
      TFRSID = TROOM
      TTOP   = TROOM
      TRAFTT = TROOM
      TRABKT = TROOM
      TRALTT = TROOM
      TRARTT = TROOM
      TRADT  = TROOM
      TRADBT = TROOM
      TRFRNT = TROOM
      TRBACK = TROOM
      TRLSID = TROOM
      TRRSID = TROOM
      TRAFTB = TROOM
      TRABKB = TROOM
      TRALTB = TROOM
      TRARTB = TROOM
      TRADBT = TROOM
C
C          DEFINE THE CONVECTION COEFFICIENTS
C
      HIREF = 1.0
      HOREF = 1.47
 
  !!  Correct for the inner and outer liners
      HIREF = 1.0 / (1.0 / HIREF + DIL / 12.0 / CIL)
      HOREF = 1.0 / (1.0 / HOREF + DOL / 12.0 / COL)
 
      HI = HIREF
      HO = HOREF
      HIFTT = HIREF
 
 !!   HIFMUL = HIREF
      HIFMUL = 1.0
 
      HIBKT = HIREF
      HILTT = HIREF
      HIRTT = HIREF
 
  !!  HIRMUL = HIREF
      HIRMUL = 1.0
 
      HITOP = HIREF
      HIFTB = HIREF
      HIBKB = HIREF
      HILTB = HIREF
      HIRTB = HIREF
      HIBOT = HIREF
      HIFRT = HIREF
      HIBCK = HIREF
      HILFT = HIREF
      HIRGT = HIREF
      HOFTT = HOREF
      HOBKT = HOREF
      HOLTT = HOREF
      HORTT = HOREF
      HOTOP = HOREF
      HOFTB = HOREF
      HOBKB = HOREF
      HOLTB = HOREF
      HORTB = HOREF
      HOBOT = HOREF
      HOFRT = HOREF
      HOBCK = HOREF
      HOLFT = HOREF
      HORGT = HOREF
      HIWP = HIREF
 
      RETURN
      END
