      SUBROUTINE QINIT
C     ******************************************************************
C     *    CONVERT DATA VALUES INTO UNITS OF FEET                      *
C     ******************************************************************
 
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / QLTF /   HIWP
      COMMON / QLTWO /  DCOMP,WALL
      COMMON / QLONE /  TMF,TMS,TMB
      COMMON / QLFIVE / CWIDE,CHGT,SCIN,STIN,TKIN
      COMMON / QLFOUR / FH,FW,FD
      COMMON / QLMISC / TOPMUL,THMUL,TIFT,TIRT,CINSUL,HLRG,CKMUL,HLFZG,
     .                  DGSKT,CDUP,CDDN,CCHGT,NCCTYPE                     !!!
      COMMON / QLN5 /   WKIN,WEDGE,FLANGE,FLGB,WEDGER,FLANGER,wkinr       !!!
      COMMON / HOTWAL / WTHW,DTHW,RTO,TSO,HRHW,WRHW,TRO,RO,
     .                  HRSHW,WRSHW,TSRO,SRO,HLSHW,WLSHW,TLSO,SLO,HFHW,
     .                  WFHW,FTO,FSO
      COMMON / LINER / DOL, DIL, COL, CIL
C
C           CONVERT ALL MEASUREMENTS TO FEET
C
      HEIGHT= HEIGHT/12.0
      WIDTH = WIDTH/12.0
      DEPTH = DEPTH/12.0
      BOTTOM= BOTTOM/12.0
 
      DIL = DIL / 12.0
      DOL = DOL / 12.0
 
      ACOMP = ACOMP/12.0
      BCOMP = BCOMP/12.0
      DCOMP = DCOMP/12.0
      AOCOMP= 3.14159*ACOMP*BCOMP/4.0
 
      BINSUL= BINSUL/12.0
      BINFRZ= BINFRZ/12.0
      CINSUL= CINSUL/12.0
 
      TIRLS = TIRLS/12.0
      TIRRS = TIRRS/12.0
      TIFLS = TIFLS/12.0
      TIFRS = TIFRS/12.0
 
      DGSKT = DGSKT/12.0                                                  !!!
 
      IF(NMOD .NE. 5) THEN
         WEDGE = WEDGE/12.0
         FLANGE= FLANGE/12.0
         FLGB  = FLGB/12.0
         WEDGER= WEDGER/12.0                                              !!!
         FLANGER=FLANGER/12.0                                             !!!
         FZEVAP= FZEVAP/12.0
      END IF
 
      TIFT  = TIFT/12.0
      TIFB  = TIFB/12.0
      TIFS  = TIFS/12.0
      TIFF  = TIFF/12.0
 
      SELECT CASE (NMOD)
         CASE (1, 3, 8)
            TOPMUL = TOPMUL/12.0
            THMUL  = THMUL/12.0
 
         CASE (2)
            THMUL  = THMUL/12.0
            WALL = WALL/12.0
 
      END SELECT
 
      IF(NMOD .LE.4 .OR. NMOD .EQ.8) THEN
         TIRF  = TIRF/12.0
         TIRB  = TIRB/12.0
         TIRS  = TIRS/12.0
 
         IF(NMOD .EQ. 2 .OR. NMOD .EQ. 4 .OR. NMOD .EQ. 8) THEN
            TIRT = TIRT/12.0
         END IF
      END IF
 
      SELECT CASE (NMOD)                                                  !!!
         CASE (5)
            SCIN  = SCIN/12.0
            CWIDE = CWIDE/12.0
            CHGT  = CHGT/12.0
            STIN  = STIN/12.0
 
         CASE (2, 3, 4, 7, 8)                                             !!!
            CDUP  = CDUP/12.0                                             !!!
            CDDN  = CDDN/12.0                                             !!!
            CCHGT = CCHGT/12.0                                            !!!
      END SELECT                                                          !!!
 
      IF(NMOD .EQ. 1) THEN
         TMS   = TMS/12.0
         TMB   = TMB/12.0
         TMF   = TMF/12.0
      END IF
 
      IF (NMOD .EQ. 4) THEN
         FW = FW / 12.0
         FD = FD / 12.0
         FH = FH / 12.0
      END IF
 
      RETURN
      END
