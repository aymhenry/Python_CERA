      LOGICAL CABYN
C
      COMMON / QLA / HEIGHT, WIDTH, DEPTH, BOTTOM, ACOMP,
     +     BCOMP,  TROOM,  TBTM,   TCOMP,  HO,     HI,
     +     BINSUL, DKIN,   TLSIDE, TRSIDE, TTOP,   TBACK,
     +     TFRONT, NMOD,   NCHNG,  ITPCON, QLSIDE, QRSIDE,
     +     BINFRZ, QTOP,   QBACK,  QFRONT, QBOTTM, QBCOMP,
     +     QSV,    QSH
      COMMON /ENR/ ENERGY, EOFF, STEADY, RIST, QOFF, QE, QRE,
     +      QZE, QS, T1, T7, QGZN, QGZF, EALL2
      COMMON / QHEAT / QFRZ, QFFT, QMUL, QMULI, QMULE, QW,
     +     QGR, QGON, QGOF, QTON, QTOF, QWFF, QWFZ
      COMMON / Q12348 / TIRF, TIRRS, TIRLS, TIRB, VOLUZ, VOLAZ,
     +                  VOLUR, VOLAR, TFF
      COMMON / QLN4 / TIFRS, TIFLS, TIFF, TIFB, TFRZ, HLGZN, HLGZF
      COMMON / EFFIC / CISENT, FHOT, EF, NDUMMY
      COMMON / QLAREA / AZOUT, AROUT, AZIN, ARHW, AZHW, AOCOMP
      COMMON / RINIO / RIRHTR, RIRMOT, RIRGSK, RIZHTR, RIZMOT,
     +      RIZGSK, RIZDEF, AWP,    AZHWNP, UA, CA,  UACOND,
     +      DT2T3,  UAINT,  CMIN,   CMAX,   EI, TOL, QETOL, HWP,
     +      DTWP,   ITYPE,  IDUMMY, ITYPIN
      COMMON / INIO / IN, IO, IPRINT, ICYCL
      COMMON /HOTWIN/ TR, TZ, TIRHW, TIZHW, RKIN,  RKW,   TW,
     +      RL, ES,   R,  T2, TA,    QPC,   CHARL, RJMBL, ITYPC,
     +      RKINFF, RKINFZ, DKINFF, DKINFZ
      COMMON / HOTWIO / TRSH, TRS,   TRSO,  TZSH, TZS, TZSO, HAR,
     +                  HAZ,  DQRHW, DQZHW, QCOND
      COMMON / QJAMB / CZINS, CRINS, CZJMON, CZJMOF, CRJAMB
C
C          ERMxxx is the emissivity of the Room (FRT = Front, BCK = Back
C                 LFT = Left, RGT = Right, TOP = Top, BOT = Bottom)
C                 for the top mount freezer (1 & 3) FTT = Front/Top,
C                 FTB = Front/Bottom, BKT = Back/Top, BKB = Back/Bottom,
C                 LTT = Left/Top, LTB = Left/Bottom, RTT = Right/Top,
C                 RTB = Right/Bottom
C          EFRxxx is the emissivity of the refrigerator walls
C          TRADxx or TRAxxx is the Radiative temperature of the Room walls (F)
C          TFSccc is the CALCULATED Freezer outside wall surface
C                 Temperature (F)
C          TRSccc is the CALCULATED Fresh Food outside wall surface
C                 Temperature (F)
C          QRDxxx is the Radiation Heat Transfer from the room walls to the
C                 outside wall of the refrigerator (BTU/hr)
C          QFCxxx is the Forced Convection Heat Transfer from the air to
C                 the outside wall of the refrigerator (BTU/hr)
C          HIxxx  is the Forced Convection Heat Transfer from the air to
C                 the outside wall of the refrigerator (BTU/hr)
C          HOxxx  is the Forced Convection Heat Transfer from the air to
C                 the inside wall of the refrigerator (BTU/hr)
C          TxxSID is the air temperature on the outside of the F (Freezer) or
C                 R (Fresh Food) cabinet on the L (Left) or R (Right) SIDe
C          TxBACK is the air temperature on the outside of the F (Freezer) or
C                 R (Fresh Food) cabinet on the BACK
C          TxFRNT is the air temperature on the outside of the F (Freezer) or
C                 R (Fresh Food) cabinet on the FRoNT
C
      COMMON / RADTON / ERMFRT, ERMBCK, ERMLFT, ERMRGT, ERMTOP,
     +     ERMBOT, EFRFRT, EFRBCK, EFRLFT, EFRRGT, EFRTOP, EFRBOT,
     +     TRADF, TRADL, TRADR, TRADT, TRADBT, TRADBK,
     +     TFSRGT, TFSLFT, TFSTOP, TFSBCK, TFSFRT, TFSBOT,
     +     TRSRGT, TRSLFT, TRSTOP, TRSBCK, TRSFRT, TRSBOT,
     +     QRDTOP, QRDBOT, QRDFRT, QRDBCK, QRDLFT, QRDRGT,
     +     QFCTOP, QFCBOT, QFCFRT, QFCBCK, QFCLFT, QFCRGT,
     +     HITOP,  HIBOT,  HIFRT,  HIBCK,  HILFT,  HIRGT,
     +     HOTOP,  HOBOT,  HOFRT,  HOBCK,  HOLFT,  HORGT
C
      COMMON / RADT13 /
     +     ERMFTT, ERMFTB, ERMBKT, ERMBKB,
     +     ERMLTT, ERMLTB, ERMRTT, ERMRTB,
     +     EFRFTT, EFRFTB, EFRBKT, EFRBKB,
     +     EFRLTT, EFRLTB, EFRRTT, EFRRTB,
     +     TRAFTT, TRAFTB, TRABKT, TRABKB,
     +     TRALTT, TRALTB, TRARTT, TRARTB,
     +     QRDFTT, QRDFTB, QRDBKT, QRDBKB,
     +     QRDLTT, QRDLTB, QRDRTT, QRDRTB,
     +     QFCFTT, QFCFTB, QFCBKT, QFCBKB,
     +     QFCLTT, QFCLTB, QFCRTT, QFCRTB,
     +     HIFTT,  HIFTB,  HIBKT,  HIBKB,
     +     HILTT,  HILTB,  HIRTT,  HIRTB,
     +     HOFTT,  HOFTB,  HOBKT,  HOBKB,
     +     HOLTT,  HOLTB,  HORTT,  HORTB,
     +     HIFMUL, HIRMUL,
     +     TFLSID, TFRSID, TFBACK, TFFRNT,
     +     TRLSID, TRRSID, TRBACK, TRFRNT,
     +     QFLSID, QFRSID, QFBACK, QFFRNT,
     +     QRLSID, QRRSID, QRBACK, QRFRNT,
     +     QLCOMP, QRCOMP, QLTOP,  QRTOP,
     +     QLBTTM, QRBTTM, QBACKL, QBACKR,
     +     QFRNTL, QFRNTR
      COMMON / OPENDR / FFCOPN, FRZOPN, HRFFC, HRFRZ, NEVAP, TDRAIR,
     +     RELHUM, QLFFC, QSFFC, QNVFFC, QNDFFC, QLFRZ, QSFRZ,
     +     QNVFRZ, QNDFRZ, SECFFC, SECFRZ, FZEVAP
      COMMON / CABTST / CABYN
 !    COMMON / HTRAUX / FFGSKW, FFGSKQ , FZGSKW, FZGSKQ, FFASHW,
 !   +     FFASHQ, FZASHW, FZASHQ, FFAUXW, FFAUXQ, FZAUXW, FZAUXQ
      COMMON / HTRAUX /  FFASHW, FZASHW, FFAUXW, FZAUXW, OTHERW,
     +                   FFASHQ, FZASHQ, FFREFQ, FZREFQ
