$DEBUG
      SUBROUTINE QL8
C     ******************************************************************
C     *    CALCULATE CABINET HEAT LEAK FOR CONFIGURATION 8             *
C     *                                                                *
C     *    CONFIGURATION 8: Bottom Mount R/F                           *
C     ******************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / QLMISC / TOPMUL,THMUL,TIFT,TIRT,CINSUL,HLRG,CKMUL,HLFZG,
     .                  DGSKT,CDUP,CDDN,CCHGT,NCCTYPE                     !!!
      COMMON / QLN5 /   WKIN,WEDGE,FLANGE,FLGB,WEDGER,FLANGER,wkinr       !!!
      COMMON / OPEND /  QDFFCS, QDFFFS, QDFZCS, QDFZFS, QDFFCL, QDFFFL,
     .                  QDFZCL, QDFZFL
C
      TIFLS = TIFRS
      TIRRS = TIRLS
      TIFS  = TIFRS
      TIRS  = TIRLS
C
C          Calculate overall INTERNAL Height, Width and Depth of Freezer
C          compartment (FRZ) and the Fresh Food Compartment (FFC)
C
      HFFC = TOPMUL - TIRT
      WFFC = WIDTH - TIRLS - TIRRS
      DFFC = DEPTH - WEDGER - TIRF - TIRB - DGSKT
 
      HFRZ = HEIGHT - TOPMUL - THMUL - BINSUL
      WFRZ = WIDTH - TIFLS - TIFRS
      DFRZ = DEPTH - WEDGE - TIFF - TIFB -DGSKT                     !!?
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            !Nothing
 
         CASE (2)                                                         !!!
            BETA = ATAN(CDDN/CCHGT)                                       !!!
            ALPHA = 3.14159/4.0-BETA/2.0                                  !!!
            H1 = HEIGHT-TOPMUL-THMUL-BINSUL                               !!!
  !         H2 = H1-CCHGT+BINSUL-TIFB*TAN(BETA/2.0)                       !!!
  !         D1 = CDDN/SIN(BETA)                                           !!!
  !  .                  -BINSUL*(TAN(BETA/2.0)+TAN(ALPHA))                !!!
  !         D2 = DEPTH-CDDN-TIRF-WEDGE-BINSUL*TAN(ALPHA)-DGSKT            !!!
 
            HTRIAN = CCHGT-BINSUL+binsul/sin(beta)
     .             - tifb/tan(beta)
 
            DC = DEPTH-WEDGE-TIFF-TIFB-DGSKT                              !!?
 
            h2 = h1 - htrian
            d1 = htrian/cos(beta)
            d2 = dc - htrian*tan(beta)
 
            WFRZ = WIDTH-TIFLS-TIFRS                                      !!!
            HFRZ = H1                                                     !!!
                                                                          !!!
         CASE (3)                                                         !!!
            BETA = ATAN((CDDN-CDUP)/CCHGT)                                !!!
            ALPHA = 3.14159/4.0-BETA/2.0                                  !!!
            H1 = HEIGHT-TOPMUL-THMUL-BINSUL                               !!!
            H2 = H1-CCHGT                                                 !!!
            D1 = CDUP-TIFB                                                !!!
            IF ((CDDN-CDUP).EQ.0.0) THEN                                  !!!
                D2 = CCHGT-BINSUL                                         !!!
            ELSE                                                          !!!
                D2 = (CDDN-CDUP)/SIN(BETA)-BINSUL*TAN(ALPHA)              !!!
            END IF                                                        !!!
            D3 = DEPTH-CDDN-TIFF-WEDGE-BINSUL*TAN(ALPHA)-DGSKT            !!?
            DC = DEPTH-WEDGE-TIFF-TIFB-DGSKT                              !!?
            WFRZ = WIDTH-TIFLS-TIFRS                                      !!!
            HFRZ = H1                                                     !!!
                                                                          !!!
      END SELECT                                                          !!!
                                                                          !!!
      FALPHA = 4.0*ALPHA/3.14159                                          !!!
      FBETA  = 2.0*BETA/3.14159                                           !!!
 
C
C           Internal Fresh Food (R) Areas
C
      AIRSID = HFFC*DFFC
      AIRTOP = WFFC*DFFC
      AIRBCK = HFFC*WFFC
C
C           External Fresh Food (R) Areas
C
      AORSID = (TOPMUL + THMUL/2.0)*(DEPTH-WEDGER-DGSKT)                   !!!
      AORTOP = WIDTH*(DEPTH-WEDGER-DGSKT)                                  !!!
      AORBCK = (TOPMUL + THMUL/2.0)*WIDTH
C
C           Internal Freezer (F) Areas
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AIFSID = HFRZ*DFRZ
            AIFBOT = WFRZ*DFRZ
            AIFBCK = HFRZ*WFRZ
            AIFFNT = AIFBCK
                                                                          !!!
         CASE (2)                                                         !!!
            AIFSID = DC*H2+(DC+D2)*(H1-H2)/2.0                            !!!
            AIFBCK = WFRZ*H2                                              !!!
            AIFFNT = WFRZ*H1                                              !!!
            AIFBTM1 = WFRZ*D1                                             !!!
            AIFBTM2 = WFRZ*D2                                             !!!
                                                                          !!!
         CASE (3)                                                         !!!
            AIFSID = DC*H2+CCHGT*(D3+D3+CCHGT*TAN(BETA))/2.0              !!!
            AIFBCK = WFRZ*H2                                              !!!
            AIFFNT = WFRZ*H1                                              !!!
            AIFBTM1 = WFRZ*(D1+BINSUL*TAN(ALPHA))                         !!!
            AIFBTM2 = WFRZ*(D2+BINSUL*TAN(ALPHA))                         !!!
            AIFBTM3 = WFRZ*D3                                             !!!
                                                                          !!!
      END SELECT                                                          !!!
C
C           External Freezer (F) Areas
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AOFSID = (HEIGHT-TOPMUL-THMUL/2.0)*(DEPTH-WEDGE-DGSKT)       !!!
            AOFBOT = WIDTH*(DEPTH-WEDGE-DGSKT)                           !!!
            AOFBCK = (HEIGHT-TOPMUL-THMUL/2.0)*WIDTH
            AOFFNT = AOFBCK
                                                                          !!!
         CASE (2)                                                         !!!
            AOFSID = (HEIGHT-TOPMUL-THMUL/2.0)*(DEPTH-WEDGE-DGSKT)       !!!
     .                      -CDDN*CCHGT/2.0                               !!!
            AOFFNT = (HEIGHT-TOPMUL-THMUL/2.0)*WIDTH                      !!!
            AOFBCK = AOFFNT-CCHGT*WIDTH                                   !!!
            AOFBTM1 = WIDTH*CCHGT/COS(BETA)                               !!!
            AOFBTM2 = WIDTH*(DEPTH-CDDN-WEDGE-DGSKT)                     !!!
                                                                          !!!
         CASE (3)                                                         !!!
            AOFSID = (HEIGHT-TOPMUL-THMUL/2.0)*(DEPTH-WEDGE-DGSKT)       !!!
     .                      -(CDDN+CDUP)*CCHGT/2.0                        !!!
            AOFFNT = (HEIGHT-TOPMUL-THMUL/2.0)*WIDTH                      !!!
            AOFBCK = AOFFNT-CCHGT*WIDTH                                   !!!
            AOFBTM1 = WIDTH*CDUP                                          !!!
            AOFBTM2 = WIDTH*CCHGT/COS(BETA)                               !!!
            AOFBTM3 = WIDTH*(DEPTH-CDDN-WEDGE-DGSKT)                     !!!
                                                                          !!!
      END SELECT                                                          !!!
 
C     Calculate the average insulation conductivity. DKINFZ
C     is the freezer door insulation. DKINFF is the fresh food
C     door insulation conductivity. RKINFF is the insulation
C     conductivity for the sides, back, top and bottom of the
C     fresh food compartment, RKINFZ is for the Freezer.
C
            TAVGL = 0.25*(DKINFF+DKINFZ+RKINFF+RKINFZ)
C
C     Calculate the internal and external surface areas of the freezer
C
C     NOTE: The internal and external surface areas were separatly
C           calculated for each component (excluding the bottom surface)
C           of the FRZ.
C
C     Corners and edges boardering the mullion were accounted for in the
C     mullion heat leak calculation. SEE FIGURE 1
C
C
C     CALCULATE THE AREA AND OVERALL HEAT TRANSFER OF THE MULLION
C
C        AMUL is the mullion area (Ft^2)
C        UMUL is the mullion overall heat transfer (Btu/Hr-Ft^2-Deg F)
C
            AMUL = WFRZ*DFRZ
            UMUL = 1./(1./HIFMUL+1./HIRMUL+THMUL/CKMUL)
            QMUL = UMUL*AMUL*(TFF-TFRZ)
C
C     CALCULATE R AND HEAT LEAK FOR FREEZER
C
C     R is actually the conductance (1/Resistance) of the various
C     components that make up the freezer compartment. Heat leak through
C     the corners and edges is accounted for in the R calculations.
C
C     The shape factor for an edge is 0.54*length of the edge.
C     The shape factor for a corner is 0.15*wall thickness.
C     (Holman p.54)
C
C     NOTE: Since each corner is made up of three different insulation
C           thicknesses, an average thickness was used in the calculation.
C
C
C     The corner shape factor is divided by three since each
C     corner is shared by three walls of different thicknesses.
C     (The corner shape factor is actually divided by 9 since the
C     three corner thicknesses are averaged (3x3=9))
C
C     The edge shape factor is divided by two since each edge is shared
C     by two walls.
C
C     Note: R calculation split into seperate components
C
C
C           Calculate the heat leak out of the LEFT Fresh Food Side
C
      RR = AIRSID/TIRLS + 0.54*(DFFC + 2.0*HFFC)/2.0
     .                  + 0.15*(2.0*TIRLS+TIRB+2.0*TIRT+TIRF)/9.
      R1 = 1.0/(RR*RKINFF) + 1.0/(HI*AIRSID)
      R2 = 1.0/(HO*AORSID)
 
      CALL RADTRN(R1, R2, TFF, TROOM, TRALTT, ERMLTT, EFRLTT, AORSID,
     .            QRLSID, TFSLFT, QRDLTT, QFCLTT)
C
C           Calculate the heat leak out of the LEFT Freezer Side
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            RF = AIFSID/TIFLS + 0.54*(DFRZ + 2.0*HFRZ)/2.0
     .                  + 0.15*(2.0*TIFLS+TIFB+2.0*BINSUL+TIFF)/9.0       !!?
                                                                          !!!
         CASE (2)                                                         !!!
            RF = AIFSID/TIFLS+0.54*(H1+H2+D1+D2)/2.0 + 0.15               !!!
     .                  * ((2.0*BINSUL+TIFLS)*FALPHA                      !!!
     .                  + (TIFB+TIFLS+BINSUL)*FBETA                       !!!
     .                  + (TIFF+TIFLS+BINSUL))/9.0                        !!?
                                                                          !!!
         CASE (3)                                                         !!!
            RF = AIFSID/TIFLS+0.54*(H1+H2+D1+D2+D3)/2.0 + 0.15            !!!
     .                  * (2.0*(2.0*BINSUL+TIFLS)*FALPHA                  !!!
     .                  + (2.0*(TIFLS+BINSUL)+TIFF+TIFB))/9.0             !!?
      END SELECT                                                          !!!
 
      R1 = 1.0/(RF*RKINFZ) + 1.0/(HI*AIFSID)
      R2 = 1.0/(HO*AOFSID)
 
      CALL RADTRN(R1, R2, TFRZ, TROOM, TRALTB, ERMLTB, EFRLTB, AOFSID,
     .            QFLSID, TRSLFT, QRDLTB, QFCLTB)
C
C           Calculate the heat leak out of the RIGHT Fresh Food Side
C
      RR = AIRSID/TIRRS + 0.54*(DFFC + 2.0*HFFC)/2.0
     .                  + 0.15*(2.0*TIRRS+TIRB+2.0*TIRT+TIRF)/9.
      R1 = 1.0/(RR*RKINFF) + 1.0/(HI*AIRSID)
      R2 = 1.0/(HO*AORSID)
 
      CALL RADTRN(R1, R2, TFF, TROOM, TRARTT, ERMRTT, EFRRTT, AORSID,
     .            QRRSID, TFSRGT, QRDRTT, QFCRTT)
C
C           Calculate the heat leak out of the RIGHT Freezer Side
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            RF = AIFSID/TIFRS + 0.54*(DFRZ + 2.0*HFRZ)/2.0
     .                  + 0.15*(2.0*TIFRS+TIFB+2.0*BINSUL+TIFF)/9.0       !!?
                                                                          !!!
         CASE (2)                                                         !!!
            RF = AIFSID/TIFRS + 0.54*(H1+H2+D1+D2)/2.0 + 0.15             !!!
     .                  * ((2.0*BINSUL+TIFRS)*FALPHA                      !!!
     .                  + (TIFB+TIFRS+BINSUL)*FBETA                       !!!
     .                  + (TIFF+TIFRS+BINSUL))/9.0                        !!?
                                                                          !!!
         CASE (3)                                                         !!!
            RF = AIFSID/TIFRS + 0.54*(H1+H2+D1+D2+D3)/2.0 + 0.15          !!!
     .                  * (2.0*(2.0*BINSUL+TIFRS)*FALPHA                  !!!
     .                  + (2.0*(TIFRS+BINSUL)+TIFF+TIFB))/9.0             !!?
 
      END SELECT                                                          !!!
 
      R1 = 1.0/(RF*RKINFZ) + 1.0/(HI*AIFSID)
      R2 = 1.0/(HO*AOFSID)
      CALL RADTRN(R1, R2, TFRZ, TROOM, TRARTB, ERMRTB, EFRRTB, AOFSID,
     .            QFRSID, TRSRGT, QRDRTB, QFCRTB)
C
C           Calculate the heat leak out of the top (Fresh Food)
C           The top has two Depth and two Width length edges
C
      R  = AIRTOP/TIRT + 0.54*(2.0*DFFC + 2.0*WFFC)/2.0
     .     + 0.15*(2.*TIRLS+2.*TIRRS+2.*TIRB+4.*TIRT+2.*TIRF)/9.0
      R1 = 1.0/(R*RKINFF) + 1.0/(HI*AIRTOP)
      R2 = 1.0/(HO*AORTOP)
 
      CALL RADTRN(R1, R2, TFF, TROOM, TRADT, ERMTOP, EFRTOP, AORTOP,
     .            QTOP, TFSTOP, QRDTOP, QFCTOP)
 
C
C           Calculate the heat leak out of the BACK Fresh Food
C           The back has two Height and one Width length edges
C
      R  = AIRBCK/TIRB + 0.54*(2.0*HFFC + WFFC)/2.0
     .                 + 0.15*(TIRLS+TIRRS+2.0*TIRB+2.0*TIRT)/9.0
      R1 = 1.0/(R*RKINFF) + 1.0/(HI*AIRBCK)
      R2 = 1.0/(HO*AORBCK)
 
      CALL RADTRN(R1, R2, TFF, TROOM, TRABKT, ERMBKT, EFRBKT, AORBCK,
     .            QRBACK, TFSBCK, QRDBKT, QFCBKT)
C
C           Calculate the heat leak out of the BACK Freezer
C           The back has two Height and one Width length edges
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            R  = AIFBCK/TIFB + 0.54*(2.0*HFRZ + WFRZ)/2.0
     .                  + 0.15*(TIFLS+TIFRS+2.0*TIFB+2.0*BINSUL)/9.0
                                                                          !!!
         CASE (2)                                                         !!!
            R  = AIFBCK/TIFB+0.54*(2.0*H2+WFRZ*FBETA)/2.0+0.15            !!!
     .                  *(TIFLS+TIFRS+2.0*TIFB+2.0*BINSUL)*FBETA/9.0      !!!
                                                                          !!!
         CASE (3)                                                         !!!
            R  = AIFBCK/TIFB + 0.54*(2.0*H2+WFRZ)/2.0                     !!!
     .                  + 0.15*(TIFLS+TIFRS+2.0*TIFB+2.0*BINSUL)/9.0      !!!
                                                                          !!!
       END SELECT                                                         !!!
 
      R1 = 1.0/(R*RKINFZ) + 1.0/(HI*AIFBCK)
      R2 = 1.0/(HO*AOFBCK)
 
      CALL RADTRN(R1, R2, TFRZ, TROOM, TRABKB, ERMBKB, EFRBKB, AOFBCK,
     .            QFBACK, TRSBCK, QRDBKB, QFCBKB)
C
C           Calculate the heat leak out of the FRONT Fresh Food
C           The front has two Height and one Width length edges
C
      R  = AIRBCK/TIRF + 0.54*(2.0*HFFC + WFFC)/2.0
     .                 + 0.15*(TIRLS+TIRRS+2.0*TIRF+2.0*TIRT)/9.0
      R1 = 1.0/(R*DKINFF) + 1.0/(HI*AIRBCK)
      R2 = 1.0/(HO*AORBCK)
 
      CALL RADTRN(R1, R2, TFF, TROOM, TRAFTT, ERMFTT, EFRFTT, AORBCK,
     .            QRFRNT, TFSFRT, QRDFTT, QFCFTT)
C
C           Calculate the heat leak out of the FRONT Freezer
C           The front has two Height and one Width length edges
C
      R  = AIFFNT/TIFF + 0.54*(2.0*HFRZ + WFRZ)/2.0
     .                  + 0.15*(TIFLS+TIFRS+2.0*TIFF+2.0*BINSUL)/9.0
 
      R1 = 1.0/(R*DKINFZ) + 1.0/(HI*AIFFNT)
      R2 = 1.0/(HO*AOFFNT)
 
      CALL RADTRN(R1, R2, TFRZ, TROOM, TRAFTB, ERMFTB, EFRFTB, AOFFNT,
     .            QFFRNT, TRSFRT, QRDFTB, QFCFTB)
C
C           Calculate the heat leak out of the bottom, both
C           with and without compressor notch
C           The top has two Depth and two Width length edges
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AOB = AOFBOT
            AIB = AIFBOT
            R = AIB/BINSUL + 0.54*(2.0*DFRZ + 2.0*WFRZ)/2.0
     .      + 0.15*(4.*BINSUL+2.*TIFB+2.*TIFLS+2.*TIFRS+2.*TIFF)/9.0      !!?
            R1 = 1.0/(R*RKINFZ) + 1.0/(HI*AIB)
            R2 = 1.0/(HO*AOB)
            CALL RADTRN(R1, R2, TFRZ, TBTM, TRADBT, ERMBOT, EFRBOT, AOB,
     .            QBOTTM, TRSBOT, QRDBOT, QFCBOT)
                                                                          !!!
         CASE (2)                                                         !!!
            RB1 = AIFBTM1/BINSUL                                          !!!
     .          + 0.54*(WFRZ*(FALPHA+FBETA)+2.0*D1)/2.0                   !!!
     .          + 0.15*((2.0*TIFB+2.0*BINSUL+TIFLS+TIFRS)*FBETA           !!!
     .          + (4.0*BINSUL+TIFLS+TIFRS)*FALPHA)/9.0                    !!!
            R1 = 1.0/(RB1*RKINFZ)+1.0/(HI*AIFBTM1)                     !!!
            R2 = 1.0/(HO*AOFBTM1)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AOFBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB2 = AIFBTM2/BINSUL                                          !!!
     .          + 0.54*(WFRZ*(1.0+FALPHA)+2.0*D2)/2.0                     !!!
     .          + 0.15*((4.0*BINSUL+TIFLS+TIFRS)*FALPHA                   !!!
     .          + (2.0*BINSUL+2.0*TIFF+TIFLS+TIFRS))/9.0                  !!?
            R1 = 1.0/(RB2*RKINFZ)+1.0/(HI*AIFBTM2)                     !!!
            R2 = 1.0/(HO*AOFBTM2)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AOFBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
            QBOTTM = QBOTTM1 + QBOTTM2                                    !!!
                                                                          !!!
         CASE (3)                                                         !!!
            RB1 = AIFBTM1/BINSUL+0.54*(WFRZ*(1+FALPHA)+2.0*D1)/2.0        !!!
     .          + 0.15 * ((4.0*BINSUL+TIFLS+TIFRS)*FALPHA
     .          + (2.0*BINSUL+2.0*TIFB+TIFLS+TIFRS))/9.0                  !!!
            R1 = 1.0/(RB1*RKINFZ)+1.0/(HI*AIFBTM1)                     !!!
            R2 = 1.0/(HO*AOFBTM1)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AOFBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB2 = AIFBTM2/BINSUL+0.54*(2.0*WFRZ*FALPHA+2.0*D2)/2.0        !!!
     .          + 0.15 * (8.0*BINSUL+2.0*TIFLS+2.0*TIFRS)*FALPHA/9.0      !!!
            R1 = 1.0/(RB2*RKINFZ)+1.0/(HI*AIFBTM2)                     !!!
            R2 = 1.0/(HO*AOFBTM2)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AOFBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB3 = AIFBTM3/BINSUL+0.54*((1+FALPHA)*WFRZ+2.0*D3)/2.0        !!!
     .          + 0.15 * ((4.0*BINSUL*TIFLS+TIFRS)*FALPHA                 !!!
     .          + (2.0*BINSUL+2.0*TIFF+TIFLS+TIFRS))/9.0                  !!?
            R1 = 1.0/(RB3*RKINFZ)+1.0/(HI*AIFBTM3)                     !!!
            R2 = 1.0/(HO*AOFBTM3)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AOFBTM3,      !!!
     .            QBOTTM3,TRSBOT,QRDBOT,QFCBOT)                           !!!
            QBOTTM = QBOTTM1+QBOTTM2+QBOTTM3                              !!!
      END SELECT                                                          !!!
C
C           Sum all the heat leaks to get the cabinet heat leak.
C
      QFRZ = QFLSID + QFRSID + QBOTTM + QFBACK + QFFRNT + QMUL
      QFFT = QRLSID + QRRSID + QRBACK + QRFRNT + QTOP - QMUL
 
C
C     CALCULATE WEDGE HEAT LEAKS
C
      IF (WEDGER .NE. 0.0) THEN                                           !!!
          THETA = DATAN((TIRS-FLANGER)/WEDGER)                            !!!
          AWEDGE = WEDGER*(TIRS/(TIRS-FLANGER))
          BWEDGE = AWEDGE - WEDGER                                        !!!
          WL1 = 2.0*HFFC + WFFC
          WL2 = 2.0*(HFFC-FLANGER+TIRT) + WFFC + 2.0*(TIRS-FLANGER)       !!!
 
          R1 =  1.0/(HO*WEDGER*(2.0*TOPMUL+WIDTH))                        !!!
          R2 = THETA/(WKINr*DLOG(AWEDGE/BWEDGE)*(WL1+WL2)/2.0)
          DL =  DLOG(AWEDGE/BWEDGE)                                       !!!
          QWFF = (1.0/(1.0/(HO*WEDGER*(2.0*TOPMUL+WIDTH)) + THETA/(WKINr   !!!
     .            *DLOG(AWEDGE/BWEDGE)*(WL1+WL2)/2.0)))*(TROOM-TFF)
      END IF                                                              !!!
 
          THETA = DATAN((TIFS-FLANGE)/WEDGE)
          AWEDGE = WEDGE*(TIFS/(TIFS-FLANGE))
          BWEDGE = AWEDGE - WEDGE
          WL = HFRZ + BINSUL - FLGB
          DL =  DLOG(AWEDGE/BWEDGE)
          R1 =  1.0/(2.0*HO*HFRZ*WEDGE)
          R2 = THETA/(WKIN*DLOG(AWEDGE/BWEDGE)*(WL+HFRZ))
          QWFZS = (1.0/(1.0/(2.0*HO*HFRZ*WEDGE) + THETA/(WKIN*
     .              DLOG(AWEDGE/BWEDGE)*(WL+HFRZ))))*(TROOM-TFRZ)
          THETA = DATAN((BINSUL-FLGB)/WEDGE)
          AWEDGE = BINSUL/(BINSUL-FLGB)
          BWEDGE = AWEDGE - WEDGE
          WL = WIDTH - 2.0*FLANGE
          QWFZB = (1.0/(1.0/(HO*WEDGE*WIDTH)+THETA/(WKIN*DLOG(AWEDGE/
     .              BWEDGE)*(WL+WFRZ)*0.5)))*(TBTM-TFRZ)
C
C      QW is the total heat leak through the wedge
C
            QW = QWFF + QWFZS + QWFZB
            QWFZ = QWFZS + QWFZB
C
C      CALCULATE THE GASKET HEAT LEAKS FOR EACH COMPARTMENT AND FOR
C      FREEZER FAN ON AND FREEZER FAN OFF
C
C      QGR  is the gasket heat leak through the fresh food compartment
C      QGZN is the gasket heat leak through the freezer with the
C           freezer fan off
C      QGZF is the gasket heat leak through the freezer with the
C           freezer fan on
C
            QGR =  24.0*HLRG*(TOPMUL+WIDTH)*(TROOM-TFF)
 
            QGZF = 24.0*HLGZF*(HFRZ+WIDTH)*(TROOM-TFRZ)
            QGZN = QGZF
C
C           The heat leak due to Door Openings
C
      CALL DOORPN(TFF, TFRZ, HFFC, WFFC, DFFC, HFRZ, WFRZ, DFRZ,
     .            QDFFCS, QDFFFS, QDFZCS, QDFZFS,QDFFCL, QDFFFL,
     .            QDFZCL, QDFZFL)
 
      RETURN
      END
