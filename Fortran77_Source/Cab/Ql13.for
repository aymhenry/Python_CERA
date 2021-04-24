$DEBUG
      SUBROUTINE QL13
C     ******************************************************************
C     *    CALCULATE CABINET HEAT LOADS FOR CONFIGURATION 3            *
C     *                                                                *
C     *    Configuration 3: Top Mount R/F                              *
C     ******************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / QLONE /  TMF,TMS,TMB
      COMMON / QLMISC / TOPMUL,THMUL,TIFT,TIRT,CINSUL,HLRG,CKMUL,HLFZG,
     .                  DGSKT,CDUP,CDDN,CCHGT,NCCTYPE                     !!!
      COMMON / QLN5 /   WKIN,WEDGE,FLANGE,FLGB,WEDGER,FLANGER,wkinr       !!!
      COMMON / OPEND /  QDFFCS, QDFFFS, QDFZCS, QDFZFS, QDFFCL, QDFFFL,
     .                  QDFZCL, QDFZFL
      COMMON / ERA /    IRFTYP, FZHEAT, FFHEAT, WATERZ, WATERF, HXVUR,
     .                  HXVUZ
C
C          Calculate overall INTERNAL Height, Width and Depth of Freezer
C          compartment (FRZ) and the Fresh Food Compartment (FFC)
C
      HFRZ = TOPMUL - TIFT
      WFRZ = WIDTH - TIFLS - TIFRS
      DFRZ = DEPTH - WEDGE - TIFF - TIFB - DGSKT                          !!?
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            HFFC = HEIGHT - TOPMUL - THMUL - BINSUL
            WFFC = WIDTH - TIRLS - TIRRS
            DFFC = DEPTH - WEDGER - TIRF - TIRB -DGSKT                    !!!
 
         CASE (2)                                                         !!!
            BETA = ATAN(CDDN/CCHGT)                                       !!!
            ALPHA = 3.14159/4.0-BETA/2.0                                  !!!
            H1 = HEIGHT-TOPMUL-THMUL-BINSUL                               !!!
 
            HTRIAN = CCHGT-BINSUL+binsul/sin(beta)
     .             - tirb/tan(beta)
 
  !         H2 = H1-CCHGT+BINSUL-TIRB*TAN(BETA/2.0)                       !!!
  !         D1 = CDDN/SIN(BETA)                                           !!!
  !  .                  -BINSUL*(TAN(BETA/2.0)+TAN(ALPHA))                !!!
  !         D2 = DEPTH-CDDN-TIRF-WEDGER-BINSUL*TAN(ALPHA)-DGSKT           !!!
            DC = DEPTH-WEDGER-TIRF-TIRB-DGSKT                             !!!
 
            h2 = h1 - htrian
            d1 = htrian/cos(beta)
            d2 = dc - htrian*tan(beta)
 
            WFFC = WIDTH-TIRLS-TIRRS                                      !!!
            HFFC = H1                                                     !!!
                                                                          !!!
         CASE (3)                                                         !!!
            BETA = ATAN((CDDN-CDUP)/CCHGT)                                !!!
            ALPHA = 3.14159/4.0-BETA/2.0                                  !!!
            H1 = HEIGHT-TOPMUL-THMUL-BINSUL                               !!!
            H2 = H1-CCHGT                                                 !!!
            D1 = CDUP-TIRB                                                !!!
            IF ((CDDN-CDUP).EQ.0.0) THEN                                  !!!
                D2 = CCHGT-BINSUL                                         !!!
            ELSE                                                          !!!
                D2 = (CDDN-CDUP)/SIN(BETA)-BINSUL*TAN(ALPHA)              !!!
            END IF                                                        !!!
            D3 = DEPTH-CDDN-TIRF-WEDGER-BINSUL*TAN(ALPHA)-DGSKT           !!!
            DC = DEPTH-WEDGER-TIRF-TIRB-DGSKT                             !!!
            WFFC = WIDTH-TIRLS-TIRRS                                      !!!
            HFFC = H1                                                     !!!
                                                                          !!!
      END SELECT                                                          !!!
                                                                          !!!
      FALPHA = 4.0*ALPHA/3.14159                                          !!!
      FBETA  = 2.0*BETA/3.14159                                           !!!
C
C           AMUL IS THE MULLION AREA (FT2)
C           UMUL IS THE MULLION OVERALL HEAT TRANSFER (BTU/HR-FT2-DEG F)
C           QMULI is the heat leaking into the freezer from the fresh food
C
      AMUL = WFRZ*DFRZ
      UMUL = 1./(1./HIFMUL + 1./HIRMUL + THMUL/CKMUL)
      QMULI = UMUL*AMUL*(TFF - TFRZ)
C
C           Internal Freezer (F) Areas
C
      AIFSID = HFRZ*DFRZ
      AIFTOP = WFRZ*DFRZ
      AIFBCK = HFRZ*WFRZ
C
C           External Freezer (F) Areas
C
      AOFSID = (TOPMUL + THMUL/2.0)*(DEPTH-WEDGE-DGSKT)                   !!!
      AOFTOP = WIDTH*(DEPTH-WEDGE-DGSKT)                                  !!!
      AOFBCK = (TOPMUL + THMUL/2.0)*WIDTH
C
C           Internal Fresh Food (R) Areas
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AIRSID = HFFC*DFFC
            AIRBOT = WFFC*DFFC
            AIRBCK = HFFC*WFFC
            AIRFNT = AIRBCK
                                                                          !!!
         CASE (2)                                                         !!!
            AIRSID = DC*H2+(DC+D2)*(H1-H2)/2.0                            !!!
            AIRBCK = WFFC*H2                                              !!!
            AIRFNT = WFFC*H1                                              !!!
            AIRBTM1 = WFFC*D1                                             !!!
            AIRBTM2 = WFFC*D2                                             !!!
                                                                          !!!
         CASE (3)                                                         !!!
            AIRSID = DC*H2+CCHGT*(D3+D3+CCHGT*TAN(BETA))/2.0              !!!
            AIRBCK = WFFC*H2                                              !!!
            AIRFNT = WFFC*H1                                              !!!
            AIRBTM1 = WFFC*(D1+BINSUL*TAN(ALPHA))                         !!!
            AIRBTM2 = WFFC*(D2+BINSUL*TAN(ALPHA))                         !!!
            AIRBTM3 = WFFC*D3                                             !!!
                                                                          !!!
      END SELECT                                                          !!!
C
C           External Fresh Food (R) Areas
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AORSID = (HEIGHT-TOPMUL-THMUL/2.0)*(DEPTH-WEDGER-DGSKT)       !!!
            AORBOT = WIDTH*(DEPTH-WEDGER-DGSKT)                           !!!
            AORBCK = (HEIGHT-TOPMUL-THMUL/2.0)*WIDTH
            AORFNT = AORBCK
                                                                          !!!
         CASE (2)                                                         !!!
            AORSID = (HEIGHT-TOPMUL-THMUL/2.0)*(DEPTH-WEDGER-DGSKT)       !!!
     .                      -CDDN*CCHGT/2.0                               !!!
            AORFNT = (HEIGHT-TOPMUL-THMUL/2.0)*WIDTH                      !!!
            AORBCK = AORFNT-CCHGT*WIDTH                                   !!!
            AORBTM1 = WIDTH*CCHGT/COS(BETA)                               !!!
            AORBTM2 = WIDTH*(DEPTH-CDDN-WEDGER-DGSKT)                     !!!
                                                                          !!!
         CASE (3)                                                         !!!
            AORSID = (HEIGHT-TOPMUL-THMUL/2.0)*(DEPTH-WEDGER-DGSKT)       !!!
     .                      -(CDDN+CDUP)*CCHGT/2.0                        !!!
            AORFNT = (HEIGHT-TOPMUL-THMUL/2.0)*WIDTH                      !!!
            AORBCK = AORFNT-CCHGT*WIDTH                                   !!!
            AORBTM1 = WIDTH*CDUP                                          !!!
            AORBTM2 = WIDTH*CCHGT/COS(BETA)                               !!!
            AORBTM3 = WIDTH*(DEPTH-CDDN-WEDGER-DGSKT)                     !!!
                                                                          !!!
      END SELECT                                                          !!!
C
C           The average insualtion conductivity. DKIN is the door
C           insulation conductivity. RKIN is the side, back, top and
C           bottom insulation conductivity, i.e. the cabinet.
C           FF is the Fresh Food, FZ is the FreeZer
C
      TAVGL=0.25*(DKINFF + DKINFZ + RKINFF + RKINFZ)
C
C           Calculate the cabinet heat leak as the sum of the top, sides,
C                     bottom, front and back heat leaks.
C
C           Note that the cabinet has six wall sections (top, bottom
C           left side, right side, bottom and back), 12 edges and 8
C           corners. The door, which involves 4 edges and 4
C           corners, has in addition to the edge and corner effect,
C           a gasket and a wedge heat leak added in.
C           The shape factor for an edge is 0.54*Length of the edge
C           The shape factor for a corner is 0.15*Wall thickness.
C           (Holman p. 54).
C           Please note that most corners have two or three
C           insulation thicknesses, thus we average all three
C           thicknesses.
C
C           TIFT   is the insulation thickness on top (FT)
C           TIFRS  is the insulation thickness on the Freezer Right side (FT)
C           TIFLS  is the insulation thickness on the Freezer Left side (FT)
C           TIRLS  is the insulation thickness on the Left Fresh Food Side (FT)
C           TIRRS  is the insulation thickness on the Right Fresh Food Side (FT)
C           TIFB   is the insulation thickness on the Freezer back (FT)
C           TIRB   is the insulation thickness on the Fresh Food back (FT)
C           TIFF   is the insulation thickness on the Freezer front (FT)
C           TIRF   is the insulation thickness on the Fresh Food front (FT)
C           BINSUL is the insulation thickness on the bottom (FT)
C
C           R is the conduction resistance. It is the sum of the
C             wall resistance plus the edge resistance plus the
C             corner resistance in that order.
C
C           The Left Side Wall Resistance (R) and heat leak (QLFSID)
C           The side walls have two Depth and two Height length edges
C           The edge shape factor is divided by two because
C           each edge is shared by two walls.
C
C           The corner shape factor is divided by three because
C           each corner is shared by three walls.
C           (Actually each corner shape factor is divided by 9 because
C           the three corner thicknesses are averaged (9 = 3X3))
C
C           Calculate the heat leak out of the LEFT Freezer Side
C
      RF = AIFSID/TIFLS + 0.54*(DFRZ + 2.0*HFRZ)/2.0
     .                  + 0.15*(2.0*TIFLS+TIFB+2.0*TIFT+TIFF)/9.          !!?
      R1 = 1.0/(RF*RKINFZ) + 1.0/(HILTT*AIFSID)
      R2 = 1.0/(HOLTT*AOFSID)
 
      CALL RADTRN(R1, R2, TFRZ, TFLSID, TRALTT, ERMLTT, EFRLTT, AOFSID,
     .            QFLSID, TFSLFT, QRDLTT, QFCLTT)
C
C           Calculate the heat leak out of the LEFT Fresh Food Side
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            RR = AIRSID/TIRLS + 0.54*(DFFC + 2.0*HFFC)/2.0
     .                  + 0.15*(2.0*TIRLS+TIRB+2.0*BINSUL+TIRF)/9.0
                                                                          !!!
         CASE (2)                                                         !!!
            RR = AIRSID/TIRLS+0.54*(H1+H2+D1+D2)/2.0 + 0.15               !!!
     .                  * ((2.0*BINSUL+TIRLS)*FALPHA                      !!!
     .                  + (TIRB+TIRLS+BINSUL)*FBETA                       !!!
     .                  + (TIRF+TIRLS+BINSUL))/9.0                        !!!
                                                                          !!!
         CASE (3)                                                         !!!
            RR = AIRSID/TIRLS+0.54*(H1+H2+D1+D2+D3)/2.0 + 0.15            !!!
     .                  * (2.0*(2.0*BINSUL+TIRLS)*FALPHA                  !!!
     .                  + (2.0*(TIRLS+BINSUL)+TIRF+TIRB))/9.0             !!!
      END SELECT                                                          !!!
 
      R1 = 1.0/(RR*RKINFF) + 1.0/(HILTB*AIRSID)
      R2 = 1.0/(HOLTB*AORSID)
 
      CALL RADTRN(R1, R2, TFF, TRLSID, TRALTB, ERMLTB, EFRLTB, AORSID,
     .            QRLSID, TRSLFT, QRDLTB, QFCLTB)
C
C           Calculate the heat leak out of the RIGHT Freezer Side
C
      RF = AIFSID/TIFRS + 0.54*(DFRZ + 2.0*HFRZ)/2.0
     .                  + 0.15*(2.0*TIFRS+TIFB+2.0*TIFT+TIFF)/9.          !!?
      R1 = 1.0/(RF*RKINFZ) + 1.0/(HIRTT*AIFSID)
      R2 = 1.0/(HORTT*AOFSID)
 
      CALL RADTRN(R1, R2, TFRZ, TFRSID, TRARTT, ERMRTT, EFRRTT, AOFSID,
     .            QFRSID, TFSRGT, QRDRTT, QFCRTT)
C
C           Calculate the heat leak out of the RIGHT Fresh Food Side
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            RR = AIRSID/TIRRS + 0.54*(DFFC + 2.0*HFFC)/2.0
     .                  + 0.15*(2.0*TIRRS+TIRB+2.0*BINSUL+TIRF)/9.0
                                                                          !!!
         CASE (2)                                                         !!!
            RR = AIRSID/TIRRS + 0.54*(H1+H2+D1+D2)/2.0 + 0.15             !!!
     .                  * ((2.0*BINSUL+TIRRS)*FALPHA                      !!!
     .                  + (TIRB+TIRRS+BINSUL)*FBETA                       !!!
     .                  + (TIRF+TIRRS+BINSUL))/9.0                        !!!
                                                                          !!!
         CASE (3)                                                         !!!
            RR = AIRSID/TIRRS + 0.54*(H1+H2+D1+D2+D3)/2.0 + 0.15          !!!
     .                  * (2.0*(2.0*BINSUL+TIRRS)*FALPHA                  !!!
     .                  + (2.0*(TIRRS+BINSUL)+TIRF+TIRB))/9.0             !!!
 
      END SELECT                                                          !!!
 
      R1 = 1.0/(RR*RKINFF) + 1.0/(HIRTB*AIRSID)
      R2 = 1.0/(HORTB*AORSID)
      CALL RADTRN(R1, R2, TFF, TRRSID, TRARTB, ERMRTB, EFRRTB, AORSID,
     .            QRRSID, TRSRGT, QRDRTB, QFCRTB)
C
C           Calculate the heat leak out of the top (Freezer Only)
C           The top has two Depth and two Width length edges
C
      R  = AIFTOP/TIFT + 0.54*(2.0*DFRZ + 2.0*WFRZ)/2.0
     .     + 0.15*(2.*TIFLS+2.*TIFRS+2.*TIFB+4.*TIFT+2.*TIFF)/9.0         !!?
      R1 = 1.0/(R*RKINFZ) + 1.0/(HITOP*AIFTOP)
      R2 = 1.0/(HOTOP*AOFTOP)
 
      CALL RADTRN(R1, R2, TFRZ, TTOP, TRADT, ERMTOP, EFRTOP, AOFTOP,
     .            QTOP, TFSTOP, QRDTOP, QFCTOP)
 
C
C           Calculate the heat leak out of the BACK Freezer
C           The back has two Height and one Width length edges
C
      R  = AIFBCK/TIFB + 0.54*(2.0*HFRZ + WFRZ)/2.0
     .                 + 0.15*(TIFLS+TIFRS+2.0*TIFB+2.0*TIFT)/9.0
      R1 = 1.0/(R*RKINFZ) + 1.0/(HIBKT*AIFBCK)
      R2 = 1.0/(HOBKT*AOFBCK)
 
      CALL RADTRN(R1, R2, TFRZ, TFBACK, TRABKT, ERMBKT, EFRBKT, AOFBCK,
     .            QFBACK, TFSBCK, QRDBKT, QFCBKT)
C
C           Calculate the heat leak out of the BACK Fresh Food
C           The back has two Height and one Width length edges
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            R  = AIRBCK/TIRB + 0.54*(2.0*HFFC + WFFC)/2.0
     .                  + 0.15*(TIRLS+TIRRS+2.0*TIRB+2.0*BINSUL)/9.0
                                                                          !!!
         CASE (2)                                                         !!!
            R  = AIRBCK/TIRB+0.54*(2.0*H2+WFFC*FBETA)/2.0+0.15            !!!
     .                  *(TIRLS+TIRRS+2.0*TIRB+2.0*BINSUL)*FBETA/9.0      !!!
                                                                          !!!
         CASE (3)                                                         !!!
            R  = AIRBCK/TIRB + 0.54*(2.0*H2+WFFC)/2.0                     !!!
     .                  + 0.15*(TIRLS+TIRRS+2.0*TIRB+2.0*BINSUL)/9.0      !!!
                                                                          !!!
       END SELECT                                                         !!!
 
      R1 = 1.0/(R*RKINFF) + 1.0/(HIBKB*AIRBCK)
      R2 = 1.0/(HOBKB*AORBCK)
 
      CALL RADTRN(R1, R2, TFF, TRBACK, TRABKB, ERMBKB, EFRBKB, AORBCK,
     .            QRBACK, TRSBCK, QRDBKB, QFCBKB)
C
C           Calculate the heat leak out of the FRONT Freezer
C           The front has two Height and one Width length edges
C
      R  = AIFBCK/TIFF + 0.54*(2.0*HFRZ + WFRZ)/2.0
     .                 + 0.15*(TIFLS+TIFRS+2.0*TIFF+2.0*TIFT)/9.0         !!?
      R1 = 1.0/(R*DKINFZ) + 1.0/(HIFTT*AIFBCK)
      R2 = 1.0/(HOFTT*AOFBCK)
 
      CALL RADTRN(R1, R2, TFRZ, TFFRNT, TRAFTT, ERMFTT, EFRFTT, AOFBCK,
     .            QFFRNT, TFSFRT, QRDFTT, QFCFTT)
C
C           Calculate the heat leak out of the FRONT Fresh Food
C           The front has two Height and one Width length edges
C
      R  = AIRFNT/TIRF + 0.54*(2.0*HFFC + WFFC)/2.0
     .                  + 0.15*(TIRLS+TIRRS+2.0*TIRF+2.0*BINSUL)/9.0
 
      R1 = 1.0/(R*DKINFF) + 1.0/(HIFTB*AIRFNT)
      R2 = 1.0/(HOFTB*AORFNT)
 
      CALL RADTRN(R1, R2, TFF, TRFRNT, TRAFTB, ERMFTB, EFRFTB, AORFNT,
     .            QRFRNT, TRSFRT, QRDFTB, QFCFTB)
C
C           Calculate the heat leak out of the bottom, both
C           with and without compressor notch
C           The top has two Depth and two Width length edges
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AOB = AORBOT
            AIB = AIRBOT
            R = AIB/BINSUL + 0.54*(2.0*DFFC + 2.0*WFFC)/2.0
     .      + 0.15*(4.*BINSUL+2.*TIRB+2.*TIRLS+2.*TIRRS+2.*TIRF)/9.0
            R1 = 1.0/(R*RKINFF) + 1.0/(HIBOT*AIB)
            R2 = 1.0/(HOBOT*AOB)
            CALL RADTRN(R1, R2, TFF, TBTM, TRADBT, ERMBOT, EFRBOT, AOB,
     .            QBOTTM, TRSBOT, QRDBOT, QFCBOT)
                                                                          !!!
         CASE (2)                                                         !!!
            RB1 = AIRBTM1/BINSUL                                          !!!
     .          + 0.54*(WFFC*(FALPHA+FBETA)+2.0*D1)/2.0                   !!!
     .          + 0.15*((2.0*TIRB+2.0*BINSUL+TIRLS+TIRRS)*FBETA           !!!
     .          + (4.0*BINSUL+TIRLS+TIRRS)*FALPHA)/9.0                    !!!
            R1 = 1.0/(RB1*RKINFF)+1.0/(HIBOT*AIRBTM1)                     !!!
            R2 = 1.0/(HOBOT*AORBTM1)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB2 = AIRBTM2/BINSUL                                          !!!
     .          + 0.54*(WFFC*(1.0+FALPHA)+2.0*D2)/2.0                     !!!
     .          + 0.15*((4.0*BINSUL+TIRLS+TIRRS)*FALPHA                   !!!
     .          + (2.0*BINSUL+2.0*TIRF+TIRLS+TIRRS))/9.0                  !!!
            R1 = 1.0/(RB2*RKINFF)+1.0/(HIBOT*AIRBTM2)                     !!!
            R2 = 1.0/(HOBOT*AORBTM2)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
            QBOTTM = QBOTTM1 + QBOTTM2                                    !!!
                                                                          !!!
         CASE (3)                                                         !!!
            RB1 = AIRBTM1/BINSUL+0.54*(WFFC*(1+FALPHA)+2.0*D1)/2.0        !!!
     .          + 0.15 * ((4.0*BINSUL+TIRLS+TIRRS)*FALPHA                        !!!
     .          + (2.0*BINSUL+2.0*TIRB+TIRLS+TIRRS))/9.0                  !!!
            R1 = 1.0/(RB1*RKINFF)+1.0/(HIBOT*AIRBTM1)                     !!!
            R2 = 1.0/(HOBOT*AORBTM1)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB2 = AIRBTM2/BINSUL+0.54*(2.0*WFFC*FALPHA+2.0*D2)/2.0        !!!
     .          + 0.15 * (8.0*BINSUL+2.0*TIRLS+2.0*TIRRS)*FALPHA/9.0      !!!
            R1 = 1.0/(RB2*RKINFF)+1.0/(HIBOT*AIRBTM2)                     !!!
            R2 = 1.0/(HOBOT*AORBTM2)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB3 = AIRBTM3/BINSUL+0.54*((1+FALPHA)*WFFC+2.0*D3)/2.0        !!!
     .          + 0.15 * ((4.0*BINSUL*TIRLS+TIRRS)*FALPHA                 !!!
     .          + (2.0*BINSUL+2.0*TIRF+TIRLS+TIRRS))/9.0                  !!!
            R1 = 1.0/(RB3*RKINFF)+1.0/(HIBOT*AIRBTM3)                     !!!
            R2 = 1.0/(HOBOT*AORBTM3)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM3,      !!!
     .            QBOTTM3,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            QBOTTM = QBOTTM1+QBOTTM2+QBOTTM3                              !!!
 
      END SELECT                                                          !!!
C
C           Sum all the heat leaks to get the cabinet heat leak.
C
      QFRZ = QFLSID + QFRSID + QTOP + QFBACK + QFFRNT + QMUL
      QFFT = QRLSID + QRRSID + QRBACK + QRFRNT + QBOTTM - QMUL
 
C
C           CALCULATE GASKET HEAT LEAKS FOR EACH COMPARTMENT AND FOR
C           FREEZER FAN ON AND FREEZER FAN OFF
 
      IF (IRFTYP .EQ. 7) THEN
         QGZN = 12.0*HLFZG*(2.0 * TOPMUL + WIDTH) * (TROOM - TFRZ)
         QGZF =QGZN
         QGR = 12.0*HLRG*(2.0 * HFFC+WIDTH)*(TROOM-TFF)
      ELSE
         QGZN = 24.0*HLFZG*(TOPMUL + WIDTH) * (TROOM - TFRZ)
         QGZF =QGZN
         QGR = 24.0*HLRG*(HFFC+WIDTH)*(TROOM-TFF)
      END IF
C
C           CALCULATE WEDGE HEAT LEAKS
C
      THETAL = DATAN((TIFLS-FLANGE)/WEDGE)
      THETAR = DATAN((TIFRS-FLANGE)/WEDGE)
      AWEDGL = WEDGE*(TIFLS/(TIFLS-FLANGE))
      AWEDGR = WEDGE*(TIFRS/(TIFRS-FLANGE))
      BWEDGL = AWEDGL-WEDGE
      BWEDGR = AWEDGR-WEDGE
 
      WL1 = 2.0*HFRZ+WFRZ
      WL2 = 2.0*(HFRZ-FLANGE+TIFT) + WFRZ + (TIFLS-FLANGE)
     .                             + (TIFRS-FLANGE)
 
      QWFZ = (1.0/(1.0/(HOLTT*WEDGE*(2.0*TOPMUL+WIDTH))
     .       + (THETAL/(WKIN*DLOG(AWEDGL/BWEDGL)*(WL1+WL2)/2.0)
     .       + THETAR/(WKIN*DLOG(AWEDGR/BWEDGR)*(WL1+WL2)/2.0))/2.0))*
     .         (TROOM-TFRZ)
 
      IF (WEDGER .NE. 0.0) THEN                                           !!!
         THETAL = DATAN((TIRLS-FLANGER)/WEDGER)                           !!!
         THETAR = DATAN((TIRRS-FLANGER)/WEDGER)                           !!!
 
         AWEDGL = WEDGER*(TIRLS/(TIRLS-FLANGER))                          !!!
         AWEDGR = WEDGER*(TIRRS/(TIRRS-FLANGER))                          !!!
         BWEDGL = AWEDGL-WEDGER                                           !!!
         BWEDGR = AWEDGR-WEDGER                                           !!!
 
         WL = HFFC+BINSUL-FLANGER                                         !!!
 
         QWFS = (1.0/(1.0/(HOLTB*WEDGER*2.0*HFFC)                         !!!
     .       + (THETAL/(WKINr*DLOG(AWEDGL/BWEDGL)*(WL+HFFC))
     .       + THETAR/(WKINr*DLOG(AWEDGR/BWEDGR)*(WL+HFFC)))/2.0))*
     .         (TROOM-TFF)
         THETA = DATAN((BINSUL-FLGB)/WEDGER)                              !!!
         AWEDGE = WEDGER*(BINSUL/(BINSUL-FLGB))                           !!!
         BWEDGE = AWEDGE-WEDGER                                           !!!
 
         WL = WIDTH - 2.0*FLANGER                                         !!!
         QWFB = (1.0/(1.0/(HOBOT*WEDGER*WIDTH)+THETA/                     !!!
     .       (WKINr*DLOG(AWEDGE/BWEDGE)*(WFFC+WL)/2.0)))*(TBTM-TFF)
      END IF
 
      QW = QWFZ + QWFS + QWFB
      QWFF = QWFS + QWFB
C
C           SUM VARIOUS COMPONENTS OF THE HEAT LEAK
C
      QGON = QGR + QGZN
      QGOF = QGR + QGZF
      QTON = QFRZ + QFFT + QGON + QW
      QTOF = QFRZ + QFFT + QGOF + QW
C
C           The heat leak due to Door Openings
C
      CALL DOORPN(TFF, TFRZ, HFFC, WFFC, DFFC, HFRZ, WFRZ, DFRZ,
     .            QDFFCS, QDFFFS, QDFZCS, QDFZFS,QDFFCL, QDFFFL,
     .            QDFZCL, QDFZFL)
 
      RETURN
      END
