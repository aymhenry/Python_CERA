$DEBUG
      SUBROUTINE QL467
C     ******************************************************************
C     *     CALCULATE HEAT LEAKS FOR UPRIGHT FREEZER                   *
C     ******************************************************************
C
 
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / QLFOUR / FH, FW, FD
      COMMON / QLMISC / TOPMUL,THMUL,TIFT,TIRT,CINSUL,HLRG,CKMUL,HLFZG,
     .                  DGSKT,CDUP,CDDN,CCHGT,NCCTYPE                     !!!
      COMMON / QLN5 /   WKIN,WEDGE,FLANGE,FLGB
      COMMON / OPEND /  QDFFCS, QDFFFS, QDFZCS, QDFZFS, QDFFCL, QDFFFL,
     .                  QDFZCL, QDFZFL
C
C     This type of refrigerator has individual wall HI & HO so set the
C     universal HI & HO to zero
C
      HI = 0.0
      HO = 0.0
C
C     CALCULATE INTERNAL DIMENSTIONS OF THE COMPARTMENTS
C
      HC = HEIGHT - BINSUL - TIFT
      WC = WIDTH - TIFRS - TIFLS
      DC = DEPTH - TIFF - TIFB - WEDGE - DGSKT                            !!!
      TBOX = TFRZ
      TITOP = TIFT
      TIBK = TIFB
      TIRSD = TIFRS
      TILSD = TIFLS
      TIFRT = TIFF
 
      SELECT CASE (NCCTYPE)                                               !!!
         CASE (2)                                                         !!!
            BETA = ATAN(CDDN/CCHGT)                                       !!!
            ALPHA = 3.14159/4.0-BETA/2.0                                  !!!
            H1 = HC                                                       !!!
  !         H2 = H1-CCHGT+BINSUL-TIFB*TAN(BETA/2.0)                       !!!
  !         D1 = CDDN/SIN(BETA)-BINSUL*(TAN(BETA/2.0)+TAN(ALPHA))         !!!
  !         D2 = DEPTH-CDDN-TIFF-WEDGE-BINSUL*TAN(ALPHA)-DGSKT            !!!
 
             HTRIAN = CCHGT-BINSUL+binsul/sin(beta)
     .              - tifb/tan(beta)
 
            h2 = h1 - htrian
            d1 = htrian/cos(beta)
            d2 = dc - htrian*tan(beta)
 
         CASE (3)                                                         !!!
            BETA = ATAN((CDDN-CDUP)/CCHGT)                                !!!
            ALPHA = 3.14159/4.0-BETA/2.0                                  !!!
            H1 = HC                                                       !!!
            H2 = H1-CCHGT                                                 !!!
            D1 = CDUP-TIFB                                                !!!
            IF ((CDDN-CDUP) .EQ. 0.0) THEN                                !!!
                D2 = CCHGT-BINSUL                                         !!!
            ELSE                                                          !!!
                D2 = (CDDN-CDUP)/SIN(BETA)-BINSUL*TAN(ALPHA)              !!!
            END IF                                                        !!!
            D3 = DEPTH-CDDN-TIFF-WEDGE-BINSUL*TAN(ALPHA)-DGSKT            !!!
      END SELECT                                                          !!!
                                                                          !!!
      FALPHA = 4.0*ALPHA/3.14159                                          !!!
      FBETA  = 2.0*BETA/3.14159                                           !!!
C
C           CALCULATE INTERNAL SURFACE AREAS
C
C           The internal area of the left or right side - AISIDE
C           The internal area of the top or bottom - AITOP
C           The internal area of the front or back - AIBACK
C
      SELECT CASE (NCCTYPE)                                               !!!
 
         CASE (1)
            AISIDE = HC*DC
            AITOP  = WC*DC
            AIBACK = HC*WC
            AIFRNT = AIBACK
            AIBTM  = AITOP
 
         CASE (2)                                                         !!!
            AISIDE = DC*H2+(DC+D2)*(H1-H2)/2.0                            !!!
            AITOP  = WC*DC                                                !!!
            AIBACK = WC*H2                                                !!!
            AIFRNT = WC*H1                                                !!!
            AIBTM1 = WC*D1                                                !!!
            AIBTM2 = WC*D2                                                !!!
                                                                          !!!
         CASE (3)                                                         !!!
            AISIDE = DC*H2+CCHGT*(D3+D3+CCHGT*TAN(BETA))/2.0              !!!
            AITOP  = WC*DC                                                !!!
            AIBACK = WC*H2                                                !!!
            AIFRNT = WC*H1                                                !!!
            AIBTM1 = WC*(D1+BINSUL*TAN(ALPHA))                            !!!
            AIBTM2 = WC*(D2+BINSUL*TAN(ALPHA))                            !!!
            AIBTM3 = WC*D3                                                !!!
      END SELECT                                                          !!!
C
C           CALCULATE EXTERNAL SURFACE AREAS
C           The external area of the left or right side - AOSIDE
C           The external area of the top or bottom - AOTOP
C           The external area of the front or back - AOBACK
C
      SELECT CASE (NCCTYPE)                                               !!!
         CASE (1)
            AOSIDE = HEIGHT*(DEPTH-WEDGE-DGSKT)                           !!!
            AOTOP  = WIDTH*(DEPTH-WEDGE-DGSKT)                            !!!
            AOBACK = HEIGHT*WIDTH
            AOFRNT = AOBACK
            AOBTM  = AOTOP
 
         CASE (2)                                                         !!!
            AOSIDE = HEIGHT*(DEPTH-WEDGE-DGSKT)-CDDN*CCHGT/2.0            !!!
            AOTOP  = WIDTH*(DEPTH-WEDGE-DGSKT)                            !!!
            AOBACK = (HEIGHT-CCHGT)*WIDTH                                 !!!
            AOFRNT = HEIGHT*WIDTH                                         !!!
            AOBTM1 = WIDTH*CCHGT/COS(BETA)                                !!!
            AOBTM2 = WIDTH*(DEPTH-CDDN-WEDGE-DGSKT)                       !!!
 
         CASE (3)                                                         !!!
            AOSIDE = HEIGHT*(DEPTH-WEDGE-DGSKT)-(CDDN+CDUP)*CCHGT/2.0     !!!
            AOTOP  = WIDTH*(DEPTH-WEDGE-DGSKT)                            !!!
            AOBACK = (HEIGHT-CCHGT)*WIDTH                                 !!!
            AOFRNT = HEIGHT*WIDTH                                         !!!
            AOBTM1 = WIDTH*CDUP                                           !!!
            AOBTM2 = WIDTH*CCHGT/COS(BETA)                                !!!
            AOBTM3 = WIDTH*(DEPTH-CDDN-WEDGE-DGSKT)                       !!!
 
      END SELECT                                                          !!!
C
C           Account for the presence of the freezer compartment in a
C           single-door refrigerator.
C
      IF (NMOD .NE. 4) THEN
         ftop = 0
         fbak = 0
         fsid = 0
         TCABt = TFRZ
         TCABb = TFRZ
         TCABs = TFRZ
      ELSE
         TBOX = TFF
         ftop = FD * FW / AITOP
         fbak = FH * FW / AIBACK
         fsid = FH * FD / (2.0 * AISIDE)
         IF (FW .GE. 0.9* WC) fsid = 2.0 * fsid
         TCABt = ftop * TFRZ + (1.0 - ftop) * TFF
         TCABb = fbak * TFRZ + (1.0 - fbak) * TFF
         TCABs = fsid * TFRZ + (1.0 - fsid) * TFF
      END IF
 
C
C           The average insualtion conductivity. DKIN is the door
C           insulation conductivity. RKIN is the side, back, top and
C           bottom insulation conductivity, i.e. the cabinet.
C
      TAVGL = 0.5*(DKIN+RKIN)
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
C           TITOP  is the insulation thickness on top (FT)
C           TIRSD  is the insulation thickness on the Right side (FT)
C           TILSD  is the insulation thickness on the Left side (FT)
C           TIBK   is the insulation thickness on the back (FT)
C           TIFRT  is the insulation thickness on the front (FT)
C           BINSUL is the insulation thickness on the bottom (FT)
C
C           R is the conduction resistance. It is the sum of the
C             wall resistance plus the edge resistance plus the
C             corner resistance in that order.
C
C           The Left Side Wall Resistance (R) and heat leak (QLSIDE)
C           The side walls have two Depth and two Height length edges
C           The edge shape factor is divided by two because
C           each edge is shared by two walls.
C
C           The corner shape factor is divided by three because
C           each corner is shared by three walls.
C           (Actually each corner shape factor is divided by 9 because
C           the three corner thicknesses are averaged (9 = 3X3))
C
C           Calculate the heat leak out of the left side
C
      SELECT CASE (NCCTYPE)
         CASE (1)
            R = AISIDE/TILSD+0.54*(2.0*DC+2.0*HC)/2.0+0.15*(4.0*TILSD
     .                  + 2.0*TIBK+2.0*BINSUL+2.0*TITOP+2.0*TIFRT)/9.0
 
         CASE (2)                                                         !!!
            R = AISIDE/TILSD+0.54*(H1+H2+D1+D2+DC)/2.0                    !!!
     .                  + 0.15*((2.0*BINSUL+TILSD)*FALPHA                 !!!
     .                  + (3.0*TILSD+2.0*TITOP+2.0*TIFRT+BINSUL+TIBK)     !!!
     .                  + (TIBK+TILSD+BINSUL)*FBETA)/9.0                  !!!
                                                                          !!!
         CASE (3)                                                         !!!
            R = AISIDE/TILSD+0.54*(H1+H2+D1+D2+D3+DC)/2.0                 !!!
     .                  + 0.15*(2.0*(2.0*BINSUL+TILSD)*FALPHA             !!!
     .                  + (2.0*(2.0*TILSD+BINSUL+TIBK+TITOP+TIFRT)))/9.0  !!!
      END SELECT                                                          !!!
 
      R1 = 1.0/(R*RKIN) + 1.0/(HILFT*AISIDE)
      R2 = 1.0/(HOLFT*AOSIDE)
 
      CALL RADTRN(R1, R2, TCABs, TLSIDE, TRADL, ERMLFT, EFRLFT, AOSIDE,
     .      QLSIDE, TRSLFT, QRDLFT, QFCLFT)
 
C
C           Calculate the heat leak out of the right side
C
      SELECT CASE (NCCTYPE)
         CASE (1)
            R = AISIDE/TIRSD+0.54*(2.0*DC+2.0*HC)/2.0+0.15*(4.0*TIRSD
     .                  + 2.0*TIBK+2.0*BINSUL+2.0*TITOP+2.0*TIFRT)/9.0
 
         CASE (2)                                                         !!!
            R = AISIDE/TIRSD+0.54*(H1+H2+D1+D2+DC)/2.0                    !!!
     .                  + 0.15*((2.0*BINSUL+TIRSD)*FALPHA                 !!!
     .                  + (3.0*TIRSD+2.0*TITOP+2.0*TIFRT+BINSUL+TIBK)     !!!
     .                  + (TIBK+TIRSD+BINSUL)*FBETA)/9.0                  !!!
                                                                          !!!
         CASE (3)                                                         !!!
            R = AISIDE/TIRSD+0.54*(H1+H2+D1+D2+D3+DC)/2.0                 !!!
     .                  + 0.15*(2.0*(2.0*BINSUL+TIRSD)*FALPHA             !!!
     .                  + (2.0*(2.0*TIRSD+BINSUL+TIBK+TITOP+TIFRT)))/9.0  !!!
      END SELECT                                                          !!!
 
      R1 = 1.0/(R*RKIN) + 1.0/(HIRGT*AISIDE)
      R2 = 1.0/(HORGT*AOSIDE)
 
      CALL RADTRN(R1, R2, TCABs, TRSIDE, TRADR, ERMRGT, EFRRGT, AOSIDE,
     .      QRSIDE, TRSRGT, QRDRGT, QFCRGT)
C
C           Calculate the heat leak out of the top
C           The top has two Depth and two Width length edges
C
      R  = AITOP/TITOP + 0.54*(2.0*DC + 2.0*WC)/2.0
     .      + 0.15*(2.*TIRSD+2.*TILSD+2.*TIBK+4.*TITOP+2.*TIFRT)/9.
      R1 = 1.0/(R*RKIN) + 1.0/(HITOP*AITOP)
      R2 = 1.0/(HOTOP*AOTOP)
 
      CALL RADTRN(R1, R2, TCABt, TTOP, TRADT, ERMTOP, EFRTOP, AOTOP,
     .      QTOP, TRSTOP, QRDTOP, QFCTOP)
C
C           Calculate the heat leak out of the back
C           The back has two Height and two Width length edges
C
      SELECT CASE (NCCTYPE)
         CASE (1)
            R = AIBACK/TIBK + 0.54*(2.0*HC+2.0*WC)/2.0 + 0.15*(2.*TILSD
     .                  + 2.*TIRSD+4.*TIBK+2.*TITOP+2.*BINSUL)/9.0
 
         CASE (2)                                                         !!!
            R = AIBACK/TIBK + 0.54*(2.0*H2+(1+FBETA)*WC)/2.0              !!!
     .                  + 0.15*((TILSD+TIRSD+2.0*TIBK+2.0*BINSUL)*FBETA   !!!
     .                  + (TILSD+TIRSD+2.0*TIBK+2.0*TITOP))/9.0           !!!
                                                                          !!!
         CASE (3)                                                         !!!
            R = AIBACK/TIBK + 0.54*(2.0*H2+2.0*WC)/2.0 + 0.15*(2.*TILSD   !!!
     .                  + 2.*TIRSD+4.*TIBK+2.*TITOP+2.*BINSUL)/9.0        !!!
      END SELECT                                                          !!!
 
      R1 = 1.0/(R*RKIN) + 1.0/(HIBCK*AIBACK)
      R2 = 1.0/(HOBCK*AOBACK)
 
      CALL RADTRN(R1, R2, TCABb, TBACK, TRADBK, ERMBCK, EFRBCK, AOBACK,
     .      QBACK, TRSBCK, QRDBCK, QFCBCK)
C
C           Calculate the heat leak out of the front
C           The front has two Height and two Width length edges
C
      R  = AIFRNT/TIFRT + 0.54*(2.0*HC + 2.0*WC)/2.0
     .      + 0.15*(2.*TILSD+2.*TIRSD+4.*TIFRT+2.*TITOP+2.*BINSUL)/9.0
      R1 = 1.0/(R*DKIN) + 1.0/(HIFRT*AIFRNT)
      R2 = 1.0/(HOFRT*AOFRNT)
 
      CALL RADTRN(R1, R2, TBOX, TFRONT, TRADF, ERMFRT, EFRFRT, AOBACK,
     .      QFRONT, TRSFRT, QRDFRT, QFCFRT)
C
C           Calculate the heat leak out of the bottom, both
C           with and without compressor notch
C           The top has two Depth and two Width length edges
C
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AOB = AOBTM
            AIB = AIBTM
            R = AIB/BINSUL + 0.54*(2.0*DC + 2.0*WC)/2.0
     .      + 0.15*(4.*BINSUL+2.*TIBK+2.*TILSD+2.*TIRSD+2.*TIFRT)/9.0
            R1 = 1.0/(R*RKIN) + 1.0/(HIBOT*AIB)
            R2 = 1.0/(HOBOT*AOB)
            CALL RADTRN(R1,R2,TBOX,TBTM,TRADBT,ERMBOT,EFRBOT,AOB,
     .            QBOTTM, TRSBOT, QRDBOT, QFCBOT)
                                                                          !!!
         CASE (2)                                                         !!!
            RB1 = AIBTM1/BINSUL                                           !!!
     .          + 0.54*(WC*(FALPHA+FBETA)+2.0*D1)/2.0                     !!!
     .          + 0.15*((2.0*TIBK+2.0*BINSUL+TILSD+TIRSD)*FBETA           !!!
     .          + (4.0*BINSUL+TILSD+TIRSD)*FALPHA)/9.0                    !!!
            R1 = 1.0/(RB1*RKIN)+1.0/(HIBOT*AIBTM1)                        !!!
            R2 = 1.0/(HOBOT*AOBTM1)                                       !!!
            CALL RADTRN(R1,R2,TBOX,TBTM,TRADBT,ERMBOT,EFRBOT,AOBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB2 = AIBTM2/BINSUL                                           !!!
     .          + 0.54*(WC*(1.0+FALPHA)+2.0*D2)/2.0                       !!!
     .          + 0.15*((4.0*BINSUL+TILSD+TIRSD)*FALPHA                   !!!
     .          + (2.0*BINSUL+2.0*TIFRT+TILSD+TIRSD))/9.0                 !!!
            R1 = 1.0/(RB2*RKIN)+1.0/(HIBOT*AIBTM2)                        !!!
            R2 = 1.0/(HOBOT*AOBTM2)                                       !!!
            CALL RADTRN(R1,R2,TBOX,TBTM,TRADBT,ERMBOT,EFRBOT,AOBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
            QBOTTM = QBOTTM1 + QBOTTM2                                    !!!
                                                                          !!!
         CASE (3)                                                         !!!
            RB1 = AIBTM1/BINSUL+0.54*(WC*(1+FALPHA)+2.0*D1)/2.0           !!!
     .          + 0.15 * ((4.0*BINSUL+TILSD+TIRSD)*FALPHA                 !!!
     .          + (2.0*BINSUL+2.0*TIBK+TILSD+TIRSD))/9.0                  !!!
            R1 = 1.0/(RB1*RKIN)+1.0/(HIBOT*AIBTM1)                        !!!
            R2 = 1.0/(HOBOT*AOBTM1)                                       !!!
            CALL RADTRN(R1,R2,TBOX,TBTM,TRADBT,ERMBOT,EFRBOT,AOBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
                                                                          !!!
            RB2 = AIBTM2/BINSUL+0.54*(2.0*WC*FALPHA+2.0*D2)/2.0           !!!
     .          + 0.15 * (8.0*BINSUL+2.0*TILSD+2.0*TIRSD)*FALPHA/9.0      !!!
            R1 = 1.0/(RB2*RKIN)+1.0/(HIBOT*AIBTM2)                        !!!
            R2 = 1.0/(HOBOT*AOBTM2)                                       !!!
            CALL RADTRN(R1,R2,TBOX,TBTM,TRADBT,ERMBOT,EFRBOT,AOBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
                                                                          !!!
            RB3 = AIBTM3/BINSUL+0.54*((1+FALPHA)*WC+2.0*D3)/2.0           !!!
     .          + 0.15 * ((4.0*BINSUL*TILSD+TIRSD)*FALPHA                 !!!
     .          + (2.0*BINSUL+2.0*TIFRT+TILSD+TIRSD))/9.0                 !!!
            R1 = 1.0/(RB3*RKIN)+1.0/(HIBOT*AIBTM3)                        !!!
            R2 = 1.0/(HOBOT*AOBTM3)                                       !!!
            CALL RADTRN(R1,R2,TBOX,TBTM,TRADBT,ERMBOT,EFRBOT,AOBTM3,      !!!
     .            QBOTTM3,TRSBOT,QRDBOT,QFCBOT)                           !!!
            QBOTTM = QBOTTM1+QBOTTM2+QBOTTM3                              !!!
      END SELECT                                                          !!!
C
C           Sum all the heat leaks to get the cabinet heat leak.
C
      QFRZ = QLSIDE + QRSIDE + QTOP + QBACK + QFRONT + QBOTTM
C
C           CALCULATE GASKET HEAT LEAK
C
      QGZN = 24.0*(HEIGHT-BOTTOM+WIDTH)*HLFZG*(TROOM-TBOX)
      QGZF = QGZN
C
C     CALCULATE WEDGE HEAT LEAK
C
      QW = 0
      if (wedge .ne. 0.0) then
         THETAL = DATAN((TILSD-FLANGE)/WEDGE)
         THETAR = DATAN((TIRSD-FLANGE)/WEDGE)
 
         IF(((TILSD-FLANGE).EQ.0.0) .OR. ((TIRSD-FLANGE).EQ.0.0)) THEN
            WRITE(IO,900) TIRSD, TILSD, FLANGE
            CALL EXIT
         END IF
 
         AWEDGL = WEDGE*(TILSD/(TILSD-FLANGE))
         AWEDGR = WEDGE*(TIRSD/(TIRSD-FLANGE))
         BWEDGL = AWEDGL - WEDGE
         BWEDGR = AWEDGR - WEDGE
         WL1 = 2.0*HC + WC
         WL2 = 2.0*(HC+BINSUL-FLGB+TITOP-FLANGE) + WIDTH - 2.0*FLANGE
         HOWDG = (HOLFT + HORGT + HOTOP + HOBOT)/4.0
 
         QWC = (1.0/(1.0/(HOWDG*WEDGE*(2.0*(HEIGHT-BOTTOM)+WIDTH))+
     .        (THETAR/(WKIN*DLOG(AWEDGR/BWEDGR)*(WL1+WL2)/2.0) +
     .        THETAL/(WKIN*DLOG(AWEDGL/BWEDGL)*(WL1+WL2)/2.0))/2.0))*
     .        (TROOM-TBOX)
 
         THETA = DATAN((BINSUL-FLGB)/WEDGE)
 
         IF((BINSUL-FLGB) . EQ.0.0) WRITE(IO,901) BINSUL, FLGB
 
         AWEDGE = WEDGE*(BINSUL/(BINSUL-FLGB))
         BWEDGE = AWEDGE - WEDGE
C
C           SUM VARIOUS COMPONENTS OF HEAT LEAK
C
         QWB = (1.0/(1.0/(HOWDG*WEDGE*WIDTH) + THETA/
     .        (WKIN*DLOG(AWEDGE/BWEDGE)*
     .        (WC+WIDTH-2.0*FLANGE)/2.0)))*(TBTM-TBOX)
         QW = QWB + QWC
      end if
 
      QTON = QFRZ + QGZN + QW
      QTOF = QFRZ + QGZF + QW
C
C           The heat leak due to Door Openings
C
C
C           The heat leak due to Door Openings
C
      HFFC = 0.0
      WFFC = 0.0
      DFFC = 0.0
      HFRZ = HC
      WFRZ = WC
      DFRZ = DC
 
      IF (NMOD .NE. 4) THEN
         CALL DOORPN(TFF, TFRZ, HC, WC, DC, HC, WC, DC,
     .               QDFFCS, QDFFFS, QDFZCS, QDFZFS,QDFFCL, QDFFFL,
     .               QDFZCL, QDFZFL)
      ELSE
         CALL DOORPN(TFF, TFF, HC, WC, DC, HC, WC, DC,
     .               QDFFCS, QDFFFS, QDFZCS, QDFZFS,QDFFCL, QDFFFL,
     .               QDFZCL, QDFZFL)
 
      END IF
      RETURN
C
C          ERROR MESSAGES
C
  900 FORMAT(' In subroutine QL467, we are dividing by zero'/
     .      ' because TIRSD/TILSD (insulation thickness on the side)'/
     .      ' and FLANGE (depth of top and side flanges)'/
     .      ' are equal. TIRSD/TILSD should be larger. TIRSD, TILSD',
     .      ' and FLANGE are respectively :'/3F15.7)
  901 FORMAT(' In subroutine QL467, we are dividing by zero'/
     .      ' because BINSUL (bottom insulation thickness)'/
     .      ' and FLGB (bottom flange depth) are equal.'/
     .      ' BINSUL should be larger than FLGB. BINSUL & FLGB are',
     .      ' respectively :'/2f15.7)
      END
