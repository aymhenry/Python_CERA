$DEBUG
      SUBROUTINE QL2
C     ******************************************************************
C     *     CALCULATE CABINET HEAT LEAKS FOR CONFIGURATION 2           *
C     *     Configuration 2: Side by Side R/F                          *
C     ******************************************************************
C
C        FIGURE 1: CONFIGURATION 2 SIDE BY SIDE R/F
C
C
C               __________________**_____________
C              /|                 /|           / |
C             / |                / |          /  |
C            /  |               /  |         /   |
C           /   |             ***  |        /    |
C          /    |             /    |       /     |
C         /     |            /     |      /      |
C        /_________________ **___________/       |
C       |       |           |            |       |
C       |       |           |      |     |       |
C       |       |           |      |     |       |
C       |       |           |      |     |       |
C       |       |           |      |     |       |
C       |       |           |     ***    |       |
C       |       |           |      |     |       |
C       |       |           |      |     |       |
C       |       |          ***     |     |       |
C       |       |           |      |     |       |
C       |       |           |      |     |       |
C       |       |           |      |     |       |
C       |       |           |      |     |       |
C       |       |           |      |     |       |
C       |       |___________|_____**_____________|
C       |      /            |      /     |      /
C       |     /             |     /      |     /
C       |    /              |    /       |    /
C       |   /               |  ***       |   /
C       |  /                |  /         |  /
C       | /                 | /          | /
C       |/_________________**/___________|/
C
C
C     NOTE: THE FREEZER COMPARTMENT IS ON THE RIGHT
C
C     ** = HEAT LEAK THROUGH CORNER IS ACCOUNTED FOR IN THE MULLION HEAT
C          LEAK CALCULATION.
C
C     *** = HEAT LEAK THROUGH EDGE IS ACCOUNTED FOR IN THE MULLION HEAT
C           LEAK CALCULATION.
C
 
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / QLTWO /  DCOMP, WALL
      COMMON / QLMISC / TOPMUL,THMUL,TIFT,TIRT,CINSUL,HLRG,CKMUL,HLFZG,
     .                  DGSKT,CDUP,CDDN,CCHGT,NCCTYPE                     !!!
      COMMON / QLN5 /   WKIN,WEDGE,FLANGE,FLGB,WEDGER,FLANGER,wkinr       !!!
      COMMON / OPEND /  QDFFCS, QDFFFS, QDFZCS, QDFZFS, QDFFCL, QDFFFL,
     .                  QDFZCL, QDFZFL
C
C     CALCULATE THE INTERNAL DIMENSIONS OF THE TWO COMPARTMENTS, FREEZER (FRZ)
C     AND FRESH FOOD COMPARTMENT (FFC)
 
      TIFS = TIFRS
      TIRS = TIRLS
 
      HFRZ = HEIGHT - BINFRZ - TIFT
      DFRZ = DEPTH - TIFF - TIFB - WEDGE -DGSKT
      WFRZ = WIDTH - WALL - THMUL - TIFS
 
      DFFC = DEPTH - TIRF - TIRB - WEDGER - DGSKT
      WFFC = WALL - TIRS
      HFFC = HEIGHT - BINSUL - TIRT
 
      SELECT CASE (NCCTYPE)
 
         CASE (2)                                                         !!!
            BETA = ATAN(CDDN/CCHGT)                                       !!!
            ALPHA = 3.14159/4.0-BETA/2.0                                  !!!
 
            H1F = HEIGHT - BINSUL - TIRT
            H1Z = HEIGHT - BINFRZ - TIFT
 
            HTRIANF = CCHGT - BINSUL + BINSUL/SIN(BETA) - TIRB/TAN(BETA)
            HTRIANZ = CCHGT - BINFRZ + BINFRZ/SIN(BETA) - TIFB/TAN(BETA)
 
            H2F= H1F - HTRIANF
            H2Z= H1Z - HTRIANZ
 
            DCF = DEPTH - TIRF - TIRB - WEDGER - DGSKT
            DCZ = DEPTH - TIFF - TIFB - WEDGE - DGSKT
 
            D1F= HTRIANF/COS(BETA)                                        !!!
            D1Z= HTRIANZ/COS(BETA)                                        !!!
 
            D2F= DCF - HTRIANF*TAN(BETA)
            D2Z= DCZ - HTRIANZ*TAN(BETA)
 
         CASE (3)                                                         !!!
            BETA = ATAN((CDDN-CDUP)/CCHGT)                                !!!
            ALPHA = 3.14159/4.0-BETA/2.0                                  !!!
 
            H1F = HEIGHT - BINSUL - TIRT
            H1Z = HEIGHT - BINFRZ - TIFT
 
            H2F= H1F-CCHGT
            H2Z= H1Z-CCHGT
 
            D1F= CDUP-TIRB                                                !!!
            D1Z= CDUP-TIFB                                                !!!
 
            IF ((CDDN-CDUP).EQ.0.0) THEN                                  !!!
                D2F= CCHGT-BINSUL                                         !!!
                D2Z= CCHGT-BINFRZ                                        !!!
            ELSE                                                          !!!
                D2F= (CDDN-CDUP)/SIN(BETA)-BINSUL*TAN(ALPHA)              !!!
                D2Z= (CDDN-CDUP)/SIN(BETA)-BINFRZ*TAN(ALPHA)              !!!
            END IF                                                        !!!
 
            D3F= DEPTH-CDDN-TIRF-WEDGER-BINSUL*TAN(ALPHA)-DGSKT           !!!
            D3Z= DEPTH-CDDN-TIFF-WEDGE-BINFRZ*TAN(ALPHA)-DGSKT           !!!
 
            DCF= DEPTH-WEDGER-TIRF-TIRB-DGSKT                             !!!
            DCZ= DEPTH-WEDGE-TIFF-TIFB-DGSKT                             !!!
 
      END SELECT                                                          !!!
 
      FALPHA = 4.0*ALPHA/3.14159                                          !!!
      FBETA  = 2.0*BETA/3.14159                                           !!!
C
C               CALCULATE INTERNAL SURFACE AREAS
C          The internal area of the left (fresh food) side        - AILSDE
C          The internal area of the right (freezer) side          - AIRSDE
C          The internal area of the front or back fresh food side - AILBCK
C          The internal area of the front or back freezer side    - AIRBCK
C          The internal area of the top fresh food side           - AILTOP
C          The internal area of the bottom fresh food side        -
C            not including the compressor area since the insulation
C            may (but does not have to be) thinner there          - AILBOT
C          The internal area of the top or bottom freezer side    - AIRTOP
C          AOCOMP is the area of the compressor
C
 
C
C           Internal Areas
C
      AILTOP = WFFC*DFFC
      AIRTOP = WFRZ*DFRZ
      AILFNT = WFFC*HFFC
      AIRFNT = WFRZ*HFRZ
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AILSDE = HFFC*DFFC
            AIRSDE = HFRZ*DFRZ
            AILBCK = WFFC*HFFC
            AIRBCK = WFRZ*HFRZ
            AILBOT = AILTOP
            AIRBOT = AIRTOP
                                                                          !!!
         CASE (2)                                                         !!!
            AILSDE = DCF*H2F+(DCF+D2F)*(H1F-H2F)/2.0                            !!!
            AIRSDE = DCZ*H2Z+(DCZ+D2Z)*(H1Z-H2Z)/2.0                            !!!
            AILBCK = WFFC*H2F                                             !!!
            AIRBCK = WFRZ*H2Z                                             !!!
            AILBTM1 = WFFC*D1F                                            !!!
            AIRBTM1 = WFRZ*D1Z                                            !!!
            AILBTM2 = WFFC*D2F                                            !!!
            AIRBTM2 = WFRZ*D2Z                                            !!!
                                                                          !!!
         CASE (3)                                                         !!!
            AILSDE = DCF*H2F+CCHGT*(D3F+D3F+CCHGT*TAN(BETA))/2.0              !!!
            AIRSDE = DCZ*H2Z+CCHGT*(D3Z+D3Z+CCHGT*TAN(BETA))/2.0              !!!
            AILBCK = WFFC*H2F                                             !!!
            AIRBCK = WFRZ*H2Z                                             !!!
            AILBTM1 = WFFC*(D1F+BINSUL*TAN(ALPHA))                         !!!
            AIRBTM1 = WFRZ*(D1Z+BINFRZ*TAN(ALPHA))                         !!!
            AILBTM2 = WFFC*(D2F+BINSUL*TAN(ALPHA))                         !!!
            AIRBTM2 = WFRZ*(D2Z+BINFRZ*TAN(ALPHA))                         !!!
            AILBTM3 = WFFC*D3F                                             !!!
            AIRBTM3 = WFRZ*D3Z                                             !!!
                                                                          !!!
      END SELECT                                                          !!!
C
C               CALCULATE EXTERNAL SURFACE AREAS
C          The external area of the left (fresh food) side        - AOLSDE
C          The external area of the right (freezer) side          - AORSDE
C          The external area of the front or back fresh food side - AOLBCK
C          The external area of the front or back freezer side    - AORBCK
C          The external area of the top fresh food side           - AOLTOP
C          The external area of the bottom fresh food side        -
C            not including the compressor area since the insulation
C            may (but does not have to be) thinner there          - AOLBOT
C          The external area of the top or bottom freezer side    - AORTOP
C          AOCOMP is the area of the compressor
C
      AOLSDE = (HEIGHT-BOTTOM)*(DEPTH-WEDGER-DGSKT)                       !!!
      AORSDE = (HEIGHT-BOTTOM)*(DEPTH-WEDGE-DGSKT)                        !!!
      AOLBCK = (HEIGHT-BOTTOM)*(WALL+THMUL/2.0)
      AORBCK = (HEIGHT-BOTTOM)*(WIDTH-WALL-THMUL/2.0)
      AOLTOP = (DEPTH-WEDGER-DGSKT)*(WALL+THMUL/2.0)                      !!!
      AORTOP = (DEPTH-WEDGE-DGSKT)*(WIDTH-WALL-THMUL/2.0)                     !!!
      AOLBOT = AOLTOP
      AORBOT = AORTOP
      AOLFNT = AOLBCK
      AORFNT = AORBCK
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (2)                                                         !!!
            AOLSDE = AOLSDE - CDDN*CCHGT/2.0
            AORSDE = AORSDE - CDDN*CCHGT/2.0
            AOLBCK = AOLBCK - CCHGT*(WALL+THMUL/2.0)
            AORBCK = AORBCK - CCHGT*(WIDTH-WALL-THMUL/2.0)
            AOLBTM1 = (WALL+THMUL/2.0)*CCHGT/COS(BETA)
            AORBTM1 = (WIDTH-WALL-THMUL/2.0)*CCHGT/COS(BETA)
            AOLBTM2 = (WALL+THMUL/2.0)*(DEPTH-CDDN-WEDGER-DGSKT)
            AORBTM2 = (WIDTH-WALL-THMUL/2.0)*(DEPTH-CDDN-WEDGE-DGSKT)
 
         CASE (3)
            AOLSDE = AOLSDE - (CDDN+CDUP)*CCHGT/2.0
            AORSDE = AORSDE - (CDDN+CDUP)*CCHGT/2.0
            AOLBCK = AOLBCK - CCHGT*(WALL+THMUL/2.0)
            AORBCK = AORBCK - CCHGT*(WIDTH-WALL-THMUL/2.0)
            AOLBTM1 = (WALL+THMUL/2.0)*CDUP
            AORBTM1 = (WIDTH-WALL-THMUL/2.0)*CDUP
            AOLBTM2 = (WALL+THMUL/2.0)*CCHGT/COS(BETA)
            AORBTM2 = (WIDTH-WALL-THMUL/2.0)*CCHGT/COS(BETA)
            AOLBTM3 = (WALL+THMUL/2.0)*(DEPTH-CDDN-WEDGER-DGSKT)
            AORBTM3 = (WIDTH-WALL-THMUL/2.0)*(DEPTH-CDDN-WEDGER-DGSKT)
 
      END SELECT
C
C           Calculate the average insulation conductivity. DKINFZ
C           is the freezer door insulation. DKINFF is the fresh food door.
C           door insulation conductivity where RKINFF is the insulation
C           conductivity for the sides, back, top and bottom of the
C           fresh food compartment. RKINFZ is for the Freezer.
C
      TAVGL = 0.25*(DKINFF+RKINFF+DKINFZ+RKINFZ)
C
C          "TAVGL CALCULATION" ADDED BY A.ESPOSITO 7DEC89.
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
C           Left Side is the Fresh Food, Right Side is the Freezer
C           TIRT   is the insulation thickness on the left top (FT)
C           TIFT   is the insulation thickness on the right top (FT)
C           TIRS   is the insulation thickness on the left side (FT)
C           TIFS   is the insulation thickness on the right side (FT)
C           TIRB   is the insulation thickness on the left back (FT)
C           TIFB   is the insulation thickness on the right back (FT)
C           TIRF   is the insulation thickness on the left front (FT)
C           TIFF   is the insulation thickness on the right front (FT)
C           BINSUL is the insulation thickness on the left bottom (FT)
C           BINFRZ is the insulation thickness on the right bottom (FT)
C
C           R is the conduction resistance. It is the sum of the
C             wall resistance plus the edge resistance plus the
C             corner resistance in that order.
C
C           The side walls have two Depth and one Height length edges
C           The edge effects are divided by two since two walls share
C           each edge. The Corner effects are divided by 3 because each
C           corner shares 3 walls. (They are actually divided by 9 to
C           average the 3 insulation thicknesses).
C
C           The Left (Fresh Food) Side
C
 !    R = RKINFF*(AILSDE/TIRS + 0.54*(2.0*DFFC + HFFC)/2.0
 !   .            + 0.15*(2.0*TIRS+2.0*TIRB+BINSUL+TIRT)/9.0)
 !   .            + TAVGL*(0.54*HFFC/2.0
 !   .            + 0.15*(2.0*TIRF+2.0*TIRS+BINSUL + TIRT)/9.0)
 !    QLSIDE = (1.0/(1.0/(HO*AOLSDE) + 1.0/R
 !   .          + 1.0/(HI*AILSDE)))*(TLSIDE - TFF)
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            RR = AILSDE/TIRS + 0.54*(2.0*DFFC + 2.0*HFFC)/2.0
     .         + 0.15*((2.0*TIRS+TIRB+2.0*BINSUL+TIRF)
     .                +(2.0*TIRS+TIRB+2.0*TIRT+TIRF))/9.0
                                                                          !!!
         CASE (2)                                                         !!!
            RR = AILSDE/TIRS+0.54*(H1F+H2F+D1F+D2F+DFFC)/2.0 + 0.15               !!!
     .                  * ((2.0*BINSUL+TIRS)*FALPHA                      !!!
     .                  + (TIRB+TIRS+BINSUL)*FBETA                       !!!
     .                  + (TIRF+TIRS+BINSUL)
     .                +   (2.0*TIRS+TIRB+2.0*TIRT+TIRF))/9.0
                                                                          !!!
         CASE (3)                                                         !!!
            RR = AILSDE/TIRS+0.54*(H1F+H2F+D1F+D2F+D3F+DFFC)/2.0 + 0.15
     .                  * (2.0*(2.0*BINSUL+TIRS)*FALPHA                  !!!
     .                  + (2.0*(TIRS+BINSUL)+TIRF+TIRB)
     .                  + (2.0*TIRS+TIRB+2.0*TIRT+TIRF))/9.0
 
      END SELECT                                                          !!!
 
      R1 = 1.0/(RR*RKINFF) + 1.0/(HI*AILSDE)
      R2 = 1.0/(HO*AOLSDE)
 
      CALL RADTRN(R1, R2, TFF, TLSIDE, TRALTB, ERMLTB, EFRLTB, AORSID,
     .            QLSIDE, TRSLFT, QRDLTB, QFCLTB)
 
C
C           Calculate the heat leak out of the right (Freezer) side
C
 !    R = RKINFZ*(AIRSDE/TIFS + 0.54*(2.0*DFRZ + HFRZ)/2.0
 !   .            + 0.15*(2.0*TIFS+2.0*TIFB+BINFRZ+TIFT)/9.0)
 !   .            + TAVGL*(0.54*HFRZ/2.0
 !   .            + 0.15*(2.0*TIFS+2.0*TIRF+BINFRZ + TIFT)/9.0)           !!?
 !    QRSIDE = (1.0/(1.0/(HO*AORSDE) + 1.0/R
 !   .           + 1.0/(HI*AIRSDE)))*(TRSIDE - TFRZ)
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            RR = AIRSDE/TIFS + 0.54*(2.0*DFRZ + 2.0*HFRZ)/2.0
     .         + 0.15*((2.0*TIFS+TIFB+2.0*BINFRZ+TIFF)
     .                +(2.0*TIFS+TIFB+2.0*TIFT+TIFF))/9.0
                                                                          !!!
         CASE (2)                                                         !!!
            RR = AIRSDE/TIFS+0.54*(H1Z+H2Z+D1Z+D2Z+DFRZ)/2.0 + 0.15
     .                  * ((2.0*BINFRZ+TIFS)*FALPHA                      !!!
     .                  + (TIFB+TIFS+BINFRZ)*FBETA                       !!!
     .                  + (TIFF+TIFS+BINFRZ)
     .                  + (2.0*TIFS+TIFB+2.0*TIFT+TIFF))/9.0
                                                                          !!!
         CASE (3)                                                         !!!
            RR = AIRSDE/TIFS+0.54*(H1Z+H2Z+D1Z+D2Z+D3Z+DFRZ)/2.0 + 0.15            !!!
     .                  * (2.0*(2.0*BINFRZ+TIFS)*FALPHA                  !!!
     .                  + (2.0*(TIFS+BINFRZ)+TIFF+TIFB)
     .                  + (2.0*TIFS+TIFB+2.0*TIFT+TIFF))/9.0
 
      END SELECT                                                          !!!
 
      R1 = 1.0/(RR*RKINFZ) + 1.0/(HI*AIRSDE)
      R2 = 1.0/(HO*AORSDE)
 
      CALL RADTRN(R1, R2, TFRZ, TRSIDE, TRALTB, ERMLTB, EFRLTB, AORSID,
     .            QRSIDE, TRSLFT, QRDLTB, QFCLTB)
 
C
C           Calculate the heat leak out of the left (fresh food) top
C           The top has two Depth and one Width length edges
C
      R = RKINFF*(AILTOP/TIRT + 0.54*(DFFC + WFFC)/2.0
     .           + 0.15*(TIRS + TIRB + TIRT)/9.0)
     .           + TAVGL*(0.54*WFFC/2.0 + 0.15*(TIRF + TIRS + TIRT)/9.0)
      QLTOP = (1.0/(1.0/(HO*AOLTOP) + 1.0/R
     .          + 1.0/(HI*AILTOP)))*(TTOP - TFF)
C
C           Calculate the heat leak out of the right (freezer) top
C           The top has two Depth and one Width length edges
 
      R = RKINFZ*(AIRTOP/TIFT + 0.54*(DFRZ + WFRZ)/2.0
     .           + 0.15*(TIFS + TIFB + TIFT)/9.0)
     .           + TAVGL*(0.54*WFRZ/2.0 + 0.15*(TIFF + TIFT + TIFS)/9.0)  !!?
      QRTOP = (1.0/(1.0/(HO*AORTOP) + 1.0/R
     .         + 1.0/(HI*AIRTOP)))*(TTOP - TFRZ)
 
C
C           Calculate the heat leak out of the left (fresh food) back
C           The back has two Height and two Width length edges
C
 
  !   R = RKINFF*(AILBCK/TIRB + 0.54*(HFFC + 2.*WFFC)/2.0
  !  .            + 0.15*(2.*TIRS + 2.*TIRB + TIRT + BINSUL)/9.0)
  !   QBACKL = (1.0/(1.0/(HO*AOLBCK) + 1.0/R
  !  .          + 1.0/(HI*AILBCK)))*(TBACK - TFF)
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            R  = AILBCK/TIRB + 0.54*(HFFC + 2.0*WFFC)/2.0
     .                  + 0.15*(2.0*TIRS+2.0*TIRB+TIRT+BINSUL)/9.0
                                                                          !!!
         CASE (2)                                                         !!!
            R  = AILBCK/TIRB+0.54*(H2F+WFFC*FBETA+WFFC)/2.0
     .                  + 0.15*(TIRS+TIRB+BINSUL)*FBETA/9.0
     .                  + 0.15*(TIRS+TIRB+TIRT)/9.0
                                                                          !!!
         CASE (3)                                                         !!!
            R  = AILBCK/TIRB + 0.54*(H2F+2.0*WFFC)/2.0
     .                  + 0.15*(2.0*TIRS+2.0*TIRB+TIRT+BINSUL)/9.0
                                                                          !!!
       END SELECT                                                         !!!
 
      R1 = 1.0/(R*RKINFF) + 1.0/(HI*AILBCK)
      R2 = 1.0/(HO*AOLBCK)
 
      CALL RADTRN(R1, R2, TFF, TBACK, TRABKB, ERMBKB, EFRBKB, AORBCK,
     .            QBACKL, TRSBCK, QRDBKB, QFCBKB)
 
C
C           Calculate the heat leak out of the right (freezer) back
C           The back has two Height and two Width length edges
C
  !   R = RKINFZ*(AIRBCK/TIFB + 0.54*(HFRZ + 2.0*WFRZ)/2.0
  !  .            + 0.15*(2.0*TIFS + 2.0*TIFB + TIFT + BINFRZ)/9.0)
  !   QBACKR = (1.0/(1.0/(HO*AORBCK) + 1.0/R
  !  .          + 1.0/(HI*AIRBCK)))*(TBACK - TFRZ)
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            R  = AIRBCK/TIFB + 0.54*(HFRZ + 2.0*WFRZ)/2.0
     .                  + 0.15*(2.0*TIFS+2.0*TIFB+TIFT+BINFRZ)/9.0
                                                                          !!!
         CASE (2)                                                         !!!
            R  = AIRBCK/TIFB+0.54*(H2Z+WFRZ*FBETA+WFRZ)/2.0
     .                  + 0.15*(TIFS+TIFB+BINFRZ)*FBETA/9.0
     .                  + 0.15*(TIFS+TIFB+TIFT)/9.0
                                                                          !!!
         CASE (3)                                                         !!!
            R  = AIRBCK/TIFB + 0.54*(H2Z+2.0*WFRZ)/2.0
     .                  + 0.15*(2.0*TIFS+2.0*TIFB+TIFT+BINFRZ)/9.0
                                                                          !!!
       END SELECT                                                         !!!
 
      R1 = 1.0/(R*RKINFZ) + 1.0/(HI*AIRBCK)
      R2 = 1.0/(HO*AORBCK)
 
      CALL RADTRN(R1, R2, TFRZ, TBACK, TRABKB, ERMBKB, EFRBKB, AORBCK,
     .            QBACKR, TRSBCK, QRDBKB, QFCBKB)
 
 
C
C           Calculate the heat leak out of the left (fresh food) front
C
      R = DKINFF*(AILFNT/TIRF) + TAVGL*(0.54*(HFFC + 2.*WFFC)/2.0
     .           + 0.15*(2.0*TIRS + 2.0*TIRF + TIRT + BINSUL)/9.0)
      QFRNTL = (1.0/(1.0/(HO*AOLFNT) + 1.0/R
     .          + 1.0/(HI*AILFNT)))*(TFRONT - TFF)
C
C           Calculate the heat leak out of the right (freezer) front
C
      R = DKINFZ*(AIRFNT/TIFF) + TAVGL*(0.54*(HFRZ + 2.*WFRZ)/2.0
     .           + 0.15*(2.0*TIFS + 2.0*TIFF + TIFT + BINFRZ)/9.0)        !!?
      QFRNTR = (1.0/(1.0/(HO*AORFNT) + 1.0/R
     .          + 1.0/(HI*AIRFNT)))*(TFRONT - TFRZ)
 
C
C          Calculate the heat leak out of the left (fresh food) bottom
C
  !   R = RKINFF*(AILBOT/BINSUL + 0.54*(DFFC + WFFC)/2.0
  !  .            + 0.15*(TIRS + TIRB + BINSUL)/9.0)
  !  .            + TAVGL*(0.54*WFFC/2.0
  !  .            + 0.15*(TIRF + BINSUL + TIRS)/9.0)
  !   QLBTTM = (1.0/(1.0/(HO*AOLBOT)+1.0/R
  !  .          + 1.0/(HI*AILBOT)))*(TBTM-TFF)
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AOB = AOLBOT
            AIB = AILBOT
            R = AIB/BINSUL + 0.54*(DFFC + 2.0*WFFC)/2.0
     .      + 0.15*(2.*BINSUL+TIRB+2.*TIRS+TIRF)/9.0
            R1 = 1.0/(R*RKINFF) + 1.0/(HI*AIB)
            R2 = 1.0/(HO*AOB)
            CALL RADTRN(R1, R2, TFF, TBTM, TRADBT, ERMBOT, EFRBOT, AOB,
     .            QLBTTM, TRSBOT, QRDBOT, QFCBOT)
                                                                          !!!
         CASE (2)                                                         !!!
            RB1 = AILBTM1/BINSUL                                          !!!
     .          + 0.54*(WFFC*(FALPHA+FBETA)+D1F)/2.0                   !!!
     .          + 0.15*((TIRB+BINSUL+TIRS)*FBETA           !!!
     .          + (2.0*BINSUL+TIRS)*FALPHA)/9.0                    !!!
            R1 = 1.0/(RB1*RKINFF)+1.0/(HI*AILBTM1)                     !!!
            R2 = 1.0/(HO*AOLBTM1)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB2 = AILBTM2/BINSUL                                          !!!
     .          + 0.54*(WFFC*(1.0+FALPHA)+D2F)/2.0                     !!!
     .          + 0.15*((2.0*BINSUL+TIRS)*FALPHA                   !!!
     .          + (BINSUL+TIRF+TIRS))/9.0                  !!!
            R1 = 1.0/(RB2*RKINFF)+1.0/(HI*AILBTM2)                     !!!
            R2 = 1.0/(HO*AOLBTM2)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
            QLBTTM = QBOTTM1 + QBOTTM2                                    !!!
                                                                          !!!
         CASE (3)                                                         !!!
            RB1 = AILBTM1/BINSUL+0.54*(WFFC*(1+FALPHA)+D1F)/2.0        !!!
     .          + 0.15 * ((2.0*BINSUL+TIRS)*FALPHA                        !!!
     .          + (BINSUL+TIRB+TIRS))/9.0                  !!!
            R1 = 1.0/(RB1*RKINFF)+1.0/(HI*AILBTM1)                     !!!
            R2 = 1.0/(HO*AOLBTM1)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB2 = AILBTM2/BINSUL+0.54*(2.0*WFFC*FALPHA+D2F)/2.0        !!!
     .          + 0.15 * (4.0*BINSUL+2.0*TIRS)*FALPHA/9.0      !!!
            R1 = 1.0/(RB2*RKINFF)+1.0/(HI*AILBTM2)                     !!!
            R2 = 1.0/(HO*AOLBTM2)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB3 = AILBTM3/BINSUL+0.54*((1+FALPHA)*WFFC+D3F)/2.0        !!!
     .          + 0.15 * ((2.0*BINSUL+TIRS)*FALPHA                 !!!
     .          + (BINSUL+TIRF+TIRS))/9.0                  !!!
            R1 = 1.0/(RB3*RKINFF)+1.0/(HI*AILBTM3)                     !!!
            R2 = 1.0/(HO*AOLBTM3)                                      !!!
            CALL RADTRN(R1,R2,TFF,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM3,      !!!
     .            QBOTTM3,TRSBOT,QRDBOT,QFCBOT)                           !!!
            QLBTTM = QBOTTM1+QBOTTM2+QBOTTM3                              !!!
 
      END SELECT                                                          !!!
C
C          Calculate the heat leak out of the right (freezer) bottom
C
 !    R = RKINFZ*(AIRBOT/BINFRZ + 0.54*(DFRZ + WFRZ)/2.0
 !   .            + 0.15*(TIFS + TIFB + BINFRZ)/9.0)
 !   .            + TAVGL*(0.54*WFRZ/2.0
 !   .            + 0.15*(TIRF + BINFRZ + TIFS)/9.0)                      !!?
 !    QRBTTM = (1.0/(1.0/(HO*AORBOT)+1.0/R
 !   .          + 1.0/(HI*AIRBOT)))*(TBTM-TFRZ)
 
      SELECT CASE (NCCTYPE)                                               !!!
                                                                          !!!
         CASE (1)                                                         !!!
            AOB = AORBOT
            AIB = AIRBOT
            R = AIB/BINFRZ + 0.54*(DFRZ + 2.0*WFRZ)/2.0
     .      + 0.15*(2.*BINFRZ+TIFB+2.*TIFS+TIFF)/9.0
            R1 = 1.0/(R*RKINFZ) + 1.0/(HI*AIB)
            R2 = 1.0/(HO*AOB)
            CALL RADTRN(R1, R2, TFRZ, TBTM, TRADBT, ERMBOT, EFRBOT, AOB,
     .            QRBTTM, TRSBOT, QRDBOT, QFCBOT)
                                                                          !!!
         CASE (2)                                                         !!!
            RB1 = AIRBTM1/BINFRZ                                          !!!
     .          + 0.54*(WFRZ*(FALPHA+FBETA)+D1Z)/2.0                   !!!
     .          + 0.15*((TIFB+BINFRZ+TIFS)*FBETA           !!!
     .          + (2.0*BINFRZ+TIFS)*FALPHA)/9.0                    !!!
            R1 = 1.0/(RB1*RKINFZ)+1.0/(HI*AIRBTM1)                     !!!
            R2 = 1.0/(HO*AORBTM1)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB2 = AIRBTM2/BINFRZ                                          !!!
     .          + 0.54*(WFRZ*(1.0+FALPHA)+D2Z)/2.0                     !!!
     .          + 0.15*((2.0*BINFRZ+TIFS)*FALPHA                   !!!
     .          + (BINFRZ+TIFF+TIFS))/9.0                  !!!
            R1 = 1.0/(RB2*RKINFZ)+1.0/(HI*AIRBTM2)                     !!!
            R2 = 1.0/(HO*AORBTM2)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
            QRBTTM = QBOTTM1 + QBOTTM2                                    !!!
                                                                          !!!
         CASE (3)                                                         !!!
            RB1 = AIRBTM1/BINFRZ+0.54*(WFRZ*(1+FALPHA)+D1Z)/2.0        !!!
     .          + 0.15 * ((2.0*BINFRZ+TIFS)*FALPHA                        !!!
     .          + (BINFRZ+TIFB+TIFS))/9.0                  !!!
            R1 = 1.0/(RB1*RKINFZ)+1.0/(HI*AIRBTM1)                     !!!
            R2 = 1.0/(HO*AORBTM1)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM1,      !!!
     .            QBOTTM1,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB2 = AIRBTM2/BINFRZ+0.54*(2.0*WFRZ*FALPHA+D2Z)/2.0        !!!
     .          + 0.15 * (4.0*BINFRZ+2.0*TIFS)*FALPHA/9.0      !!!
            R1 = 1.0/(RB2*RKINFZ)+1.0/(HI*AIRBTM2)                     !!!
            R2 = 1.0/(HO*AORBTM2)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM2,      !!!
     .            QBOTTM2,TRSBOT,QRDBOT,QFCBOT)                           !!!
 
            RB3 = AIRBTM3/BINFRZ+0.54*((1+FALPHA)*WFRZ+D3Z)/2.0        !!!
     .          + 0.15 * ((2.0*BINFRZ+TIFS)*FALPHA                 !!!
     .          + (BINFRZ+TIFF+TIFS))/9.0                  !!!
            R1 = 1.0/(RB3*RKINFZ)+1.0/(HI*AIRBTM3)                     !!!
            R2 = 1.0/(HO*AORBTM3)                                      !!!
            CALL RADTRN(R1,R2,TFRZ,TBTM,TRADBT,ERMBOT,EFRBOT,AORBTM3,      !!!
     .            QBOTTM3,TRSBOT,QRDBOT,QFCBOT)                           !!!
            QRBTTM = QBOTTM1+QBOTTM2+QBOTTM3                              !!!
 
      END SELECT                                                          !!!
C
C           Calculate the heat leak through the mullion
C                AMULL is the mullion surface area on the fresh food side.
C                AMULR is the mullion surface area on the freezer side.
C
      AMULL = HFFC*DFFC
      AMULR = HFRZ*DFRZ
      QMUL = (1.0/(1.0/(HIRMUL*AMULL) + 1.0/(HIFMUL*AMULR)
     .        + THMUL/(CKMUL*AMULL)))*(TFF - TFRZ)
C
C         "Mullion heat leak" added by A.Esposito 7DEC89
C
C           Sum all the heat leaks to get the cabinet heat leak.
C
      QFFT = QLSIDE + QLTOP + QBACKL + QFRNTL + QLBTTM - QMUL
      QFRZ = QRSIDE + QRTOP + QBACKR + QFRNTR + QRBTTM + QMUL
 
C
C     CALCULATE GASKET HEAT LEAKS FOR FREEZER FAN ON AND OFF
C
      QGZN = 24.0*HLGZF*(WFRZ+HFRZ)*(TROOM-TFRZ)  !Change on 8/26/92
      QGZF = QGZN
      QGR = 24.0*HLRG*(HFFC+WFFC)*(TROOM-TFF)
C
C     CALCULATE HEAT LEAKS FOR THE WEDGE
C
      THETA = DATAN((TIFS-FLANGE)/WEDGE)
      AWEDGE = WEDGE*(TIFS/(TIFS-FLANGE))
      BWEDGE = AWEDGE - WEDGE
 
      WL1 = (HFRZ+TIFT+BINFRZ-FLGB+WFRZ+TIFS-2.0*FLANGE)
      WL2 = HFRZ+WFRZ
      QWFZC = (1.0/(1.0/(HO*WEDGE*(HEIGHT-BOTTOM+WFRZ+TIFS))
     .         + THETA/(WKIN*DLOG(AWEDGE/BWEDGE)
     .         * (WL1+WL2)/2.0)))*(TROOM-TFRZ)
 
      THETA = DATAN((BINFRZ-FLGB)/WEDGE)
      AWEDGE = WEDGE*(BINFRZ/(BINFRZ-FLGB))
      BWEDGE = AWEDGE - WEDGE
 
      QWFZB = (1.0/(1.0/(HO*WEDGE*WIDTH)
     .         + THETA/(WKIN*DLOG(AWEDGE/BWEDGE)
     .         * (WFRZ+0.5*(TIFS-FLANGE)))))*(TBTM-TFRZ)
 
      IF (WEDGER .NE. 0.0) THEN                                           !!!
         THETA = DATAN((TIRS-FLANGER)/WEDGER)                             !!!
         AWEDGE = WEDGER*(TIRS/(TIRS-FLANGER))                            !!!
         BWEDGE = AWEDGE - WEDGER                                         !!!
 
         W1 = WALL + HFFC + BINSUL - 2.0*FLANGER - FLGB                   !!!
         W2 = HFFC + WFFC
         QWFFC = (1.0/(1.0/(HO*WEDGER*(HEIGHT-BOTTOM+WALL))               !!!
     .         + THETA/(WKINr*DLOG(AWEDGE/BWEDGE)
     .         * (W1+W2)/2.0)))*(TROOM-TFF)
 
         THETA = DATAN((BINSUL-FLGB)/WEDGER)                              !!!
         AWEDGE = WEDGER*(BINSUL/(BINSUL-FLGB))                           !!!
         BWEDGE = AWEDGE - WEDGER                                         !!!
 
         QWFFB = (1.0/(1.0/(HO*WEDGER*WALL)                               !!!
     .         + THETA/(WKINr*DLOG(AWEDGE/BWEDGE)
     .         * (2.0*WFFC+TIRS-FLANGER)/2.0)))*(TBTM-TFF)                !!!
      END IF
C
C     SUM THE VARIOUS COMPONENTS OF THE HEAT LEAK
C
      QW = QWFZC + QWFZB + QWFFC + QWFFB
      QWFF = QWFFC + QWFFB
      QWFZ = QWFZC + QWFZB
      QGON = QGR + QGZN
      QGOF = QGR + QGZF
      QTON = QW + QGON + QFFT + QFRZ
      QTOF = QW + QGOF + QFFT + QFRZ
C
C           The heat leak due to Door Openings
C
      CALL DOORPN(TFF, TFRZ, HFFC, WFFC, DFFC, HFRZ, WFRZ, DFRZ,
     .            QDFFCS, QDFFFS, QDFZCS, QDFZFS,QDFFCL, QDFFFL,
     .            QDFZCL, QDFZFL)
 
      RETURN
      END
