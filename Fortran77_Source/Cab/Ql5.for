      SUBROUTINE QL5
C     ******************************************************************
C     *     CALCULATE HEAT LEAKS FOR CHEST FREEZER                     *
C     ******************************************************************
C
C
C          ___   ___________________________________   ___
C           |   |              Door                 |   |
C           |   |-----------------------------------|   |
C           |   |                                   |   |
C           |   |                                   |  H2
C           |   |             FRONT                 |   |
C           H1  |                                   |   |
C           |   |                          |- -W2- -|   |
C           |   |                            _______|   |
C           |   |                           |   \    \ ---
C           |   |- - - - - - -W1- - - - - - |\   \    \
C           |   |___________________________| \   \   FSHR
C          ---                                 \   \
C                                              FSV  \
C                                                   FSH
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / QLTF /   HIWP
      COMMON / QLFIVE / CWIDE,CHGT,SCIN,STIN,TKIN
      COMMON / QLMISC / TOPMUL,THMUL,TIFT,TIRT,CINSUL,HLRG,CKMUL,HLFZG,
     .                  DGSKT,CDUP,CDDN,CCHGT,NCCTYPE                     !!!
      COMMON / OPEND /  QDFFCS, QDFFFS, QDFZCS, QDFZFS, QDFFCL, QDFFFL,
     .                  QDFZCL, QDFZFL
C
C     Calculate internal and external dimensions
C           CWIDE .... The width  of the compressor compartment
C           CHGT ..... The height of the compressor compartment
C
         CWIDE = CWIDE + SCIN
         CHGT  = CHGT  + STIN
 
         TIFS = TIFRS
         W1 = WIDTH - CWIDE - TIFS
         W2 = CWIDE - TIFS
         H1 = HEIGHT - BINSUL - TIFT - DGSKT                              !!!
         H2 = HEIGHT - CHGT - TIFT - DGSKT                                !!!
         DC = DEPTH - TIFF - TIFB
         WC = WIDTH - TIFS - TIFS
         HC = HEIGHT - TIFT - BINSUL - DGSKT                              !!!
C
C     SEE FIGURE 1 FOR CONFIGURATION 5 DIMENSIONS
C
C     TAVGL is the average insulation conductivity for edges boardering the door
C     TAVGC is the average insulation conductivity for corners boardering the door
C
         TAVGL = (TKIN + RKIN)/2.0
         TAVGC = (2.0*RKIN+TKIN)/3.0                                      !!!
C
C     Calculate heat leaks
C
C     FRONT
C
         RFRONT = RKIN*((H1*W1+H2*W2)/TIFF+0.54*(H1+H2+W1+(W2-SCIN)
     .            + (H1-H2-STIN))/2.0
     .            + 0.15*(4.0*TIFF+2.0*TIFS+2.0*STIN+2.0*BINSUL
     .            + 2.0*SCIN)/9.0)
     .            + TAVGL*0.54*(W1+W2)/2.0
     .            + TAVGC*0.15*(2.0*TIFF+2.0*TIFS+2.0*TIFT)/9.0           !!!
C
         AIFRONT = (H1*W1) + (H2*W2)
         AOFRONT = (HEIGHT-DGSKT)*WIDTH - (CHGT-STIN)*(CWIDE-SCIN)        !!!
C
         IF(HIWP .NE. 0.0) RAWP = 1.0/(HIWP*AIFRONT)
         IF(HIWP .EQ. 0.0) RAWP = 1.0/(HI*AIFRONT)
         QFRONT = 1.0/(1.0/(HO*AOFRONT) + 1.0/RFRONT+RAWP)*(TROOM-TFRZ)
C
C     BACK
C
         RBACK = RKIN*((H1*W1+H2*W2)/TIFB + 0.54*(H1+H2+W1+(W2-SCIN)
     .            + (H1-H2-STIN))/2.0
     .            + 0.15*(4.0*TIFB+2.0*TIFS+2.0*STIN+2.0*BINSUL
     .            + 2.0*SCIN)/9.0)
     .            + TAVGL*0.54*(W1+W2)/2.0
     .            + TAVGC*0.15*(2.0*TIFB+2.0*TIFS+2.0*TIFT)/9.0           !!!
C
         AIBACK = (H1*W1) + (H2*W2)
         AOBACK = (HEIGHT-DGSKT)*WIDTH - (CHGT-STIN)*(CWIDE-SCIN)         !!!
C
         IF(HIWP .NE. 0.0) RAWP = 1.0/(HIWP*AIBACK)
         IF(HIWP .EQ. 0.0) RAWP = 1.0/(HI*AIBACK)
         QBACK = 1.0/(1.0/(HO*AOBACK) + 1.0/RBACK+RAWP)*(TROOM-TFRZ)
C
C     LEFT
C
         RLSIDE = RKIN*(DC*H1/TIFS + 0.54*(DC+2.0*H1)/2.0
     .            + 0.15*(2.0*BINSUL+2.0*TIFS+TIFF+TIFB)/9.0)             !!!
     .            + TAVGL*0.54*DC/2.0+TAVGC*0.15*(2.0*TIFT+2.0*TIFS
     .            + TIFF+TIFB)/9.0
C
         AILSIDE = H1*DC
         AOLSIDE = (HEIGHT-DGSKT)*DEPTH
C
         IF(HIWP .NE. 0.0) RAWP = 1.0/(HIWP*AILSIDE)
         IF(HIWP .EQ. 0.0) RAWP = 1.0/(HI*AILSIDE)
         QLSIDE = 1.0/(1.0/(HO*AOLSIDE) + 1.0/RLSIDE+RAWP)*
     .            (TROOM-TFRZ)
C
C     RIGHT
C
         RRSIDE = RKIN*(H2*DC/TIFS + 0.54*(2.0*H2+DC)/2.0
     .            + 0.15*(2.0*TIFS + 2.0*STIN+TIFF+TIFB)/9.0)
     .            + TAVGL*0.54*DC/2.0+TAVGC*0.15*(2.0*TIFS+2.0*TIFT+TIFF
     .            + TIFB)/9.0
C
         AIRSIDE = H2*DC
         AORSIDE = (HEIGHT-DGSKT-CHGT+STIN)*DEPTH
C
         IF(HIWP .NE. 0.0) RAWP = 1.0/(HIWP*AIRSIDE)
         IF(HIWP .EQ. 0.0) RAWP = 1.0/(HI*AIRSIDE)
         QRSIDE = 1.0/(1.0/(HO*AORSIDE) + 1.0/RRSIDE+RAWP)*
     .            (TROOM-TFRZ)
C
C     TOP
C
         RTOP = TKIN*(W1+W2)*DC/TIFT + TAVGL*0.54*(2.0*DC
     .          + 2.0*(W1+W2))/2.0
     .          + TAVGC*0.15*(2.0*TIFF+2.0*TIFB+4.0*TIFT+4.0*TIFS)/9.0    !!!
C
         AITOP = (W1+W2)*DC
         AOTOP = WIDTH*DEPTH
C
         IF(HIWP .NE. 0.0) RAWP = 1.0/(HIWP*AITOP)
         IF(HIWP .EQ. 0.0) RAWP = 1.0/(HI*AITOP)
         QTOP = 1.0/(1.0/(HO*AOTOP) + 1.0/RTOP+RAWP)*(TROOM-TFRZ)
C
C     COMPRESSOR STEP (HORIZONTAL)
C
         RSH = RKIN*((W2-SCIN)*DC/STIN+1.08*((W2-SCIN)+DC)/2.0+0.15*
     .          (4.0*STIN+2.0*TIFS+2.0*SCIN+2.0*TIFF+2.0*TIFB)/9.0)       !!!
C
         AISH = W2*DC
         AOSH = DEPTH*(CWIDE-SCIN)
C
         IF(HIWP .NE. 0.0) RAWP = 1.0/(HIWP*AISH)
         IF(HIWP .EQ. 0.0) RAWP = 1.0/(HI*AISH)
         QSH = 1.0/(1.0/(HO*AOSH) + 1.0/RSH+RAWP)*(TBTM-TFRZ)            !!!
C
C     COMPRESSOR STEP (VERTICAL)
C
         RSV = RKIN*((H1-H2-STIN)*DC/SCIN+1.08*(DC+(H1-H2-STIN))/2.0
     .         + 0.15*(4.0*SCIN+2.0*BINSUL+2.0*STIN+2.0*TIFF
     .         + 2.0*TIFB)/9.0)                                           !!!
C
         AISV = (CHGT-BINSUL)*DC                                          !!!
         AOSV = DEPTH*(CHGT-STIN)
C
         IF(HIWP .NE. 0.0) RAWP = 1.0/(HIWP*AISV)
         IF(HIWP .EQ. 0.0) RAWP = 1.0/(HI*AISV)
         QSV = 1.0/(1.0/(HO*AOSV) + 1.0/RSV+RAWP)*(TBTM-TFRZ)            !!!
C
C     HEAT LEAK THROUGH CABINET EXCLUDING THE BOTTOM
C
         QTNOBOT = QSV + QSH + QTOP + QRSIDE + QLSIDE + QBACK + QFRONT
C
C     HEAT LEAK THROUGH BOTTOM
C
         AIB1 = W1*DC
         AOB1 = DEPTH*(WIDTH-CWIDE+SCIN)                                  !!!
         RBOT = RKIN*(AIB1/BINSUL + 1.08*(W1+DC)/2.0 + 0.15*(4.0*BINSUL
     .           + 2.0*TIFF+2.0*TIFB+2.0*SCIN+2.0*TIFS)/9.0)
C
         QBOTTM = 1.0/(1.0/(HO*AOB1) + 1.0/RBOT+1.0/(HI*AIB1))*
     .             (TROOM-TFRZ)                                           !!!
C
         QBCOMP = 0
C
         QFRZ = QTNOBOT + QBOTTM + QBCOMP
C
C     CALCULATE GASKET HEAT LEAK
C
         QGZN = 24.0*HLFZG*(WIDTH+DEPTH)*(TROOM-TFRZ)
         QGZF = QGZN
C
C     SUM COMPONENTS OF HEAT LEAK
C
         QTON = QFRZ + QGZN
         QTOF = QFRZ + QGZF
C
C           The heat leak due to Door Openings
C
      CALL DOORPN(TFF, TFRZ, HC, WC, DC, HC, WC, DC,
     .            QDFFCS, QDFFFS, QDFZCS, QDFZFS,QDFFCL, QDFFFL,
     .            QDFZCL, QDFZFL)
 
      RETURN
      END
