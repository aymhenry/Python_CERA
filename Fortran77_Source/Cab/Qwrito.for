$DEBUG
      SUBROUTINE QWRITO
C     ******************************************************************
C     *     PRINT THE RESULTS OF THE CALCULATIONS                      *
C     ******************************************************************
 
      IMPLICIT REAL*8(A-H,O-Z)
 
$INCLUDE:'COMMON.FOR'
 
      COMMON / ERA /    IRFTYP, FZHEAT, FFHEAT, WATERZ, WATERF, HXVUR,
     .                  HXVUZ
      COMMON / OPEND /  QDFFCS, QDFFFS, QDFZCS, QDFZFS, QDFFCL, QDFFFL,
     .                  QDFZCL, QDFZFL
      COMMON / PENAT /  FFPENA, FZPENA
C
C          UNIT CONVERSION FUNCTIONS
C
      F1(T) = (T - 32.0)/1.8                 !F to C
      F6(Q) = Q/3.413
C
C           Total Door Opening Heat Leaks
C
      QSDRFF = QDFFCS
      QLDRFF = QDFFCL
      QFDRFF = QDFFFL - QDFFCL
 
      QSDRFZ = QDFZFS
      QLDRFZ = QDFZCL
      QFDRFZ = QDFZFL - QDFZCL
 
      ZERO = 0.0
C
C          OUTPUT CABINET LOADS ANALYSIS
C
      SELECT CASE (NMOD)
         CASE (3)                                          !(1) Top Mount R/F
            QMULN = -QMULI
            QFFTOT = QRRSID + QRLSID + QRBACK + QRFRNT + QBOTTM +
     .               QMULN  + QWFF   + QGR    + QSDRFF + QFDRFF +
     .               QLDRFF + FFASHQ + FFHEAT + FFREFQ + FFPENA
 
            QFZTOT = QFRSID + QFLSID + QFBACK + QFFRNT + QTOP +
     .               QMULI  + QWFZ   + QGZF   + QSDRFZ + QLDRFZ +
     .               QFDRFZ + FZASHQ + FZHEAT + FZREFQ + FZPENA
 
            QHTFF = FFASHQ + FFHEAT
            QHTFZ = FZASHQ + FZHEAT
 
            WRITE(IO,900)
     .         F6(QRRSID),  F6(QFRSID),
     .         F6(QRLSID),  F6(QFLSID),
     .         F6(QRBACK),  F6(QFBACK),
     .         F6(QRFRNT),  F6(QFFRNT),
     .         F6(ZERO),    F6(QTOP),
     .         F6(QBOTTM),  F6(ZERO),
     .         F6(QMULN),   F6(QMULI),
     .         F6(QWFF),    F6(QWFZ),
     .         F6(QGR),     F6(QGZF),
     .         F6(QSDRFF),  F6(QSDRFZ),
     .         F6(QLDRFF),  F6(QLDRFZ),
     .         F6(QFDRFF),  F6(QFDRFZ),
     .         F6(FFASHQ),  F6(FZASHQ),
     .         F6(FFREFQ),  F6(FZREFQ),
     .         F6(FFPENA),  F6(FZPENA),
     .         F6(FFHEAT),  F6(FZHEAT),
     .         F6(QFFTOT),  F6(QFZTOT)
 
         CASE (2)                                          !(3) Side by Side R/F
 
            QMULN = -QMUL
            QFFTOT = QLSIDE + QBACKL + QFRNTL + QLTOP  + QLBTTM +
     .               QMULN  + QWFF   + QGR    + QSDRFF + QLDRFF +
     .               QFDRFF + FFASHQ + FFHEAT + FFREFQ + FFPENA
 
            QFZTOT = QRSIDE + QBACKR + QFRNTR + QRTOP  + QRBTTM +
     .               QMUL   + QWFZ   + QGZF   + QSDRFZ + QLDRFZ +
     .               QFDRFZ + FZASHQ + FZHEAT + FZREFQ + FZPENA
 
            QHTFF = FFASHQ + FFHEAT
            QHTFZ = FZASHQ + FZHEAT
 
            WRITE(IO,900)
     .         F6(ZERO),   F6(QRSIDE),
     .         F6(QLSIDE), F6(ZERO),
     .         F6(QBACKL), F6(QBACKR),
     .         F6(QFRNTL), F6(QFRNTR),
     .         F6(QLTOP),  F6(QRTOP),
     .         F6(QLBTTM), F6(QRBTTM),
     .         F6(QMULN),  F6(QMUL),
     .         F6(QWFF),   F6(QWFZ),
     .         F6(QGR),    F6(QGZF),
     .         F6(QSDRFF), F6(QSDRFZ),
     .         F6(QLDRFF), F6(QLDRFZ),
     .         F6(QFDRFF), F6(QFDRFZ),
     .         F6(FFASHQ), F6(FZASHQ),
     .         F6(FFREFQ), F6(FZREFQ),
     .         F6(FFPENA), F6(FZPENA),
     .         F6(FFHEAT), F6(FZHEAT),
     .         F6(QFFTOT), F6(QFZTOT)
 
         CASE (5)                                          !(4) Chest Freezer
            QBTTM  = QBOTTM + QSH    + QSV
            QFZTOT = QRSIDE + QLSIDE + QBACK  + QFRONT + QTOP +
     .               QBTTM  + QGZF   + QSDRFZ + QLDRFZ + QFDRFZ +
     .               FZASHQ + FZHEAT + FZREFQ + FZPENA
 
            QHTFZ  = FZASHQ + FZHEAT
 
            WRITE(IO,901) F6(QRSIDE), F6(QLSIDE), F6(QBACK),
     .                    F6(QFRONT), F6(QTOP),   F6(QBTTM),
     .                    F6(QGZF),   F6(QSDRFZ), F6(QLDRFZ),
     .                    F6(QFDRFZ), F6(FZASHQ), F6(FZREFQ),
     .                    F6(FZPENA), F6(FZHEAT), F6(QFZTOT)
 
         CASE (4, 7)                                       !(5) Upright Freezer
            QFZTOT = QRSIDE + QLSIDE + QBACK  + QFRONT + QTOP +
     .               QBOTTM + QW     + QGZF   + QSDRFZ + QLDRFZ +
     .               QFDRFZ + FZASHQ + FZHEAT + FZREFQ + FZPENA
 
            QHTFZ  = FZASHQ + FZHEAT
 
            WRITE(IO,902) F6(QRSIDE), F6(QLSIDE), F6(QBACK),
     .                    F6(QFRONT), F6(QTOP),   F6(QBOTTM),
     .                    F6(QW),     F6(QGZF),   F6(QSDRFZ),
     .                    F6(QLDRFZ), F6(QFDRFZ), F6(FZASHQ),
     .                    F6(FZREFQ), F6(FZPENA), F6(FZHEAT),
     .                    F6(QFZTOT)
 
         CASE (8)                                          !(2) Bottom Mount R/F
            QMULN = -QMUL
            QFFTOT = QRRSID + QRLSID + QRBACK + QRFRNT + QTOP +
     .               QMULN  + QWFF   + QGR    + QSDRFF + QLDRFF +
     .               QFDRFF + FFASHQ + FFHEAT + FFREFQ + FFPENA
 
            QFZTOT = QFRSID + QFLSID + QFBACK + QFFRNT + QBOTTM +
     .               QBCOMP + QMUL   + QWFZ   + QGZF   + QSDRFZ +
     .               QLDRFZ + QFDRFZ + FZASHQ + FZPENA + FZHEAT +
     .               FZREFQ
 
            QHTFF = FFASHQ + FFHEAT
            QHTFZ = FZASHQ + FZHEAT
 
            WRITE(IO,900)
     .         F6(QRRSID),  F6(QFRSID),
     .         F6(QRLSID),  F6(QFLSID),
     .         F6(QRBACK),  F6(QFBACK),
     .         F6(QRFRNT),  F6(QFFRNT),
     .         F6(QTOP),    F6(ZERO),
     .         F6(ZERO),    F6(QBOTTM),
     .         F6(QMULN),   F6(QMUL),
     .         F6(QWFF),    F6(QWFZ),
     .         F6(QGR),     F6(QGZF),
     .         F6(QSDRFF),  F6(QSDRFZ),
     .         F6(QLDRFF),  F6(QLDRFZ),
     .         F6(QFDRFF),  F6(QFDRFZ),
     .         F6(FFASHQ),  F6(FZASHQ),
     .         F6(FFREFQ),  F6(FZREFQ),
     .         F6(FFPENA),  F6(FZPENA),
     .         F6(FFHEAT),  F6(FZHEAT),
     .         F6(QFFTOT),  F6(QFZTOT)
 
      END SELECT
C
C          OUTPUT DATA TO CYCLE PROGRAM
C
      IF(ICYCL .EQ. 4) THEN
         SELECT CASE (NMOD)
            CASE (2, 3, 8)
               WRITE(ICYCL,800) F6(QFFTOT), F6(QFZTOT), F6(QSDRFF),
     .                          F6(QLDRFF), F6(QFDRFF), F6(QSDRFZ),
     .                          F6(QLDRFZ), F6(QFDRFZ), F6(FFPENA),
     .                          F6(FZPENA), F6(QHTFF),  F6(QHTFZ),
     .                          F6(FFREFQ), F6(FZREFQ), F6(-QMULN)
 
            CASE (4, 5, 7)
               WRITE(ICYCL,800) F6(QFZTOT), F6(QFZTOT), F6(QSDRFZ),
     .                          F6(QLDRFZ), F6(QFDRFZ), F6(QSDRFZ),
     .                          F6(QLDRFZ), F6(QFDRFZ), F6(FZPENA),
     .                          F6(FZPENA), F6(QHTFZ),  F6(QHTFZ),
     .                          F6(FZREFQ), F6(FZREFQ), 0.0
 
         END SELECT
 
         CLOSE(ICYCL)
      END IF
 
      RETURN
C
C     FORMAT STATEMENTS
C
  800 FORMAT(F10.2,T19,'FRESH FOOD NET LOAD (W)'/
     .       F10.2,T19,'FREEZER LOAD (W)'/
     .       F10.2,T19,'FRESH FOOD DOOR SENSIBLE LOAD (W)'/
     .       F10.2,T19,'FRESH FOOD DOOR CONDENSATION LOAD (W)'/
     .       F10.2,T19,'FRESH FOOD DOOR FROST LOAD (W)'/
     .       F10.2,T19,'FREEZER DOOR SENSIBLE LOAD (W)'/
     .       F10.2,T19,'FREEZER DOOR CONDENSATION LOAD (W)'/
     .       F10.2,T19,'FREEZER DOOR FROST LOAD (W)'/
     .       F10.2,T19,'FRESH FOOD PENETRATIONS (W)'/
     .       F10.2,T19,'FREEZER PENETRATIONS (W)'/
     .       F10.2,T19,'FRESH FOOD HEATERS AND CONTROLS (W)'/
     .       F10.2,T19,'FREEZER HEATERS AND CONTROLS (W)'/
     .       F10.2,T19,'FRESH FOOD REFRIGERANT LINE (W)'/
     .       F10.2,T19,'FREEZER REFRIGERANT LINE (W)'/
     .       F10.2,T19,'MULLION HEAT LOAD (W)')
 
  900 FORMAT(//T10,'HEAT LEAK BREAKDOWN'/
     .       T49,'FRESH FOOD',T70,'FREEZER'/
     .       T49,'   (W)   ', T70,'  (W)'/
     .       T20,'RIGHT WALL',              T45,F10.2,T65,F10.2/
     .       T20,'LEFT WALL',               T45,F10.2,T65,F10.2/
     .       T20,'BACK WALL',               T45,F10.2,T65,F10.2/
     .       T20,'FRONT (DOOR)',            T45,F10.2,T65,F10.2/
     .       T20,'TOP WALL',                T45,F10.2,T65,F10.2/
     .       T20,'BOTTOM',                  T45,F10.2,T65,F10.2/
     .       T20,'MULLION ',                T45,F10.2,T65,F10.2/
     .       T20,'WEDGE',                   T45,F10.2,T65,F10.2/
     .       T20,'GASKET',                  T45,F10.2,T65,F10.2/
     .       T20,'OPEN DOOR SENSIBLE LOAD ',T45,F10.2,T65,F10.2/
     .       T20,'MOISTURE LOAD - CONDENSE',T45,F10.2,T65,F10.2/
     .       T20,'MOISTURE LOAD - FROST',   T45,F10.2,T65,F10.2/
     .       T20,'ANTI-SWEAT HEATER',       T45,F10.2,T65,F10.2/
     .       T20,'REFRIGERANT LINE HEAT',   T45,F10.2,T65,F10.2/
     .       T20,'PENETRATIONS',            T45,F10.2,T65,F10.2/
     .       T20,'OTHER THERMAL INPUT',     T45,F10.2,T65,F10.2/
     .       T18,'TOTAL ',                  T45,F10.2,T65,F10.2)
 
  901 FORMAT(//T10,'HEAT LEAK BREAKDOWN'/
     .       T49,'FREEZER'/
     .       T49,'  (W)'/
     .       T20,'RIGHT WALL',              T45,F10.2/
     .       T20,'LEFT WALL',               T45,F10.2/
     .       T20,'BACK WALL',               T45,F10.2/
     .       T20,'FRONT WALL',              T45,F10.2/
     .       T20,'TOP (DOOR)',              T45,F10.2/
     .       T20,'BOTTOM',                  T45,F10.2/
     .       T20,'GASKET',                  T45,F10.2/
     .       T20,'OPEN DOOR SENSIBLE LOAD ',T45,F10.2/
     .       T20,'MOISTURE LOAD - CONDENSE',T45,F10.2/
     .       T20,'MOISTURE LOAD - FROST',   T45,F10.2/
     .       T20,'ANTI-SWEAT HEATER',       T45,F10.2/
     .       T20,'REFRIGERANT LINE HEAT',   T45,F10.2/
     .       T20,'PENETRATIONS',            T45,F10.2/
     .       T20,'OTHER THERMAL INPUT',     T45,F10.2/
     .       T18,'TOTAL',                   T45,F10.2)
 
  902 FORMAT(//T10,'HEAT LEAK BREAKDOWN'/
     .       T49,'  (W)'/
     .       T20,'RIGHT WALL',              T45,F10.2/
     .       T20,'LEFT WALL',               T45,F10.2/
     .       T20,'BACK WALL',               T45,F10.2/
     .       T20,'FRONT (DOOR)',            T45,F10.2/
     .       T20,'TOP WALL',                T45,F10.2/
     .       T20,'BOTTOM',                  T45,F10.2/
     .       T20,'WEDGE',                   T45,F10.2/
     .       T20,'GASKET',                  T45,F10.2/
     .       T20,'OPEN DOOR SENSIBLE LOAD ',T45,F10.2/
     .       T20,'MOISTURE LOAD - CONDENSE',T45,F10.2/
     .       T20,'MOISTURE LOAD - FROST',   T45,F10.2/
     .       T20,'ANTI-SWEAT HEATER',       T45,F10.2/
     .       T20,'REFRIGERANT LINE HEAT',   T45,F10.2/
     .       T20,'PENETRATIONS',            T45,F10.2/
     .       T20,'OTHER THERMAL INPUT',     T45,F10.2/
     .       T18,'TOTAL',                   T45,F10.2)
      END
