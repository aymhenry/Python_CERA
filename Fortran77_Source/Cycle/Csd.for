      BLOCK DATA BDESC
C
C   THIS ROUTINE INITIALIZES THE COMMON BLOCKS CONTAINING INFORMATION
C   ABOUT THE PURE COMPONENTS.  IT IS NOT REFERENCED DIRECTLY BY ANY
C   OTHER SUBROUTINE BUT MUST BE INCLUDED IN THE EXECUTABLE ELEMENT.
C   DATA ARRAYS ARE DIMENSIONED TO ACCOMODATE ADDITIONAL
C   PURE COMPONENTS.
C
C   EXPLANATION OF CONSTANTS:
C      COEFF(I,J) - FOR REFRIGERANT J, COEFFICIENTS OF A, B, CP0
C         CURVE FITS:
C         A = A0 * EXP(A1*T + A2*T*T)  (KJ M**3/KMOL**2)
C         B = B0 + B1*T + B2*T*T  (M**3/KMOL)
C         CP0 = C0 + C1*T + C2*T*T (KJ/KMOL K)
C         (STORED IN ORDER A0,A1,A2,B0,B1,B2,C0,C1,C2)
C      CRIT(I,J) - FOLLOWING INFORMATION FOR REFRIGERANT J:
C         I = 1 - MOLECULAR WEIGHT
C             2 - REFERENCE TEMPERATURE FOR ENTHALPY AND ENTROPY (K)
C             3 - CRITICAL TEMPERATURE (K)
C             4 - CRITICAL PRESSURE (KPA)
C             5 - CRITICAL VOLUME (M**3/KMOL)
C      HREF(J) - REFRIGERANT NAME (ASHRAE DESIGNATION)
C      HZERO(J) - VALUE OF SATURATED LIQUID ENTHALPY OF REFRIGERANT
C         J AT ITS REFERENCE TEMPERATURE (KJ/KMOL)
C      SZERO(J) - VALUE OF SATURATED LIQUID ENTROPY AT REFERENCE
C         TEMPERATURE (KJ/KMOL K)
C      R - GAS CONSTANT (KJ/KMOL K)
C      TOLR - RELATIVE CONVERGENCE TOLERANCE FOR ITERATION LOOPS
C         SHOULD BE AT LEAST 10 TIMES LARGER THAN MACHINE PRECISION
C      ITMAX - MAXIMUM ITERATION COUNT FOR ITERATIVE LOOPS
C      LUP - LOGICAL UNIT TO WHICH ANY WARNING MESSAGES ARE WRITTEN
C
C   A, B COEFFFICIENTS EVALUATED FROM ASHRAE (1981) SATURATION
C   DATA UNLESS INDICATED.
C
C   DATA VALUES UPDATED ON 3/15/94 TO BE CONSISTENT WITH REFPROP 4.0
C
C   REFRIGERANTS NOW ARE:
C       1: R11     2: R12     3: R13     4: n-C5     5: R14     6: R22
C       7: R23     8: R113    9: R114   10: R142B   11: R152A  12: R216A
C      13: R125   14: R143A  15: R134A  16: R123    17: RC318  18: R134
C      19: RC270  20: R141B  21: i-C5   22: R290    23: R600   24: R600A
C      25: R32    26: R1270  27: R124   28: R115    29: CE-216 30: E-125
C      31: R123A  32: R143   33: R218   34: E134
C
C  NOTE: REFPROP 4.0 ALLOWS USE OF A MODIFIED B-W-R EQUATION OF STATE FOR
C        R32, R123, R124, R125, AND R134A AS PURE FULIDS.  THE CSD EQUATION
C        OF STATE IS USED BY REFPROP FOR MIXTURES.
C
C  NOTE: THE FOLLOWING REFRIGERANTS WERE FIT WITH THE OLDER REFPROP (3.0)
C        COEFFICIENTS (BECAUSE OF A BETTER MATCH TO ASHRAE DATA OVER THE
C        -20 F TO + 130 F TEMPERATURE RANGE): R114
C
C  NOTE: THE COEFFICIENTS FOR R12 ARE THE SAME AS USED IN REFPROP 3.O TO
C        PROVIDE CONSISTENCY WITH EARLIER ANALYSES.  THE REFPROP 4.0
C        COEFFICIENTS CHANGE THE ENERGY BY ABOUT 0.001 KWH/DAY.  IT IS NOT
C        WORTH EXPLAINING AWAY DIFFERENCES BETWEEN THE RESULTS IN THE ERA
C        DOCUMENTATION AND A REVISED ERA USING THE REFPROP 4.O COEFFICIENTS
C        FOR R12.
 
      IMPLICIT REAL (A-H,O-Z)
      DIMENSION COEFF(9,34),CRIT(5,34),HZERO(34),SZERO(34)
      CHARACTER*6 HREF(34),REFH(34)
      COMMON /ESDATA/ COEFF,CRIT
      COMMON /HREF1/ HREF,REFH
      COMMON /HSZERO/ HZERO,SZERO
      COMMON /RDATA4/ R
      COMMON /TOL/ TOLR,ITMAX,LUP
      COMMON /TOLSH/ TOLH,TOLS
      DATA R /8.314/
      DATA TOLR /1.0E-7/
      DATA TOLH,TOLS /0.01,0.001/
      DATA ITMAX,LUP /20,9/
C
C   R11, TRICHLOROFLUOROMETHANE (CFCL3)
C
      DATA HREF(1) /'R11'/
      DATA REFH(1) /'   R11'/
      DATA (CRIT(I,1), I=1,5) / 137.37, 296.91, 471.2, 4467.0, 0.247 /
      DATA HZERO(1),SZERO(1)  / 0.0, 0.0/
      DATA (COEFF(I,1), I=1,9) / 4967.07, -2.23098E-3,-5.59203E-7,
     .                          0.178148, -1.82363E-4,-2.54131E-8,
     .                           23.4805,  0.251072,  -2.28722E-4 /
C
C  R12, DICHLORODIFLUOROMETHANE (CF2CL2)
C
      DATA HREF(2) /'R12'/
      DATA REFH(2) /'   R12'/
      DATA (CRIT(I,2), I=1,5) / 120.91, 243.39, 384.95, 4180.0, 0.181 /
      DATA HZERO(2),SZERO(2)  / 0.0, 0.0/
      DATA (COEFF(I,2), I=1,9) / 3819.88, -3.31988E-3,  2.41944E-7,
     .                          0.165350, -2.65758E-4,  9.13878E-8,
     .                           18.4874,    0.241782, -2.04594E-4 /
C
C   R13, CHLOROTRIFLUOROMETHANE (CF3CL)
C
      DATA HREF(3) /'R13'/
      DATA REFH(3) /'   R13'/
      DATA (CRIT(I,3), I=1,5) / 104.46, 191.67, 302.0, 3870.0, 0.181 /
      DATA HZERO(3),SZERO(3)  / 0.0, 0.0/
      DATA (COEFF(I,3), I=1,9) / 2157.20, -2.84478E-3, -2.75122E-6,
     .                          0.129485, -1.93746E-4, -9.01119E-8,
     .                           13.8691,    0.232370, -1.83095E-4 /
C
C  n-C5, n-Pentane (C5H12)
C
      DATA HREF(4)/'n-C5 '/
      DATA REFH(4)/'  n-C5'/
      DATA (CRIT(I,4), I=1,5) / 72.15, 309.34, 469.5, 3359.9, 0.295 /
      DATA HZERO(4),SZERO(4)  / 0.0, 0.0/
      DATA (COEFF(I,4), I=1,9) / 6745.80, -2.29793E-3, -0.70747E-6,
     .                          0.228716, -2.36350E-4, -0.32793E-7,
     .                           54.7577,    0.143042,  2.53720E-4 /
 
!!!!Data from Bill Koplo (8/6/92)
 
!!!   DATA (CRIT(I,4), I=1,5) / 72.15, 233.16, 469.5, 3359.9, 0.295 /
!!!   DATA HZERO(4),SZERO(4)  / -12388.16, -45.772 /
!!!   DATA (COEFF(I,4), I=1,9) / 6339.56, -1.97165E-3, -1.12780E-6,
!!!  .                          0.208196, -1.24833E-4, -1.82407E-7,
!!!  .                           54.7577,    0.143042,  2.53720E-4 /
 
 
C
C   R14, TETRAFLUOROMETHANE (CF4)
C
      DATA HREF(5) /'R14'/
      DATA REFH(5) /'   R14'/
      DATA (CRIT(I,5), I=1,5) / 88.00, 145.17, 227.5, 3795.0, 0.141 /
      DATA HZERO(5),SZERO(5)  / 0.0, 0.0/
      DATA (COEFF(I,5), I=1,9) / 1272.41, -3.42946E-3, -6.47573E-6,
     .                          0.099664, -1.57113E-4, -2.95020E-7,
     .                           14.4296,    0.184530, -9.51890E-5 /
C
C   R22, CHLORODIFLUOROMETHANE (CHF2CL)
C
      DATA HREF(6) /'R22'/
      DATA REFH(6) /'   R22'/
      DATA (CRIT(I,6), I=1,5) / 86.47, 232.29, 369.3, 5054.0, 0.169 /
      DATA HZERO(6),SZERO(6)  / 0.0, 0.0/
      DATA (COEFF(I,6), I=1,9) / 2624.62, -2.67304E-3, -1.33238E-6,
     .                          0.117395, -1.40272E-4, -0.52161E-7,
     .                           21.9839,    0.127744, -4.78872E-5 /
C
C   R23, TRIFLUOROMETHANE (CHF3)
C
      DATA HREF(7) /'R23'/
      DATA REFH(7) /'   R23'/
      DATA (CRIT(I,7), I=1,5) / 70.01, 191.12, 299.1, 4900.0, 0.133 /
      DATA HZERO(7),SZERO(7)  / 0.0,0.0/
      DATA (COEFF(I,7), I=1,9) / 1743.89, -3.52595E-3, -1.12774E-6,
     .                          0.090205, -1.25602E-4, -0.50675E-7,
     .                           23.6029,    0.082287,  3.18265E-5 /
C
C   R113, 1,1,2-TRICHLOROTRIFLUOROETHANE (CF2CL-CFCL2)
C
      DATA HREF(8) /'R113'/
      DATA REFH(8) /'  R113'/
      DATA (CRIT(I,8), I=1,5) / 187.38, 320.80, 487.5, 3456.0, 0.329 /
      DATA HZERO(8),SZERO(8)  / 0.0,0.0/
      DATA (COEFF(I,8), I=1,9) / 7284.48, -2.15870E-3, -8.03754E-7,
     .                          0.234712, -2.11131E-4, -7.33758E-8,
     .                           76.2637,    0.119641,  7.18786E-5 /
C
C   R114, 1,2-DICHLOROTETRAFLUOROETHANE  (CF2CL-CF2CL): Version 3.0 Refprop
C
      DATA HREF(9) /'R114'/
      DATA REFH(9) /'  R114'/
      DATA (CRIT(I,9),I=1,5) / 170.92, 276.80, 418.80, 3248.0, 0.307 /
      DATA HZERO(9),SZERO(9) / 0.0, 0.0/
      DATA (COEFF(I,9),I=1,9) / 5929.74, -2.86018E-3, -4.81520E-7,
     .                         0.221874, -2.88304E-4,  1.81892E-8,
     .                          37.2482,    0.337339, -2.39995E-4 /
 
C
C   R142B, 1-CHLORO-1,1-DIFLUOROETHANE (CF2CL-CH3)
C
      DATA HREF(10) /'R142B'/
      DATA REFH(10) /' R142B'/
      DATA (CRIT(I,10), I=1,5) / 100.49, 264.01, 410.3, 4120.0, 0.231 /
      DATA HZERO(10),SZERO(10) / 0.0, 0.0/
      DATA (COEFF(I,10),I=1,9) / 4180.75, -2.73043E-3, -5.43638E-7,
     .                          0.169138, -2.41068E-4,  0.67566E-7,
     .                           16.3914,    0.271719, -1.58933E-4 /
C
C   R152A, 1,1-DIFLUOROETHANE  (CHF2-CH3)
C
      DATA HREF(11) /'R152A'/
      DATA REFH(11) /' R152A'/
      DATA (CRIT(I,11), I=1,5) / 66.05, 248.50, 386.7, 4492.0, 0.181 /
      DATA HZERO(11),SZERO(11) / 0.0, 0.0/
      DATA (COEFF(I,11),I=1,9) / 3198.63, -2.96134E-3,  -0.32190E-6,
     .                          0.133264, -2.03633E-4,  0.777251E-7,
     .                           22.2832,    0.153987, -3.015434E-6 /
C
C   R216A, 1,3-DICHLOROHEXAFLUOROPROPANE [NOT IN REFPROP4.0]
C
      DATA HREF(12) /'R216A'/
      DATA REFH(12) /' R216A'/
      DATA (CRIT(I,12),I=1,5) / 220.93, 233.15, 453.14, 2754.1, 0.3847 /
      DATA HZERO(12),SZERO(12) / 0.0, 0.0/
      DATA (COEFF(I,12),I=1,9) / 8431.44, -2.45916E-3, -9.91754E-7,
     .                          0.265720, -2.20418E-4, -1.68111E-7,
     .                           8.79769,    0.654246, -5.39923E-4 /
C
C  R125, PENTAFLUOROETHANE (C2HF5)
C
      DATA HREF(13) /'R125'/
      DATA REFH(13) /'  R125'/
      DATA (CRIT(I,13), I=1,5) / 120.03, 224.6, 339.4, 3629.0, 0.2099 /
      DATA HZERO(13),SZERO(13) / 0.0, 0.0/
      DATA (COEFF(I,13),I=1,9) / 3427.92, -3.17461E-3, -1.75729E-6,
     .                          0.149380, -1.80851E-4, -1.18813E-7,
     .                          22.65024,    0.295668, -1.69490E-4 /
C
C   R143A, 1,1,1-TRIFLUOROETHANE   (CF3-CH3)
C
      DATA HREF(14) /'R143A'/
      DATA REFH(14) /' R143A'/
      DATA (CRIT(I,14), I=1,5) / 84.04, 225.8, 346.3, 3811., 0.194/
      DATA HZERO(14),SZERO(14) / 0.0, 0.0/
      DATA (COEFF(I,14),I=1,9) / 2763.90920, -2.509056E-3, -1.797108E-6,
     .                           0.133153E0, -1.589538E-4, -0.583311E-7,
     .                           13.89426E0,     .2554913, -1.300829E-4/
C
C
C   R134A:  1,1,1,2-TETRAFLUOROETHANE  (CF3-CH2F)
C
      DATA HREF(15) /'R134a'/
      DATA REFH(15) /' R134a'/
      DATA (CRIT(I,15), I=1,5) / 102.030, 247.0, 374.3, 4067.0, 0.199 /
      DATA HZERO(15),SZERO(15) / 0.0, 0.0/
      DATA (COEFF(I,15),I=1,9) / 3582.17, -2.81114E-3, -1.44679E-6,
     .                          0.141750, -1.62763E-4, -.628933E-7,
     .                           19.4006,    0.258531, -1.29665E-4 /
C
C   R123, 1,1-DICHLORO-2,2,2-TRIFLUOROETHANE (CHCL2-CF3)
C
      DATA HREF(16) /'R123'/
      DATA REFH(16) /'  R123'/
      DATA (CRIT(I,16), I=1,5) / 152.93, 301.02, 456.9, 3674.0, 0.278 /
      DATA HZERO(16),SZERO(16) / 0.0, 0.0/
      DATA (COEFF(I,16),I=1,9) / 6033.29, -2.37891E-3, -0.84728E-6,
     .                          0.199549, -1.89493E-4, -0.67680E-7,
     .                           29.2604,    0.302994, -1.92907E-4 /
 
C
C  RC318, PERFLUOROCYCLOBUTANE (C4F8)
C
      DATA HREF(17) /'RC-318'/
      DATA REFH(17) /'RC-318'/
      DATA (CRIT(I,17), I=1,5) / 200.04, 266.1, 388.4, 2778., 0.3248 /
      DATA HZERO(17),SZERO(17) / 0.0, 0.0/
      DATA (COEFF(I,17),I=1,9) / 6182.614E0, -2.536687E-3, -2.265766E-6,
     .                           .2255416E0, -1.898425E-4, -2.635465E-7,
     .                          28.972075E0,     .5333363, -3.557726E-4/
C
C  R134, 1,1,2,2-TETRAFLOUROETHANE (CHF2-CHF2)
C
      DATA HREF(18) /'R134'/
      DATA REFH(18) /'  R134'/
      DATA (CRIT(I,18), I=1,5) / 102.03, 253.34, 392.1, 4562.0, 0.189 /
      DATA HZERO(18),SZERO(18) / 0.0, 0.0/
      DATA (COEFF(I,18),I=1,9) / 3547.10, -2.68720E-3, -1.41578E-6,
     .                           0.13856,  -1.5991E-4, -0.55880E-7,
     .                           32.5208,    0.222819, -1.06829E-4 /
C
C   RC270, CYCLOPROPANE (C3H6)
C
      DATA HREF(19) /'RC270'/
      DATA REFH(19) /' RC270'/
      DATA (CRIT(I,19), I=1,5) / 42.081, 240.25, 398.30, 5580.0, 0.194 /
      DATA HZERO(19),SZERO(19) / 0.0, 0.0/
      DATA (COEFF(I,19),I=1,9) / 2745.00, -2.98122E-3,  1.64391E-7,
     .                          0.125065, -2.01031E-4,   7.8506E-8,
     .                          8.19470,     0.136885, 0.777583E-4 /
C
C  R141b,  1,1-DICHLORO-1-FLUOROETHANE (CFCL2-CH3)
C
      DATA HREF(20) /'R141B'/
      DATA REFH(20) /' R141b'/
      DATA (CRIT(I,20), I=1,5) / 116.94, 305.35, 477.3, 4120., 0.217 /
      DATA HZERO(20),SZERO(20) / 0.0, 0.0/
      DATA (COEFF(I,20),I=1,9) / 5422.38, -2.24167E-3, -6.04435E-7,
     .                          0.180853, -1.61856E-4, -6.23542E-8,
     .                           35.8434, 0.175268,     0.0 /
C
C  i-C5 ISO-PENTANE (C4H9-CH3)
C
      DATA HREF(21) /'i-C5'/
      DATA REFH(21) /'  i-C5'/
      DATA (CRIT(I,21), I=1,5) / 72.150, 300.9, 460.51, 3370.7, 0.306 /
      DATA HZERO(21),SZERO(21) / 0.0, 0.0/
      DATA (COEFF(I,21),I=1,9) / 6408.1, -2.3216E-3, -0.7087E-6,
     .                         0.227727, -2.4414E-4, -2.9694E-8,
     .                           12.216,    0.37563, -5.9925E-5 /
C
C  R290,  PROPANE (C3H8)
C
      DATA HREF(22) /'R290'/
      DATA REFH(22) /'  R290'/
      DATA (CRIT(I,22), I=1,5) / 44.10, 231.1, 369.85, 4247.7, 0.220 /
      DATA HZERO(22),SZERO(22) / 0.0, 0.0/
      DATA (COEFF(I,22),I=1,9)/ 2988.28, -2.62902E-3, -1.09706E-6,
     .                         0.142963, -1.76519E-4, -5.78514E-8,
     .                         26.88900,   0.1250300,  1.07890E-4 /
C
C  R600,  N-BUTANE (C4H10)
C
      DATA HREF(23) /'R600'/
      DATA REFH(23) /'  R600'/
      DATA (CRIT(I,23), I=1,5) / 58.124, 272.6, 425.16, 3796., 0.2548 /
      DATA HZERO(23),SZERO(23) / 0.0, 0.0/
      DATA (COEFF(I,23),I=1,9) / 4822.7, -2.6499E-3, -0.4397E-6,
     .                           0.1908, -2.4836E-4,  0.2846E-7,
     .                            9.442,     0.3317, -1.1297E-4 /
C
C  R600a,  ISOBUTANE [C(CH3)3]
C
      DATA HREF(24) /'R600a'/
      DATA REFH(24) /' R600a'/
      DATA (CRIT(I,24), I=1,5) / 58.124, 261.39, 407.9, 3630.6, 0.256 /
      DATA HZERO(24),SZERO(24) / 0.0, 0.0/
      DATA (COEFF(I,24),I=1,9) / 4197.24, -2.1894E-3, -1.3004E-6,
     .                            0.1803, -1.8719E-4, -8.1778E-8,
     .                           27.6833,   0.199384, 1.06305E-4 /
C
C   R32:  DIFLUOROMETHANE (CH2F2)
C
      DATA HREF(25) /'R32'/
      DATA REFH(25) /'   R32'/
      DATA (CRIT(I,25), I=1,5) / 52.024, 221.40, 351.36, 5791.0, .120 /
      DATA HZERO(25),SZERO(25) / 0.0, 0.0/
      DATA (COEFF(I,25),I=1,9) / 1662.27, -2.19753E-3, -1.88903E-6,
     .                         0.0779879, -0.75238E-4, -0.53011E-7,
     .                           29.2127,   0.0192902,  8.91429E-5 /
C
C  R1270,  PROPYLENE (C3H6)
C
      DATA HREF(26) /'R1270'/
      DATA REFH(26) /' R1270'/
      DATA (CRIT(I,26), I=1,5) / 42.09, 255.46, 364.9, 4621.7, 0.1937 /
      DATA HZERO(26),SZERO(26) / -8695.95, 170.53 /
      DATA (COEFF(I,26),I=1,9) / 2294.38, -1.57422E-03, -2.98847E-06,
     .                          .1253157, -1.28616E-04, -1.09990E-07,
     .                           3.856,     0.2321, -1.0308E-4 /
 
C
C  R124, 1-CHLORO-1,2,2,2-TETRAFLOUROETHANE (C2HF5)
C
      DATA HREF(27) /'R124'/
      DATA REFH(27) /'  R124'/
      DATA (CRIT(I,27), I=1,5) / 136.48, 259.96, 395.62, 3637., 0.244 /
      DATA HZERO(27),SZERO(27) / 0.0, 0.0/
      DATA (COEFF(I,27),I=1,9) / 4504.401, -2.574376E-3,  -1.4705E-6,
     .                           0.173954,  -1.79579E-4, -1.04407E-7,
     .                            30.9777,     0.254206, -9.36414E-5 /
C
C   R115, CHLOROPENTAFLOUROETHANE  (CF2CL-CF3)
C
      DATA HREF(28) /'R115'/
      DATA REFH(28) /'  R115'/
      DATA (CRIT(I,28), I=1,5) / 154.47, 233.98, 353.05, 3153.0, 0.252 /
      DATA HZERO(28),SZERO(28) / 0.0, 0.0/
      DATA (COEFF(I,28),I=1,9) / 3968.734, -2.471498E-3, -2.656280E-6,
     .                           .1817131, -1.797986E-4, -2.305032E-7,
     .                            20.0246,  .3765849E0,  -2.703487E-4 /
C
C CE-216, FROM JIM SANDS OF ORNL RECEIVED 12/23/91
C
      DATA HREF(29) /'CE-216'/
      DATA REFH(29) /'CE-216'/
      DATA (CRIT(I,29), I=1,5) / 166.02, 233.15, 361.8, 3094.0, 0.272 /
      DATA HZERO(29),SZERO(29) / 0.0, 0.0/
      DATA (COEFF(I,29),I=1,9) / 3808.5,  -0.0017285, -3.81991E-6,
     .                       0.16412557, -6.60150E-5, -3.83529E-7,
     .                      -52.9448624,   0.6902447, -0.0006871 /
C
C E-125, FROM CYNTHIA GAGE 2-11-93
C
      DATA HREF(30) /'E-125'/
      DATA REFH(30) /' E-125'/
      DATA (CRIT(I,30), I=1,5) / 136.02, 238.55, 353.8, 3330.0, 0.2385 /
      DATA HZERO(30),SZERO(30) / 0.0, 0.0/
      DATA (COEFF(I,30),I=1,9) / 3112.3, -0.0013240, -4.48727E-6,
     .                       0.15782070, -0.0001235, -2.51097E-7,
     .                       31.5556400,  0.3137960, -0.0001836 /
C
C   R123a 1,2-DICHLORO-1,1,2-TRIFLUOROETHANE
C
      DATA HREF(31) /'R213A'/
      DATA REFH(31) /' R123A'/
      DATA (CRIT(I,31), I=1,5) / 152.93, 303.2, 461.1, 3741.0, 0.2812 /
      DATA HZERO(31),SZERO(31) / 0.0, 0.0/
      DATA (COEFF(I,31),I=1,9) / 6376.995, -2.691077E-3, -2.524465E-7,
     .                           .2016864, -2.035804E-4, -3.644260E-8,
     .                           48.23970,     .1856480,          0.0 /
C
C   R143, 1,1,2-TRIFLUOROETHANE (CF2H-CFH2)
C
      DATA HREF(32) /'R143'/
      DATA REFH(32) /'  R143'/
      DATA (CRIT(I,32), I=1,5) /  84.04, 277.2, 429.9, 4520.0, 0.190 /
      DATA HZERO(32),SZERO(32) / 0.0, 0.0/
      DATA (COEFF(I,32),I=1,9) / 3680.023, -2.4128619E-3, -1.183791E-6,
     .                           .1221286, -8.9741778E-5, -1.068718E-7,
     .                            24.9639,       .187598, -4.031996E-5 /
C
C   R218, PERFLUOROPROPANE (C3F8)
C
      DATA HREF(33) /'R218'/
      DATA REFH(33) /'  R218'/
      DATA (CRIT(I,33), I=1,5) / 188.03, 236.4, 345.1, 2680.1, 0.2994 /
      DATA HZERO(33),SZERO(33) / 0.0, 0.0/
      DATA (COEFF(I,33),I=1,9) / 4486.64, -1.952581E-3, -4.49894E-6,
     .                           .205911, -1.493288E-4, -4.30009E-7,
     .                           23.2683,      .536728, -3.97647E-4 /
C
C  E134, BIS(DIFLUOROMETHYL) (CHF2-O-CHF2)
C
      DATA HREF(34) /'E134'/
      DATA REFH(34) /'  E134'/
      DATA (CRIT(I,34), I=1,5) / 118.03, 279.3, 420.3, 4228.0, 0.224 /
      DATA HZERO(34),SZERO(34) / 0.0, 0.0/
      DATA (COEFF(I,34),I=1,9) / 6016.695, -4.051717E-3, 8.906450E-7,
     .                           .1718950, -2.308880E-4, 2.837796E-8,
     .                           -26.7633,     .6152671, -6.58095D-4 /
      END
 
