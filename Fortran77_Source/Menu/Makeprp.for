      SUBROUTINE MAKEPRP (ILOOP)
C     ******************************************************************
C     *    SET UP THE REFRIGERANT THERMOPHYSICAL PROPERTY TABLE        *
C     ******************************************************************
C
C          ILOOP             INDICATES FIRST OR SECOND LOOP IN DUAL LOOP
C                            CIRCUIT
C
      REAL MUF, MUG, KF, KG
 
      DIMENSION X(5), XM(5), IR(5), F(5,5), XL(5), XV(5), PHI(5)
      DIMENSION WM(5), TC(5), PC(5), RC(5)
 
      COMMON /CONFIG/ icolr, iform, ifeed, iport, ignore
      COMMON /ESDATA/ COEFF(9,34),CRIT(5,34)
      COMMON /VALUES/ VAL(800),VALMN(800),VALMX(800)
C
C          SCREEN DISPLAY AND OPEN ERROR FILE
C
 
      CALL setatr(79)
      CALL window(12, 18, 16, 63, 32, 0)
      CALL window(13, 17, 20, 59, 32, 1)
 
      CALL setatr(01)                                      !Shadow
      IF(icolr .EQ. 1) THEN
        CALL window(19, 19, 18, 65, 219, 3)
        CALL window(13, 18, 64, 65, 219, 3)
      END IF
      CALL setatr(0)
      CALL setatr(79)
 
      CALL GOTOXY(28,15)
      CALL PRINT('Creating Property Table$',23,-2)
 
      OPEN (9, FILE='BLOCK2.ERR', STATUS='UNKNOWN')
C
C          INITIALIZE POINTERS TO VAL VECTOR
C
      NUM_REF = IFIX(VAL(175))
 
      DO I = 1,5
         X(I) = 0
         XM(I) = 0
         XL(I) = 0
         XV(I) = 0
         IR(I) = 0
 
         DO J = 1,5
            F(I,J) = 0
         END DO
 
      END DO
C
C          REFRIGERATION DATA: MIXTURE INTERACTION COEFFICIENTS
C
      IF(ILOOP .EQ. 1 .OR. ILOOP .EQ. 3) THEN
         IPNT = 180
         IF(NUM_REF .EQ. 2) IPNT = 320
 
      ELSE
         IPNT = 190
         IF(NUM_REF .EQ. 2) IPNT = 325
      END IF
      SELECT CASE (NUM_REF)
         CASE (1)                                          !Pure refrigerant
            IR(1) = IFIX(VAL(IPNT))
            XM(1) = 1.0
 
         CASE (2)                                          !Binary
            IR(1) = IFIX(VAL(IPNT))
            IR(2) = IFIX(VAL(IPNT+2))
            F(1,2) = VAL(IPNT+3)
            F(2,1) = F(1,2)
            XM(1) = VAL(IPNT+1)
            XM(2) = 1.0 - XM(1)
 
         CASE (3)                                          !Ternary
            IR(1) = IFIX(VAL(IPNT))
            IR(2) = IFIX(VAL(IPNT+2))
            IR(3) = IFIX(VAL(IPNT+4))
            F(1,2) = VAL(IPNT+6)
            F(1,3) = VAL(IPNT+7)
            F(2,3) = VAL(IPNT+8)
            F(2,1) = F(1,2)
            F(3,1) = F(1,3)
            F(3,2) = F(2,3)
            XM(1) = VAL(IPNT+1)
            XM(2) = VAL(IPNT+3)
            XM(3) = 1.0 - XM(1) - XM(2)
 
      END SELECT
C
C          SET UP THE PROPERTY DATA FOR THE REFRIGERANT CRITICAL
C          PARAMETERS.
C
      CALL BCONST(NUM_REF,IR,F)
 
      DO I = 1,NUM_REF
         L = IR(I)
         WM(I) = CRIT(1,L)
         TC(I) = CRIT(3,L)
         PC(I) = CRIT(4,L)
         RC(I) = 1.0/CRIT(5,L)
      END DO
C
C          CONVERT TO MOLAR COMPOSITION FOR THE CALCULATION OF
C          ALL PROPERTIES
C
      SUM = 0.0
      AMW = 0.0
 
      I = 1
      DO WHILE (I .LE. NUM_REF)
         SUM = SUM + XM(I)/WM(I)
         I = I + 1
      END DO
 
      I = 1
      DO WHILE (I .LE. NUM_REF)
         X(I) = XM(I)/WM(I)/SUM
         AMW = AMW + X(I)*WM(I)
         I = I + 1
      END DO
C
C          DETERMINE (PSEUDO) CRITICAL PROPERTIES
C
      SUM = 0
      PCRIT = 0
      RCRIT = 0
      TCRIT = 0
 
      DO I = 1,NUM_REF
         SUM = SUM + X(I)*WM(I)/RC(I)
      END DO
 
      DO I = 1,NUM_REF
         PHI(I) = (X(I)*WM(I)/RC(I))/SUM
         PCRIT = PCRIT + X(I)*PC(I)
         RCRIT = RCRIT + PHI(I)*WM(I)/RC(I)
 
         DO J = 1,NUM_REF
            TCRIT = TCRIT + X(I)*X(J)*(1.0 - F(I,J))*SQRT(TC(I)*TC(J))
         END DO
 
         TCRIT = TCRIT + X(I)*TC(I)
 
      END DO
 
      TCRIT = TCRIT/2.0
      TCRIT_F = 1.8*TCRIT - 459.67
      VCRIT = RCRIT/AMW
 
C
C          OPEN THE PROPERTY TABLE AND WRITE THE HEADING
C
      OPEN(1, FILE='REFRIG.PRP', STATUS='UNKNOWN')
      WRITE(1, '(''Refrigerant Property Table''//)')
      WRITE(1, 900)
C
C          FILL THE TABLE, STARTING AT -20F
C
      TF = -40.0
      TMAX = TCRIT_F - 10.0
      DO WHILE (TF .LE. TMAX)
         CALL MUKCP(AMW, TCRIT, PCRIT, TF, X, PPSIA, MUF, MUG, KF, KG,
     .              CPF, CPG, HF, HG, VVF, VLF)
         WRITE(1,901) TF, PPSIA, 1.0/VLF, VVF, HF, HG, MUF, MUG, CPF,
     .                CPG, KF, KG
         TF = TF + 10.0
      END DO
 
      CLOSE (1)
      CLOSE (9)
 
      CALL GOTOXY(28,15)
      CALL LIGHT(50,2)
 
      CALL GOTOXY(27,15)
      CALL ATRBUT(25,79)
 
      RETURN
C
C          OUTPUT FORMAT
C
  900 FORMAT('   TSAT   PSAT  RHOF    VG      HF    HG      MUF  MUG',
     .       '    CPF  CPG    KF    KG'/
     .       '   DEG F  PSIA LB/CUFT CUFT/LB   BTU/LBM      LBM/FT-HR',
     .       '   BTU/LBM-F  BTU/HR-FT-F'/
     .       '1     BEGIN INPUT DATA')
  901 FORMAT(F5.0, F7.2, F7.2, F7.3, 2F7.2, F6.3, F7.3, 2F6.3, 2F7.4)
      END
