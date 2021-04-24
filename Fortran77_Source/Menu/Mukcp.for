      SUBROUTINE MUKCP(AMW, TCRIT, PCRIT, TF, X, PPSIA, MUF, MUG, KF,
     .                 KG, CPF, CPG, HF, HG, VVF, VLF)
C     ******************************************************************
C     *     THERMOPHYSICAL PROPERTIES OF REFRIGERANT MIXTURE           *
C     ******************************************************************
C
C      AMW      INPUT.      MOLECULAR WEIGHT OF REFRIGERANT
C      TCRIT    INPUT.      CRITICAL TEMPERATURE (K)
C      PCRIT    INPUT.      CRITICAL PRESSURE (KPA)
C      TF       INPUT.      TEMPERATURE (F)
C      X        INPUT       MOLAR COMPOSITION
C
C      PPSIA    OUTPUT.     PRESSURE (PSIA)
C      MUF      OUTPUT.     LIQUID VISCOSITY (LBM/HR-FT)
C      MUG      OUTPUT.     VAPOR VISCOSITY (LBM/HR-FT)
C      KF       OUTPUT.     LIQUID THERMAL CONDUCTIVITY (BTU/HR-FT-F)
C      KG       OUTPUT.     VAPOR THERMAL CONDUCTIVITY (BTU/HR-FT-F)
C      CPF      OUTPUT.     LIQUID SPECIFIC HEAT (BTU/LBM-F)
C      CPG      OUTPUT.     VAPOR SPECIFIC HEAT (BTU/LBM-F)
C      HF       OUTPUT.     SATURATION LIQUID ENTHALPY (BTU/LB)
C      HG       OUTPUT.     SATURATION VAPOR  ENTHALPY (BTU/LB)
C      VVF      OUTPUT      VAPOR SPECIFIC VOLUME (FT3/LBM)
C      VLF      OUTPUT      LIQUID SPECIFIC VOLUME (FT3/LBM)
C
      LOGICAL         lcrit
      REAL            muf, mug, kf, kg
 
      DIMENSION       X(5), XL(5), XV(5)
 
      DATA            R /8.314/
C
C          FIND THE AVERAGE PRESSURE LEVEL
C
      tk = (tf + 459.67)/1.8
      CALL bublt (tk, X, XV, pbub, vl, vv, .TRUE., lcrit)
      CALL bublt (tk, XL, X, pdew, vl, vv, .FALSE., lcrit)
      p = (pbub + pdew)/2.0
      ppsia = p/6.895
      amwliq = amw
      amwvap = amw
C
C          DETERMININE CORRESPONDING DEW AND BUBBLE POINT TEMPERATURES
C
      CALL bublp (p, X, XV, tbub, vl, vv, .TRUE., lcrit)
      CALL bublp (p, XL, X, tdew, vl, vv, .FALSE., lcrit)
C
C          FIND THE CRITICAL PARAMETERS FOR THE LIQUID AND VAPOR PHASES
C
      CALL espar(0,tk,X,a,b)
 
      tcvap = tcrit
      pcvap = (pcrit/6.895)/14.696
      tcliq = tcrit
      pcliq = (pcrit/6.895)/14.696
C
C          FIND VAPOR AND LIQUID SPECIFIC VOLUMES
C
      CALL espar(0,tdew,X,a,b)
      vv = R*tdew/p
      CALL vit(tdew,p,a,b,vv,.FALSE.,lcrit)
 
      CALL espar(0,tbub,X,a,b)
      vl = 0.8*b
      CALL vit(tbub,p,a,b,vl,.TRUE.,lcrit)
 
      vvf = 35.31*vv/(2.203*amw)
      vlf = 35.31*vl/(2.203*amw)
 
C
C          USE HCVCPS TO DETERMINE THE LIQUID AND VAPOR SPECIFIC HEATS
C
      CALL hcvcps(3,tdew,vv,X,HG,cvg,cpg,vs)
      CALL hcvcps(3,tbub,vl,X,hf,cvl,cpf,vs)
 
      cpg = cpg/(2.203*amw)/1.8/1.055
      cpf = cpf/(2.203*amw)/1.8/1.055
 
      hg =  hg/(2.203*amw)/1.055
      hf =  hf/(2.203*amw)/1.055
C
C          ESTIMATE LIQUID VISCOSITY.  FIRST FIND NORMAL BOILING POINT
C
      pnorm = 6.895*14.696
      CALL bublp(pnorm,X,XV,tb_bub,vln,vvn,.TRUE.,lcrit)
      CALL bublp(pnorm,XL,X,tb_dew,vln,vvn,.FALSE.,lcrit)
      tb = (tb_bub + tb_dew)/2.0
      trb = tb/tcliq
 
      omega = 3.0*trb*ALOG10(pcliq)/(1.0-trb)/7.0 - 1.0
      etac = SQRT(amwliq)*(pcliq**0.6667)/(tcliq**0.1667)
      tr = tk/tcliq
      muf = 2.41909*etac*(0.015174 - 0.02135*tr + 0.0075*tr**2
     .      + omega*(0.042552 - 0.07674*tr + 0.0340*tr**2))
      muf = muf/(0.891667 - 0.000833*amwliq)
      corr = 1.0/(35.895 - 209.79*tr + 459.31*tr**2 - 434.692*tr**3
     .     +150.42*tr**4)
      muf = corr*muf
C
C          ESTIMATE VAPOR VISCOSITY
C
      etac = 8.46682E-04*SQRT(amwvap)*(pcvap**0.6667)/(tcvap**0.1667)
      tr = tk/tcvap
      power = 0.965
      IF(tr .GT. 1.) power = 0.71 + 0.29/tr
      mug = etac*tr**power
      corr = -0.016667 + 0.000444*amwvap
      mug = mug/(1.0+corr)
C
C          ESTIMATE LIQUID THERMAL CONDUCTIVITY
C
      tr = tk/tcliq
      kf = 0.639/SQRT(amwliq)*(3.0 + 20.0*(1.0-tr)**0.6667)/(3.0
     .     + 20.0*(1.0-trb)**0.6667)
      kf = kf/(1.725 - 6.25/SQRT(amwliq))
      corr = -11.059 + 59.21*tr - 118.554*tr**2 + 105.384*tr**3
     .     -35.144*tr**4
      kf = kf/(1.0 + corr)
C
C          ESTIMATE VAPOR THERMAL CONDUCTIVITY
C
      kg = mug*(4.0/amwvap + cpg)
      corr = -0.016 -0.00030*amwvap
      kg = kg/(1.0 + corr)
      corr = -0.84532 + 5.3081*tr - 10.4319*tr**2 + 8.3475*tr**3
     .     - 2.3780*tr**4
      kg = kg/(1.0 + corr)
 
      RETURN
      END
