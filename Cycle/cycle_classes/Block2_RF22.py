# Python import
import math, sys, datetime

# User import
from Data import Data
from Block2 import Block2

# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
def main ():
	# prepare for test Block2 functions
	obj_block2  =  Block2()
	
	#---------------------------------------------------------------
	Title ("Preoare for testing")
	print ("	 USE RF22 -- for tessting -=-= obj_block2.bconst (NC, IR, FIN)")
	code = 6
	NC = 1
	IR = [0.0, code, 0,0,0,0] 
	
	# Binary interaction parameter (BIP) 
	FIN = [[0.0] * (5+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
	obj_block2.bconst (NC, IR, FIN)
	
	print ("\n	 ---- Input ---")
	print ("		IR       = ", IR)
	print ("		NC       = ", NC)
	print ("		FIN       = ", FIN)
	
	MOLAR_WEIGHT = obj_block2.WM [NC]
	
	print ("\n	 ---- Output --")
	print ("		many, but i need Molare weight g/mole      = ", MOLAR_WEIGHT)
	
	#---------------------------------------------------------------
	Title ("	[A1, B1] = obj_block2.espar (IQ,T,X1)")
	T=273+20
	X1 =  [0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
	IQ = 2
	
	print ("\n	 ---- Input ---")
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	print ("		IQ = ", IQ)
	
	[A1, B1] = obj_block2.espar (IQ,T,X1)	# 2 i.e. calc evry thing
	print ("\n	 ---- Output --")
	print ("		A1 = %5.3f"  % (A1) )
	print ("		B1 = %5.3f"  % (B1) )
	
	print ("\n		Data.DADT  = %15.6f"  % (Data.DADT) )
	print ("		Data.DBDT  = %15.6f"  % (Data.DBDT) )
	print ("		Data.D2ADT = %15.6f"  % (Data.D2ADT) )
	print ("		Data.D2BDT = ", Data.D2BDT)
	
	print ("\n		Data.HP = ", Data.HP)
	print ("		Data.HR = ", Data.HR)
	print ("		Data.SP = ", Data.SP)

	#---------------------------------------------------------------
	Title ("	[VLOW, VUP, PLOW, PUP] = obj_block2.plimit (T, A1, B1)")
	
	print ("\n	 ---- Input ---")
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	print ("		A1 = %5.3f"  % (A1) )
	print ("		B1 = %5.3f"  % (B1) )

	[VLOW, VUP, PLOW, PUP] = obj_block2.plimit (T, A1, B1)
	
	print ("\n	 ---- Output --")
	showVolum (VLOW, MOLAR_WEIGHT, "VLOW")
	showVolum (VUP,  MOLAR_WEIGHT, "VUP ")
	
	print ("\n		PLOW(KPa)  = %5.3f"  % (PLOW) )
	print ("		PUP (KPa)  = %5.3f"  % (PUP) )

	#---------------------------------------------------------------
	Title ("	[VL, LV1CON] = obj_block2.vit (T, P, A1, B1, VL_estimate, True) ")
	
	P = 1000 #Kpa #556.42
	T = 273.11 + 00
	R= 8.3
	[A1, B1] = obj_block2.espar (1,T,X1)
	VV_estimate= R* T/P
	VL_estimate= 0.8*B1
	
	
	print ("\n	 ---- Input ---")
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	print ("		P (KPa)  = %5.3f"  % (P) )
	print ("		espar used to get the related A & B")
	showVolum (VL_estimate,  MOLAR_WEIGHT, "VL_estimate ")
	
	[VL, LV1CON] = obj_block2.vit (T, P, A1, B1, VL_estimate, True)
	
	print ("\n	 ---- Output --")
	showVolum (VL,  MOLAR_WEIGHT, "VL ")
	print ("		Status LV1CON=",LV1CON)

	#---------------------------------------------------------------
	Title ("	[VL, LV1CON] = obj_block2.vit (T, P, A1, B1, VV_estimate, False) ")
	
	P = 0.2*1000#Kpa #556.42
	T = 273.11 + 60
	R= 8.3
	[A1, B1] = obj_block2.espar (1,T,X1)
	VV_estimate= R* T/P
	VL_estimate= 0.8*B1
		
	print ("\n	 ---- Input ---")
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	print ("		P (KPa)  = %5.3f"  % (P) )
	print ("		espar used to get the related A & B")
	showVolum (VV_estimate,  MOLAR_WEIGHT, "VL_estimate ")
	
	[VL, LV1CON] = obj_block2.vit (T, P, A1, B1, VV_estimate, False)
	
	print ("\n	 ---- Output --")
	showVolum (VL,  MOLAR_WEIGHT, "VL ")
	print ("		Status LV1CON=",LV1CON)
	
	#---------------------------------------------------------------
	Title ("	[ XL, XV, P, VL, VV, LCRIT] = obj_block2.bublt ( T, XL_in, XV_in, True )")
	XL_in =  [0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
	XV_in = None
	T = 273.11 + 20
	print ("\n	 ---- Input ---")
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	print ("		XL_in         = ", XL_in)
	print ("		XV_in        = ", XV_in)
	
	[ XL, XV, P, VL, VV, LCRIT] = obj_block2.bublt ( T, XL_in, XV_in, True )
	
	print ("\n	 ---- Output --")
	print ("		XL  = ", XL)
	print ("		XV  = ", XV)
	print ("		P (KPa)  = %5.3f"  % (P) )
	showVolum (VL,  MOLAR_WEIGHT, "VL ")
	showVolum (VV,  MOLAR_WEIGHT, "VV ")
	print ("		Status LCRIT=",LCRIT)
	
	#---------------------------------------------------------------
	Title ("	[ XL, XV, P, VL, VV, LCRIT] = obj_block2.bublt ( T, XL_in, XV_in, False )")
	XL_in =  None
	XV_in = [0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
	T = 273.11 + 20
	print ("\n	 ---- Input ---")
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	print ("		XL_in        = ", XL_in)
	print ("		XV_in        = ", XV_in)
	
	[ XL, XV, P, VL, VV, LCRIT] = obj_block2.bublt ( T, XL_in, XV_in, False )
	
	print ("\n	 ---- Output --")
	print ("		XL  = ", XL)
	print ("		XV  = ", XV)
	print ("		P (KPa)  = %5.3f"  % (P) )
	showVolum (VL,  MOLAR_WEIGHT, "VL ")
	showVolum (VV,  MOLAR_WEIGHT, "VV ")
	print ("		Status LCRIT=",LCRIT)
	
	#---------------------------------------------------------------
	Title ("	[ XL, XV, T, VL, VV, LCRIT] = obj_block2.bublp ( P, XL_in, XV_in, True )")
	XL_in = [0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
	XV_in =  None
	#P = 250
	print ("\n	 ---- Input ---")
	print ("		P (KPa)  = %5.3f"  % (P) )
	print ("		XL_in         = ", XL_in)
	print ("		XV_in        = ", XV_in)
	
	[ XL, XV, T, VL, VV, LCRIT] = obj_block2.bublp ( P, XL_in, XV_in, True )
	
	print ("\n	 ---- Output --")
	print ("		XL  = ", XL)
	print ("		XV  = ", XV)
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	showVolum (VL,  MOLAR_WEIGHT, "VL ")
	showVolum (VV,  MOLAR_WEIGHT, "VV ")
	print ("		Status LCRIT=",LCRIT)
	
	#---------------------------------------------------------------
	Title ("	[ XL, XV, T, VL, VV, LCRIT] = obj_block2.bublp ( P, XL_in, XV_in, False )")
	XL_in =  None
	XV_in = [0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
	
	#P = 250
	print ("\n	 ---- Input ---")
	print ("		P (KPa)  = %5.3f"  % (P) )
	print ("		XL_in         = ", XL_in)
	print ("		XV_in        = ", XV_in)
	
	[ XL, XV, T, VL, VV, LCRIT] = obj_block2.bublp ( P, XL_in, XV_in, False )
	
	print ("\n	 ---- Output --")
	print ("		XL  = ", XL)
	print ("		XV  = ", XV)
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	showVolum (VL,  MOLAR_WEIGHT, "VL ")
	showVolum (VV,  MOLAR_WEIGHT, "VV ")
	print ("		Status LCRIT=",LCRIT)
	
	#---------------------------------------------------------------
	Title ("	[h, CV, CP, VS] = obj_block2.hcvcps ( IQ, T, VL, X)")
	
	R22_TABLE_READING = 200.0 - 49.477871426920025 	# kj/kg   200-app result at 0C need to be 200, valid only for RF22
	R22_TABLE_READING_S = 1 - 0.158	 	# kj/kg.K 1-app result at 0C need to be 1.0, valid only for RF22
	
	X = [0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
	IQ = 3
	T = 273.11 + 0.0
	
	VL_kg_per_m3 = 1221.5
	VL_kgmole_per_m3 = VL_kg_per_m3/MOLAR_WEIGHT 
	VL = MOLAR_WEIGHT /VL_kg_per_m3 # _m3_per_kmole
	
	print ("\n	 ---- Input ---")
	print ("		IQ  = ", IQ)
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	showVolum (VL,  MOLAR_WEIGHT, "VL ")
	print ("		X  = ", X)
	
	[H, CV, CP, VS] = obj_block2.hcvcps ( IQ, T, VL, X)
	
	print ("\n	 ---- Output ===========-")
	print ("		H in kJ/k-mole= %5.3f,  kJ/kg=%5.3f,  Reading in ASHRAE kJ/kg = %5.3f" %(H, H/MOLAR_WEIGHT, H/MOLAR_WEIGHT + R22_TABLE_READING) )
	print ("		CV in kJ/k-mole K = %5.3f,  kJ/kg K=%5.3f " %(CV, CV/MOLAR_WEIGHT) )
	print ("		CP in kJ/k-mole K= %5.3f,   kJ/kg K=%5.3f " %(CP, CP/MOLAR_WEIGHT) )
	print ("		CP/CV =%5.6f " %(CP/CV) )
	print ("		VS =%5.6f " %(VS) )
	print ("		H",H/MOLAR_WEIGHT)
	#---------------------------------------------------------------
	Title ("	S = obj_block2.entrop (T, VV_m3_per_kmole ,X)")
	
	T = 273.11 + 0.0
	VV_kg_per_m3 = 1396.1
	VV_kgmole_per_m3 = VV_kg_per_m3/MOLAR_WEIGHT 
	VV_m3_per_kmole = MOLAR_WEIGHT /VV_kg_per_m3
	
	print ("\n	 ---- Input ---")
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	showVolum (VV_m3_per_kmole,  MOLAR_WEIGHT, "VV_m3_per_kmole ")
	print ("		X  = ", X)
	
	S = obj_block2.entrop (T,VV_m3_per_kmole,X)
	
	print ("\n	 ---- Output --")
	print ("		S in kJ/k-mole K= %5.3f,  kJ/kg K=%5.3f,  Reading in ASHRAE kJ/kg K= %5.3f" %(S, S/MOLAR_WEIGHT, S/MOLAR_WEIGHT + R22_TABLE_READING_S) )
		
	#---------------------------------------------------------------
	Title ("	S = obj_block2.entrop (T, VV_m3_per_kmole ,X)")
	
	T = 273.11 + 28.0
	VV_kg_per_m3 = 1300.1
	VV_kgmole_per_m3 = VV_kg_per_m3/MOLAR_WEIGHT 
	VV_m3_per_kmole = MOLAR_WEIGHT /VV_kg_per_m3
	
	print ("\n	 ---- Input ---")
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	showVolum (VV_m3_per_kmole,  MOLAR_WEIGHT, "VV_m3_per_kmole ")
	print ("		X  = ", X)
	
	S = obj_block2.entrop (T,VV_m3_per_kmole,X)
	
	print ("\n	 ---- Output --")
	print ("		S in kJ/k-mole K= %5.3f,  kJ/kg K=%5.3f,  Reading in ASHRAE kJ/kg K= %5.3f" %(S, S/MOLAR_WEIGHT, S/MOLAR_WEIGHT + R22_TABLE_READING_S) )
	
	#---------------------------------------------------------------
	Title ("	[TC, PC, VC] = obj_block2.critx(X, T)")
	
	print ("\n	 ---- Input ---")
	print ("		Temperature (K) T = %5.3f,  T(C)= %5.3f"  % (T, T- 273.11) )
	print ("		X  = ", X)
	
	[TC, PC, VC] = obj_block2.critx(X, T)
	
	print ("\n	 ---- Output --")
	print ("		Temperature (K) TC = %5.3f,  T(C)= %5.3f"  % (TC, TC- 273.11) )
	print ("		PC (KPa)  = %5.3f"  % (PC) )
	showVolum (VC,  MOLAR_WEIGHT, "VC ")
		
	#---------------------------------------------------------------
	Title ("	[T, XQ,XL,XV,  VL,VV,  HL,HV] = obj_block2.hpin ( Happ,P,X )")
	Hbook =  248.0425  #related to XQ=.25%
	P = 448.95 # related to 12C
	Happ =  (Hbook - R22_TABLE_READING)*MOLAR_WEIGHT
	
	print ("\n	 ---- Input ---")
	print ("		H in kJ/k-mole= %5.3f,  kJ/kg=%5.3f,  Reading in ASHRAE kJ/kg = %5.3f" %(H, H/MOLAR_WEIGHT, H/MOLAR_WEIGHT + R22_TABLE_READING) )
	print ("		P (KPa)  = %5.3f"  % (P) )
	print ("		X  = ", X)
	
	[T, XQ,XL,XV,  VL,VV,  HL,HV] = obj_block2.hpin ( Happ,P,X )
	
	print ("\n	 ---- Output --")
	print ("		Temperature (K) TC = %5.3f,  T(C)= %5.3f"  % (TC, TC- 273.11) )
	print ("		XQ  = ", XQ)
	print ("		XL  = ", XL)
	print ("		XV  = ", XV)
	showVolum (VL,  MOLAR_WEIGHT, "VL ")
	showVolum (VV,  MOLAR_WEIGHT, "VV ")
	
	print ("		HL in kJ/k-mole= %5.3f,  kJ/kg=%5.3f,  Reading in ASHRAE kJ/kg = %5.3f" %(HL, HL/MOLAR_WEIGHT, HL/MOLAR_WEIGHT + R22_TABLE_READING) )
	print ("		HV in kJ/k-mole= %5.3f,  kJ/kg=%5.3f,  Reading in ASHRAE kJ/kg = %5.3f" %(HV, HV/MOLAR_WEIGHT, HV/MOLAR_WEIGHT + R22_TABLE_READING) )

	#---------------------------------------------------------------
	Title ("	[T, XQ,XL,XV,  VL,VV,  HL,HV] = obj_block2.spin ( S,P,X )")
	Sbook =  1.168975  #related to XQ=.25%
	P = 448.95 # related to 12C
	S =  (Sbook - R22_TABLE_READING_S)*MOLAR_WEIGHT
	
	print ("\n	 ---- Input ---")
	print ("		S in kJ/k-mole K= %5.3f,  kJ/kg K=%5.3f,  Reading in ASHRAE kJ/kg K= %5.3f" %(S, S/MOLAR_WEIGHT, S/MOLAR_WEIGHT + R22_TABLE_READING_S) )
	print ("		P (KPa)  = %5.3f"  % (P) )
	print ("		X  = ", X)
	
	[T, XQ,XL,XV,  VL,VV,  SL,SV] = obj_block2.spin ( S,P,X )
	
	print ("\n	 ---- Output --")
	print ("		Temperature (K) TC = %5.3f,  T(C)= %5.3f"  % (TC, TC- 273.11) )
	print ("		XQ  = ", XQ)
	print ("		XL  = ", XL)
	print ("		XV  = ", XV)
	showVolum (VL,  MOLAR_WEIGHT, "VL ")
	showVolum (VV,  MOLAR_WEIGHT, "VV ")
	
	print ("		SL in kJ/k-mole K= %5.3f,  kJ/kg K=%5.3f,  Reading in ASHRAE kJ/kg K= %5.3f" %(SL, SL/MOLAR_WEIGHT, SL/MOLAR_WEIGHT + R22_TABLE_READING_S) )
	print ("		SV in kJ/k-mole K= %5.3f,  kJ/kg K=%5.3f,  Reading in ASHRAE kJ/kg K= %5.3f" %(SV, SV/MOLAR_WEIGHT, SV/MOLAR_WEIGHT + R22_TABLE_READING_S) )
	

	return

# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
def bublt_test():
	objUtil  =  Block2()
	T=234.39
	
	P = 172.8
	X  = [0.0, 1.0, 0, 0, 0]
	XL = [0.0, 1.0, 0, 0, 0]
	XV = [0.0, 1.0, 0, 0, 0]
	
	FIN = [[0.0] * (3+1) for i in range(3+1)]
	objUtil.bconst (1, [0,2,0,0], FIN) # R11
	
	#X = [0.0, 1, 0,0]
	#[P5, P7] = objUtil.vit (T, P, objUtil.A , P4, P5, P6)	
	
	[TC, PC, VC] = objUtil.critx(X, T)
	print (" K  critx		TC=",TC)
	print (" KPA	  		PC=",PC)
	print (" M**3/KMOL 		VC=",VC)
	print (" M**3/Kg		VC=",VC/120.91)
	print ("		NC=", objUtil.NC)
	print ("		A=", objUtil.AP)
	print ("		B=", objUtil.BP)
	
	T=256.776 #259.776 #273.11 -11.334 # ref temp
	
	print ("input ====================objUtil.bublt (P, XL, XV, True)")

	print ("		K T= K, C", T, T-273.11)
	print ("		XL=", XL)
	print ("		XV=", XV)
	
	[XL, XV, P, VL, VV, _] = objUtil.bublt (T, XL, XV, True)
	
	print ("\nOutput =========objUtil.bublt (T, XL, XV, True)===========")
	print ("		XL=", XL)
	print ("		XV=", XV)
	print ("	kPa  P=", P)
	print ("		VL=", VL)
	
	print ("	m3/kmol	VL=", VL)
	print ("	m3/kg	VL=", VL/120.91)
	print ("---------------")


	XL_inp = [0.0, 0.0, 0, 0, 0]
	XV_inp = [0.0, 1.0, 0, 0, 0]
	print ("		XL_inp=", XL_inp)
	print ("		XV_inp=", XV_inp)
	
	[XL_out, XV_out, P, VL, VV, _] = objUtil.bublt (T, XL_inp, XV_inp, False)
	
	print ("\nOutput =========objUtil.bublt (T, XL, XV, False)===========")

	print ("	need to be not sezo 	XV_out=", XV_out)
	print ("		XL_out=", XL_out)
	print ("	kPa  P=", P)
	print ("		VV=", VV)
	print ("	m3/kmol	VV=", VV)
	print ("	m3/kg	VV=", VV/120.91)	
	
	T=273+20
	
	VSAT =  120.91/1328.9#    0.0909925502295131  #0.0308 *120.91 # =3.724028 # m3/kmol

	print ("=======input hcvcps=======---------------------------------------==========")
	print ("			 T=", T, T-273 ,"C")
	print ("			 VSAT=", VSAT)
	
	[HSAT, CV, CP, VSND] = objUtil.hcvcps (1, T, VSAT, X )

	
	print ("outs=============================================	KJ/KMOL HSAT=", HSAT)
	print ("	KJ/KMOL K	CV=", CV)
	print ("	KJ/KMOL K	CP=", CP)

	print ("=======# R12=================")
	RF_code = 2
	
	print ("	g/mole =", Data.CRIT[RF_code-1][0])
	print ("	KJ/Kg HSAT=", HSAT /120.91)
	print ("	KJ/Kg K	CV=", CV /120.91)
	print ("	KJ/Kg K	CP=", CP /120.91)
	
	print ("		VSND=", VSND)
	
	[A, B] = objUtil.espar (0, T, X)
	
	print ("		ref temp 243.39K  -29.61C", A)
	print ("		AMIX=", A)
	print ("		BMIX=", B)
	print ("		Data.HR[1]=", Data.HR[1] )
	print ("		Data.HP[1]=", Data.HP[1] )
	print ("		Data.DADT=", Data.DADT )
	print ("		Data.DBDT=", Data.DBDT )
	print ("		Data.D2ADT=", Data.D2ADT )

	print ("=======input   hpin ( H,P,X ) =================")
	R22_TABLE_READING = 200.0- 26.2257538946007

	Hbook =  370
	P = 400
	Happ =  (Hbook - R22_TABLE_READING)*120.91
	#print ("  h  =", h, " in kj/kg", h/120.91, " reading in table ",  h/120.91 + R22_TABLE_READING)
	print ("		P=", P)
	print ("		Happ=", Happ)
	print ("		Hbook=", Happ/120.91 + R22_TABLE_READING)

	[T, XQ,XL,XV,  VL,VV,  HL,HV] = objUtil.hpin ( Happ,P,X )	
	print ("outs========================")
	print ("		 T=", T, T-273.11)
	print ("		 XQ,XL,XV",XQ,XL,XV)
	
	print ("\n		VL=", VL)
	print ("	m3/kg	VL=", VL/120.91)
	print (" VL kg/m3   =", 120.91/VL)
	
	print ("\n		VV=", VV)
	print ("	m3/kg	VV=", VV/120.91)
	print (" VV kg/m3   =", 120.91/VV)
	
	print ("\n		HL=", HL)
	print ("	m3/kg K	HL=", HL/120.91)
	print ("	book m3/kg K	HL=",  HL/120.91 + R22_TABLE_READING)
	
	print ("\n		HV=", HV)	
	print ("	m3/kg K	HV=", HV/120.91)
	print ("	book m3/kg K	HV=",  HV/120.91 + R22_TABLE_READING)

# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
def showVolum (flt_vol_mole, flt_mole_weight, str_description =""):
	print ("		" + str_description + 	\
		" in m3/KMOL = %5.9f,  m3/Kg = %5.9f  Kg/m3 = %5.9f"  	\
		% (flt_vol_mole, flt_vol_mole/flt_mole_weight, flt_mole_weight/flt_vol_mole) )
	
# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 
def Title (strTitle):
	print ("\n\n_____________________________________________________")
	print ("######- Test Title:" + strTitle + " --#####\n")
# = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . = . 

if __name__ == '__main__':
	main()
	

