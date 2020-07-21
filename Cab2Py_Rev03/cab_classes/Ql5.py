# Python import
import math, sys, datetime

# User Import
from .CabUtils import CabUtils
from .DoorOpen import DoorOpen

class Ql5 (CabUtils):
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	# Job			: CALCULATE HEAT LEAKS FOR CHEST FREEZER    
	#				
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	def __init__(self, Cab):
		#
		#          ___   ___________________________________   ___
		#           |   |              Door                 |   |
		#           |   | -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   - |   |
		#           |   |                                   |   |
		#           |   |                                   |  H2
		#           |   |             FRONT                 |   |
		#           H1  |                                   |   |
		#           |   |                          | -   - W2 -   - |   |
		#           |   |                            _______|   |
		#           |   |                           |   \    \  -  -  - 
		#           |   | -   -   -   -   -   -   - W1 -   -   -   -   -   -  |\   \    \
		#           |   |___________________________| \   \   FSHR
		#           -  -  -                                  \   \
		#                                              FSV  \
		#                                                   FSH
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     Calculate internal and external dimensions
		# Cab.CWIDE 	The width  of the compressor compartment
		# Cab.CHGT 		The height of the compressor compartment
		#
		CWIDE = Cab.CWIDE +  Cab.SCIN
		CHGT  = Cab.CHGT  +  Cab.STIN
		
		TIFS = Cab.TIFRS
		W1 = Cab.WIDTH  -  CWIDE  -  TIFS
		W2 = CWIDE  -  TIFS
		H1 = Cab.HEIGHT  -  Cab.BINSUL  -  Cab.TIFT  -  Cab.DGSKT 
		H2 = Cab.HEIGHT  -  CHGT  -  Cab.TIFT  -  Cab.DGSKT   
		DC = Cab.DEPTH  -  Cab.TIFF  -  Cab.TIFB
		WC = Cab.WIDTH  -  TIFS  -  TIFS
		HC = Cab.HEIGHT  -  Cab.TIFT  -  Cab.BINSUL  -  Cab.DGSKT 
		
		#     SEE FIGURE 1 FOR CONFIGURATION 5 DIMENSIONS
		#
		# TAVGL 	average insulation conductivity for edges boardering the door
		# TAVGC 	average insulation conductivity for corners boardering the door
		
		TAVGL = (Cab.TKIN  +  Cab.RKIN) / 2.0
		TAVGC = (2.0 * Cab.RKIN + Cab.TKIN) / 3.0         
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     Calculate heat leaks
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     FRONT
		#
		RFRONT = Cab.RKIN * ((H1 * W1 + H2 * W2) / Cab.TIFF + 54.0 * (H1 + H2 + W1 + (W2 - Cab.SCIN)		\
			 +  (H1 - H2 - Cab.STIN)) / 2.0													\
			 +  0.15 * (4.0 * Cab.TIFF + 2.0 * TIFS + 2.0 * Cab.STIN + 2.0 * Cab.BINSUL				\
			 +  2.0 * Cab.SCIN) / 9.0)													\
			 +  TAVGL * 54.0 * (W1 + W2) / 2.0												\
			 +  TAVGC * 0.15 * (2.0 * Cab.TIFF + 2.0 * TIFS + 2.0 * Cab.TIFT) / 9.0 

		AIFRONT = (H1 * W1)  +  (H2 * W2)
		AOFRONT = (Cab.HEIGHT - Cab.DGSKT) * Cab.WIDTH  -  (CHGT - Cab.STIN) * (CWIDE - Cab.SCIN)       

		if Cab.HIWP != 0:
			RAWP = 1.0 / (Cab.HIWP * AIFRONT)
		
		if Cab.HIWP==0:
			RAWP = 1.0 / (Cab.HI * AIFRONT)

		Cab.QFRONT = 1.0 / (1.0 / (Cab.HO * AOFRONT)  +  1.0 / RFRONT + RAWP) * (Cab.TROOM - Cab.TFRZ)
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     BACK
		#
		RBACK = Cab.RKIN * ((H1 * W1 + H2 * W2) / Cab.TIFB  +  54.0 * (H1 + H2 + W1 + (W2 - Cab.SCIN)	\
			+  (H1 - H2 - Cab.STIN)) / 2.0		\
			+  0.15 * (4.0 * Cab.TIFB + 2.0 * TIFS + 2.0 * Cab.STIN + 2.0 * Cab.BINSUL		\
			+  2.0 * Cab.SCIN) / 9.0)		\
			+  TAVGL * 54.0 * (W1 + W2) / 2.0		\
			+  TAVGC * 0.15 * (2.0 * Cab.TIFB + 2.0 * TIFS + 2.0 * Cab.TIFT) / 9.0

		AIBACK = (H1 * W1)  +  (H2 * W2)
		AOBACK = (Cab.HEIGHT - Cab.DGSKT) * Cab.WIDTH  -  (CHGT - Cab.STIN) * (CWIDE - Cab.SCIN)        

		if Cab.HIWP != 0:
			RAWP = 1.0 / (Cab.HIWP * AIBACK)
		
		if Cab.HIWP==0:
			RAWP = 1.0 / (Cab.HI * AIBACK)
			
		Cab.QBACK = 1.0 / (1.0 / (Cab.HO * AOBACK)  +  1.0 / RBACK + RAWP) * (Cab.TROOM - Cab.TFRZ)
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     LEFT
		RLSIDE = Cab.RKIN * (DC * H1 / TIFS  +  54.0 * (DC + 2.0 * H1) / 2.0		\
			+  0.15 * (2.0 * Cab.BINSUL + 2.0 * TIFS + Cab.TIFF + Cab.TIFB) / 9.0)		\
			+  TAVGL * 54.0 * DC / 2.0 + TAVGC * 0.15 * (2.0 * Cab.TIFT + 2.0 * TIFS	\
			+  Cab.TIFF + Cab.TIFB) / 9.0

		AILSIDE = H1 * DC
		AOLSIDE = (Cab.HEIGHT - Cab.DGSKT) * Cab.DEPTH

		if Cab.HIWP != 0:
			RAWP = 1.0 / (Cab.HIWP * AILSIDE)
		
		if Cab.HIWP==0:
			RAWP = 1.0 / (Cab.HI * AILSIDE)
			

		Cab.QLSIDE = 1.0 / (1.0 / (Cab.HO * AOLSIDE)  +  1.0 / RLSIDE + RAWP) * (Cab.TROOM - Cab.TFRZ)
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     RIGHT
		#
		RRSIDE = Cab.RKIN * (H2 * DC / TIFS  +  54.0 * (2.0 * H2 + DC) / 2.0	\
			+  0.15 * (2.0 * TIFS  +  2.0 * Cab.STIN + Cab.TIFF + Cab.TIFB) / 9.0)	\
			+  TAVGL * 54.0 * DC / 2.0 + TAVGC * 0.15 * (2.0 * TIFS + 2.0 * Cab.TIFT + Cab.TIFF	\
			+  Cab.TIFB) / 9.0

		AIRSIDE = H2 * DC
		AORSIDE = (Cab.HEIGHT - Cab.DGSKT - CHGT + Cab.STIN) * Cab.DEPTH

		if Cab.HIWP != 0:
			RAWP = 1.0 / (Cab.HIWP * AIRSIDE)
		
		if Cab.HIWP==0:
			RAWP = 1.0 / (Cab.HI * AIRSIDE)
			
		Cab.QRSIDE = 1.0 / (1.0 / (Cab.HO * AORSIDE)  +  1.0 / RRSIDE + RAWP) * (Cab.TROOM - Cab.TFRZ)
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     TOP
		#
		RTOP = Cab.TKIN * (W1 + W2) * DC / Cab.TIFT  +  TAVGL * 54.0 * (2.0 * DC		\
			+  2.0 * (W1 + W2)) / 2.0			\
			+  TAVGC * 0.15 * (2.0 * Cab.TIFF + 2.0 * Cab.TIFB + 4.0 * Cab.TIFT + 4.0 * TIFS) / 9.0   

		AITOP = (W1 + W2) * DC
		AOTOP = Cab.WIDTH * Cab.DEPTH
		
		if Cab.HIWP != 0:
			RAWP = 1.0 / (Cab.HIWP * AITOP)
		
		if Cab.HIWP==0:
			RAWP = 1.0 / (Cab.HI * AITOP)
			

		Cab.QTOP = 1.0 / (1.0 / (Cab.HO * AOTOP)  +  1.0 / RTOP + RAWP) * (Cab.TROOM - Cab.TFRZ)
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     compressor step (horizontal)
		RSH = Cab.RKIN * ((W2 - Cab.SCIN) * DC / Cab.STIN + 1.08 * ((W2 - Cab.SCIN) + DC) / 2.0 + 0.15 * \
			(4.0 * Cab.STIN + 2.0 * TIFS + 2.0 * Cab.SCIN + 2.0 * Cab.TIFF + 2.0 * Cab.TIFB) / 9.0)      

		AISH = W2 * DC
		AOSH = Cab.DEPTH * (CWIDE - Cab.SCIN)

		if Cab.HIWP != 0:
			RAWP = 1.0 / (Cab.HIWP * AISH)
		
		if Cab.HIWP==0:
			RAWP = 1.0 / (Cab.HI * AISH)
			
		Cab.QSH = 1.0 / (1.0 / (Cab.HO * AOSH)  +  1.0 / RSH + RAWP) * (Cab.TBTM - Cab.TFRZ)           
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     compressor step (vertical)
		#
		RSV = Cab.RKIN * ((H1 - H2 - Cab.STIN) * DC / Cab.SCIN + 1.08 * (DC + (H1 - H2 - Cab.STIN)) / 2.0	\
			+  0.15 * (4.0 * Cab.SCIN + 2.0 * Cab.BINSUL + 2.0 * Cab.STIN + 2.0 * Cab.TIFF	\
			+  2.0 * Cab.TIFB) / 9.0)              

		AISV = (CHGT - Cab.BINSUL) * DC             
		AOSV = Cab.DEPTH * (CHGT - Cab.STIN)

		if Cab.HIWP != 0:
			 RAWP = 1.0 / (Cab.HIWP * AISV)
		
		if Cab.HIWP==0:
			RAWP = 1.0 / (Cab.HI * AISV)
			
		Cab.QSV = 1.0 / (1.0 / (Cab.HO * AOSV)  +  1.0 / RSV + RAWP) * (Cab.TBTM - Cab.TFRZ)
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     Heat leak through cabinet excluding the cab.bottom
		#
		QTNOBOT = Cab.QSV  +  Cab.QSH  +  Cab.QTOP  +  Cab.QRSIDE  +  Cab.QLSIDE  +  Cab.QBACK  +  Cab.QFRONT

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     Heat leak through cab.bottom
		#
		AIB1 = W1 * DC
		AOB1 = Cab.DEPTH * (Cab.WIDTH - CWIDE + Cab.SCIN)     
		RBOT = Cab.RKIN * (AIB1 / Cab.BINSUL  +  1.08 * (W1 + DC) / 2.0  +  0.15 * (4.0 * Cab.BINSUL	\
			+  2.0 * Cab.TIFF + 2.0 * Cab.TIFB + 2.0 * Cab.SCIN + 2.0 * TIFS) / 9.0)

		Cab.QBOTTM = 1.0 / (1.0 / (Cab.HO * AOB1)  +  1.0 / RBOT + 1.0 / (Cab.HI * AIB1)) *(Cab.TROOM - Cab.TFRZ)              
		Cab.QBCOMP = 0.0
		Cab.QFRZ = QTNOBOT  +  Cab.QBOTTM  +  Cab.QBCOMP

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     calculate gasket heat leak
		#
		Cab.QGZN = 24.0 * Cab.HLFZG * (Cab.WIDTH + Cab.DEPTH) * (Cab.TROOM - Cab.TFRZ)
		Cab.QGZF = Cab.QGZN

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     sum components of heat leak
		#
		Cab.QTON = Cab.QFRZ  +  Cab.QGZN
		Cab.QTOF = Cab.QFRZ  +  Cab.QGZF

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# The heat leak due to Door Openings
		obj_doorpn = DoorOpen().DoorpnBuilder ()	\
			.withTempFFSetpoint (Cab.TFF)		\
			.withTempFZSetpoint (Cab.TFRZ)		\
			.withHeighFF (HC) 				\
			.withWidthFF (WC)			\
			.withDepthFF (DC)			\
			.withHeighFZ (HC)			\
			.withWidthFZ (WC)			\
			.withDepthFZ (DC)			\
			.withMode (Cab.NMOD)			\
			.withVolumeFZ (Cab.VOLAZ)	\
			.withVolumeFF (Cab.VOLAR)	\
			.withRelHumidity (Cab.RELHUM)		\
			.withTempAirAmbient ( Cab.TDRAIR)	\
			.withOpenHrFF (Cab.FFCOPN)		\
			.withWaterFF (Cab.WATERF)		\
			.withWaterFZ (Cab.WATERZ)		\
			.withOpenHrFZ (Cab.FRZOPN)	\
			.withHrOpenFF (Cab.HRFFC)	\
			.withHrOpenFZ (Cab.HRFRZ)	\
			.build()

		lstRes = obj_doorpn.main()

		Cab.QDFFCS = lstRes [0]
		Cab.QDFFFS = lstRes [1]
		Cab.QDFZCS = lstRes [2]
		Cab.QDFZFS = lstRes [3]

		Cab.QDFFCL = lstRes [4]
		Cab.QDFFFL = lstRes [5]
		Cab.QDFZCL = lstRes [6]
		Cab.QDFZFL = lstRes [7]
		return 

