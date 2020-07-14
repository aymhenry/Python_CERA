# Python Import
import math, sys, datetime

# User Import
from .CabUtils import CabUtils
from .Doorpn import Doorpn

class Ql13 (Doorpn):
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	# Job			: CALCULATE CABINET HEAT LOADS FOR CONFIGURATION
	#				 Configuration 3: Top Mount self.R/ F 
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	def __init__(self, Cab):

		# Calculate overall internal Height, Width and Depth of Freezer
		# compartment (FRZ) and the Fresh Food Compartment (FFC)

		#	Internal dimensions
		loc_HFRZ = Cab.TOPMUL - Cab.TIFT
		loc_WFRZ = Cab.WIDTH - Cab.TIFLS - Cab.TIFRS
		loc_DFRZ = Cab.DEPTH - Cab.WEDGE - Cab.TIFF - Cab.TIFB - Cab.DGSKT

		ALPHA= 0.0
		BETA = 0.0
		H1 = 0.0
		DFFC = 0.0

		if Cab.NCCTYPE == 1:
			HFFC = Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL - Cab.BINSUL
			WFFC = Cab.WIDTH - Cab.TIRLS - Cab.TIRRS
			DFFC = Cab.DEPTH - Cab.WEDGER - Cab.TIRF - Cab.TIRB - Cab.DGSKT

		if Cab.NCCTYPE == 2:
			BETA  = math.atan(Cab.CDDN/ Cab.CCHGT)
			ALPHA = math.pi / 4.0 - BETA/ 2.0
			H1 = Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL - Cab.BINSUL

			HTRIAN = Cab.CCHGT - Cab.BINSUL + binsul / math.sin(beta) - tirb/ math.tan(beta)

			H2 = H1 - htrian
			D1 = htrian/ math.cos(beta)
			D2 = dc - htrian * math.tan(beta)

			WFFC = Cab.WIDTH - Cab.TIRLS - Cab.TIRRS
			HFFC = H1

		if Cab.NCCTYPE == 3:
			BETA  = math.atan((Cab.CDDN - Cab.CDUP)/ Cab.CCHGT)
			ALPHA = math.pi / 4.0 - BETA/ 2.0
			H1 = Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL - Cab.BINSUL
			H2 = H1 - Cab.CCHGT
			D1 = Cab.CDUP - Cab.TIRB

			if Cab.CDDN == Cab.CDUP:
				D2 = Cab.CCHGT - Cab.BINSUL
			else:
				D2 = (Cab.CDDN - Cab.CDUP)/ math.sin(BETA) - Cab.BINSUL * math.tan(ALPHA)

			D3 = Cab.DEPTH - Cab.CDDN - Cab.TIRF - Cab.WEDGER - Cab.BINSUL * math.tan(ALPHA) - Cab.DGSKT
			DC = Cab.DEPTH - Cab.WEDGER - Cab.TIRF - Cab.TIRB - Cab.DGSKT
			WFFC = Cab.WIDTH - Cab.TIRLS - Cab.TIRRS
			HFFC = H1

		FALPHA = 4.0 * ALPHA/ math.pi
		FBETA = 2.0 * BETA/ math.pi

		#	Internal Areas
		# AMUL IS THE MULLION AREA (FT2)
		# UMUL IS THE MULLION OVERALL HEAT TRANSFER (BTU/ HR - FT2 - DEG F)
		# Cab.QMULI is the heat leaking into the freezer from the fresh food

		AMUL = loc_WFRZ * loc_DFRZ
		UMUL = 1.0/ (1.0/ Cab.HIFMUL + 1.0/ Cab.HIRMUL + Cab.THMUL/ Cab.CKMUL)
		Cab.QMULI = UMUL * AMUL * (Cab.TFF - Cab.TFRZ)

		# Internal Freezer (F) Areas
		AIFSID = loc_HFRZ * loc_DFRZ
		AIFTOP = loc_WFRZ * loc_DFRZ
		AIFBCK = loc_HFRZ * loc_WFRZ

		# External Freezer (F) Areas
		AOFSID = (Cab.TOPMUL + Cab.THMUL/ 2.0) * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT)
		AOFTOP = Cab.WIDTH * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT)
		AOFBCK = (Cab.TOPMUL + Cab.THMUL/ 2.0) * Cab.WIDTH

		#	Internal Fresh Food (Cab.R) Areas
		if Cab.NCCTYPE == 1:
			AIRSID = HFFC * DFFC
			AIRBOT = WFFC * DFFC
			AIRBCK = HFFC * WFFC
			AIRFNT = AIRBCK

		if Cab.NCCTYPE == 2:
			AIRSID = DC * H2 + (DC + D2) * (H1 - H2)/ 2.0
			AIRBCK = WFFC * H2
			AIRFNT = WFFC * H1
			AIRBTM1 = WFFC * D1
			AIRBTM2 = WFFC * D2

		if Cab.NCCTYPE == 3:
			AIRSID = DC * H2 + Cab.CCHGT * (D3 + D3 + Cab.CCHGT * math.tan(BETA))/ 2.0
			AIRBCK = WFFC * H2
			AIRFNT = WFFC * H1
			AIRBTM1 = WFFC * (D1 + Cab.BINSUL * math.tan(ALPHA))
			AIRBTM2 = WFFC * (D2 + Cab.BINSUL * math.tan(ALPHA))
			AIRBTM3 = WFFC * D3

		# External Fresh Food (Cab.R) Areas
		if Cab.NCCTYPE == 1:
			AORSID = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL/ 2.0) * (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT)
			AORBOT = Cab.WIDTH * (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT)
			AORBCK = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL/ 2.0) * Cab.WIDTH
			AORFNT = AORBCK

		if Cab.NCCTYPE == 2:
			AORSID = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL/ 2.0) * (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT) 	\
				 - Cab.CDDN * Cab.CCHGT/ 2.0
			AORFNT = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL/ 2.0) * Cab.WIDTH
			AORBCK = AORFNT - Cab.CCHGT * Cab.WIDTH
			AORBTM1 = Cab.WIDTH * Cab.CCHGT/ math.cos(BETA)
			AORBTM2 = Cab.WIDTH * (Cab.DEPTH - Cab.CDDN - Cab.WEDGER - Cab.DGSKT)

		if Cab.NCCTYPE == 3:
			AORSID = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL/ 2.0) * (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT)	\
				 - (Cab.CDDN + Cab.CDUP) * Cab.CCHGT/ 2.0
			AORFNT = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL/ 2.0) * Cab.WIDTH
			AORBCK = AORFNT - Cab.CCHGT * Cab.WIDTH
			AORBTM1 = Cab.WIDTH * Cab.CDUP
			AORBTM2 = Cab.WIDTH * Cab.CCHGT/ math.cos(BETA)
			AORBTM3 = Cab.WIDTH * (Cab.DEPTH - Cab.CDDN - Cab.WEDGER - Cab.DGSKT)

		# The average insulation conductivity. Cab.DKIN is the door
		# insulation conductivity. Cab.RKIN is the side, back, top and
		# bottom insulation conductivity, i.e. the cabinet.
		# FF is the Fresh Food, FZ is the FreeZer
		#
		TAVGL=0.25 * (Cab.DKINFF + Cab.DKINFZ + Cab.RKINFF + Cab.RKINFZ)

		# Calculate the cabinet heat leak as the sum of the top, sides,
		# bottom, front and back heat leaks.

		# Note that the cabinet has
		#		- six wall sections (top, bottom, left side, right side, bottom and back)
		#		- 12 edges
		#		- and 8 corners.

		# The door, which involves :-
		#		4 edges and
		#		4 corners,
		#		has in addition to the edge and corner effect, a gasket and a wedge heat leak added in.

		# The shape factor for an edge is 0.54 * Length of the edge.
		# The shape factor for a corner is 0.15 * Wall thickness.
		# 			(Holman p. 54).

		# note that most corners have two or three insulation thicknesses, thus we average all three thicknesses.
		#
		# Cab.TIFT :	insulation thickness on top (FT)
		# Cab.TIFRS:	insulation thickness on the Freezer Right side (FT)
		# Cab.TIFLS:	insulation thickness on the Freezer Left side (FT)
		# Cab.TIRLS:	insulation thickness on the Left Fresh Food Side (FT)
		# Cab.TIRRS:	insulation thickness on the Right Fresh Food Side (FT)
		# Cab.TIFB :	insulation thickness on the Freezer back (FT)
		# Cab.TIRB :	insulation thickness on the Fresh Food back (FT)
		# Cab.TIFF :	insulation thickness on the Freezer front (FT)
		# Cab.TIRF :	insulation thickness on the Fresh Food front (FT)
		# Cab.BINSUL:	insulation thickness on the bottom (FT)

		# Cab.R:		conduction resistance.
		#		It is the sum of the wall resistance plus the edge resistance plus the corner resistance in that order.

		# The Left Side Wall Resistance (Cab.R) and heat leak (QLFSID)

		# The side walls have two Depth and two Height length edges.
		# The edge shape factor is divided by two because each edge is shared by two walls.
		# The corner shape factor is divided by three because each corner is shared by three walls.

		# (Actually each corner shape factor is divided by 9 because the three corner thicknesses are averaged (9 = 3X3))

		# Calculate the heat leak out of the LEFT Freezer Side
		#
		RF = AIFSID/ Cab.TIFLS + 0.54 * (loc_DFRZ + 2.0 * loc_HFRZ)/ 2.0	\
			 + 0.15 * (2.0 * Cab.TIFLS + Cab.TIFB + 2.0 * Cab.TIFT + Cab.TIFF)/ 9.0

		R1 = 1.0/ (RF * Cab.RKINFZ) + 1.0/ (Cab.HILTT * AIFSID)
		R2 = 1.0/ (Cab.HOLTT * AOFSID)

		loc_list = self.radtrn (R1, R2, Cab.TFRZ, Cab.TFLSID)
		Cab.QFLSID = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the LEFT Fresh Food Side

		if Cab.NCCTYPE == 1:
			loc_RR = AIRSID/ Cab.TIRLS + 0.54 * (DFFC + 2.0 * HFFC)/ 2.0	\
				 + 0.15 * (2.0 * Cab.TIRLS + Cab.TIRB + 2.0 * Cab.BINSUL + Cab.TIRF)/ 9.0

		if Cab.NCCTYPE == 2:
			loc_RR = AIRSID/ Cab.TIRLS + 0.54 * (H1 + H2 + D1 + D2)/ 2.0 + 0.15		\
				 * ((2.0 * Cab.BINSUL + Cab.TIRLS) * FALPHA		\
				 + (Cab.TIRB + Cab.TIRLS + Cab.BINSUL) * FBETA		\
				 + (Cab.TIRF + Cab.TIRLS + Cab.BINSUL))/ 9.0

		if Cab.NCCTYPE == 3:
			loc_RR = AIRSID/ Cab.TIRLS + 0.54 * (H1 + H2 + D1 + D2 + D3)/ 2.0 + 0.15		\
				 * (2.0 * (2.0 * Cab.BINSUL + Cab.TIRLS) * FALPHA					\
				 + (2.0 * (Cab.TIRLS + Cab.BINSUL) + Cab.TIRF + Cab.TIRB))/ 9.0

		R1 = 1.0/ (loc_RR * Cab.RKINFF) + 1.0/ (Cab.HILTB * AIRSID)
		R2 = 1.0/ (Cab.HOLTB * AORSID)

		loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TRLSID)
		Cab.QRLSID = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the RIGHT Freezer Side

		RF = AIFSID/ Cab.TIFRS + 0.54 * (loc_DFRZ + 2.0 * loc_HFRZ)/ 2.0		\
			 + 0.15 * (2.0 * Cab.TIFRS + Cab.TIFB + 2.0 * Cab.TIFT + Cab.TIFF)/ 9.0

		R1 = 1.0/ (RF * Cab.RKINFZ) + 1.0/ (Cab.HIRTT * AIFSID)
		R2 = 1.0/ (Cab.HORTT * AOFSID)

		loc_list = self.radtrn (R1, R2, Cab.TFRZ, Cab.TFRSID)
		Cab.QFRSID = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the RIGHT Fresh Food Side
		if Cab.NCCTYPE == 1:
			loc_RR = AIRSID/ Cab.TIRRS + 0.54 * (DFFC + 2.0 * HFFC)/ 2.0			\
				 + 0.15 * (2.0 * Cab.TIRRS + Cab.TIRB + 2.0 * Cab.BINSUL + Cab.TIRF)/ 9.0

		if Cab.NCCTYPE == 2:
			loc_RR = AIRSID/ Cab.TIRRS + 0.54 * (H1 + H2 + D1 + D2)/ 2.0 + 0.15		\
				 * ((2.0 * Cab.BINSUL + Cab.TIRRS) * FALPHA		\
				 + (Cab.TIRB + Cab.TIRRS + Cab.BINSUL) * FBETA		\
				 + (Cab.TIRF + Cab.TIRRS + Cab.BINSUL))/ 9.0

		if Cab.NCCTYPE == 3:
			loc_RR = AIRSID/ Cab.TIRRS + 0.54 * (H1 + H2 + D1 + D2 + D3)/ 2.0 + 0.15	\
				 * (2.0 * (2.0 * Cab.BINSUL + Cab.TIRRS) * FALPHA					\
				 + (2.0 * (Cab.TIRRS + Cab.BINSUL) + Cab.TIRF + Cab.TIRB))/ 9.0

		R1 = 1.0/ (loc_RR * Cab.RKINFF) + 1.0/ (Cab.HIRTB * AIRSID)
		R2 = 1.0/ (Cab.HORTB * AORSID)

		loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TRRSID)

		Cab.QRRSID = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the top (Freezer Only)
		# The top has two Depth and two Width length edges
		#
		Cab.R = AIFTOP/ Cab.TIFT + 0.54 * (2.0 * loc_DFRZ + 2.0 * loc_WFRZ)/ 2.0	\
			 + 0.15 * ( 2.0 * Cab.TIFLS + 2.0 * Cab.TIFRS + 2.0 * Cab.TIFB + 4.0 * Cab.TIFT + 2.0 * Cab.TIFF)/ 9.0
		R1 = 1.0/ (Cab.R * Cab.RKINFZ) + 1.0/ (Cab.HITOP * AIFTOP)
		R2 = 1.0/ (Cab.HOTOP * AOFTOP)

		loc_list = self.radtrn (R1, R2, Cab.TFRZ, Cab.TTOP)
		Cab.QTOP = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the BACK Freezer
		# The back has two Height and one Width length edges
		Cab.R = AIFBCK/ Cab.TIFB + 0.54 * (2.0 * loc_HFRZ + loc_WFRZ)/ 2.0		\
			 + 0.15 * (Cab.TIFLS + Cab.TIFRS + 2.0 * Cab.TIFB + 2.0 * Cab.TIFT)/ 9.0
		R1 = 1.0/ (Cab.R * Cab.RKINFZ) + 1.0/ (Cab.HIBKT * AIFBCK)
		R2 = 1.0/ (Cab.HOBKT * AOFBCK)

		loc_list = self.radtrn (R1, R2, Cab.TFRZ, Cab.TFBACK)
		Cab.QFBACK = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the BACK Fresh Food
		# The back has two Height and one Width length edges
		if Cab.NCCTYPE == 1:
			Cab.R = AIRBCK/ Cab.TIRB + 0.54 * (2.0 * HFFC + WFFC)/ 2.0		\
				 + 0.15 * (Cab.TIRLS + Cab.TIRRS + 2.0 * Cab.TIRB + 2.0 * Cab.BINSUL)/ 9.0

		if Cab.NCCTYPE == 2:
			Cab.R = AIRBCK/ Cab.TIRB + 0.54 * (2.0 * H2 + WFFC * FBETA)/ 2.0 + 0.15		\
				 * (Cab.TIRLS + Cab.TIRRS + 2.0 * Cab.TIRB + 2.0 * Cab.BINSUL) * FBETA/ 9.0

		if Cab.NCCTYPE == 3:
			Cab.R = AIRBCK/ Cab.TIRB + 0.54 * (2.0 * H2 + WFFC)/ 2.0		\
				 + 0.15 * (Cab.TIRLS + Cab.TIRRS + 2.0 * Cab.TIRB + 2.0 * Cab.BINSUL)/ 9.0

		R1 = 1.0/ (Cab.R * Cab.RKINFF) + 1.0/ (Cab.HIBKB * AIRBCK)
		R2 = 1.0/ (Cab.HOBKB * AORBCK)

		loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TRBACK)
		Cab.QRBACK = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the FRONT Freezer
		# The front has two Height and one Width length edges
		#
		Cab.R = AIFBCK/ Cab.TIFF + 0.54 * (2.0 * loc_HFRZ + loc_WFRZ)/ 2.0		\
			 + 0.15 * (Cab.TIFLS + Cab.TIFRS + 2.0 * Cab.TIFF + 2.0 * Cab.TIFT)/ 9.0
		R1 = 1.0/ (Cab.R * Cab.DKINFZ) + 1.0/ (Cab.HIFTT * AIFBCK)
		R2 = 1.0/ (Cab.HOFTT * AOFBCK)

		loc_list = self.radtrn (R1, R2, Cab.TFRZ, Cab.TFFRNT)
		Cab.QFFRNT = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the FRONT Fresh Food
		# The front has two Height and one Width length edges
		#
		Cab.R = AIRFNT/ Cab.TIRF + 0.54 * (2.0 * HFFC + WFFC)/ 2.0		\
			 + 0.15 * (Cab.TIRLS + Cab.TIRRS + 2.0 * Cab.TIRF + 2.0 * Cab.BINSUL)/ 9.0

		R1 = 1.0/ (Cab.R * Cab.DKINFF) + 1.0/ (Cab.HIFTB * AIRFNT)
		R2 = 1.0/ (Cab.HOFTB * AORFNT)

		loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TRFRNT)
		Cab.QRFRNT = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the bottom, both
		# with and without compressor notch
		# The top has two Depth and two Width length edges
		if Cab.NCCTYPE == 1:
			AOB = AORBOT
			AIB = AIRBOT
			Cab.R = AIB/ Cab.BINSUL + 0.54 * (2.0 * DFFC + 2.0 * WFFC)/ 2.0			\
				 + 0.15 * (4.0 * Cab.BINSUL + 2.0 * Cab.TIRB + 2.0 * Cab.TIRLS + 2.0 * Cab.TIRRS + 2.0 * Cab.TIRF)/ 9.0
			R1 = 1.0/ (Cab.R * Cab.RKINFF) + 1.0/ (Cab.HIBOT * AIB)
			R2 = 1.0/ (Cab.HOBOT * AOB)

			loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TBTM)
			Cab.QBOTTM = loc_list[0]
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

		if Cab.NCCTYPE == 2:
			RB1 = AIRBTM1/ Cab.BINSUL		\
				 + 0.54 * (WFFC * (FALPHA + FBETA) + 2.0 * D1)/ 2.0		\
				 + 0.15 * ((2.0 * Cab.TIRB + 2.0 * Cab.BINSUL + Cab.TIRLS + Cab.TIRRS) * FBETA	\
				 + (4.0 * Cab.BINSUL + Cab.TIRLS + Cab.TIRRS) * FALPHA)/ 9.0
			R1 = 1.0/ (RB1 * Cab.RKINFF) + 1.0/ (Cab.HIBOT * AIRBTM1)
			R2 = 1.0/ (Cab.HOBOT * AORBTM1)

			loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TBTM)
			Cab.QBOTTM1 = loc_list[0]

			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			RB2 = AIRBTM2/ Cab.BINSUL		\
				 + 0.54 * (WFFC * (1.0 + FALPHA) + 2.0 * D2)/ 2.0		\
				 + 0.15 * ((4.0 * Cab.BINSUL + Cab.TIRLS + Cab.TIRRS) * FALPHA		\
				 + (2.0 * Cab.BINSUL + 2.0 * Cab.TIRF + Cab.TIRLS + Cab.TIRRS))/ 9.0
			R1 = 1.0/ (RB2 * Cab.RKINFF) + 1.0/ (Cab.HIBOT * AIRBTM2)
			R2 = 1.0/ (Cab.HOBOT * AORBTM2)

			loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TBTM)
			Cab.QBOTTM2 = loc_list[0]

			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			Cab.QBOTTM = Cab.QBOTTM1 + Cab.QBOTTM2

		if Cab.NCCTYPE == 3:
			RB1 = AIRBTM1/ Cab.BINSUL + 0.54 * (WFFC * (1 + FALPHA) + 2.0 * D1)/ 2.0		\
				 + 0.15 * ((4.0 * Cab.BINSUL + Cab.TIRLS + Cab.TIRRS) * FALPHA		\
				 + (2.0 * Cab.BINSUL + 2.0 * Cab.TIRB + Cab.TIRLS + Cab.TIRRS))/ 9.0
			R1 = 1.0/ (RB1 * Cab.RKINFF) + 1.0/ (Cab.HIBOT * AIRBTM1)
			R2 = 1.0/ (Cab.HOBOT * AORBTM1)

			loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TBTM)
			Cab.QBOTTM1 = loc_list[0]
	
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

			RB2 = AIRBTM2/ Cab.BINSUL + 0.54 * (2.0 * WFFC * FALPHA + 2.0 * D2)/ 2.0		\
				 + 0.15 * (D(8.0) * Cab.BINSUL + 2.0 * Cab.TIRLS + 2.0 * Cab.TIRRS) * FALPHA/ 9.0
			R1 = 1.0/ (RB2 * Cab.RKINFF) + 1.0/ (Cab.HIBOT * AIRBTM2)
			R2 = 1.0/ (Cab.HOBOT * AORBTM2)

			loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TBTM)
			Cab.QBOTTM2 = loc_list[0]
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			RB3 = AIRBTM3/ Cab.BINSUL + 0.54 * ((1 + FALPHA) * WFFC + 2.0 * D3)/ 2.0		\
				 + 0.15 * ((4.0 * Cab.BINSUL * Cab.TIRLS + Cab.TIRRS) * FALPHA			\
				 + (2.0 * Cab.BINSUL + 2.0 * Cab.TIRF + Cab.TIRLS + Cab.TIRRS))/ 9.0
			R1 = 1.0/ (RB3 * Cab.RKINFF) + 1.0/ (Cab.HIBOT * AIRBTM3)
			R2 = 1.0/ (Cab.HOBOT * AORBTM3)

			loc_list = self.radtrn (R1, R2, Cab.TFF, Cab.TBTM)
			Cab.QBOTTM3 = loc_list[0]

			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			Cab.QBOTTM = Cab.QBOTTM1 + Cab.QBOTTM2 + Cab.QBOTTM3

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Sum all the heat leaks to get the cabinet heat leak.
		Cab.QFRZ = Cab.QFLSID + Cab.QFRSID + Cab.QTOP + Cab.QFBACK + Cab.QFFRNT + Cab.QMUL
		Cab.QFFT = Cab.QRLSID + Cab.QRRSID + Cab.QRBACK + Cab.QRFRNT + Cab.QBOTTM - Cab.QMUL

		# calculate gasket heat leaks for each compartment and for
		# freezer fan on and freezer fan off

		if Cab.IRFTYP == 7:
			Cab.QGZN = 12.0 * Cab.HLFZG * (2.0 * Cab.TOPMUL + Cab.WIDTH) * (Cab.TROOM - Cab.TFRZ)
			Cab.QGZF = Cab.QGZN
			Cab.QGR = 12.0 * Cab.HLRG * (2.0 * HFFC + Cab.WIDTH) * (Cab.TROOM - Cab.TFF)
		else:
			Cab.QGZN = 24.0 * Cab.HLFZG * (Cab.TOPMUL + Cab.WIDTH) * (Cab.TROOM - Cab.TFRZ)
			Cab.QGZF =Cab.QGZN
			Cab.QGR = 24.0 * Cab.HLRG * (HFFC + Cab.WIDTH) * (Cab.TROOM - Cab.TFF)

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# CALCULATE WEDGE HEAT LEAKS
		THETAL = math.atan((Cab.TIFLS - Cab.FLANGE)/ Cab.WEDGE)
		THETAR = math.atan((Cab.TIFRS - Cab.FLANGE)/ Cab.WEDGE)
		AWEDGL = Cab.WEDGE * (Cab.TIFLS/ (Cab.TIFLS - Cab.FLANGE))
		AWEDGR = Cab.WEDGE * (Cab.TIFRS/ (Cab.TIFRS - Cab.FLANGE))
		BWEDGL = AWEDGL - Cab.WEDGE
		BWEDGR = AWEDGR - Cab.WEDGE

		WL1 = 2.0 * loc_HFRZ + loc_WFRZ
		WL2 = 2.0 * (loc_HFRZ - Cab.FLANGE + Cab.TIFT) + loc_WFRZ + (Cab.TIFLS - Cab.FLANGE)	\
			 + (Cab.TIFRS - Cab.FLANGE)

		Cab.QWFZ = (1.0/ (1.0/ (Cab.HOLTT * Cab.WEDGE * (2.0 * Cab.TOPMUL + Cab.WIDTH))		\
			 + (THETAL/ (Cab.WKIN * math.log(AWEDGL/ BWEDGL) * (WL1 + WL2)/ 2.0)			\
			 + THETAR / (Cab.WKIN  * math.log(AWEDGR/ BWEDGR) * (WL1 + WL2)/ 2.0))/ 2.0)) * 		\
			 (Cab.TROOM - Cab.TFRZ)

		if Cab.WEDGER != 0.0:
			THETAL = math.atan((Cab.TIRLS - Cab.FLANGER)/ Cab.WEDGER)
			THETAR = math.atan((Cab.TIRRS - Cab.FLANGER)/ Cab.WEDGER)

			AWEDGL = Cab.WEDGER * (Cab.TIRLS/ (Cab.TIRLS - Cab.FLANGER))
			AWEDGR = Cab.WEDGER * (Cab.TIRRS/ (Cab.TIRRS - Cab.FLANGER))
			BWEDGL = AWEDGL - Cab.WEDGER
			BWEDGR = AWEDGR - Cab.WEDGER

			WL = HFFC + Cab.BINSUL - Cab.FLANGER

			QWFS = (1.0/ (1.0/ (Cab.HOLTB * Cab.WEDGER * 2.0 * HFFC)		\
				 + (THETAL/ (Cab.WKINR * math.log(AWEDGL/ BWEDGL) * (WL + HFFC))				\
				 + THETAR / (Cab.WKINR * math.log(AWEDGR/ BWEDGR) * (WL + HFFC)) )/ 2.0))		\
				 * (Cab.TROOM - Cab.TFF)
			THETA  = math.atan((Cab.BINSUL - Cab.FLGB)/ Cab.WEDGER)
			AWEDGE = Cab.WEDGER * (Cab.BINSUL/ (Cab.BINSUL - Cab.FLGB))
			BWEDGE = AWEDGE - Cab.WEDGER

			WL = Cab.WIDTH - 2.0 * Cab.FLANGER
			QWFB = (1.0/ (1.0/ (Cab.HOBOT * Cab.WEDGER * Cab.WIDTH) + THETA/ 		\
				(Cab.WKINR * math.log(AWEDGE/ BWEDGE) * (WFFC + WL)/ 2.0))) * (Cab.TBTM - Cab.TFF)

		Cab.QW = Cab.QWFZ + QWFS + QWFB
		Cab.QWFF = QWFS + QWFB

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# SUM VARIOUS COMPONENTS OF THE HEAT LEAK
		Cab.QGON = Cab.QGR + Cab.QGZN
		Cab.QGOF = Cab.QGR + Cab.QGZF
		Cab.QTON = Cab.QFRZ + Cab.QFFT + Cab.QGON + Cab.QW
		Cab.QTOF = Cab.QFRZ + Cab.QFFT + Cab.QGOF + Cab.QW

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# The heat leak due to Door Openings
		obj_doorpn = Doorpn().DoorpnBuilder ()	\
			.withTempFFSetpoint (Cab.TFF)		\
			.withTempFZSetpoint (Cab.TFRZ)		\
			.withHeighFF ( HFFC) 				\
			.withWidthFF (WFFC)			\
			.withDepthFF (DFFC)			\
			.withHeighFZ (loc_HFRZ)			\
			.withWidthFZ (loc_WFRZ)			\
			.withDepthFZ (loc_DFRZ)			\
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

#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
#___________ End of Ql13 class _________________________________________________________________