# Python import
import math, sys

# User Import
from .CabUtils import CabUtils
from .DoorOpen import DoorOpen

class Ql467 (CabUtils):
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	# Job			: CALCULATE HEAT LEAKS FOR UPRIGHT FREEZER 
	#				 
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	def __init__(self,Cab):
		#
		# This type of refrigerator has individual wall Cab.HI & Cab.HO so set the
		# universal Cab.HI & Cab.HO to zero
		#
		Cab.HI = 0.0
		Cab.HO = 0.0
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# CALCULATE INTERNAL DIMENSTIONS OF THE COMPARTMENTS
		#
		HC = Cab.HEIGHT - Cab.BINSUL - Cab.TIFT
		WC = Cab.WIDTH - Cab.TIFRS - Cab.TIFLS
		DC = Cab.DEPTH - Cab.TIFF - Cab.TIFB - Cab.WEDGE - Cab.DGSKT
		TBOX = Cab.TFRZ
		TITOP = Cab.TIFT
		TIBK = Cab.TIFB
		TIRSD = Cab.TIFRS
		TILSD = Cab.TIFLS
		TIFRT = Cab.TIFF

		if Cab.NCCTYPE == 2:
			BETA = math.atan(Cab.CDDN / Cab.CCHGT)
			ALPHA = math.pi / 4.0 - BETA / 2.0
			H1 = HC

			HTRIAN = Cab.CCHGT - Cab.BINSUL + binsul / math.sin(beta) - TIFB / math.tan(beta)

			H2 = H1 - HTRIAN
			d1 = HTRIAN / cos(beta)
			d2 = dc - HTRIAN * math.tan(beta)

		if Cab.NCCTYPE == 3:
			BETA = math.atan((Cab.CDDN - Cab.CDUP) / Cab.CCHGT)
			ALPHA = math.pi / 4.0 - BETA / 2.0
			H1 = HC
			H2 = H1 - Cab.CCHGT
			D1 = Cab.CDUP - Cab.TIFB
			
			if ((Cab.CDDN - Cab.CDUP) == 0.0):
				D2 = Cab.CCHGT - Cab.BINSUL
			else:
				D2 = (Cab.CDDN - Cab.CDUP) / math.sin(BETA) - Cab.BINSUL * math.tan(ALPHA)

			D3 = Cab.DEPTH - Cab.CDDN - Cab.TIFF - Cab.WEDGE - Cab.BINSUL * math.tan(ALPHA) - Cab.DGSKT

			FALPHA = 4.0 * ALPHA / math.pi
			FBETA = 2.0 * BETA / math.pi
			
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# CALCULATE INTERNAL SURFACE AREAS
		#
		# AISIDE	Internal area of the left or right side - 
		# AITOP		Internal area of the top or bottom - 
		# AIBACK	Internal area of the front or back - 
		#

		if Cab.NCCTYPE == 1:
			AISIDE = HC * DC
			AITOP = WC * DC
			AIBACK = HC * WC
			AIFRNT = AIBACK
			AIBTM = AITOP
		
		if Cab.NCCTYPE == 2:
			AISIDE = DC * H2 + (DC + D2) * (H1 - H2) / 2.0
			AITOP = WC * DC
			AIBACK = WC * H2
			AIFRNT = WC * H1
			AIBTM1 = WC * D1
			AIBTM2 = WC * D2
					
		if Cab.NCCTYPE == 3:
			AISIDE = DC * H2 + Cab.CCHGT * (D3 + D3 + Cab.CCHGT * math.tan(BETA)) / 2.0
			AITOP = WC * DC
			AIBACK = WC * H2
			AIFRNT = WC * H1
			AIBTM1 = WC * (D1 + Cab.BINSUL * math.tan(ALPHA))
			AIBTM2 = WC * (D2 + Cab.BINSUL * math.tan(ALPHA))
			AIBTM3 = WC * D3

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# CALCULATE EXTERNAL SURFACE AREAS
		# AOSIDE		External area of the left or right side 
		# AOTOP			External area of the top or bottom 
		# AOBACK		External area of the front or back 
		#
		
		if Cab.NCCTYPE == 1:
			AOSIDE = Cab.HEIGHT * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT)
			AOTOP = Cab.WIDTH * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT)
			AOBACK = Cab.HEIGHT * Cab.WIDTH
			AOFRNT = AOBACK
			AOBTM = AOTOP

		if Cab.NCCTYPE == 2:
			AOSIDE = Cab.HEIGHT * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT) - Cab.CDDN * Cab.CCHGT / 2.0
			AOTOP = Cab.WIDTH * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT)
			AOBACK = (Cab.HEIGHT - Cab.CCHGT) * Cab.WIDTH
			AOFRNT = Cab.HEIGHT * Cab.WIDTH
			AOBTM1 = Cab.WIDTH * Cab.CCHGT / COS(BETA)
			AOBTM2 = Cab.WIDTH * (Cab.DEPTH - Cab.CDDN - Cab.WEDGE - Cab.DGSKT)
		
		if Cab.NCCTYPE == 3:
			AOSIDE = Cab.HEIGHT * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT) - (Cab.CDDN + Cab.CDUP) * Cab.CCHGT / 2.0
			AOTOP = Cab.WIDTH * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT)
			AOBACK = (Cab.HEIGHT - Cab.CCHGT) * Cab.WIDTH
			AOFRNT = Cab.HEIGHT * Cab.WIDTH
			AOBTM1 = Cab.WIDTH * Cab.CDUP
			AOBTM2 = Cab.WIDTH * Cab.CCHGT / COS(BETA)
			AOBTM3 = Cab.WIDTH * (Cab.DEPTH - Cab.CDDN - Cab.WEDGE - Cab.DGSKT)

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Account for the presence of the freezer compartment in a single - door refrigerator.
		#
		if (Cab.NMOD != 4) :
			FTOP = 0.0
			FBAK = 0.0
			FSID = 0.0
			TCABT = Cab.TFRZ
			TCABB = Cab.TFRZ
			TCABS = Cab.TFRZ
		else:
			TBOX = Cab.TFF
			FTOP = Cab.FD * Cab.FW / AITOP
			FBAK = Cab.FH * Cab.FW / AIBACK
			FSID = Cab.FH * Cab.FD / (2.0 * AISIDE)
			
			if (Cab.FW >= 0.9 * WC):
				FSID = 2.0 * FSID
				
			TCABT = FTOP * Cab.TFRZ + (1.0 - FTOP) * Cab.TFF
			TCABB = FBAK * Cab.TFRZ + (1.0 - FBAK) * Cab.TFF
			TCABS = FSID * Cab.TFRZ + (1.0 - FSID) * Cab.TFF 

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# The average insualtion conductivity.
		# Cab.DKIN is the door insulation conductivity.
		# Cab.RKIN is the side, back, top and bottom insulation conductivity, i.e. the cabinet.
		#
		
		TAVGL = 0.5 * (Cab.DKIN + Cab.RKIN)
		#
		# Calculate the cabinet heat leak as the sum of the top, sides, bottom, front and back heat leaks.
		#
		# NOTE
		# 	The cabinet has six wall sections (top, bottom, left side, right side, bottom and back), 12 edges and 8corners.
		# 	The door, which involves 4 edges and 4 corners, has in addition to the edge and corner effect, 
		#		a gasket and a Cab.WEDGE heat leak added in the shape factor for an edge is 0.54 * Length of the edge
		# The shape factor for a corner is 0.15 * Wall thickness. (Holman p. 54).
		#
		# Note that most corners have two or three insulation thicknesses, thus we average all three thicknesses.
		#
		# TITOP 	Insulation thickness on top (FT)
		# TIRSD 	insulation thickness on the Right side (FT)
		# TILSD 	Insulation thickness on the Left side (FT)
		# TIBK 		Insulation thickness on the back (FT)
		# TIFRT 	Insulation thickness on the front (FT)
		# Cab.BINSUL 	Insulation thickness on the bottom (FT)
		#
		# Cab.R is the conduction resismath.tance. It is the sum of the wall resismath.
		#		tance plus the edge resismath.tance plus the corner resismath.tance in that order.
		#
		# The Left Side Wall Resismath.tance (Cab.R) and heat leak (Cab.QLSIDE)
		# The side walls have two Depth and two Height length edges
		# The edge shape factor is divided by two because each edge is shared by two walls.
		#
		# The corner shape factor is divided by three because each corner is shared by three walls.
		# 		(Actually each corner shape factor is divided by 9 
		#		because the three corner thicknesses are averaged (9 = 3X3))

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the left side
		#
		if (Cab.NCCTYPE == 1) :
			Cab.R = AISIDE / TILSD + 0.54 * (2.0 * DC + 2.0 * HC) / 2.0 + 	\
				0.15 * (4.0 * TILSD + 2.0 * TIBK + 2.0 * Cab.BINSUL + 2.0 * TITOP + 2.0 * TIFRT) / 9.0

		if (Cab.NCCTYPE == 2) :
			Cab.R = AISIDE / TILSD + 0.54 * (H1 + H2 + D1 + D2 + DC) / 2.0		\
				+ 0.15 * ((2.0 * Cab.BINSUL + TILSD) * FALPHA		\
				+ (3.0 * TILSD + 2.0 * TITOP + 2.0 * TIFRT + Cab.BINSUL + TIBK)	\
				+ (TIBK + TILSD + Cab.BINSUL) * FBETA) / 9.0

		if (Cab.NCCTYPE == 3) :
			Cab.R = AISIDE / TILSD + 0.54 * (H1 + H2 + D1 + D2 + D3 + DC) / 2.0	\
				+ 0.15 * (2.0 * (2.0 * Cab.BINSUL + TILSD) * FALPHA	\
				+ (2.0 * (2.0 * TILSD + Cab.BINSUL + TIBK + TITOP + TIFRT))) / 9.0
		
		Cab.R1 = 1.0 / (Cab.R * Cab.RKIN) + 1.0 / (Cab.HILFT * AISIDE)
		Cab.R2 = 1.0 / (Cab.HOLFT * AOSIDE)
		
		loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TCABS, Cab.TLSIDE)
		Cab.QLSIDE = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		# Calculate the heat leak out of the right side
		#
		if (Cab.NCCTYPE == 1) :
			Cab.R = AISIDE / TIRSD + 0.54 * (2.0 * DC + 2.0 * HC) / 2.0 + \
				0.15 * (4.0 * TIRSD + 2.0 * TIBK + 2.0 * Cab.BINSUL + 2.0 * TITOP + 2.0 * TIFRT) / 9.0

		if (Cab.NCCTYPE == 2) :
			Cab.R = AISIDE / TIRSD + 0.54 * (H1 + H2 + D1 + D2 + DC) / 2.0	\
			 + 0.15 * ((2.0 * Cab.BINSUL + TIRSD) * FALPHA	\
			 + (3.0 * TIRSD + 2.0 * TITOP + 2.0 * TIFRT + Cab.BINSUL + TIBK)	\
			 + (TIBK + TIRSD + Cab.BINSUL) * FBETA) / 9.0

		if (Cab.NCCTYPE == 3) :
			Cab.R = AISIDE / TIRSD + 0.54 * (H1 + H2 + D1 + D2 + D3 + DC) / 2.0	\
				+ 0.15 * (2.0 * (2.0 * Cab.BINSUL + TIRSD) * FALPHA			\
				+ (2.0 * (2.0 * TIRSD + Cab.BINSUL + TIBK + TITOP + TIFRT))) / 9.0

		Cab.R1 = 1.0 / (Cab.R * Cab.RKIN) + 1.0 / (Cab.HIRGT * AISIDE)
		Cab.R2 = 1.0 / (Cab.HORGT * AOSIDE)
		
		loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TCABS, Cab.TRSIDE)
		Cab.QRSIDE = loc_list[0]
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		# Calculate the heat leak out of the top the top has two Depth and two Width length edges
		#
		Cab.R = AITOP / TITOP + 0.54 * (2.0 * DC + 2.0 * WC) / 2.0	\
			+ 0.15 * (2. * TIRSD + 2. * TILSD + 2. * TIBK + 4. * TITOP + 2. * TIFRT) / 9.0
			
		Cab.R1 = 1.0 / (Cab.R * Cab.RKIN) + 1.0 / (Cab.HITOP * AITOP)
		Cab.R2 = 1.0 / (Cab.HOTOP * AOTOP)
		
		loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TCABT, Cab.TTOP)
		Cab.QTOP = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		# Calculate the heat leak out of the back the back has two Height and two Width length edges
		#
		
		if (Cab.NCCTYPE == 1) :
			Cab.R = AIBACK / TIBK + 0.54 * (2.0 * HC + 2.0 * WC) / 2.0 	\
				+ 0.15 * (2. * TILSD + 2. * TIRSD + 4. * TIBK + 2. * TITOP + 2. * Cab.BINSUL) / 9.0

		if (Cab.NCCTYPE == 2) :
			Cab.R = AIBACK / TIBK + 0.54 * (2.0 * H2 + (1 + FBETA) * WC) / 2.0	\
				+ 0.15 * ((TILSD + TIRSD + 2.0 * TIBK + 2.0 * Cab.BINSUL) * FBETA	\
				+ (TILSD + TIRSD + 2.0 * TIBK + 2.0 * TITOP)) / 9.0

		if (Cab.NCCTYPE == 3) :
			Cab.R = AIBACK / TIBK + 0.54 * (2.0 * H2 + 2.0 * WC) / 2.0 +	\
				0.15 * (2. * TILSD + 2. * TIRSD + 4. * TIBK + 2. * TITOP + 2. * Cab.BINSUL) / 9.0

		Cab.R1 = 1.0 / (Cab.R * Cab.RKIN) + 1.0 / (Cab.HIBCK * AIBACK)
		Cab.R2 = 1.0 / (Cab.HOBCK * AOBACK)

		loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TCABB, Cab.TBACK)
		Cab.QBACK = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		# Calculate the heat leak out of the front the front has two Height and two Width length edges
		#
		Cab.R = AIFRNT / TIFRT + 0.54 * (2.0 * HC + 2.0 * WC) / 2.0	\
			+ 0.15 * (2. * TILSD + 2. * TIRSD + 4. * TIFRT + 2. * TITOP + 2. * Cab.BINSUL) / 9.0
			
		Cab.R1 = 1.0 / (Cab.R * Cab.DKIN) + 1.0 / (Cab.HIFRT * AIFRNT)
		Cab.R2 = 1.0 / (Cab.HOFRT * AOFRNT)
		
		loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TBOX, Cab.TFRONT)
		Cab.QFRONT = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		# Calculate the heat leak out of the bottom, both with and without compressor notch
		# The top has two Depth and two Width length edges
		#
		
		if (Cab.NCCTYPE == 1) :
			AOB = AOBTM
			AIB = AIBTM
			
			Cab.R = AIB / Cab.BINSUL + 0.54 * (2.0 * DC + 2.0 * WC) / 2.0	\
				+ 0.15 * (4. * Cab.BINSUL + 2. * TIBK + 2. * TILSD + 2. * TIRSD + 2. * TIFRT) / 9.0
				
			Cab.R1 = 1.0 / (Cab.R * Cab.RKIN) + 1.0 / (Cab.HIBOT * AIB)
			Cab.R2 = 1.0 / (Cab.HOBOT * AOB)
			
			loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TBOX, Cab.TBTM)
			Cab.QBOTTM = loc_list[0]
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

		if (Cab.NCCTYPE == 2) :
			RB1 = AIBTM1 / Cab.BINSUL	\
				+ 0.54 * (WC * (FALPHA + FBETA) + 2.0 * D1) / 2.0	\
				+ 0.15 * ((2.0 * TIBK + 2.0 * Cab.BINSUL + TILSD + TIRSD) * FBETA	\
				+ (4.0 * Cab.BINSUL + TILSD + TIRSD) * FALPHA) / 9.0	
			 
			Cab.R1 = 1.0 / (RB1 * Cab.RKIN) + 1.0 / (Cab.HIBOT * AIBTM1)
			Cab.R2 = 1.0 / (Cab.HOBOT * AOBTM1)

			loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TBOX, Cab.TBTM)
			Cab.QBOTTM1 = loc_list[0]
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

			RB2 = AIBTM2 / Cab.BINSUL	\
				+ 0.54 * (WC * (1.0 + FALPHA) + 2.0 * D2) / 2.0	\
				+ 0.15 * ((4.0 * Cab.BINSUL + TILSD + TIRSD) * FALPHA	\
				+ (2.0 * Cab.BINSUL + 2.0 * TIFRT + TILSD + TIRSD)) / 9.0
				
			Cab.R1 = 1.0 / (RB2 * Cab.RKIN) + 1.0 / (Cab.HIBOT * AIBTM2)
			Cab.R2 = 1.0 / (Cab.HOBOT * AOBTM2)
			
			loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TBOX, Cab.TBTM)
			Cab.QBOTTM2 = loc_list[0]
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
			
			Cab.QBOTTM = Cab.QBOTTM1 + Cab.QBOTTM2
			
		if (Cab.NCCTYPE == 3) :
			RB1 = AIBTM1 / Cab.BINSUL + 0.54 * (WC * (1 + FALPHA) + 2.0 * D1) / 2.0	\
				+ 0.15 * ((4.0 * Cab.BINSUL + TILSD + TIRSD) * FALPHA	\
				+ (2.0 * Cab.BINSUL + 2.0 * TIBK + TILSD + TIRSD)) / 9.0
				
			Cab.R1 = 1.0 / (RB1 * Cab.RKIN) + 1.0 / (Cab.HIBOT * AIBTM1)
			Cab.R2 = 1.0 / (Cab.HOBOT * AOBTM1)
			
			loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TBOX, Cab.TBTM)
			Cab.QBOTTM1 = loc_list[0]
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
			
			RB2 = AIBTM2 / Cab.BINSUL + 0.54 * (2.0 * WC * FALPHA + 2.0 * D2) / 2.0	\
				+ 0.15 * (8.0 * Cab.BINSUL + 2.0 * TILSD + 2.0 * TIRSD) * FALPHA / 9.0
				
			Cab.R1 = 1.0 / (RB2 * Cab.RKIN) + 1.0 / (Cab.HIBOT * AIBTM2)
			Cab.R2 = 1.0 / (Cab.HOBOT * AOBTM2)
			
			loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TBOX, Cab.TBTM)
			Cab.QBOTTM2 = loc_list[0]
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
			
			RB3 = AIBTM3 / Cab.BINSUL + 0.54 * ((1 + FALPHA) * WC + 2.0 * D3) / 2.0	\
				+ 0.15 * ((4.0 * Cab.BINSUL * TILSD + TIRSD) * FALPHA	\
				+ (2.0 * Cab.BINSUL + 2.0 * TIFRT + TILSD + TIRSD)) / 9.0
				
			Cab.R1 = 1.0 / (RB3 * Cab.RKIN) + 1.0 / (Cab.HIBOT * AIBTM3)
			Cab.R2 = 1.0 / (Cab.HOBOT * AOBTM3)

			loc_list = self.getRadHeatFlux (Cab.R1, Cab.R2, TBOX, Cab.TBTM)
			Cab.QBOTTM3 = loc_list[0]
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
			
			Cab.QBOTTM = Cab.QBOTTM1 + Cab.QBOTTM2 + Cab.QBOTTM3
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		# Sum all the heat leaks to get the cabinet heat leak.
		#
		Cab.QFRZ = Cab.QLSIDE + Cab.QRSIDE + Cab.QTOP + Cab.QBACK + Cab.QFRONT + Cab.QBOTTM
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		# CALCULATE GASKET HEAT LEAK
		#
		
		Cab.QGZN = 24.0 * (Cab.HEIGHT - Cab.BOTTOM + Cab.WIDTH) * Cab.HLFZG * (Cab.TROOM - TBOX)
		Cab.QGZF = Cab.QGZN
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		# CALCULATE Cab.WEDGE HEAT LEAK
		#
		
		Cab.QW = 0
		
		if (Cab.WEDGE != 0.0) :
			THETAL = math.atan((TILSD - Cab.FLANGE) / Cab.WEDGE)
			THETAR = math.atan((TIRSD - Cab.FLANGE) / Cab.WEDGE)

			if (((TILSD - Cab.FLANGE) == 0.0) or ((TIRSD - Cab.FLANGE) == 0.0)):
				print(' In subroutine QL467, we are dividing by zero 	\
					\n because TIRSD / TILSD (insulation thickness on the side)		\
					\n and Cab.FLANGE (depth of top and side flanges)				\
					\n are equal. TIRSD / TILSD should be larger. TIRSD, TILSD ,	\
					\n and Cab.FLANGE are respectively : %15.7f \t %15.7f \t %15.7f ' %(TIRSD, TILSD, Cab.FLANGE) )
				
				sys.exit('2000')	# End app , not found in Fortran, to prevent div by zero

			AWEDGL = Cab.WEDGE * (TILSD / (TILSD - Cab.FLANGE))
			AWEDGR = Cab.WEDGE * (TIRSD / (TIRSD - Cab.FLANGE))
			BWEDGL = AWEDGL - Cab.WEDGE
			BWEDGR = AWEDGR - Cab.WEDGE
			
			WL1 = 2.0 * HC + WC
			WL2 = 2.0 * (HC + Cab.BINSUL - Cab.FLGB + TITOP - Cab.FLANGE) + Cab.WIDTH - 2.0 * Cab.FLANGE
			HOWDG = (Cab.HOLFT + Cab.HORGT + Cab.HOTOP + Cab.HOBOT) / 4.0

			QWC = (1.0 / (1.0 / (HOWDG * Cab.WEDGE * (2.0 * (Cab.HEIGHT - Cab.BOTTOM) + Cab.WIDTH))	\
				+ (THETAR / (Cab.WKIN * math.log(AWEDGR / BWEDGR) * (WL1 + WL2) / 2.0) 		\
				+ THETAL / (Cab.WKIN * math.log(AWEDGL / BWEDGL) * (WL1 + WL2) / 2.0)) / 2.0)) * (Cab.TROOM - TBOX)

			THETA = math.atan((Cab.BINSUL - Cab.FLGB) / Cab.WEDGE)

			if ((Cab.BINSUL - Cab.FLGB) == 0.0):
				print ( ' In subroutine QL467, we are dividing by zero 	\
						\n because Cab.BINSUL (bottom insulation thickness) 	\
						\n and Cab.FLGB (bottom flange depth) are equal. 	\
						\n Cab.BINSUL should be larger than Cab.FLGB. Cab.BINSUL & Cab.FLGB are,	\
						\n respectively : %15.7f \t %15.7f' %( Cab.BINSUL, Cab.FLGB)  )
					
				sys.exit('2010')	# End app

			AWEDGE = Cab.WEDGE * (Cab.BINSUL / (Cab.BINSUL - Cab.FLGB))
			BWEDGE = AWEDGE - Cab.WEDGE
			
			# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
			# SUM VARIOUS COMPONENTS OF HEAT LEAK
			#
			QWB = (1.0 / (1.0 / (HOWDG * Cab.WEDGE * Cab.WIDTH)		\
				+ THETA / (Cab.WKIN * math.log(AWEDGE / BWEDGE) 		\
				* (WC + Cab.WIDTH - 2.0 * Cab.FLANGE) / 2.0))) * (Cab.TBTM - TBOX)

			Cab.QW = QWB + QWC

		Cab.QTON = Cab.QFRZ + Cab.QGZN + Cab.QW
		Cab.QTOF = Cab.QFRZ + Cab.QGZF + Cab.QW

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		# The heat leak due to Door Openings
		HFFC = 0.0
		WFFC = 0.0
		DFFC = 0.0
		HFRZ = HC
		WFRZ = WC
		DFRZ = DC

		if (Cab.NMOD != 4) :
			para_TFRZ = Cab.TFRZ
		else:
			para_TFRZ = Cab.TFF
			
		obj_doorpn = DoorOpen().DoorpnBuilder ()	\
			.withTempFFSetpoint (Cab.TFF)		\
			.withTempFZSetpoint (para_TFRZ)		\
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
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 