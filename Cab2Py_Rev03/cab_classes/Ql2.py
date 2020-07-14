# Python import
import math, sys, datetime

# User Import
from .CabUtils import CabUtils
from .Doorpn import Doorpn

class Ql2 (CabUtils):
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	# Job			: CALCULATE CABINET HEAT LEAKS FOR CONFIGURATION 2
	#				  Configuration 2: Side by Side R/F
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	def __init__(self, Cab):
		# FIGURE 1: CONFIGURATION 2 SIDE BY SIDE R/F
		#
		#        __________________**_____________
		#       /|   /|    / |
		#      / |  / |   /  |
		#     /  |        /  |  /   |
		#    /   |      ***  | /    |
		#   /    |      /    |       /     |
		#  /     |     /     |      /      |
		# /_________________ **___________/       |
		#       |       |    |     |       |
		#       |       |    |      |     |       |
		#       |       |    |      |     |       |
		#       |       |    |      |     |       |
		#       |       |    |      |     |       |
		#       |       |    |     ***    |       |
		#       |       |    |      |     |       |
		#       |       |    |      |     |       |
		#       |       |   ***     |     |       |
		#       |       |    |      |     |       |
		#       |       |    |      |     |       |
		#       |       |    |      |     |       |
		#       |       |    |      |     |       |
		#       |       |    |      |     |       |
		#       |       |___________|_____**_____________|
		#       |      /     |      /     |      /
		#       |     /      |     /      |     /
		#       |    /       |    /       |    /
		#       |   /        |  ***       |   /
		#       |  /  |  /  |  /
		#       | /   | /   | /
		#       |/_________________**/___________|/
		#
		#     NOTE: THE FREEZER COMPARTMENT IS ON THE RIGHT
		#
		# ** = Heat leak through corner is accounted for in the mullion heat leak calculation.
		# *** = Heat leak through edge is accounted for in the mullion heat leak calculation.
		#
		# Calculate the internal dimensions of the two compartments, freezer (FRZ), and fresh food compartment (FFC)

		#	Internal dimensions
		loc_TIFS = Cab.TIFRS
		loc_TIRS = Cab.TIRLS

		loc_HFRZ = Cab.HEIGHT - Cab.BINFRZ - Cab.TIFT
		loc_DFRZ = Cab.DEPTH  - Cab.TIFF   - Cab.TIFB  - Cab.WEDGE - Cab.DGSKT
		loc_WFRZ = Cab.WIDTH  - Cab.WALL   - Cab.THMUL - loc_TIFS

		loc_DFFC = Cab.DEPTH  - Cab.TIRF   - Cab.TIRB  - Cab.WEDGER - Cab.DGSKT
		loc_WFFC = Cab.WALL   - loc_TIRS
		loc_HFFC = Cab.HEIGHT - Cab.BINSUL - Cab.TIRT

		loc_ALPHA = 0.0
		loc_BETA = 0.0

		if Cab.NCCTYPE == 2:
			loc_BETA = math.atan(Cab.CDDN/Cab.CCHGT)
			loc_ALPHA = math.pi /4.0 - loc_BETA / 2.0

			loc_H1F = Cab.HEIGHT - Cab.BINSUL - Cab.TIRT
			loc_H1Z = Cab.HEIGHT - Cab.BINFRZ - Cab.TIFT

			loc_HTRIANF = Cab.CCHGT - Cab.BINSUL + Cab.BINSUL / math.sin(loc_BETA) - Cab.TIRB / math.tan(loc_BETA)
			loc_HTRIANZ = Cab.CCHGT - Cab.BINFRZ + Cab.BINFRZ / math.sin(loc_BETA) - Cab.TIFB / math.tan(loc_BETA)

			loc_H2F= loc_H1F - loc_HTRIANF
			loc_H2Z= loc_H1Z - loc_HTRIANZ

			loc_DCF = Cab.DEPTH - Cab.TIRF - Cab.TIRB - Cab.WEDGER - Cab.DGSKT
			loc_DCZ = Cab.DEPTH - Cab.TIFF - Cab.TIFB - Cab.WEDGE - Cab.DGSKT

			loc_D1F= loc_HTRIANF / math.cos(loc_BETA)
			loc_D1Z= loc_HTRIANZ / math.cos(loc_BETA)

			loc_D2F= loc_DCF - loc_HTRIANF * math.tan(loc_BETA)
			loc_D2Z= loc_DCZ - loc_HTRIANZ * math.tan(loc_BETA)

		if Cab.NCCTYPE == 3:
			loc_BETA = math.atan((Cab.CDDN-Cab.CDUP)/Cab.CCHGT)
			loc_ALPHA = math.pi /4.0 - loc_BETA / 2.0

			loc_H1F = Cab.HEIGHT - Cab.BINSUL - Cab.TIRT
			loc_H1Z = Cab.HEIGHT - Cab.BINFRZ - Cab.TIFT

			loc_H2F = loc_H1F - Cab.CCHGT
			loc_H2Z = loc_H1Z - Cab.CCHGT

			loc_D1F = Cab.CDUP - Cab.TIRB
			loc_D1Z = Cab.CDUP - Cab.TIFB

			if (Cab.CDDN-Cab.CDUP) == 0.0 :
				loc_D2F = Cab.CCHGT - Cab.BINSUL
				loc_D2Z = Cab.CCHGT - Cab.BINFRZ
			else:
				loc_D2F = (Cab.CDDN - Cab.CDUP) /  math.sin(loc_BETA) - Cab.BINSUL * math.tan(loc_ALPHA)
				loc_D2Z = (Cab.CDDN - Cab.CDUP) /  math.sin(loc_BETA) - Cab.BINFRZ * math.tan(loc_ALPHA)
				
			loc_D3F = Cab.DEPTH - Cab.CDDN - Cab.TIRF - Cab.WEDGER -Cab.BINSUL * math.tan(loc_ALPHA) - Cab.DGSKT
			loc_D3Z = Cab.DEPTH - Cab.CDDN - Cab.TIFF - Cab.WEDGE - Cab.BINFRZ * math.tan(loc_ALPHA) - Cab.DGSKT

			loc_DCF = Cab.DEPTH - Cab.WEDGER - Cab.TIRF - Cab.TIRB - Cab.DGSKT
			loc_DCZ = Cab.DEPTH - Cab.WEDGE  - Cab.TIFF - Cab.TIFB - Cab.DGSKT
			
		loc_FALPHA = 4.0 * loc_ALPHA / math.pi
		loc_FBETA  = 2.0 * loc_BETA  / math.pi

		#
		# CALCULATE INTERNAL SURFACE AREAS
		#	Internal area of the left (fresh food) side   	     	loc_AILSDE
		#	Internal area of the right (freezer) side   		loc_AIRSDE
		#	Internal area of the front or back fresh food side 		loc_AILBCK
		#	Internal area of the front or back freezer side    		loc_AIRBCK
		#	Internal area of the top fresh food side    		loc_AILTOP
		#	Internal area of the bottom fresh food side
		#		not including the compressor area since the insulation
		#		may (but does not have to be) thinner there			loc_AILBOT
		#	Internal area of the top or bottom freezer side  		loc_AIRTOP
		#	Area of the compressor									AOCOMP

		#	Internal Areas
		loc_AILTOP = loc_WFFC * loc_DFFC
		loc_AIRTOP = loc_WFRZ * loc_DFRZ
		loc_AILFNT = loc_WFFC * loc_HFFC
		loc_AIRFNT = loc_WFRZ * loc_HFRZ

		if Cab.NCCTYPE == 1:
			loc_AILSDE = loc_HFFC * loc_DFFC
			loc_AIRSDE = loc_HFRZ * loc_DFRZ
			loc_AILBCK = loc_WFFC * loc_HFFC
			loc_AIRBCK = loc_WFRZ * loc_HFRZ
			loc_AILBOT = loc_AILTOP
			loc_AIRBOT = loc_AIRTOP

		elif Cab.NCCTYPE == 2:
			loc_AILSDE = loc_DCF  * loc_H2F + (loc_DCF + loc_D2F) * (loc_H1F-loc_H2F) / 2.0
			loc_AIRSDE = loc_DCZ  * loc_H2Z + (loc_DCZ + loc_D2Z) * (loc_H1Z-loc_H2Z) / 2.0
			loc_AILBCK = loc_WFFC * loc_H2F
			loc_AIRBCK = loc_WFRZ * loc_H2Z
			loc_AILBTM1 = loc_WFFC * loc_D1F
			loc_AIRBTM1 = loc_WFRZ * loc_D1Z
			loc_AILBTM2 = loc_WFFC * loc_D2F
			loc_AIRBTM2 = loc_WFRZ * loc_D2Z

		elif Cab.NCCTYPE == 3:
			loc_AILSDE = loc_DCF  * loc_H2F + Cab.CCHGT * (loc_D3F + loc_D3F + Cab.CCHGT *  math.tan(loc_BETA)) / 2.0
			loc_AIRSDE = loc_DCZ  * loc_H2Z + Cab.CCHGT * (loc_D3Z + loc_D3Z + Cab.CCHGT *  math.tan(loc_BETA)) / 2.0
			loc_AILBCK = loc_WFFC * loc_H2F
			loc_AIRBCK = loc_WFRZ * loc_H2Z
			loc_AILBTM1 = loc_WFFC * (loc_D1F + Cab.BINSUL *  math.tan(loc_ALPHA))
			loc_AIRBTM1 = loc_WFRZ * (loc_D1Z + Cab.BINFRZ *  math.tan(loc_ALPHA))
			loc_AILBTM2 = loc_WFFC * (loc_D2F + Cab.BINSUL *  math.tan(loc_ALPHA))
			loc_AIRBTM2 = loc_WFRZ * (loc_D2Z + Cab.BINFRZ *  math.tan(loc_ALPHA))
			loc_AILBTM3 = loc_WFFC * loc_D3F
			loc_AIRBTM3 = loc_WFRZ * loc_D3Z

		# CALCULATE EXTERNAL SURFACE AREAS
		# loc_AOLSDE  The external area of the left (fresh food) side  		
		# loc_AORSDE  The external area of the right (freezer) side   		
		# loc_AOLBCK  The external area of the front or back fresh food side 	
		# loc_AORBCK  The external area of the front or back freezer side    	
		# loc_AOLTOP  The external area of the top fresh food side    		
		#				The external area of the bottom fresh food side -
		# 				not including the compressor area since the insulation
		# loc_AOLBOT  may (but does not have to be) thinner there   		
		# loc_AORTOP  The external area of the top or bottom freezer side    	
		# AOCOMP	  Area of the compressor									

		loc_AOLSDE = (Cab.HEIGHT-Cab.BOTTOM) * (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT)
		loc_AORSDE = (Cab.HEIGHT-Cab.BOTTOM) * (Cab.DEPTH - Cab.WEDGE  - Cab.DGSKT)
		loc_AOLBCK = (Cab.HEIGHT-Cab.BOTTOM) * (Cab.WALL  + Cab.THMUL / 2.0)
		loc_AORBCK = (Cab.HEIGHT-Cab.BOTTOM) * (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0)
		loc_AOLTOP = (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT) * (Cab.WALL  + Cab.THMUL / 2.0)
		loc_AORTOP = (Cab.DEPTH - Cab.WEDGE  - Cab.DGSKT) * (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0)
		loc_AOLBOT = loc_AOLTOP
		loc_AORBOT = loc_AORTOP
		loc_AOLFNT = loc_AOLBCK
		loc_AORFNT = loc_AORBCK

		if Cab.NCCTYPE == 2:
			loc_AOLSDE = loc_AOLSDE - Cab.CDDN * Cab.CCHGT / 2.0
			loc_AORSDE = loc_AORSDE - Cab.CDDN * Cab.CCHGT / 2.0
			loc_AOLBCK = loc_AOLBCK - Cab.CCHGT * (Cab.WALL + Cab.THMUL/2.0)
			loc_AORBCK = loc_AORBCK - Cab.CCHGT * (Cab.WIDTH - Cab.WALL - Cab.THMUL/2.0)
			loc_AOLBTM1 = (Cab.WALL  + Cab.THMUL / 2.0) * Cab.CCHGT/ math.cos(loc_BETA)
			loc_AORBTM1 = (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0) * Cab.CCHGT/ math.cos(loc_BETA)
			loc_AOLBTM2 = (Cab.WALL  + Cab.THMUL / 2.0) *( Cab.DEPTH - Cab.CDDN- Cab.WEDGER- Cab.DGSKT)
			loc_AORBTM2 = (Cab.WIDTH - Cab.WALL-Cab.THMUL / 2.0) * (Cab.DEPTH - Cab.CDDN - Cab.WEDGE - Cab.DGSKT)

		if Cab.NCCTYPE == 3:
			loc_AOLSDE = loc_AOLSDE - (Cab.CDDN + Cab.CDUP) * Cab.CCHGT/2.0
			loc_AORSDE = loc_AORSDE - (Cab.CDDN + Cab.CDUP) * Cab.CCHGT/2.0
			loc_AOLBCK = loc_AOLBCK - Cab.CCHGT * (Cab.WALL + Cab.THMUL/2.0)
			loc_AORBCK = loc_AORBCK - Cab.CCHGT * (Cab.WIDTH - Cab.WALL - Cab.THMUL/2.0)
			loc_AOLBTM1 = (Cab.WALL  + Cab.THMUL/2.0) * Cab.CDUP
			loc_AORBTM1 = (Cab.WIDTH - Cab.WALL - Cab.THMUL/2.0) * Cab.CDUP
			loc_AOLBTM2 = (Cab.WALL  + Cab.THMUL/2.0) * Cab.CCHGT/ math.cos(loc_BETA)
			loc_AORBTM2 = (Cab.WIDTH - Cab.WALL - Cab.THMUL/2.0) * Cab.CCHGT/ math.cos(loc_BETA)
			loc_AOLBTM3 = (Cab.WALL  + Cab.THMUL/2.0) * (Cab.DEPTH - Cab.CDDN - Cab.WEDGER - Cab.DGSKT)
			loc_AORBTM3 = (Cab.WIDTH - Cab.WALL-Cab.THMUL/2.0) * (Cab.DEPTH- Cab.CDDN - Cab.WEDGER - Cab.DGSKT)

		# Calculate the average insulation conductivity.
		#Cab.DKINFZ		Freezer door insulation.
		#Cab.DKINFF		Fresh food door insulation conductivity
		#Cab.RKINFF		Insulation conductivity for the sides, back, top and bottom of the fresh food compartment.
		#Cab.RKINFZ		Insulation conductivity for the Freezer.
		#

		loc_TAVGL = 0.25*(Cab.DKINFF + Cab.RKINFF + Cab.DKINFZ + Cab.RKINFZ)
		#
		#   "loc_TAVGL CALCULATION" ADDED BY A.ESPOSITO 7DEC89.
		#
		# Calculate the cabinet heat leak as the sum of the top, sides, bottom, front and back heat leaks.
		#
		# NOTE 
		# The cabinet has six wall sections (top, bottom, left side, right side, bottom and back), 12 edges and 8
		#    Corners. The door, which involves 4 edges and 4 corners, has in addition to the edge and corner effect,
		#    a gasket and a wedge heat leak added in the shape factor for an edge is 0.54*Length of the edge.
		#    The shape factor for a corner is 0.15*Wall thickness. (Holman p. 54).
		#
		# Note that most corners have two or three insulation thicknesses, thus we average all three thicknesses.
		#
		# Left Side is the Fresh Food, Right Side is the Freezer
		#	Cab.TIRT   insulation thickness on the left top (FT)
		#	Cab.TIFT   insulation thickness on the right top (FT)
		#	loc_TIRS   insulation thickness on the left side (FT)
		#	loc_TIFS   insulation thickness on the right side (FT)
		#	Cab.TIRB   insulation thickness on the left back (FT)
		#	Cab.TIFB   insulation thickness on the right back (FT)
		#	Cab.TIRF   insulation thickness on the left front (FT)
		#	Cab.TIFF   insulation thickness on the right front (FT)
		#	Cab.BINSUL insulation thickness on the left bottom (FT)
		#	Cab.BINFRZ insulation thickness on the right bottom (FT)
		#
		#	loc_R is the conduction resistance. It is the sum of the wall resistance plus the edge resistance plus the
		#	  corner resistance in that order.
		#
		#	The side walls have two depth and one Height length edges
		#	The edge effects are divided by two since two walls share each edge.
		# 	The Corner effects are divided by 3 because each corner shares 3 walls.
		#	(They are actually divided by 9 to average the 3 insulation thicknesses).
		
		
		#	The Left (Fresh Food) Side
		#
		if Cab.NCCTYPE == 1:
			loc_RR = loc_AILSDE / loc_TIRS + 0.54 * (2.0 * loc_DFFC + 2.0 * loc_HFFC) / 2.0
			loc_RR = loc_RR	+ 0.15* ((2.0 * loc_TIRS + Cab.TIRB + 2.0 * Cab.BINSUL + Cab.TIRF) \
				+(2.0 * loc_TIRS + Cab.TIRB + 2.0 * Cab.TIRT + Cab.TIRF)) / 9.0

		if Cab.NCCTYPE == 2:
			loc_RR = loc_AILSDE / loc_TIRS + 0.54*(loc_H1F + loc_H2F + loc_D1F + loc_D2F + loc_DFFC)/2.0 + 0.15 \
				* ((2.0*Cab.BINSUL + loc_TIRS)* loc_FALPHA \
				+ (Cab.TIRB + loc_TIRS + Cab.BINSUL) * loc_FBETA	\
				+ (Cab.TIRF+loc_TIRS+Cab.BINSUL) \
				+ (2.0*loc_TIRS+Cab.TIRB+2.0*Cab.TIRT+Cab.TIRF))/9.0

		if Cab.NCCTYPE == 3:
			loc_RR = loc_AILSDE/loc_TIRS+0.54*(loc_H1F + loc_H2F + loc_D1F + loc_D2F + loc_D3F + loc_DFFC)/2.0
			loc_RR = loc_RR	+ 0.15* (2.0 * (2.0 * Cab.BINSUL + loc_TIRS) * loc_FALPHA	\
				+ (2.0 * (loc_TIRS + Cab.BINSUL) + Cab.TIRF + Cab.TIRB)		\
				+ (2.0 * loc_TIRS  + Cab.TIRB + 2.0 * Cab.TIRT + Cab.TIRF)) / 9.0

		loc_R1 = 1.0/(loc_RR*Cab.RKINFF) + 1.0/(Cab.HI*loc_AILSDE)
		loc_R2 = 1.0/(Cab.HO*loc_AOLSDE)

		loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFF, Cab.TLSIDE)
		Cab.QLSIDE = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#   Calculate the heat leak out of the right (Freezer) side
		if Cab.NCCTYPE == 1:
			loc_RR = loc_AIRSDE / loc_TIFS + 0.54 * (2.0 * loc_DFRZ + 2.0* loc_HFRZ)/2.0 \
					+ 0.15 * ((2.0 * loc_TIFS + Cab.TIFB + 2.0 * Cab.BINFRZ + Cab.TIFF) \
					+ (2.0 * loc_TIFS + Cab.TIFB + 2.0 * Cab.TIFT + Cab.TIFF )) /9.0

		if Cab.NCCTYPE == 2:
			loc_RR = loc_AIRSDE / loc_TIFS + 0.54 * (loc_H1Z + loc_H2Z + loc_D1Z + loc_D2Z + loc_DFRZ) / 2.0 + 0.15 \
					* ((2.0*Cab.BINFRZ+loc_TIFS)*loc_FALPHA        \
					+ (Cab.TIFB + loc_TIFS + Cab.BINFRZ) * loc_FBETA  \
					+ (Cab.TIFF + loc_TIFS + Cab.BINFRZ)							\
					+ (2.0*loc_TIFS + Cab.TIFB + 2.0 * Cab.TIFT + Cab.TIFF))/9.0

		if Cab.NCCTYPE == 3:
			loc_RR = loc_AIRSDE / loc_TIFS+ 0.54* (loc_H1Z + loc_H2Z + loc_D1Z + loc_D2Z + loc_D3Z + loc_DFRZ)/2.0 \
			+ 0.15* (2.0*(2.0*Cab.BINFRZ+loc_TIFS)*loc_FALPHA	\
			+ (2.0*(loc_TIFS+Cab.BINFRZ)+Cab.TIFF+Cab.TIFB)		\
			+ (2.0*loc_TIFS+Cab.TIFB+2.0*Cab.TIFT+Cab.TIFF))/9.0


		loc_R1 = 1.0/(loc_RR*Cab.RKINFZ)+1.0/(Cab.HI*loc_AIRSDE)

		loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFRZ, Cab.TRSIDE)
		Cab.QRSIDE = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#   Calculate the heat leak out of the left (fresh food) top
		#   The top has two Cab.DEPTH and one Width length edges
		#
		loc_R = Cab.RKINFF * (loc_AILTOP/Cab.TIRT + 0.54*(loc_DFFC + loc_WFFC)/2.0  \
				+ 0.15 * (loc_TIRS + Cab.TIRB + Cab.TIRT)/9.0)  \
				+ loc_TAVGL * (0.54 * loc_WFFC/2.0 + 0.15 * (Cab.TIRF + loc_TIRS + Cab.TIRT)/9.0)

		loc_QLTOP = (1.0/(1.0/(Cab.HO * loc_AOLTOP) + 1.0/loc_R \
				+ 1.0/(Cab.HI * loc_AILTOP))) * (Cab.TTOP - Cab.TFF)
				
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#   Calculate the heat leak out of the right (freezer) top
		#   The top has two Cab.DEPTH and one Width length edges

		Loc_R = Cab.RKINFZ   * (loc_AIRTOP/Cab.TIFT + 0.54 * (loc_DFRZ + loc_WFRZ)/2.0	\
			+ 0.15   * (loc_TIFS + Cab.TIFB + Cab.TIFT)/9.0)	\
			+ loc_TAVGL * (0.54 * loc_WFRZ/2.0 + 0.15*(Cab.TIFF + Cab.TIFT + loc_TIFS)/9.0)

		loc_QRTOP = (1.0/(1.0/(Cab.HO*loc_AORTOP) + 1.0/loc_R + 1.0/(Cab.HI*loc_AIRTOP)))*(Cab.TTOP - Cab.TFRZ)

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#   Calculate the heat leak out of the left (fresh food) back
		#   The back has two Height and two Width length edges


		if Cab.NCCTYPE == 1:
			loc_R  = loc_AILBCK/Cab.TIRB + 0.54*(loc_HFFC + 2.0 * loc_WFFC)/2.0 \
				+ 0.15*(2.0*loc_TIRS+2.0 * Cab.TIRB+Cab.TIRT+Cab.BINSUL)/9.0

		if Cab.NCCTYPE == 2:
			loc_R  = loc_AILBCK/Cab.TIRB+0.54*(loc_H2F + loc_WFFC * loc_FBETA + loc_WFFC)/2.0 \
				+ 0.15*(loc_TIRS + Cab.TIRB + Cab.BINSUL)*loc_FBETA/9.0 \
				+ 0.15*(loc_TIRS  +Cab.TIRB + Cab.TIRT)/9.0

		if Cab.NCCTYPE == 3:
			loc_R  = loc_AILBCK/Cab.TIRB + 0.54*(loc_H2F+2.0*loc_WFFC)/2.0	\
				+ 0.15*(2.0*loc_TIRS+2.0 * Cab.TIRB+Cab.TIRT+Cab.BINSUL)/9.0

		loc_R1 = 1.0/(Loc_R * Cab.RKINFF) + 1.0/(Cab.HI*loc_AILBCK)
		loc_R2 = 1.0/(Cab.HO * loc_AOLBCK)

		loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFF, Cab.TBACK)
		Cab.QBACKL = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#   Calculate the heat leak out of the right (freezer) back
		#   The back has two Height and two Width length edges
		if Cab.NCCTYPE == 1:
			loc_R  = loc_AIRBCK/Cab.TIFB + 0.54*(loc_HFRZ + 2.0*loc_WFRZ)/2.0 \
				+ 0.15*(2.0*loc_TIFS+2.0*Cab.TIFB+Cab.TIFT+Cab.BINFRZ)/9.0

		if Cab.NCCTYPE == 2:
			loc_R  = loc_AIRBCK/Cab.TIFB+0.54*(loc_H2Z+loc_WFRZ*loc_FBETA+loc_WFRZ)/2.0 \
				+ 0.15*(loc_TIFS+Cab.TIFB+Cab.BINFRZ)*loc_FBETA/9.0 \
				+ 0.15*(loc_TIFS+Cab.TIFB+Cab.TIFT)/9.0

		if Cab.NCCTYPE == 3:
			loc_R  = loc_AIRBCK/Cab.TIFB + 0.54*(loc_H2Z+2.0*loc_WFRZ)/2.0 \
				+ 0.15*(2.0*loc_TIFS+2.0*Cab.TIFB+Cab.TIFT+Cab.BINFRZ)/9.0


		loc_R1 = 1.0/(loc_R * Cab.RKINFZ) + 1.0/(Cab.HI*loc_AIRBCK)
		loc_R2 = 1.0/(Cab.HO * loc_AORBCK)

		loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFRZ, Cab.TBACK)
		Cab.QBACKR = loc_list[0]

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#    Calculate the heat leak out of the left (fresh food) front
		#
		loc_R = Cab.DKINFF*(loc_AILFNT/Cab.TIRF) + loc_TAVGL*(0.54*(loc_HFFC + 2.0 * loc_WFFC)/2.0 \
			+ 0.15*(2.0*loc_TIRS + 2.0*Cab.TIRF + Cab.TIRT + Cab.BINSUL)/9.0)

		Cab.QFRNTL = (1.0/(1.0/(Cab.HO*loc_AOLFNT) + 1.0/loc_R \
			+ 1.0/(Cab.HI*loc_AILFNT)))*(Cab.TFRONT - Cab.TFF)

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#    Calculate the heat leak out of the right (freezer) front
		#
		loc_R = Cab.DKINFZ*(loc_AIRFNT/Cab.TIFF) + loc_TAVGL * (0.54*(loc_HFRZ + 2.0 * loc_WFRZ)/2.0 \
			+ 0.15*(2.0*loc_TIFS + 2.0*Cab.TIFF + Cab.TIFT + Cab.BINFRZ)/9.0) \

		loc_QFRNTR = (1.0/(1.0/(Cab.HO * loc_AORFNT) + 1.0/loc_R \
			+ 1.0/(Cab.HI * loc_AIRFNT))) * (Cab.TFRONT - Cab.TFRZ)

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#   Calculate the heat leak out of the left (fresh food) bottom
		#
		if Cab.NCCTYPE == 1:
			loc_AOB = loc_AOLBOT
			loc_AIB = loc_AILBOT
			loc_R = loc_AIB/Cab.BINSUL + 0.54*(loc_DFFC + 2.0*loc_WFFC)/2.0 \
				+ 0.15*(2.0 * Cab.BINSUL+Cab.TIRB + 2.0 *loc_TIRS+Cab.TIRF)/9.0

			loc_R1 = 1.0/(loc_R*Cab.RKINFF) + 1.0/(Cab.HI*loc_AIB)
			loc_R2 = 1.0/(Cab.HO*loc_AOB)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFF, Cab.TLSIDE)
			Cab.QLBTTM = loc_list[0]
			#-------------------------------------------------------------

		if Cab.NCCTYPE == 2:
			loc_RB1 = loc_AILBTM1/Cab.BINSUL  \
				+ 0.54*(loc_WFFC * (loc_FALPHA+loc_FBETA) + loc_D1F)/2.0 	\
				+ 0.15*((Cab.TIRB + Cab.BINSUL+loc_TIRS) * loc_FBETA  \
				+ (2.0*Cab.BINSUL + loc_TIRS)*loc_FALPHA)/9.0
			loc_R1 = 1.0/(loc_RB1 * Cab.RKINFF) + 1.0/(Cab.HI * loc_AILBTM1)
			loc_R2 = 1.0/(Cab.HO  * loc_AOLBTM1)


			#  CALL RADTRN(loc_R1,loc_R2,Cab.TFF,Cab.TBTM,
			# .		TRADBT,ERMBOT,EFRBOT,loc_AORBTM1, <not used inside function>
			# .     loc_QBOTTM1,Cab.TRSBOT,
			#		Cab.QRDBOT,   				same as
			#		Cab.QFCBOT 					always zero )

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFF, Cab.TBTM)
			loc_QBOTTM1 = loc_list[0]
			Cab.TRSBOT  = loc_list[1]
			Cab.QRDBOT = loc_list[0]
			Cab.QFCBOT = 0.0
			#-------------------------------------------------------------

			loc_RB2 = loc_AILBTM2/Cab.BINSUL	\
				+ 0.54*(loc_WFFC * (1.0 + loc_FALPHA) + loc_D2F)/2.0   \
				+ 0.15*((2.0 * Cab.BINSUL + loc_TIRS) * loc_FALPHA      \
				+ (Cab.BINSUL + Cab.TIRF + loc_TIRS))/9.0

			loc_R1 = 1.0/(loc_RB2 * Cab.RKINFF) + 1.0/(Cab.HI * loc_AILBTM2)

			loc_R2 = 1.0/(Cab.HO*loc_AOLBTM2)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFF, Cab.TBTM)
			loc_QBOTTM2 = loc_list[0]
			#-------------------------------------------------------------

			Cab.QLBTTM = loc_QBOTTM1 + loc_QBOTTM2


		if Cab.NCCTYPE == 3:
			loc_RB1 = loc_AILBTM1/Cab.BINSUL + 0.54 * (loc_WFFC * (1+loc_FALPHA) + loc_D1F)/2.0   \
				+ 0.15 * ((2.0 * Cab.BINSUL + loc_TIRS) * loc_FALPHA  \
				+ (Cab.BINSUL + Cab.TIRB + loc_TIRS))/9.0

			loc_R1 = 1.0/(loc_RB1*Cab.RKINFF)+1.0/(Cab.HI*loc_AILBTM1)
			loc_R2 = 1.0/(Cab.HO*loc_AOLBTM1)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFF, Cab.TBTM)
			loc_QBOTTM1 = loc_list[0]

			#-------------------------------------------------------------
			loc_RB2 = loc_AILBTM2/Cab.BINSUL+0.54*(2.0*loc_WFFC*loc_FALPHA+loc_D2F)/2.0 \
				+ 0.15 * (4.0*Cab.BINSUL+2.0*loc_TIRS)*loc_FALPHA/9.0

			loc_R1 = 1.0/(loc_RB2*Cab.RKINFF)+1.0/(Cab.HI*loc_AILBTM2)
			loc_R2 = 1.0/(Cab.HO*loc_AOLBTM2)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFF, Cab.TBTM)
			loc_QBOTTM2 = loc_list[0]
			#-------------------------------------------------------------

			loc_RB3 = loc_AILBTM3/Cab.BINSUL+0.54*((1.0 +loc_FALPHA)*loc_WFFC+loc_D3F)/2.0     \
				+ 0.15 * ((2.0*Cab.BINSUL+loc_TIRS)*loc_FALPHA   \
				+ (Cab.BINSUL + Cab.TIRF + loc_TIRS))/9.0

			loc_R1 = 1.0/(loc_RB3*Cab.RKINFF)+1.0/(Cab.HI*loc_AILBTM3)
			loc_R2 = 1.0/(Cab.HO*loc_AOLBTM3)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFF, Cab.TBTM)
			loc_QBOTTM3 = loc_list[0]
			#-------------------------------------------------------------

			Cab.QLBTTM = loc_QBOTTM1+loc_QBOTTM2+loc_QBOTTM3

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# Calculate the heat leak out of the right (freezer) bottom
		#

		if Cab.NCCTYPE == 1:
			loc_AOB = loc_AORBOT
			loc_AIB = loc_AIRBOT
			loc_R = loc_AIB/Cab.BINFRZ + 0.54*(loc_DFRZ + 2.0*loc_WFRZ)/2.0 \
				+ 0.15*(2.0 * Cab.BINFRZ+Cab.TIFB + 2.0 *loc_TIFS+Cab.TIFF)/9.0

			loc_R1 = 1.0/(loc_R*Cab.RKINFZ) + 1.0/(Cab.HI*loc_AIB)
			loc_R2 = 1.0/(Cab.HO*loc_AOB)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFRZ, Cab.TBTM)
			Cab.QRBTTM = loc_list[0]
			#-------------------------------------------------------------

		if Cab.NCCTYPE == 2:
			loc_RB1 = loc_AIRBTM1/Cab.BINFRZ          	\
				+ 0.54*(loc_WFRZ*(loc_FALPHA+loc_FBETA)+loc_D1Z)/2.0	\
				+ 0.15*((Cab.TIFB+Cab.BINFRZ+loc_TIFS)*loc_FBETA   		\
				+ (2.0*Cab.BINFRZ+loc_TIFS)*loc_FALPHA)/9.0

			loc_R1 = 1.0/(loc_RB1*Cab.RKINFZ)+1.0/(Cab.HI*loc_AIRBTM1)
			loc_R2 = 1.0/(Cab.HO*loc_AORBTM1)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFRZ, Cab.TBTM)
			loc_QBOTTM1 = loc_list[0]
			#-------------------------------------------------------------

			loc_RB2 = loc_AIRBTM2/Cab.BINFRZ								\
				+ 0.54*(loc_WFRZ*(1.0+loc_FALPHA)+loc_D2Z)/2.0		\
				+ 0.15*((2.0*Cab.BINFRZ+loc_TIFS)*loc_FALPHA			\
				+ (Cab.BINFRZ+Cab.TIFF+loc_TIFS))/9.0

			loc_R1 = 1.0/(loc_RB2*Cab.RKINFZ)+1.0/(Cab.HI*loc_AIRBTM2)
			loc_R2 = 1.0/(Cab.HO*loc_AORBTM2)
			
			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFRZ, Cab.TBTM)
			loc_QBOTTM2 = loc_list[0]
			#-------------------------------------------------------------
			Cab.QRBTTM = loc_QBOTTM1 + loc_QBOTTM2
			
		if Cab.NCCTYPE == 3:
			loc_RB1 = loc_AIRBTM1/Cab.BINFRZ+0.54*(loc_WFRZ*(1+loc_FALPHA)+loc_D1Z)/2.0 \
				+ 0.15 * ((2.0*Cab.BINFRZ+loc_TIFS)*loc_FALPHA   \
				+ (Cab.BINFRZ+Cab.TIFB+loc_TIFS))/9.0

			loc_R1 = 1.0/(loc_RB1*Cab.RKINFZ)+1.0/(Cab.HI*loc_AIRBTM1)
			loc_R2 = 1.0/(Cab.HO*loc_AORBTM1)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFRZ, Cab.TBTM)
			loc_QBOTTM1 = loc_list[0]
			#-------------------------------------------------------------

			loc_RB2 = loc_AIRBTM2/Cab.BINFRZ+0.54*(2.0*loc_WFRZ*loc_FALPHA+loc_D2Z)/2.0       \
					+ 0.15 * (4.0*Cab.BINFRZ+2.0*loc_TIFS)*loc_FALPHA/9.0

			loc_R1 = 1.0/(loc_RB2*Cab.RKINFZ)+1.0/(Cab.HI*loc_AIRBTM2)
			loc_R2 = 1.0/(Cab.HO*loc_AORBTM2)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFRZ, Cab.TBTM)
			loc_QBOTTM2 = loc_list[0]
			#-------------------------------------------------------------

			loc_RB3 = loc_AIRBTM3/Cab.BINFRZ+0.54*((1+loc_FALPHA)*loc_WFRZ+loc_D3Z)/2.0 \
				   + 0.15 * ((2.0*Cab.BINFRZ+loc_TIFS)*loc_FALPHA   \
				   + (Cab.BINFRZ+Cab.TIFF+loc_TIFS))/9.0

			loc_R1 = 1.0/(loc_RB3*Cab.RKINFZ)+1.0/(Cab.HI*loc_AIRBTM3)
			loc_R2 = 1.0/(Cab.HO*loc_AORBTM3)

			loc_list = self.radtrn (loc_R1, loc_R2, Cab.TFRZ, Cab.TBTM)
			loc_QBOTTM3 = loc_list[0]
			#-------------------------------------------------------------

			Cab.QRBTTM = loc_QBOTTM1+loc_QBOTTM2+loc_QBOTTM3

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#
		# Calculate the heat leak through the mullion
		#
		
		#	loc_AMULL 	mullion surface area on the fresh food side.
		#	loc_AMULR 	mullion surface area on the freezer side.
		
		loc_AMULL = loc_HFFC * loc_DFFC
		loc_AMULR = loc_HFRZ * loc_DFRZ
		Cab.QRBTTM = (1.0/(1.0/(Cab.HIRMUL*loc_AMULL) + 1.0/(Cab.HIFMUL*loc_AMULR) \
				 + Cab.THMUL/(Cab.CKMUL*loc_AMULL)))*(Cab.TFF - Cab.TFRZ)


		#  "Mullion heat leak" added by A.Esposito 7DEC89
		#
		#    Sum all the heat leaks to get the cabinet heat leak.
		#
		Cab.QFFT = Cab.QLSIDE + loc_QLTOP + Cab.QBACKL + Cab.QFRNTL + Cab.QLBTTM - Cab.QRBTTM
		Cab.QFRZ = Cab.QRSIDE + loc_QRTOP + Cab.QBACKR + loc_QFRNTR + Cab.QRBTTM + Cab.QRBTTM

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     CALCULATE GASKET HEAT LEAKS FOR FREEZER FAN ON AND OFF
		#
		Cab.QGZN = 24.0* Cab.HLGZF *(loc_WFRZ + loc_HFRZ)*(Cab.TROOM - Cab.TFRZ)  # Change on 8/26/92
		Cab.QGZF = Cab.QGZN
		Cab.QGR  = 24.0* Cab.HLRG  *(loc_HFFC + loc_WFFC) * (Cab.TROOM - Cab.TFF)
		
		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     CALCULATE HEAT LEAKS FOR THE Cab.WEDGE
		#
		loc_THETA = math.atan((loc_TIFS-Cab.FLANGE)/Cab.WEDGE)

		loc_AWEDGE = Cab.WEDGE * (loc_TIFS/(loc_TIFS-Cab.FLANGE))
		loc_BWEDGE = loc_AWEDGE - Cab.WEDGE

		loc_WL1 = (loc_HFRZ + Cab.TIFT+Cab.BINFRZ-Cab.FLGB+loc_WFRZ+loc_TIFS-2.0 * Cab.FLANGE)
		loc_WL2 = loc_HFRZ + loc_WFRZ
		
		loc_QWFZC = (1.0/(1.0/(Cab.HO * Cab.WEDGE * (Cab.HEIGHT - Cab.BOTTOM + loc_WFRZ + loc_TIFS))	\
				  + loc_THETA/(Cab.WKIN * math.log(loc_AWEDGE/loc_BWEDGE)	\
				  * (loc_WL1+loc_WL2)/2.0))) * (Cab.TROOM-Cab.TFRZ)

		loc_THETA = math.atan((Cab.BINFRZ-Cab.FLGB)/Cab.WEDGE)
		loc_AWEDGE = Cab.WEDGE *(Cab.BINFRZ/(Cab.BINFRZ-Cab.FLGB))
		loc_BWEDGE = loc_AWEDGE - Cab.WEDGE

		loc_QWFZB = (1.0/(1.0/(Cab.HO*Cab.WEDGE*Cab.WIDTH) 	\
				  + loc_THETA/(Cab.WKIN* math.log(loc_AWEDGE/loc_BWEDGE) 	\
				  * (loc_WFRZ+0.5*(loc_TIFS-Cab.FLANGE)))))*(Cab.TBTM-Cab.TFRZ)
				
				
		if Cab.WEDGER != 0:
			loc_THETA = math.atan((loc_TIRS-Cab.FLANGER)/Cab.WEDGER)
			loc_AWEDGE = Cab.WEDGER*(loc_TIRS/(loc_TIRS-Cab.FLANGER))
			loc_BWEDGE = loc_AWEDGE - Cab.WEDGER

			loc_W1 = Cab.WALL + loc_HFFC + Cab.BINSUL - 2.0*Cab.FLANGER - Cab.FLGB
			loc_W2 = loc_HFFC + loc_WFFC

			loc_QWFFC = (1.0/(1.0/(Cab.HO*Cab.WEDGER*(Cab.HEIGHT-Cab.BOTTOM+Cab.WALL))    \
					+ loc_THETA/(Cab.WKINR* math.log(loc_AWEDGE/loc_BWEDGE)	\
					* (loc_W1+loc_W2)/2.0)))*(Cab.TROOM-Cab.TFF)

			loc_THETA = math.atan((Cab.BINSUL-Cab.FLGB)/Cab.WEDGER)
			loc_AWEDGE = Cab.WEDGER * (Cab.BINSUL/(Cab.BINSUL-Cab.FLGB))
			loc_BWEDGE = loc_AWEDGE - Cab.WEDGER

			loc_QWFFB = (1.0/(1.0/(Cab.HO*Cab.WEDGER*Cab.WALL)	\
				+ loc_THETA/(Cab.WKINR* math.log(loc_AWEDGE/loc_BWEDGE)			\
				* (2.0*loc_WFFC+loc_TIRS-Cab.FLANGER)/2.0)))*(Cab.TBTM-Cab.TFF)

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#     SUM THE VARIOUS COMPONENTS OF THE HEAT LEAK
		#
		loc_QW   = loc_QWFZC + loc_QWFZB + loc_QWFFC + loc_QWFFB
		Cab.QWFF = loc_QWFFC + loc_QWFFB
		Cab.QWFZ = loc_QWFZC + loc_QWFZB
		Cab.QGON = Cab.QGR   + Cab.QGZN
		Cab.QGOF = Cab.QGR   + Cab.QGZF
		Cab.QTON = loc_QW    + Cab.QGON + Cab.QFFT + Cab.QFRZ
		Cab.QTOF = loc_QW    + Cab.QGOF + Cab.QFFT + Cab.QFRZ

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		# The heat leak due to Door Openings
		obj_doorpn = Doorpn().DoorpnBuilder ()	\
			.withTempFFSetpoint (Cab.TFF)		\
			.withTempFZSetpoint (Cab.TFRZ)		\
			.withHeighFF ( loc_HFFC) 				\
			.withWidthFF (loc_WFFC)			\
			.withDepthFF (loc_DFFC)			\
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

		Cab.QGZF = lstRes [0]
		Cab.QDFFFS = lstRes [1]
		Cab.QDFZCS = lstRes [2]
		Cab.QDFZFS = lstRes [3]

		Cab.QDFFCL = lstRes [4]
		Cab.QDFFFL = lstRes [5]
		Cab.QDFZCL = lstRes [6]
		Cab.QDFZFL = lstRes [7]
		return
#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=