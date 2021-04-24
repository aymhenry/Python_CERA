# Python import
import math
import sys
import datetime

# User Import
from .CabUtils import CabUtils
from .DoorOpen import DoorOpen


class Ql2(CabUtils):
    # .=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    # Job           : CALCULATE CABINET HEAT LEAKS FOR CONFIGURATION 2
    #                  Configuration 2: Side by Side R/F
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    def __init__(self, Cab):
        # FIGURE 1: CONFIGURATION 2 SIDE BY SIDE R/F
        #
        #        __________________**_____________
        #      /|  /|   / |
        #     / | / |  /  |
        #    /  |       /  | /   |
        #   /   |      ***  | /    |
        #  /    |     /    |      /     |
        # /     |    /     |     /      |
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
        #       |     /     |     /     |     /
        #       |    /      |    /      |    /
        #       |   /       |   /       |   /
        #       |  /        |  ***       |  /
        #       | /  | /  | /
        #       | /   | /   | /
        #       |/_________________**/___________|/
        #
        #     NOTE: THE FREEZER COMPARTMENT IS ON THE RIGHT
        #
        # ** = Heat leak through corner is accounted for in the mullion heat leak calculation.
        # *** = Heat leak through edge is accounted for in the mullion heat leak calculation.
        #
        # Calculate the internal dimensions of the two compartments, freezer(FRZ), and fresh food compartment(FFC)
        #
        #    Internal dimensions
        # -=-=-=-=-=-=-=-=-=-=-
        TIFS = Cab.TIFRS        # thickness of insulation on right side of freezer
        TIRS = Cab.TIRLS        # thickness of insulation on left side of refrigerator

        # Effective height is height without
        #    max. thickness of bottom insulation
        #    thickness of top insulation
        HFRZ = Cab.HEIGHT - Cab.BINFRZ - Cab.TIFT

        # Effective depth is depth without
        #    thickness of insulation on front of freezer
        #    thickness of back insulation
        #    Freezer Freezer Wedge
        #    Door Gasket Thickness
        DFRZ = Cab.DEPTH - Cab.TIFF - Cab.TIFB - Cab.WEDGE - Cab.DGSKT

        # Effective widith is width without
        #    Distance From The Outside Wall Of The Fresh Food
        #    thickness of the mullion
        #    thickness of insulation on right side of freezer
        WFRZ = Cab.WIDTH - Cab.WALL - Cab.THMUL - TIFS

        # Effective Depth is the depth without
        #    thickness of insulation on front of fresh food compartment
        #    thickness of insulation on back of fresh food compartment
        #    Fresh Food Compartment Wedge
        #    Door Gasket Thickness
        DFFC = Cab.DEPTH - Cab.TIRF - Cab.TIRB - Cab.WEDGER - Cab.DGSKT
        
        # Deferance between
        #    Distance From The Outside Wall Of The Fresh Food
        #    thickness of insulation on sides of fresh food compartment.
        WFFC = Cab.WALL - TIRS
        
        # Effective height is height without
        #    maximum thickness of bottom insulation fresh food
        #    thickness of insulation on top of fresh food compartment.
        HFFC = Cab.HEIGHT - Cab.BINSUL - Cab.TIRT

        # inialize valuse
        ALPHA = 0.0
        BETA = 0.0

        # NCC type depends on CCHGT: Compartment Height & CDUP: Top Depth dimensions of compressor
        if Cab.NCCTYPE == 2:
            # Beta angle betwwen Compressor Compartment Bottom and Height
            BETA = math.atan(Cab.CDDN / Cab.CCHGT)   # Compressor Compartment Bottom Depth/ Height
            ALPHA = math.pi / 4.0 - BETA / 2.0
            
            # H1F is height without
            #    Fresh Food Compartment Wedge
            #    Door Gasket Thickness
            H1F = Cab.HEIGHT - Cab.BINSUL - Cab.TIRT
            
            # Effective height is height without
            #    max. thickness of bottom insulation
            #    thickness of top insulation
            H1Z = Cab.HEIGHT - Cab.BINFRZ - Cab.TIFT
            
            # CCHGT:Compressor Compartment Height, BINSUL  Fresh Food Compartment Wedge 
            HTRIANF = Cab.CCHGT - Cab.BINSUL + Cab.BINSUL / math.sin(BETA) - Cab.TIRB / math.tan(BETA)
            HTRIANZ = Cab.CCHGT - Cab.BINFRZ + Cab.BINFRZ / math.sin(BETA) - Cab.TIFB / math.tan(BETA)

            H2F = H1F - HTRIANF
            H2Z = H1Z - HTRIANZ

            DCF = Cab.DEPTH - Cab.TIRF - Cab.TIRB - Cab.WEDGER - Cab.DGSKT
            DCZ = Cab.DEPTH - Cab.TIFF - Cab.TIFB - Cab.WEDGE - Cab.DGSKT

            D1F = HTRIANF / math.cos(BETA)
            D1Z = HTRIANZ / math.cos(BETA)

            D2F = DCF - HTRIANF * math.tan(BETA)
            D2Z = DCZ - HTRIANZ * math.tan(BETA)

        if Cab.NCCTYPE == 3:
            BETA = math.atan((Cab.CDDN-Cab.CDUP) / Cab.CCHGT)
            ALPHA = math.pi / 4.0 - BETA / 2.0

            H1F = Cab.HEIGHT - Cab.BINSUL - Cab.TIRT
            H1Z = Cab.HEIGHT - Cab.BINFRZ - Cab.TIFT

            H2F = H1F - Cab.CCHGT
            H2Z = H1Z - Cab.CCHGT

            D1F = Cab.CDUP - Cab.TIRB
            D1Z = Cab.CDUP - Cab.TIFB

            if(Cab.CDDN-Cab.CDUP) == 0.0:
                D2F = Cab.CCHGT - Cab.BINSUL
                D2Z = Cab.CCHGT - Cab.BINFRZ
            else:
                D2F = (Cab.CDDN - Cab.CDUP) / math.sin(BETA) - Cab.BINSUL * math.tan(ALPHA)
                D2Z = (Cab.CDDN - Cab.CDUP) / math.sin(BETA) - Cab.BINFRZ * math.tan(ALPHA)

            D3F = Cab.DEPTH - Cab.CDDN - Cab.TIRF - Cab.WEDGER - Cab.BINSUL * math.tan(ALPHA) - Cab.DGSKT
            D3Z = Cab.DEPTH - Cab.CDDN - Cab.TIFF - Cab.WEDGE - Cab.BINFRZ * math.tan(ALPHA) - Cab.DGSKT

            DCF = Cab.DEPTH - Cab.WEDGER - Cab.TIRF - Cab.TIRB - Cab.DGSKT
            DCZ = Cab.DEPTH - Cab.WEDGE - Cab.TIFF - Cab.TIFB - Cab.DGSKT

        FALPHA = 4.0 * ALPHA / math.pi
        FBETA = 2.0 * BETA / math.pi

        #
        # CALCULATE INTERNAL SURFACE AREAS
        # AILSDE        Internal area of the left(fresh food) side
        # AIRSDE        Internal area of the right(freezer) side
        # AILBCK        Internal area of the front or back fresh food side
        # AIRBCK        Internal area of the front or back freezer side
        # AILTOP        Internal area of the top fresh food side
        # AILBOT        Internal area of the bottom fresh food side
        #    Internal area of the bottom fresh food side not including the compressor area
        #    since the insulation may(but does not have to be) thinner there
        
        # AIRTOP        Internal area of the top or bottom freezer side
        # AOCOMP            Area of the compressor

        #    Internal Areas
        # -=-=-=-=-=-=-=-=-=-=-
        AILTOP = WFFC * DFFC
        AIRTOP = WFRZ * DFRZ
        AILFNT = WFFC * HFFC
        AIRFNT = WFRZ * HFRZ

        if Cab.NCCTYPE == 1:
            AILSDE = HFFC * DFFC
            AIRSDE = HFRZ * DFRZ
            AILBCK = WFFC * HFFC
            AIRBCK = WFRZ * HFRZ
            AILBOT = AILTOP
            AIRBOT = AIRTOP

        elif Cab.NCCTYPE == 2:
            AILSDE = DCF * H2F + (DCF + D2F) * (H1F-H2F) / 2.0
            AIRSDE = DCZ * H2Z + (DCZ + D2Z) * (H1Z-H2Z) / 2.0
            AILBCK = WFFC * H2F
            AIRBCK = WFRZ * H2Z
            AILBTM1 = WFFC * D1F
            AIRBTM1 = WFRZ * D1Z
            AILBTM2 = WFFC * D2F
            AIRBTM2 = WFRZ * D2Z

        elif Cab.NCCTYPE == 3:
            AILSDE = DCF * H2F + Cab.CCHGT * (D3F + D3F + Cab.CCHGT * math.tan(BETA)) / 2.0
            AIRSDE = DCZ * H2Z + Cab.CCHGT * (D3Z + D3Z + Cab.CCHGT * math.tan(BETA)) / 2.0
            AILBCK = WFFC * H2F
            AIRBCK = WFRZ * H2Z
            AILBTM1 = WFFC * (D1F + Cab.BINSUL * math.tan(ALPHA))
            AIRBTM1 = WFRZ * (D1Z + Cab.BINFRZ * math.tan(ALPHA))
            AILBTM2 = WFFC * (D2F + Cab.BINSUL * math.tan(ALPHA))
            AIRBTM2 = WFRZ * (D2Z + Cab.BINFRZ * math.tan(ALPHA))
            AILBTM3 = WFFC * D3F
            AIRBTM3 = WFRZ * D3Z

        # CALCULATE EXTERNAL SURFACE AREAS
        # AOLSDE  The external area of the left(fresh food) side
        # AORSDE  The external area of the right(freezer) side
        # AOLBCK  The external area of the front or back fresh food side
        # AORBCK  The external area of the front or back freezer side
        # AOLTOP  The external area of the top fresh food side
        #                The external area of the bottom fresh food side -
        #                 not including the compressor area since the insulation
        # AOLBOT  may(but does not have to be) thinner there
        # AORTOP  The external area of the top or bottom freezer side
        # AOCOMP      Area of the compressor

        # external surface areas
        # -=-=-=-=-=-=-=-=-=-=-
        AOLSDE = (Cab.HEIGHT-Cab.BOTTOM) * (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT)
        AORSDE = (Cab.HEIGHT-Cab.BOTTOM) * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT)
        AOLBCK = (Cab.HEIGHT-Cab.BOTTOM) * (Cab.WALL + Cab.THMUL / 2.0)
        AORBCK = (Cab.HEIGHT-Cab.BOTTOM) * (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0)
        AOLTOP = (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT) * (Cab.WALL + Cab.THMUL / 2.0)
        AORTOP = (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT) * (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0)
        AOLBOT = AOLTOP
        AORBOT = AORTOP
        AOLFNT = AOLBCK
        AORFNT = AORBCK

        if Cab.NCCTYPE == 2:
            AOLSDE = AOLSDE - Cab.CDDN * Cab.CCHGT / 2.0
            AORSDE = AORSDE - Cab.CDDN * Cab.CCHGT / 2.0
            AOLBCK = AOLBCK - Cab.CCHGT * (Cab.WALL + Cab.THMUL / 2.0)
            AORBCK = AORBCK - Cab.CCHGT * (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0)
            AOLBTM1 = (Cab.WALL + Cab.THMUL / 2.0) * Cab.CCHGT / math.cos(BETA)
            AORBTM1 = (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0) * Cab.CCHGT / math.cos(BETA)
            AOLBTM2 = (Cab.WALL + Cab.THMUL / 2.0) * (Cab.DEPTH - Cab.CDDN - Cab.WEDGER - Cab.DGSKT)
            AORBTM2 = (Cab.WIDTH - Cab.WALL-Cab.THMUL / 2.0) * (Cab.DEPTH - Cab.CDDN - Cab.WEDGE - Cab.DGSKT)

        if Cab.NCCTYPE == 3:
            AOLSDE = AOLSDE - (Cab.CDDN + Cab.CDUP) * Cab.CCHGT / 2.0
            AORSDE = AORSDE - (Cab.CDDN + Cab.CDUP) * Cab.CCHGT / 2.0
            AOLBCK = AOLBCK - Cab.CCHGT * (Cab.WALL + Cab.THMUL / 2.0)
            AORBCK = AORBCK - Cab.CCHGT * (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0)
            AOLBTM1 = (Cab.WALL + Cab.THMUL / 2.0) * Cab.CDUP
            AORBTM1 = (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0) * Cab.CDUP
            AOLBTM2 = (Cab.WALL + Cab.THMUL / 2.0) * Cab.CCHGT / math.cos(BETA)
            AORBTM2 = (Cab.WIDTH - Cab.WALL - Cab.THMUL / 2.0) * Cab.CCHGT / math.cos(BETA)
            AOLBTM3 = (Cab.WALL + Cab.THMUL / 2.0) * (Cab.DEPTH - Cab.CDDN - Cab.WEDGER - Cab.DGSKT)
            AORBTM3 = (Cab.WIDTH - Cab.WALL-Cab.THMUL / 2.0) * (Cab.DEPTH - Cab.CDDN - Cab.WEDGER - Cab.DGSKT)

        # Calculate the average insulation conductivity.
        # Cab.DKINFZ        Freezer door insulation.
        # Cab.DKINFF        Fresh food door insulation conductivity
        # Cab.RKINFF        Insulation conductivity for the sides, back, top and bottom of the fresh food compartment.
        # Cab.RKINFZ        Insulation conductivity for the Freezer.
        #
        
        TAVGL = 0.25 * (Cab.DKINFF + Cab.RKINFF + Cab.DKINFZ + Cab.RKINFZ)
        #
        #   "TAVGL CALCULATION" ADDED BY A.ESPOSITO 7DEC89.
        #
        # Calculate the cabinet heat leak as the sum of the top, sides, bottom, front and back heat leaks.
        #
        # NOTE
        # The cabinet has six wall sections(top, bottom, left side, right side, bottom and back), 12 edges and 8
        #    Corners. The door, which involves 4 edges and 4 corners, has in addition to the edge and corner effect,
        #    a gasket and a wedge heat leak added in the shape factor for an edge is 0.54 *Length of the edge.
        #    The shape factor for a corner is 0.15 *Wall thickness.(Holman p. 54).
        #
        # Note that most corners have two or three insulation thicknesses, thus we average all three thicknesses.
        #
        # Left Side is the Fresh Food, Right Side is the Freezer
        #    Cab.TIRT   insulation thickness on the left top(FT)
        #    Cab.TIFT   insulation thickness on the right top(FT)
        #    TIRS   insulation thickness on the left side(FT)
        #    TIFS   insulation thickness on the right side(FT)
        #    Cab.TIRB   insulation thickness on the left back(FT)
        #    Cab.TIFB   insulation thickness on the right back(FT)
        #    Cab.TIRF   insulation thickness on the left front(FT)
        #    Cab.TIFF   insulation thickness on the right front(FT)
        #    Cab.BINSUL insulation thickness on the left bottom(FT)
        #    Cab.BINFRZ insulation thickness on the right bottom(FT)
        #
        #    R is the conduction resistance. It is the sum of the wall resistance plus the edge resistance plus the
        #      corner resistance in that order.
        #
        #    The side walls have two depth and one Height length edges
        #    The edge effects are divided by two since two walls share each edge.
        #     The Corner effects are divided by 3 because each corner shares 3 walls.
        #   (They are actually divided by 9 to average the 3 insulation thicknesses).
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #   Calculate the heat leak out of the left(fresh food) side
        #
        if Cab.NCCTYPE == 1:
            RR = AILSDE / TIRS + 0.54 * (2.0 * DFFC + 2.0 * HFFC) / 2.0
            RR = RR + 0.15 * ((2.0 * TIRS + Cab.TIRB + 2.0 * Cab.BINSUL + Cab.TIRF)
                              + (2.0 * TIRS + Cab.TIRB + 2.0 * Cab.TIRT + Cab.TIRF)
                              ) / 9.0

        if Cab.NCCTYPE == 2:
            RR = AILSDE / TIRS + 0.54 * (H1F + H2F + D1F + D2F + DFFC) / 2.0 \
                 + 0.15 * ((2.0 * Cab.BINSUL + TIRS) * FALPHA
                           + (Cab.TIRB + TIRS + Cab.BINSUL) * FBETA
                           + (Cab.TIRF+TIRS+Cab.BINSUL)
                           + (2.0 * TIRS+Cab.TIRB+2.0 * Cab.TIRT + Cab.TIRF)
                           ) / 9.0

        if Cab.NCCTYPE == 3:
            RR = AILSDE/TIRS+0.54 * (H1F + H2F + D1F + D2F + D3F + DFFC) / 2.0
            RR = RR + 0.15 * (2.0 * (2.0 * Cab.BINSUL + TIRS) * FALPHA
                              + (2.0 * (TIRS + Cab.BINSUL) + Cab.TIRF + Cab.TIRB)
                              + (2.0 * TIRS + Cab.TIRB + 2.0 * Cab.TIRT + Cab.TIRF)
                              ) / 9.0

        R1 = 1.0 / (RR * Cab.RKINFF) + 1.0 / (Cab.HI*AILSDE)
        R2 = 1.0 / (Cab.HO * AOLSDE)

        lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TLSIDE)
        Cab.QLSIDE = lst_Feedback[0]

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #   Calculate the heat leak out of the right(Freezer) side
        #
        if Cab.NCCTYPE == 1:
            RR = AIRSDE / TIFS + 0.54 * (2.0 * DFRZ + 2.0 * HFRZ) / 2.0 \
                        + 0.15 * ((2.0 * TIFS + Cab.TIFB + 2.0 * Cab.BINFRZ + Cab.TIFF)
                                  + (2.0 * TIFS + Cab.TIFB + 2.0 * Cab.TIFT + Cab.TIFF)
                                  ) / 9.0

        if Cab.NCCTYPE == 2:
            RR = AIRSDE / TIFS + 0.54 * (H1Z + H2Z + D1Z + D2Z + DFRZ) / 2.0 \
                 + 0.15 * ((2.0 * Cab.BINFRZ + TIFS) * FALPHA
                           + (Cab.TIFB + TIFS + Cab.BINFRZ) * FBETA
                           + (Cab.TIFF + TIFS + Cab.BINFRZ)
                           + (2.0 * TIFS + Cab.TIFB + 2.0 * Cab.TIFT + Cab.TIFF)
                           ) / 9.0

        if Cab.NCCTYPE == 3:
            RR = AIRSDE / TIFS \
                + 0.54 * (H1Z + H2Z + D1Z + D2Z + D3Z + DFRZ) / 2.0 \
                + 0.15 * (2.0 * (2.0 * Cab.BINFRZ+TIFS) * FALPHA
                          + (2.0 * (TIFS + Cab.BINFRZ) + Cab.TIFF + Cab.TIFB)
                          + (2.0 * TIFS + Cab.TIFB + 2.0 * Cab.TIFT + Cab.TIFF)
                          ) / 9.0
        
        R1 = 1.0 / (RR * Cab.RKINFZ) + 1.0 / (Cab.HI*AIRSDE)
        
        lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TRSIDE)
        Cab.QRSIDE = lst_Feedback[0]

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #   Calculate the heat leak out of the left(fresh food) top
        #   The top has two Cab.DEPTH and one Width length edges
        #
        R = Cab.RKINFF * (AILTOP / Cab.TIRT
                          + 0.54 * (DFFC + WFFC) / 2.0
                          + 0.15 * (TIRS + Cab.TIRB + Cab.TIRT) / 9.0) \
            + TAVGL * (0.54 * WFFC / 2.0
                       + 0.15 * (Cab.TIRF + TIRS + Cab.TIRT)
                       / 9.0)

        Cab.QLTOP = (1.0 / (1.0 / (Cab.HO * AOLTOP)
                            + 1.0 / R
                            + 1.0 / (Cab.HI * AILTOP)
                            )
                     ) * (Cab.TTOP - Cab.TFF)

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #   Calculate the heat leak out of the right(freezer) top
        #   The top has two Cab.DEPTH and one Width length edges
        R = Cab.RKINFZ * (AIRTOP / Cab.TIFT
                          + 0.54 * (DFRZ + WFRZ) / 2.0
                          + 0.15 * (TIFS + Cab.TIFB + Cab.TIFT) / 9.0)    \
            + TAVGL * (0.54 * WFRZ / 2.0 + 0.15 * (Cab.TIFF + Cab.TIFT + TIFS) / 9.0)

        Cab.QRTOP = (1.0 / (1.0 / (Cab.HO * AORTOP) + 1.0 / R + 1.0 / (Cab.HI * AIRTOP))) * (Cab.TTOP - Cab.TFRZ)

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #   Calculate the heat leak out of the left(fresh food) back
        #   The back has two Height and two Width length edges
        if Cab.NCCTYPE == 1:
            R = AILBCK / Cab.TIRB + 0.54 * (HFFC + 2.0 * WFFC) / 2.0 \
                + 0.15 * (2.0 * TIRS + 2.0 * Cab.TIRB + Cab.TIRT + Cab.BINSUL) / 9.0

        if Cab.NCCTYPE == 2:
            R = AILBCK / Cab.TIRB + 0.54 * (H2F + WFFC * FBETA + WFFC) / 2.0 \
                + 0.15 * (TIRS + Cab.TIRB + Cab.BINSUL) * FBETA / 9.0 \
                + 0.15 * (TIRS + Cab.TIRB + Cab.TIRT) / 9.0

        if Cab.NCCTYPE == 3:
            R = AILBCK / Cab.TIRB + 0.54 * (H2F+2.0 * WFFC) / 2.0    \
                + 0.15 * (2.0 * TIRS+2.0 * Cab.TIRB + Cab.TIRT + Cab.BINSUL) / 9.0

        R1 = 1.0/(R * Cab.RKINFF) + 1.0 / (Cab.HI * AILBCK)
        R2 = 1.0/(Cab.HO * AOLBCK)

        lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TBACK)
        Cab.QBACKL = lst_Feedback[0]

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #   Calculate the heat leak out of the right(freezer) back
        #   The back has two Height and two Width length edges
        if Cab.NCCTYPE == 1:
            R = AIRBCK / Cab.TIFB + 0.54 * (HFRZ + 2.0 * WFRZ) / 2.0 \
                + 0.15 * (2.0 * TIFS+2.0 * Cab.TIFB + Cab.TIFT+Cab.BINFRZ) / 9.0

        if Cab.NCCTYPE == 2:
            R = AIRBCK / Cab.TIFB+0.54 * (H2Z+WFRZ * FBETA+WFRZ) / 2.0 \
                + 0.15 * (TIFS+Cab.TIFB + Cab.BINFRZ) * FBETA / 9.0 \
                + 0.15 * (TIFS+Cab.TIFB + Cab.TIFT) / 9.0

        if Cab.NCCTYPE == 3:
            R = AIRBCK / Cab.TIFB + 0.54 * (H2Z+2.0 * WFRZ) / 2.0 \
                + 0.15 * (2.0 * TIFS+2.0 * Cab.TIFB + Cab.TIFT+Cab.BINFRZ) / 9.0

        R1 = 1.0/(R * Cab.RKINFZ) + 1.0/(Cab.HI*AIRBCK)
        R2 = 1.0/(Cab.HO * AORBCK)

        lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBACK)
        Cab.QBACKR = lst_Feedback[0]

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #    Calculate the heat leak out of the left(fresh food) front
        #
        R = Cab.DKINFF * (AILFNT / Cab.TIRF) \
            + TAVGL * (0.54 * (HFFC + 2.0 * WFFC) / 2.0
                       + 0.15 * (2.0 * TIRS + 2.0 * Cab.TIRF + Cab.TIRT + Cab.BINSUL) / 9.0)

        Cab.QFRNTL = (1.0 / (1.0 / (Cab.HO * AOLFNT)
                             + 1.0 / R
                             + 1.0 / (Cab.HI * AILFNT)
                             )
                      ) * (Cab.TFRONT - Cab.TFF)

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #    Calculate the heat leak out of the right(freezer) front
        #
        R = Cab.DKINFZ * (AIRFNT / Cab.TIFF) \
            + TAVGL * (0.54 * (HFRZ + 2.0 * WFRZ) / 2.0
                       + 0.15 * (2.0 * TIFS + 2.0 * Cab.TIFF + Cab.TIFT + Cab.BINFRZ) / 9.0)

        Cab.QFRNTR = (1.0/(1.0 / (Cab.HO * AORFNT)
                      + 1.0 / R
                      + 1.0 / (Cab.HI * AIRFNT))
                      ) * (Cab.TFRONT - Cab.TFRZ)

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #   Calculate the heat leak out of the left(fresh food) bottom
        #
        if Cab.NCCTYPE == 1:
            AOB = AOLBOT
            AIB = AILBOT
            R = AIB / Cab.BINSUL + 0.54 * (DFFC + 2.0 * WFFC) / 2.0 \
                + 0.15 * (2.0 * Cab.BINSUL+Cab.TIRB + 2.0 * TIRS + Cab.TIRF) / 9.0

            R1 = 1.0 / (R * Cab.RKINFF) + 1.0/(Cab.HI*AIB)
            R2 = 1.0 / (Cab.HO * AOB)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TLSIDE)
            Cab.QLBTTM = lst_Feedback[0]
            # ------------------------------------------------------------

        if Cab.NCCTYPE == 2:
            RB1 = AILBTM1 / Cab.BINSUL  \
                + 0.54 * (WFFC * (FALPHA+FBETA) + D1F) / 2.0     \
                + 0.15 * ((Cab.TIRB + Cab.BINSUL+TIRS) * FBETA
                          + (2.0 * Cab.BINSUL + TIRS)*FALPHA
                          ) / 9.0
            
            R1 = 1.0/(RB1 * Cab.RKINFF) + 1.0/(Cab.HI * AILBTM1)
            R2 = 1.0/(Cab.HO * AOLBTM1)
            
            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TBTM)
            QBOTTM1 = lst_Feedback[0]
            # ------------------------------------------------------------
            RB2 = AILBTM2 / Cab.BINSUL    \
                + 0.54 * (WFFC * (1.0 + FALPHA) + D2F) / 2.0   \
                + 0.15 * ((2.0 * Cab.BINSUL + TIRS) * FALPHA      
                          + (Cab.BINSUL + Cab.TIRF + TIRS)) / 9.0

            R1 = 1.0/(RB2 * Cab.RKINFF) + 1.0/(Cab.HI * AILBTM2)

            R2 = 1.0/(Cab.HO * AOLBTM2)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TBTM)
            QBOTTM2 = lst_Feedback[0]
            # ------------------------------------------------------------
            Cab.QLBTTM = QBOTTM1 + QBOTTM2

        if Cab.NCCTYPE == 3:
            RB1 = AILBTM1 / Cab.BINSUL + 0.54 * (WFFC * (1+FALPHA) + D1F) / 2.0   \
                + 0.15 * ((2.0 * Cab.BINSUL + TIRS) * FALPHA
                          + (Cab.BINSUL + Cab.TIRB + TIRS)) / 9.0

            R1 = 1.0/(RB1 * Cab.RKINFF)+1.0/(Cab.HI*AILBTM1)
            R2 = 1.0/(Cab.HO * AOLBTM1)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TBTM)
            QBOTTM1 = lst_Feedback[0]

            # ------------------------------------------------------------
            RB2 = AILBTM2 / Cab.BINSUL+0.54 * (2.0 * WFFC * FALPHA+D2F) / 2.0 \
                + 0.15 * (4.0 * Cab.BINSUL+2.0 * TIRS) * FALPHA / 9.0

            R1 = 1.0/(RB2 * Cab.RKINFF)+1.0/(Cab.HI * AILBTM2)
            R2 = 1.0/(Cab.HO * AOLBTM2)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TBTM)
            QBOTTM2 = lst_Feedback[0]
            # ------------------------------------------------------------

            RB3 = AILBTM3 / Cab.BINSUL  \
                + 0.54 * ((1.0 + FALPHA) * WFFC+D3F) / 2.0     \
                + 0.15 * ((2.0 * Cab.BINSUL+TIRS) * FALPHA
                          + (Cab.BINSUL + Cab.TIRF + TIRS)) / 9.0

            R1 = 1.0/(RB3 * Cab.RKINFF)+1.0/(Cab.HI*AILBTM3)
            R2 = 1.0/(Cab.HO * AOLBTM3)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TBTM)
            QBOTTM3 = lst_Feedback[0]
            # ------------------------------------------------------------

            Cab.QLBTTM = QBOTTM1+QBOTTM2+QBOTTM3

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the right(freezer) bottom
        #

        if Cab.NCCTYPE == 1:
            AOB = AORBOT
            AIB = AIRBOT
            R = AIB / Cab.BINFRZ \
                + 0.54 * (DFRZ + 2.0 * WFRZ) / 2.0 \
                + 0.15 * (2.0 * Cab.BINFRZ+Cab.TIFB + 2.0 * TIFS + Cab.TIFF) / 9.0

            R1 = 1.0/(R * Cab.RKINFZ) + 1.0/(Cab.HI*AIB)
            R2 = 1.0/(Cab.HO * AOB)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            Cab.QRBTTM = lst_Feedback[0]
            # ------------------------------------------------------------

        if Cab.NCCTYPE == 2:
            RB1 = AIRBTM1 / Cab.BINFRZ              \
                + 0.54 * (WFRZ * (FALPHA+FBETA)+D1Z) / 2.0    \
                + 0.15 * ((Cab.TIFB + Cab.BINFRZ+TIFS) * FBETA
                          + (2.0 * Cab.BINFRZ+TIFS)*FALPHA) / 9.0

            R1 = 1.0/(RB1 * Cab.RKINFZ) + 1.0 / (Cab.HI*AIRBTM1)
            R2 = 1.0/(Cab.HO * AORBTM1)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            QBOTTM1 = lst_Feedback[0]
            # ------------------------------------------------------------

            RB2 = AIRBTM2 / Cab.BINFRZ                         \
                + 0.54 * (WFRZ * (1.0+FALPHA)+D2Z) / 2.0        \
                + 0.15 * ((2.0 * Cab.BINFRZ+TIFS) * FALPHA
                          + (Cab.BINFRZ+Cab.TIFF+TIFS)) / 9.0

            R1 = 1.0 / (RB2 * Cab.RKINFZ)+1.0/(Cab.HI * AIRBTM2)
            R2 = 1.0 / (Cab.HO * AORBTM2)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            QBOTTM2 = lst_Feedback[0]
            # ------------------------------------------------------------
            Cab.QRBTTM = QBOTTM1 + QBOTTM2

        if Cab.NCCTYPE == 3:
            RB1 = AIRBTM1 / Cab.BINFRZ+0.54 * (WFRZ * (1+FALPHA)+D1Z) / 2.0 \
                + 0.15 * ((2.0 * Cab.BINFRZ+TIFS)*FALPHA
                          + (Cab.BINFRZ+Cab.TIFB+TIFS)) / 9.0

            R1 = 1.0 / (RB1 * Cab.RKINFZ) + 1.0 / (Cab.HI*AIRBTM1)
            R2 = 1.0 / (Cab.HO * AORBTM1)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            QBOTTM1 = lst_Feedback[0]
            # ------------------------------------------------------------

            RB2 = AIRBTM2 / Cab.BINFRZ \
                + 0.54 * (2.0 * WFRZ * FALPHA + D2Z) / 2.0       \
                + 0.15 * (4.0 * Cab.BINFRZ + 2.0 * TIFS) * FALPHA / 9.0

            R1 = 1.0/(RB2 * Cab.RKINFZ)+1.0/(Cab.HI * AIRBTM2)
            R2 = 1.0/(Cab.HO * AORBTM2)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            QBOTTM2 = lst_Feedback[0]
            # ------------------------------------------------------------

            RB3 = AIRBTM3 / Cab.BINFRZ  \
                + 0.54 * ((1 + FALPHA) * WFRZ + D3Z) / 2.0 \
                + 0.15 * ((2.0 * Cab.BINFRZ+TIFS) * FALPHA
                          + (Cab.BINFRZ+Cab.TIFF+TIFS)) / 9.0

            R1 = 1.0 / (RB3 * Cab.RKINFZ)+1.0/(Cab.HI*AIRBTM3)
            R2 = 1.0 / (Cab.HO * AORBTM3)

            lst_Feedback = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            QBOTTM3 = lst_Feedback[0]
            # ------------------------------------------------------------

            Cab.QRBTTM = QBOTTM1+QBOTTM2+QBOTTM3

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #
        # Calculate the heat leak through the mullion
        #

        #    AMULL     mullion surface area on the fresh food side.
        #    AMULR     mullion surface area on the freezer side.

        AMULL = HFFC * DFFC
        AMULR = HFRZ * DFRZ
        Cab.QMUL = (1.0 / (1.0 / (Cab.HIRMUL*AMULL)
                    + 1.0/(Cab.HIFMUL * AMULR)
                    + Cab.THMUL/(Cab.CKMUL * AMULL))
                    ) * (Cab.TFF - Cab.TFRZ)

        #  "Mullion heat leak" added by A.Esposito 7DEC89
        #    Sum all the heat leaks to get the cabinet heat leak.
        Cab.QFFT = Cab.QLSIDE + Cab.QLTOP + Cab.QBACKL + Cab.QFRNTL + Cab.QLBTTM - Cab.QMUL
        Cab.QFRZ = Cab.QRSIDE + Cab.QRTOP + Cab.QBACKR + Cab.QFRNTR + Cab.QRBTTM + Cab.QMUL
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #     CALCULATE GASKET HEAT LEAKS FOR FREEZER FAN ON AND OFF
        #
        Cab.QGZN = 24.0 * Cab.HLGZF * (WFRZ + HFRZ) * (Cab.TROOM - Cab.TFRZ)  # Change on 8/26/92
        Cab.QGZF = Cab.QGZN
        Cab.QGR = 24.0 * Cab.HLRG * (HFFC + WFFC) * (Cab.TROOM - Cab.TFF)

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #     CALCULATE HEAT LEAKS FOR THE WEDGE
        #
        THETA = math.atan((TIFS-Cab.FLANGE) / Cab.WEDGE)

        AWEDGE = Cab.WEDGE * (TIFS/(TIFS-Cab.FLANGE))
        BWEDGE = AWEDGE - Cab.WEDGE

        WL1 = (HFRZ + Cab.TIFT+Cab.BINFRZ-Cab.FLGB+WFRZ+TIFS-2.0 * Cab.FLANGE)
        WL2 = HFRZ + WFRZ

        QWFZC = (1.0 / (1.0 / (Cab.HO * Cab.WEDGE * (Cab.HEIGHT - Cab.BOTTOM + WFRZ + TIFS))
                        + THETA/(Cab.WKIN * math.log(AWEDGE / BWEDGE) * (WL1+WL2) / 2.0)
                        )
                 ) * (Cab.TROOM-Cab.TFRZ)

        THETA = math.atan((Cab.BINFRZ-Cab.FLGB) / Cab.WEDGE)
        AWEDGE = Cab.WEDGE * (Cab.BINFRZ/(Cab.BINFRZ-Cab.FLGB))
        BWEDGE = AWEDGE - Cab.WEDGE

        QWFZB = (1.0 / (1.0 / (Cab.HO * Cab.WEDGE * Cab.WIDTH)
                 + THETA/(Cab.WKIN * math.log(AWEDGE/BWEDGE)
                 * (WFRZ + 0.5 * (TIFS-Cab.FLANGE))))
                 ) * (Cab.TBTM-Cab.TFRZ)
        
        if Cab.WEDGER != 0:
            THETA = math.atan((TIRS-Cab.FLANGER) / Cab.WEDGER)
            AWEDGE = Cab.WEDGER * (TIRS/(TIRS-Cab.FLANGER))
            BWEDGE = AWEDGE - Cab.WEDGER

            W1 = Cab.WALL + HFFC + Cab.BINSUL - 2.0 * Cab.FLANGER - Cab.FLGB
            W2 = HFFC + WFFC

            QWFFC = (1.0/(1.0/(Cab.HO * Cab.WEDGER * (Cab.HEIGHT-Cab.BOTTOM+Cab.WALL))
                     + THETA/(Cab.WKINR * math.log(AWEDGE/BWEDGE)
                     * (W1+W2) / 2.0))
                     ) * (Cab.TROOM-Cab.TFF)

            THETA = math.atan((Cab.BINSUL-Cab.FLGB) / Cab.WEDGER)
            AWEDGE = Cab.WEDGER * (Cab.BINSUL/(Cab.BINSUL-Cab.FLGB))
            BWEDGE = AWEDGE - Cab.WEDGER

            QWFFB = (1.0 / (1.0 / (Cab.HO * Cab.WEDGER * Cab.WALL)
                            + THETA / (Cab.WKINR * math.log(AWEDGE/BWEDGE)
                                       * (2.0 * WFFC+TIRS-Cab.FLANGER) / 2.0))
                     ) * (Cab.TBTM-Cab.TFF)

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        #     SUM THE VARIOUS COMPONENTS OF THE HEAT LEAK
        #
        Cab.QW = QWFZC + QWFZB + QWFFC + QWFFB
        Cab.QWFF = QWFFC + QWFFB
        Cab.QWFZ = QWFZC + QWFZB
        Cab.QGON = Cab.QGR + Cab.QGZN
        Cab.QGOF = Cab.QGR + Cab.QGZF
        Cab.QTON = Cab.QW + Cab.QGON + Cab.QFFT + Cab.QFRZ
        Cab.QTOF = Cab.QW + Cab.QGOF + Cab.QFFT + Cab.QFRZ

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # The heat leak due to Door Openings
        obj_doorpn = DoorOpen().DoorpnBuilder()    \
            .withTempFFSetpoint(Cab.TFF)        \
            .withTempFZSetpoint(Cab.TFRZ)        \
            .withHeighFF(HFFC)                 \
            .withWidthFF(WFFC)            \
            .withDepthFF(DFFC)            \
            .withHeighFZ(HFRZ)            \
            .withWidthFZ(WFRZ)            \
            .withDepthFZ(DFRZ)            \
            .withMode(Cab.NMOD)            \
            .withVolumeFZ(Cab.VOLAZ)    \
            .withVolumeFF(Cab.VOLAR)    \
            .withRelHumidity(Cab.RELHUM)        \
            .withTempAirAmbient(Cab.TDRAIR)    \
            .withOpenHrFF(Cab.FFCOPN)        \
            .withWaterFF(Cab.WATERF)        \
            .withWaterFZ(Cab.WATERZ)        \
            .withOpenHrFZ(Cab.FRZOPN)    \
            .withHrOpenFF(Cab.HRFFC)    \
            .withHrOpenFZ(Cab.HRFRZ)    \
            .build()

        lstRes = obj_doorpn.main()

        Cab.QDFFCS = lstRes[0]
        Cab.QDFFFS = lstRes[1]
        Cab.QDFZCS = lstRes[2]
        Cab.QDFZFS = lstRes[3]

        Cab.QDFFCL = lstRes[4]
        Cab.QDFFFL = lstRes[5]
        Cab.QDFZCL = lstRes[6]
        Cab.QDFZFL = lstRes[7]
        return
# .=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
