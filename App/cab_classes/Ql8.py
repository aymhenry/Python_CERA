# Python import
import math


# User Import
from .CabUtils import CabUtils
from .DoorOpen import DoorOpen


class Ql8(CabUtils):
    # .=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
    # Job           : CALCULATE CABINET HEAT LEAK FOR CONFIGURATION 8 * 
    #                 CONFIGURATION 8: Bottom Mount R / F * 
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    def __init__(self, Cab):
        ALPHA = 0.0   # in Python only
        BETA = 0.0   # in Python only
        # Typthon only
        DC = H2 = D2 = H1 = D1 = D3 = 0
        AIFSID = RF = AOFSID = AIFBCK = AOFBCK = AIFFNT = AOFFNT = 0
        AOFBOT = AIFBOT = AIFBTM1 = AOFBTM1 = AIFBTM2 = AOFBTM2 = 0
        AIFBTM3 = AOFBTM3 = 0

        Cab.TIFLS = Cab.TIFRS
        Cab.TIRRS = Cab.TIRLS
        TIFS = Cab.TIFRS
        TIRS = Cab.TIRLS
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate overall INTERNAL Height, Width and Depth of Freezer
        # compartment(FRZ) and the Fresh Food Compartment(FFC)
        #
        HFFC = Cab.TOPMUL - Cab.TIRT
        WFFC = Cab.WIDTH - Cab.TIRLS - Cab.TIRRS
        DFFC = Cab.DEPTH - Cab.WEDGER - Cab.TIRF - Cab.TIRB - Cab.DGSKT

        HFRZ = Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL - Cab.BINSUL
        WFRZ = Cab.WIDTH - Cab.TIFLS - Cab.TIFRS
        DFRZ = Cab.DEPTH - Cab.WEDGE - Cab.TIFF - Cab.TIFB - Cab.DGSKT

        # if Cab.NCCTYPE == 1:
        # - - - Nothing

        if Cab.NCCTYPE == 2:
            BETA = math.atan(Cab.CDDN / Cab.CCHGT)
            ALPHA = math.pi / 4.0 - BETA / 2.0
            H1 = Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL - Cab.BINSUL
            HTRIAN = Cab.CCHGT - Cab.BINSUL + Cab.BINSUL / math.sin(BETA) - Cab.TIFB / math.tan(BETA)
            DC = Cab.DEPTH - Cab.WEDGE - Cab.TIFF - Cab.TIFB - Cab.DGSKT
            h2 = H1 - HTRIAN
            d1 = HTRIAN / math.cos(BETA)
            d2 = DC - HTRIAN * math.tan(BETA)
            WFRZ = Cab.WIDTH - Cab.TIFLS - Cab.TIFRS
            HFRZ = H1

        if Cab.NCCTYPE == 3:
            BETA = math.atan((Cab.CDDN - Cab.CDUP) / Cab.CCHGT)
            ALPHA = math.pi / 4.0 - BETA / 2.0
            H1 = Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL - Cab.BINSUL
            H2 = H1 - Cab.CCHGT
            D1 = Cab.CDUP - Cab.TIFB

            if Cab.CDDN == Cab.CDUP:
                D2 = Cab.CCHGT - Cab.BINSUL
            else:
                D2 = (Cab.CDDN - Cab.CDUP) / math.sin(BETA) - Cab.BINSUL * math.tan(ALPHA)

            D3 = Cab.DEPTH - Cab.CDDN - Cab.TIFF - Cab.WEDGE - Cab.BINSUL * math.tan(ALPHA) - Cab.DGSKT
            DC = Cab.DEPTH - Cab.WEDGE - Cab.TIFF - Cab.TIFB - Cab.DGSKT
            WFRZ = Cab.WIDTH - Cab.TIFLS - Cab.TIFRS
            HFRZ = H1

        FALPHA = 4.0 * ALPHA / math.pi
        FBETA = 2.0 * BETA / math.pi 

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Internal Fresh Food(R) Areas
        #
        AIRSID = HFFC * DFFC
        AIRTOP = WFFC * DFFC
        AIRBCK = HFFC * WFFC
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # External Fresh Food(R) Areas
        #
        AORSID = (Cab.TOPMUL + Cab.THMUL / 2.0) * (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT)
        AORTOP = Cab.WIDTH * (Cab.DEPTH - Cab.WEDGER - Cab.DGSKT)
        AORBCK = (Cab.TOPMUL + Cab.THMUL / 2.0) * Cab.WIDTH

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Internal Freezer(F) Areas
        #
        if Cab.NCCTYPE == 1:
            AIFSID = HFRZ * DFRZ
            AIFBOT = WFRZ * DFRZ
            AIFBCK = HFRZ * WFRZ
            AIFFNT = AIFBCK

        if Cab.NCCTYPE == 2:
            AIFSID = DC * H2 + (DC + D2) * (H1 - H2) / 2.0
            AIFBCK = WFRZ * H2
            AIFFNT = WFRZ * H1
            AIFBTM1 = WFRZ * D1
            AIFBTM2 = WFRZ * D2

        if Cab.NCCTYPE == 3:
            AIFSID = DC * H2 + Cab.CCHGT * (D3 + D3 + Cab.CCHGT * math.tan(BETA)) / 2.0
            AIFBCK = WFRZ * H2
            AIFFNT = WFRZ * H1
            AIFBTM1 = WFRZ * (D1 + Cab.BINSUL * math.tan(ALPHA))
            AIFBTM2 = WFRZ * (D2 + Cab.BINSUL * math.tan(ALPHA))
            AIFBTM3 = WFRZ * D3

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # External Freezer(F) Areas
        #
        if Cab.NCCTYPE == 1:
            AOFSID = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL / 2.0) * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT)
            AOFBOT = Cab.WIDTH * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT)
            AOFBCK = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL / 2.0) * Cab.WIDTH
            AOFFNT = AOFBCK

        if Cab.NCCTYPE == 2:
            AOFSID = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL / 2.0) \
                     * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT) - Cab.CDDN * Cab.CCHGT / 2.0
            AOFFNT = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL / 2.0) * Cab.WIDTH
            AOFBCK = AOFFNT - Cab.CCHGT * Cab.WIDTH
            AOFBTM1 = Cab.WIDTH * Cab.CCHGT / math.cos(BETA)
            AOFBTM2 = Cab.WIDTH * (Cab.DEPTH - Cab.CDDN - Cab.WEDGE - Cab.DGSKT)
            
        if Cab.NCCTYPE == 3:
            AOFSID = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL / 2.0) * (Cab.DEPTH - Cab.WEDGE - Cab.DGSKT) \
                 - (Cab.CDDN + Cab.CDUP) * Cab.CCHGT / 2.0
            AOFFNT = (Cab.HEIGHT - Cab.TOPMUL - Cab.THMUL / 2.0) * Cab.WIDTH
            AOFBCK = AOFFNT - Cab.CCHGT * Cab.WIDTH
            AOFBTM1 = Cab.WIDTH * Cab.CDUP
            AOFBTM2 = Cab.WIDTH * Cab.CCHGT / math.cos(BETA)
            AOFBTM3 = Cab.WIDTH * (Cab.DEPTH - Cab.CDDN - Cab.WEDGE - Cab.DGSKT)

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the average insulation conductivity.
        # DKINFZ        Freezer door insulation.
        # DKINFF         Fresh food door insulation conductivity.
        # RKINFF         insulation conductivity for the sides, back, top and bottom of the fresh food compartment.
        # RKINFZ         insulation conductivity for Freezer.
        #
        TAVGL = 0.25 * (Cab.DKINFF + Cab.DKINFZ + Cab.RKINFF + Cab.RKINFZ)

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the internal and external surface areas of the freezer
        #
        # NOTE: The internal and external surface areas were separatly
        # calculated for each component(excluding the bottom surface)of the FRZ.
        #
        # Corners and edges boardering the mullion were accounted for in the mullion heat leak calculation. SEE FIGURE 1
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the area and overall heat transfer of the mullion
        #
        # AMUL         Mullion area(Ft^2)
        # UMUL         Mullion overall heat transfer(Btu/Hr-Ft^2-Deg F)
        #
        AMUL = WFRZ * DFRZ
        UMUL = 1.0 / (1.0 / Cab.HIFMUL + 1.0 / Cab.HIRMUL + Cab.THMUL / Cab.CKMUL)
        
        Cab.QMUL = UMUL * AMUL * (Cab.TFF - Cab.TFRZ)
    
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate r and heat leak for freezer
        #
        # R is actually the conductance(1 / Resistance) of the various components that make up the freezer compartment.
        # Heat leak through the corners and edges is accounted for in the R calculations.
        #
        # The shape factor for an edge is 0.54 * length of the edge.
        # The shape factor for a corner is 0.15 * wall thickness.(Holman p.54)
        #
        # NOTE: Since each corner is made up of three different insulation
        # thicknesses, an average thickness was used in the calculation.
        #
        # The corner shape factor is divided by three since each corner is shared
        # by three walls of different thicknesses.
        #    (The corner shape factor is actually divided by 9 since the three corner thicknesses are averaged(3x3=9))
        #
        # The edge shape factor is divided by two since each edge is shared by two walls.
        #
        # Note: R calculation split into seperate components
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the LEFT Fresh Food Side
        #
        RR = AIRSID / Cab.TIRLS + 0.54 * (DFFC + 2.0 * HFFC) / 2.0 \
            + 0.15 * (2.0 * Cab.TIRLS + Cab.TIRB + 2.0 * Cab.TIRT + Cab.TIRF) / 9.0
        R1 = 1.0 / (RR * Cab.RKINFF) + 1.0 / (Cab.HI * AIRSID)
        R2 = 1.0 / (Cab.HO * AORSID)

        loc_list = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TROOM)
        Cab.QRLSID = loc_list[0]
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the LEFT Freezer Side
        #
        if Cab.NCCTYPE == 1:
            RF = AIFSID / Cab.TIFLS + 0.54 * (DFRZ + 2.0 * HFRZ) / 2.0    \
                + 0.15 * (2.0 * Cab.TIFLS + Cab.TIFB + 2.0 * Cab.BINSUL + Cab.TIFF) / 9.0

        if Cab.NCCTYPE == 2:
            RF = AIFSID / Cab.TIFLS \
                 + 0.54 * (H1 + H2 + D1 + D2) / 2.0 + 0.15 \
                 * ((2.0 * Cab.BINSUL + Cab.TIFLS) * FALPHA
                    + (Cab.TIFB + Cab.TIFLS + Cab.BINSUL) * FBETA
                    + (Cab.TIFF + Cab.TIFLS + Cab.BINSUL)) / 9.0
                
        if Cab.NCCTYPE == 3:
            RF = AIFSID / Cab.TIFLS \
                 + 0.54 * (H1 + H2 + D1 + D2 + D3) / 2.0 + 0.15 \
                 * (2.0 * (2.0 * Cab.BINSUL + Cab.TIFLS) * FALPHA
                    + (2.0 * (Cab.TIFLS + Cab.BINSUL) + Cab.TIFF + Cab.TIFB)) / 9.0

        R1 = 1.0 / (RF * Cab.RKINFZ) + 1.0 / (Cab.HI * AIFSID)
        R2 = 1.0 / (Cab.HO * AOFSID)

        loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TROOM)
        Cab.QFLSID = loc_list[0]
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the RIGHT Fresh Food Side
        #
        RR = AIRSID / Cab.TIRRS + 0.54 * (DFFC + 2.0 * HFFC) / 2.0    \
            + 0.15 * (2.0 * Cab.TIRRS + Cab.TIRB + 2.0 * Cab.TIRT + Cab.TIRF) / 9.0
        R1 = 1.0 / (RR * Cab.RKINFF) + 1.0 / (Cab.HI * AIRSID)
        R2 = 1.0 / (Cab.HO * AORSID)
        
        loc_list = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TROOM)
        Cab.QRRSID = loc_list[0]

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the RIGHT Freezer Side
        #
        if Cab.NCCTYPE == 1:
            RF = AIFSID / Cab.TIFRS + 0.54 * (DFRZ + 2.0 * HFRZ) / 2.0    \
                + 0.15 * (2.0 * Cab.TIFRS + Cab.TIFB + 2.0 * Cab.BINSUL + Cab.TIFF) / 9.0
        
        if Cab.NCCTYPE == 2:
            RF = AIFSID / Cab.TIFRS \
                + 0.54 * (H1 + H2 + D1 + D2) / 2.0 + 0.15    \
                * ((2.0 * Cab.BINSUL + Cab.TIFRS) * FALPHA
                   + (Cab.TIFB + Cab.TIFRS + Cab.BINSUL) * FBETA
                   + (Cab.TIFF + Cab.TIFRS + Cab.BINSUL)) / 9.0

        if Cab.NCCTYPE == 3:
            RF = AIFSID / Cab.TIFRS + 0.54 * (H1 + H2 + D1 + D2 + D3) / 2.0 \
                 + 0.15 * (2.0 * (2.0 * Cab.BINSUL + Cab.TIFRS) * FALPHA
                           + (2.0 * (Cab.TIFRS + Cab.BINSUL) + Cab.TIFF + Cab.TIFB)) / 9.0
                
        R1 = 1.0 / (RF * Cab.RKINFZ) + 1.0 / (Cab.HI * AIFSID)
        R2 = 1.0 / (Cab.HO * AOFSID)
        
        loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TROOM)
        Cab.QFRSID = loc_list[0]
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the top(Fresh Food) the top has two Depth and two Width length edges
        #
        Cab.R = AIRTOP / Cab.TIRT \
            + 0.54 * (2.0 * DFFC + 2.0 * WFFC) / 2.0 \
            + 0.15 * (2. * Cab.TIRLS + 2. * Cab.TIRRS + 2. * Cab.TIRB + 4. * Cab.TIRT + 2.0 * Cab.TIRF) / 9.0
        R1 = 1.0 / (Cab.R * Cab.RKINFF) + 1.0 / (Cab.HI * AIRTOP)
        R2 = 1.0 / (Cab.HO * AORTOP)
        
        loc_list = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TROOM)
        Cab.QTOP = loc_list[0]
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the BACK Fresh Food the back has two Height and one Width length edges
        #
        Cab.R = AIRBCK / Cab.TIRB + 0.54 * (2.0 * HFFC + WFFC) / 2.0    \
            + 0.15 * (Cab.TIRLS + Cab.TIRRS + 2.0 * Cab.TIRB + 2.0 * Cab.TIRT) / 9.0
        R1 = 1.0 / (Cab.R * Cab.RKINFF) + 1.0 / (Cab.HI * AIRBCK)
        R2 = 1.0 / (Cab.HO * AORBCK)
        
        loc_list = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TROOM)
        Cab.QRBACK = loc_list[0]
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the BACK Freezer the back has two Height and one Width length edges
        #
        if Cab.NCCTYPE == 1:
            Cab.R = AIFBCK / Cab.TIFB \
                + 0.54 * (2.0 * HFRZ + WFRZ) / 2.0    \
                + 0.15 * (Cab.TIFLS + Cab.TIFRS + 2.0 * Cab.TIFB + 2.0 * Cab.BINSUL) / 9.0
                
        if Cab.NCCTYPE == 2:
            Cab.R = AIFBCK / Cab.TIFB + 0.54 * (2.0 * H2 + WFRZ * FBETA) / 2.0 + 0.15    \
                 * (Cab.TIFLS + Cab.TIFRS + 2.0 * Cab.TIFB + 2.0 * Cab.BINSUL) * FBETA / 9.0
                
        if Cab.NCCTYPE == 3:
            Cab.R = AIFBCK / Cab.TIFB + 0.54 * (2.0 * H2 + WFRZ) / 2.0    \
                + 0.15 * (Cab.TIFLS + Cab.TIFRS + 2.0 * Cab.TIFB + 2.0 * Cab.BINSUL) / 9.0
                 
        R1 = 1.0 / (Cab.R * Cab.RKINFZ) + 1.0 / (Cab.HI * AIFBCK)
        R2 = 1.0 / (Cab.HO * AOFBCK)
        
        loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TROOM)
        Cab.QFBACK = loc_list[0]

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the FRONT Fresh Food
        # The front has two Height and one Width length edges
        #
        Cab.R = AIRBCK / Cab.TIRF + 0.54 * (2.0 * HFFC + WFFC) / 2.0    \
            + 0.15 * (Cab.TIRLS + Cab.TIRRS + 2.0 * Cab.TIRF + 2.0 * Cab.TIRT) / 9.0
        R1 = 1.0 / (Cab.R * Cab.DKINFF) + 1.0 / (Cab.HI * AIRBCK)
        R2 = 1.0 / (Cab.HO * AORBCK)
        
        loc_list = self.getRadHeatFlux(R1, R2, Cab.TFF, Cab.TROOM)
        Cab.QRFRNT = loc_list[0]

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the FRONT Freezer
        # The front has two Height and one Width length edges
        #
        Cab.R = AIFFNT / Cab.TIFF + 0.54 * (2.0 * HFRZ + WFRZ) / 2.0    \
            + 0.15 * (Cab.TIFLS + Cab.TIFRS + 2.0 * Cab.TIFF + 2.0 * Cab.BINSUL) / 9.0

        R1 = 1.0 / (Cab.R * Cab.DKINFZ) + 1.0 / (Cab.HI * AIFFNT)
        R2 = 1.0 / (Cab.HO * AOFFNT)
        
        loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TROOM)
        Cab.QFFRNT = loc_list[0]
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Calculate the heat leak out of the bottom, both with and without compressor notch
        # The top has two Depth and two Width length edges
        #
        if Cab.NCCTYPE == 1:
            AOB = AOFBOT
            AIB = AIFBOT
            Cab.R = AIB / Cab.BINSUL + 0.54 * (2.0 * DFRZ + 2.0 * WFRZ) / 2.0 \
                + 0.15 * (4. * Cab.BINSUL + 2. * Cab.TIFB + 2. * Cab.TIFLS + 2. * Cab.TIFRS + 2. * Cab.TIFF) / 9.0
            R1 = 1.0 / (Cab.R * Cab.RKINFZ) + 1.0 / (Cab.HI * AIB)
            R2 = 1.0 / (Cab.HO * AOB)
            
            loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            Cab.QBOTTM = loc_list[0]
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     

        if Cab.NCCTYPE == 2:
            RB1 = AIFBTM1 / Cab.BINSUL    \
               + 0.54 * (WFRZ * (FALPHA + FBETA) + 2.0 * D1) / 2.0    \
               + 0.15 * ((2.0 * Cab.TIFB + 2.0 * Cab.BINSUL + Cab.TIFLS + Cab.TIFRS) * FBETA
                         + (4.0 * Cab.BINSUL + Cab.TIFLS + Cab.TIFRS) * FALPHA) / 9.0

            R1 = 1.0 / (RB1 * Cab.RKINFZ) + 1.0 / (Cab.HI * AIFBTM1)
            R2 = 1.0 / (Cab.HO * AOFBTM1)

            loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            Cab.QBOTTM1 = loc_list[0]
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     

            RB2 = AIFBTM2 / Cab.BINSUL    \
                + 0.54 * (WFRZ * (1.0 + FALPHA) + 2.0 * D2) / 2.0    \
                + 0.15 * ((4.0 * Cab.BINSUL + Cab.TIFLS + Cab.TIFRS) * FALPHA
                          + (2.0 * Cab.BINSUL + 2.0 * Cab.TIFF + Cab.TIFLS + Cab.TIFRS)) / 9.0
            R1 = 1.0 / (RB2 * Cab.RKINFZ) + 1.0 / (Cab.HI * AIFBTM2)
            R2 = 1.0 / (Cab.HO * AOFBTM2)
            
            loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            Cab.QBOTTM2 = loc_list[0]
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -          
            Cab.QBOTTM = Cab.QBOTTM1 + Cab.QBOTTM2
                
        if Cab.NCCTYPE == 3:
            RB1 = AIFBTM1 / Cab.BINSUL + 0.54 * (WFRZ * (1 + FALPHA) + 2.0 * D1) / 2.0    \
                + 0.15 * ((4.0 * Cab.BINSUL + Cab.TIFLS + Cab.TIFRS) * FALPHA
                          + (2.0 * Cab.BINSUL + 2.0 * Cab.TIFB + Cab.TIFLS + Cab.TIFRS)) / 9.0
            R1 = 1.0 / (RB1 * Cab.RKINFZ) + 1.0 / (Cab.HI * AIFBTM1)
            R2 = 1.0 / (Cab.HO * AOFBTM1)
            
            loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            Cab.QBOTTM1 = loc_list[0]
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     

            RB2 = AIFBTM2 / Cab.BINSUL + 0.54 * (2.0 * WFRZ * FALPHA + 2.0 * D2) / 2.0    \
                + 0.15 * (8.0 * Cab.BINSUL + 2.0 * Cab.TIFLS + 2.0 * Cab.TIFRS) * FALPHA / 9.0
            R1 = 1.0 / (RB2 * Cab.RKINFZ) + 1.0 / (Cab.HI * AIFBTM2)
            R2 = 1.0 / (Cab.HO * AOFBTM2)
            
            loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            Cab.QBOTTM2 = loc_list[0]
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     

            RB3 = AIFBTM3 / Cab.BINSUL + 0.54 * ((1 + FALPHA) * WFRZ + 2.0 * D3) / 2.0    \
                + 0.15 * ((4.0 * Cab.BINSUL * Cab.TIFLS + Cab.TIFRS) * FALPHA    
                          + (2.0 * Cab.BINSUL + 2.0 * Cab.TIFF + Cab.TIFLS + Cab.TIFRS)) / 9.0
            R1 = 1.0 / (RB3 * Cab.RKINFZ) + 1.0 / (Cab.HI * AIFBTM3)
            R2 = 1.0 / (Cab.HO * AOFBTM3)
            
            loc_list = self.getRadHeatFlux(R1, R2, Cab.TFRZ, Cab.TBTM)
            Cab.QBOTTM3 = loc_list[0]
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -          
            Cab.QBOTTM = Cab.QBOTTM1 + Cab.QBOTTM2 + Cab.QBOTTM3

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Sum all the heat leaks to get the cabinet heat leak.
        #
        Cab.QFRZ = Cab.QFLSID + Cab.QFRSID + Cab.QBOTTM + Cab.QFBACK + Cab.QFFRNT + Cab.QMUL
        Cab.QFFT = Cab.QRLSID + Cab.QRRSID + Cab.QRBACK + Cab.QRFRNT + Cab.QTOP - Cab.QMUL

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # CALCULATE WEDGE HEAT LEAKS
        #
        if Cab.WEDGER != 0.0:
            THETA = math.atan((TIRS - Cab.FLANGER) / Cab.WEDGER)
            AWEDGE = Cab.WEDGER * (TIRS / (TIRS - Cab.FLANGER))
            
            BWEDGE = AWEDGE - Cab.WEDGER
            WL1 = 2.0 * HFFC + WFFC
            WL2 = 2.0 * (HFFC - Cab.FLANGER + Cab.TIRT) + WFFC + 2.0 * (TIRS - Cab.FLANGER)

            R1 = 1.0 / (Cab.HO * Cab.WEDGER * (2.0 * Cab.TOPMUL + Cab.WIDTH))
            R2 = THETA / (Cab.WKINR * math.log(AWEDGE / BWEDGE) * (WL1 + WL2) / 2.0)
            DL = math.log(AWEDGE / BWEDGE)
            Cab.QWFF = (1.0 / (1.0 / (Cab.HO * Cab.WEDGER * (2.0 * Cab.TOPMUL + Cab.WIDTH))
                               + THETA / (Cab.WKINR * math.log(AWEDGE / BWEDGE)
                                          * (WL1 + WL2) / 2.0))) * (Cab.TROOM - Cab.TFF)

        THETA = math.atan((TIFS - Cab.FLANGE) / Cab.WEDGE)
        AWEDGE = Cab.WEDGE * (TIFS / (TIFS - Cab.FLANGE))
        BWEDGE = AWEDGE - Cab.WEDGE
        WL = HFRZ + Cab.BINSUL - Cab.FLGB
        DL = math.log(AWEDGE / BWEDGE)
        R1 = 1.0 / (2.0 * Cab.HO * HFRZ * Cab.WEDGE)
        R2 = THETA / (Cab.WKIN * math.log(AWEDGE / BWEDGE) * (WL + HFRZ))
        QWFZS = (1.0 / (1.0 / (2.0 * Cab.HO * HFRZ * Cab.WEDGE)
                        + THETA / (Cab.WKIN * math.log(AWEDGE / BWEDGE) * (WL + HFRZ)))) * (Cab.TROOM - Cab.TFRZ)
        THETA = math.atan((Cab.BINSUL - Cab.FLGB) / Cab.WEDGE)
        AWEDGE = Cab.BINSUL / (Cab.BINSUL - Cab.FLGB)
        BWEDGE = AWEDGE - Cab.WEDGE
        WL = Cab.WIDTH - 2.0 * Cab.FLANGE
        
        QWFZB = (1.0 / (1.0 / (Cab.HO * Cab.WEDGE * Cab.WIDTH)
                        + THETA / (Cab.WKIN * math.log(AWEDGE / BWEDGE)
                                   * (WL + WFRZ) * 0.5))) * (Cab.TBTM - Cab.TFRZ)
        
        #
        # QW is the total heat leak through the wedge
        #
        Cab.QW = Cab.QWFF + QWFZS + QWFZB
        Cab.QWFZ = QWFZS + QWFZB
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # CALCULATE THE GASKET HEAT LEAKS FOR EACH COMPARTMENT AND FOR
        # FREEZER FAN ON AND FREEZER FAN OFF
        #
        # QGR             Gasket heat leak through the fresh food compartment
        # QGZN             Gasket heat leak through the freezer with the freezer fan off
        # QGZF             Gasket heat leak through the freezer with the freezer fan on
        #
        Cab.QGR = 24.0 * Cab.HLRG * (Cab.TOPMUL + Cab.WIDTH) * (Cab.TROOM - Cab.TFF)

        Cab.QGZF = 24.0 * Cab.HLGZF * (HFRZ + Cab.WIDTH) * (Cab.TROOM - Cab.TFRZ)
        Cab.QGZN = Cab.QGZF
        
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
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
