# Python Import ==================

# User Import ======================
from cycle_classes.ErrorException import ErrorException

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
# Job             : Check point stats Cycle app
#                   check if given point in wet region
#                     if inside, return super-heated value (if X=1) 
#                     or sub-cool value (X=0)
# Editor        : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class CoolPrpUtil:
    ERR_MARGIN = 0.001

    def __init__(self, objCP):
        self.objCP = objCP
    
    def getProp(self, P, T, prp="H", X=1):
        P_sat = self.objCP.Property('P', T=T, X=0)  # Pa
        
        # check if in wet area
        if abs(P_sat - P) > CoolPrpUtil.ERR_MARGIN:
            # if so, get liquid or vap. value
            prop = self.objCP.Property(prp, T=T, P=P)
            
        else:
            prop = self.objCP.Property(prp, T=T, X=X)
        
        return prop

    def get_coolQuality(self, Enthalpy, P, T):
        # as, liquid, twophase,  supercritical_gas
        P_sat = self.objCP.Property('P', T=T, X=0)  # Pa

        if abs(P_sat - P) > CoolPrpUtil.ERR_MARGIN:
            sta_phas = False

        else:
            sta_phas = True

        if sta_phas:
            # get Quality
            H_liq = self.objCP.Property('H', T=T, X=0)  # j/kg
            H_vap = self.objCP.Property('H', T=T, X=1)  # j/kg
            quality = round((Enthalpy - H_liq) / (H_vap - H_liq), 4)
            
            if quality < 0 or quality > 1:
                raise ErrorException(
                    'Error in Entry data for quality '
                        + "\ngive: " + str(Enthalpy)
                        + "\n max: " + str(H_vap)
                        + "\n min: " + str(H_liq)
                        + "\n quality: " + str(quality)
                    , 'CycleSolver1001')

        else:
            # not in sat.
            quality = -1

        return quality