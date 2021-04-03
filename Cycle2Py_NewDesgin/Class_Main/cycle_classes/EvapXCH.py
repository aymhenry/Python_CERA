import datetime
from abc import ABC, abstractmethod

# User import


from cycle_classes.ErrorException import ErrorException

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Create Condenser object based on ISPEC
#    input: ISPEC = 1  Evap superheat
#         : ISPEC = 2  Interchanger super-
#         : ISPEC = 3  Evap exit quality
#         : objCP = CoolProp object
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class EvapXCH:
    def getObject (self, ISPEC, objCP, dt):
        if (ISPEC == 1):  # Evap superheat specified
            objEvapXCH = EvapXCH_Super(ISPEC, objCP, dt)

        elif (ISPEC == 2):  # Interchanger superheat specified
            objEvapXCH = EvapXCH_Inter(ISPEC, objCP, dt)

        elif (ISPEC == 3):  # Evap exit quality specified
            objEvapXCH = EvapXCH_Quality(ISPEC, objCP, dt)

        else:
            objEvapXCH = None
            raise ErrorException('ISPEC value error', 'EvapXCH1000')
        return objEvapXCH
        
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Evaprator configration
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class EvapXCH_Abstract (ABC):
    def __init__(self, ISPEC, objCP, dt):
        self.ISPEC = ISPEC
        self.objCP = objCP
        self.dt = dt
    
    def get_1stVal(T3):
        pass


class EvapXCH_Super (EvapXCH_Abstract):  # Evap superheat
    def get_1stVal(TS3):
        TS3 = TS3 - (self.dt.DTSUPE + 2.0)
        TE1 = T15 + self.DTSUPE
        T15 = self.objCP.Property('P', X=1, T=T15)  # pas
        
        return [TS3, TE1, T15]
        
        
class EvapXCH_Inter (EvapXCH_Abstract):  # Interchanger super-
    def get_1stVal(TS3):
        return  [1,1,1]


        
class EvapXCH_Quality (EvapXCH_Abstract):  # Evap exit quality
    def get_1stVal(TS3):
        return [1,1,1]
