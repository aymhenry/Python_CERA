# Python import
from CoolProp.CoolProp import PhaseSI, PropsSI, get_global_param_string

# User import


class CoolPrp:
    # Class Startic vars
    # Error internal code
    ERR_NOT_FOUND = 0  # no error code
    ERR_FUILD_NOT_FOUND = 10   	# some error found
    ERR_PROB_NOT_FOUND = 500   # Property not supported
    ERR_PROB_ERROR = 510   # Property not supported

    PHASE_TWO = "twophase"
    PHASE_LIQ = "liquid"
    PHASE_GAS = "gas"
    
    def setup(self, strFluid):
        # member varbiable
        self.m_error = CoolPrp.ERR_FUILD_NOT_FOUND
        self.m_error_desc = ""
        self.m_fluid = None

        lstFluid = get_global_param_string("FluidsList").split(',')

        # CP.PropsSI('D','T',300,'P',101325,'HEOS::R32[0.697615]&R125[0.302385]')
        if strFluid in lstFluid:
            self.m_fluid = strFluid
            self.m_error = CoolPrp.ERR_NOT_FOUND
        else:
            self.m_error_desc = strFluid

    # -----------------------------------------------------------
    # Job 			: Check if there are an error
    #
    # Input 		:
    # Output		: True if error, else false
    # -----------------------------------------------------------
    def isError(self):
        # return true if error, else return false
        return self.m_error != CoolPrp.ERR_NOT_FOUND

    # -----------------------------------------------------------
    # Job 			: descript the error occured if any
    # Input 		:
    #
    # Output		: text of error number and error description
    # -----------------------------------------------------------
    def err_description(self):
        if self.m_error == CoolPrp.ERR_FUILD_NOT_FOUND:
            return "Err " + str(CoolPrp.ERR_FUILD_NOT_FOUND) + \
                " Fuild is not supported: " + str(self.m_error_desc)

        elif self.m_error == CoolPrp.ERR_PROB_ERROR:
            return "Err " + str(CoolPrp.ERR_PROB_ERROR) + \
                " Call Prop error, error in parameters " + str(self.m_error_desc)

        elif self.m_error == CoolPrp.ERR_PROB_NOT_FOUND:
            return "Err " + str(CoolPrp.ERR_PROB_NOT_FOUND) + \
                " Property not supported: " + str(self.m_error_desc)

        elif self.m_error == CoolPrp.ERR_NOT_FOUND:
            return "No error."
        else:
            return "No error description, info. number CoolPrp:" + \
                str(self.m_error)

    # -----------------------------------------------------------
    # Job 			: get of of the properties 'P','T','V','V','H','S','CP','CV'
    # Input 		: getProp code of required property, referance properties
    #
    # Output		: text of error number and error description
    # -----------------------------------------------------------
    def Property(self, getProp, P=None, T=None, V=None, D=None,
                 H=None, S=None, X=None):
        MAX_PARA = 2
        lst_prob_io = ['P', 'T', 'V', 'D', 'H', 'S']  # props for input/output
        lst_prob_o = ['CP', 'CV']  # props for output only
        lst_prob_i = ['Q']  # props for input only

        # adjust X (quality) - CoolPrp use Q for quality
        Q = X
            
        getProp_adj = getProp.upper()

        # Check that input property in good, else raise an error
        if getProp_adj not in lst_prob_io + lst_prob_o:
            self.m_error = CoolPrp.ERR_PROB_NOT_FOUND
            self.m_error_desc = "Property: " + getProp
            return

        # adjust id for Cp and Cv
        if getProp_adj in lst_prob_o:
            getProp_adj = getProp_adj + "MASS"

        # adjust V (volume) - CoolPrp use D for dencity
        if getProp == 'V':
            getProp_adj = "D"

        # get two referance parameters
        int_count = 0

        try:
            str_command = "PropsSI(" + "'" + getProp_adj + "'"
            for prop in lst_prob_io + lst_prob_i:
                if eval(prop + " !=None"):
                    int_count = int_count + 1

                    if prop == 'V':  # CoolPrp use D for dencity
                        prob_code = "D"
                        prob_val = 1 / eval(prop)
                    else:
                        prob_code = prop
                        prob_val = eval(prop)

                    str_command = str_command + ", " + "'" + \
                        prob_code + "'" + ", " + str(prob_val)

                    if int_count >= MAX_PARA:
                        break  # only limited number of parameters

            str_command = str_command + ', self.m_fluid)'

            result = eval(str_command)
            if getProp == 'V':
                result = 1 / result

            return result
        except BaseException:
            self.m_error = CoolPrp.ERR_PROB_ERROR
            self.m_error_desc = str_command
            return None

    # -----------------------------------------------------------
    # Job 			: Get Used Fuild
    # Input 		:
    #
    # Output		: Name of used fuild
    # -----------------------------------------------------------
    def getFuild(self):
        return self.m_fluid

    # -----------------------------------------------------------
    # Job 			: Get Critical Temp
    # Input 		:
    #
    # Output		: Critical Temp in K
    # -----------------------------------------------------------
    def getCrtiticalTemp(self):
        if self.isError():
            return None
        return PropsSI(self.m_fluid, "Tcrit")

    # -----------------------------------------------------------
    # Job 			: Get Critical Press
    # Input 		:
    #
    # Output		: Critical Pressure in Pa
    # -----------------------------------------------------------
    def getCrtiticalPress(self):
        if self.isError():
            return None
        return PropsSI(self.m_fluid, "Pcrit")

    # -----------------------------------------------------------
    # Job 			: Get Critical Press
    # Input 		:
    #
    # Output		: Critical Volume in kg/m3
    # -----------------------------------------------------------
    def getCrtiticalVolume(self):
        if self.isError():
            return None
        return PropsSI(self.m_fluid, "rhocrit")

    # -----------------------------------------------------------
    # Job 			: Check if liquied or gas phase
    # Input 		: fltPressure pressure in Pascal, temperature in K
    #
    # Output		: String descript the phase gas, liquid or twophase
    #                   supercritical_gas
    # -----------------------------------------------------------

    def phase_byPressTemp(self, flt_P_Pascal, flt_Temp_K):
        if self.isError():
            return None

        return PhaseSI("P", flt_P_Pascal, "T", flt_Temp_K, self.m_fluid)

    # -----------------------------------------------------------
    # Job 			: Check if liquied or gas phase
    # Input 		: phase in string, as return from phase function
    #
    # Output		: True or false
    # -----------------------------------------------------------
    def is_gas_phase (self, strPhase):
        if CoolPrp.PHASE_GAS in (strPhase):
            return True
        else:
            return False

    def is_liquid_phase (self, strPhase):
        if CoolPrp.PHASE_LIQ in (strPhase):
            return True
        else:
            return False

    def is_two_phase (self, strPhase):
        if CoolPrp.PHASE_TWO in (strPhase):
            return True
        else:
            return False            
# =====================
def main():
    objCP = CoolPrp()
    objCP.setup("R12")

    if objCP.isError():
        print(objCP.err_description())
        return

    TK_C = 273.15  # K
    T1 = TK_C + 72  # K
    P1 = 200000  # Pa
    # H = 400061.6198132278	# J/kg
    S = 1741.3033288796132  # J/kg/K
    # D = 8.632966162031755  #kg/m3
    V = 0.11583504223589493  # m3/kg

    print("Fuild: ", objCP.getFuild())
    print("----------------------------------------")
    print("Critical temperature: ", objCP.getCrtiticalTemp(), "K")
    print("Critical Pressure:    ", objCP.getCrtiticalPress(), "Pa")
    print("Critical Volume:      ", objCP.getCrtiticalVolume(), "kg/m3")

    print("----------------------------------------")
    print("---Saturated condition")
    T1 = TK_C + 18  # K
    P1 = 535130  # Pa
    print("Given P1=", P1, "  T1=", T1)
    print("----------------------------------------")
    print("V-liq  by T ", objCP.Property('V', T=T1, X=0), 'kg/m3')
    print("V-gas  by T ", objCP.Property('V', T=T1, X=1), 'm3/kg')
    print("D-liq  by T ", objCP.Property('D', T=T1, X=0), 'kg/m3')
    print("D-gas  by T ", objCP.Property('D', T=T1, X=1), 'm3/kg')

    print("----------------------------------------")
    print("V-liq  by P ", objCP.Property('V', P=P1, X=0), 'm3/kg')
    print("D-liq  by P ", objCP.Property('D', P=P1, X=0), 'kg/m3')
    print("V-liq  by P ", objCP.Property('V', P=P1, X=1), 'm3/kg')
    print("D-liq  by P ", objCP.Property('D', P=P1, X=1), 'kg/m3')

    print("----------------------------------------")
    Tsat = 315.14934314624594
    print("Sat-Temp for P = 1006800 Pa by P X=0",
          objCP.Property('T', P=1006800, X=0), 'K')
    print("Sat-Temp for P = 1006800 Pa by P X=1",
          objCP.Property('T', P=1006800, X=1), 'K')

    print("Sat-Pressure for T = 315.15 Pa by P X=0",
          objCP.Property('P', T=Tsat, X=0), 'Pa')
    print("Sat-Pressure for T = 315.15 Pa by P X=1",
          objCP.Property('P', T=Tsat, X=1), 'Pa')

    print("Phase at T=373 K, P = 1013250 Pa: (confirmed)",
          objCP.phase_byPressTemp(1013250, TK_C + 100))

    print("========================================")
    print("---others=-=-=-=-=")
    print("H (liquid) at T=273 K, P = 308150 Pa: by P, T",
          objCP.Property('h', P=308150, T=TK_C), 'J/kg/K')

    print("----------------------------------------")
    print(
        "Cp  at T=273 K, P = 308150 Pa: by P,T",
        objCP.Property(
            'cp',
            P=308150,
            T=TK_C),
        'J/kg/K')
    print(
        "Cp  at T=273 K, P = 308150 Pa: by P,T",
        objCP.Property(
            'cv',
            P=308150,
            T=TK_C),
        'J/kg/K')

    print("Cp  by P, V", objCP.Property('cp', P=P1, V=V), 'J/kg/K')

    print("Cp  by P, V", objCP.Property('cv', P=P1, V=V), 'J/kg/K')

    print("----------------------------------------")
    print("Given P1=", P1, "  T1=", T1, "V=", V)
    print("S   by P, T ", objCP.Property("S", P=P1, T=T1), 'J/kg/K')

    print("S   by T, V ", objCP.Property("S", T=T1, V=V), 'J/kg/K')

    print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
    print("V       Press,Temp ", objCP.Property("V", P=P1, T=T1), 'J/kg/K')
    print("Den     Press,Temp ", objCP.Property("D", P=P1, T=T1), 'kg/m3')
    print("T          by S, P ", objCP.Property("T", S=S, P=P1), 'K')

    print("----------------------------------------")
    print("H (gas) by P,T ", objCP.Property("h", P=P1, T=T1), 'J/kg')
    print("H       by Press, Vol  ", objCP.Property("h", P=P1, V=V), 'J/kg')
    print("H       by T, V  ", objCP.Property("h", T=T1, V=V), 'J/kg')

    print("========================================")
    print ("T1 = ", T1)
    print("H       by T, X  ", objCP.Property("h", T=T1, X=0), 'J/kg')
    if objCP.isError():
        print ("Error: " + objCP.err_description())

    print("========================================")
    print ("Output ->",objCP.phase_byPressTemp (1013250, 273+100))
    print ("Is Gas ->",objCP.is_gas_phase(objCP.phase_byPressTemp (1013250, 273+100)))

    print("========================================")
    print ("Ouput  ->",objCP.phase_byPressTemp (1013250, 273+00 ))
    print ("Is liquid ->", objCP.is_liquid_phase(objCP.phase_byPressTemp (1013250, 273+00)))
# =====================
if __name__ == '__main__':
    main()
