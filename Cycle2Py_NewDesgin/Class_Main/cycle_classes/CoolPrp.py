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

                    if prop == 'V':  # CoolPrp use D for density
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
            # print ("str_command: ",str_command)

            result = eval(str_command)
            if getProp == 'V':
                result = 1 / result

            return result

        except:   # BaseException:
            self.m_error = CoolPrp.ERR_PROB_ERROR
            self.m_error_desc = str_command
            raise ValueError(self.err_description())

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
    def is_gas_phase(self, strPhase):
        if CoolPrp.PHASE_GAS in (strPhase):
            return True
        else:
            return False

    def is_liquid_phase(self, strPhase):
        if CoolPrp.PHASE_LIQ in (strPhase):
            return True
        else:
            return False

    def is_two_phase(self, strPhase):
        if CoolPrp.PHASE_TWO in (strPhase):
            return True
        else:
            return False
