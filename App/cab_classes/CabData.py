# Python Import


# User Import
from common_classes.DataModelBuiler import DataModelBuiler

# -----------------------------------------------------------
# Job 			: Creates object with data from file for Q class
#
# Editor		: aymhenry@gmail.com
# -----------------------------------------------------------


class CabData (DataModelBuiler):
    # -----------------------------------------------------------
    # Job 			: Assign default values for class
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
 
    def __init__(self, strFileName, strPath=""):
        super().__init__(strFileName, strPath)
        
        DataModelBuiler.CONFIGRATION_COUNT = 7 			# max. number of configrations
        DataModelBuiler.CONFIGRATION_ROW = 6 			# set row number that has the configration
        
        # required data for each configration
        DataModelBuiler.lst_required_data = [71, 68, 71, 44, 47, 53, 71]

    def build_var_list(self, int_config_no=None):
        if not self.chk_befor_assign(int_config_no):
            return False

        # assign vars in data object
        self.assign_vars()
        return True
        
    def assign_vars(self):
        lst_config1 = [
            'IRFTYP',
            'HEIGHT',
            'WIDTH',
            'DEPTH',
            'WEDGE',
            'FLANGE',
            'WEDGER',
            'FLANGER',
            'DOOREDG',
            'DGSKT',
            'CDUP',
            'CDDN',
            'CCHGT',
            'DOL',
            'COL',
            'DIL',
            'CIL',
            'TOPMUL',
            'THMUL',
            'TIFT',
            'TIFLS',
            'TIFRS',
            'TIFF',
            'TIFB',
            'TIRLS',
            'TIRRS',
            'TIRF',
            'TIRB',
            'BINSUL',
            'CINSUL',
            'HXVUZ',
            'VOLAZ',
            'HXVUR',
            'VOLAR',
            'TROOM',
            'TFRZ',
            'TFF',
            'TBTM',
            'RFF',
            'RFRZ',
            'RWEDGER',
            'RWEDGE',
            'RDRFF',
            'RDRFZ',
            'RMUL',
            'HLFZG',
            'HLRG',
            'TDRAIR',
            'RELHUM',
            'FFCOPN',
            'SECFFC',
            'FRZOPN',
            'SECFRZ',
            'FFASHW',
            'FZASHW',
            'FFAUXW',
            'FZAUXW',
            'OTHERW',
            'FFPENA',
            'FZPENA',
            'FFASHQ',
            'FZASHQ',
            'FFREFQ',
            'FZREFQ',
            'FFHEAT',
            'FZHEAT']
        lst_config2 = [
            'IRFTYP',
            'HEIGHT',
            'WIDTH',
            'DEPTH',
            'WEDGE',
            'FLANGE',
            'WEDGER',
            'FLANGER',
            'DOOREDG',
            'DGSKT',
            'CDUP',
            'CDDN',
            'CCHGT',
            'DOL',
            'COL',
            'DIL',
            'CIL',
            'TOPMUL',
            'THMUL',
            'TIFRS',
            'TIFF',
            'TIFB',
            'TIRT',
            'TIRLS',
            'TIRF',
            'TIRB',
            'BINSUL',
            'HXVUZ',
            'VOLAZ',
            'HXVUR',
            'VOLAR',
            'TROOM',
            'TFRZ',
            'TFF',
            'TBTM',
            'RFF',
            'RFRZ',
            'RWEDGER',
            'RWEDGE',
            'RDRFF',
            'RDRFZ',
            'RMUL',
            'HLGZF',
            'HLRG',
            'TDRAIR',
            'RELHUM',
            'FFCOPN',
            'SECFFC',
            'FRZOPN',
            'SECFRZ',
            'FFASHW',
            'FZASHW',
            'FFAUXW',
            'FZAUXW',
            'OTHERW',
            'FFPENA',
            'FZPENA',
            'FFASHQ',
            'FZASHQ',
            'FFREFQ',
            'FZREFQ',
            'FFHEAT',
            'FZHEAT']
        lst_config3 = [
            'IRFTYP',
            'HEIGHT',
            'WIDTH',
            'DEPTH',
            'WEDGE',
            'FLANGE',
            'WEDGER',
            'FLANGER',
            'DOOREDG',
            'DGSKT',
            'CDUP',
            'CDDN',
            'CCHGT',
            'DOL',
            'COL',
            'DIL',
            'CIL',
            'WALL',
            'THMUL',
            'TIFT',
            'TIFRS',
            'TIFF',
            'TIFB',
            'TIRT',
            'TIRLS',
            'TIRF',
            'TIRB',
            'BINSUL',
            'CINSUL',
            'BINFRZ',
            'HXVUZ',
            'VOLAZ',
            'HXVUR',
            'VOLAR',
            'TROOM',
            'TFRZ',
            'TFF',
            'TBTM',
            'RFF',
            'RFRZ',
            'RWEDGER',
            'RWEDGE',
            'RDRFF',
            'RDRFZ',
            'RMUL',
            'HLGZF',
            'HLRG',
            'TDRAIR',
            'RELHUM',
            'FFCOPN',
            'SECFFC',
            'FRZOPN',
            'SECFRZ',
            'FFASHW',
            'FZASHW',
            'FFAUXW',
            'FZAUXW',
            'OTHERW',
            'FFPENA',
            'FZPENA',
            'FFASHQ',
            'FZASHQ',
            'FFREFQ',
            'FZREFQ',
            'FFHEAT',
            'FZHEAT']
        lst_config4 = [
            'IRFTYP',
            'HEIGHT',
            'WIDTH',
            'DEPTH',
            'DOOREDG',
            'DGSKT',
            'CWIDE',
            'CHGT',
            'DOL',
            'COL',
            'DIL',
            'CIL',
            'TIFT',
            'TIFRS',
            'TIFF',
            'TIFB',
            'BINSUL',
            'CINSUL',
            'SCIN',
            'STIN',
            'HXVUZ',
            'VOLAZ',
            'TROOM',
            'TFRZ',
            'TBTM',
            'RCAB',
            'RTOP',
            'HLFZG',
            'TDRAIR',
            'RELHUM',
            'FRZOPN',
            'SECFRZ',
            'FZASHW',
            'FZAUXW',
            'OTHERW',
            'FZPENA',
            'FZASHQ',
            'FZREFQ',
            'FZHEAT']
        lst_config5 = [
            'IRFTYP',
            'HEIGHT',
            'WIDTH',
            'DEPTH',
            'WEDGE',
            'FLANGE',
            'DOOREDG',
            'DGSKT',
            'CDUP',
            'CDDN',
            'CCHGT',
            'DOL',
            'COL',
            'DIL',
            'CIL',
            'TIFT',
            'TIFLS',
            'TIFRS',
            'TIFF',
            'TIFB',
            'BINSUL',
            'CINSUL',
            'HXVUZ',
            'VOLAZ',
            'TROOM',
            'TFRZ',
            'TBTM',
            'RCAB',
            'RWEDGE',
            'RDOOR',
            'HLFZG',
            'TDRAIR',
            'RELHUM',
            'FRZOPN',
            'SECFRZ',
            'FZASHW',
            'FZAUXW',
            'OTHERW',
            'FZPENA',
            'FZASHQ',
            'FZREFQ',
            'FZHEAT']
        lst_config6 = [
            'IRFTYP',
            'HEIGHT',
            'WIDTH',
            'DEPTH',
            'WEDGE',
            'FLANGE',
            'DOOREDG',
            'DGSKT',
            'CDUP',
            'CDDN',
            'CCHGT',
            'DOL',
            'COL',
            'DIL',
            'CIL',
            'TIFT',
            'TIFLS',
            'TIFRS',
            'TIFF',
            'TIFB',
            'BINSUL',
            'CINSUL',
            'HXVUZ',
            'VOLAZ',
            'HXVUR',
            'VOLAR',
            'FH',
            'FW',
            'FD',
            'TROOM',
            'TFRZ',
            'TFF',
            'TBTM',
            'RCAB',
            'RWEDGE',
            'RDOOR',
            'HLFZG',
            'TDRAIR',
            'RELHUM',
            'FRZOPN',
            'SECFRZ',
            'FZASHW',
            'FZAUXW',
            'OTHERW',
            'FZPENA',
            'FZASHQ',
            'FZREFQ',
            'FZHEAT']
        # lst_config7 =  same as lst_config1

        lst_var_names = []

        # according to the given configration in data file, saved in int_configration
        # set the realtive list of variable name in list
        if self.int_configration == 1:
            lst_var_names = lst_config1

        elif self.int_configration == 2:
            lst_var_names = lst_config2

        elif self.int_configration == 3:
            lst_var_names = lst_config3

        elif self.int_configration == 4:
            lst_var_names = lst_config4

        elif self.int_configration == 5:
            lst_var_names = lst_config5

        elif self.int_configration == 6:
            lst_var_names = lst_config6

        elif self.int_configration == 7:
            lst_var_names = lst_config1

        # send data list of variable, and relative list of values to data object.
        # data object put values of data list on variable
        self.obj_qdata.setup_vars(
            self.lst_data,
            lst_var_names,
            self.CONFIGRATION_ROW,
            self.int_parameter_count)

        # create a list with items title1 ,2 .. up to CONFIGRATION_ROW
        lst_title = ['title' + str(n) for n in range(0, self.CONFIGRATION_ROW)]

        # create vars title1 up to CONFIGRATION_ROW, with the given values in
        # list
        self.obj_qdata.setup_vars(
            self.lst_data,
            lst_title,
            0,
            self.CONFIGRATION_ROW - 1,
            True)

