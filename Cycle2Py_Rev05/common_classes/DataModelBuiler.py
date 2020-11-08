# Python Import


# User Import
from .FileAccess import FileAccess
from .QData import QData

# -----------------------------------------------------------
# Job 			: Creates object with data from file for Q class
#
# Editor		: aymhenry@gmail.com
# -----------------------------------------------------------


class DataModelBuiler (FileAccess):
    # maximum data file lines to read, this need to be updated if there is any
    # data line more than this
    MAX_DATA_FILE_TO_READ = 100
    CONFIGRATION_COUNT = 7 			# max. number of configrations
    CONFIGRATION_ROW = 6 			# set row number that has the configration
    # required data for each configration
    lst_required_data = [71, 68, 71, 44, 47, 53, 71]

    ERR_FOUND_LIST = 100   			# list content error
    ERR_FOUND_DATA_TYPE = 101   	# data type error
    ERR_FOUND_DATA_CONF = 102   	# configration num is not valid
    ERR_FOUND_DATA_COUNT = 103   	# error in number of parameter for the given file
    ERR_FOUND_READLINE = 104	   	# error during read data file
    ERR_FOUND_MAXLIE = 105		   	# lines in file more than expected
    ERR_FOUND_DATA_TYPE2 = 106   	# more than one decimal point, or (-) sign

    def __init__(self):
        self.lst_data = []
        self.obj_qdata = QData()
        self.int_parameter_count = None 	# number of parameter for the selected configration

        self.int_configration = None 		# configration selected

    # -----------------------------------------------------------
    # Job 			: get created data class
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
    def get_data_object(self):
        return self.obj_qdata
    # -----------------------------------------------------------
    # Job 			: Set basic data for super class
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------

    def set_init_data(self, strFileName, strPath=""):
        super().__init__(strFileName, "read", strPath)

    # -----------------------------------------------------------
    # Job 			: Create vars in object
    # Input 		:
    #
    # Output		: object from this sigle tone class
    # -----------------------------------------------------------
    def build_var_list(self):
        # if error return
        if not self.readlines():
            return False

        # check that all input data, set error flag if any
        self.chk_data_model()

        # read error flag, after last check, return false if any
        if self.isError():
            return False

        # close file all  data is ok
        super().__del__

        # assign vars in data object
        self.assign_vars()
        return True

    # -----------------------------------------------------------
    # Job 			: Assign values from table to selected class vars
    # Input 		:
    #
    # Output		:
    # -----------------------------------------------------------
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

    # -----------------------------------------------------------
    # Job 			: descript the error occured if any
    # Input 		:
    #
    # Output		: text of error number and error description
    # -----------------------------------------------------------
    def err_description(self):
        # check if error descript is found or send blank text
        def show_extar_err_desc():
            if self.m_error_desc is not None:
                return str(self.m_error_desc)
            else:
                return ""

        # return error description as the give error code
        if self.m_error == DataModelBuiler.ERR_FOUND:
            return "Err " + str(DataModelBuiler.ERR_NOT_FOUND) 	\
                + " File access error "	\
                + show_extar_err_desc()

        elif self.m_error == DataModelBuiler.ERR_FOUND_DATA_TYPE2:
            return "Err " + str(DataModelBuiler.ERR_FOUND_DATA_TYPE2) 	\
                + " more than one decimal point, or (-) sign "	\
                + show_extar_err_desc()

        elif self.m_error == DataModelBuiler.ERR_FOUND_DATA_TYPE:
            return "Err " + str(DataModelBuiler.ERR_FOUND_DATA_TYPE) 	\
                + " data type error, given string not float in input file "	\
                + show_extar_err_desc()

        elif self.m_error == DataModelBuiler.ERR_NOT_FOUND:
            return "No error found" + + show_extar_err_desc()

        elif self.m_error == DataModelBuiler.ERR_FOUND_LIST:
            return "Err " + str(DataModelBuiler.ERR_FOUND_LIST) +	\
                + " empty list,or very short list "	\
                + show_extar_err_desc()

        elif self.m_error == DataModelBuiler.ERR_FOUND_DATA_CONF:
            return "Err " + str(DataModelBuiler.ERR_FOUND_DATA_CONF) 	\
                + " configration num is not valid"	\
                + show_extar_err_desc()

        elif self.m_error == DataModelBuiler.ERR_FOUND_DATA_COUNT:
            return "Err " + str(DataModelBuiler.ERR_FOUND_DATA_COUNT) 	\
                + " error in number of parameter for the given file"	\
                + show_extar_err_desc()

        elif self.m_error == DataModelBuiler.ERR_FOUND_READLINE:
            return "Err " + \
                str(DataModelBuiler.ERR_FOUND_READLINE) + self.m_error_desc

        elif self.m_error == DataModelBuiler.ERR_FOUND_MAXLIE:
            return "Err " + str(DataModelBuiler.ERR_FOUND_MAXLIE) 	\
                + " number of line in file more than expeted, check file content"	\
                + show_extar_err_desc()

        elif self.m_error != FileAccess.ERR_NOT_FOUND:
            return super().err_description()

        else:
            return "No error descriptio, info. number DataModelBuiler:" + \
                str(self.m_error)

    # -----------------------------------------------------------
    # Job 			: check Configration data from list ,according to the goven configration
    # Input 		:
    #
    # Output		: True if data is written,false if error
    #	Set the int_configration  : configration number
    #			return True  if no error, else Flase
    # -----------------------------------------------------------
    def chk_data_model(self):
        # Configration from 1 to CONFIGRATION_ROW
        # every configration has number of parameters rquirted.
        # configration no. 1 count of parameters is lst_required_data[0]
        # configration not 5 is NA

        # every configration has a set of data, configration no.1 requires 71, 2 requires 68, and so on
        # no number in this list more than MAX_DATA_FILE_TO_READ, or this
        # constant needs update

        # read feed_back, related number of data to read
        feed_back = self.getData_from_list(
            DataModelBuiler.CONFIGRATION_ROW, "int")

        # if error is returned from the above method, return with Flase, with
        # error code set by getData_from_list
        if self.isError():
            self.err_desc = feed_back
            return False

        # if no error, then it save number
        self.int_configration = feed_back

        # check configration is in the accaptable range as setting in the
        # current class, if error return False
        if self.int_configration < 0 or self.int_configration > DataModelBuiler.CONFIGRATION_COUNT:
            self.m_error = DataModelBuiler.ERR_FOUND_DATA_CONF
            return False

        # retrive required input count of data
        self.int_parameter_count = self.lst_required_data[self.int_configration - 1]

        # if number of data given is not as the required by configration
        # number, return False with error code
        if len(self.lst_data) != self.int_parameter_count + \
                1:  # error if not the same number + header row
            self.m_error_desc = " given (" + str(len(self.lst_data)) + \
                ") required (" + str(self.int_parameter_count + 1) + ")"
            self.m_error = DataModelBuiler.ERR_FOUND_DATA_COUNT
            return False  # error in number of parameter for the given file

        # check that all list are number as required, or return False
        for int_items in range(
                DataModelBuiler.CONFIGRATION_ROW + 1,
                self.int_parameter_count):
            # read feed_back from method
            feed_back = self.getData_from_list(int_items, "float")

            # if error return save feed back error to error string member.
            if self.isError():
                self.m_error_desc = feed_back
                self.m_error = DataModelBuiler.ERR_FOUND_DATA_TYPE
                return False

        return True

    # -----------------------------------------------------------
    # Job 			: Read all lines of text from the opened file
    #
    # Input 		:
    #
    # Output		: True if data is written,false if error
    # -----------------------------------------------------------
    def readlines(self):
        # check if there was an error before, if so return.
        b_current_err = self.isError()
        if self.isError():
            return not b_current_err 	# if error do nothing,return false

        # set no error flag
        self.m_error = FileAccess.ERR_NOT_FOUND

        try:
            # reset line number to read
            self.m_lineNo = 0

            # do until check finish all lines
            while True:
                # set line count + 1
                self.m_lineNo = self.m_lineNo + 1

                # check that lines is not more this max. value, this for safety
                # only
                if self.m_lineNo > DataModelBuiler.MAX_DATA_FILE_TO_READ:
                    self.m_error = DataModelBuiler.ERR_FOUND_MAXLIE
                    break

                # read the current line (CSV) format
                str_all_line = self.m_handler.readline()

                # convert the line to list, items seperated by (,)
                num_list = str_all_line.split(",")  # convert line to list

                # if blank line, or no items in list then exit
                if str_all_line == "" or len(
                        num_list) < 1:  # if blank line or data then stop
                    break

                # update class list by the current read list
                # append first col,to the main list
                self.lst_data.append(num_list[0])

        except OSError as err_desc:
            self.m_error = DataModelBuiler.ERR_FOUND_READLINE
            # this var in super class FileAccess
            self.m_error_desc = str(err_desc) + " line" + str(self.m_lineNo)

        return not self.isError()  # if read done,then true,else false

    # -----------------------------------------------------------
    # Job 			: Read data from list,as the given type
    #
    # Input 		: int_row_number : row number to access
    #				: strType : 'int','float','string' according to required type.
    # Output		: if Error, flag is set, and return string of error descript
    #				other, return return string of the catched value
    # -----------------------------------------------------------
    def getData_from_list(self, int_row_number, strType=""):
        # check input parameter, if not good return
        if int_row_number > len(
                self.lst_data) or int_row_number < 0:  # exit if error entry
            # set Error Flag
            self.m_error = DataModelBuiler.ERR_FOUND_LIST
            return ""

        # read data from data list
        str_text = self.lst_data[int_row_number]

        # if type type is string then ok, return it
        if strType == "string":
            return str_text  # String return the value

        # else data required, is number. check the string is a number format
        # remove spaces if any
        str_text = str_text.strip()  # remove space

        # count then number of (.) and (-) in text, check that no more than
        # one(-) and one  "." only.
        n_count_muns = str_text.count("-")
        n_count_dec = str_text.count(".")

        # check that the remaning text is number, after removeing all (.)s and
        # (-)s
        b_is_int = str_text.replace('.', '', 1).replace('-', '', 1).isdigit()

        # if all data in text, is not numbers (b_is_int is false) or more than one (.) or more than one (.)
        # i.e. if error exit with error flag
        if not b_is_int or n_count_muns > 1 or n_count_dec > 1:
            # set Error Flag
            self.m_error_desc = " data given (" + str(str_text) + ")"
            self.m_error = DataModelBuiler.ERR_FOUND_DATA_TYPE2
            return 0  # "more than one decimal point, or (-) sign"

        # (-) must be on left. remove (-) from text if any
        str_text = str_text.replace('-', '', 1)

        # convert text to float
        flt_value = float(str_text)

        # if there was (-), convert number to -ve.
        if n_count_muns == 1:
            flt_value = -1.0 * flt_value

        # if the required type is float, then job is done.
        if strType == "float":
            return flt_value

        # Type required is int, convert float to int
        int_value = int(flt_value)

        return int_value  # int return

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
