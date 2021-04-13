# Python Import ==================
import sys

# User Import
from common_classes.FileAccess import FileAccess


# ==============================


class CompMap(FileAccess):
    TRY_COUNT = 120
    UNIT_SI = 1
    UNIT_ENG = 2

    def __init__(self, strFileName, strPath="", is_one_dim=False):
        # member varbiable for compressor
        super().__init__(strFileName, strPath)

        self.arr_x_values = None

        self.arr_y1_values = None
        self.arr_y2_values = None

        self.arr_capacity = None
        self.arr_power = None

        self.is_one_dim = is_one_dim

        # compressor type (1 - reciprocating; 2 - rotary)
        self.int_comp_type = 0

        self.int_x_values = 0
        self.int_y12_values = 0
        self.bytes = b''

        self.str_manf = ''
        self.str_model = ''
        self.str_kcal_hr = ''
        self.str_eer = ''
        self.str_rpm = ''
        self.str_volt = ''

        # member varbiable
        self.m_error = FileAccess.ERR_NOT_FOUND  # Current error
        self.m_error_desc = ""

        self.m_text = ""  # last line that was read from file.
        self.m_access = ""  # Access type r,w,a,x as list
        self.m_name = ""  # Path & file name to access
        self.m_lineNo = 0  # Line number, last read
        self.m_handler = ""

        self.current_pos = 0

        # default strPath is this script path
        if strPath == "":
            strPath = sys.path[0]  # get current path

        # Full name  is path and file name
        self.m_name = strPath + "\\" + strFileName

        # Save to member variable
        self.m_access = "rb"  # read binary

        try:
            # open Text file in current directory with the given access
            self.m_handler = open(self.m_name, self.m_access)

        except OSError as err_description:
            self.m_error_desc = str(err_description)
            self.m_error = CompMap.ERR_FILE_ACCESS

    # -----------------------------------------------------------
    def is_number(self, str_data):
        str_text = str_data.strip()  # remove space

        # check that no more than one(-) & ".",
        n_count_muns = str_text.count("-")
        n_count_dec = str_text.count(".")

        b_is_int = str_text.replace('.', '', 1).replace('-', '', 1).isdigit()

        if not b_is_int or n_count_muns > 1 or n_count_dec > 1:
            self.m_error_desc = " EAR App Error: Data Error, Required Number, given Text ->" + str_text + "<-"
            self.m_error = CompMap.ERR_FILE_ACCESS
            return False

        # (-) must be on left.
        str_text = str_text.replace('-', '', 1)

        flt_value = float(str_text)
        if n_count_muns == 1:
            flt_value = -1.0 * flt_value

        return True

    # -----------------------------------------------------------
    def readValue(self, int_pos=-1, isWord=False):

        # check if there is an error
        b_current_err = self.isError()
        if self.isError():
            return not b_current_err  # if error do nothing, return false

        if int_pos != -1:
            self.current_pos = int_pos

        if isWord:
            int_bytes_count = 2
        else:
            int_bytes_count = 1

        # seek to position
        self.m_handler.seek(self.current_pos)

        # set no error flag
        self.m_error = FileAccess.ERR_NOT_FOUND
        try:
            while True:
                self.bytes = self.m_handler.read(
                    int_bytes_count)  # read rec. length
                self.current_pos = self.current_pos + int_bytes_count

                if self.bytes == b"":
                    self.m_error = FileAccess.ERR_BLANK_LINE
                    return not self.isError()

                int_rec_len = int.from_bytes(self.bytes, "big")
                if int_rec_len == 0:
                    continue
                break

        except OSError as err:
            self.m_error = err

        return not self.isError()  # if read done, then true, else false

    # -----------------------------------------------------------
    def readrecord(self, int_pos=-1):
        # check if there is an error
        b_current_err = self.isError()
        if self.isError():
            return not b_current_err  # if error do nothing, return false

        if int_pos != -1:
            self.current_pos = int_pos

        # set no error flag
        self.m_error = FileAccess.ERR_NOT_FOUND

        try:
            # seek to position
            self.m_handler.seek(self.current_pos)
            while True:
                byte = self.m_handler.read(1)  # read rec. length
                self.current_pos = self.current_pos + 1

                if byte == b"":
                    self.m_error = FileAccess.ERR_BLANK_LINE
                    return not self.isError()

                int_rec_len = int.from_bytes(byte, "big")
                if int_rec_len == 0:
                    continue

                self.bytes = self.m_handler.read(
                    int_rec_len)  # read rec. length
                self.current_pos = self.current_pos + int_rec_len
                break

        except OSError as err:
            self.m_error = err

        return not self.isError()  # if read done, then true, else false

    # -----------------------------------------------------------
    def getRec(self, is_num=True):
        if self.isError():
            return

        self.m_text = self.bytes.decode("utf-8")

        if is_num:
            if self.is_number(self.m_text):
                self.m_text = float(self.m_text)

        return self.m_text

    # -----------------------------------------------------------
    def getValue(self):
        if self.isError():
            return
        return int.from_bytes(self.bytes, "big")

    # -----------------------------------------------------------
    def setBasicInfo(self):
        self.readrecord()
        self.str_manf = self.getRec(False)
        self.readrecord()
        self.str_model = self.getRec(False)
        self.readrecord()
        # some time is not given, text put
        self.str_kcal_hr = self.getRec(False)
        self.readrecord()
        self.str_eer = self.getRec(False)  # some time is not given, text put
        self.readrecord()
        self.str_rpm = self.getRec(False)  # some time is not given, text put
        self.readrecord()
        self.str_volt = self.getRec(False)  # some time is not given, text put

    # -----------------------------------------------------------
    def setX_Y(self):
        self.readValue()
        self.int_comp_type = self.getValue()
        self.readValue()
        self.int_y12_values = self.getValue()
        self.readValue()
        self.int_x_values = self.getValue()

    # -----------------------------------------------------------
    def read_Xdata(self):

        if self.isError():
            return

        self.arr_x_values = [0.0] * self.int_x_values  # base value is 0

        for ncnt in range(0, self.int_x_values):

            if ncnt == 0:  # check entry point for data, starts with none zzero value
                Value = self.getNonZero()

            else:
                if not self.readrecord():
                    return
                Value = self.getRec()

            self.arr_x_values[ncnt] = Value

    # -----------------------------------------------------------
    def read_Ydata(self):
        if self.isError():
            return

        self.arr_y1_values = [0.0] * self.int_y12_values  # base is 0
        self.arr_y2_values = [0.0] * self.int_y12_values

        for ncnt in range(0, self.int_y12_values):
            if not self.readrecord():
                return

            self.arr_y1_values[ncnt] = self.getRec()

            if not self.readrecord():
                return
            self.arr_y2_values[ncnt] = self.getRec()

    # -----------------------------------------------------------
    def readCapacity(self):
        if self.isError():
            return

        if self.is_one_dim:
            self.arr_capacity = [0.0] * \
                                (self.int_x_values * self.int_y12_values)
        else:
            self.arr_capacity = [[0.0] * self.int_x_values
                                 for i in range(self.int_y12_values)]

        # add shift one word to the current position
        for ncnt_x in range(0, self.int_y12_values):
            # self.current_pos = self.current_pos + CompMap.SEG_BLOCK
            for ncnt_y in range(0, self.int_x_values):

                if self.is_one_dim:
                    self.arr_capacity[
                        int(ncnt_x *
                            self.int_x_values +
                            ncnt_y)] = self.getNonZero()
                else:
                    self.arr_capacity[ncnt_x][ncnt_y] = self.getNonZero()

    # -----------------------------------------------------------
    def getNonZero(self):
        Value = 0
        nTry = 0
        str_last_err = ""
        # skip all zeros or errors if any
        while nTry < CompMap.TRY_COUNT and (Value == 0 or Value >= 100000):
            nTry = nTry + 1
            if not self.readrecord():
                return

            Value = self.getRec()
            if self.isError():
                str_last_err = self.m_error
                self.m_error = FileAccess.ERR_NOT_FOUND
                Value = 0
                # print ("Test nTry=",nTry, self.bytes.decode("utf-8") )
                continue

        # if exit without reading
        if nTry >= CompMap.TRY_COUNT:
            self.m_error = str_last_err

        return Value

    # -----------------------------------------------------------
    def readPower(self):
        if self.isError():
            return

        if self.is_one_dim:
            # base is 0
            self.arr_power = [0.0] * (self.int_x_values * self.int_y12_values)
        else:
            self.arr_power = [[0.0] * self.int_x_values
                              for i in range(self.int_y12_values)]

        # add shift one word to the current position

        for ncnt_x in range(0, self.int_y12_values):
            # self.current_pos = self.current_pos + CompMap.SEG_BLOCK
            for ncnt_y in range(0, self.int_x_values):

                if self.is_one_dim:
                    self.arr_power[int(
                        ncnt_x * self.int_x_values + ncnt_y)] = \
                        self.getNonZero()
                else:
                    self.arr_power[ncnt_x][ncnt_y] = self.getNonZero()

    # -----------------------------------------------------------
    def readMapData(self):
        self.setBasicInfo()
        self.setX_Y()

        self.read_Ydata()
        self.read_Xdata()

        self.readCapacity()
        self.readPower()

    def getManif(self):
        return self.str_manf

    # -----------------------------------------------------------

    def getModel(self):
        return self.str_model

    # -----------------------------------------------------------

    def getKcal(self):
        return self.str_kcal_hr

    # -----------------------------------------------------------

    def getEer(self):
        return self.str_eer

    # -----------------------------------------------------------

    def getRpm(self):
        return self.str_rpm

    # -----------------------------------------------------------

    def getVolt(self):
        return self.str_volt

    # -----------------------------------------------------------
    def getY_count(self):
        return self.int_y12_values

    # -----------------------------------------------------------
    def getX_count(self):
        return self.int_x_values

    # -----------------------------------------------------------
    def getUnit(self):
        if self.isError():
            return
        # capacity, temper, mass flow
        # unit 1 - kcal/hr, deg c, kg/hr
        # unit 2 - btu/hr,  deg f, lb/hr
        #
        # power data must be in watts
        #
        if self.arr_y1_values[1] <= 50:
            return CompMap.UNIT_SI  # kcal/hr, deg c, kg/hr
        else:
            return CompMap.UNIT_ENG  # btu/hr,  deg f, lb/hr

    # -----------------------------------------------------------
    def getType(self):
        return self.int_comp_type

    # -----------------------------------------------------------

    def getCapacity(self):
        return self.arr_capacity

    # -----------------------------------------------------------
    def getPower(self):
        return self.arr_power

    # -----------------------------------------------------------
    def getX_values(self):
        return self.arr_x_values

    # -----------------------------------------------------------
    def getY1_values(self):
        return self.arr_y1_values

    # -----------------------------------------------------------
    def getY2_values(self):
        return self.arr_y2_values
