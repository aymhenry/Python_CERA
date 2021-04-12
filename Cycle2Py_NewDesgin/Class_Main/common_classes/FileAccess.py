# Python Import
import sys

# User Import

# -----------------------------------------------------------
# Job 			: Read/write data to text file
#
# Editor		: aymhenry@gmail.com
# -----------------------------------------------------------


class FileAccess:
    # Class Startic vars
    # Error internal code
    ERR_NOT_FOUND = 0  # no error code
    ERR_FOUND = 20   	# some error found
    ERR_FILE_ACCESS = 30   	# some error found
    ERR_BLANK_LINE = 40   	# blank line in data entry

    def __init__(self, strFileName, strAccess="", strPath=""):
        # "r" - read - Default value. Opens a file for reading, error if the file does not exist
        # "a" - append - Opens a file for appending, creates the file if it does not exist
        # "w" - write - Opens a file for writing, creates the file if it does not exist
        # "x" - create - Creates the specified file, returns an error if the file exists

        # "t" - Text - Default value. Text mode (Default)
        # "b" - Binary - Binary mode (e.g. images)

        # member varbiable
        self.m_error = FileAccess.ERR_NOT_FOUND  # Current error
        self.m_error_desc = ""

        self.m_text = ""		# last line that was read from file.
        self.m_access = ""  # Access type r,w,a,x as list
        self.m_name = ""		# Path & file name to access
        self.m_lineNo = 0  # Line number, last read
        self.m_handler = ""

        # Create Access Dic.
        FILE_ACCESS = {
            'read': 'r',
            'append': 'a',
            'write': 'w',
            'create': 'x'}  # Access dictionary

        # default strPath is this script path
        if strPath == "":
            strPath = sys.path[0]  # get current path

        # Full name  is path and file name
        self.m_name = strPath + "\\" + strFileName

        if not (
                strAccess in FILE_ACCESS): 	# check if the give access is not found in standard list
            strAccess = "read"  			# if not found make it Read

        # Save to member variable
        self.m_access = FILE_ACCESS.get(strAccess)

        try:
            # open Text file in current directory with the given access
            self.m_handler = open(self.m_name, self.m_access)

        except OSError as err_description:
            self.m_error_desc = str(err_description)
            self.m_error = FileAccess.ERR_FILE_ACCESS

    # -----------------------------------------------------------
    # Job 			: Check if there are an error
    #
    # Input 		:
    # Output		: True if error, else false
    # -----------------------------------------------------------

    def isError(self):
        # return true if error, else return false
        return self.m_error != FileAccess.ERR_NOT_FOUND

    # -----------------------------------------------------------
    # Job 			: Write text on the opened file
    #
    # Input 		: strText : String, Text - data to write in text file
    # Output		: True if data is written, false if error
    # -----------------------------------------------------------
    def write(self, strText):
        # check if there ia an error
        if self.m_access == "r":
            self.terminate("Access open - Read Only")
            return

        b_current_err = self.isError()
        if self.isError():
            return not b_current_err 	# if error do nothing, return false

        # set no error flag
        self.m_error = FileAccess.ERR_NOT_FOUND
        try:
            self.m_handler.write(strText + "\r")  # add a trailing newline
        except OSError as err:
            self.m_error = err

        return not self.isError()  # if error return false, else true

    # -----------------------------------------------------------
    # Job 			: Write text on the opened file, if error terminate app
    #
    # Input 		: strText : String, Text - data to write in text file
    # Output		: True if data is written, false if error
    # -----------------------------------------------------------
    def write_or_terminate(self, strText):
        if not self.write(strText):
            self.terminate("Error write data to file " +
                           self.m_name + " Err" + str(self.m_error))

    # -----------------------------------------------------------
    # Job 			: Write text on the opened file, if error terminate app
    #
    # Input 		: strText : String, Text - data to write in text file
    # Output		: True if data is written, false if error
    # -----------------------------------------------------------
    def read_or_terminate(self):
        if not self.readline():
            self.terminate("Error read data to file " + self.m_name)

        str_Text = self.getText()
        if str_Text == "":
            self.terminate("EOF mark, No Data to read")
        return self.getText()

    # -----------------------------------------------------------
    # Job 			: Read one line of text from the opened file
    #
    # Input 		: num_col_num : col number to read, if <0 read all Cols
    #
    # Output		: True if data is written, false if error
    # -----------------------------------------------------------
    def readline(self, num_col_num=0):
        # read line for file
        # check if there is an error
        b_current_err = self.isError()
        if self.isError():
            return not b_current_err 	# if error do nothing, return false

        # set no error flag
        self.m_error = FileAccess.ERR_NOT_FOUND
        try:
            str_all_line = self.m_handler.readline().rstrip()  # remove a trailing newline

            if str_all_line == "":
                self.m_error = FileAccess.ERR_BLANK_LINE

                self.m_lineNo = self.m_lineNo + 1
                return not self.isError()

            if num_col_num < 0:
                self.m_text = str_all_line
            else:
                num_list = str_all_line.split(",")

                if num_col_num > len(num_list):
                    num_col_num = len(num_list) - 1

                self.m_text = num_list[num_col_num]

        except OSError as err:
            self.m_error = err

        self.m_lineNo = self.m_lineNo + 1
        # --for test only   print (self.m_lineNo, self.m_text)
        return not self.isError()  # if read done, then true, else false

    # -----------------------------------------------------------
    # Job 			: descript the error occured if any
    # Input 		:
    #
    # Output		: text of error number and error description
    # -----------------------------------------------------------
    def err_description(self):
        if self.m_error == FileAccess.ERR_FILE_ACCESS:
            return "Err " + str(FileAccess.ERR_FILE_ACCESS) + \
                str(self.m_error_desc)

        elif self.m_error == FileAccess.ERR_BLANK_LINE:
            return "Err " + str(FileAccess.ERR_BLANK_LINE) + \
                "Blank line in data file row:" + str(self.m_lineNo)

        elif self.m_error != FileAccess.ERR_NOT_FOUND:
            return "Err, can cannot continue"

        else:
            return "No error descriptio, info. number FieAcess:" + \
                str(self.m_error)

    # -----------------------------------------------------------
    # Job 			: return read line
    #
    # Input 		:
    # Output		: text return read line
    # -----------------------------------------------------------
    def getText(self):
        return self.m_text  # return read line

    # -----------------------------------------------------------
    # Job 			: Terminate App
    #
    # Input 		:
    # Output		:
    # -----------------------------------------------------------
    def terminate(self, strErr=""):
        # display File name & line number
        str_file_Info = "ERA app Terminated : " "\nFile " + self.m_name

        if self.m_access == "r":  # if read access
            str_file_Info = str_file_Info + " ,At line " + str(self.m_lineNo)

        strErr = str_file_Info + "\n" + strErr
        print(strErr)		# print error message

        self.__del__()  # close file
        sys.tracebacklimit = 0
        sys.exit('1000')

    # -----------------------------------------------------------
    # Job 			: Close file
    #
    # Input 		:
    # Output		:
    # -----------------------------------------------------------
    def __del__(self):
        # close file
        try:
            if self.m_handler != "":  # check if file handller is created
                self.m_handler.close()
        except OSError as err:
            self.m_error = err

        # File closed
        return
# =.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
# ___________ End of Cab class ___________________________________________
