# Python Import ==================
import sys


# User Import
from common_classes.FileAccess import FileAccess

#==============================
class CompMap (FileAccess):
	
	def __init__(self, strFileName, strPath = "", is_one_dim = False):
		# member varbiable for compressor
		self.is_one_dim = is_one_dim
		
		self.int_unit = 0
		
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
		self.m_error =  FileAccess.ERR_NOT_FOUND	# Current error
		self.m_error_desc = ""
		
		self.m_text = ""		# last line that was read from file.
		self.m_access = ""	# Access type r,w,a,x as list
		self.m_name = ""		# Path & file name to access
		self.m_lineNo = 0	# Line number, last read
		self.m_handler = ""			
		
		self.current_pos = 0
				
		# default strPath is this script path
		if strPath == "":
			strPath = sys.path[0] # get current path
		
		# Full name  is path and file name			
		self.m_name  = strPath + "\\" + strFileName
		
		# Save to member variable
		self.m_access = "rb" # read binary
		
		try:
			# open Text file in current directory with the given access 
			self.m_handler = open( self.m_name, self.m_access)
			
		except OSError as err_description:
			self.m_error_desc = str(err_description)
			self.m_error = CompMap.ERR_FILE_ACCESS
			
	#-----------------------------------------------------------
	def readValue (self, int_pos=-1, isWord = False):
		# check if there is an error
		b_current_err = self.isError()
		if self.isError() :
			return not b_current_err 	# if error do nothing, return false
		
		if int_pos != -1:
			self.current_pos = int_pos 
		
		if isWord :
			int_bytes_count = 2
		else:
			int_bytes_count = 1
			
		# seek to position
		self.m_handler.seek(self.current_pos )
		
		# set no error flag
		self.m_error = FileAccess.ERR_NOT_FOUND
		try:
			while True:
				self.bytes = self.m_handler.read(int_bytes_count)	# read rec. length
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
		
		return not self.isError() # if read done, then true, else false

	#-----------------------------------------------------------
	def readrecord (self, int_pos = -1):
		# check if there is an error
		b_current_err = self.isError()
		if self.isError() :
			return not b_current_err 	# if error do nothing, return false
		
		if int_pos != -1:
			self.current_pos = int_pos 
		
		# set no error flag
		self.m_error = FileAccess.ERR_NOT_FOUND
				
		try:
			# seek to position
			self.m_handler.seek(self.current_pos )
			while True:
				byte = self.m_handler.read(1)	# read rec. length
				self.current_pos = self.current_pos + 1
				
				if byte == b"":
					self.m_error = FileAccess.ERR_BLANK_LINE
					return not self.isError()
				
				int_rec_len = int.from_bytes(byte, "big")
				if int_rec_len == 0:
					continue
					
				self.bytes = self.m_handler.read(int_rec_len)	# read rec. length
				self.current_pos = self.current_pos + int_rec_len 
				break
			
		except OSError as err:
			self.m_error = err
		
		return not self.isError() # if read done, then true, else false
	
	#-----------------------------------------------------------
	def getRec (self):
		self.m_text = self.bytes.decode("utf-8")
		return self.m_text 

	#-----------------------------------------------------------
	def getValue (self):
		return int.from_bytes(self.bytes, "big")

	#-----------------------------------------------------------
	def setBasicInfo (self):
		self.readrecord(); self.str_manf = self.getRec()
		self.readrecord(); self.str_model = self.getRec()
		self.readrecord(); self.str_kcal_hr = self.getRec()
		self.readrecord(); self.str_eer = self.getRec()
		self.readrecord(); self.str_rpm = self.getRec()
		self.readrecord(); self.str_volt = self.getRec()
	
	#-----------------------------------------------------------
	def setX_Y (self):
		self.readValue(); self.int_unit  = self.getValue()
		self.readValue(); self.int_y12_values = self.getValue()
		self.readValue(); self.int_x_values = self.getValue()
	
	#-----------------------------------------------------------
	def read_Xdata (self):
		self.arr_x_values = [0.0] * (self.int_x_values)
		for ncnt in range (0, self.int_x_values):
			if not self.readrecord (): return
			self.arr_x_values [ncnt] = self.getRec ()
			
	#-----------------------------------------------------------
	def read_Ydata (self):
		self.arr_y1_values = [0.0] * (self.int_y12_values)
		self.arr_y2_values = [0.0] * (self.int_y12_values)
		
		for ncnt in range (0, self.int_y12_values):
			if not self.readrecord (): return
		
			self.arr_y1_values [ncnt] = self.getRec ()
			
			if not self.readrecord (): return
			self.arr_y2_values [ncnt] = self.getRec ()
			
	#-----------------------------------------------------------
	def readCapacity (self):
		if self.is_one_dim :
			self.arr_capacity = [0.0] * (self.int_x_values * self.int_y12_values)
		else:
			self.arr_capacity = [[0.0] * (self.int_x_values) for i in range(self.int_y12_values)]	
		
		#add shift one word to the current position
		for ncnt_x in range (0, self.int_y12_values):
			#self.current_pos = self.current_pos + CompMap.SEG_BLOCK
			for ncnt_y in range (0, self.int_x_values):
				if not self.readrecord ():  return
				
				if self.is_one_dim :
					self.arr_capacity [int(ncnt_x * self.int_x_values + ncnt_y)] = self.getRec ()
				else:
					self.arr_capacity [ncnt_x][ncnt_y] = self.getRec ()				
	
	#-----------------------------------------------------------
	def readPower (self):
		if self.is_one_dim :
			self.arr_power = [0.0] * (self.int_x_values * self.int_y12_values)
		else:
			self.arr_power = [[0.0] * (self.int_x_values) for i in range(self.int_y12_values)]
			
			
		#add shift one word to the current position
		#self.current_pos = self.current_pos + CompMap.SEG_POWER - CompMap.SEG_BLOCK
		
		for ncnt_x in range (0, self.int_y12_values):
			#self.current_pos = self.current_pos + CompMap.SEG_BLOCK
			for ncnt_y in range (0, self.int_x_values):
				if not self.readrecord (): return
				
				if self.is_one_dim :
					self.arr_power [int(ncnt_x * self.int_x_values + ncnt_y)] = self.getRec ()
				else:
					self.arr_power [ncnt_x][ncnt_y] = self.getRec ()

	#-----------------------------------------------------------
	def readMapData (self):
		self.setBasicInfo()
		self.setX_Y()
		
		self.read_Ydata()
		self.read_Xdata()
		
		self.readCapacity ()
		self.readPower ()
		
	
	def getManif (self):
		return self.str_manf 
	#-----------------------------------------------------------
	def getModel (self):
		return self.str_model 
	#-----------------------------------------------------------
	def getKcal (self):	
		return  self.str_kcal_hr
	#-----------------------------------------------------------
	def getEer (self):	
		return self.str_eer 
	#-----------------------------------------------------------
	def getRpm (self):
		return self.str_rpm 
	#-----------------------------------------------------------
	def getVolt (self):
		return self.str_volt 

	#-----------------------------------------------------------
	def getY_count (self):
		return self.int_y12_values 

	#-----------------------------------------------------------
	def getX_count (self):
		return self.int_x_values 

	#-----------------------------------------------------------
	def getUnit (self):
		return self.int_unit 

	#-----------------------------------------------------------
	def getCapacity (self):
		return self.arr_capacity  

	#-----------------------------------------------------------
	def getPower(self):
		return self.arr_power  

	#-----------------------------------------------------------
	def getX_values(self):
		return self.arr_x_values
		
	#-----------------------------------------------------------
	def getY1_values(self):
		return self.arr_y1_values  

	#-----------------------------------------------------------
	def getY2_values(self):
		return self.arr_y2_values  
		
