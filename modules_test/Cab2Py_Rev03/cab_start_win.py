# Python Import ==================
import os
import tkinter as tk
from tkinter import *
#from tkinter import ttk
from functools import partial
from tkinter import messagebox as mb


# User Import ======================
from cab_classes.Start import Start
#------------------
class CabApp( Frame ):
	DATA_FOLDER  = "data_cab"
	FILE_INP_CAB = "Cabinet_dat.csv" 
	FILE_OUT_CAB = "Cabinet_out.csv"
	FILE_OUT_CYC = "Cycle_out.csv"
	
	#-----------------------------------------------------------
	# Job 			: Runc Start class as selected configration, as button pressed
	# Input 		:
	# Output		: 
	#-----------------------------------------------------------
	def __init__( self ):
		self.obj_start = Start()
		self.str_file_cab_in  = None
		self.str_file_cab_out = None
		
		tk.Frame.__init__(self)
		self.pack()
		self.master.title("Cab Application")

		self.button1 = Button( self, text = "Config1", width = 25,
							command=partial(self.set_config, 1) )

		self.button2 = Button( self, text = "Config2", width = 25,
							command=partial(self.set_config, 2) )

		self.button3 = Button( self, text = "Config3", width = 25,
							command=partial(self.set_config, 3) )
							
		self.button4 = Button( self, text = "Config4", width = 25,
							command=partial(self.set_config, 4) )

		self.button5 = Button( self, text = "Config5", width = 25,
							command=partial(self.set_config, 5) )

		self.button6 = Button( self, text = "Config6", width = 25,
							command=partial(self.set_config, 6) )
							
		self.button7 = Button( self, text = "Config7", width = 25,
							command=partial(self.set_config, 7) )
			
		self.buttonNew = Button( self, text = "User Current", width = 25,
							command=partial(self.set_config))
		
		self.button1.grid( row = 0, column = 1 )
		self.button2.grid( row = 0, column = 2)
		
		self.button3.grid( row = 1, column = 1)
		self.button4.grid( row = 1, column = 2)
		
		self.button5.grid( row = 2, column = 1)
		self.button6.grid( row = 2, column = 2)
		
		self.button7.grid( row = 3, column = 1)
		self.buttonNew.grid( row = 3, column = 2)

	#-----------------------------------------------------------
	# Job 			: run the main object class Start, using input file name
	# Input 		: Configration number
	#				 Creates input file name "n-Cabinet_dat.csv" and output "n-Cabinet_out.csv"
	#				  n is the configration number
	# Output		: 
	#-----------------------------------------------------------
	def set_config(self, int_config =0):
		strPath = sys.path[0] + "\\" + CabApp.DATA_FOLDER
		
		if int_config != 0:
			self.str_file_cab_in    = str(int_config) + "-" + CabApp.FILE_INP_CAB
			self.str_file_cab_out   = str(int_config) + "-" + CabApp.FILE_OUT_CAB
			self.str_file_cycle_out = str(int_config) + "-" + CabApp.FILE_OUT_CYC
		else:
			self.str_file_cab_in    = CabApp.FILE_INP_CAB
			self.str_file_cab_out   = CabApp.FILE_OUT_CAB
			self.str_file_cycle_out = CabApp.FILE_OUT_CYC
			
		if not self.isFileExisits  ( self.str_file_cab_in, strPath):
			mb.showerror("Error", "Cannot find file:" + strPath + "\\" + self.str_file_cab_in)
			return
		
		if not self.isFileExisits  ( self.str_file_cab_out, strPath ):
			mb.showerror("Error", "Cannot find file:" + strPath + "\\" + self.str_file_cab_out)
			return
		
		self.obj_start.set_filenames( self.str_file_cab_in, 	\
			self.str_file_cab_out, self.str_file_cycle_out, 	\
			strPath, strPath)
		
		self.obj_start.main()
		
		mb.showinfo('Cab. App Done Succufully', ""\
			+ "File create on current directory\n\n"		\
			+ "Input File: " + self.obj_start.str_file_cab_input + "\n" 	\
			+ "File create on current directory: " + self.obj_start.str_file_cab_output + "\n\n"	\
			+ "Configration: " + str(self.obj_start.obj_data.IRFTYP) + "\n"				\
			+ "Mode        : " + str(self.obj_start.obj_data.NMOD) )
			
		print ("==================================================")
		print ("|            Cab. App Done Succufully            |")
		print ("|     was create on current directory            |")
		print ("|............................................... |")
		print ("|                                                |")
		print ("       Input File: " + Start.FILE_CAB_INPUT )
		print ("      Output File: " + Start.FILE_CAB_OUTPUT )
		print ("|                                                |")
		print ("            Configration: " , self.obj_start.obj_data.IRFTYP)
		print ("            Mode        : " , self.obj_start.obj_data.NMOD)
		print ("==================================================")

	#-----------------------------------------------------------
	# Job 			: Check if input data file is found in the given folder
	# Input 		:
	#
	# Output		: True if found else False
	#-----------------------------------------------------------
	def isFileExisits (self, strFileName, strPath =""):
		if strPath =="":
			strPath = sys.path[0] 

		if os.path.isfile( strPath + "\\" + strFileName ):
			return True
		else:
			return False


#=======================================================
def main(): 
	CabApp().mainloop()
if __name__ == '__main__':
	main()

