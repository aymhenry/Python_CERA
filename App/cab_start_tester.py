# Python Import ==================

import tkinter as tk
from tkinter import *

from functools import partial
from tkinter import messagebox as mb

# User Import ======================
from cab_classes.Start import Start
# ------------------


class CabApp(Frame):
    strCurrent_path = sys.path[0] 

    FILE_CAB_INPUT = "cab_dat.csv"  # input file for cabinit module
    FILE_OUT_CAB = "cab_out.csv"  # output file for cabinit module
    FILE_CAB2CYC = "cab2cyc_out.csv"  # output file for cabinit module

    FLDR_CAB_IN = strCurrent_path + "\\" + "data\\data_cab"
    FLDR_CAB_OUT = strCurrent_path + "\\" + "data\\data_cab"

    # -----------------------------------------------------------
    # Job             : Runc Start class as selected configration, as button pressed
    # Input         :
    # Output        : 
    # -----------------------------------------------------------

    def __init__(self):
        self.str_file_cab_in = None
        self.str_file_cab_out = None
        self.str_file_cab2cyc = None
        
        tk.Frame.__init__(self)
        self.pack()
        self.master.title("Cab Application")

        self.button1 = Button(self, text="Config1", width=25, command=partial(self.set_config, 1))
        self.button2 = Button(self, text="Config2", width=25, command=partial(self.set_config, 2))
        self.button3 = Button(self, text="Config3", width=25, command=partial(self.set_config, 3))
        self.button4 = Button(self, text="Config4", width=25, command=partial(self.set_config, 4))
        self.button5 = Button(self, text="Config5", width=25, command=partial(self.set_config, 5))
        self.button6 = Button(self, text="Config6", width=25, command=partial(self.set_config, 6))
        self.button7 = Button(self, text="Config7", width=25, command=partial(self.set_config, 7))

        self.buttonNew = Button(self, text="User Current", width=25, command=partial(self.set_config))
        
        self.button1.grid(row=0, column=1)
        self.button2.grid(row=0, column=2)
        
        self.button3.grid(row=1, column=1)
        self.button4.grid(row=1, column=2)
        
        self.button5.grid(row=2, column=1)
        self.button6.grid(row=2, column=2)
        
        self.button7.grid(row=3, column=1)
        self.buttonNew.grid(row=3, column=2)

    # -----------------------------------------------------------
    # Job             : run the main object class Start, using input file name
    # Input         : Configration number
    #                 Creates input file name "n-Cabinet_dat.csv" and output "n-Cabinet_out.csv"
    #                  n is the configration number
    # Output        : 
    # -----------------------------------------------------------
    def set_config(self, int_config=0):
        strfldr_cab_in = CabApp.FLDR_CAB_IN
        strfldr_cab_out = CabApp.FLDR_CAB_OUT
        
        if int_config != 0:
            self.str_file_cab_in = str(int_config) + "-" + CabApp.FILE_CAB_INPUT
            self.str_file_cab_out = str(int_config) + "-" + CabApp.FILE_OUT_CAB
            self.str_file_cab2cyc = str(int_config) + "-" + CabApp.FILE_CAB2CYC
        else:
            self.str_file_cab_in = CabApp.FILE_CAB_INPUT
            self.str_file_cab_out = CabApp.FILE_OUT_CAB
            self.str_file_cab2cyc = CabApp.FILE_CAB2CYC
        
        obj_start = Start(self.str_file_cab_in,
                          self.str_file_cab_out,
                          self.str_file_cab2cyc,
                          strfldr_cab_in,
                          strfldr_cab_out)
                          
        is_solution = obj_start.main(True)
        
        if is_solution:
            mb.showinfo('Cab. App Done Succufully', ''
                        + "File create on current directory\n\n"
                        + "Input File: " + self.str_file_cab_in + "\n"
                        + "File create on current directory: " + self.str_file_cab_out + "\n\n"
                        + "Configration: " + str(obj_start.obj_data.IRFTYP) + "\n"
                        + "Mode        : " + str(obj_start.obj_data.NMOD)
                        )

        obj_start.print_scr_rep(is_solution)


if __name__ == '__main__':
    CabApp().mainloop()
