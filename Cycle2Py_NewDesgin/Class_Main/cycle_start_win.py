# Python Import ==================
import os


from tkinter import *
import tkinter as tk
from tkinter import messagebox as mb

from functools import partial

# User Import ======================
from cycle_classes.Start import Start
# ------------------


class CycleApp(Frame):
    DATA_FOLDER = "data_cycle"
    FILE_INP_CYC = "Cycle_dat.csv"
    FILE_OUT_CYC = "Cycle_out.csv"
    lst_Cycle_Type = ["Standard", "Lorenz", "Dual Loop", "Dual Evap"]

    # -----------------------------------------------------------
    # Job 			: Runc Start class as selected configration, as button pressed
    # Input 		:
    # Output		:
    # -----------------------------------------------------------
    def __init__(self):
        self.obj_start = Start()
        self.str_file_cyc_in = None
        self.str_file_cyc_out = None

        tk.Frame.__init__(self)
        self.pack()
        self.master.title("Cycle Application")

        self.button1 = Button(
            self,
            text="Type 1-" +
            CycleApp.lst_Cycle_Type[0],
            width=25,
            command=partial(
                self.set_config,
                1))

        self.button2 = Button(
            self,
            text="Type 2-" +
            CycleApp.lst_Cycle_Type[1],
            width=25,
            command=partial(
                self.set_config,
                2))

        self.button3 = Button(
            self,
            text="Type 3-" +
            CycleApp.lst_Cycle_Type[2],
            width=25,
            command=partial(
                self.set_config,
                3))

        self.button4 = Button(
            self,
            text="Type 4-" +
            CycleApp.lst_Cycle_Type[3],
            width=25,
            command=partial(
                self.set_config,
                0))

        self.buttonNew = Button(self, text="User Current", width=25,
                                command=partial(self.set_config))

        self.button1.grid(row=0, column=1)
        self.button2.grid(row=1, column=1)
        self.button3.grid(row=2, column=1)
        self.button4.grid(row=3, column=1)

        self.buttonNew.grid(row=3, column=2)

    # -----------------------------------------------------------
    # Job 			: run the main object class Start, using input file name
    # Input 		: Configration number
    #              Creates input file name "n-Cabinet_dat.csv" and output "n-Cabinet_out.csv"
    #               n is the configration number
    # Output		:
    # -----------------------------------------------------------
    def set_config(self, int_config=0):
        strPath = sys.path[0] + "\\" + CycleApp.DATA_FOLDER

        if int_config != 0:
            self.str_file_cyc_in = str(
                int_config) + "-" + CycleApp.FILE_INP_CYC
            self.str_file_cyc_out = str(
                int_config) + "-" + CycleApp.FILE_OUT_CYC

        else:
            self.str_file_cyc_in = CycleApp.FILE_INP_CYC
            self.str_file_cyc_out = CycleApp.FILE_OUT_CYC

        if not self.isFileExisits(self.str_file_cyc_in, strPath):
            mb.showerror(
                "Error",
                "Cannot find file:" +
                strPath +
                "\\" +
                self.str_file_cyc_in)
            return

        self.obj_start.set_filenames(
            self.str_file_cyc_in,
            self.str_file_cyc_out,
            strPath,
            strPath)

        self.obj_start.main()

        mb.showinfo('Cycle. App Done Succufully', "" +
                    "File create on current directory\n\n" +
                    "Input File: " +
                    strPath +
                    "\\" +
                    self.obj_start.FILE_CYC_INPUT +
                    "\n" +
                    "File create on  " +
                    strPath +
                    "\\" +
                    self.obj_start.FILE_CYC_OUTPUT +
                    "\n\n" +
                    "Configration: " +
                    str(self.obj_start.dt.IRFTYP) +
                    "\n" +
                    "Cycle        : " +
                    str(self.obj_start.dt.ICYCL) +
                    CycleApp.lst_Cycle_Type[self.obj_start.dt.ICYCL -
                                            1])

        print("==================================================")
        print("|            Cycle. App Done Succufully          |")
        print("|     was create on current directory            |")
        print("|............................................... |")
        print("|                                                |")
        print("       Input File: " + strPath + "\\" + Start.FILE_CYC_INPUT)
        print("      Output File: " + strPath + "\\" + Start.FILE_CYC_OUTPUT)
        print("|                                                |")
        print("            Configration: " +
              str(self.obj_start.dt.IRFTYP))
        print("            Cycle       : " + str(self.obj_start.dt.ICYCL)
              + CycleApp.lst_Cycle_Type[self.obj_start.dt.ICYCL - 1])
        print("==================================================")

    # -----------------------------------------------------------
    # Job 			: Check if input data file is found in the given folder
    # Input 		:
    #
    # Output		: True if found else False
    # -----------------------------------------------------------
    def isFileExisits(self, strFileName, strPath=""):
        if strPath == "":
            strPath = sys.path[0]

        if os.path.isfile(strPath + "\\" + strFileName):
            return True
        else:
            return False


# =======================================================
def main():
    CycleApp().mainloop()


if __name__ == '__main__':
    main()
