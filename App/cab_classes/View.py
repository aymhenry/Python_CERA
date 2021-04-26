# Python Import ==================
from abc import ABC, abstractmethod
import datetime
import sys

# User Import ======================
from common_classes.FileAccess import FileAccess
from common_classes.Unit import Unit

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job            : Abstract class for all View class
#
# Editor       : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class View(ABC):
    def __init__(self, obj_data, str_file_cab_out, str_file_cycle_out, str_path_cab, str_path_cycle):
        self.objCabout = FileAccess(str_file_cab_out, "write", str_path_cab)
        self.objCycout = FileAccess(str_file_cycle_out, "write", str_path_cycle)
        self.obj_data = obj_data

    # Abstract methods
    # ----------------------------------------------------------
    # Job            : show final output result(individual for every sub-class)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    @abstractmethod
    def show_final_results(self):
        pass
    
    # ----------------------------------------------------------
    # Job            : show input data to final output report(individual for every sub-class)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    @abstractmethod
    def show_rep(self):
        pass

    # ----------------------------------------------------------
    # Job            : show report repoprt headinh
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def rep_heading(self):
        #        PRINT THE DATE AND TIME
        now = datetime.datetime.now()
        self.objCycout.write_or_terminate(("RUN AT " 
                                           + now.strftime("%H %M %S %d %b %Y"))
                                          + " - Python Output aymhenry@gmail")
        # print user title
        self.objCycout.write_or_terminate(self.obj_data.title0)
        self.objCycout.write_or_terminate(self.obj_data.title1)
        self.objCycout.write_or_terminate(self.obj_data.title2)
        self.objCycout.write_or_terminate(self.obj_data.title3)
        
        # removed to as cyc_dat file (4 lines string data)
        # self.objCycout.write_or_terminate(self.obj_data.title4)
        # self.objCycout.write_or_terminate(self.obj_data.title5)
    # ----------------------------------------------------------
    # Job            : show report title(common for all)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def rep_title(self):
        #        PRINT THE DATE AND TIME
        now = datetime.datetime.now()
        self.objCabout.write_or_terminate(("RUN AT " 
                                           + now.strftime("%H %M %S %d %b %Y"))
                                          + " - Python Output aymhenry@gmail")
        self.objCabout.write_or_terminate(("Configration, %2.0f " % self.obj_data.IRFTYP))
        self.objCabout.write_or_terminate(("Mode, %2.0f " % self.obj_data.NMOD))
        
        # print user title
        self.objCabout.write_or_terminate(self.obj_data.title0)
        self.objCabout.write_or_terminate(self.obj_data.title1)
        self.objCabout.write_or_terminate(self.obj_data.title2)
        self.objCabout.write_or_terminate(self.obj_data.title3)
        self.objCabout.write_or_terminate(self.obj_data.title4)
        self.objCabout.write_or_terminate(self.obj_data.title5)
        
        # other user info, will be catched later

        # output the type of refrigerator
        if self.obj_data.IRFTYP == 1.0: 
            self.objCabout.write_or_terminate('Thermal analysis of a two-door top-mount')
            self.objCabout.write_or_terminate('refrigerator/freezer')

        elif self.obj_data.IRFTYP == 2.0:
            self.objCabout.write_or_terminate('Thermal Analysis of a Two-Door')
            self.objCabout.write_or_terminate('Bottom-mount Refrigerator/Freezer')

        elif self.obj_data.IRFTYP == 3.0:
            self.objCabout.write_or_terminate('Thermal Analysis of a Side-By-Side')
            self.objCabout.write_or_terminate('Refrigerator/Freezer')

        elif self.obj_data.IRFTYP == 4.0:
            self.objCabout.write_or_terminate('Thermal Analysis of a Chest Freezer')

        elif self.obj_data.IRFTYP == 5.0:
            self.objCabout.write_or_terminate('Thermal Analysis of an upright Freezer')

        elif self.obj_data.IRFTYP == 6.0:
            self.objCabout.write_or_terminate('Thermal Analysis of a one-door Refrigerator')

        elif self.obj_data.IRFTYP == 7.0:
            self.objCabout.write_or_terminate('Thermal Analysis of a one-door Refrigerator/Freezer')
        
        # dimensional specification
        self.objCabout.write_or_terminate(' ')
        self.objCabout.write_or_terminate(' , CONFIGURATION TYPE:,' + str(self.obj_data.IRFTYP))

        self.objCabout.write_or_terminate(' ')
        self.objCabout.write_or_terminate('Dimensional specifications')

        self.objCabout.write_or_terminate(" ,High, % 7.3f,CM" % (Unit.feet_cm(self.obj_data.HEIGHT)))
        self.objCabout.write_or_terminate(" ,Wide, % 7.3f,CM" % (Unit.feet_cm(self.obj_data.WIDTH)))
        self.objCabout.write_or_terminate(" ,Deep, % 7.3f,CM" % (Unit.feet_cm(self.obj_data.DEPTH)))

        self.objCabout.write_or_terminate(" ,Outer Liner, % 7.3f,MM" % (Unit.feet_mm(self.obj_data.DOL)))
        self.objCabout.write_or_terminate(" ,Inner Liner, % 7.3f,MM" % (Unit.feet_mm(self.obj_data.DIL)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 2,3,8
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec01_Mode238(self):   # configration 3(1 or 7) 2
        self.objCabout.write_or_terminate("Freezer:")   # Fortran 890
        self.objCabout.write_or_terminate(" ,Wedge deep,   % 7.3f ,CM" % (Unit.feet_cm(self.obj_data.WEDGE)))
        self.objCabout.write_or_terminate(" ,Flanges wide, % 7.3f ,CM" % (Unit.feet_cm(self.obj_data.FLANGE)))

        self.objCabout.write_or_terminate("Fresh food compartment:")   # Fortran 891
        self.objCabout.write_or_terminate(" ,Wedge deep, % 7.3f,CM" % (Unit.feet_cm(self.obj_data.WEDGER)))
        self.objCabout.write_or_terminate(" ,Flanges wide, % 7.3f,CM" % (Unit.feet_cm(self.obj_data.FLANGER)))

        if self.obj_data.WEDGE == 0.0:
            print('Freezer Wedge Should not be Zero ')
            print('Thickness.  Run Aborted')
            sys.exit('36')
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 4,7
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec02_Mode47(self):
        self.objCabout.write_or_terminate("  ,Wedge deep,  % 7.3f,CM" % (Unit.feet_cm(self.obj_data.WEDGE)))
        self.objCabout.write_or_terminate("  ,Flanges wide,% 7.3f,CM" % (Unit.feet_cm(self.obj_data.FLANGE)))

    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for mode 7
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec02_Mode7(self):
        if self.obj_data.WEDGE == 0.0:   # and(self.obj_data.NMOD == 7.0):
            print('Freezer Wedge Should not be Zero ')
            print('Thickness.  Run Aborted')
            sys.exit('36')    
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 2
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec03_Mode2(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Mullion")
        self.objCabout.write_or_terminate(" ,Side of mullion from the outside wall, % 7.3f, "
                                          + "CM,from the outside wall of the fresh food compartment"
                                          % (Unit.feet_cm(self.obj_data.WALL)))
        self.objCabout.write_or_terminate(" ,Mullion thickness,% 7.3f, CM" % (Unit.feet_cm(self.obj_data.THMUL)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("COMPRESSOR COMPARTMENT DIMENSIONS")
        self.objCabout.write_or_terminate(" ,Top Depth,    % 7.3f, CM" % (Unit.feet_cm(self.obj_data.CDUP)))

        self.objCabout.write_or_terminate(" ,Bottom Depth, % 7.3f, CM" % (Unit.feet_cm(self.obj_data.CDDN)))
        self.objCabout.write_or_terminate(" ,Height Depth, % 7.3f, CM" % (Unit.feet_cm(self.obj_data.CCHGT)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("INSULATION THICKNESS")
        self.objCabout.write_or_terminate(" ,Top,% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFT)))
        self.objCabout.write_or_terminate(" ,Right side,  % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFRS)))
        self.objCabout.write_or_terminate(" ,Front(door),% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFF)))
        self.objCabout.write_or_terminate(" ,Back,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFB)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("FRESH FOOD COMPARTMENT")
        self.objCabout.write_or_terminate(" ,Top,% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRT)))
        self.objCabout.write_or_terminate(" ,Left side,   % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRLS)))
        self.objCabout.write_or_terminate(" ,Front(door),% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRF)))
        self.objCabout.write_or_terminate(" ,Back,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRB)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Bottom")
        self.objCabout.write_or_terminate(" ,Fresh food, % 7.3f, CM" % (Unit.feet_cm(self.obj_data.BINSUL)))
        self.objCabout.write_or_terminate(" ,Freezer,    % 7.3f, CM" % (Unit.feet_cm(self.obj_data.BINFRZ)))    
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for mode 3
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec04_Mode3(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Mullion")
        self.objCabout.write_or_terminate(" ,Top of the mullion, % 7.3f, CM, from the top of the cabinet"
                                          % (Unit.feet_cm(self.obj_data.TOPMUL)))
        self.objCabout.write_or_terminate(" ,Mullion thickness,% 7.3f, CM" % (Unit.feet_cm(self.obj_data.THMUL)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("COMPRESSOR COMPARTMENT DIMENSIONS")
        self.objCabout.write_or_terminate(" ,Top Depth, % 7.3f, CM" % (Unit.inch_cm(self.obj_data.CDUP)))
        self.objCabout.write_or_terminate(" ,Bottom Depth, % 7.3f, CM" % (Unit.inch_cm(self.obj_data.CDDN)))
        self.objCabout.write_or_terminate(" ,Height Depth, % 7.3f, CM" % (Unit.inch_cm(self.obj_data.CCHGT)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("INSULATION THICKNESS")
        self.objCabout.write_or_terminate("Freezer")
        
        self.objCabout.write_or_terminate(" ,Top,% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFT)))
        self.objCabout.write_or_terminate(" ,Left side,   % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFLS)))
        self.objCabout.write_or_terminate(" ,Right side,  % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFRS)))
        self.objCabout.write_or_terminate(" ,Front(door),% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFF)))
        self.objCabout.write_or_terminate(" ,Back,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFB)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("INSULATION THICKNESS")
        self.objCabout.write_or_terminate("Fresh food")
        self.objCabout.write_or_terminate(" ,Left side,   % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRLS)))
        self.objCabout.write_or_terminate(" ,Right side,  % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRRS)))
        self.objCabout.write_or_terminate(" ,Front(door),% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRF)))
        self.objCabout.write_or_terminate(" ,Back,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRB)))
        self.objCabout.write_or_terminate(" ,Bottom,      % 7.3f, CM" % (Unit.feet_cm(self.obj_data.BINSUL)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for mode 5
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec05_Mode5(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("COMPRESSOR COMPARTMENT")
        self.objCabout.write_or_terminate(" ,Height, % 7.3f, CM" % (Unit.feet_cm(self.obj_data.CWIDE)))
        self.objCabout.write_or_terminate(" ,Width,  % 7.3f, CM" % (Unit.feet_cm(self.obj_data.CHGT)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("INSULATION THICKNESS(CM) ")
        self.objCabout.write_or_terminate("Freezer")
        self.objCabout.write_or_terminate(" ,Top,  % 7.3f,CM" % (Unit.feet_cm(self.obj_data.TIFT)))
        self.objCabout.write_or_terminate(" ,Side, % 7.3f,CM" % (Unit.feet_cm(self.obj_data.TIFRS)))
        self.objCabout.write_or_terminate(" ,Front(door),% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFF)))
        self.objCabout.write_or_terminate(" ,Back, % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFB)))

        self.obj_data.TIFLS = self.obj_data.TIFRS
        self.objCabout.write_or_terminate(" ,Bottom, % 7.3f, CM" % (Unit.feet_cm(self.obj_data.BINSUL)))

        self.objCabout.write_or_terminate(" ,Compartment side, % 7.3f,CM" % (Unit.feet_cm(self.obj_data.SCIN)))
        self.objCabout.write_or_terminate(" ,Compartment top,  % 7.3f,CM" % (Unit.feet_cm(self.obj_data.STIN)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 4,7
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec06_Mode47(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("COMPRESSOR COMPARTMENT DIMENSIONS")
        self.objCabout.write_or_terminate(" ,Top Depth, % 7.3f, CM" % (Unit.inch_cm(self.obj_data.CDUP)))
        self.objCabout.write_or_terminate(" ,Bottom Depth, % 7.3f, CM" % (Unit.inch_cm(self.obj_data.CDDN)))
        self.objCabout.write_or_terminate(" ,Height Depth, % 7.3f, CM" % (Unit.inch_cm(self.obj_data.CCHGT)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("INSULATION THICKNESS")
        self.objCabout.write_or_terminate(" ,Top,% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFT)))
        self.objCabout.write_or_terminate(" ,Left side,   % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFLS)))
        self.objCabout.write_or_terminate(" ,Right side,  % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFRS)))
        self.objCabout.write_or_terminate(" ,Front(door),% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFF)))
        self.objCabout.write_or_terminate(" ,Back,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFB)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for mode 8
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec07_Mode8(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Mullion")
        self.objCabout.write_or_terminate(" ,Top of the mullion, % 7.3f,CM,from the top of the cabinet"
                                          % (Unit.feet_cm(self.obj_data.TOPMUL)))
        self.objCabout.write_or_terminate(" ,Mullion thickness,% 7.3f, CM"
                                          % (Unit.feet_cm(self.obj_data.THMUL)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("COMPRESSOR COMPARTMENT DIMENSIONS:")
        self.objCabout.write_or_terminate(" ,Top Depth, % 7.3f, CM" % (Unit.feet_cm(self.obj_data.CDUP)))
        self.objCabout.write_or_terminate(" ,Bottom Depth, % 7.3f, CM" % (Unit.feet_cm(self.obj_data.CDDN)))
        self.objCabout.write_or_terminate(" ,Height Depth, % 7.3f, CM" % (Unit.feet_cm(self.obj_data.CCHGT)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("INSULATION THICKNESS")
        self.objCabout.write_or_terminate("Freezer")
        self.objCabout.write_or_terminate(" ,Side,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFRS)))
        self.objCabout.write_or_terminate(" ,Front(door),% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFF)))
        self.objCabout.write_or_terminate(" ,Back,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIFB)))
        self.objCabout.write_or_terminate(" ,Bottom,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.BINSUL)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Fresh food compartment ")
        self.objCabout.write_or_terminate(" ,TOP,         % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRT)))
        self.objCabout.write_or_terminate(" ,Side,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRLS)))
        self.objCabout.write_or_terminate(" ,Front(door),% 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRF)))
        self.objCabout.write_or_terminate(" ,Back,        % 7.3f, CM" % (Unit.feet_cm(self.obj_data.TIRB)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for all modes
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec08_Mode_all(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("VOLUMES")
        self.objCabout.write_or_terminate("Freezer Refrigerated Volume")    
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for mode 4
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec09_Mode4(self):
        self.objCabout.write_or_terminate(" ,Specified, % 7.3f, Liter" % (Unit.ft3_liter(self.obj_data.VOLAZ)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 2,3,5,7,8
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec10_Mode23578(self):
        self.objCabout.write_or_terminate("  ,Calculated,% 7.3f, Liter" % self.obj_data.VFZ)
        self.objCabout.write_or_terminate("Specified ")
        self.objCabout.write_or_terminate(" ,Shelf/Evap ,% 7.3f, Liter" % (Unit.ft3_liter(self.obj_data.HXVUZ)))
        self.objCabout.write_or_terminate(" ,Net volume ,% 7.3f, Liter" % (Unit.ft3_liter(self.obj_data.VOLAZ)))

    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 2,3,8
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec11_Mode238(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate(" GENERAL REFRIGERATED VOLUME")
        self.objCabout.write_or_terminate("  ,Calculated, % 7.3f, Liter" % self.obj_data.VFF)
        self.objCabout.write_or_terminate("  Specified ")
        self.objCabout.write_or_terminate(" ,Shelf/Evap ,% 7.3f, Liter" % (Unit.ft3_liter(self.obj_data.HXVUR)))
        self.objCabout.write_or_terminate(" ,Net volume ,% 7.3f, Liter" % (Unit.ft3_liter(self.obj_data.VOLAR)))

    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 2,3,8
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec12_Mode238(self):
        #          output temperatures and thermal characteristics
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("TEMPERATURES")
        self.objCabout.write_or_terminate("  ,Room,                   % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TROOM)))
        self.objCabout.write_or_terminate("  ,Freezer cabinet,        % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TFRZ)))
        self.objCabout.write_or_terminate("  ,Fresh food cabinet,     % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TFF)))
        self.objCabout.write_or_terminate("  ,Air under refrigerator, % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TBTM)))
            
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("THERMAL CHARACTERISTICS")
        self.objCabout.write_or_terminate("Thermal Resistivity")
        self.objCabout.write_or_terminate("  ,Fresh food cabinet,  % 10.4f, m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.RKINFF)))
        self.objCabout.write_or_terminate("  ,Freezer cabinet,     % 10.4f, m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.RKINFZ)))
        self.objCabout.write_or_terminate("  ,Fresh food wedge,    % 10.4f, m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.WKINR)))

        self.objCabout.write_or_terminate("  ,Freezer wedge,       % 10.4f, m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.WKIN)))
        self.objCabout.write_or_terminate("  ,Fresh food door,     % 10.4f, m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.DKINFF)))

        self.objCabout.write_or_terminate("  ,Freezer door,% 10.4f, m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.DKINFZ)))
        self.objCabout.write_or_terminate("  ,Mullion,     % 10.4f, m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.CKMUL)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for mode 3
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec13_Mode3(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Gasket Heat Leak")
        self.objCabout.write_or_terminate("  ,Freezer,% 10.4f, W/m-C"
                                          % (Unit.BtuThHInchF_WattM(self.obj_data.HLFZG)))
        self.objCabout.write_or_terminate("  ,Refrigerator,% 10.4f, W/m-C"
                                          % (Unit.BtuThHInchF_WattM(self.obj_data.HLRG)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 2,8
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec14_Mode28(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Gasket Heat Leak")
        self.objCabout.write_or_terminate("  ,Freezer, % 10.4f, W/m-C"
                                          % (Unit.BtuThHInchF_WattM(self.obj_data.HLGZF)))
        self.objCabout.write_or_terminate("  ,Refrigerator,% 10.4f, W/m-C"
                                          % (Unit.BtuThHInchF_WattM(self.obj_data.HLRG)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for mode 4
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec15_Mode4(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Temperatures")
        self.objCabout.write_or_terminate("  ,Room,                   % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TROOM)))
        self.objCabout.write_or_terminate("  ,Freezer cabinet,        % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TFRZ)))
        self.objCabout.write_or_terminate("  ,Fresh food cabinet,     % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TFF)))
        self.objCabout.write_or_terminate("  ,Air under refrigerator, % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TBTM)))
            
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("THERMAL CHARACTERISTICS")
        self.objCabout.write_or_terminate("Thermal Resistivity")
        
        self.objCabout.write_or_terminate("  ,Cabinet,% 10.4f,m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.RKIN)))
        self.objCabout.write_or_terminate("  ,Wedge,  % 10.4f,m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.WKIN)))
        self.objCabout.write_or_terminate("  ,Door,   % 10.4f,m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.DKIN)))
        self.objCabout.write_or_terminate("  ,Door,   % 10.4f,m2-C/W-cm "
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.DKIN)))
        self.objCabout.write_or_terminate("  ,Gasket heat leak,   % 10.4f, W/m-C"
                                          % (Unit.BtuThHInchF_WattM(self.obj_data.HLFZG)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for mode 5
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec16_Mode5(self):
        self.objCabout.write_or_terminate("Temperatures")
        self.objCabout.write_or_terminate("  ,Room,                   % 7.3f, DEG C "
                                          % (Unit.f_c(self.obj_data.TROOM)))
        self.objCabout.write_or_terminate("  ,Freezer cabinet,        % 7.3f, DEG C "
                                          % (Unit.f_c(self.obj_data.TFRZ)))
        self.objCabout.write_or_terminate("  ,Air under refrigerator, % 7.3f, DEG C "
                                          % (Unit.f_c(self.obj_data.TBTM)))

        # RFREEZ = 1.0/(self.obj_data.RKIN * 12.0)
        # RTOP = 1.0/(self.obj_data.TKIN * 12.0)

        self.objCabout.write_or_terminate("THERMAL CHARACTERISTICS")
        self.objCabout.write_or_terminate("Thermal resistivity")
        self.objCabout.write_or_terminate("  ,Freezer cabinet,    % 10.4f, m2-C/W-cm"
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.RKIN)))
        self.objCabout.write_or_terminate("  ,Freezer top(door), % 10.4f, m2-C/W-cm"
                                          % (1.0/Unit.BtuHrFtF_CmWattM2K(self.obj_data.TKIN)))
        
        self.objCabout.write_or_terminate("  ,Gasket heat leak,% 10.4f, W/m-C"
                                          % (Unit.BtuThHInchF_WattM(self.obj_data.HLFZG)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for mode 7
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec17_Mode7(self):
        self.objCabout.write_or_terminate("Temperatures")
        self.objCabout.write_or_terminate("  ,Room,                   % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TROOM)))
        self.objCabout.write_or_terminate("  ,Freezer cabinet,        % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TFRZ)))
        self.objCabout.write_or_terminate("  ,Air under refrigerator, % 7.3f, DEG C " % (Unit.f_c(self.obj_data.TBTM)))

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("THERMAL CHARACTERISTICS")
        self.objCabout.write_or_terminate("Thermal resistivity")

        self.objCabout.write_or_terminate("  ,Cabinet,% 10.4f,m2-C/W-cm " 
                                          % (Unit.CmWattM2K_BtuInchHrF2F(self.obj_data.RCAB)))
        self.objCabout.write_or_terminate("  ,Wedge,  % 10.4f,m2-C/W-cm " 
                                          % (Unit.CmWattM2K_BtuInchHrF2F(self.obj_data.RWEDGE)))
        self.objCabout.write_or_terminate("  ,Door,   % 10.4f,m2-C/W-cm " 
                                          % (Unit.CmWattM2K_BtuInchHrF2F(self.obj_data.RDOOR)))

        self.objCabout.write_or_terminate("  ,Gasket heat leak,   % 10.4f, W/m-C " 
                                          % (Unit.BtuThHInchF_WattM(self.obj_data.HLFZG)))
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 2,3,8
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec18_Moode238(self):
        #           Door Opening Characterization
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Door Openings")
        self.objCabout.write_or_terminate("  ,Air temperature,  % 7.1f, Deg C " 
                                          % (Unit.f_c(self.obj_data.TDRAIR)))
        self.objCabout.write_or_terminate("  ,Relative humidity,% 7.1f,(%%) " % self.obj_data.RELHUM)
        
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Fresh Food Compartment")
        self.objCabout.write_or_terminate("  ,Openings/hr,          % 7.1f,#/HR " % self.obj_data.FFCOPN)
        self.objCabout.write_or_terminate("  ,Duration of 1 opening,% 7.1f,SECONDS " % self.obj_data.SECFFC)

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Freezer Compartmen")
        self.objCabout.write_or_terminate("  ,Openings/hr,          % 7.1f,#/HR " % self.obj_data.FRZOPN)
        self.objCabout.write_or_terminate("  ,Duration of 1 opening,% 7.1f,SECONDS " % self.obj_data.SECFRZ)

    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 4.5.7
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec19_Mode457(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Door Openings")
        self.objCabout.write_or_terminate("  ,Air temperature,  % 7.1f, Deg C " % (Unit.f_c(self.obj_data.TDRAIR)))
        self.objCabout.write_or_terminate("  ,Relative humidity,% 7.1f, (%%) " % self.obj_data.RELHUM)

        self.objCabout.write_or_terminate("  ,Openings/hr,          % 7.1f,#/HR " % self.obj_data.FRZOPN)
        self.objCabout.write_or_terminate("  ,Duration of 1 opening,% 7.1f,SECONDS " % self.obj_data.SECFRZ)
    
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 2,3,8
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec20_Mode238(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("ANTI-SWEAT Heaters")
        self.objCabout.write_or_terminate("  ,FRESH FOOD CABINE, % 7.3f, W " % self.obj_data.FFASHW)
        self.objCabout.write_or_terminate("  ,FREEZER CABINET,   % 7.3f, W " % self.obj_data.FZASHW)

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Auxiliary Energy")
        self.objCabout.write_or_terminate("  ,Fresh food cabine,  % 7.3f, W " % self.obj_data.FFAUXW)
        self.objCabout.write_or_terminate("  ,Freezer cabinet,    % 7.3f, W " % self.obj_data.FZAUXW)
        self.objCabout.write_or_terminate("  ,Outside cabinet,    % 7.3f, W " % self.obj_data.OTHERW)

    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 4,5,7
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec21_Mode457(self):
        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("ANTI-SWEAT Heaters")
        self.objCabout.write_or_terminate(" ,Cabinet,%7.3f, W " % self.obj_data.FZASHW)

        self.objCabout.write_or_terminate(" ")
        self.objCabout.write_or_terminate("Auxiliary Energy")
        self.objCabout.write_or_terminate("  ,Inside cabinet, % 7.3f, W " % self.obj_data.FZAUXW)
        self.objCabout.write_or_terminate("  ,Outside cabinet,% 7.3f, W " % self.obj_data.OTHERW)
        
    # ----------------------------------------------------------
    # Job            : show input data, this section is displayed for modes 5,7
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def final_Mode57(self):    
        self.objCabout.write_or_terminate(' ')
        self.objCabout.write_or_terminate('HEAT LEAK BREAKDOWN')   # 902
        self.objCabout.write_or_terminate(' ,Description,(W)')

        self.objCabout.write_or_terminate(' ,Right wall' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QRSIDE)))
        self.objCabout.write_or_terminate(' ,Left wall' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QLSIDE)))
        self.objCabout.write_or_terminate(' ,Back wall' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QBACK)))
        self.objCabout.write_or_terminate(' ,Front(door)' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QFRONT)))
        self.objCabout.write_or_terminate(' ,Top(door)' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QTOP)))
        self.objCabout.write_or_terminate(' ,Bottom' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QBOTTM)))

        self.objCabout.write_or_terminate(' ,Wedge' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QW)))
        self.objCabout.write_or_terminate(' ,Gasket' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QGZF)))

        self.objCabout.write_or_terminate(' ,Open door sensible load' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QSDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - condense' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QLDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - frost' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QFDRFZ)))

        self.objCabout.write_or_terminate(' ,Anti-sweat heater' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FZASHQ)))
        self.objCabout.write_or_terminate(' ,Refrigerant line heat' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FZREFQ)))
        self.objCabout.write_or_terminate(' ,Penetrations' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FZPENA)))
        self.objCabout.write_or_terminate(' ,Other thermal input' + ',%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.FZHEAT)))
        self.objCabout.write_or_terminate('Total ,' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFZTOT)))
    
    # ============ Output to Cycle file ==============
    #    PASS DATA VALUES TO THE CYCLE MODEL
    
    # ----------------------------------------------------------
    # Job            : show input data(cycle report), this section is displayed for modes 2,3,8
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec1_cycle_Mode238(self):
        self.rep_heading()
        
        self.objCycout.write_or_terminate("1,,,CABINET LOADS DATA")
        self.objCycout.write_or_terminate(
            " %2.0f, #, IRFTYP, Configration (1 to 5)" % self.obj_data.IRFTYP)
                
        self.objCycout.write_or_terminate(
            "%10.2f, watt, FFASHW,Fresh food antisweat heater" % self.obj_data.FFASHW)
        self.objCycout.write_or_terminate(
            "%10.2f,watt, FFAUXW, Fresh food auxiliary power" % self.obj_data.FFAUXW)

        self.objCycout.write_or_terminate(
            "%10.2f, watt, FZASHW, Freezer antisweat heater" % self.obj_data.FZASHW)
        self.objCycout.write_or_terminate(
            "%10.2f, watt, FZAUXW, Freezer auxiliary power" % self.obj_data.FZAUXW)

        self.objCycout.write_or_terminate(
            "%10.2f, watt, OTHERW, Outside cabinet power" % self.obj_data.OTHERW)
            
        self.objCycout.write_or_terminate(
            "%10.2f, DEG C, TROOM, Room temperature " % (Unit.f_c(self.obj_data.TROOM)))
        self.objCycout.write_or_terminate(
            "%10.2f, DEG C, TFF, Fresh food temperature " % (Unit.f_c(self.obj_data.TFF)))
        self.objCycout.write_or_terminate(
            "%10.2f, DEG C, TFRZ, Freezer temperature" % (Unit.f_c(self.obj_data.TFRZ)))
    
    # ----------------------------------------------------------
    # Job            : show input data(cycle report), this section is displayed for mode 4
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec2_cycle_Mode4(self):
        self.sec3_cycle_Mode57()
            
    # ----------------------------------------------------------
    # Job            : show input data(cycle report), this section is displayed for modes 5.7
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec3_cycle_Mode57(self):
        self.rep_heading()
        
        self.objCycout.write_or_terminate("1,,,CABINET LOADS DATA")
        self.objCycout.write_or_terminate(
            " %2.0f, #, IRFTYP, Configration (1 to 5)" % self.obj_data.IRFTYP)
                
        self.objCycout.write_or_terminate(
            "%10.2f, FFASHW, watt, Fresh food antisweat heater (same as Freezer antisweat power)" 
            % self.obj_data.FZASHW)
        self.objCycout.write_or_terminate(
            "%10.2f, FFASH, watt, Fresh food auxiliary power (same as Freezer Aux. power)" 
            % self.obj_data.FZAUXW)

        self.objCycout.write_or_terminate(
            "%10.2f, FZASH, watt, Freezer antisweat heater" % self.obj_data.FZASHW)
        self.objCycout.write_or_terminate(
            "%10.2f, FZAUX, watt, Freezer auxiliary power" % self.obj_data.FZAUXW)

        self.objCycout.write_or_terminate(
            "%10.2f, OTHERW, watt, Outside cabinet power" % self.obj_data.OTHERW)

        self.objCycout.write_or_terminate(
            "%10.2f, TROOM, DEG C, Room temperature " % (Unit.f_c(self.obj_data.TROOM)))
        self.objCycout.write_or_terminate(
            "%10.2f, FFTEMP, DEG C, Fresh food temperature " % (Unit.f_c(self.obj_data.TFF)))
        self.objCycout.write_or_terminate(
            "%10.2f, FZTEMP, DEG C, Freezer temperature" % (Unit.f_c(self.obj_data.TFRZ)))
    
    # ----------------------------------------------------------
    # Job         : show output data(cycle report), this section is displayed for modes 2,3,8
    #         this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec4_cycle_Mode238(self):
        self.objCycout.write_or_terminate('2,,,HEAT LEAK BREAKDOWN')
        self.objCycout.write_or_terminate('%10.2f, watt, FFQ, Fresh food net load'
                                          % (Unit.BtuH_Watt(self.obj_data.QFFTOT)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZQOFF, Freezer load'
                                          % (Unit.BtuH_Watt(self.obj_data.QFZTOT)))

        self.objCycout.write_or_terminate('%10.2f, watt, FFSEN, Fresh food door sensible load'
                                          % (Unit.BtuH_Watt(self.obj_data.QSDRFF)))
        self.objCycout.write_or_terminate('%10.2f, watt, FFLAT, Fresh food door condensation load'
                                          % (Unit.BtuH_Watt(self.obj_data.QLDRFF)))
        self.objCycout.write_or_terminate('%10.2f, watt, FROSTF, Fresh food door frost load'
                                          % (Unit.BtuH_Watt(self.obj_data.QFDRFF)))

        self.objCycout.write_or_terminate('%10.2f, watt, FZSEN, Freezer door sensible load'
                                          % (Unit.BtuH_Watt(self.obj_data.QSDRFZ)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZLAT, Freezer door condensation load'
                                          % (Unit.BtuH_Watt(self.obj_data.QLDRFZ)))
        self.objCycout.write_or_terminate('%10.2f, watt, FROSTZ, Freezer door frost load'
                                          % (Unit.BtuH_Watt(self.obj_data.QFDRFZ)))

        self.objCycout.write_or_terminate('%10.2f, watt, FFPENA, Fresh food penetrations'
                                          % (Unit.BtuH_Watt(self.obj_data.FFPENA)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZPENA, Freezer penetrations'
                                          % (Unit.BtuH_Watt(self.obj_data.FZPENA)))

        self.objCycout.write_or_terminate('%10.2f, watt, FFHTQ, Fresh food heaters and controls'
                                          % (Unit.BtuH_Watt(self.obj_data.QHTFF)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZHTQ, Freezer heaters and controls'
                                          % (Unit.BtuH_Watt(self.obj_data.QHTFZ)))

        self.objCycout.write_or_terminate('%10.2f, watt, FFREFQ, Fresh food refrigerant line'
                                          % (Unit.BtuH_Watt(self.obj_data.FFREFQ)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZREFQ, Freezer refrigerant line'
                                          % (Unit.BtuH_Watt(self.obj_data.FZREFQ)))

        self.objCycout.write_or_terminate('%10.2f, watt, QMUL, Mullion heat load'
                                          % (Unit.BtuH_Watt(-1*self.obj_data.QMULN)))
    
    # ----------------------------------------------------------
    # Job            : show output data(cycle report), this section is displayed for modes 4,5,7
    #                this method is called from show_rep method(from sub class methods)
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    def sec5_cycle_Mode457(self):
        self.objCycout.write_or_terminate('2,,,HEAT LEAK BREAKDOWN')
        # similar values 1,2-3,4,7-8,5,8-9,10-11,12,13-14
        self.objCycout.write_or_terminate('%10.2f, watt, FFQ, Fresh food net load'   
                                          % (Unit.BtuH_Watt(self.obj_data.QFZTOT)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZQOFF, Freezer load'
                                          % (Unit.BtuH_Watt(self.obj_data.QFZTOT)))

        self.objCycout.write_or_terminate('%10.2f, watt, FFSEN, Fresh food sensible load'
                                          % (Unit.BtuH_Watt(self.obj_data.QSDRFZ)))
        self.objCycout.write_or_terminate('%10.2f, watt, FFLAT, Fresh food condensation load'
                                          % (Unit.BtuH_Watt(self.obj_data.QLDRFZ)))                                       
        self.objCycout.write_or_terminate('%10.2f, watt, FROSTF, Fresh food frost load'
                                          % (Unit.BtuH_Watt(self.obj_data.QFDRFZ)))
                                          
        self.objCycout.write_or_terminate('%10.2f, watt, FZSEN, Freezer door sensible load'        
                                          % (Unit.BtuH_Watt(self.obj_data.QSDRFZ)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZLAT, Freezer door condensation load'   
                                          % (Unit.BtuH_Watt(self.obj_data.QLDRFZ)))
        self.objCycout.write_or_terminate('%10.2f, watt, FROSTZ, Freezer door frost load'         
                                          % (Unit.BtuH_Watt(self.obj_data.QFDRFZ)))

        self.objCycout.write_or_terminate('%10.2f, watt, FFPENA, Fresh food penetrations'           
                                          % (Unit.BtuH_Watt(self.obj_data.FZPENA)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZPENA, Freezer penetrations'
                                          % (Unit.BtuH_Watt(self.obj_data.FZPENA)))

        self.objCycout.write_or_terminate('%10.2f, watt, FFHTQ, Fresh food heaters and controls'   
                                          % (Unit.BtuH_Watt(self.obj_data.QHTFZ)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZHTQ, Freezer heaters and controls'
                                          % (Unit.BtuH_Watt(self.obj_data.QHTFZ)))
                                          
        self.objCycout.write_or_terminate('%10.2f, watt, FFREFQ, Fresh food refrigerant line'
                                          % (Unit.BtuH_Watt(self.obj_data.FZREFQ)))
        self.objCycout.write_or_terminate('%10.2f, watt, FZREFQ, Freezer refrigerant line'     
                                          % (Unit.BtuH_Watt(self.obj_data.FZREFQ)))

        self.objCycout.write_or_terminate('%10.2f, watt, QMUL, Mullion heat load'
                                          % 0.0)
    # ----------------------------------------------------------
    # Job            : close data output files
    # Input        :
    #
    # Output       :
    # ----------------------------------------------------------
    
    def __del__(self):
        self.objCabout = None
        self.objCycout = None

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job            : Volume
#
# Editor       : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class View_Ql2(View):
    def show_final_results(self):
        self.objCabout.write_or_terminate(' ')
        self.objCabout.write_or_terminate('HEAT LEAK BREAKDOWN')
        self.objCabout.write_or_terminate(' , ,FRESH FOOD,FREEZER')
        self.objCabout.write_or_terminate(' ,Description,(W),(W)')

        self.objCabout.write_or_terminate(' ,Right wall' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(0.0), Unit.BtuH_Watt(self.obj_data.QRSIDE)))
        self.objCabout.write_or_terminate(' ,Left wall' + ',%10.2f,%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QLSIDE)
                                             , Unit.BtuH_Watt(0.0)))
        self.objCabout.write_or_terminate(' ,Back wall' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QBACKL)
                                             , Unit.BtuH_Watt(self.obj_data.QBACKR)))
        self.objCabout.write_or_terminate(' ,Front(door)' + ',%10.2f,%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QFRNTL)
                                             , Unit.BtuH_Watt(self.obj_data.QFRNTR)))

        self.objCabout.write_or_terminate(' ,Top wall' + ',%10.2f,%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QLTOP)
                                             , Unit.BtuH_Watt(self.obj_data.QRTOP)))
        self.objCabout.write_or_terminate(' ,Bottom' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QLBTTM)
                                             , Unit.BtuH_Watt(self.obj_data.QRBTTM)))
        self.objCabout.write_or_terminate(' ,Mullion' + ',%10.2f,%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QMULN)
                                             , Unit.BtuH_Watt(self.obj_data.QMUL)))
        self.objCabout.write_or_terminate(' ,Wedge' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QWFF)
                                             , Unit.BtuH_Watt(self.obj_data.QWFZ)))
        self.objCabout.write_or_terminate(' ,Gasket' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QGR)
                                             , Unit.BtuH_Watt(self.obj_data.QGZF)))
        
        self.objCabout.write_or_terminate(' ,Open door sensible load' + ',%10.2f,%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.QSDRFF)
                                             , Unit.BtuH_Watt(self.obj_data.QSDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - condense' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QLDRFF)
                                             , Unit.BtuH_Watt(self.obj_data.QLDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - frost' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFDRFF)
                                             , Unit.BtuH_Watt(self.obj_data.QFDRFZ)))

        self.objCabout.write_or_terminate(' ,Anti-sweat heater' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFASHQ)
                                             , Unit.BtuH_Watt(self.obj_data.FZASHQ)))
        self.objCabout.write_or_terminate(' ,Refrigerant line heat' + ',%10.2f,%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.FFREFQ)
                                             , Unit.BtuH_Watt(self.obj_data.FZREFQ)))
        self.objCabout.write_or_terminate(' ,Penetrations' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFPENA)
                                             , Unit.BtuH_Watt(self.obj_data.FZPENA)))
        self.objCabout.write_or_terminate(' ,Other thermal input' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFHEAT)
                                             , Unit.BtuH_Watt(self.obj_data.FZHEAT)))

        self.objCabout.write_or_terminate('Total ,' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFFTOT)
                                             , Unit.BtuH_Watt(self.obj_data.QFZTOT)))

    # -----------------------------------
    def show_rep(self):
        self.rep_title()
        self.sec01_Mode238()
        self.sec03_Mode2()
        self.sec08_Mode_all()
        self.sec10_Mode23578()
        self.sec11_Mode238()
        self.sec12_Mode238()
        self.sec14_Mode28()
        self.sec18_Moode238()
        self.sec20_Mode238()
        self.show_final_results()
        
        self.sec1_cycle_Mode238()
        self.sec4_cycle_Mode238()
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job            : View for Configration3(Mode 2)
#
# Editor       : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class View_Ql13(View):
    def show_final_results(self):
        self.objCabout.write_or_terminate(' ')
        self.objCabout.write_or_terminate('HEAT LEAK BREAKDOWN')
        self.objCabout.write_or_terminate(' , ,FRESH FOOD,FREEZER')
        self.objCabout.write_or_terminate(' ,Description,(W),(W)')

        self.objCabout.write_or_terminate(' ,Right wall' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QRRSID)
                                             , Unit.BtuH_Watt(self.obj_data.QFRSID)))
        self.objCabout.write_or_terminate(' ,Left wall' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QRLSID)
                                             ,  Unit.BtuH_Watt(self.obj_data.QFLSID)))
        self.objCabout.write_or_terminate(' ,Back wall' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QRBACK)
                                             ,  Unit.BtuH_Watt(self.obj_data.QFBACK)))
        self.objCabout.write_or_terminate(' ,Front(door)' + ',%10.2f,%10.2f' 
                                          % (Unit.BtuH_Watt(self.obj_data.QRFRNT)
                                             ,  Unit.BtuH_Watt(self.obj_data.QFFRNT)))

        self.objCabout.write_or_terminate(' ,Top wall' + ',%10.2f,%10.2f' 
                                          % (Unit.BtuH_Watt(0.0)
                                             , Unit.BtuH_Watt(self.obj_data.QTOP)))
        self.objCabout.write_or_terminate(' ,Bottom' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QBOTTM)
                                             , Unit.BtuH_Watt(0.0)))
        self.objCabout.write_or_terminate(' ,Mullion' + ',%10.2f,%10.2f' 
                                          % (Unit.BtuH_Watt(self.obj_data.QMULN)
                                             , Unit.BtuH_Watt(self.obj_data.QMULI)))
        self.objCabout.write_or_terminate(' ,Wedge' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QWFF)
                                             , Unit.BtuH_Watt(self.obj_data.QWFZ)))

        self.objCabout.write_or_terminate(' ,Gasket' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QGR)
                                             , Unit.BtuH_Watt(self.obj_data.QGZF)))
        self.objCabout.write_or_terminate(' ,Open door sensible load' + ',%10.2f,%10.2f' 
                                          % (Unit.BtuH_Watt(self.obj_data.QSDRFF)
                                             , Unit.BtuH_Watt(self.obj_data.QSDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - condense' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QLDRFF)
                                             , Unit.BtuH_Watt(self.obj_data.QLDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - frost' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFDRFF)
                                             , Unit.BtuH_Watt(self.obj_data.QFDRFZ)))

        self.objCabout.write_or_terminate(' ,Anti-sweat heater' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFASHQ)
                                             , Unit.BtuH_Watt(self.obj_data.FZASHQ)))
        self.objCabout.write_or_terminate(' ,Refrigerant line heat' + ',%10.2f,%10.2f'  
                                          % (Unit.BtuH_Watt(self.obj_data.FFREFQ)
                                             , Unit.BtuH_Watt(self.obj_data.FZREFQ)))
        self.objCabout.write_or_terminate(' ,Penetrations' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFPENA)
                                             , Unit.BtuH_Watt(self.obj_data.FZPENA)))
        self.objCabout.write_or_terminate(' ,Other thermal input' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFHEAT)
                                             , Unit.BtuH_Watt(self.obj_data.FZHEAT)))

        self.objCabout.write_or_terminate('Total ,' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFFTOT)
                                             , Unit.BtuH_Watt(self.obj_data.QFZTOT)))

    # -----------------------------------
    def show_rep(self):
        self.rep_title()
        self.sec01_Mode238()
        self.sec04_Mode3()
        self.sec08_Mode_all()
        self.sec10_Mode23578()
        self.sec11_Mode238()
        self.sec12_Mode238()
        self.sec13_Mode3()
        self.sec18_Moode238()
        self.sec20_Mode238()
        self.show_final_results()
        
        self.sec1_cycle_Mode238()
        self.sec4_cycle_Mode238()
        
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job            : View for Configration3(Mode 2)
#
# Editor       : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class View_Ql4(View):
    def show_final_results(self):
        pass
    # -----------------------------------

    def show_rep(self):
        self.rep_title()
        self.sec02_Mode47()
        self.sec06_Mode47()
        self.sec08_Mode_all()
        self.sec09_Mode4()
        self.sec15_Mode4()
        self.sec19_Mode457()
        self.sec21_Mode457()
        self.show_final_results()
        
        self.sec2_cycle_Mode4()
        self.sec5_cycle_Mode457()
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job            : View for Configration3(Mode 2)
#
# Editor       : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class View_Ql5(View):
    def show_final_results(self):
        self.objCabout.write_or_terminate(' ')
        self.objCabout.write_or_terminate('HEAT LEAK BREAKDOWN')   # 901
        self.objCabout.write_or_terminate(' , ,Freezer,(w)')

        self.objCabout.write_or_terminate(' ,Right wall' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QRSIDE)))
        self.objCabout.write_or_terminate(' ,Left wall' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QLSIDE)))
        self.objCabout.write_or_terminate(' ,Back wall' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QBACK)))
        self.objCabout.write_or_terminate(' ,Front(door)' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFRONT)))

        self.objCabout.write_or_terminate(' ,Top(door)' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QTOP)))
        self.objCabout.write_or_terminate(' ,Bottom' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QBTTM)))
        self.objCabout.write_or_terminate(' ,Gasket' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QGZF)))

        self.objCabout.write_or_terminate(' ,Open door sensible load' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QSDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - condense' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QLDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - frost' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFDRFZ)))

        self.objCabout.write_or_terminate(' ,Anti-sweat heater' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FZASHQ)))
        self.objCabout.write_or_terminate(' ,Refrigerant line heat' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FZREFQ)))
        self.objCabout.write_or_terminate(' ,Penetrations' + ',%10.2f' % (Unit.BtuH_Watt(self.obj_data.FZPENA)))
        self.objCabout.write_or_terminate(' ,Other thermal input' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FZHEAT)))
        self.objCabout.write_or_terminate('Total ,' + ',%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFZTOT)))
        
    # -----------------------------------
    def show_rep(self):
        self.rep_title()
        self.sec05_Mode5()
        self.sec08_Mode_all()
        self.sec10_Mode23578()
        self.sec16_Mode5()
        self.sec19_Mode457()
        self.sec21_Mode457()
        self.show_final_results()
        self.final_Mode57()
        
        self.sec3_cycle_Mode57()
        self.sec5_cycle_Mode457()
        
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job            : View for Configration3(Mode 2)
#
# Editor       : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class View_Ql7(View):
    def show_final_results(self):
        pass
    # -----------------------------------

    def show_rep(self):
        self.rep_title()
        self.sec02_Mode47()
        self.sec02_Mode7()
        self.sec06_Mode47()
        self.sec08_Mode_all()
        self.sec10_Mode23578()
        self.sec17_Mode7()
        self.sec19_Mode457()
        self.sec21_Mode457()
        self.show_final_results()
        self.final_Mode57()
        
        self.sec3_cycle_Mode57()
        self.sec5_cycle_Mode457()
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job            : View for Configration1(Mode 8)
#
# Editor       : aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


class View_Ql8(View):
    def show_final_results(self):
        # Format 900
        self.objCabout.write_or_terminate(' ')
        self.objCabout.write_or_terminate('HEAT LEAK BREAKDOWN')  # 902
        self.objCabout.write_or_terminate(' , ,Fresh food,Freezer')
        self.objCabout.write_or_terminate(' ,Description,(W),(W)')

        self.objCabout.write_or_terminate(' ,Right wall' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QRRSID)
                                             , Unit.BtuH_Watt(self.obj_data.QFRSID)))
        self.objCabout.write_or_terminate(' ,Left wall' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QRLSID)
                                             , Unit.BtuH_Watt(self.obj_data.QFLSID)))
        self.objCabout.write_or_terminate(' ,Back wall' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QRBACK)
                                             , Unit.BtuH_Watt(self.obj_data.QFBACK)))
        self.objCabout.write_or_terminate(' ,Front(door)' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QRFRNT)
                                             , Unit.BtuH_Watt(self.obj_data.QFFRNT)))

        self.objCabout.write_or_terminate(' ,Top(door)' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QTOP), Unit.BtuH_Watt(0.0)))   # chk order
        self.objCabout.write_or_terminate(' ,Bottom' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(0.0),  Unit.BtuH_Watt(self.obj_data.QBOTTM)))

        self.objCabout.write_or_terminate(' ,Mullion' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QMULN), Unit.BtuH_Watt(self.obj_data.QMUL)))
        self.objCabout.write_or_terminate(' ,Wedge' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QWFF), Unit.BtuH_Watt(self.obj_data.QWFZ)))
        self.objCabout.write_or_terminate(' ,Gasket' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QGR), Unit.BtuH_Watt(self.obj_data.QGZF)))

        self.objCabout.write_or_terminate(' ,Open door sensible load' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QSDRFF)
                                             , Unit.BtuH_Watt(self.obj_data.QSDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - condense' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QLDRFF)
                                             , Unit.BtuH_Watt(self.obj_data.QLDRFZ)))
        self.objCabout.write_or_terminate(' ,Moisture load - frost' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFDRFF)
                                             , Unit.BtuH_Watt(self.obj_data.QFDRFZ)))

        self.objCabout.write_or_terminate(' ,Anti-sweat heater' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFASHQ)
                                             , Unit.BtuH_Watt(self.obj_data.FZASHQ)))
        self.objCabout.write_or_terminate(' ,Refrigerant line heat' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFREFQ)
                                             , Unit.BtuH_Watt(self.obj_data.FZREFQ)))
        self.objCabout.write_or_terminate(' ,Penetrations' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFPENA)
                                             , Unit.BtuH_Watt(self.obj_data.FZPENA)))
        self.objCabout.write_or_terminate(' ,Other thermal input' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.FFHEAT)
                                             , Unit.BtuH_Watt(self.obj_data.FZHEAT)))

        self.objCabout.write_or_terminate('Total ,' + ',%10.2f,%10.2f'
                                          % (Unit.BtuH_Watt(self.obj_data.QFFTOT)
                                             , Unit.BtuH_Watt(self.obj_data.QFZTOT)))

    # -----------------------------------
    def show_rep(self):
        self.rep_title()
        self.sec01_Mode238()
        self.sec07_Mode8()
        self.sec08_Mode_all()
        self.sec10_Mode23578()
        self.sec11_Mode238()
        self.sec12_Mode238()
        self.sec14_Mode28()
        self.sec18_Moode238()
        self.sec20_Mode238()
        self.show_final_results()
        
        self.sec1_cycle_Mode238()
        self.sec4_cycle_Mode238()
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
