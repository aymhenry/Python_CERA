# Python Import ====================


# User Import ======================


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job 			: Show inputs given to cycle class
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class ShowInput:
        
    def __init__(self, dt):
        self.dt = dt

    def showdata(self, str_var_name, str_var_unit, str_var_desc):
        print(str_var_name,
              " = ", eval('self.dt.' + str_var_name),
              "\t", str_var_unit,
              "\t", str_var_desc
              )

    def graph(self):
        self.show_sigle_cycle()
        # self.show_double_cycle()
            
    def show(self):
        self.show_type1()
            
    def show_type1(self):
        self.showdata('TITLE', '#', 'MULTIPLE PATHWAYS BASELINE PROTOTYPE')
        self.showdata('TITLE2', '#', 'Title of report')
        self.showdata('FILERA', '#', 'FILE NAME')
        # self.showdata('ICYCL', '#', 'Cycle Type')
        self.showdata('IRFTYP', '#', 'Refrigeration Type')
        self.showdata('IDFRST', '#', 'Manual Defrost')
        # self.showdata('HRSOFF', 'hr', 'Hours Shutdown For Cycle Defrost')
        # self.showdata('TOL_FRSH', 'DEG C',
        #              'Torelance in iteration for fresh food')
        # self.showdata('TOL_FRZ', 'DEG C',
        #              'Torelance in iteration for frezzer')
        # self.showdata('TOL_COND', 'DEG C',
        #              'Torelance in iteration for condenser')
        # self.showdata('TOL_MASS', '%', 'Torelance in iteration for mass')
        # self.showdata('TOL_HX', '#',
        #               'Torelance in iteration for heat exchanger')
        self.showdata('N_EVAP', '#', 'Number of Zones on Evaporator')
        self.showdata('N_COND', '#', 'Number of Zones on Condenser')
        self.showdata('ICOMP', '#', 'Compressor Type')
        # self.showdata('IMAP', '#', 'Compressor Analysis')
        # self.showdata('I_CYCLE', '#', 'Cycling Loss Analysis')
        self.showdata('T_CYCLE', '#', 'Cycles Per Hour')
        self.showdata('I_VALVE', '#', 'Shut-Off Valve')
        self.showdata('FILE_NAME', '#', 'file name for map file')
        self.showdata('REF', '#', 'Code of Refrigerant ')
        # self.showdata('IR[2][1]', '#', 'Code (IR(2)) For 2nd Refrigerant')
        # self.showdata('IR[3][1]', '#', 'Code (IR(3)) For 3rd Refrigerant')
        # self.showdata('NC[1]', '#', 'Number Of Components (Maximum Of Three)')
        # self.showdata('F[1][2][1]', '#', 'Mixture Interaction Parameter- Component 1-2')
        # self.showdata('F[1][3][1]', '#', 'Mixture Interaction Parameter- Component 1-3')
        # self.showdata('F[2][3][1]', '#', 'Mixture Interaction Parameter- Component 2-3')
        # self.showdata('X[1][1]', '#', 'Mass Fraction Of 1st Refrigerant')
        # self.showdata('X[2][1]', '#', 'Mass Fraction Of 2nd Refrigerant')
        # self.showdata('X[3][1]', '#', 'Mass Fraction Of 3rd Refrigeran')
        self.showdata('ICONDI[1]', '#', 'Heat Exchanger Configuration')
        self.showdata('TS1[1]', 'DEG C', 'Temp Of Air Entering Condenser')
        self.showdata('CFMCI[1]', 'L/s', 'Air Flow Rate Across Coil')
        self.showdata('FNPWRC[1]', 'watt', 'Fan Power')
        self.showdata('DPC[1]', 'kpa', 'Pressure Drop Through Condenser')
        self.showdata('UDSCI[1]', 'W/m2-c',
                      'Desuperheating Heat Transfer Conductance')
        self.showdata('UTPCI[1]', 'W/m2-c', 'Two-Phase Heat Transfer Conductance')
        self.showdata('USCCI[1]', 'W/m2-c',
                      'Subcooling Heat Transfer Conductance')
        self.showdata('ATOTCI[1]', 'm2',
                      'Condenser Total Heat Transfer Surface Area')
        self.showdata('DTSBCI[1]', 'DEG C', 'Refrigerant Exit Subcooling')
        self.showdata('CONDHT[1]', 'watt', 'Liquid-Line Anti-Sweat Heat')
        self.showdata('CONDVP[1]', 'watt', 'Vapor-Line Anti-Sweat Heat')
        self.showdata('ISPECI[1]', '#', 'Evaporator Specification')
        self.showdata('IFRSHI[1]', '#', 'Heat Exchanger Configuration')
        self.showdata('TS3[1]', 'DEG C',
                      'Temp Of Air Entering Fresh Food Section Evaporator')
        self.showdata('CFMEI[1]', 'L/s', 'Air Flow Rate Across Coil')
        self.showdata('FNPWRE[1]', 'watt', 'Fan Power')
        self.showdata('DPE[1]', 'kPa',
                      'Pressure Drop Through Fresh Food Evaporator')
        self.showdata('UTPEI[1]', 'W/m2-c', 'Two-Phase Heat Transfer Conductance')
        self.showdata('USUPEI[1]', 'W/m2-c', 'Superheat Region Conductance')
        self.showdata('ATOTEI[1]', 'm2', 'Total Heat Transfer Surface Area')
        self.showdata('DTSPEI[1]', '#', 'Refrigerant Exit Superheat (C)')
        self.showdata('QUALITY[1]', '#', 'Quality (0-1)')
        self.showdata('MREFI[1]', 'kg/hr',
                      'Initial Guess For Refrigerant Mas Flow Rate')
        self.showdata('SPEEDI[1]', 'rpm', 'Nominal Speed')
        self.showdata('TSPECI[1]', 'DEG C',
                      'Temp. At Comp., Inlet or -1 If Unspecified')
            
        # self.showdata('DISPLC[1]', 'cm3', 'Compressor Displacement')
        # self.showdata('SIZEN[1]', 'kcal/hr', 'Rated Capacity')
        # self.showdata('SPDNOM[1]', '#', 'Fractional Speed')
        # self.showdata('EERN[1]', '#', 'Rated EER')
        # self.showdata('ICOOLN[1]', '#', 'Fan cooling method')
        # self.showdata('CEI[1]', 'cm3', 'Estimated clearance volume')
        # self.showdata('SEFFI[1]', '%', 'isentropic efficiency')
        # self.showdata('MEFF[1]', '%', 'Mechanical Efficiency')
        # self.showdata('ELOSS[1]', '#', 'Electrical LOSSES')
        # self.showdata('QCAN[1]', 'watt'
        #    ,'Compressor shell loss normalized to power input')
        # self.showdata('QHILO[1]', 'watt'
        #    ,'Normalized heat loss from dischange line inside the compressor'
        #        + 'shell to suction gas')
                
        self.showdata('SUPIHX[1]', 'DEG C', 'Interchanger exit superheat')
        self.showdata('ETHX[1]', '#', 'Effectiveness Of High Temp Interchanger')
        self.showdata('UA_FF', 'W/K',
                      'Evap: A/R In Fresh Food Section (Or Cabinet Walls)')
        self.showdata('UA_FZ', 'W/K',
                      'Evap: A/R In Freezer Section Walls (If Separate Section)')
        self.showdata('UA_ML', 'W/K',
                      'Evap: A/R In Mullion Section (If Present)')
        self.showdata('UA_FF_CND', 'W/K',
                      'Cond: A/R In Fresh Food Section (Or Cabinet Walls)')
        self.showdata('UA_FZ_CND', 'W/K',
                      'Cond: A/R In Freezer Section Walls (If Separate Section)')
        self.showdata('UA_FF_HXS', 'W/K',
                      'Both: A/R In Fresh Food Section (Or Cabinet Walls)')
        self.showdata('UA_FZ_HXS', 'W/K',
                      'Both: A/R In Freezer Section Walls (If Separate Section)')
        self.showdata('FRACT_FF', 'watt',
                      'Fraction Of Fresh Food Section (Or Cabinet Walls)')
        self.showdata('FRACT_FZ', 'watt',
                      'Fraction Of Freezer Section Walls (Including Mullion)')
        self.showdata('IWALL_FF', '#',
                      'FF (Or Cabinet) Evaporator Behind Liner')
        self.showdata('IWALL_FZ', '#', 'FZ Evaporator Behind Liner')
        self.showdata('DFSTCYC', 'watt', 'Closed-Door Automatic Defrost')
        self.showdata('FFCYC', 'watt', 'Fresh Food Section')
        self.showdata('FZCYC', 'watt', 'Freezer Section')
        self.showdata('OUTCYC', 'watt', 'Outside Cabinet')
        
        self.showdata('ICAB', '#',
                      'Flag to represent presence of cabinet loads in input, 0 =No')
            
        self.showdata('FFASH', 'watt', 'Fresh Food Antisweat Heater')
        self.showdata('FFAUX', 'watt', 'Fresh Food Auxiliary Power')
        self.showdata('FZASH', 'watt', 'Freezer Antisweat Heater')
        self.showdata('FZAUX', 'watt', 'Freezer Auxiliary Power')
        self.showdata('OTHERW', 'watt', 'Other heat')
        self.showdata('TROOM', 'DEG C', 'Room Temperature')
        self.showdata('FFTEMP', 'DEG C', 'Fresh Food Temperature')
        self.showdata('FZTEMP', 'DEG C', 'Freezer Temperature')
        self.showdata('FFQ', 'watt', 'Fresh Food Net Load')
        self.showdata('FZQOFF', 'watt', 'Freezer Load')
        self.showdata('FFSEN', 'watt', 'Fresh Food Door Sensible Load')
        self.showdata('FFLAT', 'watt', 'Fresh Food Door Condensation')
        self.showdata('FROSTF', 'watt', 'Fresh Food Door Frost Load')
        self.showdata('FZSEN', 'watt', 'Freezer Door Sensible Load')
        self.showdata('FZLAT', 'watt', 'Freezer Door Condensation Load')
        self.showdata('FROSTZ', 'watt', 'Freezer Door Frost Load')
        self.showdata('FFPENA', 'watt', 'Fresh Food Penetrations')
        self.showdata('FZPENA', 'watt', 'Freezer Penetrations')
        self.showdata('FFHTQ', 'watt', 'Fresh Food Heaters And Controls')
        self.showdata('FZHTQ', 'watt', 'Freezer Heaters And Controls')
        self.showdata('FFREFQ', 'watt', 'Fresh Food Refrigerant Line')
        self.showdata('FZREFQ', 'watt', 'Freezer Refrigerant Line')
        self.showdata('QMUL', 'watt', 'Mullion Heat Load')

    def show_type2(self):
        pass

    def show_type3(self):
        pass

    def show_type4(self):
        pass
    
    def show_sigle_cycle(self):
        print("\n\n    Diagram of a Single Evaporator Cycle")
        print("                           ")
        print("              COND BUB    COND-DEW                           ")
        print(" COND OUT     11          3         COND IN                  ")
        print("         4    +-----------+         14                       ")
        print("         +----| Condenser +----------+------<---+            ")
        print("         |    +-----------+                     | 2  COMP DIS")
        print("         |                                    +-+--+         ")
        print("HX MULL  |+---------+------------>--+         |    |         ")
        print("      16 ||         13              |         |Comp|         ")
        print("       +-||-+     HX OUT            |         +-+--+         ")
        print("       | || |                       |           | 1  COMP IN ")
        print("       | || |interchanger           |           |            ")
        print("       | || |                       |           |            ")
        print("       +-||-+                       +---->------+            ")
        print("       6 ||                                      ")
        print("SUBCOOL  ||                                      ")
        print("         |+------<------------------------------+")
        print("         |  EVAP DEW                            |")
        print("         |    12                                |")
        print("        (X)   +----------+                      |")
        print("         +->--|Evaparator|-->-------------------+")
        print("         5    +----------+    7                  ")
        print("         EVAP IN              EVAP OUT     \n\n\n")

    def show_double_cycle(self):
        print("\n\n    Diagram of a Two Evaporator Cycle")
        print("                           ")
        print("              COND BUB    COND-DEW                           ")
        print(" COND OUT     11          3         COND IN                  ")
        print("         4    +-----------+         14                       ")
        print("         +----| Condenser +----------+------<---+            ")
        print("         |    +-----------+                     | 2  COMP DIS")
        print("         |                                    +-+--+         ")
        print("HX MULL  |+---------+------------>--+         |    |         ")
        print("      16 ||         13              |         |Comp|         ")
        print("       +-||-+     HX OUT            |         +-+--+         ")
        print("       | || |                       |           | 1  COMP IN ")
        print("       | || | hight temp            |           |            ")
        print("       +-||-+ interchanger          +---->------+            ")
        print("       6 ||                                      ")
        print("SUBCOOL2 ||                                      ")
        print("         |+------<------------------------------+")
        print("         |    FRSH DEW                          |")
        print("         |    12                                |")
        print("         |    +---------------------+           |")
        print("         |+---|Fresh Food Evaparator|-->--------+")
        print("         || 5  +--------------------+  7         ")
        print("         || FRSH IN                    FRSH OUT  ")
        print("       +-||-+                                    ")
        print("       | || |                                    ")
        print("       | || | low temp                           ")
        print("       +-||-+ interchanger                       ")
        print("      10 ||                                      ")        
        print("SUBCOOL1 ||                                      ")        
        print("         |+------<------------------------------+")
        print("         |                                      |")
        print("         |    FREZ DEW                          |")
        print("        (X)   +------------------+              |")
        print("         +->--|Freezer Evaparator|-->-----------+")
        print("         8    +------------------+  9            ")
        print("         FREZ IN                   FREZ OUT\n\n\n")
