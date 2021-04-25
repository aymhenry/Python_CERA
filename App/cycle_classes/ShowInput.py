# Python Import ====================


# User Import ======================


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job 			: Show inputs given to cycle class
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class ShowInput:
        
    def __init__(self, dt, cab):
        self.dt = dt
        self.cab = cab

    def showdata(self, str_var_name, str_data_sor, str_var_unit, str_var_desc):
        print(str_data_sor + "." + str_var_name, 
              " = ", eval('self.' + str_data_sor + '.' + str_var_name),
              "\t", str_var_unit,
              "\t", str_var_desc
              )

    def graph(self):
        self.show_sigle_cycle()
        # self.show_double_cycle()
            
    def show(self):
        self.show_type1()
            
    def show_type1(self):
        self.showdata('TITLE', 'dt', '#', 'MULTIPLE PATHWAYS BASELINE PROTOTYPE')
        self.showdata('TITLE2', 'dt', '#', 'Title of report')
        self.showdata('FILERA', 'dt', '#', 'FILE NAME')
  
        self.showdata('IDFRST', 'dt', '#', 'Manual Defrost')

        self.showdata('N_EVAP', 'dt', '#', 'Number of Zones on Evaporator')
        self.showdata('N_COND', 'dt', '#', 'Number of Zones on Condenser')
        self.showdata('ICOMP', 'dt', '#', 'Compressor Type')

        self.showdata('T_CYCLE', 'dt', '#', 'Cycles Per Hour')
        self.showdata('I_VALVE', 'dt', '#', 'Shut-Off Valve')
        self.showdata('FILE_NAME', 'dt', '#', 'file name for map file')
        self.showdata('REF', 'dt', '#', 'Code of Refrigerant ')

        self.showdata('ICONDI[1]', 'dt', '#', 'Heat Exchanger Configuration')
        self.showdata('TS1[1]', 'dt', 'DEG C', 'Temp Of Air Entering Condenser')
        self.showdata('CFMCI[1]', 'dt', 'L/s', 'Air Flow Rate Across Coil')
        self.showdata('FNPWRC[1]', 'dt', 'watt', 'Fan Power')
        self.showdata('DPC[1]', 'dt', 'kpa', 'Pressure Drop Through Condenser')
        self.showdata('UDSCI[1]', 'dt', 'W/m2-c',
                      'Desuperheating Heat Transfer Conductance')
        self.showdata('UTPCI[1]', 'dt', 'W/m2-c', 'Two-Phase Heat Transfer Conductance')
        self.showdata('USCCI[1]', 'dt', 'W/m2-c',
                      'Subcooling Heat Transfer Conductance')
        self.showdata('ATOTCI[1]', 'dt', 'm2',
                      'Condenser Total Heat Transfer Surface Area')
        self.showdata('DTSBCI[1]', 'dt', 'DEG C', 'Refrigerant Exit Subcooling')
        self.showdata('CONDHT[1]', 'dt', 'watt', 'Liquid-Line Anti-Sweat Heat')
        self.showdata('CONDVP[1]', 'dt', 'watt', 'Vapor-Line Anti-Sweat Heat')
        self.showdata('ISPECI[1]', 'dt', '#', 'Evaporator Specification')
        self.showdata('IFRSHI[1]', 'dt', '#', 'Heat Exchanger Configuration')
        self.showdata('TS3[1]', 'dt', 'DEG C',
                      'Temp Of Air Entering Fresh Food Section Evaporator')
        self.showdata('CFMEI[1]', 'dt', 'L/s', 'Air Flow Rate Across Coil')
        self.showdata('FNPWRE[1]', 'dt', 'watt', 'Fan Power')
        self.showdata('DPE[1]', 'dt', 'kPa',
                      'Pressure Drop Through Fresh Food Evaporator')
        self.showdata('UTPEI[1]', 'dt', 'W/m2-c', 'Two-Phase Heat Transfer Conductance')
        self.showdata('USUPEI[1]', 'dt', 'W/m2-c', 'Superheat Region Conductance')
        self.showdata('ATOTEI[1]', 'dt', 'm2', 'Total Heat Transfer Surface Area')
        self.showdata('DTSPEI[1]', 'dt', '#', 'Refrigerant Exit Superheat (C)')
        self.showdata('QUALITY[1]', 'dt', '#', 'Quality (0-1)')
        self.showdata('MREFI[1]', 'dt', 'kg/hr',
                      'Initial Guess For Refrigerant Mas Flow Rate')
        self.showdata('SPEEDI[1]', 'dt', 'rpm', 'Nominal Speed')
        self.showdata('TSPECI[1]', 'dt', 'DEG C',
                      'Temp. At Comp., Inlet or -1 If Unspecified')
                
        self.showdata('SUPIHX[1]', 'dt', 'DEG C', 'Interchanger exit superheat')
        self.showdata('ETHX[1]', 'dt', '#', 'Effectiveness Of High Temp Interchanger')
        self.showdata('UA_FF', 'dt', 'W/K',
                      'Evap: A/R In Fresh Food Section (Or Cabinet Walls)')
        self.showdata('UA_FZ', 'dt', 'W/K',
                      'Evap: A/R In Freezer Section Walls (If Separate Section)')
        self.showdata('UA_ML', 'dt', 'W/K',
                      'Evap: A/R In Mullion Section (If Present)')
        self.showdata('UA_FF_CND', 'dt', 'W/K',
                      'Cond: A/R In Fresh Food Section (Or Cabinet Walls)')
        self.showdata('UA_FZ_CND', 'dt', 'W/K',
                      'Cond: A/R In Freezer Section Walls (If Separate Section)')
        self.showdata('UA_FF_HXS', 'dt', 'W/K',
                      'Both: A/R In Fresh Food Section (Or Cabinet Walls)')
        self.showdata('UA_FZ_HXS', 'dt', 'W/K',
                      'Both: A/R In Freezer Section Walls (If Separate Section)')
        self.showdata('FRACT_FF', 'dt', 'watt',
                      'Fraction Of Fresh Food Section (Or Cabinet Walls)')
        self.showdata('FRACT_FZ', 'dt', 'watt',
                      'Fraction Of Freezer Section Walls (Including Mullion)')
        self.showdata('IWALL_FF', 'dt', '#',
                      'FF (Or Cabinet) Evaporator Behind Liner')
        self.showdata('IWALL_FZ', 'dt', '#', 'FZ Evaporator Behind Liner')
        self.showdata('DFSTCYC', 'dt', 'watt', 'Closed-Door Automatic Defrost')
        self.showdata('FFCYC', 'dt', 'watt', 'Fresh Food Section')
        self.showdata('FZCYC', 'dt', 'watt', 'Freezer Section')
        self.showdata('OUTCYC', 'dt', 'watt', 'Outside Cabinet')
        
        self.showdata('ICAB', 'dt', '#',
                      'Flag to represent presence of cabinet loads in input, 0 =No')

        self.showdata('IRFTYP', 'cab', '#', 'Refrigeration Type')
        self.showdata('FFASH', 'cab', 'watt', 'Fresh Food Antisweat Heater')
        self.showdata('FFAUX', 'cab', 'watt', 'Fresh Food Auxiliary Power')
        
        self.showdata('FZASH', 'cab', 'watt', 'Freezer Antisweat Heater')
        self.showdata('FZAUX', 'cab', 'watt', 'Freezer Auxiliary Power')
        self.showdata('OTHERW', 'cab', 'watt', 'Other heat')
        
        self.showdata('TROOM', 'cab', 'DEG C', 'Room Temperature')
        self.showdata('FFTEMP', 'cab', 'DEG C', 'Fresh Food Temperature')
        self.showdata('FZTEMP', 'cab', 'DEG C', 'Freezer Temperature')
        
        self.showdata('FFQ', 'cab', 'watt', 'Fresh Food Net Load')
        self.showdata('FZQOFF', 'cab', 'watt', 'Freezer Load')
        
        self.showdata('FFSEN', 'cab', 'watt', 'Fresh Food Door Sensible Load')
        self.showdata('FFLAT', 'cab', 'watt', 'Fresh Food Door Condensation')
        self.showdata('FROSTF', 'cab', 'watt', 'Fresh Food Door Frost Load')
        
        self.showdata('FZSEN', 'cab', 'watt', 'Freezer Door Sensible Load')
        self.showdata('FZLAT', 'cab', 'watt', 'Freezer Door Condensation Load')
        self.showdata('FROSTZ', 'cab', 'watt', 'Freezer Door Frost Load')
        
        self.showdata('FFPENA', 'cab', 'watt', 'Fresh Food Penetrations')
        self.showdata('FZPENA', 'cab', 'watt', 'Freezer Penetrations')
        
        self.showdata('FFHTQ', 'cab', 'watt', 'Fresh Food Heaters And Controls')
        self.showdata('FZHTQ', 'cab', 'watt', 'Freezer Heaters And Controls')
        
        self.showdata('FFREFQ', 'cab', 'watt', 'Fresh Food Refrigerant Line')
        self.showdata('FZREFQ', 'cab', 'watt', 'Freezer Refrigerant Line')
        
        self.showdata('QMUL', 'cab', 'watt', 'Mullion Heat Load')

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
