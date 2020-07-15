# Python Import ==================

# User Import ======================
from cab_classes.Start import Start

obj_start = Start()

obj_start.main()
print ("==================================================")
print ("|            Cab. App Done Succufully            |")
print ("|     was create on current directory            |")
print ("|............................................... |")
print ("|                                                |")
print ("       Input File: " + Start.FILE_CAB_INPUT )
print ("      Output File: " + Start.FILE_CAB_OUTPUT )
print ("|                                                |")
print ("            Configration: " , obj_start.obj_data.IRFTYP)
print ("            Mode        : " , obj_start.obj_data.NMOD)
print ("==================================================")

