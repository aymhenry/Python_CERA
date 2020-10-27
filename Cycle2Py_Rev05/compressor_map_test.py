# Python Import ==================

# User Import ======================
from cycle_classes.CompMap import *

#================== Test Driver ========
def main ( str_file):
	
	print ("File Name: ", str_file)
	
	FLDER_COMPMAP_DAT = sys.path[0] + "\\" + "compmap"
	
	obj_comp_map = CompMap (str_file, FLDER_COMPMAP_DAT)

	if obj_comp_map.isError():
		print (obj_comp_map.err_description())
		return
		
	obj_comp_map.readMapData()
	
	if obj_comp_map.isError():
		print ( obj_comp_map.err_description() )
	
	else:
		print ("========= No error ==========")
		
		
	print (" Manif. = ",obj_comp_map.getManif() )
	print (" Model = ",obj_comp_map.getModel() )
	print (" kCal/hr = ",obj_comp_map.getKcal() )
	print (" EER = ",obj_comp_map.getEer () )
	print (" rpm = ",obj_comp_map.getRpm() )
	print (" Volt = ",obj_comp_map.getVolt () )
	
	print (" ......................................." )
	print (" unit = ",obj_comp_map.getUnit ()  )
	print (" Type = ",obj_comp_map.getType ()  )
	print (" x = ",obj_comp_map.getX_count ())
	print (" y = ",obj_comp_map.getY_count ())
	
	print (" ......................................." )
	print (" x = ",obj_comp_map.getX_values () )
	print (" y1 = ",obj_comp_map.getY1_values () )
	print (" y2 = ",obj_comp_map.getY2_values () )
	
	print (" ......................................." )
	print (" Capacity = ",obj_comp_map.getCapacity () )

	print (" ......................................." )
	print (" Power = ",obj_comp_map.getPower() )
	


#-------------------------------------
if __name__ == '__main__':
	arr_files = ['ABB_EMX70HSC.cmp','badbad.cmp.cmp','DG57C84TAU6.cmp','DG73C12.cmp','DG73C12RAU6.cmp',	\
		'DGH66C94.cmp','EGX90HLC.cmp','EGZ100HLP.cmp','Embraco_Model.cmp','EMBRACO_NT6215Z.cmp','EMU30HSC.cmp',	\
		'EMX70HSC.cmp','EMY60HER.cmp','files.txt','GVT44AD.cmp','GVY44AD.cmp','SF51C97.cmp','SF51NEW.cmp',	\
		'smoothed.cmp','SP51C97.cmp','TSA1374YAS.cmp','TTE46FK.cmp']
		
	#main("DGH66C94.cmp")
	main(arr_files[0])
	#for str_file in arr_files:
	#	main( str_file )
	#	print ("\n\n\n=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")

