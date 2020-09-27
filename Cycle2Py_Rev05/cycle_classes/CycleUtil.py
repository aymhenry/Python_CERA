# Python Import ==================

# User Import

class CycleUtil:
	# =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
	# Utililty for extract 2d array to 1d array
	# =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
	
	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	# in Python only, to fix Fortant issue
	def setArr2dCol(self, arry_2dim, int_col, arr_1dim ):
		for ncnt in range (len(arry_2dim)):
			arry_2dim [ncnt][int_col] = arr_1dim [ncnt]
		return

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def getArr2dCol(self, arry_2dim, int_col):
		arr_1dim = []
		for ncnt in range (len(arry_2dim)):
			arr_1dim.append ( arry_2dim [ncnt][int_col])
		return arr_1dim

	#=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.==.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=.=
	def getArr3dLevel(self, arry_3dim, int_level):
		x_direc = len(arry_3dim)
		y_direc = len(arry_3dim[0])
		arr_2dim = [[0.0] * (y_direc +1) for i in range(x_direc)]
		for x in range (x_direc):
			for y in range (y_direc):
				arr_2dim [x][y] =( arry_3dim [x][y][int_level])
		return arr_2dim
		
