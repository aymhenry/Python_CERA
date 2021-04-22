# Python Import


# User Import
#from .FileAccess import FileAccess
#from .QData import QData
from .DataModelBuiler import DataModelBuiler

#-----------------------------------------------------------
# Job 			: Creates object with data from file for Q class
#
# Editor		: aymhenry@gmail.com
#-----------------------------------------------------------
class CycleDataModelBuiler (DataModelBuiler):
	DataModelBuiler.lst_required_data =[ 98,98, 98,98] # required data for each configration
	DataModelBuiler.MAX_DATA_FILE_TO_READ = 150		# maximum data file lines to read, this need to be updated if there is any data line more than this	
	
	DataModelBuiler.CONFIGRATION_COUNT = 4			# max. number of configrations
	DataModelBuiler.CONFIGRATION_ROW = 3+1 			# set row number that has the configration
	
	#-----------------------------------------------------------
	# Job 			: Assign values from table to selected class vars
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def assign_vars (self):
		lst_config1 = ['TITLE','TITLE2','FILERA','ICYCL','IRFTYP','IDFRST','HRSOFF','ICOMP','IMAP','I_CYCLE','T_CYCLE','I_VALVE','IR[1][1]','IR[2][1]','IR[3][1]','NC[1]','F[1][2][1]','F[1][3][1]','F[2][3][1]','X[1][1]','X[2][1]','X[3][1]','ICONDI[1]','TS1[1]','CFMCI[1]','FNPWRC[1]','DPC[1]','UDSCI[1]','UTPCI[1]','USCCI[1]','ATOTCI[1]','DTSBCI[1]','CONDHT[1]','CONDVP[1]','ISPECI[1]','IFRSHI[1]','TS3[1]','CFMEI[1]','FNPWRE[1]','DPE[1]','UTPEI[1]','USUPEI[1]','ATOTEI[1]','DTSPEI[1]','MREFI[1]','SPEEDI[1]','TSPECI[1]','DISPLC[1]','SIZEN[1]','SPDNOM[1]','EERN[1]','ICOOLN[1]','CEI[1]','SEFFI[1]','MEFF[1]','ELOSS[1]','QCAN[1]','QHILO[1]','SUPIHX[1]','ETHX[1]','UA_FF','UA_FZ','UA_ML','UA_FF_CND','UA_FZ_CND','UA_FF_HXS','UA_FZ_HXS','FRACT_FF','FRACT_FZ','IWALL_FF','IWALL_FZ','DFSTCYC','FFCYC','FZCYC','OUTCYC','FFASH','FFAUX','FZASH','FZAUX','OTHERW','TROOM','FFTEMP','FZTEMP','FFQ','FZQOFF','FFSEN','FFLAT','FROSTF','FZSEN','FZLAT','FROSTZ','FFPENA','FZPENA','FFHTQ','FZHTQ','FFREFQ','FZREFQ','QMUL']
		lst_config2 = ['TITLE','TITLE2','FILERA','ICYCL','IRFTYP','IDFRST','HRSOFF','ICOMP','IMAP','I_CYCLE','T_CYCLE','I_VALVE','IR[1][1]','IR[2][1]','IR[3][1]','NC[1]','F[1][2][1]','F[1][3][1]','F[2][3][1]','X[1][1]','X[2][1]','X[3][1]','ICONDI[1]','TS1[1]','CFMCI[1]','FNPWRC[1]','DPC[1]','UDSCI[1]','UTPCI[1]','USCCI[1]','ATOTCI[1]','DTSBCI[1]','CONDHT[1]','CONDVP[1]','ISPECI[1]','IFRSHI[1]','TS3[1]','CFMEI[1]','FNPWRE[1]','DPE[1]','UTPEI[1]','USUPEI[1]','ATOTEI[1]','DTSPEI[1]','MREFI[1]','SPEEDI[1]','TSPECI[1]','DISPLC[1]','SIZEN[1]','SPDNOM[1]','EERN[1]','ICOOLN[1]','CEI[1]','SEFFI[1]','MEFF[1]','ELOSS[1]','QCAN[1]','QHILO[1]','SUPIHX[1]','ETHX[1]','UA_FF','UA_FZ','UA_ML','UA_FF_CND','UA_FZ_CND','UA_FF_HXS','UA_FZ_HXS','FRACT_FF','FRACT_FZ','IWALL_FF','IWALL_FZ','DFSTCYC','FFCYC','FZCYC','OUTCYC','FFASH','FFAUX','FZASH','FZAUX','OTHERW','TROOM','FFTEMP','FZTEMP','FFQ','FZQOFF','FFSEN','FFLAT','FROSTF','FZSEN','FZLAT','FROSTZ','FFPENA','FZPENA','FFHTQ','FZHTQ','FFREFQ','FZREFQ','QMUL']
		lst_config3 = ['TITLE','TITLE2','FILERA','ICYCL','IRFTYP','IDFRST','HRSOFF','ICOMP','IMAP','I_CYCLE','T_CYCLE','I_VALVE','IR[1][1]','IR[2][1]','IR[3][1]','NC[1]','F[1][2][1]','F[1][3][1]','F[2][3][1]','X[1][1]','X[2][1]','X[3][1]','ICONDI[1]','TS1[1]','CFMCI[1]','FNPWRC[1]','DPC[1]','UDSCI[1]','UTPCI[1]','USCCI[1]','ATOTCI[1]','DTSBCI[1]','CONDHT[1]','CONDVP[1]','ISPECI[1]','IFRSHI[1]','TS3[1]','CFMEI[1]','FNPWRE[1]','DPE[1]','UTPEI[1]','USUPEI[1]','ATOTEI[1]','DTSPEI[1]','MREFI[1]','SPEEDI[1]','TSPECI[1]','DISPLC[1]','SIZEN[1]','SPDNOM[1]','EERN[1]','ICOOLN[1]','CEI[1]','SEFFI[1]','MEFF[1]','ELOSS[1]','QCAN[1]','QHILO[1]','SUPIHX[1]','ETHX[1]','UA_FF','UA_FZ','UA_ML','UA_FF_CND','UA_FZ_CND','UA_FF_HXS','UA_FZ_HXS','FRACT_FF','FRACT_FZ','IWALL_FF','IWALL_FZ','DFSTCYC','FFCYC','FZCYC','OUTCYC','FFASH','FFAUX','FZASH','FZAUX','OTHERW','TROOM','FFTEMP','FZTEMP','FFQ','FZQOFF','FFSEN','FFLAT','FROSTF','FZSEN','FZLAT','FROSTZ','FFPENA','FZPENA','FFHTQ','FZHTQ','FFREFQ','FZREFQ','QMUL']
		lst_config4 = ['TITLE','TITLE2','FILERA','ICYCL','IRFTYP','IDFRST','HRSOFF','ICOMP','IMAP','I_CYCLE','T_CYCLE','I_VALVE','IR[1][1]','IR[2][1]','IR[3][1]','NC[1]','F[1][2][1]','F[1][3][1]','F[2][3][1]','X[1][1]','X[2][1]','X[3][1]','ICONDI[1]','TS1[1]','CFMCI[1]','FNPWRC[1]','DPC[1]','UDSCI[1]','UTPCI[1]','USCCI[1]','ATOTCI[1]','DTSBCI[1]','CONDHT[1]','CONDVP[1]','ISPECI[1]','IFRSHI[1]','TS3[1]','CFMEI[1]','FNPWRE[1]','DPE[1]','UTPEI[1]','USUPEI[1]','ATOTEI[1]','DTSPEI[1]','MREFI[1]','SPEEDI[1]','TSPECI[1]','DISPLC[1]','SIZEN[1]','SPDNOM[1]','EERN[1]','ICOOLN[1]','CEI[1]','SEFFI[1]','MEFF[1]','ELOSS[1]','QCAN[1]','QHILO[1]','SUPIHX[1]','ETHX[1]','UA_FF','UA_FZ','UA_ML','UA_FF_CND','UA_FZ_CND','UA_FF_HXS','UA_FZ_HXS','FRACT_FF','FRACT_FZ','IWALL_FF','IWALL_FZ','DFSTCYC','FFCYC','FZCYC','OUTCYC','FFASH','FFAUX','FZASH','FZAUX','OTHERW','TROOM','FFTEMP','FZTEMP','FFQ','FZQOFF','FFSEN','FFLAT','FROSTF','FZSEN','FZLAT','FROSTZ','FFPENA','FZPENA','FFHTQ','FZHTQ','FFREFQ','FZREFQ','QMUL']

		lst_var_names = []
		
		# creates blank array to put the basic inputs
		self.setup_array ()
		
		# according to the given configration in data file, saved in int_configration
		# set the realtive list of variable name in list
		if self.int_configration == 1:
			lst_var_names = lst_config1

		elif self.int_configration == 2:
			lst_var_names = lst_config2

		elif self.int_configration == 3:
			lst_var_names = lst_config3

		elif self.int_configration == 4:
			lst_var_names = lst_config4
		
		# send data list of variable, and relative list of values to data object.
		# data object put values of data list on variable
		self.obj_qdata.setup_vars ( self.lst_data, lst_var_names\
			,self.CONFIGRATION_ROW, self.int_parameter_count)
		
		# create a list with items title1 ,2 .. up to CONFIGRATION_ROW
		lst_title = ['title'+str(n) for n in range(0, self.CONFIGRATION_ROW)]
		
		#create vars title1 up to CONFIGRATION_ROW, with the given values in list
		self.obj_qdata.setup_vars ( self.lst_data, lst_title, 0, self.CONFIGRATION_ROW -1, True)
		
	#-----------------------------------------------------------
	# Job 			: creates blank array to put the basic inputs
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	def setup_array (self):
		
		##  X(5,2)  
		self.obj_qdata.X  =  [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]
		
		## IR(5,2) 
		self.obj_qdata.IR  =  [[0.0] * (2+1) for i in range(5+1)]	# array(Rows, Cols) = [[0] * Cols for i in range(Rows)]

		self.obj_qdata.F =  [  [[0.0] * (2+1) for i in range(5+1)] for j in range(5+1)  ]

		self.obj_qdata.NC = [0.0] * (2+1)
		#self.obj_qdata.pythNC = [0.0] * (2+1)
	
		self.obj_qdata.ICONDI = [0.0] * (2+1)
		self.obj_qdata.TS1 = [0.0] * (2+1)
		self.obj_qdata.CFMCI = [0.0] * (2+1)
		self.obj_qdata.FNPWRC = [0.0] * (2+1)
		self.obj_qdata.DPC = [0.0] * (2+1)
		self.obj_qdata.UDSCI = [0.0] * (2+1)
		self.obj_qdata.UTPCI = [0.0] * (2+1)
		self.obj_qdata.USCCI = [0.0] * (2+1)
		self.obj_qdata.ATOTCI = [0.0] * (2+1)
		self.obj_qdata.DTSBCI = [0.0] * (2+1)
		self.obj_qdata.CONDHT = [0.0] * (2+1)
		self.obj_qdata.CONDVP = [0.0] * (2+1)
		self.obj_qdata.ISPECI = [0.0] * (2+1)
		self.obj_qdata.IFRSHI = [0.0] * (2+1)
		self.obj_qdata.TS3 = [0.0] * (2+1)
		self.obj_qdata.CFMEI = [0.0] * (2+1)
		self.obj_qdata.FNPWRE = [0.0] * (2+1)
		self.obj_qdata.DPE = [0.0] * (2+1)
		self.obj_qdata.UTPEI = [0.0] * (2+1)
		self.obj_qdata.USUPEI = [0.0] * (2+1)
		self.obj_qdata.ATOTEI = [0.0] * (2+1)
		self.obj_qdata.DTSPEI = [0.0] * (2+1)
		self.obj_qdata.MREFI = [0.0] * (2+1)
		self.obj_qdata.SPEEDI = [0.0] * (2+1)
		self.obj_qdata.TSPECI = [0.0] * (2+1)
		self.obj_qdata.DISPLC = [0.0] * (2+1)
		self.obj_qdata.SIZEN = [0.0] * (2+1)
		self.obj_qdata.SPDNOM = [0.0] * (2+1)
		self.obj_qdata.EERN = [0.0] * (2+1)
		self.obj_qdata.ICOOLN = [0.0] * (2+1)
		self.obj_qdata.CEI = [0.0] * (2+1)
		self.obj_qdata.SEFFI = [0.0] * (2+1)
		self.obj_qdata.MEFF = [0.0] * (2+1)
		self.obj_qdata.ELOSS = [0.0] * (2+1)
		self.obj_qdata.QCAN = [0.0] * (2+1)
		self.obj_qdata.QHILO = [0.0] * (2+1)
		self.obj_qdata.SUPIHX = [0.0] * (2+1)
		self.obj_qdata.ETHX = [0.0] * (2+1)




