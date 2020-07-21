# Python Import ====================
import math
from abc import ABC,abstractmethod

# User Import ======================
from .Volume import *
from .View import *

from .Unit import Unit

from .Ql2 import Ql2
from .Ql13 import Ql13
from .Ql467 import Ql467
from .Ql5 import Ql5
from .Ql8 import Ql8
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Abstract Class from Control Q class
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class QCtrl_Abstract (ABC):
	def __init__ (self, objdata):
		self.obj_data = objdata
		
	# Abstract methods
	#-----------------------------------------------------------
	# Job 			: Change units, from the give SI units. (individual for every sub-class)
	# Input 		:
	#
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def adjust_units (self):
		pass
		
	#-----------------------------------------------------------
	# Job 			: Output data from obj_data data store. (individual for every sub-class)
	# Input 		: str_file_cab : cabinet data file name to save to
	#				str_file_cycle : cycle data file name to save to
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def view (self, str_file_cab, str_file_cycle, str_path_cab = "", str_path_cycle = ""):
		pass

	#-----------------------------------------------------------
	# Job 			: calcultae volume of freezer & fresh food compartment (individual for every sub-class)
	# Input 		: 
	#				
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def volume (self):
		pass
		
	#-----------------------------------------------------------
	# Job 			: inialize extra varibale in ob_data object with value (individual for every sub-class)
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def setup_vars_extra (self):
		pass
		
	#-----------------------------------------------------------
	# Job 			: Calculate heat rate anaylis for cabinet. (individual for every sub-class)
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def calculte (self):
		pass		

	#-----------------------------------------------------------
	# Job 			: Calculate cycle data, (individual for every sub-class)
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	@abstractmethod
	def calculte_cycle (self):
		pass

	#-----------------------------------------------------------
	# Job 			: setup common vars required by all module with zero value. (common for all)
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def set_vars_common (self):
		self.obj_data.setup_vars ( 0.0, ['VFZ', 'VFF', 'NCCTYPE', 'NMOD', 'WATERF','WATERZ'] )
		
	#-----------------------------------------------------------
	# Job 			: Calculate cycle type according to Compressor Compartment dimentions. (common for all)
	# Input 		: CCHGT : Compressor Compartment Height
	#				 CDUP : Compressor Compartment Top Depth
	# Output		:
	#-----------------------------------------------------------
	def set_ncctype (self):
		# this value is NA in mode 5
		if self.obj_data.NMOD == 5: return
		
		ncc_type = None
		if self.obj_data.CCHGT != 0.0:
			if self.obj_data.CDUP != 0.0:
				ncc_type =  3
			else:
				ncc_type =  2
		else:
			ncc_type = 1
		self.obj_data.NCCTYPE = ncc_type
	
	#-----------------------------------------------------------
	# Job 			: Get the mode, related to the current configration. (common for all).
	# Input 		: Configration
	#
	# Output		: Mode
	#-----------------------------------------------------------
	def setMode (self):
		# Python comment IRFTYP
		# 1: Two door, top - mount refrigerator / freezer
		# 2: Two door, bottom - mount refrigerator / freezer
		# 3: Side by side refrigerator / freezer
		# 4: Chest freezer
		# 5: Upright freezer
		# 6: One door refrigerator
		# 7: Two door refrigerator / freezer
		
		ITYMOD = [3,8,2,5,7,4,3] # internal number of the configuration to be run
		# Configration 1 == > Cab_NMOD = 3	Top - mount refrigerator / freezer
		# Configration 2 == > Cab_NMOD = 8	Bottom - mount refrigerator / freezer
		# Configration 3 == > Cab_NMOD = 2	Side - by - side refrigerator / freezer
		# Configration 4 == > Cab_NMOD = 5	Chest freezer
		# Configration 5 == > Cab_NMOD = 7	Upright freezer
		# Configration 6 == > Cab_NMOD = 4	Single - door refrigerator
		# Configration 7 == > Cab_NMOD = 3	Top - mount refrigerator / freezer
		
		if self.obj_data.IRFTYP >=1 and self.obj_data.IRFTYP <=7:	# extar safety check
			self.obj_data.NMOD = ITYMOD [self.obj_data.IRFTYP -1]
		else:
			print ("Error in Configration @Start")
			
	#-----------------------------------------------------------
	# Job 			: common equation to all modules used to start cycle calulations. (common for all)
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def calc_cycle_start (self):
		#
		#           Total Door Opening Heat Leaks
		#
		self.obj_data.QSDRFF = self.obj_data.QDFFCS
		self.obj_data.QLDRFF = self.obj_data.QDFFCL
		self.obj_data.QFDRFF = self.obj_data.QDFFFL - self.obj_data.QDFFCL

		self.obj_data.QSDRFZ = self.obj_data.QDFZFS
		self.obj_data.QLDRFZ = self.obj_data.QDFZCL
		self.obj_data.QFDRFZ = self.obj_data.QDFZFL - self.obj_data.QDFZCL
	
	#-----------------------------------------------------------
	# Job 			: common unit change from SI. (common for all)
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def adj_unit_common (self):
		self.obj_data.HEIGHT = Unit.cm_feet(self.obj_data.HEIGHT )
		self.obj_data.WIDTH = Unit.cm_feet (self.obj_data.WIDTH )
		self.obj_data.DEPTH = Unit.cm_feet (self.obj_data.DEPTH )
		
		self.obj_data.DIL = Unit.mm_feet (self.obj_data.DIL )
		self.obj_data.DOL = Unit.mm_feet (self.obj_data.DOL )
		
		self.obj_data.TIFRS = Unit.cm_feet (self.obj_data.TIFRS )
		
		self.obj_data.DGSKT = Unit.cm_feet (self.obj_data.DGSKT )
		#---------------------
		self.obj_data.BINSUL = Unit.cm_feet (self.obj_data.BINSUL )
		#---------------------
		self.obj_data.TIFB = Unit.cm_feet (self.obj_data.TIFB )
		self.obj_data.TIFF = Unit.cm_feet (self.obj_data.TIFF )
			
		self.obj_data.COL = Unit.WattMK_BtuHrFtF (self.obj_data.COL )
		self.obj_data.CIL = Unit.WattMK_BtuHrFtF (self.obj_data.CIL )
		
		self.obj_data.HXVUZ = Unit.liter_ft3 (self.obj_data.HXVUZ )
		self.obj_data.VOLAZ = Unit.liter_ft3 (self.obj_data.VOLAZ )
		
		self.obj_data.HRFRZ = ( self.obj_data.SECFRZ / 3600.0 ) * self.obj_data.FRZOPN
		
		self.obj_data.TDRAIR = Unit.c_f (self.obj_data.TDRAIR )
		self.obj_data.TA = self.obj_data.TDRAIR
		
		self.obj_data.TROOM = Unit.c_f (self.obj_data.TROOM )
		self.obj_data.TFRZ = Unit.c_f (self.obj_data.TFRZ )
		
		self.obj_data.TBTM = Unit.c_f (self.obj_data.TBTM )
		
		self.qrdset ( )

	#-----------------------------------------------------------
	# Job 			: cycle calculation pnly for modes 5 & 7. (common for all)
	#				this method called from Ql5 for mode (5)  and Ql467 (form mode 4,6,7)
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def calc_cycle_Mode57(self):
		self.obj_data.QFZTOT = self.obj_data.QRSIDE       + self.obj_data.QLSIDE + self.obj_data.QBACK  + self.obj_data.QFRONT + self.obj_data.QTOP 		\
			+ self.obj_data.QBOTTM + self.obj_data.QW     + self.obj_data.QGZF   + self.obj_data.QSDRFZ + self.obj_data.QLDRFZ 				\
			+ self.obj_data.QFDRFZ + self.obj_data.FZASHQ + self.obj_data.FZHEAT + self.obj_data.FZREFQ + self.obj_data.FZPENA

		self.obj_data.QHTFZ  = self.obj_data.FZASHQ + self.obj_data.FZHEAT	
		
	#-----------------------------------------------------------
	# Job 			: setup room temperatures value, for many variables
	# Input 		: 
	#			
	# Output		:
	#-----------------------------------------------------------
	def qrdset (self):
		# set up the convection coefficients around the cabinet 
		# 	modified to combine radiation and convection 
		# 	define the temperatures seen by each side
		self.obj_data.TFRONT = self.obj_data.TROOM
		self.obj_data.TLSIDE = self.obj_data.TROOM
		self.obj_data.TRSIDE = self.obj_data.TROOM
		self.obj_data.TTOP = self.obj_data.TROOM
		self.obj_data.TBACK = self.obj_data.TROOM
		self.obj_data.TFFRNT = self.obj_data.TROOM
		self.obj_data.TFBACK = self.obj_data.TROOM
		self.obj_data.TFLSID = self.obj_data.TROOM
		self.obj_data.TFRSID = self.obj_data.TROOM
		self.obj_data.TTOP = self.obj_data.TROOM
		self.obj_data.TRAFTT = self.obj_data.TROOM
		self.obj_data.TRABKT = self.obj_data.TROOM
		self.obj_data.TRALTT = self.obj_data.TROOM
		self.obj_data.TRARTT = self.obj_data.TROOM
		self.obj_data.TRADT = self.obj_data.TROOM
		self.obj_data.TRADBT = self.obj_data.TROOM
		self.obj_data.TRFRNT = self.obj_data.TROOM
		self.obj_data.TRBACK = self.obj_data.TROOM
		self.obj_data.TRLSID = self.obj_data.TROOM
		self.obj_data.TRRSID = self.obj_data.TROOM
		self.obj_data.TRAFTB = self.obj_data.TROOM
		self.obj_data.TRABKB = self.obj_data.TROOM
		self.obj_data.TRALTB = self.obj_data.TROOM
		self.obj_data.TRARTB = self.obj_data.TROOM
		self.obj_data.TRADBT = self.obj_data.TROOM
		#
		# define the convection coefficients
		#
		HIREF = 1.0
		HOREF = 1.47

		# Correct for the inner and outer liners
		HIREF = 1.0 / ( 1.0 / HIREF + self.obj_data.DIL / 12.0 / self.obj_data.CIL )
		HOREF = 1.0 / ( 1.0 / HOREF + self.obj_data.DOL / 12.0 / self.obj_data.COL )

		self.obj_data.HI = HIREF
		self.obj_data.HO = HOREF
		self.obj_data.HIFTT = HIREF

		self.obj_data.HIFMUL = 1.0
		self.obj_data.HIBKT = HIREF
		self.obj_data.HILTT = HIREF
		self.obj_data.HIRTT = HIREF

		self.obj_data.HIRMUL = 1.0

		self.obj_data.HITOP = HIREF
		self.obj_data.HIFTB = HIREF
		self.obj_data.HIBKB = HIREF
		self.obj_data.HILTB = HIREF
		self.obj_data.HIRTB = HIREF
		self.obj_data.HIBOT = HIREF
		self.obj_data.HIFRT = HIREF
		self.obj_data.HIBCK = HIREF
		self.obj_data.HILFT = HIREF
		self.obj_data.HIRGT = HIREF
		self.obj_data.HOFTT = HOREF
		self.obj_data.HOBKT = HOREF
		self.obj_data.HOLTT = HOREF
		self.obj_data.HORTT = HOREF
		self.obj_data.HOTOP = HOREF
		self.obj_data.HOFTB = HOREF
		self.obj_data.HOBKB = HOREF
		self.obj_data.HOLTB = HOREF
		self.obj_data.HORTB = HOREF
		self.obj_data.HOBOT = HOREF
		self.obj_data.HOFRT = HOREF
		self.obj_data.HOBCK = HOREF
		self.obj_data.HOLFT = HOREF
		self.obj_data.HORGT = HOREF
		self.obj_data.HIWP = HIREF
	

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Control class for Q12 (mode 2) 
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class QCtrl_Ql2 (QCtrl_Abstract):
	def	view (self, str_cab_out, str_cycle, str_path_cab = "", str_path_cycle = ""):
		obj_view = View_Ql2(self.obj_data, str_cab_out, str_cycle, str_path_cab, str_path_cycle)
		obj_view.show_rep()
		
	def volume (self):
		obj_volume = Volume_Ql2(self.obj_data)
		obj_volume.calc_volume()

	def calculte_cycle (self):
		self.calc_cycle_start()
		
		self.obj_data.QMULN = -self.obj_data.QMUL
		self.obj_data.QFFTOT = self.obj_data.QLSIDE       + self.obj_data.QBACKL + self.obj_data.QFRNTL + self.obj_data.QLTOP  + self.obj_data.QLBTTM 	\
			+ self.obj_data.QMULN  + self.obj_data.QWFF   + self.obj_data.QGR    + self.obj_data.QSDRFF + self.obj_data.QLDRFF 		\
			+ self.obj_data.QFDRFF + self.obj_data.FFASHQ + self.obj_data.FFHEAT + self.obj_data.FFREFQ + self.obj_data.FFPENA
			
		self.obj_data.QFZTOT = self.obj_data.QRSIDE     + self.obj_data.QBACKR + self.obj_data.QFRNTR + self.obj_data.QRTOP  + self.obj_data.QRBTTM +		\
			self.obj_data.QMUL   + self.obj_data.QWFZ   + self.obj_data.QGZF   + self.obj_data.QSDRFZ + self.obj_data.QLDRFZ +		\
			self.obj_data.QFDRFZ + self.obj_data.FZASHQ + self.obj_data.FZHEAT + self.obj_data.FZREFQ + self.obj_data.FZPENA
			
		self.obj_data.QHTFF = self.obj_data.FFASHQ + self.obj_data.FFHEAT
		self.obj_data.QHTFZ = self.obj_data.FZASHQ + self.obj_data.FZHEAT
		
	def calculte (self):
		obj = Ql2(self.obj_data)
		
	def setup_vars_extra (self):
		self.obj_data.setup_vars ( 0.0,	\
			['QRTOP','QFRNTR','QDFFCS','QLTOP','QMUL']\
			)
		
		self.obj_data.setup_vars ( 0.0,	\
			['BOTTOM','FLGB','NCCTYPE','RKIN', 'RKINFF','RKINFZ','WKINR','WKIN', 'DKINFF','DKINFZ','CKMUL']\
			)
		
	def adjust_units (self):
		self.obj_data.HLGZF = Unit.WattM_BtuThHInchF (self.obj_data.HLGZF)
		self.obj_data.HLRG  = Unit.WattM_BtuThHInchF (self.obj_data.HLRG)
		
		self.obj_data.CINSUL = Unit.cm_feet (self.obj_data.CINSUL )
		self.obj_data.TIFT = Unit.cm_feet (self.obj_data.TIFT )
		self.obj_data.TIRLS = Unit.cm_feet (self.obj_data.TIRLS )
		self.obj_data.BINFRZ = Unit.cm_feet (self.obj_data.BINFRZ )
		self.obj_data.TFF = Unit.c_f (self.obj_data.TFF )
		#self.obj_data.RKIN = 0.0
		
		self.obj_data.RKINFF = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RFF )
		self.obj_data.RKINFZ = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RFRZ )
		self.obj_data.WKINR = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RWEDGER )
		self.obj_data.WKIN = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RWEDGE )
		
		self.obj_data.DKINFF = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RDRFF )
		self.obj_data.DKINFZ = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RDRFZ )
		self.obj_data.CKMUL = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RMUL )
		#-----
		self.obj_data.TIRT = Unit.cm_feet(self.obj_data.TIRT )
		
		self.obj_data.TIRF = Unit.cm_feet(self.obj_data.TIRF )
		self.obj_data.TIRB = Unit.cm_feet(self.obj_data.TIRB )		
		#---
		self.obj_data.CDUP = Unit.cm_feet(self.obj_data.CDUP )
		self.obj_data.CDDN = Unit.cm_feet(self.obj_data.CDDN )
		self.obj_data.CCHGT = Unit.cm_feet(self.obj_data.CCHGT )		
		#-----
		self.obj_data.WALL = Unit.cm_feet (self.obj_data.WALL )
		self.obj_data.THMUL = Unit.cm_feet (self.obj_data.THMUL )		
		#-----	
		self.obj_data.FFASHQ = Unit.Watt_BtuH (self.obj_data.FFASHQ )
		self.obj_data.FZASHQ = Unit.Watt_BtuH (self.obj_data.FZASHQ )
		self.obj_data.FFREFQ = Unit.Watt_BtuH (self.obj_data.FFREFQ )
		
		self.obj_data.FZREFQ = Unit.Watt_BtuH (self.obj_data.FZREFQ )
		self.obj_data.FFHEAT = Unit.Watt_BtuH (self.obj_data.FFHEAT )
		self.obj_data.FZHEAT = Unit.Watt_BtuH (self.obj_data.FZHEAT )
		
		self.obj_data.FZPENA = Unit.Watt_BtuH (self.obj_data.FZPENA )
		self.obj_data.FFPENA = Unit.Watt_BtuH (self.obj_data.FFPENA )
		
		self.obj_data.HXVUR = Unit.liter_ft3 (self.obj_data.HXVUR )
		self.obj_data.HRFFC = ( self.obj_data.SECFFC / 3600.0 ) * self.obj_data.FFCOPN
		self.obj_data.WEDGE = Unit.cm_feet (self.obj_data.WEDGE )
		self.obj_data.WEDGER = Unit.cm_feet (self.obj_data.WEDGER )
		self.obj_data.VOLAR = Unit.liter_ft3 (self.obj_data.VOLAR )
		
		self.obj_data.FLANGE = Unit.cm_feet (self.obj_data.FLANGE )
		self.obj_data.FLANGER = Unit.cm_feet (self.obj_data.FLANGER )
		self.obj_data.FLGB = self.obj_data.FLANGER

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Control class for Q113 (mode 1,3) 
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class QCtrl_Ql13 (QCtrl_Abstract):
	def	view (self, str_cab_out, str_cycle, str_path_cab = "", str_path_cycle = ""):
		obj_view = View_Ql13(self.obj_data, str_cab_out, str_cycle, str_path_cab, str_path_cycle)
		obj_view.show_rep()

	def volume (self):
		obj_volume = Volume_Ql13(self.obj_data)
		obj_volume.calc_volume()
				
	def calculte (self):
		obj = Ql13(self.obj_data)
		
	def calculte_cycle (self):
		self.calc_cycle_start()
		
		self.obj_data.QMULN = -self.obj_data.QMULI
		self.obj_data.QFFTOT = self.obj_data.QRRSID       + self.obj_data.QRLSID + self.obj_data.QRBACK + self.obj_data.QRFRNT + self.obj_data.QBOTTM \
			+ self.obj_data.QMULN  + self.obj_data.QWFF   + self.obj_data.QGR    + self.obj_data.QSDRFF + self.obj_data.QFDRFF 				\
			+ self.obj_data.QLDRFF + self.obj_data.FFASHQ + self.obj_data.FFHEAT + self.obj_data.FFREFQ + self.obj_data.FFPENA

		self.obj_data.QFZTOT = self.obj_data.QFRSID       + self.obj_data.QFLSID + self.obj_data.QFBACK + self.obj_data.QFFRNT + self.obj_data.QTOP 	\
			+ self.obj_data.QMULI  + self.obj_data.QWFZ   + self.obj_data.QGZF   + self.obj_data.QSDRFZ + self.obj_data.QLDRFZ 			\
			+ self.obj_data.QFDRFZ + self.obj_data.FZASHQ + self.obj_data.FZHEAT + self.obj_data.FZREFQ + self.obj_data.FZPENA

		self.obj_data.QHTFF = self.obj_data.FFASHQ + self.obj_data.FFHEAT
		self.obj_data.QHTFZ = self.obj_data.FZASHQ + self.obj_data.FZHEAT
		
	def setup_vars_extra (self):
		self.obj_data.setup_vars ( 0.0,	\
			['QMUL','FLGB','NCCTYPE', 'RKIN', 'RKINFF','RKINFZ','WKINR','WKIN', 'DKINFF','DKINFZ','CKMUL']\
			 )
		
	def adjust_units (self):
		self.obj_data.HLFZG = Unit.WattM_BtuThHInchF ( self.obj_data.HLFZG )
		self.obj_data.HLRG  = Unit.WattM_BtuThHInchF ( self.obj_data.HLRG  )
		
		self.obj_data.TFF = Unit.c_f (self.obj_data.TFF )
		self.obj_data.TIRRS = Unit.cm_feet (self.obj_data.TIRRS )
		self.obj_data.TIRLS = Unit.cm_feet (self.obj_data.TIRLS )
		self.obj_data.TIFLS = Unit.cm_feet (self.obj_data.TIFLS )
		self.obj_data.TIFT = Unit.cm_feet (self.obj_data.TIFT )
		
		#self.obj_data.RKIN = 0.0
		self.obj_data.CINSUL = Unit.cm_feet (self.obj_data.CINSUL )
		
		self.obj_data.RKINFF = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RFF )
		self.obj_data.RKINFZ = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RFRZ )
		self.obj_data.WKINR = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RWEDGER )
		self.obj_data.WKIN = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RWEDGE )
		
		self.obj_data.DKINFF = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RDRFF )
		self.obj_data.DKINFZ = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RDRFZ )
		self.obj_data.CKMUL = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RMUL )
		#-----
		self.obj_data.FFASHQ = Unit.Watt_BtuH (self.obj_data.FFASHQ )
		self.obj_data.FZASHQ = Unit.Watt_BtuH (self.obj_data.FZASHQ )
		self.obj_data.FFREFQ = Unit.Watt_BtuH (self.obj_data.FFREFQ )
		
		self.obj_data.FZREFQ = Unit.Watt_BtuH (self.obj_data.FZREFQ )
		self.obj_data.FFHEAT = Unit.Watt_BtuH (self.obj_data.FFHEAT )
		self.obj_data.FZHEAT = Unit.Watt_BtuH (self.obj_data.FZHEAT )
		
		self.obj_data.FFPENA = Unit.Watt_BtuH (self.obj_data.FFPENA )
		self.obj_data.FZPENA = Unit.Watt_BtuH (self.obj_data.FZPENA )
		
		self.obj_data.HRFFC = ( self.obj_data.SECFFC / 3600.0 ) * self.obj_data.FFCOPN
		
		self.obj_data.WEDGE = Unit.cm_feet (self.obj_data.WEDGE )
		self.obj_data.WEDGER = Unit.cm_feet (self.obj_data.WEDGER )
		
		self.obj_data.FLANGE = Unit.cm_feet (self.obj_data.FLANGE )
		self.obj_data.FLANGER = Unit.cm_feet (self.obj_data.FLANGER )
		
		self.obj_data.TOPMUL = Unit.cm_feet (self.obj_data.TOPMUL )
		self.obj_data.THMUL = Unit.cm_feet (self.obj_data.THMUL )

		self.obj_data.HXVUR = Unit.liter_ft3 (self.obj_data.HXVUR )

		self.obj_data.TIRF = Unit.cm_feet(self.obj_data.TIRF )
		self.obj_data.TIRB = Unit.cm_feet(self.obj_data.TIRB )
		self.obj_data.VOLAR = Unit.liter_ft3 (self.obj_data.VOLAR )
		
		self.obj_data.CDUP = Unit.cm_feet (self.obj_data.CDUP )
		self.obj_data.CDDN = Unit.cm_feet (self.obj_data.CDDN )
		self.obj_data.CCHGT = Unit.cm_feet (self.obj_data.CCHGT )
		
		self.obj_data.FLGB = self.obj_data.FLANGER
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Control class for Q1467 (mode 4,7) 
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class QCtrl_Ql467 (QCtrl_Abstract):
	def	view (self, str_cab_out, str_cycle, str_path_cab = "", str_path_cycle = ""):
		if self.obj_data.NMOD == 7:
			obj_view = View_Ql7(self.obj_data, str_cab_out, str_cycle, str_path_cab, str_path_cycle)
		else:
			obj_view = View_Ql4(self.obj_data, str_cab_out, str_cycle, str_path_cab, str_path_cycle)
			
		obj_view.show_rep()

	def volume (self):
		if self.obj_data.NMOD==4:
			obj_volume = Volume_Ql4(self.obj_data)
		else:
			obj_volume = Volume_Ql7(self.obj_data)
			
		obj_volume.calc_volume()
		
	def calculte (self):
		obj = Ql467(self.obj_data)
		
	def calculte_cycle (self):
		self.calc_cycle_start()
		self.calc_cycle_Mode57()
		
	def setup_vars_extra (self):
		# TFF was add, it is found in Config.6(mode 4), and not found in Config.5(mode7)
		# To prevent var is not found error in Ql456 with deal with both mode.
		self.obj_data.setup_vars ( 0.0,\
		['BOTTOM','FLGB','NCCTYPE',  'RKIN', 'TKIN', 'HIWP','TFF']\
			 )
		self.obj_data.setup_vars ( 0.0,['FH','FW','FD','RTOP','VOLAR','FFCOPN','HRFFC'])
		
	def adjust_units (self):
		self.obj_data.TIFLS = Unit.cm_feet (self.obj_data.TIFLS )
		self.obj_data.TIFT = Unit.cm_feet (self.obj_data.TIFT )
		
		self.obj_data.CINSUL = Unit.cm_feet (self.obj_data.CINSUL )
		self.obj_data.FH = Unit.cm_inch (self.obj_data.FH )
		self.obj_data.FW = Unit.cm_inch (self.obj_data.FW )
		self.obj_data.FD = Unit.cm_inch (self.obj_data.FD )
		
		self.obj_data.CDUP = Unit.cm_feet(self.obj_data.CDUP )
		self.obj_data.CDDN = Unit.cm_feet(self.obj_data.CDDN )
		
		self.obj_data.RKIN = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RCAB )
		self.obj_data.WKIN = 1.0/Unit.BtuHrFtF_CmWattF2K (self.obj_data.RWEDGE) 
		self.obj_data.DKIN = 1.0/Unit.BtuHrFtF_CmWattF2K (self.obj_data.RDOOR) 
		
		self.obj_data.RCAB = 1.0 / ( self.obj_data.RKIN *12)
		self.obj_data.RWEDGE = 1.0 / ( self.obj_data.WKIN *12)
		self.obj_data.RDOOR = 1.0 / ( self.obj_data.RKIN *12)
		
		self.obj_data.HLFZG = Unit.WattM_BtuThHInchF (self.obj_data.HLFZG )
		#---
		self.obj_data.FZHEAT = Unit.Watt_BtuH (self.obj_data.FZHEAT )
		self.obj_data.FZASHQ = Unit.Watt_BtuH (self.obj_data.FZASHQ )
		self.obj_data.FZREFQ = Unit.Watt_BtuH (self.obj_data.FZREFQ )
		self.obj_data.FZPENA = Unit.Watt_BtuH (self.obj_data.FZPENA )

		self.obj_data.FW = Unit.cm_feet(self.obj_data.FW )
		self.obj_data.FD = Unit.cm_feet(self.obj_data.FD )
		self.obj_data.FH = Unit.cm_feet(self.obj_data.FH )

		self.obj_data.TFF = Unit.c_f (self.obj_data.TFF )
		self.obj_data.WEDGE = Unit.cm_feet (self.obj_data.WEDGE )
		self.obj_data.FLANGE = Unit.cm_feet (self.obj_data.FLANGE )
		self.obj_data.FLGB = self.obj_data.FLANGE
		
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			:  Control class for Q15 (mode 5) 
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class QCtrl_Ql5 (QCtrl_Abstract):
	def	view (self, str_cab_out, str_cycle, str_path_cab = "", str_path_cycle = ""):
		obj_view = View_Ql5(self.obj_data, str_cab_out, str_cycle, str_path_cab, str_path_cycle)
		obj_view.show_rep()

	def volume (self):
		obj_volume = Volume_Ql5(self.obj_data)
		obj_volume.calc_volume()
		
	def calculte (self):
		obj = Ql5(self.obj_data)
		
	def calculte_cycle (self):
		self.calc_cycle_start()
		
		self.obj_data.QBTTM  = self.obj_data.QBOTTM       + self.obj_data.QSH    + self.obj_data.QSV
		self.obj_data.QFZTOT = self.obj_data.QRSIDE       + self.obj_data.QLSIDE + self.obj_data.QBACK  + self.obj_data.QFRONT + self.obj_data.QTOP 	\
			+ self.obj_data.QBTTM  + self.obj_data.QGZF   + self.obj_data.QSDRFZ + self.obj_data.QLDRFZ + self.obj_data.QFDRFZ 			\
			+ self.obj_data.FZASHQ + self.obj_data.FZHEAT + self.obj_data.FZREFQ + self.obj_data.FZPENA
			
		self.obj_data.QHTFZ  = self.obj_data.FZASHQ + self.obj_data.FZHEAT
		
		self.calc_cycle_Mode57()

	def setup_vars_extra (self):
		self.obj_data.setup_vars ( 0.0,	\
			['RKIN', 'TKIN', 'HIWP']\
			 )
		self.obj_data.setup_vars ( 0.0,	\
			['QW','VOLAR','FFCOPN','HRFFC'] )
	
	def adjust_units (self):
		self.obj_data.TIFT = Unit.cm_feet (self.obj_data.TIFT )
		self.obj_data.CINSUL = Unit.cm_feet (self.obj_data.CINSUL )
		
		self.obj_data.CWIDE = Unit.cm_feet (self.obj_data.CWIDE )
		self.obj_data.CHGT = Unit.cm_feet (self.obj_data.CHGT )
		
		self.obj_data.SCIN = Unit.cm_feet (self.obj_data.SCIN )
		self.obj_data.STIN = Unit.cm_feet (self.obj_data.STIN )
		
		self.obj_data.TFF = self.obj_data.TFRZ
		self.obj_data.TIFLS = self.obj_data.TIFRS
		
		self.obj_data.RKIN = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RCAB )
		self.obj_data.TKIN = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RTOP )
		self.obj_data.RCAB = 1.0 / ( self.obj_data.RKIN *12)
		#---
		self.obj_data.FZASHQ = Unit.Watt_BtuH (self.obj_data.FZASHQ )
		self.obj_data.FZREFQ = Unit.Watt_BtuH (self.obj_data.FZREFQ )
		self.obj_data.FZHEAT = Unit.Watt_BtuH (self.obj_data.FZHEAT )
		self.obj_data.FZPENA = Unit.Watt_BtuH (self.obj_data.FZPENA )
		#---
		#self.obj_data.HIWP = 0.0
		self.obj_data.HLFZG = Unit.WattM_BtuThHInchF ( self.obj_data.HLFZG )
		
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Control class for Q18 (mode 8)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
class QCtrl_Ql8 (QCtrl_Abstract):
	def	view (self, str_cab_out, str_cycle, str_path_cab = "", str_path_cycle = ""):
		obj_view = View_Ql8(self.obj_data, str_cab_out, str_cycle, str_path_cab, str_path_cycle)
		obj_view.show_rep()
		
	def volume (self):
		obj_volume = Volume_Ql8(self.obj_data)
		obj_volume.calc_volume()
						
	def calculte (self):
		obj = Ql8(self.obj_data)
		
	def calculte_cycle (self):
		self.calc_cycle_start()
		
		self.obj_data.QMULN = -self.obj_data.QMUL
		
		self.obj_data.QFFTOT = self.obj_data.QRRSID       + self.obj_data.QRLSID + self.obj_data.QRBACK + self.obj_data.QRFRNT + self.obj_data.QTOP +		\
			+ self.obj_data.QMULN  + self.obj_data.QWFF   + self.obj_data.QGR    + self.obj_data.QSDRFF + self.obj_data.QLDRFF +					\
			+ self.obj_data.QFDRFF + self.obj_data.FFASHQ + self.obj_data.FFHEAT + self.obj_data.FFREFQ + self.obj_data.FFPENA

		self.obj_data.QFZTOT = self.obj_data.QFRSID          + self.obj_data.QFLSID + self.obj_data.QFBACK + self.obj_data.QFFRNT + self.obj_data.QBOTTM 	\
			   + self.obj_data.QBCOMP + self.obj_data.QMUL   + self.obj_data.QWFZ   + self.obj_data.QGZF   + self.obj_data.QSDRFZ 		\
			   + self.obj_data.QLDRFZ + self.obj_data.QFDRFZ + self.obj_data.FZASHQ + self.obj_data.FZPENA + self.obj_data.FZHEAT 			\
			   + self.obj_data.FZREFQ

		self.obj_data.QHTFF = self.obj_data.FFASHQ + self.obj_data.FFHEAT
		self.obj_data.QHTFZ = self.obj_data.FZASHQ + self.obj_data.FZHEAT
		
	def setup_vars_extra (self):
		self.obj_data.setup_vars ( 0.0,	\
			['QBCOMP','FLGB','NCCTYPE', 'RKIN', 'RKINFF','RKINFZ','WKINR','WKIN', 'DKINFF','DKINFZ','CKMUL']\
			 )
	
	def adjust_units (self):
		
		#self.obj_data.RKIN = 0.0
		self.obj_data.RKINFF = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RFF )
		self.obj_data.RKINFZ = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RFRZ )
		self.obj_data.WKINR = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RWEDGER )
		self.obj_data.WKIN = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RWEDGE )
				
		self.obj_data.DKINFF = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RDRFF )
		self.obj_data.DKINFZ = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RDRFZ )
		self.obj_data.CKMUL = 1.0 / Unit.BtuHrFtF_CmWattF2K( self.obj_data.RMUL )
		
		self.obj_data.CDUP = Unit.cm_feet(self.obj_data.CDUP )
		self.obj_data.CDDN = Unit.cm_feet(self.obj_data.CDDN )
		
		self.obj_data.HLGZF = Unit.WattM_BtuThHInchF ( self.obj_data.HLGZF )
		self.obj_data.HLRG  = Unit.WattM_BtuThHInchF ( self.obj_data.HLRG  )
		self.obj_data.HXVUR = Unit.liter_ft3 (self.obj_data.HXVUR )
		#-----
		self.obj_data.FFPENA = Unit.Watt_BtuH (self.obj_data.FFPENA )
		self.obj_data.FZPENA = Unit.Watt_BtuH (self.obj_data.FZPENA )
		
		self.obj_data.FFASHQ = Unit.Watt_BtuH (self.obj_data.FFASHQ )
		self.obj_data.FZASHQ = Unit.Watt_BtuH (self.obj_data.FZASHQ )
		self.obj_data.FFREFQ = Unit.Watt_BtuH (self.obj_data.FFREFQ )
		
		self.obj_data.FZREFQ = Unit.Watt_BtuH (self.obj_data.FZREFQ )
		self.obj_data.FFHEAT = Unit.Watt_BtuH (self.obj_data.FFHEAT )
		self.obj_data.FZHEAT = Unit.Watt_BtuH (self.obj_data.FZHEAT )

		#-----------
		self.obj_data.WEDGE = Unit.cm_feet (self.obj_data.WEDGE )
		self.obj_data.WEDGER = Unit.cm_feet (self.obj_data.WEDGER )
		
		self.obj_data.TIRLS = Unit.cm_feet (self.obj_data.TIRLS )
		self.obj_data.TIRF = Unit.cm_feet(self.obj_data.TIRF )
		self.obj_data.TIRB = Unit.cm_feet(self.obj_data.TIRB )	
		
		self.obj_data.FLANGE = Unit.cm_feet (self.obj_data.FLANGE )
		self.obj_data.FLANGER = Unit.cm_feet (self.obj_data.FLANGER )
		
		self.obj_data.TOPMUL = Unit.cm_feet (self.obj_data.TOPMUL )
		self.obj_data.THMUL = Unit.cm_feet (self.obj_data.THMUL )
		
		self.obj_data.TIRT = Unit.cm_feet(self.obj_data.TIRT )
		self.obj_data.VOLAR = Unit.liter_ft3 (self.obj_data.VOLAR )
		self.obj_data.HRFFC = ( self.obj_data.SECFFC / 3600.0 ) * self.obj_data.FFCOPN
		self.obj_data.TFF = Unit.c_f (self.obj_data.TFF )
		
		self.obj_data.FLGB = self.obj_data.FLANGER 
		self.obj_data.TIFLS = self.obj_data.TIFRS
		self.obj_data.TIRRS = self.obj_data.TIRLS
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
