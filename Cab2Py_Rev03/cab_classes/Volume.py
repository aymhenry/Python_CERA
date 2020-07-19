# Python Import ==================
from abc import ABC,abstractmethod

# User Import ======================
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
# Job 			: Abstract Class from Control Volume
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
# CABINET SECTION INTERNAL VOLUMES
	# HXVUZ is the freezer volume used for heat exchangers ( CU. FT. )
	# HXVUR is the fresh food volume used for heat exchangers ( CU. FT. )
	
	# VOLAZ is the adjusted freezer volume ( CU. FT. )
	# VOLAR is the adjusted general refrigerated volume ( CU. FT. )
class Volume (ABC):
	def __init__(self, obj_data):
		self.obj_data = obj_data
		self.DINS = ( self.obj_data.DOL + self.obj_data.DIL)/10 # change from mm to cm
		
		self.TIFRS = self.obj_data.TIFRS + self.DINS
		self.TIFF = self.obj_data.TIFF + self.DINS
		self.TIFB = self.obj_data.TIFB + self.DINS

		self.BINSUL = obj_data.BINSUL + self.DINS

		self.HXVUZ = self.obj_data.HXVUZ *1000 # change from liter to cm3
	# --------------------
	@abstractmethod
	def calc_volume (self):
		pass
	

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job 			: Volume for Configration3 (Mode 2)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Volume_Ql2(Volume):
	def calc_volume (self): 
		self.HXVUR = self.obj_data.HXVUR *1000 # change from liter to cm3
		self.TIRT = self.obj_data.TIRT + self.DINS
		self.TIRLS = self.obj_data.TIRLS + self.DINS
		self.TIFT = self.obj_data.TIFT + self.DINS
		self.TIRF = self.obj_data.TIRF + self.DINS
		self.TIRB = self.obj_data.TIRB + self.DINS
		self.CINSUL = self.obj_data.CINSUL + self.DINS
		self.BINFRZ = self.obj_data.BINFRZ + self.DINS
		
		self.obj_data.VFZ = ( ( self.obj_data.WIDTH - self.obj_data.WALL - self.obj_data.THMUL - self.TIFRS ) * ( self.obj_data.DEPTH - self.TIFF - self.TIFB ) * \
			( self.obj_data.HEIGHT - self.BINFRZ - self.TIFT ) )  - self.HXVUZ
		self.obj_data.VFF = ( self.obj_data.WALL - self.TIRLS ) * ( self.obj_data.DEPTH - self.TIRF - self.TIRB ) * 	\
			( self.obj_data.HEIGHT - self.BINSUL - self.TIRT ) - self.HXVUR

		if self.obj_data.NCCTYPE == 2:	
			BETA = math.atan( self.obj_data.CDDN / self.obj_data.CCHGT )
			HTRIAN = self.obj_data.CCHGT - self.BINSUL + self.BINSUL / math.sin( BETA )	\
				 - self.TIRB / math.tan( BETA )
			self.obj_data.VFF = self.obj_data.VFF - 0.5 * HTRIAN * HTRIAN * math.tan( BETA ) * 	\
				( self.obj_data.WALL - self.TIRLS ) 

			BETA = math.atan( self.obj_data.CDDN / self.obj_data.CCHGT )
			HTRIAN = self.obj_data.CCHGT - self.BINFRZ + self.BINFRZ / math.sin( BETA )	\
				 - tifb / math.tan( BETA )
			self.obj_data.VFZ = self.obj_data.VFZ - 0.5 * HTRIAN * HTRIAN * math.tan( BETA ) * 	\
				( self.obj_data.WIDTH - self.obj_data.WALL - self.obj_data.THMUL - self.TIFRS ) 

		if self.obj_data.NCCTYPE == 3:
			BETA = math.atan( ( self.obj_data.CDDN - self.obj_data.CDUP ) / self.obj_data.CCHGT )
			ALPHA = math.pi / 4.0 - BETA / 2.0
			UPPERL = self.obj_data.CDUP - self.TIRB+ self.BINSUL * math.tan( ALPHA )
			self.obj_data.VFF = self.obj_data.VFF - 0.5 * ( 2.0 * UPPERL + self.obj_data.CCHGT * math.tan( BETA ) ) * 	\
				self.obj_data.CCHGT * ( self.obj_data.WALL - self.TIRLS ) 

			BETA = math.atan( ( self.obj_data.CDDN - self.obj_data.CDUP ) / self.obj_data.CCHGT )
			ALPHA = math.pi / 4.0 - BETA / 2.0
			UPPERL= self.obj_data.CDUP - self.TIFB +  self.BINFRZ * math.tan( ALPHA )
			self.obj_data.VFZ = self.obj_data.VFZ - 0.5 * ( 2.0 * UPPERL+self.obj_data.CCHGT * math.tan( BETA ) ) * 	\
				self.obj_data.CCHGT * ( self.obj_data.WIDTH - self.obj_data.WALL - self.obj_data.THMUL - self.TIFRS ) 

		self.obj_data.VFZ = self.obj_data.VFZ / 1000
		self.obj_data.VFF = self.obj_data.VFF / 1000
		

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job 			: Volume for Configration 1 or 7 (Mode 3)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Volume_Ql13 (Volume):
	def calc_volume (self): 
		self.HXVUR = self.obj_data.HXVUR *1000 # change from liter to cm3
		self.TIFLS = self.obj_data.TIFLS + self.DINS
		self.TIRRS = self.obj_data.TIRRS + self.DINS
		self.TIRLS = self.obj_data.TIRLS + self.DINS
		self.TIFT = self.obj_data.TIFT + self.DINS
		self.TIRF = self.obj_data.TIRF + self.DINS
		self.TIRB = self.obj_data.TIRB + self.DINS
			
		self.CINSUL = self.obj_data.CINSUL + self.DINS
		
		self.obj_data.VFZ =   ( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) 	\
							* ( self.obj_data.DEPTH - self.TIFF - self.TIFB ) 		\
							* ( self.obj_data.TOPMUL - self.TIFT ) - self.HXVUZ
			
		self.obj_data.VFF = ( self.obj_data.DEPTH - self.TIRF - self.TIRB ) 	\
						  * ( self.obj_data.WIDTH - self.TIRLS - self.TIRRS ) 	\
						  * ( self.obj_data.HEIGHT - self.obj_data.TOPMUL - self.obj_data.THMUL - self.BINSUL ) \
							- self.HXVUR
		
		if self.obj_data.NCCTYPE == 2:
			BETA = math.atan( self.obj_data.CDDN / self.obj_data.CCHGT )
			HTRIAN = self.obj_data.CCHGT - self.BINSUL + self.BINSUL / math.sin( BETA )		\
				 - self.TIRB / math.tan( BETA )
				 
			self.obj_data.VFF = self.obj_data.VFF - 0.5 * HTRIAN * HTRIAN * math.tan( BETA ) * 		\
				( self.obj_data.WIDTH - self.TIRLS - self.TIRRS ) 

		if self.obj_data.NCCTYPE == 3:
			BETA = math.atan( ( self.obj_data.CDDN - self.obj_data.CDUP ) / self.obj_data.CCHGT )
			ALPHA = math.pi / 4.0 - BETA / 2.0
			UPPERL= self.obj_data.CDUP - self.TIRB + self.BINSUL * math.tan( ALPHA)
			self.obj_data.VFF = self.obj_data.VFF - 0.5 * ( 2.0 * UPPERL + self.obj_data.CCHGT * math.tan( BETA ) ) * 	\
				self.obj_data.CCHGT * ( self.obj_data.WIDTH - self.TIRLS - self.TIRRS ) 
		
		self.obj_data.VFZ = self.obj_data.VFZ / 1000
		self.obj_data.VFF = self.obj_data.VFF / 1000

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job 			: Volume for Configration6 (Mode 4)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Volume_Ql4(Volume):
	def calc_volume (self): 
		self.TIFLS = self.obj_data.TIFLS + self.DINS
		self.TIFT = self.obj_data.TIFT + self.DINS
		
		self.obj_data.VFZ = self.obj_data.VOLAZ
		self.obj_data.VFF = ( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) * ( self.obj_data.HEIGHT - self.BINSUL - self.TIFT ) * 	\
			( self.obj_data.DEPTH - self.TIFF - self.TIFB ) - self.obj_data.VOLAZ

		if self.obj_data.NCCTYPE == 2:
			BETA = math.atan( self.obj_data.CDDN / self.obj_data.CCHGT )
			HTRIAN= self.obj_data.CCHGT - self.BINSUL + self.BINSUL / math.sin( BETA )	\
				 - self.TIFB / math.tan( BETA )
			self.obj_data.VFF = self.obj_data.VFF - 0.5 * HTRIAN * HTRIAN * math.tan( BETA ) * 	\
				( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) 

		if self.obj_data.NCCTYPE == 3:
			BETA = math.atan( ( self.obj_data.CDDN - self.obj_data.CDUP ) / self.obj_data.CCHGT )
			ALPHA = math.pi / 4.0 - BETA / 2.0
			UPPERL= self.obj_data.CDUP - self.TIFB + self.BINSUL * math.tan( ALPHA )
			self.obj_data.VFF = sef.VFF - 0.5 * ( 2.0 * UPPERL + self.obj_data.CCHGT * TAN( BETA ) ) * 	\
					self.obj_data.CCHGT * ( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) 
		
		self.obj_data.VFZ = self.obj_data.VFZ / 1000
		self.obj_data.VFF = self.obj_data.VFF / 1000
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job 			: Volume for Configration4 (Mode 5)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Volume_Ql5(Volume):
	def calc_volume (self): 
		self.TIFT = self.obj_data.TIFT + self.DINS
		self.CINSUL = self.obj_data.CINSUL + self.DINS
		
		self.SCIN = self.obj_data.SCIN + self.DINS
		self.STIN = self.obj_data.STIN + self.DINS
		
		self.obj_data.TIFLS = self.obj_data.TIFRS # create a new var in data
		self.TIFLS = self.obj_data.TIFLS + self.DINS
		#-----
		
		self.obj_data.VFZ = ( ( ( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) * ( self.obj_data.DEPTH - self.TIFF - self.TIFB ) * \
			( self.obj_data.HEIGHT - self.BINSUL - self.TIFT ) ) - ( ( self.obj_data.DEPTH - self.TIFF - self.TIFB ) * ( self.obj_data.CWIDE - \
			( self.TIFLS + self.TIFRS ) / 2.0 ) * ( self.obj_data.CHGT - self.CINSUL ) ) ) - self.HXVUZ
		
		self.obj_data.VFZ = self.obj_data.VFZ / 1000
		self.obj_data.VFF = self.obj_data.VFF / 1000
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job 			: Volume for Configration5 (Mode 7)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Volume_Ql7(Volume):
	def calc_volume (self): 
		self.TIFLS = self.obj_data.TIFLS + self.DINS
		self.TIFT = self.obj_data.TIFT + self.DINS
		self.CINSUL = self.obj_data.CINSUL + self.DINS
		
		self.obj_data.VFZ = ( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) * ( self.obj_data.HEIGHT - self.BINSUL - self.TIFT ) * \
			( self.obj_data.DEPTH - self.TIFF - self.TIFB ) - self.HXVUZ

		if self.obj_data.NCCTYPE == 2:
			BETA = math.atan( self.obj_data.CDDN / self.obj_data.CCHGT )
			HTRIAN = self.obj_data.CCHGT - self.BINSUL + self.BINSUL / math.sin( BETA )	\
				 - self.TIFB / math.tan( BETA )
			self.obj_data.VFZ = self.obj_data.VFZ - 0.5 * HTRIAN * HTRIAN * math.tan( BETA ) * 	\
				( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) 

		if self.obj_data.NCCTYPE == 3:
			BETA = math.atan( ( self.obj_data.CDDN - self.obj_data.CDUP ) / self.obj_data.CCHGT )
			ALPHA = math.pi / 4.0 - BETA / 2.0
			UPPERL = self.obj_data.CDUP - self.TIFB + self.BINSUL * math.tan( ALPHA )
			self.obj_data.VFZ = self.obj_data.VFZ - 0.5 * ( 2.0 * UPPERL + self.obj_data.CCHGT * math.tan( BETA ) ) * 		\
				self.obj_data.CCHGT * ( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) 
		
		self.obj_data.VFZ = self.obj_data.VFZ / 1000
		self.obj_data.VFF = self.obj_data.VFF / 1000
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Job 			: Volume for Configration2 (Mode 8)
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
class Volume_Ql8(Volume):
	def calc_volume (self): 
		self.HXVUR = self.obj_data.HXVUR *1000 # change from liter to cm3	
		self.TIRT = self.obj_data.TIRT + self.DINS
		self.TIRF = self.obj_data.TIRF + self.DINS
		self.TIRB = self.obj_data.TIRB + self.DINS
		self.TIRLS = self.obj_data.TIRLS + self.DINS

		self.obj_data.VFZ = ( ( self.obj_data.HEIGHT - self.obj_data.TOPMUL - self.obj_data.THMUL - self.BINSUL ) * ( self.obj_data.WIDTH - 2.0 * self.TIFRS ) * \
			 ( self.obj_data.DEPTH - self.TIFF - self.TIFB ) ) - self.HXVUZ
		self.obj_data.VFF = ( ( self.obj_data.TOPMUL - self.TIRT ) * ( self.obj_data.WIDTH - 2.0 * self.TIRLS ) * ( self.obj_data.DEPTH - self.TIRF - self.TIRB ) )	\
			 - self.HXVUR

		if self.obj_data.NCCTYPE == 2:
			BETA = math.atan ( self.obj_data.CDDN / self.obj_data.CCHGT )
			HTRIAN = self.obj_data.CCHGT - self.BINSUL + self.BINSUL / math.sin ( BETA) - self.TIFB / math.tan ( BETA )
			self.obj_data.VFZ = self.obj_data.VFZ - 0.5 * HTRIAN * HTRIAN * math.tan ( BETA ) * 		\
				 ( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) 

		if self.obj_data.NCCTYPE == 3:
			BETA = math.atan ( ( self.obj_data.CDDN - self.obj_data.CDUP ) ) / self.obj_data.CCHGT
			ALPHA = math.pi / 4.0 - BETA / 2.0
			UPPERL = self.obj_data.CDUP - self.TIFB + self.BINSUL * math.tan ( ALPHA )
			self.obj_data.VFZ = self.obj_data.VFZ - 0.5 * ( 2.0 * UPPERL		\
				+ self.obj_data.CCHGT * math.tan ( BETA ) ) * self.obj_data.CCHGT * ( self.obj_data.WIDTH - self.TIFLS - self.TIFRS ) 
		
		self.obj_data.VFZ = self.obj_data.VFZ / 1000
		self.obj_data.VFF = self.obj_data.VFF / 1000