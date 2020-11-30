# Python Import ==================

# User Import ======================
from read_comp_file import *


# ================== Test Driver ========
strFile_name = "DG73C12RAU6.cmp"  # File name

print("\n\n==Test read_comp_file only IMAP=0 ................")
lstRes = read_comp_file(strFile_name=strFile_name)

print("  IUNITS   ", lstRes[0])
print("...............................")

print("  TEDATA   ", lstRes[1])
print("...............................")

print("  TCDATA   ", lstRes[2])
print("...............................")

print("  CAPAC    ", lstRes[3])
print("...............................")

print("  POWER    ", lstRes[4])
