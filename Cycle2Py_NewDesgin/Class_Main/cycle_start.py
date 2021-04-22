# Python Import ==================

# User Import ======================
from cycle_classes.Start import Start

lng_width = 50


def print_fixed_width(str_data):
    str_extenstion = ""
    if len(str_data) < lng_width:
        str_extenstion = " " * (lng_width - len(str_data))

    print("|" + str_data + str_extenstion + "|")
    

obj_start = Start()

obj_start.main()

print("\n")
print_fixed_width("=" * lng_width)
print_fixed_width("     Cycle. App Done Succufully")
print_fixed_width("     was create on current directory ")

print_fixed_width(" ")
print_fixed_width(" ")

print_fixed_width("     Input File: " + Start.FILE_CYC_INPUT)
print_fixed_width("     Output File: " + Start.FILE_CYC_OUTPUT)
print_fixed_width(" ")

print_fixed_width("     Configration: " + str(obj_start.dt.IRFTYP))
print_fixed_width("=" * lng_width)
