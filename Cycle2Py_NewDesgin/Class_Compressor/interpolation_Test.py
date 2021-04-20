from interpolation import *


# interplate values in data list.

x_series = [-40, -30, -20, -10, 0, 10]
y_series = [70, 80, 90, 100, 110, 120, 130]

data = [
  [370.4, 500.5, 659.3, 851.1, 1080.8, 1353.5],
  [358.1, 488.2, 647.0, 838.8, 1068.5, 1341.2],
  [344.3, 474.4, 633.1, 824.9, 1054.6, 1327.2],
  [328.9, 458.9, 617.7, 809.4, 1039.0, 1311.6],
  [311.8, 441.8, 600.4, 792.1, 1021.6, 1294.1],
  [292.9, 422.8, 581.4, 772.9, 1002.3, 1274.6],
  [272.1, 401.9, 560.3, 751.8,  980.9, 1253.0]
  ]
  

x_value = -30
y_value = 70

print("\n............... Test 1 .........................................")
print("\n............... Test 1-A1 . X less min  ........................")
interpolation (x_value=-50, y_value=y_value,
        x_series=x_series, y_series=y_series, data=data)

print("\n............... Test 1-A2 ..X more max.......................................")
interpolation (x_value=30, y_value=y_value,
        x_series=x_series, y_series=y_series, data=data)
               
print("\n............... Test 1-A3 . Y less min  ........................")
interpolation (x_value=x_value, y_value=60,
        x_series=x_series, y_series=y_series, data=data)

print("\n............... Test 1-A4 ..Y more max.......................................")
interpolation (x_value=x_value, y_value=150,
        x_series=x_series, y_series=y_series, data=data)

print("\n............... Test 1-A5 ..X,Y more max.......................................")
interpolation (x_value=x_value*10, y_value=150,
        x_series=x_series, y_series=y_series, data=data)
        
print ("\n............... Test 3 .........................................")
interpolation (x_value=x_value, y_value=y_value,
        x_series=x_series, y_series=y_series, data=data)

print ("\n............... Test 4 .........................................")
interpolation (x_value=-40, y_value=70,
        x_series=x_series, y_series=y_series, data=data)  

print ("\n............... Test 5 .........................................")
interpolation (x_value=-20, y_value=90,
        x_series=x_series, y_series=y_series, data=data)        

print ("\n............... Test 6 .........................................")
interpolation (x_value=-15, y_value=95,
        x_series=x_series, y_series=y_series, data=data)
