# Testing Class: Compressors

## General information

Compressor (Compressor.py) class solves the compressor module, compressor module.

### CompMap Class (Sub Class for Compressor Class)

- Function.

  * CompMap by method (compmap) which localted in (Compressor) class. and only in case of IMAP = 0 (see above)

  * CompMap function is to decode data in (*.cmp) files. every one (.cmp) has data for one compressor.

  * (*.cmp) file will be localted in (compmap) folder located in the same folder of (CompMap) class.

    

- This class inherits class (FileAccess).

- Outputs, One (cmp) file has the following information :-

  * Manufactural 

  * Model

  * ERR value

  * rpm 

  * Volt in volts.

  * Unit used

    * if unit is 1 - Capacity (btu/hr), Temperature (deg F), Mass flow (lb/hr).
    * if unit is 2 - Capacity (kcal/hr), Temperature (deg C), Mass flow (kg/hr).
    * Power in watts.

  * Type, type (1 - reciprocating; 2 - rotary)

  * Capacity. Capacity data example
  
        COND TEMP (F) 	EVAPORATING TEMPERATURE(F)
    
  |   |-40  |  -30|  -20|   10|     0|    10|
  |---|----:|----:|----:|----:|-----:|-----:|
  |70 |370.4|500.5|659.3|851.1|1080.8|1353.5|
  |80 |358.1|488.2|647.0|838.8|1068.5|1341.2|
  |90 |344.3|474.4|633.1|824.9|1054.6|1327.2|
  |100|328.9|458.9|617.7|809.4|1039.0|1311.6|
  |110|311.8|441.8|600.4|792.1|1021.6|1294.1|
  |120|292.9|422.8|581.4|772.9|1002.3|1274.6|
  |130|272.1|401.9|560.3|751.8| 980.9|1253.0|

  
  * Power Power Data example (in watts).
  
        COND TEMP(F) 	EVAPORATING TEMPERATURE (F)
    
  |    |-40 | -30| -20 | -10 |0    |   10|
  |:---|---:|---:|----:|----:|----:|----:|
  |70  |65.5|75.6| 84.8| 92.3|97.7 |100.1|
  |80  |69.1|80.9| 91.9|101.7|109.5|114.7|
  |90  |72.2|85.7|98.5|110.4|120.6 |128.6|
  |100 |74.6|89.8|104.6|118.5|131.1|141.8|
  |110 |76.4|93.3|110.0|126.0|141.0|154.2|
  |120 |77.3|96.1|114.7|132.9|150.2|166.0|
  |130 |77.3|98.1|118.7|139.0|158.6|177.0|

## Tester files

* All class methods was taken as separate functions in separate file. Example (comp.py) with will be (comp method) in (compressor) class.
* Every function will have a separate file for test. Example (comp_Test.py) to test (comp.py).
  * Inputs variable, and description, and proposed test value.
  * Output values, to approve function output.    
* Every function will have a separate file for sample output. Example (comp.txt). also could have other information to tester.
        

## Python Decoration function

Python decoration function is used to print out function inputs and outputs.
you can check source file in (decorators.py).

### Using decoration function

#### Code sample

Decoration function take one parameters, could be the string IN, OUT or ALL. to print only inputs, or outputs or both.
it is prefaded to call the basic function (the following example, function test) using named arguments, so decoration function can print argument name with its value.
or decoration function with list arguments by number. see example below.

Example of named argument

    test(arg1=3, arg3=5, arg2=7)

The following example shows how to use decoration function 

    @show_input_output ("ALL")     
    def test (arg1, arg2, arg3):
        return arg1 + arg2 + arg3
    
    print ("test(3, 6, 3) =",test(3, 6, 3))
    print ("test(3, arg3=5, arg2=7) =",test(3, arg3=5, arg2=7))

#### Sample output

    #-- Function Name: test ===============
        #-- inputs:
            arg no 1 =  3
            arg no 2 =  6
            arg no 3 =  3
        #-- Output:
            Result = 12


    test(3, 6, 3) = 12
    #-- Function Name: test ===============
        #-- inputs:
            arg no 1 =  3
            arg3 =  5
            arg2 =  7
        #-- Output:
            Result = 15


    test(3, arg3=5, arg2=7) = 15

## Help files

Another help file is required to complete the test, all those help function are not related to the basic function of the class.
Example of this is (FileAccess.py) which read/write data to external file, of (*.cmp) files which has a decoded data for compressor, also the decoration functions.

## Testing function output

Simply run xxx_Test.py function. Example :-

    python filename_Test.py

## Using Python (**unittest**) Module
Some function will be tested also using Python **unittest**. to use this :-

1. A predefined value inputs and output is given.
2. The function needs to evaluate this predefined values to pass the Tests.

	python filename_Test2.py

## Available function for test

| File Name                  |   Type   |           Test function           |      Sample output |                          Dependency | Status(Approved/Draft) |
| :------------------------- | :------: | :-------------------------------: | -----------------: | ----------------------------------: | ---------------------- |
| CompMap.py                 |  Class   |          CompMap_Test.py          |        CompMap.txt |                          FileAccess | Approved (by Ayman)    |
| read_comp_file.py          | Function |      read_comp_file_Test.py       | read_comp_file.txt |                          CompMap.py | Approved (by Ayman)    |
| CoolPrp.py                 |  Class   | CoolPrp_Test.py, CoolPrp_Test2.py |        CoolPrp.txt |                                None | Approved (by Ayman)    |
| decoration.py              | Function |        decoration_Test.py         |     decoration.txt |                                None | Approved (by Ayman)    |
| interpolation.py           | Function |       interpolation_Test.py       |  interpolation.txt |                                None | Approved (by Ayman)    |
| CompressorClass_methods.py |  method  |       func_compmap_Test.py        |   func_compmap.txt | interpolation.py, read_comp_file.py | Approved (by Omar)     |
| CompressorClass_methods.py |  method  |       func_compcall_Test.py       |  func_compcall.txt |                      compmap method | Approved (by Omar)     |
| ErrorException.py          |  Class   |      ErrorException_Test.py       | ErrorException.txt |                                None | Approved (by Ayman)    |

------

## Complete Class: Compressor

| File Name     | Type  |   Test function    |  Sample output | Dependency | Status(Approved/Draft) |
| :------------ | :---: | :----------------: | -------------: | ---------: | ---------------------- |
| Compressor.py | Class | Compressor_Test.py | Compressor.txt |  all above | Draft                  |