# Testing Class: Compressors
## General information
Compressor (Compressor.py) class solves the compressor module, compressor module is one of three type:-

1. MAP - Tabular MAP based compressor model. IMAP = 0, with is calucalted using class: Comp_MAP
2. ERR - Efficiency Model. IMAP = 1, with is calucalted using class: Comp_ERR
3. EMOD - Theoretically based model. Compressor IMAP = 2, with is calucalted using class: Comp_EMOD

All the three class inherits the base Abstract class Comp_Abstract.
Another class Compressor class is exists to take Compressor type, and return the required class according to IMAP value.

In Fortran there was function (comp) which is used in to server two types of compressors, IMAP=1, and 2. In the futue this function will be devided to two seperate parts, but now t will be tested once after approving fuction logic.

### CompMap Class (Sub Class for Compressor Class)
- Function.
    * CompMap by method (compmap) which localted in (Compressor) class.
    * CompMap function is to decode data in (*.cmp) files. every one (.cmp) has data for one compressor.
    * (*.cmp) file will be localted in (compmap) folder located in the same folder of (CompMap) class.

- This class inherits class (FileAccess).

- Outputs, One (cmp) file has the following information :-
    * Manifactural 
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
					-40		-30		-20 	10		0		10
			70		370.4	500.5	659.3	851.1	1080.8	1353.5
			80		358.1	488.2	647.0	838.8	1068.5	1341.2
			90		344.3	474.4	633.1	824.9	1054.6	1327.2
			100		328.9	458.9	617.7	809.4	1039.0	1311.6
			110		311.8	441.8	600.4	792.1	1021.6	1294.1
			120		292.9	422.8	581.4	772.9	1002.3	1274.6
			130		272.1	401.9	560.3	751.8	980.9	1253.0
            
    * Power Power Data Example
    
			COMPRESSOR POWER (WATTS)
			COND TEMP(F) 	EVAPORATING TEMPERATURE (F)
					-40 	-30		-20		-10		0 		10
			70		65.5	75.6	84.8	92.3	97.7	100.1
			80		69.1	80.9	91.9	101.7	109.5	114.7
			90		72.2	85.7	98.5	110.4	120.6	128.6
			100		74.6	89.8	104.6	118.5	131.1	141.8
			110		76.4	93.3	110.0	126.0	141.0	154.2
			120		77.3	96.1	114.7	132.9	150.2	166.0
			130		77.3	98.1	118.7	139.0	158.6	177.0
    
## Tester files
* All class methods was taken as seperate functions in seperate file. Example (comp.py) with will be (comp method) in (compressor) class.
* Every function will have a seperate file for test. Example (Test_comp.py) to test (comp.py).
    * Inputs vaiable, and description, and proposed test value.
    * Output valus, to approve function output.    
* Every function will have a seperate file for sample output. Example (Test_comp_data.txt). also could have other information to tester.
        
## Python Decoration function
Python decoration function is used to print out function inputs and outputs.
you can check source file in (decorators.py).

### Using decoration function
#### Code sample
Decoration function take one paramer, could be the string IN, OUT or ALL. to print only inputs, or outputs or both.
it is prefared to call the basic function (the following example, function test) using named argments, so decoration function can print argment name with its value.
or decoration function with list argments by number. see example below.

Example of named argement

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

## Testing function
Simply run Test_xxx.py function. Example :-

    python Test_comp.py

## Availabile function for test
| Function      | Type            | Test function   | Sample output        | Status(Approved/Draft)  |
|:--------------|:---------------:|:---------------:|---------------------:|------------------------:|
| comp.py       | Function        |Test_comp.py     | Test_comp_data.txt   | Draft                   |
| CompMap.py    | Class           |Test_CompMap.py  | Test_CompMap_data.txt| Draft                   |
