# Testing Class: Compressors
## General information
Compressor (Compressor.py) class solves the compressor module, compressor module is one of three type:-

1. MAP - Tabular MAP based compressor model. IMAP = 0, with is calucalted using class: Comp_MAP
2. ERR - Efficiency Model. IMAP = 1, with is calucalted using class: Comp_ERR
3. EMOD - Theoretically based model. Compressor IMAP = 2, with is calucalted using class: Comp_EMOD

All the three class inheritance the base Abstract class Comp_Abstract.
Another class Compressor class is exists to take Compressor type, and return the required class according to IMAP value.

In Fortran there was function (comp) which is used in to server two types of compressors, IMAP=1, and 2. In the futue this function will be devided to two seperate parts, but now t will be tested once after approving fuction logic.

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

## Availabile function for test
| Function      | Test function   | Sample output      | Status(Approved/Draft)  |
|:--------------|:---------------:|-------------------:|------------------------:|
| comp.py       | Test_comp.py    | Test_comp_data.txt | Draft                   |

## Testing function
Simply run Test_xxx.py function. Example :-

    python Test_comp.py
    