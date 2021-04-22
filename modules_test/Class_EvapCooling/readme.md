# Testing Class: Evaporator

## General information

Condenser(Evaporator.py) class solves the Evaporator module.

**Class has three sub-class  :-**

1. Natural Convection: use class EvapCool_FFNat, called when IFRSH= 0.
2. Cross-Flow: use class EvapCool_FFCross, called when IFRSH= 1.
3. Counter-Flow: use class EvapCool_FFCountt, called when IFRSH= 2.
4. Other common method frsh.

**Testing Function**

- Before testing function some basic parameter will be set for the class using method setParamters.

    


**Notes**

- In Evaporator_Test.py, class is called three times, one for each type of cooling, and then frsh method is called.
- Some approved and tested classes is used from the other classes, example CoolPrp.py class. 
- method exf, is found in external common class (exf4Cond_Evap), this class is used on both Condenser and Evaporator class.

## Available function for test

| File Name     | Type  |   Test function    |  Sample output |             Dependency | Status(Approved/Draft) |
| :------------ | :---: | :----------------: | -------------: | ---------------------: | ---------------------- |
| Evaporator.py | Class | Evaporator_Test.py | Evaporator.txt | method exf, CoolPrp.py | Draft                  |

------


