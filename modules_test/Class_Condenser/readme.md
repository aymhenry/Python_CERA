# Testing Class: Condenser

## General information

Condenser(Condenser.py) class solves the Condenser module.

**Class has three sub-class  :-**

1. Natural Convection: use class CondCool_CNat, called when ICOND = 0.
2. Cross-Flow: use class  CondCool_CCross, called when ICOND = 1.
3. Counter-Flow: use class CondCool_CCount, called when ICOND = 2.
4. Other common method cond.

**Testing Function**

- Before testing function some basic parameter will be set for the class using method setParamters.

- Some side calculation is evaluated will be given by method getExtarOutputs.

- Items USCC, UTPC, UDSC are app inputs, and is revaluated in cond method.

   

**Notes**

- In Condenser_Test.py, class is called three times, one for each type of cooling, and then cond method is called.
- Some approved and tested classes is used from the other classes, example CoolPrp.py class. 
- method exf, is found in external common class (exf4Cond_Evap), this class is used on both Condenser and Evaporator class.

## Available function for test

| File Name    | Type  |   Test function   | Sample output |             Dependency | Status(Approved/Draft) |
| :----------- | :---: | :---------------: | ------------: | ---------------------: | ---------------------- |
| Condenser.py | Class | Condenser_Test.py | Condenser.txt | method exf, CoolPrp.py | Draft                  |

------

## 
