# Python_CERA

[TOC]

## General information

### **Introduction**

​	This project man target is to translate an old Fortran77 application, to a new Python language. OOP technology need to be used in the final project output. Source code for Fortran77  is included in this application documents, folder **(Source code for Fortran77)**.

### **Project target**

1. Convert an application, from old Fortran77 to Python.**
2. Translate the source-code language of CERA from Fortran to Python. This should include testing and verification of the translation to ensure that source code operates effectively under the new version.
   Deliverable: A source code of CERA converted to python 
3. Improve and change the source-code architecture to Object-Oriented Programing
   Deliverable: A source code converted to an object-oriented programming
4. Develop a simplified text input interface using simplified text editors or Microsoft Excel and a simplified output format using a comma- or tab-delimited format that can be read in
   different tools (text editors or Microsoft Excel)
   Deliverable: The text input and output interfaces using simplified text editors or Microsoft Excel completed

Cycle Solver is a solver in Python for refrigerator cycle. it contains a group of class as shown in graph:

### **Philosophy**

​	To make the output of Python application debug is more simple, the main function names and variable name will be the same in Python application. It is not normal to use caps for variable names in Python, but we used caps variables name, to be similar as the Fortran source. Methods name was set in small letters, but the same as function names in Fortran.

​	CoolProb http://www.coolprop.org/Utility utility is used as an alternative to Fortran library which do the same task.

​	Some other Python classes was created as an alternative to old dos function, for example Python FileAcccess class.

​	Fortran source has some issue, that was clear after transfer application to Python :-

- Some variables is not used

- some calculated values is not used nor printed in reports.

   

### Basic design of Python



### Input / Output entry values to Python



### Running Application

### **Application Start**

python app_start.py



![cycle_oop.png](./charts/cycle_oop.png)
![cab_oop.png](./charts/cab_oop.png)

**Solver has the following class  :-**

1. Condenser class
2. Evaporator class.
3. Compressor class.
4. CycleType class.
5. Trace class.
6. ErrorExpection class
7. CoolPrp class
8. CoolPrpUtil class
9. Other helper classes:
   1. FileAccess class
   2. Qdata class
   3. CycleModelBuilder class

## **Basic class functions**

------

### *Class: Condenser*

#### 		Description 
​		- later

#### 	Class methods 
​		- Later

------

### *Class: Evaporator*

#### 		Description 
​		- later

#### 	Class methods 
​		- Later

------

### *Class: Compressor*

#### 		Description 
​		- later

#### 	Class methods 
​		- Later

------

### *Class: CycleType*

#### 		Description 
​		- later

#### 	Class methods 
​		- Later

------

### *Class: Trace*

#### 		Description 
​		- later

#### 	Class methods 
​		- Later

------

### *Class: ErrorExpection*

#### 		Description 
​		- later

#### 	Class methods 
​		- Later

------

### *Class: CoolPrp*

#### 		Description 
​		- later

#### 	Class methods 
​		- Later

------

### *Class: CoolPrpUtil*

#### 		Description 
​		- later

#### 	Class methods 
​		- Later

------

### *Other helper classes:*

#### 		Description 
​		- later

#### 	Class methods 
​		- Later

------

## List of Cycle Solver Files

| File Name             | Approximate lines count |
| :-------------------- | :--------------------: |
| CompMap.py            | 420                  |
| Compressor.py         | 468                  |
| Condenser.py          | 1077                  |
| CoolPrp.py            | 238                  |
| CoolPrpUtil.py        | 83                  |
| CycleDataModelBuiler.py | 707                |
| CycleSolver.py        | 1290                  |
| CycleType.py          | 447                  |
| CycleUtils.py         | 893                  |
| ErrorException.py     | 20                  |
| Evaporator.py         | 834                  |
| exf4Cond_Evap.py      | 106                  |
| ShowInput.py          | 264                  |
| Start.py              | 176                  |
| Trace.py              | 196                  |
| View.py               | 645                  |
| QData.py              | 268                  |
| FileAccess.py         | 242                  |

------


