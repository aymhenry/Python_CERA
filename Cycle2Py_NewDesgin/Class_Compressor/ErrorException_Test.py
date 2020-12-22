from ErrorException import ErrorException

def list_check(lst):
    if len(lst) % 2 != 0:
        raise ErrorException('Custom Message', 'Custom Error')
 
# MyException will not be raised
list_check([1, 2, 3, 4])
 
# MyException will be raised
list_check([1, 3, 5])