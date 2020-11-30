from decorators import *

@show_input_output ("ALL")     
def test (arg1, arg2, arg3):
    return arg1 + arg2 + arg3

print ("test(3, 6, 3) =",test(3, 6, 3))
print ("test(3, arg3=5, arg2=7) =",test(3, arg3=5, arg2=7))