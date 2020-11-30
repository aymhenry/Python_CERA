# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Job 			: Python Decorator, print function input and ouput
#
# Editor		: aymhenry@gmail.com
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
def show_input_output (show= None):
    SHOW_INS = "IN"
    SHOW_OUT = "OUT"
    SHOW_ALL = "ALL"
    
    if show == None or show not in[SHOW_INS, SHOW_OUT, SHOW_ALL]:
        show = SHOW_ALL
        
    def warper_fun_name (func_name):
        def wraper_args(*args, **kargs):
            ''' actual func_name logic here '''
            
            print ("#-- Function Name: " + func_name.__name__ + " ===============")
            if show in [SHOW_INS, SHOW_ALL]:
                print ("    #-- inputs: " )
                
                int_count = 1
                for arg in args:
                    print ("        arg no " + str(int_count) + " = ", arg)
                    int_count +=1

                for key, value in kargs.items():
                    print ("        " + key + " = ", value)
                    int_count +=1

            
            result = func_name(*args, **kargs)
            
            if show in [SHOW_OUT, SHOW_ALL]:
                print ("    #-- Output: " )
                print ("        Result =", result)
                print ("\n")

            return result
            
        return wraper_args
    return warper_fun_name


# @show_input_output ("ALL")     
# def test (arg1, arg2, arg3):
    # return arg1 + arg2 + arg3
    

# print ("test(3, 6, 3) =",test(3, 6, 3))
# print ("test(3, arg3=5, arg2=7) =",test(3, arg3=5, arg2=7))
