# Python import

# User import

class ErrorException(Exception):
    def __init__(self, message, errors):
        # Call Exception.__init__(message)
        # to use the same Message header as the parent class
        super().__init__(message)
        self.errors = errors
        
        # Display the errors
        print ("\n=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
        print ('       Error in app Data Entry:')
        print (message)
        print ("-------------------------------\n")
        print ("Error Details\n")
        print(errors)