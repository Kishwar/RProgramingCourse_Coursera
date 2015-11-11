# Caches are commonly used both in hardware and software to speed-up the process.
# There are thousands of efficient methods to use cache mechanism. In following
# functions we will see one of them. Following functions use setters and getters 
# to perform caching operation. setters and getters are one of the basic mechanism
# to let other people change your private function variables which are not accessable
# directly from outside world (out side of the function).

# Following function reacts a class in any OOP language. It has getters and setters to 
# let user play around with its private variables. How to play and what should be the sequence
# is all designed by this function/class.
# Following (class) object has following methods/function/function-objects
# - SetMatrix  = Setter for New Matrix object
# - GetMatrix  = Getter for Matrix object for which inv was calculated.
# - SetInverse = Setters for inverse of Matrix object.
# - GetInverse = Getter for inverse of Matrix object.
# - @Param  - Class object take matrix object when instantiated.
# - @return - Returns list of its functions/methods/fnc-objects 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # lets set inverse variable object to NULL when class is instantiated.
    
    # Call this method when matrix is changed. This method will udpate the object
    # for which inv will be calculated.
    # NOTE: It is always a good idea to call GetMatrix before calling SetMatrix to check
    #       if the current matrix has same values. In case values are same.. there is no need
    #       to call SetMatrix hence no recalculation of inverse.
    SetMatrix <- function(y) {
        x <<- y      # set x of parent to y. This will change x, if any.
        inv <<- NULL # as x has been changed, we need to nulify inv because x is new now.
    }
    
    # Returns current matrix object for which inv is calculated. use GetInverse to get inverse
    # for current matrix object.
    # NOTE: SetInverse should be called at any time otherwise GetInverse will return NULL
    GetMatrix <- function() {
        return (x)
    }
    
    # Function sets the inverse to the inv object of this class. This is setter for inv.
    # NOTE: Make sure inv is for current matrix object. This method doesn't calculate 
    #       inverse hence it is responsiblity of caller to set good matrix using SetMatrix
    #       and set good inverse of it using SetInverse.
    #       User can calculate inverse of matrix using solve(X).
    SetInverse <- function(z) {
        inv <<- z   # set the inv object with inverse of the matrix.
    }
    
    # Function to get inverse of currently set Matrix object.
    # NOTE: In case SetInverse was not called, this function will return NULL.
    #       It is always a good indea to call functions in following pattern.
    #       1. GetMatrix >> returns Matrix object to be checked against user matrix object
    #       2. SetMatrix >> Skip if step1 is true (same matrix object) otherwise set the Matrix object
    #       3. GetInverse >> In case matrix is same as intended, check if any inverse is stored before.
    #          In case GetInverse gives NULL. Do step 4 otherwise object has already inverse.
    #       4. SetInverse >> Costly step at the end. Calculate inverse of matrix because step 2 said two 
    #          matrix objects are different. and SetInverse.
    GetInverse <- function() {
        return (inv)
    }
    
    #lets return the list of methods/fnc/fnc-objects to caller who instantiated it.
    list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, 
         SetInverse = SetInverse, GetInverse = GetInverse)
}


# This function is like unit testing for makeCacheMatrix class. This function follows the guide lines provided
# by makeCacheMatrix to execute its sequence. Mainly it does following things -
# 1. Checks if any inverse previously calculated. (step3 of makeCacheMatrix guide lines)
# 2. Case1: Inv was previously calculated and set.
#    Case2: Inv was not previously cacluated hence caluculate inverse and set it for future.
# @Param  - List object containing methods names to be called over x class
# @return - Inverse of matrix previously set. (NOTE: Matrix setting is out of scope of this function) 
cacheSolve <- function(x, ...) {
    
    #1. lets first get inverse
    inv <-  x$GetInverse()
    
    #2. lets check if inv is not NULL
    if(!is.null(inv)) {
        #CASE1:
        # We are here because inv is not null that means cache was there..
        # Lets return inv of x object Matrix object.
        message("This value is from cache...")
        return (inv)
    }
    
    #CASE2:
    # we didn't find value in cache.. lets calculate inverse
    # a. get the matrix first
    Mat <- x$GetMatrix()
    
    # b. calculate inverse
    inv <- solve(Mat)
    
    # c. set this to inv of x class object
    x$SetInverse(inv)
    
    #d. return inverse
    return (inv)
}
