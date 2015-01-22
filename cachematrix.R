#     This assigment will create two functions the first which will allow the user to create 
#     a "cacheMatrix" object can manipulate stored values of the passed matrix and the inverse
#     of the matrix.
#     The second function will either retrieve the cached inverse or create, cache and return
#     the inverse
#     

    
        

makeCacheMatrix <- function(x = matrix()) {
    # This function creats an envireonment in which it stores the parameter as x and the 
    # value NULL as i (the inverse of the matris).  It also creates four functions that can maniulate the stored 
    # values
    # 
    # Args:
    #    x: a matrix,
    #
    # Returns:
    #    a list of the four functions that can be used to manipulate the values.
    #   
    
    i <- NULL
    set <- function(y) {
        # 
        # Resets the value of the current matrix to the passed parameter
        # and sets the current inverse to NULL if the parameter matrix 
        # is different from the current value of the matrix,
        # otherwise does nothing.
        #
        # Args:
        #    y: a matrix,
        #
        # Returns:
        #   
        if(!identical(x,y)) {  
            x <<- y
            i <<- NULL
        }
    }
    get <- function() x
        # 
        # Returns the the current matrix 
        #
        # Args:
        #    None
        #
        # Returns:
        #    the current matrix
        #   
    setInverse <- function(inv) i <<- inv
        # 
        # Set the the current inverse matrix 
        #
        # Args:
        #    a matrix
        #
        # Returns:
        #    
        #   
    getInverse <- function() i
        # 
        # Returns the the current inverse 
        #
        # Args:
        #    None
        #
        # Returns:
        #    the inverse matrix
        #   
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



cacheSolve <- function(x) {
    # 
    # Returns the the inverse of the matrix that is currently in the structure that
    # was used to create the input parameter.  
    # If the inverse has been cached, the cached inverse is returned, 
    # otherwise the inverse is computed, cached and then returned.
    #
    # Args:
    #    A list of funtions, the output of makeCacheMatrix
    #
    # Returns:
    #    the inverse of the matrix that was the input to the makeCacheMatrix call
    #    that made the input parametere
    # 
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}

