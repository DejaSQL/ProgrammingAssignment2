
## The 2 functions below are used in unison to calculate the inverse of a matrix
##  and to store it in a variable for later retrieval.


## makeCacheMatrix() accepts a matrix and assigns that to a variable 'matr'; 
##  the variable 'invr' is instantiated but not set. makeCacheMatrix() then 
##  provides a set of 4 functions that can be leveraged by the cacheSolve(). 
makeCacheMatrix <- function(x = matrix()) {
        # cache supplied matrix into variable "matr"  
        matr <- x
        invr <- NULL
        
        # provide function to cache supplied matrix into variable "matr"   
        set <- function(y) {
                matr <<- y
                invr <<- NULL
        }
        
        # provide function to return cached matrix
        get <- function() matr
        
        # provide function to cache inverse of the matrix into variable "invr" 
        setInverse <- function(solve) invr <<- solve
        
        # provide function to return the cached inverse of the matrix
        getInverse <- function() invr
        
        # generate list of functions for use in other functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##  It is cacheSolve() that evaluates 'invr' to determine whether an inverse of 
##  the matrix has been calculated & stored . The absence of a value for 'invr'
##  indicates that it has either never been calculated or the matrix supplied 
##  has changed. Either case requires a calculation to be performed by running 
##  solve() against the matrix.
cacheSolve <- function(x, ...) {
        
        # locally store inverse of the matrix cached through makeCacheMatrix
        local_invr <- x$getInverse()
        
        # The varible returned by x$getInverse() defaults to a NULL value when 
        #  makeCacheMatrix() is used. Only when cacheSolve() is called does 
        #  x$getInverse() get a non-NULL value. So, if the matrix passed to 
        #  makeCacheMatrix() is changed, the value returned by x$getInverse()
        #  reset to NULL. This section tests for non-NULL values; if a value 
        #  exists it is returned. Otherwise the rest of the function is run 
        #  and 
        if (!is.null(local_invr) )
        {
                message("getting cached data")
                return(local_invr)
        }
        
        # get the currently stored matrix, calc the inverse, store that into 
        #  the variable 'invr' fo future use, and return the results
        matr <- x$get()
        message("calc inverse")
        invr <- solve(matr, ...)
        x$setInverse(invr)
        return(invr)
}

