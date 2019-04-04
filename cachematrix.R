## The following two functions allow to 
##     1. create a special object that stores a matrix and 
##     2. caches its inverse given it's invertible

## This function creates a specical matrix object. 
## Its returned value is a list of four functions to
##     set the value of the special matrix
##     get the value of the special matrix
##     set the inverse of the special matrix
##     get the inverse of the special matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x 
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function first checks if the inverse of a special matrix object (created with the above function)
##     has been calculated and cached. 
## If not, then it calculates and stores the inverse.
## Otherwise, it returns the stored inverse. 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
