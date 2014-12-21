## Create two functions, one that creates setter and getter functions
## for an invertible matrix and caches its data, another that returns
## the matrix's inverse by either checking the cache, or solving it
## storing it in the cache, and the returning it.

## makeCacheMatrix takes a matrix as an argument, stores its value and
## its inverse, and returns a list of setter and getter functions

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    # Set and get functions for matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x

    # Set and get functions for inverse of matrix
    setInverse <- function(calculated_inverse) {
        inverse <<- calculated_inverse
    }
    getInverse <- function() inverse

    # Return list of functions
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes in a cached matrix (the list created in
## makeCacheMatrix) and returns the inverse by either retrieving it from
## the cache, or solving it, setting it in the cache, and then returning it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # First, get inverse and check if its null...
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        # If it isn't null, return the index
        message("Getting cached data...")
        return(inverse)
    }

    # Otherwise, calculate the inverse, set it, and return it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
