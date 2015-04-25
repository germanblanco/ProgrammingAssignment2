## Library cachematrix created April 25th 2015 by German Blanco
## The two functions in this library provide a matrix with
## a cached copy of it inverse matrix. In this way, the inverse
## matrix only needs to be evaluated once.

## This function creates a matrix that includes a cached copy
## of its inverse matrix.
## Parameter x is the original matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function returns the inverse of the matrix. If the
## inverse has been evaluated previously, it will return
## the cached copy of the result, otherwise it will evaluate
## the inverse matrix store the result and return it.
## This function assumes that x is invertible.
## Parameter x is the original matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
