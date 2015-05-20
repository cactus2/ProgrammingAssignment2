## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a vector of type list, composed of pairs of names and associated functions.
## These functions can be individually called, to operate against the matrix named x

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {                                            # a function that can replace the original
        x <<- y                                                     # matrix (x) with a new one (y)
        inv <<- NULL
    }
    get <- function() {                                             # obtain the input matrix
        x
    }
    setinv <- function(invm) {                                      # place inverse of matrix into cache
        inv <<- invm
    }
    getinv <- function() {                                          # retrieve the inverse from cache
        inv
    }    
    list(set = set, get = get, setinv = setinv, getinv = getinv)    # return list of functions for latter use
}


## cacheSolve uses the functions created above.  When first run against the data instance of x,
## it creates a cached inverse of x.  Future uses of cacheSolve will find the inverse cached
## and therefore return the cached version without recomputation.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                           # Check the cache for inverse of x
    if(!is.null(inv)) {                         # if the inverse was cached -
        message("getting cached data")
        return(inv)                             # return the inverse and exit function
    }
    data <- x$get()                             # otherwise, put the data in 'data'
    inv <- solve(data, ...)                     # compute the inverse of the data
    x$setinv(inv)                               # call function to cache the inverse
    inv                                         # return the inverse
}


zippy <- matrix(c(-1, 0, 3, 5), 2, 2)           # create an arbitrary 2x2 matrix

drop <- makeCacheMatrix(zippy)                  # execute makeCacheMatrix using

flop <- cacheSolve(drop)                        # execute cacheSolve based on the instance
                                                # of makeCacheMatrix using zippy as input

# flop ends up being a 2x2 matrix, with values -1, 0, 0.6, 0.2 which is the inverse of zippy