## Set of methods for computing and caching the inverse of square matrix

# This file contains a solution to R Programming Coursera MOOC, programming
# assignment 2. The code is based on an example provided in the assignment,
# (C) Roger D. Peng, which can be obtained through the course page in
# https://class.coursera.org/rprog-015/.

## Creates an object used for computing and caching the inverse of square matrix

# The argument for this function is a matrix. The matrix is assumed to be
# invertible. Using non-invertible matrix will cause error in cacheSolve
# function.

# The returned objects is not a matrix but rather a set of functions for
# manipulating the matrix and cache. The object is meant to be manipulated
# using cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Applies solve to a matrix and caches the result

# The argument for this functions is a cache matrix object created using
# makeCacheMatrix function, plus any additional arguments to solve function.
# If the result has already been calculated, just returns the cached result.
# Otherwise the function will calculate the inverse matrix of the original
# matrix using solve function, and cache the result. The inverse matrix will
# be returned.

# This function will make no check if the cache matrix object contains an
# invertible matrix. Rather, errors caused by solve function will be
# propagated.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
