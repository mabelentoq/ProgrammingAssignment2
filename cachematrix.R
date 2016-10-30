## Program for caching the inverse of matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. Below functions are used to create a special object 
## that stores a matrix and caches the inverse of it.

## This function creates a special type matrix object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setInverse <- function(inv) inverse <<- inv
     getInverse <- function() inverse
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function computes the inverse of the special type of matrix created by above function makeCacheMatrix.
## Function retrives inverse from cache, in case it has been calculated already.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverse <- x$getInverse()
     if (!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     ip.matrix <- x$get()
     inverse <- solve(ip.matrix, ...)
     x$setInverse(inverse)
     inverse
}
