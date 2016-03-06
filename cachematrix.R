## Coursera R Programming Assignment 2 - Ksnyder
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix.  These two functions create a
## special vector that stores a matrix and caches its inverse

## The makeCacheMatrix function below creates a special matrix object that can cache
## its inverse. It requires that the specified matrix is invertable.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
       
## The cacheSolve function below computes the inverse of the special matrix from
## MakeCacheMatrix above.  It first checks to see if the inverse has already been
## calculated and returns the inverse from cache instead of recalculating.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
