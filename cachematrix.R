
## makeCacheMatrix provides 4 functions: set, get, setInverse, and getInverse
## and creates a matrix object that is able to store its own inverse in a "cached" variable
## get returns the matrix that is stored.
## set decides the matrix value that will be stored and resets the inverse value.
## setInverse decides the inverse value that will be stored.
## getInverse returns the inverse value that is stored.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix created by 
## makeCacheMatrix. The generated inverse is stored in a internally scoped (cached) variable.
## If the function is subsequently called with the same arguments, the cached value will be 
## returned, rather than recalculate the inverse. Inverse calculation is computationally expensive.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}