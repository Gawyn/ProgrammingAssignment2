## This system is divided into two functions:
## - makeCacheMatrix: Defines a matrix like object, with the ability of caching its inverse.
## - cacheSolve: Returns the inverse matrix of the matrix like object mentioned before.

## makeCacheMatrix function
## Parameters : m, where m is a matrix
##
## It returns a list of 4 functions: get, set(x), getinverse() and setinverse(x).

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL

  # The set(x) function changes the stored matrix for x.
  set <- function(y) {
    m <<- y
    i <<- NULL
  }

  # The get function returns the passed matrix.
  get <- function() m

  # The setinverse(x) function sets the cached inverse matrix as x.
  setinverse <- function(solve) i <<- solve

  # The getinverse() function returns the cached inverse matrix, if any.
  getinverse <- function() i

  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## cacheSolve function
## Parameters: x, where x is a list as the one returned by makeCacheMatrix.
## The user can also pass extra parameters that will be used in the calculation
## of the inverse.
##
## It returns the inverse of the matrix used to generate x.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()

  ## If the inverse is cached, we just return it.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  ## If the inverse is not cached, we need to calculate it and store it.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}