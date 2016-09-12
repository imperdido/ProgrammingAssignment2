## These set of functions are created to avoid unnecessary computation time
## for inverse matrix calculations by caching already calculated values and 
## retrieving them when called.

## makeCacheMatrix function creates the object that holds the vector and functions 
## required for caching the inversion values.

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(z) {
    x <<- z
    k <<- NULL
  }
  get <- function () x
  setInv <- function (solve) k <<- solve
  getInv <- function () k
  list (set = set, get = get, setInv = setInv, getInv = getInv )
}


## CacheSolve function retrieves the cached value of an inversed value of matrix
## if the value is already calculated.

cacheSolve <- function(x, ...) {        ## Return a matrix that is the inverse of 'x'
  k <- x$getInv()
  if (!is.null(k)) {
      message("cached data is being retrieved.")
    return (k)
  }
  output <- x$get()
  k <- solve (output, ...)
  x$setInv(k)
  k
}
