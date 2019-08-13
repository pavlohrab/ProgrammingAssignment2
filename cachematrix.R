
## The following is a pair of functions that cache and compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  inv <- NULL
  set <- function(x) {
     inv <<- NULL;matrix <<- x;
  }
  get <- function() return(matrix);
  setinv <- function(invr) inv <<- invr;
  getinv <- function() return(inv);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(matrix, ...) {
  inv <- matrix$getinv()
  if(!is.null(inv)) {
    message("Retrieving cached data...")
    return(inv)
  }
  data <- matrix$get()
  invserse <- solve(data, ...)
  matrix$setinv(inv)
  return(inv)
}
