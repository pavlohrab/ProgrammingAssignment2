## Code includes two functions, that caching and then producing an inversion of a function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
  inv <- NULL
  ## Here we set matrix
  set <- function(x) {
     inv <<- NULL; matrix <<- x;
  }
  ## A method to get a matrix
  get <- function() return(matrix);
  stinv <- function(invr) inv <<- invr;
  gtinv <- function() return(inv);
  return(list(set = set, get = get, stinv = stinv, gtinv = gtinv))
}

## An above function returns cashed matrix, which is an input for "cacheSolve". If the inverse has
## already been calculated (and the matrix has not changed), then cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(matrix, ...) {
  inv <- matrix$gtinv()
  if(!is.null(inv)) {
    message("Retrieving cached data...")
    return(inv)
  }
  
  ## Get the matrix from our object
  data <- matrix$get()
  invserse <- solve(data, ...)
  
  ## Set the inverse to the object
  matrix$stinv(inv)
  return(inv)
}
