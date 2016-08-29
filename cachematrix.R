## Function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
## Next function, called 'cacheSolve', computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.



## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inv_matrix) inv <<- inv_matrix
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## Computes the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$set_inv(inv)
  inv
}
