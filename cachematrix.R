## This pair of methods allows the user to cache the inverse of a matrix for quick repeated use.

## makeCacheMatrix stores a matrix and its inverse and supplies getters and setters for both.
makeCacheMatrix <- function(cached_matrix = matrix()) {
  inverse <- NULL
  set <- function(new_matrix) {
    cached_matrix <<- new_matrix
    inverse <<- NULL
  }
  get <- function() cached_matrix
  set_inverse <- function(new_inverse) inverse <<- new_inverse
  get_inverse <- function() inverse
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve returns the inverse of the matrix stored in the cached matrix object.
## If the inverse is not yet cached, the function solves the matrix and caches the inverse.
## If the inverse is cached, the function returns the cached value.
cacheSolve <- function(cached_matrix, ...) {
  inverse <- cached_matrix$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- cached_matrix$get()
  inverse <- solve(data, ...)
  cached_matrix$set_inverse(inverse)
  inverse
}
