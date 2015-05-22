## These functions serve the purpose of taking a matrix and returning its inverse.
## When the inverse is calculated, the value is stored in a variable so that on subsequent
## calls, the value can just be pull from the cache instead of running calculations again.

## makeCacheMatrix creates an object to hold the inverse matrix of a matrix passed to it
## as well as a group of functions to set and get the matrix, as well as set and get
## the inverse matrix. This function returns a list of its child functions to be used
## when passed to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL;
  set_matrix <- function(y){
    x <<- y;
    inverse_matrix <<- NULL;
  }
  get_matrix <- function(){
    x;
  }
  set_inverse <- function(inv){
    inverse_matrix <<- inv;
  }
  get_inverse <- function(){
    inverse_matrix;
  }
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse, get_inverse = get_inverse);
}


## This function returns the inverse of the matrix store by makeCacheMatrix().
## If the inverse has already been calculated, it is returned from the cache
## otherwise the inverse is calculated, stored in the cache, and returned.

cacheSolve <- function(x, ...) {
  matrix <- x$get_inverse();
  if(!is.null(matrix)){
    message("getting cached data");
    return(matrix);
  }
  data <- x$get_matrix();
  matrix <- solve(data, ...);
  x$set_inverse(matrix);
  matrix;
}