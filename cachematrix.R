## The makeCacheMatrix creates a matrix object that stores certain functions
## It also cache the inverse and returns the inverse from cache when invoked
## The cacheSolve function gets the inverse of matrix from cache 
## If not found it calculates the inverse and cache it.

## The following function creates a matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set_mat <- function(y){
    x <<- y
    mat_inverse <- NULL
  }
  get_mat <- function() x
  set_inverse <- function(inverse) mat_inverse <<- inverse
  get_inverse <- function() mat_inverse
  list( set_mat = set_mat, get_mat = get_mat, 
        set_inverse = set_inverse,
        get_inverse = get_inverse)
  
}


## The following function returns the inverse of matrix from cache
## or if not found then computes the inverse and stores it in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inverse <- x$get_inverse()
  if(!is.null(mat_inverse)){
    message("getting cache data")
    return(mat_inverse)
  }
  data <- x$get_mat
  mat_inverse <- solve(x, ...)
  x$set_inverse(mat_inverse)
  mat_inverse
}
