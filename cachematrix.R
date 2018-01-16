## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  IM = NULL
  set <- function(y){
    x <<- y
    IM <<- NULL
  }
  get <- function() x
  set_IM <- function(inverse) IM <<- inverse
  get_IM <- function() IM
  list(set=set, get=get, set_IM=set_IM, get_IM=get_IM)
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  IM <- x$get_IM()
  if(!is.null(IM)){
    message("getting cached data")
    return(IM)
  }
  matrix <- x$get()
  IM <- solve(matrix, ...)
  x$set_IM(IM)
  IM
}
