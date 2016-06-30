## This function gets a matrix and creates a special list for it.Each element of the list is a function.This list is used
## to save a matrix and its inverse. Then the function checks whether the inverse of 
## the matrix has already been calculated. If it has been, the function just retuns the inverse matrix but if it
## has not been calculated, the function calculates the inverse matrix, saves it in cache, and returns the inverse matrix.

## This function gets a matrix and creats a list, elements of which are functions to store the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inver) m <<- inver
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function operates on the output of previous function. It checks whether the inverse of the matrix has been calculated or not. 
## If it has been calculated, the function returns the saved inverse matrix. 
## Otherwise, it caculates the inverse matrix, saves it, and returns the inverse matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    return(m)
}
