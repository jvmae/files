## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  #set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  #get the value of the  matrix
  get <- function() x
  #set the value of the inverse
  setInverse <- function(inverse) inv<<- inverse
  
  #get the value of the inverse Matrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
  ## Evaluates if data is already cached
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <-x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv  }