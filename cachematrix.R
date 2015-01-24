## The function makeCacheMatrix creates an objects wrapper of a matrix
## The wrapper allows for caching the inverse of the matrix, which
## can be correlated with the function cacheSolve. cacheSolve either returns
## a freshly calculated inverse of the matrix or if the inverse has been cached
## already it returns the cached value. 


## This function creates an object wrapper for a matrix.
## Parameters: x - a matrix
## Return: a matrix wrapper
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <- NULL
  }
  get <- function() x
  
  getinverse <- function() inverse
  setinverse <- function(inv) inverse <<- inv
  
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}

## This function calculates the inverse of a matrix or return the cached value
## Parameters: x - a matrix wrapper
## Return: The inverse of the matrix stored in the wrapper
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message ("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  return(inverse)
}
