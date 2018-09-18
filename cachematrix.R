## This function, makeCacheMatrix, creates a special "matrix" object,
## that set and cache a matrix and its inverse. It returns a list 
## containing functions to:
# - set and get the value of the matrix
# - set and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function, cacheSolve, return the inverse of the matrix 
## returned by makeCacheMatrix. If the inverse has already been calculated,
## it gets the information from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value in cache via 
## the setinverse function.
## It Assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
