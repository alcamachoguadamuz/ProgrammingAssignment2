## Put comments here that give an overall description of what your
## functions do

#==================================
# Function 01: makeCacheMatrix
#==================================
# Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set=set, get=get,
       setInverse=setInverse, getInverse=getInverse)
}


#==================================
# Function 02: cacheSolve
#==================================
# Computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated, then cacheSolve should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  m <- x$get()
  i <- solve(m, ...)
  x$setInverse(i)
  i
}

