## These functions are used to create a special object that stores a matrix and
## cache's its inverse.

## makeCacheMatrix creates a special matrix. It is essentially a list that 
## contains functions to (1) set the value of a matrix, (2) get the value of the
## matrix, (3) set of the value of the inverse, and (4) get the value of the 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special matrix created from the 
## above function. It first checks if the inverse was already calculated. If so,
## it will get the inverse from the cache and skip the computation. Otherwise,
## it calculates the inverse of the data and sets the inverse of the data in the
## cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
