#This script creates 2 functions. The first function - makeCacheMatrix creates a
#special matrix that is a list of functions to set a matrix, get a matrix, set the
#inverse of a matrix and get the inverse of a matrix. The second function - cacheSolve
#returns the inverse of the special matrix created using makeCacheMatrix. This funcion
#however, first checks if the inverse of the matrix already been computed and stored
#(in a variable of this special matrix). If yes it returns the stored value(cached) else
#it computes a fresh one and also stores it in a variable for the special matrix.

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(m, ...) {
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  i
}
