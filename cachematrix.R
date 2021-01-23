## This file contains two functions, makeCacheMatrix takes a matrix and 
## convert it to a list of functions which can be used to speed up 
## the inversion process by storing the result in a cache. cacheSolve 
## uses the list created by the makeCacheMatrix to solve the inverse of 
## the matrix, either by pulling it from the cached version if it is 
## already available, or by calculating it and storing it in the cache
## before returning it.

## makeCacheMatrix takes a matrix as an argument, 
## converts and returns it as a list containing 
## set, get, setinv and getinv functions

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## The cacheSolve function calculates the inverse of the 
## cached matrix created in the makeCacheMatrix function. 
## cacheSolve checks to see if the inverse has already been
## calculated, and if it has, returns the value from the cache.
## Otherwise, it will calculate the inverse using the solve()
## function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
