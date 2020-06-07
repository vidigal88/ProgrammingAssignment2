## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_ <- NULL
  
  ## Set the value of the matrix
  setMatrix <- function(y) {
    x <<- matrix(y)
    inverse_ <<- NULL
  }
  
  ## Get the value of the matrix
  getMatrix <- function() x
  
  ## Set the inverse of the matrix
  setInverse <- function(solve) inverse_ <<- solve

  ## Get the value of the inverse
  getInverse <- function() inverse_
  
  ## List output
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

## cacheSolve function computes the inverse of the sepcial matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inverse_ <- x$getInverse()
  
  if(!is.null(inverse_)){
    message("getting cached data")
    return(inverse_)
  }
  
  data <- x$getMatrix()
  inverse_ <- solve(data, ...)
  x$setInverse(inverse_)
  
 ## Return a matrix that is the inverse of 'x'
  inverse_
  
}
