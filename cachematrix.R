## makeCacheMatrix: This function creates a special "matrix" object
##                  that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # initialize cache store
  cache <- NULL  

  # creates matrix 
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # gets value of the matrix
  get <- function() x
  
  # inverts matrix and stores in cache
  invMatrix <- function(inverse) cache <<- inverse
  
  # gets inverted matrix from cache
  getInverse <- function() cache
  
  # returns the created function
  list(set = set, get = get, invMatrix = invMatrix, getInverse = getInverse)
  
}


## cacheSolve:  This function computes the inverse of the special "matrix"
##              returned by makeCacheMatrix above. If the inverse has already
##              been calculated (and the matrix has not changed), then cacheSolve
##              should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## gets the inverse of the matrix stored in cache
  cache <- x$getInverse()
  
  ## returns inverted matrix from cache, if it exists
  if(!is.null(cache)) {
    message("getting cached data...")
    return(cache)
  }
  ## creates the matrix, if it doesn't already exist
  matrix <- x$get()
  
  ## returns inverse of the matrix
  cache <- solve(matrix, ...)
  
  ## sets inverted matrix in cache
  x$invMatrix(cache)
  
  # display matrix in console
  return(cache)
  
}


