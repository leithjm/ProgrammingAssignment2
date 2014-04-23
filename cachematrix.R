## This file contains functions that construct a cached matrix from
## the matrix passed as a parameter. The cached matrix provides functions
## in a list that allow the values of the matrix to be get and set. It also
## provides functions for getting and setting the inverse of the matrix.
##
## It is assumed that the matrix is a square matrix.


## Construct a cached matrix from the matrix passed as a parameter.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialise the inverse value to NULL
  matrix_inverse <- NULL
  
  ## Set the matrix value to the paramter value passed
  ## and the inverse to be NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  
  ## Return the cached matrix
  get <- function() x
  
  ## Set the inverse for the cached matrix
  setInverse <- function(m_inverse) matrix_inverse <<- m_inverse
  
  ## Get the inverse of the cached matrix
  getInverse <- function() matrix_inverse
  
  list( set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse )
  
  
}


## Return the inverse of the cached matrix supplied as a parameter.
## If the inverse has not been set, use the "solve" function to compute
## it and use the setter function to set it. If the value has been previously
## set, then use the getter function to get the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$getInverse()
  
  ## Check to see if the getter returned NULL. If not, then
  ## the inverse has been set previously, so return the 
  ## retrieved value.
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  
  ## If execution reaches here, then the value of the inverse has not 
  ## been set previously, so use the "solve" function, with the setter
  ## to set it.
  
  ## Get the cached matrix
  data <- x$get()
  
  ## Set the inverse to the return from the "solve" function.
  matrix_inverse <- solve(data, ...)
  
  ## Use the setter to set the inverse for the cached matrix.
  x$setInverse(matrix_inverse)
  matrix_inverse
}



