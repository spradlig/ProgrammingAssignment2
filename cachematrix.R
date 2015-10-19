## The purpose of the functions within this file are to provide the
## inverse of an invertable matrix. Since the inversion calculation can
## can be time and computationally expensive a means of caching prior
## results is provided.
##
## cacheSolve: Provides the matrix inverse
## makeCacheMatrix: Provides an object which caches the inverse

## makeCacheMatrix
##  This function provides an object for storing and returning any
##  calculated matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Init the inverse function variable
  inv <- NULL
  
  # Create the set function for setting the input (x) & output (inv)
  #  cached data
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Return the cached input matrix
  get <- function () x
  
  # Set the value of the inverse into the cache
  setInv <- function(inverse) inv <<- inverse 
  
  # Return the value of the cached inverse
  getInv <- function() inv
  
  # Assemble the object for cache
  list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv
  )
}


## cacheSolve
##  This function employs R's solve function to calculate the a matrix
##  inverse. However, before calculating that inverse this function
##  checks for the existence of a previously cacluated inverse as well
##  as any changes to the input matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get the currently cached value of the inverse
  inv <- x$getInv()
  
  # Determine if the cached inverse exists
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  ## If we've gotten this far... then the cached inverse 
  ##  didn't exist.
  
  # Get the input data from the cache
  data <- x$get()
  
  # Calculate the inverse
  inv <- solve(data, ...)
  
  # Set that inverse to the makeCacheMatrix created object
  x$setInv(inv)
  
  # Return the calculated inverse
  inv
}
