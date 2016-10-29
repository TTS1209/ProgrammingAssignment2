## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## Initialize the inverse property
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix      
    x
  }
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
   inv <<- inverse
  }
  ## Method to get the inverse of the matrix
  getInverse <- function() {
   ## Return the inverse property
   inv
  }
  ## Return a list of the methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" craeted by function above. Now we can retrieve the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  ## Return the inverse if its already set
  if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  ## Get the matrix from our object
  mat <- x$get()
  ## Calculate the inverse using matrix multiplication
  inv <- solve(mat, ...)
  ## Set the inverse to the object
  x$setInverse(inv)
  ## Return the matrix
  inv
}
