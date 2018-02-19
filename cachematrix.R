## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initially set to NULL
  inv <- NULL
  
  #set the Matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get the Matrix
  get <- function() x
  
  #Set the Inverse of the Matrix
  setInverse <- function(inverse) inv <<- inverse
  
  #get the Inverse of the Matrix
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Get the Inverse of Matrix
  inv <- x$getInverse()
  #Check if Inverse value is available
  if (!is.null(inv)) {
    # if value available returned the cached version
    message("getting cached data")
    return(inv)
  }
  #Get the matrix
  mat <- x$get()
  #inverse the matrix
  inv <- solve(mat, ...)
  #Cached the inversed matrix
  x$setInverse(inv)
  #retun the new inverse matrix
  inv
}
