##Two functions for caching the inverse of a matrix and returning it

##First function caches the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  inv <- NULL
  ## set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the matrix
  get <- function() x
  ## set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  ## Method to get the inverse of the matrix
  getInverse <- function() inv
  ## Return a list of the methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

  

## second function returns the inverse matrix from the cache. If no object cached,
##the inverse will be computed
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  ## Just return the inverse of the matrix if its already cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## get the matrix from our object
  mat <- x$get()
  ## calculate the inverse
  inv <- solve(mat, ...)
  ## set the inverse to the object
  x$setInverse(inv)
  ## print the inverted/cached matrix
  inv
}
