## makeCacheMatrix - create new matrix object with inverse matrix cache
## cacheSolve - return inverse matrix with cache functionality

## Create new cache matrix with inverse matrix cache function
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    ## when setting new matrix, inverse is cleared
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Compute inverse matrix for makeCacheMatrix object
## if inverse is cached, cache is returned
## else inverse is computed and cached
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  ## if inverse matrix is cached
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## else compute new inverse matrix
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
