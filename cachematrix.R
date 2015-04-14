## Caching the Inverse of a Matrix

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ivs <- NULL
  set <- function(y) {
    x <<- y
    ivs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ivs <<- inverse
  getinverse <- function() ivs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ivs <- x$getinverse()
  if(!is.null(ivs)) {
    message("getting cached data")
    return(ivs)
  }
  data <- x$get()
  ivs <- solve(data, ...)
  x$setinverse(ivs)
  ivs
}
