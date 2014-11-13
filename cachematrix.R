# Function creates an object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  matInv  <- NULL
  set  <- function(y) {
    x <<- y
    matInv <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) matInv  <<- inverse
  getinverse  <- function() matInv
  list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Function computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should return the inverse from the cache
cacheSolve <- function(x, ...) {
  
  matInv  <- x$getinverse()
  if (!is.null(matInv)){
    message("getting cached data")
    return(matInv)
  }
  data  <- x$get()
  matInv  <- solve(data, ...)
  x$setinverse(matInv)
  matInv
}
