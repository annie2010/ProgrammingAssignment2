## This progrom computes a matrix inverse
## For this assignment, assume that the matrix supplied is always invertible
## 
## The program consists of two functions:
##
## One function returns a matrix inverse either
##   from cache or from computation with cache miss
##   a computed inverse is cached by design
##
## The other function provides handles to the original matrix and its inverse
##   In this function, we can set/get the original matrix
##   we can also set/get the matrix inverse

## This function creates a special "matrix" object
##   that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the invertible matrix to null
  b <- NULL

  ## populate the original matrix per argument
  ## and initialize its inverse to null
  set <- function(y){
    x <<- y
    b <<- NULL
  }

  ## fetch the original matrix 
  get <- function() {
    x
  }

  ## populate the invertible matrix per argument
  setinverse <- function(inverse) {
    b <<- inverse
  }

  ## fetch the invertible matrix 
  getinverse <- function() {
    b
  }

  list(set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function computes the inverse of the special
##   "matrix" returned by `makeCacheMatrix` above. If the inverse has
##   already been calculated (and the matrix has not changed), then
##   `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # fetch matrix inverse from cache
  b <- x$getinverse()

  # if in cache, return cached inverse
  if (!is.null(b)){
    message("getting cached data")
    return(b)
  }

  # otherwise, compute the matrix inverse
  data <- x$get()
  b <- solve(data)
  # cache the computed inverse matrix
  x$setinverse(b)

  ## return the computed inverse matrix
  b 
}
