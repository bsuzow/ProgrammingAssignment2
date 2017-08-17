## cachematrix.R
## This script hosts two functions for getting an inverse 
## of a matrix. As the solve() can be taxing for a matrix with  
## large dimensions, the cached value is utilized if available.
## ==============================================================

## The makeCacheMatrix function returns 4 functions for setting &
## getting objects for the matrix passed in and its inverse.

makeCacheMatrix <- function(x = matrix()) {


  inversemat <- NULL
  set <- function(y) {
    x <<- y
    inversemat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversemat <<- inverse
  getinverse <- function() inversemat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function returns the inverse of a matrix passed
## into. If the inverse has already been calculated and found
## in cache, its cached value gets returned.  Otherwise, the
## solve() function is called. The function argument of x should
## be an object instantiated by calling makeCacheMatrix().


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inversemat <- x$getinverse()
   if(!is.null(inversemat)) {
      message("getting cached data")
      return(inversemat)
   }
   data <- x$get()
   inversemat <- solve(data, ...)
   x$setinverse(inversemat)
   inversemat
}
