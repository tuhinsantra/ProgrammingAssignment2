## this caches the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  seti <- function(inverse) inv <<- inverse
  geti <- function() inv
  list(set = set,get = get,seti = seti,geti = geti)

}


## this solves the matrix or returns cached matrix if already solved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...)
  {
    ## Return an inverse matrix of 'x'
    inv <- x$geti()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$seti(inv)
    inv
  }
  }
