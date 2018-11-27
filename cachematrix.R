## The following functions will cache a matrix inverse
## and pull that inverse from the cache if it is present.

## This function will put an inverse of a matrix into a cache.

makeCacheMatrix <- function(x = matrix()) {
      i = NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) i <<- inverse
      getinv <- function() i
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function will pull the inverse of the previously made matrix from the
## cache. If not inverse is found, this function will produce one.

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
      ## Return a matrix that is the inverse of 'x'
}
