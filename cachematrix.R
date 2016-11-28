## The functions below allow us to cache the inverse of a matrix rather than computing it repeatedly.

## The first function, makeCacheMatrix, takes a matrix and creates a special "matrix" object which 
## can cache its inverse.  This "matrix" object is really a list containing a function to 
## (1) set the value of the matrix, (2) get the value of the matrix, (3) set the value of the 
## inverse, and (4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The second function, cacheSolve, computes the inverse of the special "matrix" created with the 
## above function.  If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache. Otherwise, it calculates the inverse of
## the matrix and stores the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}