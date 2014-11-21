## The below functions provide a way to optimize matrix inversion calculations
## by caching the previously calculated inverse matrix. If the values in the
## original matrix are changed, then the cache is cleared to indicate that a new
## inverse matrix needs to be recalculated.

## makeCacheMatrix() creates the object that represents the original matrix and
## its previously calculated inverse matrix (or NULL if the inverse matrix for
## the current data has not yet been calculated.) It has 4 functions:
##    * set(inputtedMatrix)
##    * get()
##    * setInverse(inputtedInverseMatrix)
##    * getInverse()

makeCacheMatrix <- function(x = matrix()) {
   cachedInverse <- NULL
   set <- function(inputtedMatrix) {
      x <<- inputtedMatrix
      cachedInverse <<- NULL
   }
   get <- function() x
   setInverse <- function(inputtedInverse) cachedInverse <<- inputtedInverse
   getInverse <- function() cachedInverse
   list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve() utilizes makeCacheMatrix() to use the cached inversed matrix.
## If there is no cached inversed matrix, then it is calculated and then
## saved in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inversedMatrix <- x$getInverse()
   if(!is.null(inversedMatrix)) {
      message("Getting cached data")
   }

   else {
      dataMatrix <- x$get()
      inversedMatrix <- solve(dataMatrix)
      x$setInverse(inversedMatrix)
   }
   return(inversedMatrix)
}
