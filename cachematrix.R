##  The primary objective in this test is to write a couple of functions,
##  namely "makeCacheMatrix" and "cacheSolve",
##  which allow us to cache the inverse matrix entered in the first function.

##  Definitions:
##      "x" is the input matrix
##      "inv" is the solved value
##      "mean" is changed to "inverse"
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function()x
    setInverse  <- function(inverse) inv <<- inverse
    getInverse  <- function() inv
    list(set = set, get = get,
         setInverse  = setInverse,
         getInverse  = getInverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## IF it was already calculated, it saves it in cache.

cacheSolve <- function(x, ...){

    inv <- x$getInverse()
    if(!is.null(inv)) {

      message("getting cached result")
      return(inv)
    }
    data <- x$get()
    inv<- solve(data, ...)
    x$setInverse(inv)
    inv
}
