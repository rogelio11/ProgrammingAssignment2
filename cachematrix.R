## This function create a "matrix" object in the cache an inverse it.
makeCacheMatrix <- function(x = matrix()) {
  var <- NULL
  set <- function(y) {
    x <<- y
    var <<- NULL
  }
  get <- function() x
  sinverse <- function(inverse) var <<- inverse
  ginverse <- function() var
  list(set=set, get=get, sinverse=sinverse, ginverse=ginverse)
  
}
## This function give us the inverse of the "matrix" showed by makeCacheMatrix discrived above. 
## If the inverse has been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## In this assignment, You can assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  var <- x$getinverse()
  if(!is.null(var)) {
    message("Getting Cache Data :)")
    return(var)
  }
  data <- x$get()
  var <- solve(data)
  x$setinverse(var)
  var
}
