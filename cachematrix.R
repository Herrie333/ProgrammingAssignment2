## These two functions caches the inverse of a matrix so that it can  
## be retreived at a later stage without the need to do calculate it again

## Function (Matrix) that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  invr <- NULL
  set <- function(y){
    x <<- y
    invr <<- NULL
  }
  getm <- function() {x}
  setInverse <- function(inverse) {invr <<- inverse}
  getInverse <- function() {invr}
  list(set = set, getm = getm, setInverse = setInverse, getInverse = getInverse)
}
##Calculates the inverse of the cached matrix
cacheSolve <- function(x, ...){
  invr <- x$getInverse()
  if(!is.null(invr)){
    message("Retreiving Cached Data")
    return(invr)
  }
  matrix <- x$getm()
  invr <- solve(matrix, ...)
  x$setInverse(invr)
  invr
}
