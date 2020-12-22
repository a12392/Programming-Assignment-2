##These functions catalyze matrix inversion, which is typically a long process.

##The makeCacheMatrix function produces a matrix that can cache its inverse form. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  sln <- function() {x}
  inv2 <- function(inverse) {inv <<- inverse}
  inv3 <- function() {inv}
  list(set = set, sln = sln, inv2 = inv2, inv3 = inv3)
  
}

##The cacheSolve function solves the inverse matrix that has been cached by
##makeCacheMatrix, which is used a step before cacheSolve.

cacheSolve <- function(x, ...) {
  inv <- x$inv3()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$sln()
  inv <- solve(mat, ...)
  x$inv2(inv)
  inv
}