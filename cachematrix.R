## Functions to create a special matrix that caches its inverse

## Create a special "Matrix" that will store the Cached Inverse of itself
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  getMatrix <- function() {
    x
  }
  getInverse <- function() {
    invMat
  }
  setInverse <- function(inv) {
    invMat <<- inv
  }
  
  list ( getMatrix = getMatrix, setMatrix = setMatrix, 
         getInverse = getInverse, setInverse = setInverse)
}


## Function returns the inverse of the Cached Matrix X
##Checks to see if the Cached Inverse exists and if not, creates it, and
##sets it in the special cached matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  im <- x$getInverse()
  
  if (is.null(im)) {
    message("Not Cached... Calculating Inverse...")
    im <- solve(x$getMatrix())
    x$setInverse(im)
  } 
  im
}
