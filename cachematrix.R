## CACHEMATRIX.R
## These functions compute the inverse of a known invertible
## matrix, and store the result. If the inverse of the 
## input matrix has already been calculated then the inverse
## will be retrieved.


## MAKECACHEMATRIX(x) sets the input matrix, and initializes
## the functions used to set and store the matrix and its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  
  setMatrix <- function(input){
      x      <<- input
      invmat <<- NULL
  }
  
  getMatrix  <- function() x
  
  setInverse <- function(inverse) invmat <<- inverse
  
  getInverse <- function() invmat
  
  list(set    = setMatrix,
       get    = getMatrix,
       setInv = setInverse,
       getInv = getInverse)
  
}


## CACHESOLVE(x) looks to see if the inverse of the given
## matrix stored in 'x' is already stored and returns it, 
## or will compute the inverse of the input matrix

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  
  if (!is.null(inv)) {
    message("Retrieving inverse...")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data,...)
  
  x$setInv(inv)
  
  inv
  
}
