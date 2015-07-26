## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #Set matrix
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Get matrix
  getMatrix <- function() x
  
  # Set matrix inverse
  setMatrixInverse <- function(solve) m <<- solve
  
  #Get matrix inverse
  getMatrixInverse <- function() m
  
  #Return list of all methods
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## This function computes the inverse of the special"matrix" returned by `makeCacheMatrix`
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #Get inverse of matrix (if any)
  m <- x$getMatrixInverse()
  
  #Return inverse of matrix if already computed
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Get matrix
  matrix <- x$getMatrix()
  
  #Computer inverse of matrix
  m <- solve(matrix)
  
  #Set inverse of matrix
  x$setMatrixInverse(m)
  
  #Return inverse of matrix
  m        
}
