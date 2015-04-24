# In this R script, there are two functions: makeCaheMatrix and cacheSolve.
# The aim is to calculate the inversion of a matrix.
# To ensure the code stays efficient in the case of large more complex matricies,
# the code can cache the value of the matrix inversion, so that when needed again,
# it can be looked up in the cache rather than recomputed.



## Return a list(of functions) that can: get / set the matrix, or get / set the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMarix = getMatrix)
}

## Compute the inverse of a matrix, but first check if its inverse is stored in cache.
cacheSolve <- function(x = matrix(), ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("Getting the cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setMatrix(m)
  m
}
