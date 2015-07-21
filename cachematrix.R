## This module computes the inverse of a square matrix after checking 
## whether a cachedvalue is already available. 


## Calculates inverse of matrix and stores it in a cached matrix. 
## setinv sets value, getinv retrieves the value

makeCacheMatrix <- function(x = matrix(data, nrow = TRUE, ncol = TRUE)) 
{
    m <- NULL
    set <- function(y) 
    {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    
    ##Check if Matrix is Square
    if(nrow(x)!=ncol(x)){
      return(print.default("Please enter a square matrix"))}
    
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Checks if matrix elements has changed, if not returns the cached value
## else computes the inverse.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
 
  ## Using ginv instead of solve
  m<- MASS::ginv(data)
  x$setinv(m)
  m
}
