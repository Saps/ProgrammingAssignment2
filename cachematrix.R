## Programming Assignment 2 - doing, what they've wrote in manual

## Special object for storing matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) m <<- invert
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
  
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
  
}

## Example for test
## MxEx <- matrix(c(2,4,3,1,5,7,6,6,6),nrow=3,ncol=3,byrow=TRUE)

