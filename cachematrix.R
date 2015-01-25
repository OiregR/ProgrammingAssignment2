

## Caching the Inverse of a Matrix
## Caching potentially time-consuming computations rather than 
## computing it repeatedly.

## 'm' is where the result of the inverse is stored
## 'set' set the matix created
## 'get' return the input matrix
## 'setInverse' set the inverse matrix
## 'getInverse' return the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setInverse <- function(inverse) m <<-inverse 
  getInverse <- function() m 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Computes the inverse of the matrix unless it has already 
## been computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}
