## The following functions perfrom matrix inversion with caching the inverse of a matrix instead of computing it repeatedly

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
get <- function() x
setinverse <- function(solve) i <<- solve
getinverse <- function() i
list(set=set, get=get,
     setinverse=setinverse,
     getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } 
  ## Return a matrix that is the inverse of 'x' 
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i  
}
