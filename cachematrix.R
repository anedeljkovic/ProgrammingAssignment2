## Theese functions cache the inverse of a matrix.
## Matrix inversion can be a costly computation so caching the inverse of 
## a matrix is better approach than to repeatedly calculate it


## makeCacheMatrix Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" returned 
## by makeCacheMatrix, or retrieves the inverse from the cache
## if the inverse is already calculated

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
