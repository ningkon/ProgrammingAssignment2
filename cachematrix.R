
## This function tahkes a matrix and calculates in inverse, and then caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
      }
    get <- function() x
    setinverse <- function(solve) m <<- solve # this caches the inverse of the matrix
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
     getinverse = getinverse)
}




## This function calculates the inverse of the matrix returned by the makeCacheMatrix function. 
## If the inverse has already been computed, then it gets the cached value, otherwise it computes the inverse
# and returns the result m

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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


