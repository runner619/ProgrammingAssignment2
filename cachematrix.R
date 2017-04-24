## makeCacheMatrix will create a matrix object that can cache its inverse
## cacheSolve will compute the inverse of the matrix object
## of makeCacheMatrix. If the inverse has been solved already it will
## retrieve the value from the cache

## builds special matrix for calculating inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
  }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Check special matrix for cached inverse or calculate new

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
