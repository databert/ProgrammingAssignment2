## Function set consisting of"makeCacheMatrix" and "cacheSolve" for computation
## efficient matrix inversion.

## "makeCacheMatrix" creates an R object that stores a matrix and its corresponding
## inverted matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(invrt) inv <<- invrt
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## "cacheSolve" retrieves the inverted matrix from the cache or inverts an new
## input matrix and stores it to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
