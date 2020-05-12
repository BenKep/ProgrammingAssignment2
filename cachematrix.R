## The two functions makeCacheMatrix and cacheSolve cache the inverse
## of a matrix.

## The function makeCacheMatrix creates a matrix that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The function cacheSolve computes the inverse of the matrix returned by the 
## makeCacheMatrix. In the case the inverse matrix has already been computed
## the cacheSolve function takes the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
