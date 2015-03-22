## Here are two functions, makeCacheMatrix and cacheSolve,that cache 
the inverse of a matrix. The makeCacheMatrix function creates a 
special "matrix" object (m) that can cache its inverse. The 
cacheSolve function computes the inverse of m that is returned by 
makeCacheMatrix. If the inverse has already been calculated (and m 
has not changed), then the cachesolve function retrieves the inverse 
from the cache.

makeCacheMatrix <- function(x = numeric()){  ## The makeCacheMatrix 
                ## function creates a special "matrix" object (m) that can cache its 
                ## inverse.        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
cacheSolve <- function(x, ...) {  ## The cacheSolve function first 
                ## checks to see if the inverse of m has already been 
                ## calculated. If so, the cacheSolve function gets the inverse 
                ## of m from the cache and skips the computation. Otherwise, it 
                ## calculates the inverse of m and sets its value in the cache 
                ## via the setsolve function.
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