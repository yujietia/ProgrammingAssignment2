## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is a function cache the inverse of a matrix.

## Write a short comment describing this function
## this function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
{
    m = NULL
    set = function(y)
    {
        x <<- y
        m <<- NULL
    }
    get = function() x
    setsolve = function(solve) m <<- solve
    getsolve = function() m
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Write a short comment describing this function
## this function computes the inverse of the special "matrix" returned by
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...)
{
    m = x$getsolve()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
        ## Return a matrix that is the inverse of 'x'
    data = x$get()
    m = solve(data, ...)
    x$setsolve(m)
    m
}
