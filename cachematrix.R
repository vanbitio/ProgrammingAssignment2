## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The functions below cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)  {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inversion) inv <<- inversion
    getinv <- function() inv
    
    ## return a list containing a function to:
    ## 1. set the value of matrix
    ## 2. get the value of the matrix
    ## 3. set the value of the inverse
    ## 4. get the value of the inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    ## if inv is not NULL, get the cached data.
    if(!is.null(inv))   {
        message("getting cached data")
        return(inv)
    }
    
    ## otherwise if inv is NULL, solve for the matrix inverse.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)

    ## Return a matrix that is the inverse of 'x'    
    inv
}
