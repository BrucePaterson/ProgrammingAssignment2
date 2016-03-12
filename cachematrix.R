## There are 2 functions in his R file/workspace:
## The first function makeCacheMatrix creates a special matrix object
##      that can cache its inverse.
## The second function cachSolve computes the inverse of the special matrix 
##      returned by makeCacheMatrix (first function) - if already inverted
##      cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix creates the matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes the matrix inverse of x (from above) using the R "solve"
## function - if x has already been inverted cacheSolve will get the inverse
## stored on the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}