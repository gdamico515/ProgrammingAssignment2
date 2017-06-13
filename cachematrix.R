## Caching the Inverse of a Matrix below

## The function creates a matrix object that which can cache its inverse.


makeCacheMatrix <-function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        
        getInverse <- function() inv
        list(set = set, get = get,
             
             
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function below uses the makeCacheMatrix function to compute the Inverse

cachesolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        gd <- x$get()
        inv <- solve(gd, ...)
        x$setInverse(inv)
        inv
}
