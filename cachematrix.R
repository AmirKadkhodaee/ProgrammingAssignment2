## Put comments here that give an overall description of what your
## functions do

## A functions which cache the inverse of a matrix
## Defining a special matrix object 
makeCacheMatrix <- function( dm = matrix() ) {
        
        i <- NULL
        
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        get <- function() {
                dm
        }
        
        set <- function( matrix ) {
                dm <<- matrix
                i <<- NULL
        }
        
        getInverse <- function() {
                
                i
        }
        
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This part is for computing the inverse of the special matrix and it will be 
## returned by "makeCacheMatrix"
## If the inverse has already been calculated, then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        
        dm <- x$getInverse()
        
        if( !is.null(dm) ) {
                message("getting cached data")
                return(dm)
        }
        
        
        data <- x$get()
        
        
        dm <- solve(data) %*% data
        
        
        x$setInverse(dm)
        
        dm
}
