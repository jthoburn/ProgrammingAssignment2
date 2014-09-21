## Matrix inversion is computationally expensive. To avoid recoumputing a matrix inversion, 
## it is cached and can be retrieved without having to recalculate it.
## 

## The makeCacheMatrix function creates a list of funcitons used for caching and retrieving 
## a matrix and its inversion

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  ## Initialize the matrix inversion
    
    ## Cache the matrix and initialize inversion
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Retrieve the cached matrix
    get <- function() x
    
    ## Cache the matrix's inversion
    setinvert <- function(invert) i <<- invert
    
    ## Retrieve the matrix's inversion
    getinvert <- function() i
    
    list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## The function calculates the matrix's inversion or retrieves a previously calculated inversion.

cacheSolve <- function(x, ...) {
    ## Retrieve the cached inversion
    i <- x$getinvert()
    
    ## If the cached inversion is not null, return it
    if (!is.null(i)) {
        message("get cached data")
        return(i)
    }
    
    ## Retrieve the matrix data from the cached matrix, calculate inversion, and cache
    ## the inversion
    data <- x$get()
    i <- solve(data, ...)
    x$setinvert(i)
    
    ## Return a matrix that is the inverse of 'x'
    i  
}
