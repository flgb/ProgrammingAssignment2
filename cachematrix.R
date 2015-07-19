## FABIAN'S ASSIGNMENT 2 for RPROG-030
## Provides a pair of functions that cache the inverse of a matrix.
## 
## USAGE EXAMPLE
## 
## Define your matrix, e.g.:       my_matrix     <- matrix(c(2,4,6,1),2,2)
## Make the cache, e.g.:           cache_matrix  <- makeCacheMatrix(my_matrix)
## 
## Use the calculation, e.g:       print(cacheSolve(cache_matrix))    

## Encapsultates a cache of a matrix and its inverse via a list of functions.
## $getMatrix and $setMatrix are used to reteive and define the matrix.
## $getInverse and $setInverse are used to retrive and define its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialise the matrix (m), and the inverse of the matrix (i)
    m <- x
    i <- NULL
    
    # Set the matrix (and clear the cached inverse)
    setMatrix <- function(y) {
        m <<- y
        i <<- NULL
    }
    
    # Get the matrix
    getMatrix <- function() {
        m
    }
    
    # Set the cached Inverse of the matrix
    setInverse <- function(z) {
        i <<- z
    }
    
    # Get the cached Inverse of the matrix
    getInverse <- function() {
        i
    }
    
    # Return the getters and setters for the matrix and its inverse
    list(
        setMatrix = setMatrix,   getMatrix = getMatrix, 
        setInverse = setInverse, getInverse = getInverse
    )
}


## Calculates the inverse of a matrix given a 'CacheMatrix'. Will use the cached
## inverse of the matrix if available, rather than recalculating the inverse.
## 
cacheSolve <- function(x, ...) {
    
    i <- x$getInverse()

    if (!is.null(i)) {
        return(i)
    }
    else {
        x$setInverse(solve(x$getMatrix(), ...))
    }
}
