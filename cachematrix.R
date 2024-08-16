## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the cache variable
    m <- NULL
    
    # Set the matrix and clear the cached inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the cached inverse
    setinverse <- function(inverse) m <<- inverse
    
    # Get the cached inverse
    getinverse <- function() m
    
    # Return a list containing all the operations
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    # If the inverse is cached, return the cached data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Otherwise, compute the inverse and cache it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    # Return the computed inverse
    m
}
