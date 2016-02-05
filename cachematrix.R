## Functions cache the inverse of a matrix.
## Below are two functions that are used to create a special object 
## that stores a matrix and cache the inverse of the matrix.

## makeCacheMatrix function creates a special "matrix" that can cache its inverse,
## which is really a list containing functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix
## It is assumed that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" 
## created with the makeCacheMatrix function.
## If the inverse has already been computed,
## than it gets the inverse from the cache and skips the computation.
## Otherwise, it computes inverse of the matrix 
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse of a matrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinverse(inv)
    inv
}
