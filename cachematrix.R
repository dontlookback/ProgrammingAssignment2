## makeCacheMatrix and cacheSolve provide the ability to cache the inverse of
## a matrix, to avoid computing it repeatedly, thereby improving overall
## program performance


## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve retrieves the inverse of a "matrix" object returned by
## makeCacheMatrix if it has already been cached.
##
## If the inverse has not already been cached, cacheSolve computes, caches, and
## returns the inverse.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
