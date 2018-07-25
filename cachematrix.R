## R Programming - Assignment 2
## Sabrina Simon
## July 2018

## Simulating what internet browsers do to avoid loading repeatedly the same
## pages accessed by the user, we're going to build two functions that work
## together in order to set the S3 objects and check for the existence of the
## data (and get it) before load the page data.

## makeCacheMatrix is a function who creates an invertible matrix, as well sets
## all the functions needed in its environment for the next step.


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## The next step is to check for cached data before do the calculation.
## If there is none, it calculates the inverse matrix.


cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
