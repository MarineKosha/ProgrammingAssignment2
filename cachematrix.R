## makeCacheMatrix creates [invertible] matrix object that can cache its inverse. 
## cacheSolve computes the inverse of the matrix object returned by makeCacheMatrix
## or gets the inverse from the cache if it has been calculated already.

## makeCacheMatrix stores a list of 4 functions: set, get, setInv, getInv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { ## changes initial matrix object x with object y
                x <<- y
                inv <<- NULL ## restores to null the previous inverse if "set" were applied 
        }
        get <- function() x ## returns initial matrix object x or the result of "set"
        setInv <- function() inv <<- solve(x) ## stores the result of the inverse calculation in inv variable
        getInv <- function() inv ## returns the above result 
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve retrieves the inverse of the matrix object returned by makeCacheMatrix
## if the inverse already exists, otherwise calculates it.

cacheSolve <- function(x = matrix(), ...) {
        inv <- x$getInv() ## verifies the value inv exists in memory and is not NULL 
        if (!is.null(inv)) { ## if it does and not NULL
                message("getting cached data") ## then returns a message
                return(inv) ## and the inverse itself
        }
        getdata <- x$get() ## if inv doesn't exist then gets the matrix stored with makeCacheMatrix
        inv <- solve(getdata, ...) ## calculates the inverse
        x$setInv(inv) ## stores it in the object generated assigned with makeCacheMatrix
        inv ## and returns it
}
