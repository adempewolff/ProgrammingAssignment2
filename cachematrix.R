## These functions implement a solution for creating special matrix objects
## that hold a cached copy of their inverse once calculated for the first time.
##
## Input to makeCacheMatrix() should be a matrix.  It returns a list containing
## four functions:      1. $set (assign a new matrix and reset the cache)
##                      2. $get (retrieve the matrix)
##                      3. $setinv (store the inverse of the matrix)
##                      4. $getinv (retrieve the inverse of the matrix)
##
## Input to cacheSolve() should be the object create by makeCacheMatrix(). It
## returns the inverse of the matrix, newly calculated after the first call,
## and from the cache in subsequent calls.
##
## Based on example code from https://github.com/rdpeng/ProgrammingAssignment2


## Turns a matrix into a special object which can store the inverse in cache

makeCacheMatrix <- function(x = matrix()) {
        
        v <- NULL
        set <-function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinv <- function(inv) v <<- inv
        getinv <- function() v
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Retrieves cached inverse if found, otherwise solves for the inverse, then
## caches and returns it

cacheSolve <- function(x, ...) {
        
        v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinv(v)
        v
}
