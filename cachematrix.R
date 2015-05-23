## This file contains R functions for solving for the inverse
## of a given matrix making use of caching for quick retrevial
## of past solutions.

## makeCacheMatrix
##    Creates a cache to store a matrix and its inverse.
##    Functions provided:
##    	- set the value of the matrix
##      - get the value of the matrix
##      - set the value of the inverse
##      - get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve
##	Solves for the inverse of the given matrix.  Makes use of a 
##	cache for quick retreival of previous results.
##
##	Parameters
##		x : makeCacheMatrix containing matrix to solve for.
##            ... : additional paramters used by the solve() function.
##	Returns
##		inverse of the matrix (as a matrix).

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse        
}
