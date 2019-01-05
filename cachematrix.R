## Functions that cache matrix inversion
## Usage:
## m.cache <- makeCacheMatrix(m)  ## m is a matrix
## The first time the following call is made:
## m.inv <- cacheSolve(m.cache, ...)  
## matrix inversion is calculated using solve() and cached
## ( note that the ... are additional arguments passed to solve() )
## Subsequent calls simply return the cached inverse:
## m.inv <- cacheSolve(m.cache, ...)  
## without calling solve() again.
## To change the matrix value to new.matrix, simply use:
## m.cache$set(new.matrix)
## This will reset m.cache and the next call to cashSolve will recompute
## the inverse.

## Note: if iniital inverse calculation is performed and cached using a 
## certain set of optional arguments passed to solve () through ...
## but subsequent calls change these optional arguments, 
## function cacheSolve will ignore the optional arguments in subsequent calls
## and simply return the previously cached inverse.
## I did not address this complication as it seemed outside the scope
## of this assignment.

#############################################################################

## makeCacheMatrix returns a list of getters and setters for a function
## and its cached inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    get <- function() {x}
    
    set <- function(y) {
        
        x <<- y
        inv <<- NULL
    } 
    
    get.inv <- function() {inv}
    
    set.inv <- function(m_inv) {inv <<- m_inv}
    
    list(get=get, set=set, get.inv=get.inv, set.inv=set.inv)
}


## Return a matrix that is the inverse of cached matrix 'x'

cacheSolve <- function(x, ...) {
    
    x.inv <- solve(x$get(), ...)
    
    if (is.null(x$get.inv())) {
        x$set.inv(solve(x$get(), ...))
    } else {
        message("getting cached matrix inverse")
    }
        
    x$get.inv()
    
}
