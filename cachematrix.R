## 
## Filename: cachematrix.R
## Author: Christopher Lim
## Purpose: Coursera Data Science - R Programming Week 3 Assignment
## 
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. Below is
## a pair of functions that cache the inverse of a matrix. 
##


## This function creates a special "matrix" object that can cache its inverse.
## Is making use of R's closure behavior to so call cache the initially matrix
## and subsequently the inverse if it.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Setup 'inv' variable to hold inverse of matrix 'x'
    inv <- NULL 
    
    ## Assign a new value to matrix 'x' and reset the 'inv' variable both
    ## which are stored in the parent environment via <<- operator
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    
    ## Returns value of the matrix argument 'x'
    get <- function() x
    
    ## Assigns value of inverse matrix in parent environment
    setInverse <- function(inverse) inv <<- inverse
    
    ## Returns the value of inverse matrix from parent environment
    getInverse <- function() inv
    
    ## To allow the functions to be refer to with the $ operator
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated and not
## changed it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Obtain the value of the inverse matrix
    inv <- x$getInverse()
    
    ## Return cached inverse matrix if available
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    ## Compute the inverse of original matrix using solve function
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    ## Return the inverse of original matrix 'x'
    inv
    
}
