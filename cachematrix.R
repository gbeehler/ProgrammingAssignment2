## These R functions work together to create an object that hold 
##  a matrix, its inverse, and functions/methods to manipulate them
## Example Usage:

## Create an 8 by 8 matrix that has an inverse in h8
##  >hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
##  >h8 <- hilbert(8)

## Create object to hold h8 matrix, its inverse, and functions to manipulate them
##  >mh8 <- makeCacheMatrix( h8 )

## Calculate the inverse or return the cache copy 
##  >cacheSolve(mh8)

## Get cache inverse
##  >cacheSolve(mh8)

## Function creates an object that stores a matrix, its inverse, and
##  the methods to set and get them

makeCacheMatrix <- function(x = matrix()) {
    
    ## When first used, set the matrix to input and inverse to NULL
    m <- NULL
    
    ## Set the matrix part of the object & NULL the inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get the matrix part of object
    get <- function() x
    
    ## Set the inverse of matrix
    setsolve <- function(solve) m <<- solve
    
    ## Get inverse matrix
    getsolve <- function() m
    
    ## Return the object & functions/methods
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## The function cacheSolve inspects the matrix object.
##  If the matrix is set, then return the inverse from cache or 
##   build cache the first time.

cacheSolve <- function(x, ...) {
    
    ## Is the inverse set?
    m <- x$getsolve()
    if(!is.null(m)) {
        
        ## Yes, so return already calculated inverse
        message("getting cached data")
        return(m)
    }
    
    ## No, need to calculate the inverse
    data <- x$get()
    m <- solve(data, ...)
    
    ## And, set cache
    x$setsolve(m)
    m
    
}
