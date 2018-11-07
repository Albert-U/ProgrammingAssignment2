## We have two functions here: makeCacheMatrix and cacheSolve.
## They are meant to be used in order to cache the inverse of a matrix.


## The first one creates a list containing a function to 
## 1. set & get the value of the matrix
## 2. set & get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse 
    getinverse <- function() i
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## The second function calculates the inverse of the special "matrix" created with the function makeCacheMatrix
## If it was already calculated previously, it fetches the result from the cache. No further calculations.
## Otherwise, it proceeds with calculation and stores the result in the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }      
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)  
    return(i)
    i
}
