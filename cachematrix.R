## Creates a special "matrix" object that can cache its inverse.
## <<- works upward toward global environment until it finds the envoked variable
## This function (1) sets the value of the matrix and (2) gets the value of the
## inverse. 
## Based on the makeVector function from the Week 3 programming assignment of 
## R Programming Coursera course.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the "matrix" returned by makeCacheMatrix. If the 
## inverse is already calculated, then the function retrieves the inverse from
## the cache. Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the setinverse function.
## Based on the cachemean function from the Week 3 programming assignment of 
## R Programming Coursera course.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## Test case:
# X <- matrix(c(1,2,3,4), 2, 2)
# cacheX <- makeCacheMatrix(X)
# cacheSolve(cacheX)
