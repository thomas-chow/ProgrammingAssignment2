## The two functions below are used to create a special matrix object
## that stores a matrix and cache's its inverse

## The first function makeCacheMatrix creates a special "matrix", a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The Second function calculates the inverse of the special "matrix" created above. However, a check is
## done to see if the inverse has already been calculated. If it has been calculated, then it gets the
## inverse from the cache and skips the computation. If it hasn't been calculated, then it calculates the
## inverse of the data and sets the value of the inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
