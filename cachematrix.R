## makeCacheMatrix - creates a matrix
## cacheSolve      - calculates the inverse of the given matrix and cache the result

## Exposes 4 sub functions:
## 'set' assigns new matrix object into the main function
## 'get' reads back the matrix object 
## 'getinverse' reads back the calculated inverse of the matrix
## 'setinverse' assigns a new inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        getinverse <- function() inv 
        setinverse <- function(solve) inv <<- solve
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Caches the inverse result of a given matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
