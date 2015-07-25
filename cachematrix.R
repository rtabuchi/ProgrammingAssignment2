## These functions will cache the inverse of a given matrix

## The makeCacheMatrix function will create a special matrix
## that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) { ##defines a function
## with the "matrix" x as an argument that will create a list containing
## a function within that will preform the the following actions:
        ##sets the value of the matrix,
        ##gets the value of the matrix, 
        ##sets the value of the inverse matrix
        ##gets the value of the inverse matrix
        m <- NULL
        set <- function(y) { #this function will set the value of the matrix
                x <<- y
                m <<- NULL
        } ##END OF set function
        get <- function() x ## this function will get the value of the matrix
        setinverse <- function(solve) m <<- solve #sets the value of the inverse matrix
        getinverse <- function() m #gets the value of the inverse matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
} ##END OF makeCacheMatrix
cacheSolve <- function(x = matrix(), ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
