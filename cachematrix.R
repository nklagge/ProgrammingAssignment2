## The functions makeCacheMatrix and cacheSolve are designed to allow efficient
## repeated matrix inversion. The functions allow a matrix inverse to be 
## cached so that it does not have to be repeatedly computed.

## The function makeCacheMatrix takes a square invertible matrix as an input
## and returns a list containing functions to set the matrix, get the matrix,
## set the matrix inverse, and get the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(invmat) {
                inv <<- invmat
        }
        getinv <- function() {
                inv
        }
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv)
}


## The function cacheSolve takes as an input the list output of makeCacheMatrix
## and returns the inverse of the original input matrix. The first time it is
## called, it will compute and cache the inverse; on subsequent calls, it 
## returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
