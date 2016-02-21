## The purpose of the following pair of functions is to calculate and cache the
## inverse of a matrix. If the contents of the matrix are not changing,
## the cached inverse matrix is returned to avoid time-consuming recalculation.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse. This object is a list containing a function to
## 1) set the value of the matrix, 2) get the value of the matrix,
## 3) set the value of the inverse matrix, 4) get the value of the inverse
## matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve: This function calculates the inverse of a special "matrix"
## created with the makeCacheMatrix function. It first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the
## setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m        
}
