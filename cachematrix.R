## makeCacheMatrix takes a matrix as a argument to give out list of functions
## including cached inverse of a matrix if it is already caculated.
## cacheSolve takes an output of makeCacheMatrix as an argument to give out the
## inverse of a matrix as a cached value if it is stored in the output, or an 
## calculated outcome if there is no cached value. 

## makeCacheMatrix gives out a list of four functions, each setting and getting 
## the value of a matrix and setting and getting the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of a matrix as a cached value if it is stored
## or as a calculated value

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting chached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

