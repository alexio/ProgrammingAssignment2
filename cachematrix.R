##Creates a "special matrix" which is really a list containing functions
## to get/store/cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        if(identical(x,y)){
            message("Matrices are the same as defined by identical(), not reset")
        }
        else {
            x <<- y
            i <<- NULL
        }
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Computes and returns the inverse of the 'special matrix' returned by
## 'makeCacheMatrix'. If the inverse has already been computed,
## then returns the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)){
            message("Getting cached inverse matrix")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
