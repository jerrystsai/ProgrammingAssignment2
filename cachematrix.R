## Below are two functions that are used to create a special object 
##   that stores a (numeric) matrix and caches its inverse. These
##   functions assume the matrix is invertible.

## FUNCTION makeCacheMatrix()
## For a matrix, creates a list of four functions: set, get, setinv, and getinv, 
##   for the purpose of caching the inverse of the matrix so that it need
##   not be re-computed if already computed

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solution) inv <<- solution
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## FUNCTION cacheSolve()
## For a matrix, serve the same function as solve(), but if the
##   inverse of the matrix has already been computed, return
##   the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mtx <- x$get()
        inv <- solve(mtx, ...)
        x$setinv(inv)
        inv
}
