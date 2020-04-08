## makeCacheMatrix is applied to a matrix to create a set of functions
## that support caching its inverse after it's been calculated once, and returning that
## cached inverse thereafter.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinv = function(inverse) m <<- inverse
        getinv = function()m
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## cacheSolve can be used against any invertible matrix to get its inverse.
## If its inverse has never been calculated before, it will be cached, so any
## subsequent calls for it will simply fetch from the cached value instead of 
## re-incurring the expensive cost.

cacheSolve <- function(x, ...) {
        m = x$getinv()
        if (!is.null(m)){
                message('getting cached data')
                return(m)
        }
        data = x$get()
        m = solve(data,...)
        x$setinv(m)
        m
}
