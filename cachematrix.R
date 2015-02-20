## Since Matrix Inversion is a costly computation, the follwoing function will 
##      1. Create a cache to store the  Matrix inversion
##      2. Will calculate the inversion unless it is already exist in the cache
##      3. Will return the calculated inversion of the Matrix

## MakeCacheMatrix will create the cache we will store the inverse value in

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## CacheSolve will calculate the invesre of the Matrix using the R function Solve(), 
## unless it's already in the cache. Then will return its value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
