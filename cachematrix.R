## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# set: will set the value of the matrix
# get: will get the value of the matrix
# setinverse: will set the value of inverse of the matrix
# getinverse: will get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been calculated. If it has, it will get the cached result and skips the
# calculation If not, it will calculate the inverse and set the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
