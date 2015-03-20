## Here We are trying to set a matrix and compute inverse of the matrix 
## caching the inverse value so that it will not be computed again and again.
## It computes inverse once and returns the cached value there after

## The function, makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to 
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value inverse of matrix
## 4. get the value of inverse of matrix


makeCacheMatrix <- function(x = matrix()) {

	  mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) mi <<- solve
        getsolve <- function() mi
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## The below  function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, 
## it calculates the mean of the data and sets the value of the inverse
## in the cache via the setsolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'sourcex'
        mi <- x$getsolve()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setsolve(mi)
        mi
}
