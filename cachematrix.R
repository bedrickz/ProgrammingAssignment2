## makeCacheMatrix takes a square matrix as an arguement and returns a special  
## matrix which contains functions for getting and setting the value along with
## inverting the matrix via the solve function

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve takes a CacheMatrix as an arguement and applies its 'setsolve()'
## function unless there is already a value stored (or cached), in which case
## it will retun the preallocated value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
