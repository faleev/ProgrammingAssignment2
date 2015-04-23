## Given R script contains two functions which implements the ligic
## which allows to cache "solve" value of the square matrix. Once calculated for
## some particular matrix, this value will always be available without recalculations.

## makeCacheMatrix function provide an ability to create a matrix model to store 
## some of the matrix's parameters like matrix itself and value of solve function.
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

## cacheSolve function provides an ability to use the matrix's object created by
## makeCacheMatrix function to calculate the value of the solve function and
## re-use the cached value if any.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached matrix solve data:")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
