## Creates a closure containing the matrix data and optionally
## the inverted matrix. The semantics of the inverted matrix is
## one of a cache. Inverted matrix is invalidated when source
## matrix is changed.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolved <- function(solved) s <<- solved
    getsolved <- function() s
    list(set = set,
         get = get,
         setsolved = setsolved,
         getsolved = getsolved)
}

## Computes the inverse of a matrix closure created by 
## makeCacheMatrix. The solved matrix is cached, so that subsequent
## calls return the cached matrix rather than repeating the 
## calculation.
cacheSolve <- function(x, ...) {
    s <- x$getsolved()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolved(s)
    s
}
