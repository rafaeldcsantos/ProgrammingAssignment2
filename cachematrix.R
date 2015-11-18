## Here is my soliuton for the Programming Assignment 2.

## Creates a cached matrix. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(minv) inv <<- minv
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## Here is the cacheSolve bit, which will the matrix only if it wasn't inverted and 
## stored before.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}

# Let's see if this works.
myMat <- matrix(c(1, -1, -2, 2, -3, -5, -1, 3, 5), 3, 3, TRUE)
myMat
inverted <- makeCacheMatrix(myMat)
cacheSolve(inverted)
cacheSolve(inverted)