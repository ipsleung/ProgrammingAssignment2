
# makeCacheMatrix is created as a list that contains a function 
# allowing one to:
#   set value of matrix
#   get value of matrix
#   set value of inverse of matrix
#   get value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) 
        inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


# cacheSolve returns a matrix that is the inverse of "x"
# it first checks to see if the inverse of "x" has already been calculated
# if already calculated, it skips the calculation and returns the cached answer
# if not, it calculates the inverse of the matrix and sets value in cache
# with the setinverse function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

