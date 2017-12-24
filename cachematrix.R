## These functions check if the value of  inverse of a square matrix 
## is in the cache. If not it is calculated.
## Note: input to makecacheMatrix should be a square matrix with 
## dimensions specified already.
## The makeCacheMatrix function gets the value of the matrix, 
## sets the value of the matrix, 
## sets the inverse gets the value of the inverse
## returns an object that has all the elements of makeCacheMatrix 
## (x,inv and 4 functions)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv<-function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## cacheSolve takes value of inverse of matrix, 
## if it exists it returns it otherwise its calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        }