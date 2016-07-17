## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) {
    M <- NULL
    set <- function(Y) {
        X <<- Y
        M <<- NULL
    }
    get <- function() X
    setsolve <- function(solve) M <<- solve 
    getsolve <- function() M 
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}




## Write a short comment describing this function

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
    M <- X$getsolve() 
    if(!is.null(M)) {
        message("getting cached data")
        return(M)
    }
    data <- X$get()
    M <- solve(data, ...)
    X$setsolve(M)
    M
}
