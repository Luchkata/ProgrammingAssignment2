## Put comments here that give an overall description of what your
## functions do
## The goal in this task is to write 2 functions, one calculating the inverse of a matrix and stores the result 
## in the cache and the other checks whether the result is in the cache and gives back the solution
## Write a short comment describing this function
## makeCacheMatrix is a function which aims to create a special matrix object that takes input and stores it in the cache

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) m <<- inverse
            getinv <- function() m
            list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve is a function which aims to compute the inverse of the special matrix returned by makeCacheMatrix.
## Furthermore, it evaluates whether the inverse of that matrix has been already calculated and stored into the cache
## if so, than the result is directly taken from the cache and the computation is skipped.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getinv()
            if(!is.null(inv)) {
                message("getting cached result")
                return(inv)
                }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinv(inv)
            inv
}
