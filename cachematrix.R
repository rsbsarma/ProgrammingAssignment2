## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}


##Sample Run

##> x = rbind(c(1, 2), c(-2, 1))
##> m = makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]    1    2
##[2,]   -2    1
##> cacheSolve(m)
##     [,1] [,2]
##[1,]  0.2 -0.4
##[2,]  0.4  0.2
##> cacheSolve(m)
##getting cached data.
##     [,1] [,2]
##[1,]  0.2 -0.4
##[2,]  0.4  0.2
##>

#Reference online inverse calculator to check the answer: http://matrix.reshish.com/inverCalculation.php
