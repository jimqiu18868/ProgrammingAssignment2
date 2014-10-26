## The two functions together calculate, cache and manipulate
## the data of a matrix and its inverse. If a matrix already has
## its inverse cached, the cacheSolve function can return the
## cached data instead of calculating the inverse.

## This function stores the input matrix and its inverse in its 
## environment. It contains four functions used to manipulate
## values of the matrix and inverse. This function returns
## a list for acsessing those functions and values.

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(m){
                x <- m
                matinv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) matinv <<- inv
        getinv <- function() matinv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function return the cached inverse of the input matrix, 
## if it is calculated. If not, it will calculate the inverse of
## the input function and cache it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        
        ## Return a matrix that is the inverse of 'x'
}
