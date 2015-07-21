## The functions below can be used to calculate and cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
# This function creates a special "matrix" object that can cache its inverse
    
    Inv <- NULL
    set <- function(y){
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    getInv <- function() Inv
    setInv <- function(I) Inv <<- I
    list(set=set, get=get, getInv=getInv, setInv=setInv)
}

cacheSolve <- function(x, ...) {
# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix. If the inverse has already been calculated
# then cacheSolve retrieves the inverse from the cache.
    
    mat <- x$get()
    Inv <- x$getInv()
   
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    
    Inv <- solve(mat, ...)
    x$setInv(Inv)
    Inv
}