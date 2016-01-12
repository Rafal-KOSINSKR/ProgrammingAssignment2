## Functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## The function creates a special "matrix", that can cache its 
    ## inverse and is really a list containing a function to:
    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the value of the inverse of the matrix
    ## 4. get the value of the inverse of the matrix
    
    is <- NULL
    isnew <- function() is
    
    iv <- NULL
    set <- function(y) {
        is <<- !(dim(y)==dim(get()) && all(y==get()))
        x <<- y
        if(isnew()) iv <<- NULL
    }

    get <- function() x
    setinvrs <- function(invrs) iv <<- invrs
    getinvrs <- function() iv

    list(isnew = isnew,
         set = set, get = get,
         setinvrs = setinvrs, 
         getinvrs = getinvrs)    
}

cacheSolve <- function(x, ...) {
    ## The function calculates the inverse of the special "matrix" created 
    ## with makeCacheMatrix function above. However, it first checks to see 
    ## if the inverse has already been calculated and the matrix has not 
    ## changed. If so, it gets the inverse from the cache and skips the 
    ## computation. Otherwise, it calculates the inverse of the data and sets 
    ## the value of the inverse in the cache via the setinvrs function

    iv <- x$getinvrs()
    if(!is.null(iv)) {
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setinvrs(iv)
    iv
}
