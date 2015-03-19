## These functions accept an inverse matrix and calculate its inverse, leveraging
## the parent frame to cache the results.

## makeCacheMatrix: This function takes aninvertible matrix (ssumes that the 
## matrix supplied is always invertible), and creates a matrix "object" with
## 4 internal functions: get(), set(), getinverse(), setinverse().
## get(): gets the current matrix data.
## set(): resets the matrix data based on the matrix passed to it, and resets
## the cache.
## getinverse(): returns the inverse matrix if it is available.
## setinverse(): stores the inverse matrix data passed to it.

makeCacheMatrix <- function(x = matrix()) {
    if (!is.matrix(x)) {
        print("Value passed to function must be a matrix")
        return(NULL)
    }
    im <- NULL              ##initializes the cache variable
    print(environment())    ##for informational purposes
    evn <- environment()    
    print(parent.env(evn))  ##for informational purposes
    set <- function(y) {
        x <<- y
        im <<- NULL         ##resets cache
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    getevn<- function() environment()
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse,
         getevn = getevn)
}


## cacheSolve: This function takes the matrix object created with makeCacheMatrix
## and returns the inverse matrix. Returns a cached value if the inverse matrix 
## "im" has already been found, or generates the inverse matrix and stores it
## in the cache via the matrix object's internal "setinverse()" function.

cacheSolve <- function(x, ...) {
    ##if call function for inverse matrix that has already been calculated, 
    ## use cache
    im <- x$getinverse()
    if(!is.null(im)) {
        message("Retrieving cached matrix inverse data.")
        return(im)
    }
    ##if no cache, solve for inverse matrix
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    return(im)
}