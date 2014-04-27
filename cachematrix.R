## makeCacheMatrix(x = matrix()) function takes an optional matrix argument, and
## returns a special "matrix" (actually a list) containing 
## the matrix as well as a cache of its inverse.
## It assumes that the matrix supplied is always invertible.
##
## cacheSolve(x, ...) function takes as mandatary argument a special "matrix"  
## created by makeCacheMatrix, and any additional argument
## It returns the inverse of the matrix carried by the list argument.
## If the inverse has already been calculated (and the matrix has not changed), 
## it retrieves the inverse from the cache.
##
## Usage, assuming 'm' is an invertible matrix:
##    cm <- makeCacheMatrix(m)
##    inv <- cacheSolve(cm)
## Note that cm variable needs to be used in order to leverage caching, otherwise 
## a new cm object would be contructed every time; 
## this usage does not cache anything: cacheSolve(makeCacheMatrix(m))
## 
## Example:
##    mat <- matrix(rnorm(2048*2048), 2048) # most probably an invertible 256x256 matrix
##    determinant(mat, logarithm = FALSE) # useful to verify the matrix is invertible
##    cm <- makeCacheMatrix(mat) # creation of the list
##    inv <- cacheSolve(cm) #  invocation of cacheSolve
## 
## Example of performance benchmark:
##    system.time(cacheSolve(cm)) # first invocation of cacheSolve
##    user  system elapsed 
##    8.302   0.006   8.305 
##    system.time(cacheSolve(cm)) # further invocations of cacheSolve 
##    getting cached data
##    user  system elapsed 
##    0.000   0.001   0.000 

## makeCacheMatrix(x = matrix()) function takes an optional matrix argument, and
## returns a list containing functions to
## 1. set the value of the matrix passed as argument
## 2. get the value of this matrix
## 3. set the value of the inverse of this matrix
## 4. get the value of the inverse of this matrix
## * the inverse of the matrix is set to NULL initially, and every time 'x' is set

makeCacheMatrix <- function(x = matrix()) {

    cachedinverse <- NULL
    set <- function(y) {
        x <<- y
        cachedinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedinverse <<- inverse
    getinverse <- function() cachedinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## cacheSolve(x, ...) function takes as mandatary argument a list 
## created by makeCacheMatrix, and any additional argument (ellipsis)
## It returns the inverse of the matrix carried by the list argument:
## * if the inverse was already calculated and saved, it returns a cached value
## and displays a message to inform the user 
## * otherwise it calculates the inverse (using solve), saves it and returns it
## * additional arguments are passed to the solve function for matrix inversion
## * if the special "matrix" has changed its invers is reset to NULL (see makeCacheMatrix)


cacheSolve <- function(x, ...) {
    ## if cached inverse matrix is not null then display a message and return the inverse
    cachedinverse <- x$getinverse()
    if(!is.null(cachedinverse)) {
        message("getting cached data")
        return(cachedinverse)
    }
    ## otherwise get matrix argument
    data <- x$get()
    ## calculate the inverse of this matrix
    cachedinverse <- solve(data, ...)
    ## set the result of the inversion as cached inverse matrix
    x$setinverse(cachedinverse)
    ## return the newly computed inverse matrix
    cachedinverse
}
