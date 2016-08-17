## With these function you can calculate the inverse of a matrix. If the calculation
## is repeated without changing the matrix, the result of the inverse matrix
## comes from the cache, which saves a lot of computation time/power :-)

## This function defines a 'special' object, that stores a martix and caches
## its inverse.
## x$set(m) ... set the values of the matrix
## x$get() ... returns the matrix
## x$setinv(im) ... set the values of the inverse matrix
## x$getinv() ... returns the matrix
## Example:
## mat <- makeCacheMatrix (matrix(sqrt(1:9),3,3))
## mat$get()
## [,1]     [,2]     [,3]
## [1,] 1.000000 2.000000 2.645751
## [2,] 1.414214 2.236068 2.828427
## [3,] 1.732051 2.449490 3.000000

makeCacheMatrix <- function(x = matrix()) {
        iM <- NULL
        set <- function(y) {
                x <<- y
                iM <<- NULL
        }
        get <- function() x
        setinv <- function(inv) iM <<- inv
        getinv <- function() iM
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function calculates the inverse of the 'special' matrix created with the
## function above (makeCacheMatrix). A square invertible matrix is required as argument.
## A calculation is only made, if no changes were made to the Matrix.
## Otherwise the result comes from the chache.
## Example:
## mat <- makeCacheMatrix (matrix(sqrt(1:9),3,3))
## cacheSolve(mat)
##           [,1]      [,2]      [,3]   # result of calculating the inverse Matrix
## [1,] -20.22253   44.1901 -23.82823
## [2,]  60.33123 -145.4717  83.94491
## [3,] -37.58476   93.2640 -54.45016
## cacheSolve(mat)                      # running the function twice without changes in the Matrix
## getting cached data                  # result from cache (without calculation)
## [,1]      [,2]      [,3]
## [1,] -20.22253   44.1901 -23.82823
## [2,]  60.33123 -145.4717  83.94491
## [3,] -37.58476   93.2640 -54.45016

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
