## Coursera R Programming
## Programming assignment 2
## Write an R function that is able to cache potentially time-consuming
## computations. In this case we want to compute the inverse of a square matrix.
## For this assignment, assume that the matrix supplied is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse. This special matrix object consists of a list of functions
## to set and get a cached matrix and it's inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL ## initialize im (the inverse matrix)
        set <- function(y) { ## cache a new matrix 
                ## check if the matrix to be set is identical to the one in the
                ## cache (matrix 'x'). If so the cache will not be
                ## re-initialized/cleaned.
                if(!identical(x,y)) {
                        x <<- y
                        im <<- NULL

                } else {
                        message("identical matrix")
                }
        }
        get <- function() x ## get the cached matrix
        setinverse <- function(inv) im <<- inv ## set/cache the inverse matrix
        getinverse <- function() im ## get the inverse matrix
        
        ##return a list of the above specified functions
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        im <- x$getinverse() ## get the inverse matrix of special matrix x
                             ## fromcache if available
        if(!is.null(im)) { ## if an inverse matrix is available ...
                message("getting cached data")
                return(im) ##... the cached inverse matrix is returned 
        }
        data <- x$get() ## get the matrix data from the matrix object
        im <- solve(data, ...) ## compute the inverse matrix
        x$setinverse(im) ## set/cache the inverse matrix
        im ## return the inverse matrix as output
}
