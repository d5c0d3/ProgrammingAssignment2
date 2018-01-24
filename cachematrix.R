## Coursera R Programming
## Programming assignment 2
## Write an R function that is able to cache potentially time-consuming
## computations. In this case we want to compute the inverse of a square matrix.
## For this assignment, assume that the matrix supplied is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                ## check if the matrix to be set identical to the one in the
                ## cache. If so the cache will not be cleaned.
                if(!identical(x,y)) {
                        x <<- y
                        im <<- NULL

                } else {
                        message("identical matrix")
                }
        }
        get <- function() x
        setinverse <- function(solve) im <<- solve
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
