## This set of functions helps to reduce resource requirements for
## inverse matrix calculation by caching results

## makeCacheMatrix resembles what would be called in Object Oriented
## Programming, a "class" (although this does not make R an OO-language).
##
## It contains a number of ("internal") attributes to store a matrix m
## and its inverse im (if any)
## Access to these attributes is provided by means of a set of methods:
##
##  set(m = matrix())         - to store the original matrix
##  get()                     - to retrieve it
##  setInverse(m = matrix())  - to store the inverse matrix
##  getInverse()              - to retrieve the inverse matrix
##
## To create an instance of "class" makeCacheMatrix, do this: 
##
##      x <- makeCacheMatrix(<definion of a matrix>)
##
## E.g. x <- makeCacheMatrix(matrix(1:4,2,2))
##

makeCacheMatrix <- function(m = matrix()) {
        im <- NULL
        set <- function(y) {
              m <<- y
              im <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) im <<- inverse
        getInverse <- function() im
        list (set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)

}


## The function cacheSolve expects as first argument a variable m
## that has been created by means of makeCacheMatrix
## When applied to such a variable it returns the inverse matrix
## of the matrix associated with the argument m (i.e. which can 
## be obtained by means of m$get())
##
## cacheSolve reduces CPU resource needs by validating whether 
## the inverse matrix has already been calculated before
## If so, the result is retrieved from cache.
##
## To test this function, do the following (example):
##
##   x <- makeCacheMatrix(matrix(1:4,2,2))
##   x$get()                                - shows the matrix
##   x$getInverse()                         - shows NULL
##  ix <- cacheSolve(x)                     - assigns inverse matrix to ix
##   x$get() %*% ix                         - autoprints the Identity Matrix
##  ix <- cacheSolve(x)                     - notifies user that cache copy is used
##
## Note: the function does not check whether the matrix is invertible, i.e.
##      use of non-invertible matrices will lead to an error

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  
        im <- m$getInverse()
        if (!is.null(im)) {
              message("getting cached data")
              return(im)
        }
        data <- m$get()
        im <- solve(data,...)
        m$setInverse(im)
        im
}
