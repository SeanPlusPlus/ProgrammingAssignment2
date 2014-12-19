## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

###############################################################################
#
# This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix
#
###############################################################################

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
}


## Write a short comment describing this function

###############################################################################
#
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should    retrieve the 
# inverse from the cache.
#
###############################################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    solve(makeCacheMatrix(x))
}
