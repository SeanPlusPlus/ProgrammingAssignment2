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
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

###############################################################################
#
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache.
#
###############################################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


###############################################################################
#
# Ouput
#
###############################################################################

# > source('cachematrix.R')
# > m1 <- matrix(c(2, 4, 3, 5), nrow=2, ncol=2)
# > m2 <- makeCacheMatrix(m1)
# > cacheSolve(m2)
#      [,1] [,2]
# [1,] -2.5  1.5
# [2,]  2.0 -1.0
