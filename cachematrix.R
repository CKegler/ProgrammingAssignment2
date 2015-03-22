## @author Colin Kegler
## @file cachematrix.R
## 
## ProgrammingAssignment2 for Coursera/R_Programming/week3

## *** Please see test output below the function definitions. It provides examples
## of computing the inverse of a 2x2 and a 3x3 matrix.

## [1] makeCacheMatrix -- a function that receives a matrix and creates a 
## cache of both the matrix and the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInverseMatrix <- function(inverse) m <<- inverse
   
    getInverseMatrix <- function() m
   
    # return a list with a 4 elements: a getter/setter for the initial matrix and
    #                                  a getter/setter for the inverse matrix
    list(set = set, get = get,
        setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix)
}


## [2] cacheSolve -- a function that returns the cached value of the inverse of a matrix. 
## If the inverse of the matrix does not exist, then the function computes the inverse 
## of the matrix for the first time and caches the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if we reach this point, the inverse matrix has not yet been computed 
    # so calculate the inverse and return it.
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setInverseMatrix(m)
    
    m
}

###############################################################################

## Below is a text example with a 2x2 matrix. The inverse of the 2x2 matrix 
## has been computed in the function cacheSolve
##
# > testmatrix <- matrix(c(2,2,3,2), nrow=2, ncol=2  )
# > testmatrix
# [,1] [,2]
# [1,]    2    3
# [2,]    2    2
# 
# > example <- makeCacheMatrix(testmatrix)
# > inverseMatrix <-cacheSolve(example)
# > inverseMatrix 
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0

## Below is a text example with a 3x3 matrix. The inverse of the 3x3 matrix 
## has been computed in the function cacheSolve
##
# > testmatrix2 <- matrix( c(2,2,2,1,0,0,0,0,1), nrow=3, ncol=3)
# > testmatrix2
# [,1] [,2] [,3]
# [1,]    2    1    0
# [2,]    2    0    0
# [3,]    2    0    1
# 
# > inverseMatrix2 <- cacheSolve(example2)
# > inverseMatrix2
# [,1] [,2] [,3]
# [1,]    0  0.5    0
# [2,]    1 -1.0    0
# [3,]    0 -1.0    1

