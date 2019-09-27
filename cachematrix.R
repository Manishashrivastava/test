## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    matrix_inv <- null
    setMatrix <- function(y){
        x <<- y
        matrix_inv <<- NULL
        
        getMtrx <- function() x
        setInv <- function (inverse) matrix_inv <<- inverse
        getInv <- function() invMtrx
        list(setMtrx = setMtrx, getMtrx = getMtrx,
             setInv = setInv,
             getInv = getInv)
    }
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    matrix_inv <- x$getInv()
    
    If(!is.null(matrix_inv)){
        message("getting cache invertible matrix data")
        return(matrix_inv)
    }
    
    Matrix_Data <- x$getMtrx()
    matrix_inv <- solve(Matrix_Data,...)
    x$setInv(matrix_inv)
    matrix_inv
    
}

