## This set of functions computes the inverse of a matrix whilst caching the results for better
## performance
## The makeCacheMatrix function returns a list which is the argument for the cacheSolve function

## makeCacheMatrix will return a list containing the following functions:
## 1. setMatrix which will set a new original matrix and clear the cache;
## 2. getMAtrix which will return the original matrix;
## 3. cacheInverseMatrix which will receive a resultant inverse matrix and set the 
## inverseMatrixCache variable value with it; and
## 4. getCachedInverseMatrix which will return the inverse matrix previously cached
makeCacheMatrix <- function(mtx = matrix()) {
    inverseMatrixCache <- NULL
    
    setMatrix <- function(x){
        mtx <<- x
        inverseMatrixCache <<- NULL
    }
    getMatrix <- function() mtx
    cacheInverseMatrix <- function(inverseMatrix) inverseMatrixCache <<- inverseMatrix
    getCachedInverseMatrix <- function() inverseMatrixCache
    list(setMatrix=setMatrix, getMatrix=getMatrix,
         cacheInverseMatrix=cacheInverseMatrix,getCachedInverseMatrix=getCachedInverseMatrix)
}


## cacheSolve will receive makeCacheMatrix's return list and:
## 1. Check if there is any cached inverse matrix
## 2. If so, it will return the cached inverse matrix
## 3. otherwise it will compute the inverse matrix from the original one and cache it
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getCachedInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("retrieving cached inverse matrix")
    }
    else {
        originalMatrix <- x$getMatrix()
        inverseMatrix <- solve(originalMatrix, ...)
        x$cacheInverseMatrix(inverseMatrix)
    }
    inverseMatrix  
}
