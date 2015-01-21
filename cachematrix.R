
##The following functions will create both get and set functions to access the inverse of a Matrix
##store and cache the inverse of a matrix in the parent Global environment. 

##The makeCacheMatrix function will take an input matrix and return a List of functions. The functions will allow
##for access and storage of both the input matrix and inverse of the matrix (using the solve function) within the 
##global environment cache.   
makeCacheMatrix <- function(mtx = matrix()) {
        inverseMatrix <- NULL
        setMatrix <- function(y) {
                mtx <<- y
                inverseMatrix <<- NULL
        }
        getMatrix <- function() mtx
        setMatrixInverse <- function(solve) inverseMatrix <<- solve
        getMartixInverse <- function() inverseMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setMatrixInverse = setMatrixInverse,
             getMartixInverse = getMartixInverse)
}


##The cacheSolve function will take the input of a list of functions and return the matrix inverse which
##is stored in the parent environment cache. If the matrix inverse is null, the inverse will be calculated
##within this function and returned. 
cacheSolve <- function(mxList, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- mxList$getMartixInverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- mxList$getMatrix()
        imatx <- solve(data, ...)
        mxList$setMatrixInverse(imatx)
        imatx        
}
