## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix creates complex variable which can:
## 1. hold matrix value(aka aMatrix variable)
## 2. hold inverse of the matrix (aSolvedMatrix)
## supported operations:
## -set original matrix (makeCacheMatrix's set)
## -get original matrix (makeCacheMatrix's get)
## -set inverse of aMatrix (makeCacheMatrix's setCached)
## -get inverse of aMatrix (makeCacheMatrix's getCached)

makeCacheMatrix <- function(aMatrix = matrix()) {
    aSolvedMatrix <- NULL;
    set <- function(y) {
        aMatrix <<- y;
        aSolvedMatrix <<- NULL;
    };
    get <- function() aMatrix;
    setCached <- function(mean) aSolvedMatrix <<- mean;
    getCached <- function() aSolvedMatrix;
    list(set = set, get = get,
         setCached = setCached,
         getCached = getCached);
}



## cacheSolve finds inverse value(matrix) of supplied matrix.
## Caches solved value to avoid complex computations, 
## so real solving happens on first call only.
## precondition:
## aMatrix must be created with appropriate makeCacheMatrix call
cacheSolve <- function(aMatrix, ...) {
    ## Return a matrix that is the inverse of 'aMatrix'
    aSolvedMatrix <- aMatrix$getCached();
    if(!is.null(aSolvedMatrix)) {
        message("getting cached data");
        return(aSolvedMatrix);
    }
    data <- aMatrix$get();
    aSolvedMatrix <- solve(data, ...);
    aMatrix$setCached(aSolvedMatrix);
    aSolvedMatrix;
}