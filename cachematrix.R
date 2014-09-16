## makeCacheMatrix and cacheSolve encapsulate matrix and it's inversed value.
## inversed matrix is cached into memory, so calculated only once.
##

## makeCacheMatrix creates variable which:
## 1. holds matrix value(aka aMatrix variable)
## 2. holds inverse of the matrix (aSolvedMatrix). It's populated on first cacheSolve() call.
## supported operations:
## -set original matrix (makeCacheMatrix's set).
## -get original matrix (makeCacheMatrix's get).
## -setCached set inverse of aMatrix (makeCacheMatrix's setCached).
## -getCached get inverse of aMatrix (makeCacheMatrix's getCached).

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
## so real processing happens on first call only.
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