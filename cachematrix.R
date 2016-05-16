## makeCacheMatrix and cacheSolve functions shall enable cached operation of matrix inverse results
## to avoid unnecessary matrix inverse calculation for performance improvements


## makeCacheMatrix function is to return a list that provide the necessary operation to set/get data and cached results
## Input to the function is the matrix to be cached.
## Output is the list that provide the necessary operation to set/get data and cached results
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve function operates using makeCacheMatrix function to set/get cached matrix inverse results
## Input to the function is the list created by makeCacheMatrix
## Output is the matrix inverse result of the input matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return (inverse)
        }
        
        data<- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}


# Simple Test
matrix1 = matrix(c(1,2,3,4), 2,2)
cache1 = makeCacheMatrix(matrix1)
cacheSolve(cache1)


