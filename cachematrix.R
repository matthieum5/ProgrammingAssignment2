## The functions makeCacheMatrix and cachesolve together create a matrix and calculate
## its inverse, caching the inverse for future function calls

## create a matrix 'x' and a list of functions for setting and getting the inverse 'i' of 
## matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Creates object i with null value
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x ## 
        setinverse <- function(solve) {
                i <<- solve
        }
        getinverse <- function() i
        list(set = set,
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## Calculate and cache the inverse of the matrix, or retrieve the inverse if aready cached

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i) ## Returns inverse of matrix 'x' if not null
        }
        data <- x$get() ## Get matrix 'x'
        i <- solve(data, ...) ## Solve inverse of matrix 'x'
        x$setinverse(i) ## Set value of setinverse() in makeCacheMatrix to inverse of matrix 'x'
        i ## Print inverse of matrix 'x'
}
