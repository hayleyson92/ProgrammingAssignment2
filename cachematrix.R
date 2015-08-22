## Coursera R Programming Assignment 2
## 2015-08-22 Hayley Son
## The functions combined return an inverse of a matrix, while if the matrix's
## inverse has already been calculated and cached, the cached result is recalled
## without going through the operation again.


## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {  ## user can reset the initial matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x  ## returns initial matrix x
        setinverse <- function(inverse) m <<- inverse  ## caches the inverse
        getinverse <- function() m  ## returns the cached inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Computes the inverse of the "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()  ## loads the cached inverse
        if(!is.null(m)) {
                message("getting cached data")  
                return(m)  ## if the cache exists, returns m
        }
        data <- x$get()  ## otherwise, loads the initial matrix x
        m <- solve(data, ...)  ## computes the inverse of x
        x$setinverse(m)  ## caches the inverse
        m
}


