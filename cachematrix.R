## Paired together, the makeCacheMatrix and cacheSolve functions compute and 
## cache the inverse of a square invertible matrix. Because matrix inversion 
## can be computationally costly, the functions can be used to cache the 
## inverse for later retrieval. 


## The makeCacheMatrix function creates a object that caches the inverse of 
## a matrix. The object is a list of four functions: 
## 1. set() sets the matrix values
## 2. get() gets the matrix values
## 3. setInverse() sets the inverse of the matrix
## 4. getInverse() retrieves the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        #setmean <- function(mean) m <<- mean
        getInverse <- function() inv
        #getmean <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function computes the inverse of the matrix object returned by the 
## makeCacheMatrix function. If the inverse has already be computed, 
## cacheSolve returns the inverse stored in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
