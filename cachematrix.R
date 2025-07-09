## Caching the inverse of a matrix
## This assignment consists of two functions named "makeCacheMatrix" and "cacheSolve".

## The "makeCacheMatrix" Function creates a special "matrix" 
## object whose inverse can be cached. 
## Essentially, it stores a matrix and provides a place to store its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL 
        
        set <- function(y) {
                x <<- y
                matrix_inv <<- NULL 
        }
        
        get <- function() x
        setInv <- function(inv) matrix_inv <<- inv
        getInv <- function() matrix_inv
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## The "cacheSolve" Function calculates the inverse of the special "matrix" object
## returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
        m_inv <- x$getInv()
        
        if (!is.null(m_inv)) {
                return(m_inv)
        }
        
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setInv(m_inv)
        m_inv
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
