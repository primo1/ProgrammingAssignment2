
## Overall Description: This function computes the inverse of a square matrix and cache 
## the value of the inverse so that it can be retrieved from cache, if needed again, rather 
## than recomputed for the same matrix

## This function, makeCacheMatrix, creates a special "vector" which is a list containing 
## functions to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse of a square matrix  
## 4. get the value of the inverse of a square matrix
##
## Example: source(cachematrix.R)
## > a <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 10)))
## > cacheSolve(a)
##            [,1]       [,2]
## [1,] 1.00628931 0.02515723
## [2,] 0.02515723 0.10062893
##
## > cacheSolve(a)
## getting cached data
##            [,1]       [,2]
## [1,] 1.00628931 0.02515723
## [2,] 0.02515723 0.10062893


makeCacheMatrix <- function(x = matrix()) {
       
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
         
        setsolve <- function(solve) m <<- solve

        getsolve <- function() m
         
        list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)

}

## This function, cacheSolve, calculates the mean of the special "vector" created 
## with the 'makeCacheMatrix' function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setsolve function.


cacheSolve <- function(x, ...) {
        
        m <- x$getsolve()
        if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
        
         m <- solve(data, ...)
         x$setsolve(m)
         m
       
}
