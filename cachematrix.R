##
## Author	: jmtoledo for the course R Programming - Assignemt 2
## Date		: 17/09/2014
##
## For optimization purpose, we construct two functions that allow us
## to caching the inverse of a matrix rather than computing it repeatedly,
## and get its value when we need it.
##
## (Assume that the matrix supplied is always invertible.)
##
##
## whith makeCacheMatrix creates the object and the functions 
## to  calculate and store the value, in m object
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) m <<- inverse
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve,
             getsolve = getsolve)
}
## With cacheSolve gets the matrix inverse of x, 
## 		 - if it was calculated before, simply get it 
## 		 - if not, calculate it and store it.
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
##
## Example in console: 
##> a<-c(c(1,2,3),c(0,1,4),c(5,6,0))
##> b<-matrix(a,3,3)
##
##> b
##  	 [,1] [,2] [,3]
##	[1,]    1    0    5
##	[2,]    2    1    6
##  [3,]    3    4    0
##
##> B_inv<-makeCacheMatrix(b)
##
##> B_inv$get
##
##> cacheSolve(B_inv)
##  	 [,1] [,2] [,3]
##	[1,]  -24   20   -5
##	[2,]   18  -15    4
##	[3,]    5   -4    1
##> cacheSolve(B_inv)   
##
##getting cached data
##     	 [,1] [,2] [,3]
##	[1,]  -24   20   -5
##	[2,]   18  -15    4
##	[3,]    5   -4    1
## so, inverse matrix was obtain from cached date instead of calculate ## if again