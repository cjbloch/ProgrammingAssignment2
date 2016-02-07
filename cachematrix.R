## Matrix Inversion - Chris Bloch
## R Programming Assignment 2, Feb 2016
##
## The below functions complete a matrix inversion
## 
## To begin assign a 2x2 matrix to a variable through the 
## makeCacheMatrix function
## example:
## mymatrix<- makeCacheMatrix(matrix(c(2,4,6,8),nrow= 2,ncol= 2))

## makeCacheMatrix does the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix usig the solve() function
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
        m<- NULL
        set<- function(y) {
                x<<- y
                m<<- NULL
        }
        get<- function() x
        setsolve<- function(solve) m<<- solve
        getsolve<- function() m
        list(set= set, get= get, setsolve= setsolve, getsolve= getsolve)
}


## cacheSolve 
## This function computes, caches, and returns inverse of matrix
## created by the function makeCacheMatrix.
## cacheSolve checks if the inverse of the matrix has already been calculated.
## If the inverse matrix was calculated, the inverse matrix is retreived from cache.
## If the inverse matrix is not cached, cacheSolve calculates the inverse matrix 
## and sets the value of the inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<- x$get()
        m<- solve(data,...)
        x$setsolve(m)
        m
}