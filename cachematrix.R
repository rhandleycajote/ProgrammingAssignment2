## Submitted by Rhandley D. Cajote, rhandley@eee.upd.edu.ph
## Put comments here that give an overall description of what your
## functions do:
## makeCacheMatrix: creates a list of functions to set the value of a matrix, 
## get the value of the matrix, set the inverse of the matrix, solve the inverse of matrix
## cacheSolve: determines if the inverse of the matrix is stores in the list created by
## makeCacheMatrix, if its there, then retrieve it. If not, then solve it.

## Write a short comment describing this function
## creates the list of functions for the Matrix
## usage: A <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2)
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL                # set to NULL at the beginning
     set <- function(y){      # defines the set value function
          x <<- y
          m <<- NULL
     }
     get <- function() x      # defines the get value function
     setinv <- function(inv) m <<- inv # define the set function
     getinv <- function() m        # define the get inverse function
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Write a short comment describing this function
## solve the inverse of the matrix if not previously cached
## usage: cacheSolve(A), where A is created using the above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     if(!is.null(m)){
          # if the return value is not NULL then return the set value
          message("getting cached data")
          return(m)
     }
     data <- x$get() 
     m <- solve(data,...) # calculates the inverse of the matrix
     x$setinv(m)
     m
}
