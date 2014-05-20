## R code for Inverse of a Matrix
## The version was written by Kelly Fitzpatrick 
## The overall goal of this script is to take the inverse of an invertible square matrix
## The below function makeCahseMatrix is creating a cache matrix for an invertible square matrix 
##the below function uses the R function solve to find the inverse

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



## the function below will cache the matrix and return the inverse
## R uses the solve function to calculate the inverse of an invertible square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' using the R function solve
        
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



