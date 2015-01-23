## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. This file
## contains a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## set the value of the matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x

  ## set the value of inverse
  setinverse <- function(inverse) i <<- inverse

  ## get the value of inverse
  getinverse <- function() i
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## get cached data if exists
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## get the value of the matrix
  data <- x$get()
  
  ## computes the value of the inverse and set the value of computed inverse
  i <- solve(data)
  x$setinverse(i)

  ## Return a matrix that is the inverse of 'x'
  return(i)
}

## Test sample: calculate the inverse of given 3x3 matrix input values
## x<-matrix(c(-1, 3, -3, 0, -6, 5, -5, -3, 1), 
##           nrow=3,              
##           ncol=3,              
##           byrow = TRUE) 

## m<-makeCacheMatrix(x)

## First call (inverse calculated)
## print(cacheSolve(m))

## Second call (inverse from cache)
## print(cacheSolve(m))
