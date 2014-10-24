## Two functions to chache the inverse of a matrix


## Create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  # Method to set the matrix
  set <- function(matrix){
      x <<- matrix
      m <<- NULL
  }
  
  # Method the get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setmatrix<-function(solve) m <<- solve
  
  # Method to get the inverse of the matrix
  getmatrix <- function() m
  
  # List of methods
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# Compute the inverse of the matrix returned by "makeCacheMatrix",
# unless the inverse has already been calculated. If the matrix has not
# changed, then "cachesolve" retrieves the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  
  m <- x$getmatrix()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}