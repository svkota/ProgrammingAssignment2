## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # create a NULL vairable to save inverse of a matrix
  m <- NULL
  
  # set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get the value of the vector
  get <- function() x
  
  # set the value of the inverse of a matrix
  setinv <- function(inv) m <<- inv
  
  # get the value of the inverse of a matrix
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # read the matrix entered
  data <- x$get()
  
  # solve is builtin function to computer inverse of a matrix
  m <- solve(data, ...)
  
  # set the value of the inverse of a matrix
  x$setinv(m)
  m
}

