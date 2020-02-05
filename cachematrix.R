## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This fuction cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) { 
  a <- NULL
  set <- function(y){
  x <<- y
  a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function calculates inverse of the matrix returned by makeCacheMatrix, If the inverse has already been calculated 
## for example the matrix has not changed then cacheSolve function will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    a <- x$getinverse()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setinverse(a)
    a
}
