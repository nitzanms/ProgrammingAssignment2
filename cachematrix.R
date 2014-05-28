## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that takes a matrix and turns it into a
## cache matrix. Cache matrix is a data type that can store both a matrix 
## and a cached version of its inverse.
## Arguments: x (optional) a matrix to be cached.
## Returns: a data type containing the matrix and capable of caching its 
##          inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve accepts a cache matrix and either returns its cached inverse
## if it exists or solves the inverse and caches it in the cache matrix.
## Arguments: x cache matrix.
## Returns: a matrix that is the inverse of the cache matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
