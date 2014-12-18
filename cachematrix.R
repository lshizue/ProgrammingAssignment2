## Put comments here that give an overall description of what your
## functions do
## function 'makeCacheMatrix' defines the object with its methods, 
## but the inverse of the matrix is defined afther calling the 'cacheSolve' function

## Write a short comment describing this function
## function 'makeCacheMatrix' receives an invertible matrix and has get and set methods 
## and its invertion. It returns an object 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x

  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}


## Write a short comment describing this function
## function 'cacheSolve' receives the matrix and return the inverted matrix 
## if the inverted matrix was set, it returns the cached version, otherwise R 
## solves the matrix and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { # checking if it's already cached and return it
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...) # caching the inverted matrix
  x$setinverse(m)
  m
  
}
