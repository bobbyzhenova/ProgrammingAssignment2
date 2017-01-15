## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##initialize the inverse to be null
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##write function to return matrix
  get <- function() x
  
  ##function to set cached inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ##function to get cached inverse
  getinverse <- function() inv
  
  ##return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ##attempts to set inverse
  inv <- x$getinverse()
  ##if there is already a calculated inverse, return it
  if (!is.null(inv)){
    message("returning cached inverse")
    return (inv)
  }
  ##if we have not exited the function yet, calculate a new inverse
  data <- x$get()
  inv <- solve(data)
  ##and cache it
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
