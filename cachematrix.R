## object for storing matrices with cached inverse matrix value

## method to create and manage the object
## x = original matrix, i = the inverse matrix (solve(x))
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { # change matrix, set cached inverse to NULL
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## solve function for the special matrix object
## return cached inverse if exists, otherwise calculate and cache it
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i                    ## return inverse of 'x'
}
