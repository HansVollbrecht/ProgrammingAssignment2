## Put comments here that give an overall description of what your
## functions do
## my function creates a cacheable type of matrix that stores the inverse for square matrices

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## inverse of matrix is called i
  i <- NULL
  set <- function(y) {
  	d=dim(x)
  	if (d[1]==d[2]) {
    	x <<- y
    	i <<- NULL
    } else {
    	warning("Please provide square matrix.")
    }
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## here we calculate or retrieve the cached inverse of the special matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
