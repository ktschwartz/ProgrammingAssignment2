## This set of functions will calculate the inverse of a matrix and 
## cahce the results

## The input for makeCacheMatrix is a matrix (x).
## The function creates an object that caches the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of the object created in 
## makeCacheMatrix.  If the invere has laready been calculated 
## then the cached result will be retrieved.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i      
  
  ## Return a matrix that is the inverse of 'x'
}
