## This set of functions will calculate the inverse of a matrix and 
## cahce the results so that the computation does not have to be repeated.

## The input for makeCacheMatrix is a numeric matrix called x.
## The function creates a list that is used to cache the inverse of x.

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


## cacheSolve takes as an arguments the output of makeCacheMatrix.
## If the inverse has already been computed it returns the inverse
## along with the message "getting cached data".  Otherwise the 
## inverse is computed and returned.

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
