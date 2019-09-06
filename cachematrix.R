## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvers <- function(invers) inv <- inverse
  getinvers <- function() inv
  list(set = set, get = get, setinvers = setinvers, getinvers = getinvers)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvers()
  if(!isnull(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  invers <- solve(data, c(1,1),...)
  x$setinvers(invers)
  inv
}
