## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  set <- function(y) {
    x <<- y
    inv <<- NULL   #this function will set the value of the matrix 
  }
  get <- function() x #it will return the value of the matrix created
  setinvers <- function(invers) inv <- invers  #it will set the inverse of the matrix
  getinvers <- function() inv   #it will return that inverse
  list(set = set, get = get, setinvers = setinvers, getinvers = getinvers) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvers()
  if(!isnull(inv)){
    message("getting cached data")
    return(inv)   #to see if the inverse of that matrix has been calculated before
  }
  data <- x$get()
  invers <- solve(data,...)
  x$setinvers(invers)
  inv  #calculate the inverse of the matrix and store the result for future use
}
