## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  2nd assignment for the course R Programming
makeCacheMatrix <- function(x = matrix()) {
  # defining the variables to be returned
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solved_inv) inv <<- solved_inv
  getinv <- function() inv
  
  # return a list of variables
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) { # if inv is already computed
    message("getting cached inverse matrix")
    return(inv)
  } 
  message("computing the inverse matrix")
  inv <- solve(x$get())
  x$setinv(inv)
  inv # Return a matrix that is the inverse of 'x'
       
}



################## below are the sample codes ################## 

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
} 
