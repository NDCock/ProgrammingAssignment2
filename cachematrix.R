## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    n <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
      n <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    setold <- function(old) n <<- old
    getold <- function() n 
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv,
         setold = setold,
         getold = getold)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
    if(!is.null(inv)) {
      old <- x$getold()
      if(identical(old,x)){ #Check if the matrix has been changed
      message("getting cached data")
      return(inv)
      }
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    x$setold(x)    #Change the value of the "old" matrix
    inv
}
