## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix funcion creates, sets and gets the values of a matrix
## is also has two methods, setinverse and getinverse that are used by cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    X <- NULL
    if(length(x)) {
      # I <- solve(x)
      X <-x
    }  
  set <- function(y) {
    x <<- y
    I <<- NULL
    ## print(I)
  }
  get <- function() x
  setinverse <- function(inv) I <<- inv
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## cacheSolve retrieves from I the inverse matrix if cached and matrix did not change 
## if this is the first time, it calculates the inverse matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get()
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  else {
    i <- solve(m)
    x$setinverse(i)
    i
  }  
}


