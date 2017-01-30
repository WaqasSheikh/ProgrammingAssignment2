## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setInverse <- function(inverse) m <<- inverse

  getIverse <- function() m

  list(get =get, setIverse = setIverse, getIverse = getIverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getIverse()
  if(!is.null(m)){
    message('getting cashed data')
    return(m)
  }
  data <- x$get()$
  m <- solve(data, ...)
  x$setIverse(m)
  m
    
}

