## Put comments here that give an overall description of what your
## functions do
# These functions are used to save time by computing and cacheing the inverse of a martrix.  Then, if the inverse matrix is requested, it is called from the cache instead of computing the inverse of the matrix again.  



## This function creates a list of functions that (in order):
#1. Sets the value of the matrix.  
#2. Gets the value of the matrix
#3. sets the value of the inverse of the matrix
#4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  matrixinv<- NULL  #sets the initial value of the inverse to NULL
  set <- function(y) {   #function to set the value of the matrix.
    x <<- y
    matrixinv <<- NULL
  }
  
  get <- function() x  #function to retrieve the value of the matrix
  setinverse <- function(inverse) matrixinv <<- inverse #function to set the value of the inverse of the matrix
  getinverse <- function() matrixinv #function to get the value of the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse,  #create a list of the 4 functions above
       getinverse = getinverse)
}




## Write a short comment describing this function
#This function returns the inverse of the matrix.  First, it checks if the inverse has already been calculated.  If so, it returns that result.  If not, then the inverse the inverse is computed and set in the cache (setinverse function).
cacheSolve <- function(x, ...) {
  
  matrixinv <- x$getinverse()  #gets the inverse of the matrix
  if(!is.null(matrixinv)) {    #function to test if inverse has been computed
    message("getting cached data")
    return(matrixinv)
  }
  data <- x$get()   # gets the matrix so the inverse can be calculated
  matrixinv <- solve(data)   #finds the inverse of the matrix
  x$setinverse(matrixinv)   #sets the inverse of the matrix
  matrixinv   #returns the inverse of the matrix
  
}
