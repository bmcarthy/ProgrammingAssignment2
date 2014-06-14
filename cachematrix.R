## These R functions take advantage of R language's scoping rules to cache
## the results of the solve function.  The solve function is used to create
## the inverse of a square matrix. This can be a very a time-consuming 
## function. Therefore, cacheing the results of the function has the potential
## to significantly reduce processing time in situations requiring multiple 
## iterations of the same solve process. If the contents of the input matrix
## changes then the value of the inverted matrix will be retrieved from the
## cache otherwise the value will be recalculated.


## The makeCacheMatrix function creates a matrix that will be used to store
## the output of the solve function.  In addition, it creates a list of
## functions that will be used by the cacheSolve function. 
	
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y, n) {
    x <<- matrix (y, nrow=n, ncol=n)
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve)
  i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function first check to see if the inverse of the submitted matrix
## is already present in the cache and if so it returns that value. 
## If the value is not found in the cache then the cacheSolve function 
## computes the inverse of the matrix and stores it in the cache matrix
## created by the  makeCacheMatrix function. 

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

