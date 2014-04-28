## cachematrix.R has a pair of functions that caches the inverse of a 
## matrix. solve() is used to calculate the inverse of the matrix.
## If the inverse has been calculated, then the inverse is returned from
## the cache for the same matrix.

## Create a special matrix object that caches its inverse. The functions takes in a 
## matrix as its argument.
## The function returns a list with 4 functions : set,get,setinverse and getinverse.
## get - returns the original matrix
## set - sets a matrix
## setinverse - sets the inverse matrix.
## getinverse - returns the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(solve) {inv <<- solve}
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## The getinverse() is called on the matrix to retrive inverse from 
## the cache if any. If inverse has not been calculcated for the matrix,
## null is returned. The inverse of the matrix is calculated using solve(). 
## The new inverse is then set to the list using setinverse, so that its 
## available in the cache.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the getinverse returns a non null value and cacheSolve wil retrieve 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
