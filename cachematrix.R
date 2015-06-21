## This file contains functions to create an object that contains a matrix and its cached inverse
## In addition, it contains methods to access the cached inverse matrix for efficient repeated use


## This function tries to check for a square matrix 
isSquareMatrix <- function(x=matrix()) {
      if (!is.matrix(x)) {
            message("isSquareMatrix check performed on an object that is not a matrix")
      } else {
            my_dim <- dim(x)
            if (my_dim[[1]] == my_dim[[2]]) {
                  return(TRUE)
            }
      }
      return(FALSE)      
}

## This function makeCacheMatrix allows user to create a matrix and cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL
      set <- function(y = matrix()) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(my_inverse) inv <<- my_inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function cacheSolve allows user retrieve the inverse matrix of a matrix created by makeCacheMatrix
## Until the content of the matrix is changed, repeated calls to retrieve the inverse matrix do not incur 
## any additional computational penalty as the cached inverse matrix is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
      }
      my_matrix <- x$get()
      if (isSquareMatrix(my_matrix)) {
            inv <- solve(my_matrix, ...)
            x$setinverse(inv)
      } else {
            message("cacheSolve called on a non-square matrix")
      }
      inv
}
