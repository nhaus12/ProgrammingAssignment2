# Functions that facilitate the caching of the result(s) of matrix inversion calculations.

## Returns a list object containing functions that allow the user to: 
##(1) set the matrix value(s); (2) get (return) the stored matrix; 
## (3) set the inverse of the matrix; (4) get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL
      
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      
      setinv <- function(xinv) inv <<- xinv
      
      getinv <- function() inv
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Returns the inverse of the matrix stored in the object x, either by retrieving an
## already-calculated value or, if not available, 
## calculating the matrix inverse (and caching the result)

cacheSolve <- function(x, ...) {
        
      inv <- x$getinv()
      
      if(!is.null(inv)){
            message("getting cached matrix inverse")
            return(inv)
      }
      
      #Implicit "else" (i.e. if inv is NULL)
      matrix <- x$get()
      inv <- solve(matrix, ...)
      x$setinv(inv)
      
      inv
}

##==========
# Worked example:
#
# sampleMatrix <- matrix(c(4,2,7,6), nrow=2, ncol=2)
# x <- makeCacheMatrix()
# x$set(sampleMatrix)
# cacheSolve(x)