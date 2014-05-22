# The following functions permit one-time calculation of a matrix inverse, since 
# this can be a time-consuming operation.  The matrix inverse is calculated when
# first requested, then cached in a special "matrix" object which has accessor
# and mutator (get and set) functions.  Subsequent requests for the matrix inverse
# (calls to the "cacheSolve" function) retrieve the matrix inverse from the cache,
# rather than re-calculating it each time.


# Create a special "matrix", can store (cache) the inverse of the matrix,
# and which contains functions to get/set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
     
     # Initialize the matrix inverse as NULL
     matrixInverse <- NULL
     
     # Function to set the matrix in this "object".  Initialize the inverse as NULL
     set <- function(y) {
          x <<- y
          matrixInverse <<- NULL
     }
     
     # Function to retrieve the matrix stored in this "object"
     get <- function() x
     
     # Function to calculate and store the matrix inverse in this "object"
     setInverse <- function(solve) matrixInverse <<- solve
     
     # Function to retrieve the matrix inverse from this "object"
     getInverse <- function() matrixInverse
     
     # Return a list whose members are the functions defined here in 'makeCacheMatrix'
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
     
     # call the input parameter's 'getInverse' function to get the input matrix's inverse from cache
     matrixInverse <- x$getInverse()
     
     # If the returned matrix inverse is not null, then the inverse has already been calculated -
     # return the matrix inverse that was retrieved from the cache
     if(!is.null(matrixInverse)) {
          message("getting cached data")
          return(matrixInverse)
     }
     
     # Otherwise, calculate the inverse of the matrix that was passed in,
     # and use the matrix object's 'set' function to cache the inverse in
     # the matrix object.  Then return the matrix inverse that was just
     # calculated
     data <- x$get()
     matrixInverse <- solve(data, ...)
     x$setInverse(matrixInverse)
     matrixInverse
}



