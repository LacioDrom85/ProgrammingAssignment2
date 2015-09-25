## INTRO:
#  We write a function ables to cache potentially time-consuming computations.
#  If the computation requires more time-consuming, it may make sense to cache the
#  result of the computation so that we can use it when we need it again rather than
#  recomputed it.
#  To this purpose, we build the following two funcion.
#  Note: for this assignment, assume that the matrix supplied is always invertible.
#        solve(X) is the function used to compute the inverse of square matrix.

#  The first function "makeCacheMatrix" creaate a special "matrix" object, containing a
#  function to: - set the value of the matrix (set),
#               - get a value of the matrix (get),
#               - set the value of the inverse matrix (setinverse)
#               - get the value of the inverse matrix (getinverse)


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Generate the null object or Update for a new object
  set <- function(y) { # Set the object y with x
    x <<- y
    inv <<- NULL
  }
  get <- function() x # Return the object x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The secong function "cacheSolve" calculates the mean of the special "matrix"
#  created with the above function. However, it first checks to see if the inverse
#  matrix has already been calculated. If so, it gets the inverse matrix from the
#  cache and skips the computation. Otherwise, it calculates the inverse matrix of
#  the data and sets the value of the inverse matrix in the cache via the setmean
#  function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) { # Check if the inverse matrix is already computed
    message("getting cached data.") # Display the message and return the inverse
    return(inv)                     # without any calculation (already computed)
  }
  data <- x$get()
  inv <- solve(data) # Compute the inverse because it doesn't have been computed
  x$setinverse(inv)  # before. Set the resuts in the "setinverse()" and it displays
  inv                # the inverse.
}
