## The functions provide an efficient way to calculate
## the inverse of a matrix.

## Step 1: Cache a computed value


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cache for the inverse
  set <- function(y) {
    x <<- y  # Set the matrix
    inv <<- NULL  # Reset the cached inverse
  }
  get <- function() x  # Get the matrix
  setinverse <- function(inverse) inv <<- inverse  # Set the cached inverse
  getinverse <- function() inv  # Get the cached inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Step 2: Calculate inverse only if it has not been calculated right before

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data") # Jackpot! No need to recalculate.
    return(inv)  # Return the cached inverse
  }
  data <- x$get()  # Get the matrix
  inv  <- solve(data, ...)  # Compute the inverse
  x$setinverse(inv)  # Cache the inverse
  inv  # Return the inverse
}
