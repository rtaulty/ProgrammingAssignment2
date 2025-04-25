# This set of functions stores a matrix and caches its inverse to avoid re computation.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL  # This will store the cached inverse
  
  set <- function(y) {
    x <<- y       # Assign new matrix
    matinv <<- NULL  # Clear cached inverse
  }
  
  get <- function() x  # Return the current matrix
  
  setinverse <- function(inverse) matinv <<- inverse  # Cache the inverse
  
  getinverse <- function() matinv  # Return the cached inverse (if any)
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix if it's not already cached.

cacheSolve <- function(x, ...) {
  matinv <- x$getinverse()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)  # Return cached result
  }
  
  data <- x$get()
  
  matinv <- solve(data, ...)  # Compute inverse
  
  x$setinverse(matinv)        # Cache the result
  
  matinv                      # Return the newly computed inverse
}