#Programming Assignment 2 - Lexical Scoping
#R Suite running on Debian Linux
#Robert Taulty

# A Matrix object is created and it's inverse is computed and cached




#makeCacheMatrix
#the object t storing the matrix is inialized and the object m for the inverse
#the results is returned as an element in a list. 

makeCacheMatrix <- function(t = matrix()) {
  m <- NULL
  set <- function(y) {
    t <<- y
    m <<- NULL
  }
  get <- function() t
  setInverse<- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}
#cacheSolve
#the inverse of the object passed x is stored in m
#we return m from the cached if it has already been computed

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}