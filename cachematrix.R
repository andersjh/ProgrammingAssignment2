## Put comments here that give an overall description of what your
## functions do
# MakeCacheMatrix is called to create an matrix and inverse matrix cache 
#   (much like a class in java and other OO languages).
#   You can send it a matrix or send nothing and add the matrix via the set method.
#   Until cacheSolve is called the first time the inverse matrix is null.
# cacheSolve takes the MakeCacheMatrix variable and checks to see if the inverse has
#   already been calculated and cached.  If it has it returns the cached inverse stored in 
#   the object created by MakeCacheMatrix (along with a message).  Otherwise it calculates 
#   the inverse and stores it in the MakeCacheMatrix object.  
#   Regardless the inverse matrix is always returned.

## Write a short comment describing this function
# Create an object to hold a matrix and it's inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Write a short comment describing this function
# Return chached inverse matrix if available, otherwise calculate it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
