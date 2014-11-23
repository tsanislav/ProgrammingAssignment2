# The makeCacheMatrix function creates a list, which contains the function to
#     - set the value of a square matrix
#     - get the value of a square matrix
#     - set the value of the inverse matrix (solve)
#     - get the value of the inverse matrix (solve)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m

  # Below we create the list with the set/get values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

# The cacheSolve function returns the inverse of the matrix,
# checking if it has already been calculated. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  # If the m variable already contains the inverse values
  # of the matrix, it will return the message "getting cached
  # data", along with the inversed matrix.
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# This is work uses code adjusted from the "Caching the Mean of a 
# Vector" sample, provided at the Programming Assignment 2 in the
# course R Programming at Coursera.

## EXAMPLE

# x <- matrix(1:4, 2,2)
# m <- makeMatrix(x)
# cacheInverse(m)
#
#       [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5
#
# cacheInverse(m)
#
#   getting cached data
#       [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5