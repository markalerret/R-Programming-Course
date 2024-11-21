#These functions take a matrix, compute and cache the inverse, then return the inverse

#this function takes a matrix, calculates the inverse of the matrix, and
#caches the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = get,
       get = get,
       setinverse = setinverse,
       getinverse = setinverse)
}


#this function checks to see if the inverse of the matrix is already cached.
#it returns the inverse if it is, and computes, caches and returns it if not

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
}