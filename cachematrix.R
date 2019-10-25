## The function makeCacheMatrix returns a list of functions
## which can be used to store a matrix, and store the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the matrix
## If the inverse exists, the inverse is returned
## If the inverse does not exist, the matrix is stored, the inverse is calculated
## and the inverse is returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
## If the cacheSolve function is called again, the cached inverse matrix 
## will be returned to avoid any computation
