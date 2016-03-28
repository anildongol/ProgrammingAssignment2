#Function creates Special Matrix 
makeCacheMatrix <- function(x = matrix()) {
  invrs<- NULL
  set <- function(y) {
    x <<- y
    invrs<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invrs<<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#Function calculate inverse of the matrix created by above function. First it check if inverse has already been calculated or not.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs<- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  invrs<- solve(mat, ...)
  x$setInverse(invrs)
  inv
}