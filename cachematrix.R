## Caching the inverse of a special matrix type

## Special matrix creation

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv<-function(solve) inv <<- solve
  getinv<-function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Caching function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}
