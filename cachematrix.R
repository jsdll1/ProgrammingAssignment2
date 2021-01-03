## Minimize the time spent on calculating the inverse of a matrix by utilizeing
## the cache

## Store a matrix and it's inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solve the inverse of the matrix, or output it from the cache if available

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
