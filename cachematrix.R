makeCacheMatrix <- function(x = matrix()) {
  e <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) e <<- inverse
  getInverse <- function() e
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

##Please include your own comment to explain your code (Required in Rubric)


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  e <- x$getInverse()
  if(!is.null(e)){
  message("getting cached data")
  return(e)
  }
  mat <- x$get()
  e <- solve(mat,...)
  x$setInverse(e)
  e
}
