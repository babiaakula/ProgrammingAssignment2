makeCacheMatrix <- function(a = matrix()) {
  e <- NULL
  set <- function(y){
  a <<- y
  e <<- NULL
  }
  get <- function()a
  setInverse <- function(inverse) e <<- inverse
  getInverse <- function() e
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

cacheSolve <- function(a, ...) {
## Return a matrix that is the inverse of 'a'
  e <- a$getInverse()
  if(!is.null(e)){
  message("getting cached data")
  return(e)
  }
  mat <- a$get()
  e <- solve(mat,...)
  a$setInverse(e)
  e
}
