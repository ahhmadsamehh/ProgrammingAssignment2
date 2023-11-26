## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- solve(x)
  set <- function(mat=x) {
    matrix <<- mat
    inverse <<- solve(mat)
  }
  get <- function() matrix
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

trial <- makeCacheMatrix(matrix(c(1:4), nrow = 2))
inverse_matrix <- cacheMatrix$getInverse()
inverse_matrix
current_matrix <- cacheMatrix$get
current_matrix
changed_inverse <- trial$setInverse(c(1,2,3,4))
hh <- cacheMatrix$getInverse()
hh

cacheMatrix$set(matrix(c(4:7),nrow = 2))



## Write a short comment describing this function

cacheSolve <- function(inverse_matrix = matrix(), object = object()) {
  inv <- inverse_matrix
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  mat <- object$get()
  inv <- solve(mat)
  object$setInverse(inv)
  inv
}
inverseResult <- cacheSolve(inverse_matrix,trial)
inverseResult
