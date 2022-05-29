## Put comments here that give an overall description of what your
## functions do

## The functions solve and cache the inverse of a matrix. If the same matrix
## is called again, it will return the previously cached inverse

## Write a short comment describing this function

## This function creates an object to cache the inverse of our matrix x

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  }
  get <- function() x 
  setInverse <- function(solveInverse) inv <<- solveInverse 
  getInverse <- function() inv 
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse) 

}


## Write a short comment describing this function

## This function calculates the inverse of the object created above
## If the matrix is unchanged and there is already an inverse calculated
## cacheSolve returns the previously calculated inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}