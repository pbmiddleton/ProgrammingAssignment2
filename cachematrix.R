## This source code file contains two functions, makeCacheMatrix and cacheSolve
## cacheSolve will attempt to retrieve a cached value of the matrix inverse
## If it has not been calculated yet, the inverse will be calculated

## This function creates a special "matrix" object
## that can cache its inverse
## Note that it is based on 'makeVector' which is documented in detail in:
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setinverse <- function(solve)
    m <<- solve
  getinverse <- function()
    m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
