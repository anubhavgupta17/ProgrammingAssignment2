## This piece of code creates a wrapper over matrix, which caches 
## calculated inverse of a matrix the first time it is calculated and
## returns the cached value each subsequent time the call is made again.

## makeCacheMatrix takes in a matrix and has placeholder for caching
## inverse of the matrix. It has other getters and setters as well.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve function takes in an object of makeCacheMatrix to create
## a cacheable solve function over the new class.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
