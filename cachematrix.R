## makeCacheMatrix takes a matrix as input. It contains subfunctions that can return
## the input and cache and return the matrix's inverse.
## cacheSolve returns the inverse matrix, either from the cache or by computing it
## directly, and saves the result to cache.
## Code was based on the vector functions in the example.

## makeCacheMatrix contains three subfunctions.
## get() returns the input matrix.
## setInv() takes an inverse matrix as input and saves it to the cache.
## getInv() returns the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve checks whether the inverse matrix is stored in the cache.
## If not, it computes the inverse and caches it. Input is an object
## created from makeCacheMatrix().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
