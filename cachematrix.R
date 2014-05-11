# Functions to cache computations of the inverse of a matrix.
#
# Uses `solve` to compute the inverse of a square matrix.
#
# This should be true modulo rounding errors:
# for all integer n > 0
#   m <- makeCacheMatrix(replicate(n, rnorm(n)))
#   all.equal(diag(n), m$get() %*% cacheSolve(m))
#   all.equal(cacheSolve(m), solve(m$get()))

# makeCacheMatrix creates a wrapper around the original x matrix allowing to
# query and set the inverse value as well as query the original matrix

makeCacheMatrix <- function(x = matrix()) {
  # xinv contains the inverse of matrix x if previously computed or NULL
  xinv <- NULL
  # replaces value of current matrix and reset xinv
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  # retrieves value of current matrix
  get <- function() x
  # set xinv to its value
  setinv <- function(inv) xinv <<- inv
  # get xinv from cache (can return NULL)
  getinv <- function() xinv
  # return our list wrapper
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve returns cached inverse of matrix `x` or computes it, caches it
# then returns it.
cacheSolve <- function(x, ...) {
  # try cached value first
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  # not found, compute inverse of matrix as xinv, cache xinv and return xinv
  data <- x$get()
  xinv <- solve(data)
  x$setinv(xinv)
  xinv
}
