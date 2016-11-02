# makeCacheMatrix and cacheSolve functions employ a 'memoization' optimization technique for storing
# a computed matrix inversion in cache to speed up computations. The implementation takes
# advantage of R lexical scoping.

# Assumes that matrix is always invertible (not singular)
# No test for square matrices.
# No conditions or error traps.
# Modeled after makeVector() and cachemean() in Coursera R Programming class.

# Example usage: (a matrix of a million elements between 1 and 10)
#     a <- matrix(sample(1:10, 1000000, replace=TRUE), nrow=1000, ncol=1000)
#     b <- makeCacheMatrix(a)
#     cacheSolve(b) # first call takes time
#     cacheSolve(b) # second call is instanteneous

# creates a special matrix object with get, set, setinverse, and getinverse operations

makeCacheMatrix <- function(mat = matrix()) {
  inv_mat <- NULL
  set <- function(n) {
    mat <<- n
    inv_mat <<- NULL
  }
  get <- function() mat
  setinverse <- function(i_mat) inv_mat <<- i_mat
  getinverse <- function() inv_mat
  list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
}

# tests whether the special matrix object inverse has already been computed. It so it accesses
# the cache directly, otherwise, it computes the inverse and stores it in the cache for future use.

cacheSolve <- function(mat,...) {
  m <- mat$getinverse()
  if (is.null(m)) {
    data <- mat$get()
    m <- solve(data, ...)
    mat$setinverse(m)
  } else {
    message("Getting data from cache")
  }
  m       ## returns a matrix that is the inverse of 'mat'
}
          
