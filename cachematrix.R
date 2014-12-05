## This is Programming Assignment 2 from https://class.coursera.org/rprog-016
## This code is an example of Caching the Inverse of a Matrix. It is assumed that the matrix is always invertible.
## The first function makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## The second function cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve() should retrieve the inverse from the cache.

## makeCacheMatrix() does the following: it sets the matrix, gets the matrix, sets the inversed matrix, gets the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() gives the inversed matrix created with the above function. However, it first checks to see if the inversed matrix has already been calculated. If so, it gets the inversed matrix from the cache and skips the computation. Otherwise, it calculates the inversed matrix and sets the inversed matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Test
## > x <- rbind(c(1, 2, 0), c(2, 0, 1), c(0, 2, 2))
## > m <- makeCacheMatrix(x)
## > m$get()
##      [,1] [,2] [,3]
## [1,]    1    2    0
## [2,]    2    0    1
## [3,]    0    2    2
## > cacheSolve(m)
##      [,1] [,2] [,3]
## [1,]  0.2  0.4 -0.2
## [2,]  0.4 -0.2  0.1
## [3,] -0.4  0.2  0.4
## > cacheSolve(m)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  0.2  0.4 -0.2
## [2,]  0.4 -0.2  0.1
## [3,] -0.4  0.2  0.4
