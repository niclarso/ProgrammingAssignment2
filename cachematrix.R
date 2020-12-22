
#Below are two functions that are used to create a special matrix 
#that stores a matrix and cache's its inverse.


#This first function, makeCacheMatrix creates a special "matrix"
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMat <- function(solve) m <<- solve
  getInverseMat <- function() m
  list(set = set, get = get,
       setInverseMat = setInverseMat,
       getInverseMat = getInverseMat)
}

## The following function calculates the inverse of the special "matrix" created with the above function
cacheSolve <- function(x, ...) {
  m <- x$getInverseMat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMat(m)
  m
}


#Testing
Test = matrix(c(1:4),2,2)
TestCache = makeCacheMatrix(Test)
cacheSolve(TestCache)
cacheSolve(TestCache)
