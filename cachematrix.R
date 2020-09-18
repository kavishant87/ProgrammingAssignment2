## The following function are used to create a special object that stores a matrix and catches its inverse.
## The Function makeCacheMatrix creates a special MATRIX , which is really a list containing a function to:
## set and get the value of matrix
## set and get the value of the inverse

makeCacheMatrix <- function(x= matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## The function cacheSolve computes the inverse of the special MATRIX returned by makeCacheMatrix function.
## If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if (!is.null(inv)) {
     message("getting cached data")
     return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}




        ## Return a matrix that is the inverse of 'x'
}
