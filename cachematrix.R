## Put comments here that give an overall description of what your
## functions do

## Constructor for a matrix object that can cache it's computed inverse.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) { ## Sets the contents of the matrix using argument 'y'
    x <<- y
    cachedInverse <<- NULL ## Re-set cache inverse to null when we have new data
  }
  get <- function() {x} ## Returns matrix data set using the set() function
  setinverse <- function(inverse) {cachedInverse <<- inverse} ## Sets the cached inverse using argument 'inverse'
  getinverse <- function() {cachedInverse} ## Returns the cached matrix inverse
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) ## Create names we can access later
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  cachedInverse <- x$getinverse() ## Checks to see if inverse is cached
  if(!is.null(cachedInverse)) { ## If cached data exists return it
    message("getting cached inverse")
    return(cachedInverse)
  }
  data <- x$get() ## Retrieve data from matrix object
  cachedInverse <- solve(data, ...) ## Find the inverse of the matrix, also pass other arguments specified above.
  x$setinverse(cachedInverse) ## Sets the cached inverse so it can be retrieved later
  cachedInverse
}
