## Week 3 programming assignment: demonstrate use of lexical scoping
## to build a computation cache

## Given a matrix, return a list that implements a simple abstract
## data type with four operations: 'set', 'get', 'setinverse', and
## 'getinverse'.  The ADT is implemented as a list of four functions.
## If not matrix is supplied, an empty matrix will be created by default.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }  
  get <- function() {
    x
  } 
  setinverse <- function(inverse) {
    cache <<- inverse
  }
  getinverse <- function() {
    cache
  }  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Given a list created by makeCacheMatrix(), compute the
## inverse of the stored matrix.  Once computed, the result is
## cached for subsequent calls.

cacheSolve <- function(x, ...) {
  if (class(x) != 'list' || length(x) != 4) {
    stop("Argument is not a cacheable matrix!")
  }
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (! is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat)
  x$setinverse(inverse)
  inverse
}
