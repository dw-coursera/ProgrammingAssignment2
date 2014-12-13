## Caching of the results of possibly expensive functions
## with the help of the <<- operator.

## Create a special matrix that is able to cache its inverse.
## We assume that we always get an n-by-n matrix.

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

## Returns the inverse of a special matrix created by the
## function makeCacheMatrix
## Checks if the inverse has already been calculated. If so,
## it gets the result from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data<-x$get()
  i <- solve(data, ...) #TODOOOOO
  x$setinverse(i)  
  i
}
