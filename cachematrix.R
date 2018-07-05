## This set of functions provide a facility for matrix inverse operation.
## To optimize the cost of the inverse operation, the result of the inverse operation for a given matrix is cached
## and returned during subsequent invocation. 
## To encapsulate this feature a higher level Datastructure CacheMatrix is created.

## CacheMatrix Datastructure is a list that encapsulates a Matrix and its memoized inverse in a Closure 
## and makes them accessible through list's element functions set , get , setinverse and getinverse


## makeCacheMatrix function is a factory that creates a CacheMatrix Datastructure (List) for a given Matrix
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


## cacheSolve function accepts a List (that represents a CacheMatrix Datastructure) and 
## returns the inverse of the matrix encapsulated by the List. 
## If the inverse is previously calculated it returns the existing cached inverse.
cacheSolve <- function(x, ...) {
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
