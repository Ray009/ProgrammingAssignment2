## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.  The cacheSolve function computes and returns 
# the inverse of he special matrix object created my makeCacheMatrix

## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.  This matrix object contains methods to set and
## get the values of the matrix and the matrix inverse

makeCacheMatrix <- function(x=matrix()) {
      v <- NULL
      set <- function(y) {
            x <<- y
            v <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) v <<- inverse
      getinverse <- function() v
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" object 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x,...) {
      v <- x$getinverse()
      if(!is.null(v)) {
            message("getting cached data")
            return(v)
      }
      data <- x$get()
      v <- solve(data)
      x$setinverse(v)
      v
}

