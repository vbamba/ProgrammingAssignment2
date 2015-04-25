## cacheMatrix, provides two functions that compute the inverse of a matrix and cache it 
# for future use. The first function makeCacheMatrix takes a matrix as a single argument. 
# The matrix is assumed to be inversible. It returns a list of four helper functions to get & set a 
# matrix, and to get and set its cached inverse
# The second function cacheSolve computes the inverse of a matrix and returns its value


## This function creates a special "matrix" object that can cache its inverse. The function 
## is comprised of a list of four functions (set, get, setinverse, getinverse). get returns
## the current matrix, set is use to set the matrix to a new matrix, getinverse returns the 
## cached copy of the inverse, setinverse sets the cache to the new value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    get <- function() x
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(get = get,
         set = set,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cached copy 
## is returned. Otherwise we assume the current matrix is inversible and use solve(X) to inverse the 
## matrix and return the result

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}


