## These two functions, makeCacheMatrix and cacheSolve, are designed to 
## inverse a square matrix. If the inverse of the matrix is already in 
## in the cache, cacheSolve function, returns that result. 

## makeCacheMatirx takes a matrix and finds the inverse of it and caches it. 
## it has four function calls under the cover, set, get, setinverse, getinverse. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
        { 
          x <<- y
          m <<- NULL
        }
  
  get <- function() {x}
    
  setinverse <- function(inverse) {m <<- inverse}
  
  getinverse <- function() {m}
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## cacheSolve function checks if the inverse exists, if it does not runs the 
## solve function and stores the result to cache. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) 
      {
        message("getting cached data")
        return(m)
      }
  
  ## if the cache do not exists
  data <- x$get() ## get the original inside matrix
  
  m <- solve(data, ...) ## solve for the inverse
  x$setinverse(m) ## store the resulting matrix to cache
  m
}
