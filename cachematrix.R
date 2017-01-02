## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## will calculate the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it instead of calculating it again

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
      set <- function(z) {
            x <<- z
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## Write a short comment describing this function
              
## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve will retrieve it
## while if the is inverse is not availiable, the function will
##compute it, cache it , and then return it               
              

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
