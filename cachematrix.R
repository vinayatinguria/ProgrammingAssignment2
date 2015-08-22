##Matrix inversion is usually a costly computation and there may be some benefit  
##to caching the inverse of a matrix rather than compute it repeatedly. The  
##assignment is to write a pair of functions that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object that can cache
##its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInver <- function(inverse) inver<<- inverse
  getInver <- function() inver
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse from 
##the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getInver()
  if(!is.null(inver)) {
    message("getting cached data of the inversed matrix")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data) 
  x$setInver(inver)
  inver
}
