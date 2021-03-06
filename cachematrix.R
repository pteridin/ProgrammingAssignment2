## Functions to efficiently calculate the inverse of a matrix 
## and caches the found inverse for later use


#' "Special" matrix cache object to map matrix values and the respective inverse matrix in cache:   
#' * set the value of the matrix   
#' * get the value of the matrix   
#' * set the value of the inverse matrix   
#' * get the value of the inverse matrix   
#'
#' @param x The matrix to retrieve a 'cache-matrix object'
#'
#' @return A 'Cache-matrix object'
#' 
#' @example 
#' a <- makeCacheMatrix(matrix(c(2,5,1,3), nrow=2))
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.   
#' If the inverse has already been calculated (and the matrix has not changed), 
#' then the cachesolve should retrieve the inverse from the cache.
#'
#' @param x The 'cache-matrix object' to inverse
#'
#' @return An inverse matrix
#' 
#' @example 
#' a <- makeCacheMatrix(matrix(c(2,5,1,3), nrow=2)
#' cacheSolve(a) # => should calculate inverse matrix
#' cacheSolve(a) # => should retrieve cache instead of calculating
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    # is m cached?
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...) 
    x$setinverse(m)
    m
}
