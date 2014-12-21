## The makeCacheMatrix checks if the matrix inverse has already been cached,
## in which case it returns this value, or else it calculates the matrix inverse and caches the value.

## The function creates a cache matrix, unless it already exists.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setginv <- function(ginv) m <<- ginv
  getginv <- function() m
  list(set = set, get = get,
       setginv = setginv,
       getginv = getginv)
}


## This function calculates the inverse matrix or returns prior cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getginv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setginv(m)
  m
}
