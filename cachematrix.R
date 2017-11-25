## This routine is comprised of two functions;
## makeCacheMatrix is the first function and takes a matrix as in input, this is then passed to the second function.
## makeCacheMatrix will return the inverse matrix establised in the previous function, but will call upon a cached version if it exists.

## makeCacheMatrix will estaablish a matrix which is passed on to the subsequent function, so it's inverse can be returned. 

makeCacheMatrix <- function(m = matrix()) {           ## Define makeCacheMatrix function.
  i <- NULL                                           ## Make inverse varible null for later.
  set <- function(y) {                                ## Set function using variable y.
    m <<- y                                           ## Define matrix, m with new operator item.
    i <<- NULL                                        ## Nulls inverse varible after set (2nd time)
  }
  get <- function() m                                 ## Define get function.
  setInverse <- function(inverse) i <<- inverse       ## Set the inverse function.
  getInverse <- function() i                          ## Define getInverse function.
  list(set = set, get = get,                          ## Create list, name each element.
       setInverse = setInverse,
       getInverse = getInverse) 
}

## cacheSolve returns the inverse of a matrix defined by above, but will call upon the cached version if it exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()                                 ## Retrieve inverse matrix.
  if (!is.null(i)) {                                  ## Check if already cached.
  message("Getting cached data")                      ## Diaplay message if cached.
  return (i)                                          ## Display cached data.
}
  data <- x$get()                                     ## Obtain matrix, called data.
  i <- solve(data, ...)                               ## Calculate inverse of matrix.
  x$setInverse(i)                                     ## 
  i                                                   ## Display inverse matrix.
}
