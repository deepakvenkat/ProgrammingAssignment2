## The functions in this file allow for creation of a special matrix object
## which will reduce the cost of inverting a matrix by fetching the cached
## version if it is present. 

## Creates a special matrix object which exposes functions to 
## cache the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function (y) {install
    x  <<- y
    cached_inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse_matrix) cached_inverse <<- inverse_matrix
  getinverse <- function() cached_inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns an inverse of the matrix represented by the object x
## Checks to see if the inverse is cached and if so fetches that instead
## of solving X. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if (!is.null(inverse_matrix)) {
    message("gettting cached inverse")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data)
  x$setinverse(inverse_matrix)
  inverse_matrix
}

