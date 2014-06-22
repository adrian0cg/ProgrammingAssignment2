## Module that allows cached comutations upon a Matrix
## Out of scope: Checks for changed data

## Creates a data structure, that holds the data, the cache and getters/setters for the values

makeCacheMatrix <- function(x = matrix()) {
  # set the cached value to NULL, indicating invalid cache
  inv <- NULL
  # data setter
  set <- function(y) {
    x <<- y
    inv <<- NULL # invalidate cache
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv
  )
}


## solves the computation possibly using a cached value

cacheSolve <- function(x, ...) {
  # try retrieve from cache
  inv <- x$getInv()
  if (!is.null(inv)) { # HIT!
    message("retrieving matrix from cache")
    return(inv)
  }
  ## miss
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv) # store
  inv
}
