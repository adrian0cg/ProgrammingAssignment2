## Module that allows cached comutations upon a Matrix
## Out of scope: Checks for changed data

## Creates a data structure, that holds the data, the cache and getters/setters for the values

makeCacheMatrix <- function(x = matrix()) {
  # set the cached value to NULL, indicating invalid cache
  inv <- NULL
  # matrix data setter
  set <- function(y) {
    x <<- y
    # invalidate cache
    inv <<- NULL
  }
  # getter for matrix data
  get <- function() x
  # setter for inverse cache
  setInv <- function(inverse) inv <<- inverse
  # getter for store inverse (NULL if empty!)
  getInv <- function() inv
  # return full data structure with functions
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
  ## HIT!
  if (!is.null(inv)) {
    message("retrieving matrix from cache")
    return(inv)
  }
  ## miss
  # load matrix data
  mat <- x$get()
  #compute
  inv <- solve(mat, ...)
  # store
  x$setInv(inv)
  # return computed value
  inv
}
