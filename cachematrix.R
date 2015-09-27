## Coursera JHU R Programming, Assignment 2
## NOTE: based on structure of example in assignment instructions
## FUNCTIONS:
##    MakeCacheMatrix: creates matrix class that can store inverse
##    cacheSolve: calculates the inverse and updates the instance passed to it

## Define a class-like object that allows the inverse of a matrix to be
## stored with the original matrix to avoid re-computation
makeCacheMatrix <- function(x = matrix()) {
  
  x_inv <- NULL
  
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) x_inv <<- inv
  getinv <- function() x_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Solve the inverse of passed matrix if necessary:
##    if so, store the inverse so it doesnt need to be recalculated
##    if not, just return the previously solved inverse
cacheSolve <- function(x, ...) {
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinv(x_inv)
  x_inv
}
