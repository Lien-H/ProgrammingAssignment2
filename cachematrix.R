## makeCacheMatrix: creates a special matrix object that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initialize inverse cache
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset cached inverse when matrix changes
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse  # cache the inverse
  getinverse <- function() inv  # retrieve the cached inverse
  
  # Return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: retrieves inverse matrix if it is already stored, 
##otherwise it computates it
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # get cached inverse
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)  # return cached inverse
  }
  
  mat <- x$get()         # get the matrix
  inv <- solve(mat, ...) # compute the inverse
  x$setinverse(inv)      # cache the inverse
  inv                    # return the inverse
}


## test 1
my_matrix <- matrix(c(4, 7, 2, 6), nrow = 2)
cachedMatrix <- makeCacheMatrix(my_matrix)
inv1 <- cacheSolve(cachedMatrix)
print(inv1)
inv2 <- cacheSolve(cachedMatrix)
print(inv2)

## test 2
cachedMatrix$set(matrix(c(1, 2, 3, 4), nrow = 2))
inv3 <- cacheSolve(cachedMatrix)
print(inv3)
