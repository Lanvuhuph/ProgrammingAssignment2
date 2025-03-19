## The makeCacheMatrix function creates a special "matrix" object 
## that can store its inverse in a cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse as NULL
  
  ## Function to set a new matrix value and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Function to get the matrix value
  get <- function() x
  
  ## Function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## Function to get the cached inverse
  getInverse <- function() inv
  
  ## Return a list containing the functions
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the matrix created by 
## makeCacheMatrix. If the inverse has already been calculated and the matrix 
## has not changed, it retrieves the cached result.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if inverse is already cached
  
  ## If the inverse is cached, return it
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  ## If not cached, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse using solve()
  
  ## Store the computed inverse in cache
  x$setInverse(inv)
  
  return(inv)
}
# Create a matrix
my_matrix <- matrix(c(2, 2, 1, 3), nrow = 2, ncol = 2)

# Create a special matrix object
cached_matrix <- makeCacheMatrix(my_matrix)

# Compute the inverse (first time, so it calculates it)
cacheSolve(cached_matrix)

# Compute the inverse again (this time, it fetches from cache)
cacheSolve(cached_matrix)
