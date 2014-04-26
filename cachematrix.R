## Function to initialize a matrix which can be cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  
  # Sets the matrix specified by the user
  set <- function(y)
  {
    x <<- y
    inverse <- NULL
  }
  
  # Retrieves the matrix specified by the user.
  get <- function() x
  
  # Sets the inverse computed for the matrix to the variable "inverse"
  setInverse <- function(mat) inverse <<- mat
  
  # Retrieves the cached inverse of a matrix
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the matrix made in the makeCachematrix() function.
## If found that inverse has been computed, we return the inverse saved in memory.
cacheSolve <- function(x, ...) 
{
  # Gets the inverse of the matrix 
  inverse <- x$getInverse()
  
  # If the inverse has been already computed, notify the user else compute inverse.
  if(!is.null(inverse))
  {
    message("Getting cached data")
  }
  else
  {
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
  }
  
  # Return the inverse of the matrix.
  inverse
}
