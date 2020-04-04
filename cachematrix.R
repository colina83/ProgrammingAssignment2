## Put comments here that give an overall description of what your
## functions do

## The fuinction herein will cached the inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
  # Initialise to NULL
  cachedInverse <- NULL
  
  # The original matriz
  get <- function() x
  
  # set will reinitialise the cached variable to Null
  # Double check if is the same matrix
  set <- function(matrixU)
  {
    x <<- matrixU
    cachedInverse <<- NULL
  }
  
  # Inverse the matrix and sabes to cached memory
  setInverse <- function()
  {
    cachedInverse <<- solve(x)
  }
  
  # gets the cached inverse
  getInverse <- function() cachedInverse
  
  # Return a list of all the methods used
  list(get = get, set= set, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  val <- x$getInverse()
  if(!is.null(val))
  {
    message("getting the cached data")
    return(val)
  }
  
  ## Setting the inverse of a matrix
  x$setInverse()
  
  ## parses de value
  inverseMatrix <- x$getInverse()
  
  ## Return value
  inverseMatrix
}

cacheMatrix <- makeCacheMatrix(matrix(1:4,2,2,4))
cacheMatrix$get()
inverseMatrix <- cacheSolve(cacheMatrix)
inverseMatrix
inverseMatrix <- cacheSolve(cacheMatrix)
inverseMatrix


