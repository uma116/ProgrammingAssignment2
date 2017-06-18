
#Assignment 2 : Caching the inverse of a Matrix
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute 
#it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
#The below paiir of functions caches the inverse of a matrix.


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  
  invse <- NULL
  
  set <- function(y) 
  {
    
    x <<- y
    
    invse <<- NULL
    
  }
  
  get <- function() x
  
  setinvse <- function(inverse) invse <<- inverse  
  
  getinvse <- function() invse
  
  list(set = set, get = get,
       
       setinvse = setinvse,
       
       getinvse = getinvse)
  
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  
  invse <- x$getinvse()
  
  if(!is.null(invse)) 
  {
    
    message("getting cached data")
    
    return(invse)
    
  }
  
  data <- x$get()
  
  invse <- solve(data, ...)
  
  
  x$setinvse(invse)
  
  invse
  
}  


