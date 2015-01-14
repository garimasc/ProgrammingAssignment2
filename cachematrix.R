# Programming Assignment number 2
#Aims to avoid excess matrix inversions (a costly process) by caching a matrix and its inverse
#makeCacheMatrix essentially behaves like a class with two variables a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  # x is a numeric matrix
  # we assume here that it is invertible hence no checs forr the same
  inv <- NULL # initially setting inverse as NULL
  set <- function(y) { 
    #set the matrix as a particular value
    x <<- y
    inv <<- NULL # like a constructore intiailaize the value of inverse to NULL
    #acts as a check whether matrix has been changed or not  
  }
  
  get <- function() x # get the value of the matrix
  setInv <- function(i) inv <<- i # set the value of inverse 
  getInv <- function() inv # get the value of inverse
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# function to compute the inverse of a cached matrix 
#returned by the function above
cacheSolve <- function(x, ...) {
  
  inv <- x$getInv() #get the value of inverse 
  #if inv is not null that means the function has been run earlier/ value has been changed
  if(!is.null(inv)) { 
    message("getting cached data")
    return(inv) #  returning the value got from cached matrix
  }
  data <- x$get() # get data for the matrix as a cached matrix is a list not a matrix
  inv <- solve(data) # buil in functio to calculate inverse
  x$setInv(inv) #set the value of inverse
  inv #return the value of inverse
}