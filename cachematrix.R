
## The pair functions makeCacheMatrix and cacheSolve compute the inverse of
## a matrix. If the inverse has already been computed, the cached copy is returned


## The makeCacheMatrix function returns a list of functions 
## the set, get, getInverse and setInverse. The last 3 functions 
## would be used by the function computing and returning the inverse (cacheSolve)



makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## Create the get and set functions 
    
  set<- function(y)
  {
    ## Everytime the matrix changes, the inv variable has to be made Null
    x<<- y
    inv<<- NULL
  }
  get<- function()x
  
  getInverse<- function() inv
  setInverse<- function(inverse) inv<<- inverse
  
  list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
  

}


## The cachesolve function returns the inverse of the matrix passed as argument
## If the inverse had already been computed earlier, then the inverse is
## not computed again, but instead the cached copy is returned. 
## However, if the matrix has changed, then the inverse is computed and this 
## newly computed inverse is returned.

cacheSolve <- function(x, ...) {
    
  a<- x$getInverse()
  
  ## Check to see if the inverse is NULL
  if(!is.null(a))
  {
    ## Return the cached copy since the value is not NULL which means
    ## the matrix has not changed and the inverse can be read from the cached 
    ## copy
    message("Returning the cached copy of the inverse")
    return(a)
  }
  else
  {
    ## The inverse has been set to NULL.The only way this could have happened
    ## is in the set function. Hence, the matrix must have been a changed one
    ## or a new one. Hence compute the inverse again and return the new value
    data<- x$get()
    a<- solve(data)
    x$setInverse(a)
    a
    
  }
  
  
}
