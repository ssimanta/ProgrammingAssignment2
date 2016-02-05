## Put comments here that give an overall description of what your
## functions do

##
##
##  SS on 02/05/2016
##  Function Name: makeCacheMatrix 
##  Description :
##  Description :This function accepts a inversible Square Matrix as an input Parameter, 
##               and returns a list of following  functions:
##               set     = Store the input matrix into an environment variable 
##               get     = Returns the matrix previously stored in the environment variable(x)
##               setinv  = Store the inverse of the matrix in the environment variable
##               getinv  = Returns the inverse previously stored in the environment variable(inv)
##

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL 
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}

##
##  Function Name: cacheSolve 
##  Description  :  This function accepts the output of the function makeCacheMatrix 
##                  and returns the inverse of the Matrix.
##
##
cacheSolve <- function(x,...)
{
  inv = x$getinv()
  # if inverse exists then just retrun the value.
  if( !is.null(inv))
  {
      message("Retrieving catched data")
      return(inv)
  }
  # If inverse does not exist, calculate and return it.
  mat.data <- x$get()
  inv   <- solve(mat.data)
  x$setinv(inv)
  inv
}
