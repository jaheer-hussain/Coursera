
## Invoke the f_test_inv to test the sample matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
  }
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    else
    {
      message("getting NOT cached data")
    }
    datos<-x$get()
    m<-solve(datos, ...)
    x$setmatrix(m)
    return(m)
}

create_m <- function()
{ 
  }
f_test_inv <- function()
{
  ## Create a 3*3 Matrix
  message("Creating a 3*3 matrix for test")
  mat1 <- matrix(c(1,1,1,3,4,3,3,3,4), 3,3)
  print(mat1)
  ## Create the cache of the matrix
  message("Create the cache of the matrix")
  x <- makeCacheMatrix(mat1)
  ## Inverse the matrix by calling the solve function
  message("Inverse the matrix by calling the solve function")
  y <- cacheSolve(x)
  print(y)
  ## Inverse the matrix by calling the solve function to use the Cache.
  message("Inverse the matrix by calling the solve function to use the Cache")
  z <- cacheSolve(x)
  print(z)
}
