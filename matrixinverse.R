## script consists of 2 functions
## 1st function sets the value of matrix and its inverse and gets the value of matrix and its inverse
## assumed that the matrix given as input here are all invertible

makecachematrix <- function(x = matrix()){
  inv <- NULL
  
  ## setting the value of the matrix
  set <- function(y){
    x <<- y ## double arrow assignment helps to preserve the environment of the parent function and have access to all the variables and parameters of the parent function
    inv <<- NULL
  }
  
  ## get function to get the value of matrix
  get <- function(){x}
  
  ## setting the value of the inverse
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## getting the value of the inverse
  getinverse <- function(){
    inv
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## function to solve the inverse of matrix
cachesolve <- function(x, ...){
  inv <- x$getinverse()
 
## checking if the inverse is already calculated  
   if(!is.null(inv)){
    message("getting cached data")
    return(inv)}
  
  ## calculating the inverse as it has not been calculated
  else {
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  return(inv)
  }
}
