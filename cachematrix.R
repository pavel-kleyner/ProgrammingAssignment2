#These functions cache a matrix argument and its inverse
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#First function takes in matrix as argument and returns list that holds cached
#version of matrix and its inverse

makeCacheMatrix<-function(x=matrix()){
  inverse.matrix<-matrix()
  setMatrix<-function(y){
    x<<-y
    inverse.matrix<<-matrix()
  }
  getMatrix<-function() x
  setInverse<-function(inverse.matrix) inverse.matrix<-solve(x)
  getInverse<-function() inverse.matrix
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
##Uses global instance of returns from makeCacheMatrix to get inverse of matrix
#or solve for the matrix's inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse.matrix<-x$getInverse()
  if (!identical(inverse.matrix,matrix())){
    return(inverse.matrix)
  }
  data<-x$getMatrix()
  inverse.matrix<-solve(data,...)
  x$setInverse(inverse.matrix)
  inverse.matrix
}
