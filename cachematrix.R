## Put comments here that give an overall description of what your
## functions do
## These functions are written to fulfill 2nd assignment for Coursera R programming course.
## This function inverses inputed matrix and caches its value therefore for further calculations
## this does not need to be calculated again.
## Write a short comment describing this function
## This funciton takes matrix as an input and sets its inverse value to NULL. It further
## gets and sets values for matrix and its inverse (from cacheSolve function) and forms a easily acessible list that
## that can be called by cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
                  inverseMAT<-NULL
                  setMAT<- function(y){
                    x<<-y
                    inverse<<-NULL
                  }
                  getMAT<- function() x
                  setinverse<-function(inverse) inverseMAT<<- inverse
                  getinverse<- function() inverseMAT
                  list(setMAT=setMAT,getMAT=getMAT,
                       setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## This function takes inverse value from makeCacheMatrix function and checks if it is 
## or not NULL. If it is NULL and therefore inversion of this matrix is calculated for the
## first time, it will calculate inversion and store it in cache. If it is not NULL
## it will not calculate it again but uses cached value and writes message:"getting 
## cached inverse matrix data". 
cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMAT<-x$getinverse()
  if(!is.null(inverseMAT)) {
    message("getting cached inverse matrix data")
    return(inverseMAT)
  }
  matrixdata<-x$getMAT()
  inverseMAT<- solve(matrixdata,...)
  x$setinverse(inverseMAT)
  return(inverseMAT)
}
