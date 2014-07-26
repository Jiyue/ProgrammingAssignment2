## The function (makeCacheMatrix) defines a list of functions which are further 
## independently defined to deposit or or retrive or initiate the inverse
## matrix to be calculated. 


makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function (){x}
  setinverse<-function(inverse_matrix){inverse<<-inverse_matrix}
  getinverse<-function() {inverse}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This cacheSolve function is to retrive an existing inverse matrix if it exists
## and calculate the inverse matrix for a given matrix if its inverse
## matrix does not exist.
cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if (!is.null(inverse)){
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
}
