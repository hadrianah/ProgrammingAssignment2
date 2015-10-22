makeCacheMatrix <- function(x=matrix()){
  inverse<-NULL
  
  set <- function(y){
    x<<-y
    inverse<<-NULL
  }
  
  get <-function() x
  
  setInverse <- function (am) inverse<<-am
  
  getInverse <- function() inverse
  
  list(set=set,get=get,si=setInverse, gi=getInverse)
  
}

cacheSolve <- function (x,...){
  inverse<-x$gi()
  if (!is.null(inverse)){
    message ("getting cached data")
    return (inverse)
  }
  origMatrix <-x$get()
  im<-solve(origMatrix,...)
  x$si(im)
  im
}