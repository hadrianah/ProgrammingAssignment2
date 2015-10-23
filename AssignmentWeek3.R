
#This function create a special matrix, which contains a list of methods that can be applied on it: 
# set, get, setInverse and getInverse.
#When they are used,these methods are called as: set, get, si and gi.
makeCacheMatrix <- function(x=matrix()){
  inverse<-NULL
  
  #This method sets the values of the matrix elements and sets the invers matrix to NULL
  set <- function(y){
    x<<-y
    inverse<<-NULL
  }
  
  #This method returns the matrix 
  get <-function() x
  
  #This method receives as an argument "am" (the inverse matrix) and saves its value (the matrix) in variable "inverse"
  setInverse <- function (am) inverse<<-am
  
  #This method return the inverse matrix which is stored in parameter "inverse"
  getInverse <- function() inverse
  
  list(set=set,get=get,si=setInverse, gi=getInverse)
  
}

#This method tries to read the inverse matrix.

#If the inverse matrix "inverse" is NOT null, it will return the cached inverse matrix and a message
#If the inverse matrix "inverse" is NULL, the method saves in "origMatrix" variable the original matrix, then applies on it
#the solve function, which computers the inverse of a matrix. The results of "solve" is saved in "im" variable.

# "im" variable stores the inverse matrix, which is passed as an argument to "si" method.
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