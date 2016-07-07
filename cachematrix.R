## Put comments here that give an overall description of what your
##Calculate and Cache the Inverse of the Matrix

## functions do
## First Function takes Matrix as argument and cache its inverse.
## Second Function takes first as argument and calculates the inverse and send back to first.

## Write a short comment describing this function
## makeCacheMatrix Function takes Matrix as argument and cache its inverse.

makeCacheMatrix<- function(x=matrix()){
  inverse<-NULL
  set<- function(y){
    x<<- y
    inverse<<- NULL
  }
  get<-function()x
  setInverseMat<- function(inverseMat) inverse<<- inverseMat
  getInverseMat<- function()inverse
  list(set=set,get=get,
       setInverseMat=setInverseMat,
       getInverseMat=getInverseMat)
}


## Write a short comment describing this function
##cacheSolve Function takes first(makeCacheMatrix) as argument 
##and calculates the inverse and send back to first.

cacheSolve<- function(x,...){
  inverse <- x$getInverseMat()
  if(!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  matrx <- x$get()
  inverse <- solve(matrx,...)
  x$setInverseMat(inverse)
  inverse
  
}
