## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" oject that can cache its inverse.

makeCacheMatrix<-function(x=matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv <<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by above function.  If the inverse is already calculated, then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve<-function(x,...) {
   inv<-x$getinverse()
   if(!is.null(inv)) {
     message("...getting the cached data.")
     return(inv)
   }
   data<-x$get()
   inv<-solve(data)
   x$setinverse(inv)
   inv
 }
