##An invertible matrix is generated and passed.Careful to pass invertible 
##matrices.The function contains functions to set and get the passed
##matrix.Also the inverse can also be found out.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function()
  {
    x
  }
  setinverse<-function(k)
  {
    m<<-k
  }
  getinverse<-function()
  {
    m
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function checks whether the inverted matrix is in the cache.If it is in
##cach.If it is in the canhe computation can be avoided. Else it is calculated
##using the solve() function.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m))
  {
    message("getting cached inverse")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  m
}