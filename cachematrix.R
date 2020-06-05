##An invertible matrix is generated and passed.Careful to pass invertible matrices

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


## To draw from the cache

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