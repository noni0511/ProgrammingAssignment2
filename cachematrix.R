## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  
  setinverse<-function(inverse) i<<-inverse
  
  getinverse<-function() i
  
  set<-function(j){
    
    x<<-j
    i<<-NULL
  }
  
  get<-function() x
  
  list(set=set,setinverse=setinverse,getinverse=getinverse,get=get)
  
}


## Write a short comment describing this function

cacheSolve <- function(i, ...) {
        ## Return a matrix that is the inverse of 'i'
  l<-i$getinverse()
  
  if(!is.null(l) & identical(m$get(),i$get())){
    
    message("Returing inverse from Cache")
    return(l)
  }
  
  mymatrix<-i$get()
  
  mi<-solve(mymatrix)
  
  i$setinverse(mi)
  
  mi
}
