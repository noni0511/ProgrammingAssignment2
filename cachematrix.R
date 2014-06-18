##Student Name:   Noel Jiwanmall
##Assignment Num: 2
##Submission Via: Git Hub
##Assignment Date:06/17/2014
##Function in this assignment calculate the inverse of a matrix and cache it in memory for reuse 

## makeCacheMatrix will be my first implementation.
## function input: An invertible square Matrix with determiant not equal to zero
## this function is intended to manage to matrix from setting its values to setting its inverse
## to providing its output
## function output: none. It instead provides a list of private functions that can be executed in the global
## environment
makeCacheMatrix<-function(x=matrix()){
  
  i<-NULL ##intitalize an empty variable
  
  ##set variable i equal to the inverse that is provided as an argument
  setinverse<-function(inverse) i<<-inverse 
  
  ## return the inverse
  getinverse<-function() i
  
  ##Set will faciliate replacing of the matrix content with a new one. 
  set<-function(j){
    
    x<<-j
    i<<-NULL
  }
  
  ##return current matrix
  get<-function() x
  
  ##below declaration of functions allow us to call them from outside this function
  list(set=set,setinverse=setinverse,getinverse=getinverse,get=get)
}

## cacheSolve is our second function and performs one of two functions
## Function input: A Matrx and any other arguments
## Function 1: Calculate the inverse of a matrix and cache it in memory
## Function 2: Check if the inverse already exist, if yes then return it from cache memory
## Returns the inverse of the function provided it as an argument

cacheSolve<-function(i,...){
  ## assume the inverse exists and assign it to variable 'l'
  l<-i$getinverse()
  
  ##If the matrix inverse already exists and the matrix hasn't changed then return inverse from cache  
  if(!is.null(l) & identical(m$get(),i$get())){
    
    message("Returing inverse from Cache")
    return(l)
  }
  ##Else, get the matrix content by calling get()
  mymatrix<-i$get()
  
  ## calculated and save the inverse of the matrix with function 'Solve()'
  mi<-solve(mymatrix)
  
  ##cache the inverse in memory
  i$setinverse(mi)
  
  ##return this inverse of the matrix provided as an argument
  mi
  
}
