
# #MakeCache Matrix accepts an input matrix then does the following
## it organises itself into 2 getters and two setters
## It sets the value of a special matrix
## It gets the value of the matrix
## it sets the value of the inverse matrix
## it sets the getter for the inverse matrix.

makeCacheMatrix<-function(x2=matrix()){
  m2<-NULL
  set2<-function(a_matrix=matrix){
    x2<<-a_matrix
    m2<<-NULL
  }
  get2<-function() x2
  Set_Inverse_Matrix<-function() m2<<-solve(get2())
  Get_Inverse_Matrix<-function() m2
  list(set2=set2, get2=get2,
       Set_Inverse_Matrix=Set_Inverse_Matrix,
       Get_Inverse_Matrix=Get_Inverse_Matrix)
}


## This following function accepts the special matrix generated in "MakeCacheMAtrix above and then checks if
## the in verse had already been calculated and is in the cache.
## if it has not been cached, this function calculates the inverse and via the setter above, returns this value.
## if the value had been calculated and is already in cache, the function returns the cached value, indicating it is the cached value
## by writing "getting cached inverse matrix"...


cacheSolve<-function(x2,..){
  m2<-x2$Get_Inverse_Matrix()
  if(!is.null(m2)){
    message("getting cached inverse matrix")
    return(m2)
  }
  m2<-solve(x2$get2())
  x2$Set_Inverse_Matrix()
  m2
}
