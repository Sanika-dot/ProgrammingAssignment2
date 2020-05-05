## The functions makeCacheMatrix and cacheSolve create a matrix and can cache its inverse. 
## 

## This function defines a matrix object and clears the cache of the previous values. 

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setInv<-function(inverse) 
  m<<-inverse
getInv<-function() m
list(set = set, get = get,setInv = setInv, getInv = getInv)
}


## The function below calculates the inverse of the matrix created above. If the inverse of the concerned matrix already exists, it is merely 
## fetched from the cache. If the inverse does not exist, it is calculated.

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x, ...) {
 m<-x$getInv()
 if(!is.null(m)){
   message("obtaining cached data")
   return(m)
 }
 matrix<-x$get()
 m<-solve(matrix)
 x$setInv(m)
 m

}

}
