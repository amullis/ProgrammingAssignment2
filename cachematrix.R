## This code is designed to cache the inverse of a matrix
##This process is accomplished in two parts:
## 1-Function makeCacheMatrix creates a matrix object and can cache its inverse
## 2-Function cacheSolve computes the inverse of the matrix generated in part 1 
## if it is not already cached
 
## Part 1: Caching matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      inverse<-NULL
      set<-function(y){
      x<<-y
      inverse<<-NULL
}
    get<-function()x
    setinverse<-function(solve) inverse <<-solve
    getinverse<- function() inverse
    list(set=set, get=get,
    setinverse= setinverse,
    getinverse = getinverse)
}


## Part 2: Getting inverse

cacheSolve <- function(x, ...) {
       inverse <-x$getinverse()
       if(!is.null(inverse)){
           message("getting cached data")
           return(inverse)
      }
      data <-x$get()
      inverse<-solve(data, ...)
      x$setinverse(inverse)
        ## Return a matrix that is the inverse of 'x'
      inverse
}
