## This code takes a square matrix and calculates its inverse.
## It can be run with cacheSolve(makeCacheMatrix(x=matrix(c(),nrow=,ncol=...))).
## It has two functions makeCacheMatrix and cacheSolve.
##


## The below function(makeCacheMatrix) takes a matrix
## as an argument and returns 4 functions: set, get,
## setinv and getinv. The set function returns the
## argument it takes as a matrix that is defined in 
## its parent environment as it uses the assignment
## operator <<-, and sets the value of the inverse
## matrix (again with <<-) to be NULL initially.
## The get returns the argument x itself, setinv
## is a function that calculates the inverse of a
## matrix and assigns it to invm and getinv gets invm
## when it is called in another function.

makeCacheMatrix <- function(x = matrix()) {
    invm<-NULL
    set<-function(y){
        x<<-y
        invm<<-NULL
    }
    get<-function() x
    setinv<-function(solve) invm<<-solve
    getinv<-function() invm
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve is the main function that uses the 
## above defined makeCacheMatrix to 
## It starts by using the invm value that the
## getinv function in the above defined above is  
## called. This is not NULL only if it has already
## been calculated. In this case it prints that
## the value used was already in the cache.
## Otherwise it needs to calculate the inverse
## by calling the functions the makeCacheMatrix
## defined. It first assigns the input matrix to
## the variable dat, then calculates its inverse
## with solve and then set this as the value to
## be cached for the next call of the cacheSolve.
## Finally it returns the calculated invm.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    invm<-x$getinv()
    if(!is.null(invm)){
     message("Using the cached data")
     return(invm)
    }
    dat<-x$get()
    invm<-solve(dat,...)
    x$setinv(invm)
    invm
}