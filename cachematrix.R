##makeCacheMatrix() is a function that lists and holds the four following functions
##set() sets the value of the matrix
##get() returns the value of the matrix
##setinv() sets the value of the inverse matrix
##getinv() returns the value of the inverse matrix

makeCacheMatrix <- function(x=numeric){
    y<-NULL    
    
    set<-function(z){
        x<<-z
        y<<-NULL
          }
    
    get<-function() x
    setinv<-function(solve) y<<-solve
    getinv<-function() y
    list(set=set, 
         get=get, 
         setinv=setinv, 
         getinv=getinv)
}

##cacheSolve() solves for the inverse of the matrix provided by makeCacheMatrix()
##But if the inverse was already set in makeCacheMatrix(),
##cacheSolve() returns the cached data from makeCacheMatrix()

cacheSolve <- function(x, ...) {
    y<- x$getinv()
    if(!is.null(y)) {
        message("getting cached data")
        return(y)
    }
    data<-x$get()
    y<-solve(data,...)
    x$setinv(y)
    y
}
