## These two functions allow you to define a matrix, x, and calculate 
## its inverse.  If the inverse already exists, the inverse is called 
## cache. 
##
## The first function, makeCacheMatrrix, allows you to set the values of a matrix, get a previously defined matrix
## and get and set an inverse of this matrix, if it can be 
## successfully inverted. 

makeCacheMatrix<- function(x=matrix()) {
    inverse<- NULL
    set<-function(y) {
        x<<- y
        inverse<<- NULL
    }
    get<- function() x
    setinverse<- function(solve) inverse<<- solve
    getinverse<- function() inverse
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
## The second function,cacheSolve, calcualtes the inverse of a matrix, if it has 
## not been already calculated before. If it has been calcualted before,
## it retrieves the inverse from cache. 

cacheSolve<- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    inverse<-x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached value for inverse")
        return(inverse)
    }
    data<-x$get()
    inverse<- solve(data,...)
    x$setinverse(inverse)
    inverse
}

