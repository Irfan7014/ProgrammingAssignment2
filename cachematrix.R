## Put comments here that give an overall description of what your
## functions do

## The following matrix creates a matrix and caches it inverse

makeCacheMatrix<-function(m = matrix()){
    x<-NULL
    setMat<-function(f){
       m<<-f
       x<<-NULL
    }
    getMat <- function() m
    setMatInv<-function(matrixinv)x<<-matrixinv
    getMatInv<-function() x
    list(setMat=setMat,getMat=getMat,
	 setMatInv=setMatInv,
         getMatInv=getMatInv)
}


## The following function computes the inverse of a matrix and caches it using makeCacheMatrix function above.

cacheSolve<-function(m, ...){
    ##Return a matrix that is inverse of the matrix 'mat'
    matInv<-m$getMatInv()
    if(!is.null(x)){
	message("getting cached data")
	return(x)
    }
    data<-m$getMat()
    x<-solve(data,...)
    m$setMatInv(x)
    x
}
