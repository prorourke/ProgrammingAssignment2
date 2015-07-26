## Put comments here that give an overall description of what your
## These functions create a matrix object, determine
## if the inverse has been calculated, if so, retrieve the cached inverse, and,
## if not, calculate and cache its inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mat<-NULL
        set<-function(y){
                x<<-y
                mat<<-NULL
        get<-function() x
        setMat<-function(solve) mat<<- solve
        getMat<-function() mat
        list(set=set, get=get,
             setMat=setMat,
             getMat=getMat)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the 
## matrix has not changed), then`cacheSolve` should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        mat<-x$getMat()
        if(!is.null(mat)){
                return(mat)
        }
        matrix<-x$get()
        mat<-solve(matrix, ...)
        x$setMat(mat)
        mat
}
