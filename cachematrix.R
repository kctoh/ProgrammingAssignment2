## makeCacheMatrix() will cache the inverse matrix
## calculated by cacheResult()
## cacheResult() computes the inverse matrix and returns
## the cache result if there is one

## This function creates a matrix list that sets the matrix,
## get the matrix, set the Inverse matrix and
## get the Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedata<-NULL
  set<-function(y) {
    x<<-y
    cachedata<<-NULL
  }
  get<-function()x
  Matrixset<-function(inverse) cachedata<<-inverse
  Inversematrix<-function() cachedata
  list(set=set,
       get=get,
       Matrixset=Matrixset,
       Inversematrix=Inversematrix)
}


## This function will check if there is a cached inverse matrix
## and display the cached matrix. If there is no cached data,
## the inverse matrix is computed.

cacheResult <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedata<-x$Inversematrix()
  if(!is.null(cachedata)) {
    message("Cached data")
    return(cachedata)
  }
  result<-x$get()
  cachedata<-solve(result,...)
  x$Matrixset(cachedata)
  cachedata
}
