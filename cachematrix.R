## Pair of functions listed below that will cache the inverse of a given matrix
##     function makeCacheMatrix - Creates a matrix object that caches its inverse
##     function cacheSolve - Computes the inverse of the matrix returned by makeCacheMatrix.  
##            If inverse has already been calculated, funciton will retrieve and return the cached inverse


## makeCacheMatrix - Creates a matrix object that caches its inverse
##      - Arguement is assumed to be a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  invM<- NULL
  set <- function(y){
    x<<- y
    invM<<-NULL
  }
  get <- function() x
  setInv <- function(inv) invM<-inv
  getInv <- function() invM
  list (set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve - Computes the inverse of the matrix returned by makeCacheMatrix above
##    If inverse has already been calculated, funciton will retrieve and return the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM<- x$getInv()
  if (!is.null(invM)){
    message("getting cached data")
    return(invM)
  }
  data<-x$get()
  invM<-solve(x)
  x$setInv(invM)
  invM
}
