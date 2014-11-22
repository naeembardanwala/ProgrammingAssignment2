## makeCacheMatrix(): Input: Matrix Output:List (function objects) 
##                    Taking advantage of Lexical scaoping, "mx" paramter and "mxI" stores the values in function environment
##                    when function is called, it returns list of objects (functions)

makeCacheMatrix <- function(mx = matrix()) {
  mxI <- NULL
  set <- function (mxNew=matrix()) {
    if (!(dim(mx) == dim(mxNew) && all(mx == mxNew))) 
    {
      mx <<- mxNew
      mxI <<- solve(mx)
    }
  }
  get <- function() { 
    mx 
  }
  ## When called, takes the inverse of matrix mx (gets value from fun-environment lexical scope)
  getInverse <- function() { 
    if (is.null(mxI)) {
      mxI <<- solve(mx)
    }else {
      message("returning cached Inverse")
    }
    mxI  
  }
  list(set = set, get = get,getInverse = getInverse)
}

## cacheSolve(): Input: (function object) 
cacheSolve <- function(x, ...){
  m <- myMx$getInverse()
  m
}
##> Examples:
x<-matrix(c(10,1,1,10),2,2)
x
myMx<-makeCacheMatrix(x)
cacheSolve(myMx)
cacheSolve(myMx)
myMx<-makeCacheMatrix(x+1)
cacheSolve(myMx)
cacheSolve(myMx)
