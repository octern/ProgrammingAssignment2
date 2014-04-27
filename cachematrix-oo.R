# just fooling around, wondering whether you can have the solve function in the cachematrix "object". 
# answer: yes, but there's no real reason to.

### NOTE:
# This is not my canonical solution to the assignment. For that, see "cachematrix.R"
###


makeCacheMatrix <- function(mx = matrix()) {
  # mx is the matrix that was passed in. Retrieved with get()
  # inv is the inverse of the matrix. Initially set to null, then saved after the first time we call cacheSolve
  inv<-NULL
  # I kept set() because it was present in the template function... but nothing ever calls it.
  # seems like a bad feature because it lets you change the matrix, while possibly retaining an incorrect cached inverse.
  set<-function(y) {
    mx<<-y
  }
  # get() returns the matrix. It's the equivalent of simply evaluating the variable if it were a regular matrix.
  get<-function() {
    return(mx)
  }
  # Call from outside to set the cached inverse. 
  # Note that the <<- operator must be used because inv exists in the calling environment.
  setinv<-function(i) inv<<-i
  getinv<-function() return(inv)
  # huh. Do I pass the object a copy of itself? Or does R have an equivalent of this / self?
  # Yeah, that works. 
  cacheSolve <- function(x) {
    # this will retrieve the cached inverse, or NULL if it was never calculated.
    i<<-x$getinv()
    
    if(!is.null(i)) {
      print("returning cached inverse")
      return(i)
    }
    else {
      print("calculating inverse")
      mx<-x$get()
      i<-solve(mx)
      x$setinv(i)
      return(x$getinv())
    }        
  }
  return(list(setinv=setinv, getinv=getinv, set=set, get=get, cacheSolve=cacheSolve))
}


mxTest<-function() {
  # just an easy way to test whether I messed anything up in the code above. 
  o<-makeCacheMatrix(matrix(c(3,0,0,0,1,2,0,0,0,0,2,0,0,0,0,2), ncol=4))
  # should calculate
  cacheSolve(o)
  # should retrieve
  cacheSolve(o)
}
