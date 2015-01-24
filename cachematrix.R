## These functions Creates & Caches the inverse of a matrix
## It checks if the inverse of that matrix is already calculated or not
## If it has already been calculated it will return the result from cache..
## .. otherwise it will calculate the inverse.

## This function creates a special Matrix, gets the values of the matrix
## Computes the inverse of a new matrix
## Returns the inverse of the matrix from cache if already computed

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  
  # Compute inverse of Matrix
  setinverse<-function(solve) m<<- solve
  
  # Fetch inverse for an already computed Matrix
  getinverse<-function() m
  
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The function cacheSolve computes the inverse of a Matrix
## It checks if it has already been computed or not
## If it was calculated, it returns the inverse from cache
## If not, it computes the inverse and stores it.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  #Checl if "m" is empty or not
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matrix <- x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}
