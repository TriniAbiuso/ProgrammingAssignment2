## The function accepts a square matrix (assuming invertibility). It stores the matrix and initializes an inversed value set to null. It then declares 
## several getters and setters function to get and set the value of the matrix as well as to get and set the inverse value. It returns these functions in a list.

makeCacheMatrix <- function(mat=matrix()){
  inv <- NULL 
  setMat <- function(val){
    mat <<- val
    inv <<- NULL
  }
  getMat <- function() mat
  setInverse <- function(inversed) inv <<- inversed
  getInverse <- function() inv
  list(getMat=getMat, setMat=setMat, setInverse=setInverse, getInverse=getInverse)
}


## This function gets the list created by the makeCacheMatrix function and can now access the matrix. It checks whether the inverse has already been calculated or not, and if so it
## returns the cached value of the inverse. If the cached value has not been calculated yet it will compute the inverse using the solve() built-in function and will set the cache 
## using the setInverse function.

cacheSolve <- function(mat, ...){
        ## Return a matrix that is the inverse of 'x'

  cachedInversed <- mat$getInverse()
  if(!is.null(cachedInversed)){
    return(cachedInversed)
  }
  inversedMat <- solve(mat$getMat(), ...)
  mat$setInverse(inversedMat)
  inversedMat
  
}
