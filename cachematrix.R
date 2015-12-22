## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the function sets value for inverted matrix in cache
## just for commit
makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  
  set <- function(y){
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  
  setinvmat <- function(invmat){
    invm <<- invmat
  }
  getinvmat <- function() invm
  
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
  
}

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinvmat()
  if(!is.null(invm)){
    print("from Cache")
    return(invm)
  }
  data1 <- x$get()
  invm <- solve(data1) %*% data
  x$setinvmat(invm)
  invm
}

