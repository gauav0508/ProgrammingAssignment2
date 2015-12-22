## The overall functionality of below two functions is to produce the inverted matrix as an output
## Cache the output and use it when the matrix is not changing instead of recalculating the inverted 
## matrix again


## the function sets value for inverted matrix in cache

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

## Cachesolve function checks if the invm is null. if its not null then retrives the value
## from cache otherwise it gets the matrix and produce the inverted matrix
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinvmat()
  if(!is.null(invm)){
    print("using Cache value")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data) %*% data ## calculating the inverted matrix
  x$setinvmat(invm)  ## sets the value to cache
  invm
}
