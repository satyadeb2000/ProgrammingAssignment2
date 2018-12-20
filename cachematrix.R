## makeCacheMatrix is a special matrix which calculates the inverse of a matrix and caches it. 
## cacheSolve checks if the inverse of a matrix has been cached. If yes, it fetches the inverse from the cache. 
##Else it calculates the inverse and stores it in the cache

## makeCacheMatrix is a special matrix which calculates the inverse of a matrix and caches it. 

makeCacheMatrix <- function(myMatrix = matrix()) {
  myInverse <- NULL
  set <- function(y) {
    myMatrix <<- y
    myInverse <<- NULL
  }
  
  get <- function() myMatrix
  
  setInverse <- function(argInverse) myInverse <<- argInverse
  getInverse <- function() myInverse
  
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve checks if the inverse of a matrix has been cached. If yes, it fetches the inverse from the cache. 
##Else it calculates the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  Inv <- solve(x$get())
  x$setInverse(Inv)
  Inv
}
