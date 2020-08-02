# This function creates a list that stores the values to set and get the 
## matrix values and the cache the inverse

makeCacheMatrix <- function(x = matrix()) {
      i = NULL
      set = function(y){
            x<<-y
            i<<-NULL
      }
      get = function() x
      setinverse = function(inverse) i <<- inverse ##caching the inverse
      getinverse = function() i
      list(set = set,get = get, setinverse = setinverse,getinverse= getinverse)
}


## This function computes the inverse of the above matrix but speeds the process 
## by checking if the inverse has already been calculated

cacheSolve <- function(x, ...) {
      inv = x$getinverse()
      if(!is.null(inv)){
            message("Retrieving cache data.")
            return(inv)
      }
      matrix = x$get()
      inv = solve(matrix, ...)
      x$setinverse(inv)
      inv
      
}
