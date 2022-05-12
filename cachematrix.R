
#Returns a vector of set, get, setinverse and getinverse for cacheing matrix inverse. 
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <-function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

#Checks if inverse has already been calculted. If so, returns the previous calculation. If not, calcualtes inverse. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
