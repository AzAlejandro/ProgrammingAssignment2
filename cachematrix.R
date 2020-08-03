#As requested, a function was created to cache 
#the information of the inverse of a matrix. If 
#the information was previously saved, it will not 
#be saved again, but it will show the stored data.


#makeCacheMatrix() stores the data of the matrix and 
#its inverse. This data can be accessed using x$get() 
#and x$getinverse(). As a parameter makeCacheMatrix() 
#receives a matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve () receives the information from the 
#makeCacheMatrix () function, calculates the inverse 
#of the stored matrix, and saves the information in the 
#variable created by makeCacheMatrix (). If the information 
#previously existed, it will only print it on screen.


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

