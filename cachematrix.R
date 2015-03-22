# Function makeCacheMatrix return list containing functions
# (which is used as the input to function cacheSolve()) to:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse
# 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y     #'<<-' is use to assign value to object in 
                #environment which is different from the current environment
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

cacheSolve <- function(x, ...){
# Variable 'x' is output of function makeCacheMatrix()
# Function returns inverse of the original matrix input 
# to the function makeCacheMatrix()
  i <- x$getinverse()
  
# If the inverse has been already calculated:
  if (!is.null(i)){
# Take it from the cache, skip computation and give the message:
    message("getting cached data")
    return(i)
  }
# If not then calculates the inverse 
  data <- x$get()
  i <- solve(data)
# Sets the value into the cache via the setinverse function.
  x$setinverse(i)
  i
}
